(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE RelayRules;

        (********************************************************)
        (*                                                      *)
        (*   Rules for choosing a relay host for outgoing mail  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 August 2017                  *)
        (*  Last edited:        6 December 2019                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Names IMPORT
    (* type *)  DomainName, FilenameString;

FROM MiscFuncs IMPORT
    (* proc *)  ToLower, StringMatch;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  OpenOldFile, CloseFile,
                ReadLine;

FROM WINI IMPORT
    (* proc *)  OpenINI, CloseINI;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGetString;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    tab = CHR(9);
    CtrlZ = CHR(26);
    space = CHR(32);
    DefaultRelayRuleFileName = "RELAYRULES.TXT";

TYPE
    (* A list of destinations with the same relay host. *)

    DestList = POINTER TO
                    RECORD
                        next:   DestList;
                        target: DomainName;
                    END (*RECORD*);

    (* List of all relay hosts, and which DestList to use for each. *)

    RuleList = POINTER TO
                    RECORD
                        next:  RuleList;
                        via:   DomainName;
                        dests: DestList;
                    END (*RECORD*);

(************************************************************************)

VAR
    (* The list of rules, and a lock to control access. *)

    Rules: RuleList;

    RuleLock: Lock;

(************************************************************************)
(*                CONSULT THE LIST TO FIND A RELAY DOMAIN               *)
(************************************************************************)

PROCEDURE FindRelay (VAR (*IN*) destination: DomainName;
                            VAR (*OUT*) relay: DomainName);

    (* Given the destination domain for mail, return relay as the host  *)
    (* or domain to which the mail should be sent.  If there is no      *)
    (* applicable rule, copies destination to relay.                    *)

    VAR p: RuleList;  q: DestList;  found: BOOLEAN;
        candidate: DomainName;

    BEGIN
        Obtain (RuleLock);
        p := Rules;  found := FALSE;
        WHILE (p <> NIL) AND NOT found DO
            candidate := p^.via;
            q := p^.dests;
            WHILE (q <> NIL) AND NOT WildMatch (destination, q^.target) DO
                q := q^.next;
            END (*WHILE*);
            found := q <> NIL;
            p := p^.next;
        END (*WHILE*);
        IF found THEN
            relay := candidate;
        ELSE
            relay := destination;
        END (*IF*);
        Release (RuleLock);
    END FindRelay;

(************************************************************************)
(*                     LOADING THE RULES FROM A FILE                    *)
(************************************************************************)

PROCEDURE DiscardDestList (VAR (*INOUT*) p: DestList);

    VAR next: DestList;

    BEGIN
        WHILE p <> NIL DO
            next := p^.next;
            DISPOSE (p);
            p := next;
        END (*WHILE*);
    END DiscardDestList;

(************************************************************************)

PROCEDURE ClearRelayRules;

    VAR next: RuleList;

    BEGIN
        Obtain (RuleLock);
        WHILE Rules <> NIL DO
            DiscardDestList (Rules^.dests);
            next := Rules^.next;
            DISPOSE (Rules);
            Rules := next;
        END (*WHILE*);
        Release (RuleLock);
    END ClearRelayRules;

(************************************************************************)

PROCEDURE StripSpaces (VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Removes leading and trailing spaces from line. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := 0;
        WHILE (line[pos] = space) OR (line[pos] = tab) DO
            INC (pos);
        END (*WHILE*);
        IF pos > 0 THEN
            Strings.Delete (line, 0, pos);
        END (*IF*);
        pos := Strings.Length(line);
        WHILE (pos > 0) AND ((line[pos-1] = space) OR (line[pos-1] = tab)) DO
            DEC (pos);
            line[pos] := Nul;
        END (*WHILE*);
    END StripSpaces;

(************************************************************************)

PROCEDURE GetToken (VAR (*INOUT*) line: ARRAY OF CHAR;
                            VAR (*OUT*) token: ARRAY OF CHAR);

    (* Reads the initial "via" or "to" from line, returns it in token.  *)
    (* Strips the token and the following ':' or whitespace from line.  *)

    TYPE CharSet = SET OF CHAR;
    CONST Stoppers = CharSet {tab, space, ':'};

    VAR ch: CHAR;
        pos: CARDINAL;

    BEGIN
        ch := line[0];
        pos := 0;
        WHILE (pos <= HIGH(line)) AND NOT (ch IN Stoppers) DO
            IF pos <= HIGH(token) THEN
                token[pos] := ch;
            END (*IF*);
            INC (pos);
            IF pos <= HIGH(line) THEN
                ch := line[pos];
            END (*IF*);
        END (*WHILE*);

        (* Terminate token. *)

        IF pos <= HIGH(token) THEN
            token[pos] := Nul;
        END (*IF*);

        (* Remove token, and any trailing stoppers, from line. *)

        WHILE (pos <= HIGH(line)) AND (ch IN Stoppers) DO
            INC (pos);
            IF pos <= HIGH(line) THEN
                ch := line[pos];
            END (*IF*);
        END (*WHILE*);
        IF pos > HIGH(line) THEN
            DEC (pos);
        END (*IF*);
        Strings.Delete (line, 0, pos);

    END GetToken;

(************************************************************************)

PROCEDURE LoadRelayRules;

    (* Loads or reloads the relay rules from a file. *)

    VAR cid: ChanId;  hini: HINI;
        lastrule, thisset: RuleList;
        lastdest, thisdest: DestList;
        SYSapp: ARRAY [0..3] OF CHAR;
        key: ARRAY [0..31] OF CHAR;
        line: ARRAY [0..512] OF CHAR;
        token: ARRAY [0..15] OF CHAR;
        RelayRuleFileName: FilenameString;

    BEGIN
        hini := OpenINI();
        IF INIValid(hini) THEN
            SYSapp := "$SYS";
            key := "RelayRuleFileName";
            IF NOT INIGetString (hini, SYSapp, key, RelayRuleFileName) THEN
                RelayRuleFileName := DefaultRelayRuleFileName;
            END (*IF*);
            CloseINI;
        END (*IF*);

        (* Reload data from the file. *)

        ClearRelayRules;
        lastrule := NIL;  thisset := NIL;  lastdest := NIL;
        cid := OpenOldFile (RelayRuleFileName, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            Obtain (RuleLock);
            ReadLine (cid, line);
            WHILE line[0] <> CtrlZ DO
                StripSpaces (line);
                IF (line[0] <> Nul) AND (line[0] <> ';') THEN
                    ToLower (line);
                    GetToken (line, token);
                    IF line[0] = Nul THEN

                        (* Empty domain name, ignore this line. *)

                    ELSIF StringMatch (token, "via") THEN

                        (* A new set of rules. *)

                        NEW (thisset);
                        thisset^.next := NIL;
                        Strings.Assign (line, thisset^.via);
                        thisset^.dests := NIL;
                        IF lastrule = NIL THEN
                            Rules := thisset;
                        ELSE
                            lastrule^.next := thisset;
                        END (*IF*);
                        lastrule := thisset;
                        lastdest := NIL;


                    ELSIF StringMatch (token, "to") THEN

                        (* A new destination in the current set. *)

                        IF thisset = NIL THEN

                            (* Error: a destination without any "via". *)

                        ELSE
                            NEW (thisdest);
                            thisdest^.next := NIL;
                            Strings.Assign (line, thisdest^.target);
                            IF lastdest = NIL THEN
                                thisset^.dests := thisdest;
                            ELSE
                                lastdest^.next := thisdest;
                            END (*IF*);
                            lastdest := thisdest;
                        END (*IF*);

                    ELSE

                        (* Unknown token, ignore this line. *)

                    END (*IF*);
                END (*IF*);
                ReadLine (cid, line);
            END (*WHILE*);
            Release (RuleLock);
            CloseFile (cid);
        END (*IF*);
    END LoadRelayRules;

(************************************************************************)

BEGIN
    CreateLock (RuleLock);
    Rules := NIL;
FINALLY
    ClearRelayRules;
    DestroyLock (RuleLock);
END RelayRules.

