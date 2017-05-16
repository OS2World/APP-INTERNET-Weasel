(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE Hosts;

        (********************************************************)
        (*                                                      *)
        (*          Checks on host names and addresses          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            9 May 1998                      *)
        (*  Last edited:        11 May 2017                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM SYSTEM IMPORT
    (* type *)  CARD8;

FROM Names IMPORT
    (* type *)  HostName, DomainName, HostCategory, FilenameString;

FROM HostLists IMPORT
    (* type *)  HostList,
    (* proc *)  CreateHostList, DestroyHostList, RefreshHostList,
                RefreshHostList2, MatchHostName, MatchAnAddress;

FROM Sockets IMPORT
    (* const*)  AF_INET;

FROM NetDB IMPORT
    (* proc *)  gethostbyname;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString, Swap4;

FROM INIData IMPORT
    (* proc *)  OpenINIFile, INIGet, INIGetString;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  DummyLogID;

(************************************************************************)

CONST
    Nul = CHR(0);
    NumberOfBlacklistDomains = 8;

TYPE
    LabelString = ARRAY [0..11] OF CHAR;
    Label = ARRAY HostCategory OF LabelString;
    BlacklistType = [1..NumberOfBlacklistDomains];
    BLSuffix = ARRAY BlacklistType OF ARRAY [0..31] OF CHAR;

CONST
    LogLabel = Label {"whitelisted", "trusted", "Gatefor", "banned"};
    INILabel = Label {"Whitelisted", "MayRelay", "RelayDest", "Banned"};
    DefaultCheckSuffix = BLSuffix {"blackholes.mail-abuse.org",
                                   "dialups.mail-abuse.org",
                                   "relays.mail-abuse.org",
                                   "", "", "", "", ""};

VAR
    BLCheckSuffix: ARRAY BlacklistType OF DomainName;
    MasterList: ARRAY HostCategory OF HostList;
    BlacklistChecking: ARRAY BlacklistType OF BOOLEAN;

(************************************************************************)
(*                           SCREEN OUTPUT                              *)
(************************************************************************)

(*
PROCEDURE WriteIPAddr (addr: CARDINAL);

    (* Writes an IP address to the screen. *)

    VAR IPBuffer: ARRAY [0..16] OF CHAR;

    BEGIN
        IPToString (addr, TRUE, IPBuffer);
        WriteString (IPBuffer);
    END WriteIPAddr;

(************************************************************************)

PROCEDURE DumpList (j: HostCategory);

    (* Writes the list to the screen. *)

    VAR current: HostRecordPtr;

    BEGIN
        WriteString ("Dumping ");  WriteString (INILabel[j]);
        WriteLn;
        current := OldMasterList[j];
        WHILE current <> NIL DO
            IF current^.numeric THEN
                WriteIPAddr (current^.numaddr);
            END (*IF*);
            IF current^.text THEN
                WriteString (current^.textaddr);
            END (*IF*);
            WriteLn;
            current := current^.next;
        END (*WHILE*);
    END DumpList;
*)

(************************************************************************)
(*                 THE EXTERNALLY CALLABLE PROCEDURES                   *)
(************************************************************************)

PROCEDURE OnBlacklist (IPAddress: CARDINAL;
                            VAR (*OUT*) message: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if IPAddress is on one of the realtime blacklists   *)
    (* for which we have blacklist checking enabled.  In this case      *)
    (* message is set to a suitable error response to the client.       *)

    VAR name: HostName;  IPBuffer: ARRAY [0..15] OF CHAR;
        j: BlacklistType;  rejected: BOOLEAN;

    BEGIN
        IPToString (Swap4(IPAddress), FALSE, IPBuffer);
        rejected := FALSE;  j := MIN(BlacklistType);
        LOOP
            IF BlacklistChecking[j] THEN
                Strings.Assign (IPBuffer, name);
                Strings.Append ('.', name);
                Strings.Append (BLCheckSuffix[j], name);
                rejected := gethostbyname(name) <> NIL;
            END (*IF*);
            IF rejected OR (j=MAX(BlacklistType)) THEN EXIT(*LOOP*) END(*IF*);
            INC (j);
        END (*LOOP*);

        IF rejected THEN
            Strings.Assign ("571 Connection refused, ", message);
            IPToString (IPAddress, FALSE, IPBuffer);
            Strings.Append (IPBuffer, message);
            Strings.Append (" is on blacklist at ", message);
            Strings.Append (BLCheckSuffix[j], message);
        ELSE
            message[0] := Nul;
        END (*IF*);

        RETURN rejected;

    END OnBlacklist;

(************************************************************************)

PROCEDURE CheckHost (IPAddress: CARDINAL;
                     VAR (*OUT*) IsBanned, OnWhitelist, MayRelay: BOOLEAN);

    (* Looks up our internal list of hosts, and returns results: *)
    (*     IsBanned:   this host is on our blacklist, we will    *)
    (*                 refuse any connection from it.            *)
    (*     OnWhitelist: this host is whitelisted                 *)
    (*     MayRelay:   this host is one from whom we will        *)
    (*                 accept mail to be relayed.                *)

    BEGIN
        OnWhitelist := MatchAnAddress (MasterList[whitelisted], IPAddress);
        IsBanned := (NOT OnWhitelist) AND
                         MatchAnAddress (MasterList[banned], IPAddress);
        MayRelay := (NOT IsBanned) AND
                         MatchAnAddress (MasterList[mayrelay], IPAddress);
    END CheckHost;

(************************************************************************)

PROCEDURE BannedHost (VAR (*IN*) name: HostName): BOOLEAN;

    (* Returns TRUE if name matches a name in the "banned" list.  This  *)
    (* is different from the CheckHost check because we are now         *)
    (* checking the name rather than the address.                       *)

    VAR LogID: TransactionLogID;

    BEGIN
        LogID := DummyLogID();
        RETURN MatchHostName (MasterList[banned], name, FALSE, LogID);
    END BannedHost;

(************************************************************************)

PROCEDURE AcceptableRelayDestination (VAR (*IN*) name: HostName): BOOLEAN;

    (* Returns TRUE if name matches a name in the "acceptable relay     *)
    (* destinations" list.                                              *)

    VAR LogID: TransactionLogID;

    BEGIN
        LogID := DummyLogID();
        RETURN MatchHostName (MasterList[relaydest], name, FALSE, LogID);
    END AcceptableRelayDestination;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

PROCEDURE CheckRBLOption (UseTNI: BOOLEAN);

    VAR hini: INIData.HINI;  RBLchecking: CARD8;  j: BlacklistType;
        number: ARRAY [0..0] OF CHAR;
        app: ARRAY [0..4] OF CHAR;  key: ARRAY [0..15] OF CHAR;
        domain: DomainName;

    BEGIN
        app := "$SYS";
        key := "Weasel.INI";
        hini := OpenINIFile (key, UseTNI);
        IF NOT INIData.INIValid (hini) THEN
            RBLchecking := 0;
        ELSE
            key := "RBLcheck";
            IF NOT INIGet (hini, app, key, RBLchecking) THEN
                RBLchecking := 0;
            END (*IF*);
        END (*IF*);

        FOR j := MIN(BlacklistType) TO MAX(BlacklistType) DO
            BlacklistChecking[j] := ODD(RBLchecking);
            RBLchecking := RBLchecking DIV 2;
            IF BlacklistChecking[j] THEN
                Strings.Assign ("RBLDomain", key);
                number[0] := CHR(ORD('0') + ORD(j));
                Strings.Append (number, key);
                IF INIGetString (hini, app, key, domain) THEN
                    Strings.Assign (domain, BLCheckSuffix[j]);
                ELSE
                    Strings.Assign (DefaultCheckSuffix[j], BLCheckSuffix[j]);
                END (*IF*);
            END (*IF*);
        END (*FOR*);

        IF INIData.INIValid (hini) THEN
            INIData.CloseINIFile (hini);
        END (*IF*);

    END CheckRBLOption;

(************************************************************************)

PROCEDURE RefreshHostLists (LogIt, UseTNI: BOOLEAN);

    (* This procedure is to be called whenever there is a chance that   *)
    (* the INI data might have been updated.  Discards the existing     *)
    (* version of all host lists, and builds new copies.                *)

    VAR Loopback: ARRAY [0..1] OF CARDINAL;
        INIname: FilenameString;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..11] OF CHAR;

    BEGIN
        Loopback[0] := 16777343;
        Loopback[1] := 0;
        INIname := "Weasel.INI";
        app := "$SYS";
        key := "Whitelisted";
        RefreshHostList (INIname, app, key, UseTNI,
                             MasterList[whitelisted], FALSE, LogIt);
        key := "MayRelay";
        RefreshHostList2 (INIname, app, key, UseTNI,
                             MasterList[mayrelay], Loopback, FALSE, LogIt);
        key := "RelayDest";
        RefreshHostList (INIname, app, key, UseTNI,
                             MasterList[relaydest], FALSE, LogIt);
        key := "Banned";
        RefreshHostList (INIname, app, key, UseTNI,
                             MasterList[banned], FALSE, LogIt);
        CheckRBLOption (UseTNI);
    END RefreshHostLists;

(************************************************************************)

VAR j: HostCategory;

BEGIN
    FOR j := MIN(HostCategory) TO MAX(HostCategory) DO
        MasterList[j] := CreateHostList (LogLabel[j], FALSE);
    END (*FOR*);
FINALLY
    FOR j := MIN(HostCategory) TO MAX(HostCategory) DO
        DestroyHostList (MasterList[j]);
    END (*FOR*);
END Hosts.

