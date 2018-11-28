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

IMPLEMENTATION MODULE HammerCheck;

        (********************************************************)
        (*                                                      *)
        (*           Defence against password attacks           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            6 November 2017                 *)
        (*  Last edited:        16 November 2017                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

            (* WHILE DEBUGGING *)

IMPORT Strings;

FROM SYSTEM IMPORT CAST;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

            (* END WHILE DEBUGGING *)

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)
(*                   GUARD AGAINST DICTIONARY ATTACK                    *)
(************************************************************************)

CONST
    testing = FALSE;
    InitialThrottleTime = 5;       (* seconds *)
    ThrottleTimeIncrement = 10;     (* seconds *)

TYPE
    HammerListPointer = POINTER TO HammerListEntry;
    HammerListEntry = RECORD
                          next: HammerListPointer;
                          time, penalty: CARDINAL;          (* seconds *)
                          address: CARDINAL;
                      END (*RECORD*);

VAR HammerList: RECORD
                    access: Lock;
                    head: HammerListPointer;
                END (*RECORD*);

    (* This list is kept sorted by the "time" field.  That field says   *)
    (* when this entry is due to be removed from the list.              *)

(************************************************************************)
(*                           DEBUGGING CODE                             *)
(************************************************************************)

PROCEDURE Hex1 (val: CARDINAL): CHAR;

    (* Converts val to 1-digit hex. *)

    BEGIN
        IF val > 9 THEN
            RETURN CHR(ORD('A') - 10 + val);
        ELSE
            RETURN CHR(ORD('0') + val);
        END (*IF*);
    END Hex1;

(************************************************************************)

PROCEDURE FormatHex (val, N: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes N-digit hex number to text[pos], updates pos. *)

    BEGIN
        IF N > 1 THEN
            FormatHex (val DIV 16, N-1, text, pos);
        END (*IF*);
        text[pos] := Hex1(val MOD 16);
        INC (pos);
    END FormatHex;

(************************************************************************)

PROCEDURE FormatHex32 (val: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes 8-digit hex number to text[pos], updates pos. *)

    BEGIN
        FormatHex (val, 8, text, pos);
    END FormatHex32;

(************************************************************************)

PROCEDURE FormatDec (val: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes decimal number to text[pos], updates pos. *)

    BEGIN
        IF val > 9 THEN
            FormatDec (val DIV 10, text, pos);
            val := val MOD 10;
        END (*IF*);
        text[pos] := CHR(ORD('0') + val);  INC (pos);
    END FormatDec;

(************************************************************************)

PROCEDURE FormatRecord (p: HammerListPointer;
                                        VAR (*OUT*) text: ARRAY OF CHAR);

    VAR pos: CARDINAL;

    BEGIN
        Strings.Assign ("   ", text);
        pos := LENGTH(text);
        FormatHex32 (CAST (CARDINAL, p^.next), text, pos);
        text[pos] := ' ';  INC(pos);
        FormatDec (p^.time, text, pos);
        text[pos] := ' ';  INC(pos);
        FormatDec (p^.penalty, text, pos);
        text[pos] := ' ';  INC(pos);
        FormatHex32 (p^.address, text, pos);
        text[pos] := CHR(0);
    END FormatRecord;

(************************************************************************)

PROCEDURE DumpList (ID: TransactionLogID);

    (* Dumps the list to the transaction log.  We assume that the       *)
    (* caller has exclusive access.                                     *)

    VAR current: HammerListPointer;
        message: ARRAY [0..79] OF CHAR;

    BEGIN
        LogTransactionL (ID, "Entering DumpList");
        current := HammerList.head;
        IF current = NIL THEN
            LogTransactionL (ID, "HammerList is empty");
        ELSE
            LogTransactionL (ID, "HammerList:");
            WHILE current <> NIL DO
                FormatRecord (current, message);
                LogTransaction (ID, message);
                current := current^.next;
            END (*WHILE*);
            LogTransactionL (ID, "   ----- end of list");
        END (*IF*);
        LogTransactionL (ID, "Leaving DumpList");
    END DumpList;

(************************************************************************)
(*                        KEEPING THE LIST SORTED                       *)
(************************************************************************)

PROCEDURE ResortList (p: HammerListPointer);

    (* Moves p^ to the correct place in the list, knowing that its      *)
    (* "time" field has increased.                                      *)

    VAR previous, current: HammerListPointer;

    BEGIN
        Obtain (HammerList.access);

        (* Skip past the records that don't need to be changed. *)

        previous := NIL;  current := HammerList.head;
        WHILE current <> p DO
            previous := current;
            current := current^.next;
        END (*WHILE*);
        current := p^.next;

        (* At this point previous points to the record before p^, and   *)
        (* current points to the record following p^.  It's worth       *)
        (* checking for the special case where nothing has to be moved. *)

        IF (current <> NIL) AND (current^.time < p^.time) THEN

            (* Detach p^ from the list. *)
    
            IF previous = NIL THEN
                HammerList.head := current;
            ELSE
                previous^.next := current;
            END (*IF*);
    
            (* Work out where p really belongs. *)
    
            WHILE (current <> NIL) AND (current^.time < p^.time) DO
                previous := current;
                current := current^.next;
            END (*WHILE*);
    
            (* Reinsert p^ between previous^ and current^.  This might put  *)
            (* the record back exactly where it came from, except that that *)
            (* special case has already been checked, above.                *)
    
            IF previous = NIL THEN
                HammerList.head := p;
            ELSE
                previous^.next := p;
            END (*IF*);
            p^.next := current;

        END (*IF*);

        Release (HammerList.access);

    END ResortList;

(************************************************************************)
(*                          INCREASING THE PENALTY                      *)
(************************************************************************)

PROCEDURE IncPenalty (p: HammerListPointer);

    (* Adds ThrottleTimeIncrement to p^.penalty and p^.time. *)

    VAR t: CARDINAL;

    BEGIN
        t := time();
        IF p^.penalty > MAX(CARDINAL) - ThrottleTimeIncrement THEN
            p^.penalty := MAX(CARDINAL);
            p^.time := MAX(CARDINAL);
        ELSE
            INC (p^.penalty, ThrottleTimeIncrement);
            IF t > MAX(CARDINAL) - p^.penalty THEN
                p^.time := MAX(CARDINAL);
            ELSE
                p^.time := t + p^.penalty;
            END (*IF*);
        END (*IF*);
    END IncPenalty;

(************************************************************************)
(*                    THE EXTERNALLY CALLABLE PROCEDURES                *)
(************************************************************************)

PROCEDURE NotePasswordError (IPAddress: CARDINAL);

    (* Records this IP address as one that, because of undesirable      *)
    (* behaviour, should be locked out for a short time.  For a         *)
    (* persistent attacker, that "short time" will increase.            *)

    VAR t: CARDINAL;  found: BOOLEAN;
        insertafter, current: HammerListPointer;

    BEGIN
        t := time();          (* seconds *)
        Obtain (HammerList.access);

        (* First delete all the expired entries in the list. *)

        current := HammerList.head;
        WHILE (current <> NIL) AND (current^.time < t) DO
            HammerList.head := current^.next;
            DISPOSE (current);
            current := HammerList.head;
        END (*WHILE*);

        (* Look for an existing match for this IP address.  If there is *)
        (* such an entry, increase its penalty.  We will later resort   *)
        (* the list to take this into account.                          *)

        INC (t, InitialThrottleTime);
        found := FALSE;
        insertafter := NIL;
        WHILE (current <> NIL) AND NOT found DO
            IF current^.address = IPAddress THEN

                (* Increase the time penalty because of the new offence. *)

                IncPenalty (current);
                found := TRUE;
            ELSE
                IF current^.time < t THEN
                    insertafter := current;
                END (*IF*);
                current := current^.next;
            END (*IF*);
        END (*WHILE*);

        IF NOT found THEN

            (* Create a new entry after insertafter^. *)

            NEW (current);
            IF insertafter = NIL THEN
                current^.next := HammerList.head;
                HammerList.head := current;
            ELSE
                current^.next := insertafter^.next;
                insertafter^.next := current;
            END (*IF*);
            WITH current^ DO
                time := t;
                penalty := InitialThrottleTime;
                address := IPAddress;
            END (*WITH*);
        END (*IF*);

        Release (HammerList.access);
        IF found THEN
            ResortList (current);
        END (*IF*);

    END NotePasswordError;

(************************************************************************)

PROCEDURE Throttle (IPAddress: CARDINAL;  ID: TransactionLogID): BOOLEAN;

    (* Returns TRUE iff this IP address is on the list of addresses to  *)
    (* be blocked because of recent suspect behaviour.                  *)

    VAR t: CARDINAL;  found: BOOLEAN;
        current: HammerListPointer;

    BEGIN
        t := time();          (* seconds *)
        Obtain (HammerList.access);

        IF testing THEN
            DumpList (ID);
        END (*IF*);

        (* First delete all the expired entries in the list. *)

        current := HammerList.head;
        WHILE (current <> NIL) AND (current^.time < t) DO
            HammerList.head := current^.next;
            DISPOSE (current);
            current := HammerList.head;
        END (*WHILE*);

        (* Now see whether this IP address is on the list. *)

        found := FALSE;
        WHILE (current <> NIL) AND NOT found DO
            found := current^.address = IPAddress;
            IF NOT found THEN
                current := current^.next;
            END (*IF*);
        END (*WHILE*);

        (* If it is on the list, increase its penalty. *)

        IF found THEN
            IncPenalty (current);
        END (*IF*);

        Release (HammerList.access);

        IF found THEN
            ResortList (current);
        END (*IF*);

        RETURN found;

    END Throttle;

(************************************************************************)

BEGIN
    CreateLock (HammerList.access);
    HammerList.head := NIL;
FINALLY
    DestroyLock (HammerList.access);
END HammerCheck.

