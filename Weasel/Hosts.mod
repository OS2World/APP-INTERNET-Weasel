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
        (*  Last edited:        27 May 2017                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, INIData;

FROM SYSTEM IMPORT
    (* type *)  CARD8;

FROM Names IMPORT
    (* type *)  HostName, DomainName, HostCategory, FilenameString;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release, DestroyLock,
                CreateTask, TaskExit;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore;

FROM HostLists IMPORT
    (* type *)  HostList,
    (* proc *)  CreateHostList, DestroyHostList, RefreshHostList,
                RefreshHostList2, MatchHostName, MatchAnAddress;

FROM Sockets IMPORT
    (* const*)  AF_INET;

FROM NetDB IMPORT
    (* proc *)  gethostbyname;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM MiscFuncs IMPORT
    (* proc *)  AppendCard;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString, Swap4;

FROM INIData IMPORT
    (* proc *)  OpenINIFile, INIGet, INIGetString;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* const*)  DummyLogID,
    (* proc *)  LogTransaction;

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
    MasterList: ARRAY HostCategory OF HostList;

    (* The HostCategory means one of                        *)
    (*      (whitelisted, mayrelay, relaydest, banned)      *)
    (* MasterList keeps track of hosts in each category.    *)

    BLCheckSuffix: ARRAY BlacklistType OF DomainName;
    BlacklistChecking, BlacklistDisable: ARRAY BlacklistType OF BOOLEAN;

    (* BlacklistChecking is obtained from the INI file: the manager's   *)
    (* specification of which blacklists to use.  BlacklistDisable is   *)
    (* a flag to say that this blacklist is temporarily disabled, even  *)
    (* if it is one of the BlacklistChecking entries.                   *)

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
(*              ENABLING AND DISABLING BLACKLIST CHECKERS               *)
(************************************************************************)

TYPE
    Qptr = POINTER TO Qelt;
    Qelt =  RECORD
                next: Qptr;
                listnum: CARDINAL;
                delay: CARDINAL;        (* minutes *)
            END (*RECORD*);

    (* Note: the delay in this record is relative to the preceding      *)
    (* list element.  Only at the head of the queue does it mean        *)
    (* "minutes from now".                                              *)

VAR
    Qhead: Qptr;
    Qguard: Lock;
    shutdown: BOOLEAN;

(************************************************************************)

PROCEDURE BlacklistCheckerTask;

    (* Runs as a separate task, but it only wakes up once a minute. *)

    CONST OneMinute = 60*1000;

    VAR next: Qptr;

    BEGIN
        IF shutdown THEN
            TaskExit;
        ELSE
            Obtain(Qguard);
            IF Qhead <> NIL THEN
                IF Qhead^.delay > 0 THEN
                    DEC (Qhead^.delay);
                    IF Qhead^.delay = 0 THEN
                        BlacklistDisable[Qhead^.listnum] := FALSE;
                        next := Qhead^.next;
                        DISPOSE (Qhead);
                        Qhead := next;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            Release(Qguard);
            Sleep (OneMinute);
        END (*IF*);
    END BlacklistCheckerTask;

(************************************************************************)

PROCEDURE ScheduleWakeup (j, minutes: CARDINAL);

    (* Schedules the "disable" on blacklist checker j to be cancelled   *)
    (* after the specified number of minutes.                           *)

    VAR previous, current, p: Qptr;

    BEGIN
        Obtain(Qguard);
        previous := NIL;  current := Qhead;
        WHILE (current <> NIL) AND (minutes >= current^.delay) DO
            DEC (minutes, current^.delay);
            previous := current;  current := current^.next;
        END (*WHILE*);

        (* Put the new entry between previous and current. *);

        NEW (p);
        p^.next := current;
        p^.listnum := j;
        p^.delay := minutes;

        IF current <> NIL THEN
            DEC (current^.delay, minutes);
        END (*IF*);
        IF previous = NIL THEN
            Qhead := p;
        ELSE
            previous^.next := current;
        END (*IF*);
        Release(Qguard);
    END ScheduleWakeup;

(************************************************************************)

PROCEDURE TurnOffBlacklist (j: CARDINAL);

    (* Temporarily disables blacklist checker j.  It will be re-enabled *)
    (* one hour later.                                                  *)

    BEGIN
        BlacklistDisable[j] := TRUE;
        shutdown := FALSE;
        ScheduleWakeup(j, 60);
    END TurnOffBlacklist;

(************************************************************************)

PROCEDURE InitBlacklistDisabling;

    (* Starts the monitor that looks after disabling blacklist checkers. *)

    BEGIN
        shutdown := FALSE;
        Qhead := NIL;
        CreateLock (Qguard);
        EVAL (CreateTask(BlacklistCheckerTask, 1, "BL_checker"));
    END InitBlacklistDisabling;

(************************************************************************)

PROCEDURE CleanupBlacklistDisabling;

    (* Terminates the monitor that looks after disabling blacklist checkers. *)

    VAR next: Qptr;

    BEGIN
        Obtain(Qguard);
        WHILE Qhead <> NIL DO
            next := Qhead^.next;
            DISPOSE (Qhead);
            Qhead := next;
        END (*WHILE*);
        Release(Qguard);
        DestroyLock (Qguard);
        shutdown := TRUE;
    END CleanupBlacklistDisabling;

(************************************************************************)
(*                 THE EXTERNALLY CALLABLE PROCEDURES                   *)
(************************************************************************)

PROCEDURE OnBlacklist (IPAddress: CARDINAL;  ID: TransactionLogID;
                            watchdog: Semaphore;
                            VAR (*OUT*) message: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if IPAddress is on one of the realtime blacklists   *)
    (* for which we have blacklist checking enabled.  In this case      *)
    (* message is set to a suitable error response to the client.  We   *)
    (* signal on watchdog before checking each list, because the checks *)
    (* do involve time delays.                                          *)

    CONST patience = 60;        (* seconds *)

    VAR name: HostName;  IPBuffer: ARRAY [0..15] OF CHAR;
        start, seconds: CARDINAL;
        j: BlacklistType;  rejected: BOOLEAN;

    BEGIN
        IPToString (Swap4(IPAddress), FALSE, IPBuffer);
        rejected := FALSE;  j := MIN(BlacklistType);
        LOOP
            IF BlacklistChecking[j] AND NOT BlacklistDisable[j] THEN
                Strings.Assign (IPBuffer, name);
                Strings.Append ('.', name);
                Strings.Append (BLCheckSuffix[j], name);
                start := time();
                rejected := gethostbyname(name) <> NIL;
                seconds := time() - start;
                Strings.Assign (BLCheckSuffix[j], message);
                Strings.Append (" check took ", message);
                AppendCard (seconds, message);
                Strings.Append (" seconds", message);
                LogTransaction (ID, message);
                IF seconds > patience THEN
                    TurnOffBlacklist(j);
                END (*IF*);
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
            BlacklistDisable[j] := FALSE;
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
    InitBlacklistDisabling;
FINALLY
    CleanupBlacklistDisabling;
    FOR j := MIN(HostCategory) TO MAX(HostCategory) DO
        DestroyHostList (MasterList[j]);
    END (*FOR*);
END Hosts.

