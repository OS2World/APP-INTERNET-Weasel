(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE Domains;

        (********************************************************)
        (*                                                      *)
        (*        The local domains that Weasel is hosting      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            22 July 2002                    *)
        (*  Last edited:        6 December 2019                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT OS2, Strings, FileSys, INIData;

FROM SYSTEM IMPORT
    (* type *)  ADR, CARD8,
    (* proc *)  CAST;

FROM HostLists IMPORT
    (* type *)  HostList,
    (* proc *)  CreateHostList, RefreshHostList, RefreshHostList2,
                DestroyHostList, MatchHostName, FindAllAddresses;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM FileOps IMPORT
    (* type *)  DirectoryEntry, FileAttribute,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone, Exists;

FROM WINI IMPORT
    (* proc *)  INIForDomain, OpenINI, CloseINI;

FROM INIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  OpenINIFile, INIValid, CloseINIFile,
                GetStringList, NextString, CloseStringList,
                INIGetString, INIGet, ItemSize;

FROM Sockets IMPORT
    (* const*)  IFMIB_ENTRIES, NotASocket,
                AF_INET, SOCK_RAW, AF_UNSPEC, SIOSTATAT, SOCK_DGRAM,
    (* type *)  Socket, AddressFamily, iftype,
    (* proc *)  socket, soclose, ioctl, os2_ioctl;

FROM NetIF IMPORT
    (* const*)  IFNAMSIZ,
    (* type *)  ifconf, ifreq;

FROM ioctl IMPORT
    (* const*)  SIOCGIFCONF;

FROM MiscFuncs IMPORT
    (* proc*)   EVAL, ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  AddressToHostName, IPToString, NonRouteable;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction, LogTransactionL;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                CreateTask;

FROM Names IMPORT
    (* type *)  UserName, PassString, HostName, DomainName, FilenameString;

FROM LowLevel IMPORT
    (* proc *)  AddOffset, IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    Domain = POINTER TO DomainData;

    (* To save on memory, we don't record the INI file name for this    *)
    (* domain.  Instead, we recompute it each time it's needed.         *)

    DomainData = RECORD
                     name: DomainName;
                     strictnamematching: BOOLEAN;
                     DomainMailRoot: FilenameString;
                     hosts: HostList;
                 END (*RECORD*);

    (* IMPORTANT: the only domain records are those in the master       *)
    (* domain list (see below).  In every other DomainList, the "this"  *)
    (* fields are pointers to domain records in the master list.  When  *)
    (* we have to discard a DomainList, we dispose of the list elements *)
    (* but not the domain records.  An object of type Domain is never   *)
    (* discarded, except internally in this module when we need to      *)
    (* rebuild the master domain list.                                  *)

    DomainList = POINTER TO DLrecord;
    DLrecord = RECORD
                   next: DomainList;
                   this: Domain;
               END (*RECORD*);

    DomainSearchState = POINTER TO SSrecord;
    SSrecord = RECORD
                   pos: DomainList;
                   username: UserName;
                   domainname: DomainName;
               END (*RECORD*);

(************************************************************************)

VAR
    MultidomainMode, RelayEverything: BOOLEAN;

    (* Option to stop the username search at the first domain for       *)
    (* which a match is found.  If SingleMatch is FALSE then on a POP   *)
    (* login we use the password to disambiguate the result.            *)

    SingleMatch: BOOLEAN;

    (* List of all the domains we are hosting. *)

    MasterDomainList: DomainList;

    (* Critical section protection for MasterDomainList. *)

    MasterListLock: Lock;

    (* The overall root of the mail directory tree. *)

    MailRoot: FilenameString;

    (* The IP addresses belonging to the interfaces on this machine. *)

    OurIPAddresses: ARRAY [0..IFMIB_ENTRIES] OF CARDINAL;

    (* The IP address that we consider to be our main one.  It can be   *)
    (* optionally specified in Setup.  If the result from Setup is      *)
    (* zero, that means it has not been specified manually, and so we   *)
    (* have to calculate a suitable address in this module.             *)

    OurMainAddress: CARDINAL;

    (* The map from IP addresses to domains. *)

    AddressMap: ARRAY [0..IFMIB_ENTRIES] OF DomainList;

    (* Critical section protection for OurIPAddresses and *)
    (* PrincipalIPAddress and AddressMap.                 *)

    OurIPAddressesLock: Lock;

    (* A flag saying that we should shut down tasks. *)

    ShutdownInProgress: BOOLEAN;

    (* A flag saying that we want more detail written to the log file. *)

    ExtraLogging: BOOLEAN;

    (* A flag saying that the "ExtraLogging" task is running. *)

    LogTaskRunning: BOOLEAN;

    (* Log ID for transaction logging. *)

    LogID: TransactionLogID;

    (* Variables for debugging. *)

    CountLock: Lock;
    DomainCount, DRecordCount, SRecordCount: CARDINAL;

(************************************************************************)
(*                    CHECKING FOR ADDRESS MATCHES                      *)
(************************************************************************)

PROCEDURE AddressIsLocal (IPAddress: CARDINAL): BOOLEAN;

    (* Returns TRUE if the argument is the IP address of an     *)
    (* interface on the local host.                             *)

    VAR result: BOOLEAN;  j: CARDINAL;

    BEGIN
        Obtain (OurIPAddressesLock);
        result := FALSE;  j := 0;
        LOOP
            IF (j > IFMIB_ENTRIES) OR (OurIPAddresses[j] = 0) THEN
                EXIT (*LOOP*);
            ELSIF OurIPAddresses[j] = IPAddress THEN
                result := TRUE;
                EXIT (*LOOP*);
            ELSE
                INC(j);
            END (*IF*);
        END (*LOOP*);
        Release (OurIPAddressesLock);
        RETURN result;
    END AddressIsLocal;

(************************************************************************)
(*                   CHECKING FOR HOSTNAME MATCHES                      *)
(************************************************************************)

PROCEDURE IsInDomain (D: Domain;  VAR (*IN*) name: DomainName): BOOLEAN;

    (* Returns TRUE iff name is in the given domain. *)

    BEGIN

        (********************************************************)
        (*                                                      *)
        (*                 COMPROMISE VERSION                   *)
        (*                                                      *)
        (* Now we use a flag to determine whether we're going   *)
        (* to include hostname matching.                        *)
        (*                                                      *)
        (********************************************************)

        IF WildMatch (name, D^.name) THEN
            RETURN TRUE;
        ELSIF D^.strictnamematching THEN
            RETURN FALSE;
        ELSE
            RETURN MatchHostName (D^.hosts, name, ExtraLogging, LogID);
        END (*IF*);

    END IsInDomain;

(************************************************************************)

PROCEDURE DomainIsLocal (VAR (*IN*) name: DomainName;
                         VAR (*OUT*) D: Domain): BOOLEAN;

    (* If name is one of our own domains, or an acceptable alias for    *)
    (* it, identifies the domain and returns TRUE.  We assume the       *)
    (* name is in lower case.                                           *)

    VAR p: DomainList;

    BEGIN
        D := NIL;
        IF NOT RelayEverything THEN

            Obtain (MasterListLock);
            p := MasterDomainList;

            (* Arbitrary decision: an empty name matches the first domain. *)

            IF (name[0] = Nul) AND (p <> NIL) THEN
                D := p^.this;

                IF ExtraLogging THEN
                    LogTransactionL (LogID, "empty name, so we have arbitrarily chosen first domain");
                END (*IF*);

            END (*IF*);

            (* Normal case: match the first domain in which name occurs. *)

            WHILE (p <> NIL) AND (D = NIL) DO
                IF IsInDomain (p^.this, name) THEN
                    D := p^.this;
                ELSE
                    p := p^.next;
                END (*IF*);
            END (*WHILE*);
            Release (MasterListLock);
        END (*IF*);
        RETURN D <> NIL;
    END DomainIsLocal;

(************************************************************************)
(*                    INFORMATION ABOUT A USER                          *)
(************************************************************************)

PROCEDURE IsValidUsername (VAR (*IN*) name: ARRAY OF CHAR;  D: Domain;
                                       LogID: TransactionLogID): BOOLEAN;

    (* Returns TRUE iff this is the name of a mailbox in domain D.  We  *)
    (* assume that 'name' is already in lower case.  This merely does   *)
    (* a quick existence check, it does not load any user data.  We do, *)
    (* however, check the "Active" INI key to ensure that we return     *)
    (* FALSE for inactive accounts.                                     *)

    VAR hini: INIData.HINI;  result: BOOLEAN;  size: CARDINAL;
        key: ARRAY [0..6] OF CHAR;

    BEGIN
        hini := OpenDomainINI (D);

        IF NOT INIValid (hini) THEN
            result := FALSE;
        ELSE
            key[0] := Nul;
            result := ItemSize (hini, name, key, size) AND (size <> 0);
            IF result THEN

                (* If the Active key is missing, we still assume that   *)
                (* the account is active; but if it is present then it  *)
                (* must have a value of 1 to declare the account active.*)

                key := "Active";
                EVAL (INIGet (hini, name, key, result));

            END (*IF*);
            CloseDomainINI (D, hini);
        END (*IF*);

        RETURN result;

    END IsValidUsername;

(************************************************************************)

PROCEDURE SMTPAuthAllowed (VAR (*IN*) name: ARRAY OF CHAR;  D: Domain): BOOLEAN;

    (* Returns TRUE iff this user is allowed to authenticate SMTP       *)
    (* transactions using the AUTH command.                             *)

    VAR hini: INIData.HINI;  result: BOOLEAN;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        hini := OpenDomainINI(D);

        IF NOT INIValid (hini) THEN
            result := FALSE;
        ELSE
            app := "$SYS";
            key := "SMTPAuthAllUsers";
            IF NOT INIData.INIGetTrusted (hini, app, key,
                                               result, SIZE(BOOLEAN)) THEN

                (* To support someone who hasn't run Setup since this   *)
                (* feature was implemented, allow unrestricted AUTH.    *)

                result := TRUE;
            END (*IF*);
            IF NOT result THEN
                key := "SMTPAuth";
                IF NOT INIData.INIGetTrusted (hini, name, key,
                                               result, SIZE(BOOLEAN)) THEN
                    result := FALSE;
                END (*IF*);
            END (*IF*);
            CloseDomainINI (D, hini);
        END (*IF*);

        RETURN result;

    END SMTPAuthAllowed;

(************************************************************************)
(*                    MATCHING A USER TO A DOMAIN                       *)
(************************************************************************)

PROCEDURE MatchingDomains (VAR (*IN*) user: ARRAY OF CHAR;  domain: DomainName;
                                            DL: DomainList): DomainList;

    (* Creates a copy of a subset of DL, the subset being a list of     *)
    (* those domains for which domain is a valid name for the domain    *)
    (* and user is a valid username.  If domain is the null string we   *)
    (* skip the domain check and look only for username matches.  Note  *)
    (* that we don't create new domain records, we copy pointers.       *)

    VAR previous, current, result: DomainList;
        nodomain: BOOLEAN;
        D: Domain;

    BEGIN
        nodomain := domain[0] = Nul;
        previous := NIL;  result := NIL;
        WHILE DL <> NIL DO
            IF (nodomain OR IsInDomain(DL^.this, domain)) THEN
                D := DL^.this;
                IF IsValidUsername (user, D, LogID) THEN
                    NEW (current);
                    current^.this := DL^.this;
                    current^.next := NIL;
                    IF previous = NIL THEN
                        result := current;
                    ELSE
                        previous^.next := current;
                    END (*IF*);
                    previous := current;
                END (*IF*);
            END (*IF*);
            DL := DL^.next;
        END (*WHILE*);

        RETURN result;

    END MatchingDomains;

(************************************************************************)

PROCEDURE DiscardSearchState (VAR (*INOUT*) state: DomainSearchState);

    (* Disposes of the data that were created to do the search.  *)

    VAR next: DomainList;  count: CARDINAL;

    BEGIN
        count := 0;
        IF state <> NIL THEN
            WHILE state^.pos <> NIL DO
                next := state^.pos^.next;
                INC (count);
                DEALLOCATE (state^.pos, SIZE(DLrecord));
                state^.pos := next;
            END (*WHILE*);
            DEALLOCATE (state, SIZE(SSrecord));
            Obtain (CountLock);
            DEC (SRecordCount);
            Release (CountLock);
        END (*IF*);

    END DiscardSearchState;

(************************************************************************)

PROCEDURE StartDomainSearch (VAR (*IN*) user: ARRAY OF CHAR;
                                 VAR (*IN*) domainstring: DomainName;
                                      IPaddr: CARDINAL): DomainSearchState;

    (* Sets up the initial state for a search.  The DomainSearchState   *)
    (* is set up in such a way that the search will find all domains    *)
    (* for which IPaddr is a valid address AND which have user as a     *)
    (* valid username.                                                  *)

    VAR j, count: CARDINAL;  state: DomainSearchState;
        list: DomainList;
        (*message: ARRAY [0..127] OF CHAR;*)

    BEGIN
        IF MultidomainMode THEN
            state := NIL;
            j := 0;  count := 0;
            Obtain (OurIPAddressesLock);
            WHILE (state = NIL) AND (j <= IFMIB_ENTRIES)
                         AND (OurIPAddresses[j] <> 0) DO
                IF OurIPAddresses[j] = IPaddr THEN

                    list := MatchingDomains (user, domainstring, AddressMap[j]);
                    IF list <> NIL THEN
                        INC (count);
                        NEW (state);
                        state^.pos := list;
                        Strings.Assign (user, state^.username);
                        state^.domainname := domainstring;
                    ELSE
                        INC (j);
                    END (*IF*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*WHILE*);
            Release (OurIPAddressesLock);
        ELSE
            count := 1;
            NEW (state);
            state^.pos := NIL;
            Strings.Assign (user, state^.username);
            state^.domainname := domainstring;
        END (*IF*);

        IF count > 0 THEN
            Obtain (CountLock);
            INC (SRecordCount, count);
            Release (CountLock);
        END (*IF*);

        RETURN state;
    END StartDomainSearch;

(************************************************************************)

PROCEDURE NextDomain (VAR (*INOUT*) state: DomainSearchState;
                              VAR (*OUT*) D: Domain;
                              VAR (*OUT*) password: PassString): BOOLEAN;

    (* Finds one domain, returns FALSE if we have run out of possibilities. *)

    VAR success: BOOLEAN;  hini: INIData.HINI;
        next: DomainList;
        key: ARRAY [0..8] OF CHAR;

    BEGIN
        (* For single-domain mode, we use a method that automatically   *)
        (* picks the single domain while bypassing the IP address       *)
        (* matching, so as to give us a result roughly equivalent to    *)
        (* what was done in older versions of Weasel.  To force a       *)
        (* one-off check, we set state^.pos to NIL in StartDomainSearch *)
        (* and then toggle its value here.                              *)

        IF NOT MultidomainMode AND (state <> NIL) THEN
            Obtain (MasterListLock);
            state^.pos := MatchingDomains (state^.username, "", MasterDomainList);
        END (*IF*);

        success := FALSE;
        IF state <> NIL THEN
            Obtain (OurIPAddressesLock);
            WHILE (NOT success) AND (state <> NIL) AND (state^.pos <> NIL) DO

                D := state^.pos^.this;
                success := (D <> NIL) AND
                                  ((state^.domainname[0] = Nul)
                                    OR IsInDomain (D, state^.domainname));
                IF success THEN
                    hini := OpenDomainINI (D);
                    key := "Password";
                    success := INIValid(hini)
                                 AND INIGetString (hini, state^.username,
                                                    key, password);
                    IF success THEN

                        (* If the Active key is missing, we still assume that   *)
                        (* the account is active; but if it is present then it  *)
                        (* must have a value of 1 to declare the account active.*)

                        key := "Active";
                        EVAL (INIGet (hini, state^.username, key, success));
                    END (*IF*);

                    CloseDomainINI (D, hini);
                END (*IF*);
                IF SingleMatch THEN
                    DiscardSearchState (state);
                ELSE
                    next := state^.pos^.next;
                    DEALLOCATE (state^.pos, SIZE(DLrecord));
                    state^.pos := next;
                END (*IF*);

            END (*WHILE*);
            Release (OurIPAddressesLock);

            IF NOT MultidomainMode THEN
                DEALLOCATE (state, SIZE(SSrecord));
                Obtain (CountLock);
                DEC (SRecordCount);
                Release (CountLock);
                Release (MasterListLock);
            END (*IF*);

        END (*IF*);
        RETURN success;
    END NextDomain;

(************************************************************************)

PROCEDURE EndDomainSearch (VAR (*INOUT*) state: DomainSearchState);

    (* Final tidying-up at the end of a search. *)

    BEGIN
        IF state <> NIL THEN
            DiscardSearchState (state);
        END (*IF*);
    END EndDomainSearch;

(************************************************************************)
(*                RETURNING INFORMATION ABOUT A DOMAIN                  *)
(************************************************************************)

PROCEDURE NameOfFirstDomain (VAR (*OUT*) name: DomainName);

    (* Returns the name of one of our domains. *)

    BEGIN
        Obtain (MasterListLock);
        IF MasterDomainList = NIL THEN
            name := "";
        ELSE
            NameOfDomain (MasterDomainList^.this, name);
        END (*IF*);
        Release (MasterListLock);
    END NameOfFirstDomain;

(************************************************************************)

PROCEDURE NameOfDomain (D: Domain;  VAR (*OUT*) name: DomainName);

    (* Returns the name of domain D in name. *)

    BEGIN
        IF D = NIL THEN
            name := "";
        ELSE
            name := D^.name;
        END (*IF*);
    END NameOfDomain;

(************************************************************************)

PROCEDURE MailDirectoryFor (D: Domain;  VAR (*OUT*) DirName: FilenameString);

    (* Returns the 'mail root' directory for this domain. *)

    BEGIN
        IF D = NIL THEN
            DirName := MailRoot;
        ELSE
            DirName := D^.DomainMailRoot;
        END (*IF*);
    END MailDirectoryFor;

(************************************************************************)

PROCEDURE OpenDomainINI (D: Domain): INIData.HINI;

    (* Opens the INI or TNI file for this domain, as appropriate. *)

    VAR ININame: FilenameString;

    BEGIN
        IF (D = NIL) OR NOT MultidomainMode THEN
            RETURN OpenINI();
        ELSE
            INIForDomain (D^.DomainMailRoot, ININame);
            RETURN OpenINIFile (ININame);
        END (*IF*);
    END OpenDomainINI;

(************************************************************************)

PROCEDURE CloseDomainINI (D: Domain;  hini: INIData.HINI);

    (* Closes the INI or TNI file for this domain. *)

    BEGIN
        IF (D = NIL) OR NOT MultidomainMode THEN
            CloseINI();
        ELSE
            CloseINIFile (hini);
        END (*IF*);
    END CloseDomainINI;

(************************************************************************)

PROCEDURE AppendCard (number: CARDINAL;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts number to decimal string, appends it to result. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := Strings.Length (result);
        ConvertCard (number, result, pos);
        result[pos] := CHR(0);
    END AppendCard;

(************************************************************************)

PROCEDURE LogDomainMemory;

    (* Logs information about the dynamic storage we've used. *)

    VAR message: ARRAY [0..511] OF CHAR;

    BEGIN
        Obtain (CountLock);
        message := "Domains: ";
        AppendCard (DomainCount, message);
        Strings.Append ("; D-records: ", message);
        AppendCard (DRecordCount, message);
        Strings.Append ("; S-records: ", message);
        AppendCard (SRecordCount, message);
        LogTransaction (LogID, message);
        Release (CountLock);
    END LogDomainMemory;

(************************************************************************)

PROCEDURE StatusLogUpdateTask;

    (* A separate task that adds debugging information to the   *)
    (* transaction log approximately every 1 minute while extra *)
    (* logging is enabled.                                      *)

    CONST UpdateInterval = 60*1000;      (* 60 seconds *)

    BEGIN
        LogTaskRunning := TRUE;
        WHILE NOT ShutdownInProgress DO
            Sleep (UpdateInterval);
            IF ExtraLogging THEN
                LogDomainMemory;
            END (*IF*);
        END (*WHILE*);
        LogTaskRunning := FALSE;
    END StatusLogUpdateTask;

(************************************************************************)
(*                   LOADING THE DATA FOR ONE DOMAIN                    *)
(************************************************************************)

PROCEDURE UpdateAddressMap (address: CARDINAL;  D: Domain);

    (* Records the fact that address is an address for domain D. *)
    (* We assume we already have OurIPAddressesLock.             *)

    VAR j: CARDINAL;
        DL: DomainList;

    BEGIN
        j := 0;
        LOOP
            IF j > IFMIB_ENTRIES THEN EXIT(*LOOP*)
            ELSIF OurIPAddresses[j] = 0 THEN

                (* This is a case where the "Local" list for this       *)
                (* this domain contains an IP address that does not     *)
                (* belong to this machine.  The old version of this     *)
                (* software assumed that we need to add this to our     *)
                (* list of local addresses.  Now I've changed my mind;  *)
                (* I'm deciding not to trust the "Local" list.          *)

                (*
                OurIPAddresses[j] := address;
                IF j < IFMIB_ENTRIES THEN
                    OurIPAddresses[j+1] := 0;
                END (*IF*);
                *)
                EXIT (*LOOP*);
            ELSIF OurIPAddresses[j] = address THEN
                NEW (DL);
                DL^.next := AddressMap[j];
                DL^.this := D;
                AddressMap[j] := DL;
                EXIT (*LOOP*);
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
    END UpdateAddressMap;

(************************************************************************)

PROCEDURE LoadDomainDetails (D: Domain;  LogIt: BOOLEAN);

    (* On entry we know D^.name, but all other information is missing.  *)
    (* This procedure loads the information.                            *)
    (* Assumption: We already have IPAddressesLock.                     *)

    VAR LogMessage: ARRAY [0..255] OF CHAR;
        addresslist: ARRAY [0..IFMIB_ENTRIES] OF CARDINAL;
        j: CARDINAL;
        hini: INIData.HINI;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..14] OF CHAR;

    BEGIN
        SYSapp := "$SYS";
        WITH D^ DO
            Strings.Assign (MailRoot, DomainMailRoot);
            IF name[0] = Nul THEN
                IF LogIt THEN
                    LogTransactionL (LogID, "Refreshing host list for unnamed domain");
                END (*IF*);
                key := "Local";
                RefreshHostList2 (SYSapp, key, hosts,
                                  OurIPAddresses, TRUE, LogIt);
            ELSE
                Strings.Append (name, DomainMailRoot);
                Strings.Append ("\", DomainMailRoot);
                hini := OpenDomainINI (D);
                IF INIValid(hini) THEN
                    key := "StrictMatching";
                    IF NOT INIGet (hini, SYSapp, key, strictnamematching) THEN
                        strictnamematching := FALSE;
                    END (*IF*);
                    CloseDomainINI(D, hini);
                END (*IF*);
                IF LogIt THEN
                    Strings.Assign ("Refreshing host list for domain ", LogMessage);
                    Strings.Append (name, LogMessage);
                    LogTransaction (LogID, LogMessage);
                END (*IF*);
                key := "Local";
                RefreshHostList (SYSapp, key, hosts, FALSE, LogIt);
                (*
                IF LogIt THEN
                    Strings.Assign ("Finished refreshing host list for domain ", LogMessage);
                    Strings.Append (name, LogMessage);
                    LogTransaction (LogID, LogMessage);
                END (*IF*);
                *)
            END (*IF*);
        END (*WITH*);

        (* Add this domain to the address-to-domain map.  *)

        FindAllAddresses (D^.hosts, addresslist);
        j := 0;
        WHILE (j <= IFMIB_ENTRIES) AND (addresslist[j] <> 0) DO
            UpdateAddressMap (addresslist[j], D);
            INC (j);
        END (*WHILE*);

    END LoadDomainDetails;

(************************************************************************)

PROCEDURE DiscardDomain (VAR (*INOUT*) D: Domain);

    (* Discards the data for one single domain. *)

    BEGIN
        IF D <> NIL THEN
            DestroyHostList (D^.hosts);
            DEALLOCATE (D, SIZE(DomainData));

            Obtain (CountLock);
            DEC (DRecordCount);
            Release (CountLock);

        END (*IF*);
    END DiscardDomain;

(************************************************************************)
(*            REFRESHING THE LIST OF OUR LOCAL IP ADDRESSES             *)
(************************************************************************)

PROCEDURE RefreshOurIPAddresses(): BOOLEAN;

    (* Updates the list of our local IP addresses by reading the        *)
    (* SIOCGIFCONF data.  Based on ideas given to me by Paul Ratcliffe  *)
    (* and Bob Eager.                                                   *)
    (* Returns TRUE iff one of these is an active dial-up interface.    *)

    TYPE ifreqPtr = POINTER TO ifreq;
         InterfaceKind = (loopback, internal, normal, dialup);

    VAR s: Socket;  i, total, count, ThisAddress: CARDINAL;
        ifkind: InterfaceKind;
        PrimaryAddress: ARRAY InterfaceKind OF CARDINAL;
        ifc: ifconf;
        ifr: ifreqPtr;
        buf: ARRAY [0..IFMIB_ENTRIES*SIZE(ifreq)-1] OF CHAR;
        name: ARRAY [0..IFNAMSIZ-1] OF CHAR;
        HaveDialup: BOOLEAN;

    BEGIN
        HaveDialup := FALSE;

        (* Use an ioctl call to read all the ifreq records      *)
        (* into buffer "buf", and set "total" to the number of  *)
        (* such records.                                        *)

        s := socket(AF_INET, SOCK_DGRAM, AF_UNSPEC);
        IF s = NotASocket THEN
            total := 0;
        ELSE
            ifc.ifc_len := SIZE(buf);
            ifc.ifcu_buf := ADR(buf);
            IF ioctl (s, SIOCGIFCONF, ifc, SIZE(ifc)) < 0 THEN
                total := 0;
            ELSE
                total := ifc.ifc_len DIV SIZE(ifreq);
            END (*IF*);
            soclose(s);
        END (*IF*);

        (* Now work out which of the interface records are for  *)
        (* active interfaces, and put those addresses into the  *)
        (* OurIPAddresses array.                                *)

        count := 0;
        Obtain (OurIPAddressesLock);

        FOR ifkind := loopback TO dialup DO
            PrimaryAddress[ifkind] := 0;
        END (*FOR*);

        IF total > 0 THEN

            ifr := ADR(buf);
            FOR i := 0 TO total-1 DO

                IF (count <= IFMIB_ENTRIES)
                           AND (ifr^.ifru_addr.family = AF_INET) THEN

                    Strings.Assign (ifr^.ifr_name, name);
                    ThisAddress := ifr^.ifru_addr.in_addr.addr;
                    OurIPAddresses[count] := ThisAddress;
                    INC (count);

                    IF i > 8 THEN
                        HaveDialup := TRUE;
                        ifkind := dialup;
                    ELSIF Strings.Equal (name, "lo") THEN ifkind := loopback
                    ELSIF NonRouteable(ThisAddress) THEN ifkind := internal
                    ELSE ifkind := normal
                    END (*IF*);
                    IF PrimaryAddress[ifkind] = 0 THEN
                        PrimaryAddress[ifkind] := ThisAddress;
                    END (*IF*);

                END (*IF*);

                ifr := AddOffset (ifr, SIZE(ifreq));

            END (*FOR*);

        END (* IF total > 0 *);

        (* A zero address acts as an endmarker sentinel. *)

        IF count <= IFMIB_ENTRIES THEN
            OurIPAddresses[count] := 0;
        END (*IF*);

        (* Work out which of the addresses we are going to call *)
        (* our principal interface.                             *)

        IF OurMainAddress = 0 THEN
            FOR ifkind := loopback TO dialup DO
                ThisAddress := PrimaryAddress[ifkind];
                IF ThisAddress <> 0 THEN
                    OurMainAddress := ThisAddress;
                END (*IF*);
            END (*FOR*);
        END (*IF*);

        Release (OurIPAddressesLock);

        RETURN HaveDialup;

    END RefreshOurIPAddresses;

(************************************************************************)

PROCEDURE RecomputeLocalDomainNames (VAR (*OUT*) IPAddress: CARDINAL;
                                                          LogIt: BOOLEAN);

    (* This procedure is to be called when our host name or IP address  *)
    (* might have changed, e.g. because we've just come on-line.        *)
    (* Refreshes the master domain list, and returns the IP address     *)
    (* for our primary interface.                                       *)

    BEGIN
        IF LogIt THEN
            LogTransactionL (LogID, "Refreshing the master domain list");
        END (*IF*);
        RefreshMasterDomainList (LogIt);
        IF LogIt THEN
            LogTransactionL (LogID, "Finished refreshing the master domain list");
        END (*IF*);
        Obtain (OurIPAddressesLock);
        IPAddress := OurMainAddress;
        Release (OurIPAddressesLock);
    END RecomputeLocalDomainNames;

(************************************************************************)
(*                 REFRESHING THE LIST OF ALL DOMAINS                   *)
(************************************************************************)

PROCEDURE DiscardMasterDomainList;

    (* Discards the list of all domains.  We assume we already have     *)
    (* exclusive access to the list.                                    *)

    VAR current: DomainList;

    BEGIN
        current := MasterDomainList;
        WHILE current <> NIL DO
            MasterDomainList := current^.next;
            DiscardDomain (current^.this);
            DEALLOCATE (current, SIZE(DLrecord));
            current := MasterDomainList;
        END (*WHILE*);
    END DiscardMasterDomainList;

(************************************************************************)

PROCEDURE DiscardDomainList (VAR (*INOUT*) DL: DomainList);

    (* This procedure simply discards the list structure.  It does not  *)
    (* discard the domain records themselves, since the p^.this fields  *)
    (* are simply pointers to domain records that were created and kept *)
    (* track of by someone else - i.e. those domain records still       *)
    (* belong to some other structure.                                  *)
    (* Assumption: we have exclusive access to DL.                      *)

    VAR next: DomainList;

    BEGIN
        WHILE DL <> NIL DO
            next := DL^.next;
            DEALLOCATE (DL, SIZE(DLrecord));
            DL := next;
        END (*WHILE*);
    END DiscardDomainList;

(************************************************************************)

PROCEDURE RefreshMasterDomainList (LogIt: BOOLEAN);

    (* Discards then re-creates the list of all domains, updates        *)
    (* the global variables MailRoot and RelayEverything, and updates   *)
    (* our list of IP addresses.  This procedure is to be called during *)
    (* initialisation, and subsequently whenever there is a chance that *)
    (* the INI data might have been updated.                            *)

    VAR p, previous: DomainList;  hini: INIData.HINI;  state: StringReadState;
        Name: DomainName;
        j, dcount: CARDINAL;
        OldMultidomainMode, INIPresent: BOOLEAN;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..18] OF CHAR;

    BEGIN
        EVAL(RefreshOurIPAddresses());
        hini := OpenINI();
        INIPresent := INIValid (hini);
        SYSapp := "$SYS";

        (* The RelayEverything option prevents any local users from     *)
        (* being used; all mail is sent to the relay host.              *)

        IF INIPresent THEN
            key := "RelayEverything";
            EVAL (INIGet (hini, SYSapp, key, RelayEverything));
        END (*IF*);

        (* Check whether we have multidomain mode enabled, and also     *)
        (* read the MailRoot.                                           *)

        OldMultidomainMode := MultidomainMode;
        key := "MultiDomainEnabled";
        IF NOT INIPresent OR NOT INIGet (hini, SYSapp, key,
                                                   MultidomainMode) THEN
            MultidomainMode := FALSE;
        END (*IF*);

        IF MultidomainMode <> OldMultidomainMode THEN
            LogIt := TRUE;
            IF MultidomainMode THEN
                LogTransactionL (LogID, "Multidomain mode is enabled");
            ELSE
                LogTransactionL (LogID, "Multidomain mode has been disabled");
            END (*IF*);
        END (*IF*);

        key := "MailRoot";
        IF NOT INIPresent OR NOT INIGetString (hini, SYSapp, key, MailRoot) THEN
            MailRoot := ".\";
        END (*IF*);

        key := "SingleMatch";
        IF NOT INIPresent OR NOT INIGet (hini, SYSapp, key, SingleMatch) THEN
            SingleMatch := FALSE;
        END (*IF*);

        previous := NIL;
        Obtain (MasterListLock);
        DiscardMasterDomainList;
        dcount := 0;

        IF MultidomainMode THEN

            (* Load the list of domain names from the INI file. *)

            IF INIPresent THEN
                key := "Domains";
                GetStringList (hini, SYSapp, key, state);
                LOOP
                    NextString (state, Name);
                    IF Name[0] = Nul THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                    NEW (p);
                    p^.next := NIL;
                    INC (dcount);
                    NEW (p^.this);
                    p^.this^.name := Name;
                    p^.this^.DomainMailRoot := '';
                    p^.this^.hosts := CreateHostList(Name, TRUE);
                    IF previous = NIL THEN
                        MasterDomainList := p;
                    ELSE
                        previous^.next := p;
                    END (*IF*);
                    previous := p;
                END (*LOOP*);
                CloseStringList (state);
            END (*IF*);

        ELSE

            (* Single-domain mode, create one single domain. *)

            dcount := 1;
            NEW (p);
            p^.next := NIL;
            NEW (p^.this);
            p^.this^.name := '';
            p^.this^.strictnamematching := FALSE;
            p^.this^.DomainMailRoot := MailRoot;
            p^.this^.hosts := CreateHostList("local", TRUE);
            MasterDomainList := p;

        END (*IF*);

        CloseINI;

        (* Clear the Address-to-domain map. *)

        Obtain (OurIPAddressesLock);
        FOR j := 0 TO IFMIB_ENTRIES DO
            DiscardDomainList (AddressMap[j]);
        END (*FOR*);

        (* We have the names of all domains, but no other information   *)
        (* about them.  Now go through the list filling in the details. *)

        p := MasterDomainList;
        WHILE p <> NIL DO
            LoadDomainDetails (p^.this, LogIt);
            p := p^.next;
        END (*WHILE*);
        Release (OurIPAddressesLock);

        (* Update the count of domains kept for debugging. *)

        Obtain (CountLock);
        DomainCount := dcount;
        INC (DRecordCount, dcount);
        Release (CountLock);

        Release (MasterListLock);

    END RefreshMasterDomainList;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE SetPrincipalIPAddress (address: CARDINAL);

    (* Specifies which address is to be considered out principal IP     *)
    (* address, in case we have multiple interfaces.  If address = 0,   *)
    (* we allow the address to be calculated internally.                *)

    BEGIN
        Obtain (OurIPAddressesLock);
        OurMainAddress := address;
        Release (OurIPAddressesLock);
    END SetPrincipalIPAddress;

(************************************************************************)

PROCEDURE ClearMailboxLocks;

    (* Clears the POP lock in every mailbox, in case it was left set    *)
    (* by an improper shutdown.                                         *)

    VAR p: DomainList;
        basedir, searchname, filename: FilenameString;
        OK, dummy: BOOLEAN;
        D: DirectoryEntry;

    BEGIN
        Obtain (MasterListLock);
        p := MasterDomainList;
        WHILE p <> NIL DO

            (* Check all users for one domain. *)

            basedir := p^.this^.DomainMailRoot;
            searchname := basedir;
            Strings.Append ("*", searchname);
            OK := FirstDirEntry (searchname, TRUE, TRUE, D);
            WHILE OK DO
                IF directory IN D.attr THEN
                    filename := basedir;
                    Strings.Append (D.name, filename);
                    Strings.Append ('/', filename);
                    Strings.Append (LockFileName, filename);
                    FileSys.Remove (filename, dummy);
                END (*IF*);
                OK := NextDirEntry (D);
            END (*WHILE*);
            DirSearchDone (D);

            (* Move on to next domain. *)

            p := p^.next;
        END (*WHILE*);

        Release (MasterListLock);

    END ClearMailboxLocks;

(************************************************************************)
(*                              EXTRA LOGGING                           *)
(************************************************************************)

CONST NILID = CAST(TransactionLogID, NIL);

PROCEDURE EnableDomainExtraLogging (enable: BOOLEAN);

    (* Enables the option of putting extra detail into the log file. *)

    BEGIN
        IF enable <> ExtraLogging THEN
            IF LogID = NILID THEN
                LogID := CreateLogID (WCtx, "Domains");
            END (*IF*);
            ExtraLogging := enable;
            IF ExtraLogging THEN
                LogTransactionL (LogID, "Extra logging is enabled");
            ELSE
                LogTransactionL (LogID, "Extra logging is disabled");
            END (*IF*);
        END (*IF*);
        IF ExtraLogging AND NOT LogTaskRunning THEN
            EVAL(CreateTask (StatusLogUpdateTask, 2, "status"));
        END (*IF*);
    END EnableDomainExtraLogging;

(************************************************************************)

BEGIN
    ShutdownInProgress := FALSE;
    LogTaskRunning := FALSE;
    LogID := NILID;
    ExtraLogging := FALSE;
    MultidomainMode := FALSE;
    RelayEverything := FALSE;
    CreateLock (CountLock);
    DomainCount := 0;
    DRecordCount := 0;
    SRecordCount := 0;
    CreateLock (OurIPAddressesLock);
    OurIPAddresses[0] := 0;
    OurMainAddress := 0;
    CreateLock (MasterListLock);
    MasterDomainList := NIL;
    SingleMatch := FALSE;
FINALLY
    ShutdownInProgress := TRUE;
    DestroyLock (MasterListLock);
    DestroyLock (OurIPAddressesLock);
    DestroyLock (CountLock);
    IF LogID <> NILID THEN
        DiscardLogID (LogID);
    END (*IF*);
END Domains.

