(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE SMTPData;

        (********************************************************)
        (*                                                      *)
        (*      Part of the SMTP server - files the mail        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            27 April 1998                   *)
        (*  Last edited:        28 February 2020                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CARD8, CARD16, ADDRESS, CAST, ADR, LOC;

IMPORT WV, Strings, OS2, FileSys, INIData;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL, AddOffset;

FROM NetDB IMPORT
    (* proc *)  gethostbyname;

FROM Sockets IMPORT
    (* type *)  Socket;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Watchdog IMPORT
    (* type *)  WatchdogID,
    (* proc *)  KickWatchdog;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenOldFile, OpenAtEnd, OpenNewFile1, CloseFile,
                StartPosition, CurrentPosition, SetPosition, ReadLine, ReadRaw,
                WriteRaw, FWriteChar, FWriteString, FWriteCard, FWriteLn,
                DeleteFile;

FROM Names IMPORT
    (* type *)  UserName, HostName, DomainName,
                FilenameIndex, FilenameString, PathString;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  DomainIsLocal, NameOfFirstDomain;

FROM Hosts IMPORT
    (* proc *)  CheckHost, OnBlacklist, BannedHost;

FROM SPF IMPORT
    (* const*)  SPF_none,
    (* type *)  SPFresult,
    (* proc *)  DoSPFLookup, SPFresultToString;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SocketOf, Getch, GetRaw, SendLine, FlushOutput;

FROM MXCheck IMPORT
    (* proc *)  DoMXLookup;

FROM MyClock IMPORT
    (* proc *)  CurrentDateAndTime, AppendTimeString, CurrentTimeToString;

FROM WINI IMPORT
    (* proc *)  OpenINI, CloseINI;

FROM INIData IMPORT
    (* proc *)  INIGet, INIGetString, INIPut, INIPutString;

FROM MiscFuncs IMPORT
    (* type *)  LocArrayPointer, CharArrayPointer,
    (* proc *)  StringMatch;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString, AddressToHostName, NonRouteable, NameIsNumeric;

FROM SMTPLogin IMPORT
    (* proc *)  PostmasterCheck;

FROM Delivery IMPORT
    (* type *)  CombinedRecipientList, ListOfRecipients,
    (* proc *)  CreateCombinedRecipientList, DiscardCombinedRecipientList,
                ClearCombinedRecipientList, EmptyRecipientList,
                AddToLocalList, AddToRelayList, WriteRecipientList,
                ChooseIncomingFileDirectory, GetOurHostName,
                AddRecipient, CopyToRecipients, SortByFilter;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  GetLogPrefix, LogTransaction, LogTransactionL;

FROM SplitScreen IMPORT
    (* proc *)  ReleaseScreen, RegainScreen;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
    EmptyReversePathMarker = CHR(1);

    (* Feature not yet implemented.  This will later become a variable. *)

    ReceivedOffset = 0;

TYPE
    ChunkingState = RECORD
                        cid: ChanId;
                        HaveCR, HaveCRLF: BOOLEAN;
                    END (*RECORD*);

    (* An ItemDescriptor record keeps track of the information needed   *)
    (* to send one item of mail.  The fields are                        *)
    (*   RealIPAddr        the sender's real IP address as determined   *)
    (*                        from the socket binding                   *)
    (*   RealName          the sender's hostname as determined by       *)
    (*                        nameserver lookup                         *)
    (*   HELOname          the sender's hostname as supplied in the     *)
    (*                        HELO command                              *)
    (*   fromdomain        domain in the MAIL FROM command              *)
    (*   OurHostname       the hostname of this server                  *)
    (*   TempName          name of a file where the incoming item is    *)
    (*                       stored before being distributed to all     *)
    (*                       recipients.                                *)
    (*   chunkstate        state of the current CHUNKING operation      *)
    (*   offset            effective starting point of the file if it   *)
    (*                       has to be relayed.                         *)
    (*   charcount         bytes received, used for logging             *)
    (*   LogID             ID used for transaction logging              *)
    (*   watchID           watchdog handle                              *)
    (*   returnpath        path supplied by MAIL FROM:                  *)
    (*   firstrecipient    as supplied in first RCPT TO:                *)
    (*   recipientcount    number of RCPT TO: recipients                *)
    (*   Recipients        list of destination mailboxes                *)
    (*   SPFans1           result of HELO SPF check                     *)
    (*   SPFans2           result of MAIL FROM SPF check                *)
    (*   whitelisted       TRUE for a whitelisted sender                *)
    (*   RelayAllowed      TRUE iff the sending host is one that is     *)
    (*                       allowed to relay through us, and allowed   *)
    (*                       access to non-public aliases.              *)
    (*   dataOK            The data so far received has been acceptable *)
    (*   postmasterOK      The sender domain (in returnpath) has a      *)
    (*                          valid postmaster account.               *)
    (*   softfail          The postmaster check resulted in a           *)
    (*                          "try again later" response              *)
    (*   SkipFiltering     TRUE iff we want to bypass filtering.        *)
    (*   SkipFiltering0    The value of SkipFiltering after stage 0.    *)

    ItemDescriptor = POINTER TO Item;
    Item = RECORD
               RealIPAddr: CARDINAL;
               RealName: HostName;
               HELOname: HostName;
               fromdomain: DomainName;
               OurHostname: HostName;
               LogID: TransactionLogID;
               watchID: WatchdogID;
               TempName: FilenameString;
               chunkstate: ChunkingState;
               offset: FilePos;
               charcount: CARDINAL;
               returnpath, firstrecipient: PathString;
               recipientcount: CARDINAL;
               Recipients: CombinedRecipientList;
               SPFans1, SPFans2: SPFresult;
               whitelisted, RelayAllowed: BOOLEAN;
               dataOK, postmasterOK, softfail: BOOLEAN;
               SkipFiltering0, SkipFiltering: BOOLEAN;
           END (*RECORD*);

VAR
    (* Maximum message size. *)

    MaxMessageSize: CARDINAL;
    MaxMessageSizeLock: CARDINAL;

    (* NextName is a string used in generating unique file names. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

    (* Scripts to run as filters, and a critical section protection lock. *)

    FilterProg: ARRAY [0..4] OF FilenameString;
    FilterProgLock: Lock;

    (* Lock to ensure that only one thread at a time can execute the    *)
    (* filter script.  This is to cover the risk that people write      *)
    (* filters that aren't reentrant.  However, we only use this lock   *)
    (* if SerialiseFilters is TRUE.                                     *)

    SerialiseFilters: BOOLEAN;
    FilterAccess: Lock;

    (* Critical section protection on the mail item log. *)

    LogFileLock: Lock;

    (* "Enable" flag for the logging. *)

    LogSMTPItems: BOOLEAN;

    (* Name of the SMTP (incoming) log file. *)

    SMTPLogName: FilenameString;

    (* Flags to say that we should reject mail if a DNS or rDNS,        *)
    (* respectively, on the client address fails.                       *)

    CheckNoDNS, CheckNoRDNS: BOOLEAN;

    (* Flag to say that we should apply "unacceptable host" checks to   *)
    (* the domain in the MAIL FROM command.                             *)

    MAILFROMcheck: BOOLEAN;

    (* Flag to enable SPF lookups.  *)

    SPFenabled: BOOLEAN;

    (* What to do about postmaster check failures. *)

    pmchecklevel: (disabled, marksuspectfiles, blockpmfailures);

    (* A useful "constant". *)

    CRLF: ARRAY [0..1] OF CHAR;

(************************************************************************)
(*                KEEPING TRACK OF THE ITEM INFORMATION                 *)
(************************************************************************)

PROCEDURE CreateItemDescriptor (SB: SBuffer;  ClientIPAddress: CARDINAL;
                                 ID: TransactionLogID;  watchid: WatchdogID;
                                  MayRelay, OnWhitelist: BOOLEAN): ItemDescriptor;

    (* Creates a descriptor for a new mail item.  Returns NIL if we     *)
    (* want to reject the client because of an rDNS failure.            *)

    VAR result: ItemDescriptor;
        ClientName: HostName;

    BEGIN
        IF (NOT AddressToHostName (ClientIPAddress, ClientName))
                            AND CheckNoRDNS
                             AND NOT NonRouteable (ClientIPAddress) THEN
            RETURN NIL;
        END (*IF*);

        NEW (result);
        WITH result^ DO
            RealIPAddr := ClientIPAddress;
            RealName := ClientName;
            LogID := ID;
            watchID := watchid;
            HELOname := "";
            fromdomain := "";
            OurHostname := "localhost";      (* fallback default *)
            GetOurHostName (SocketOf(SB), OurHostname);
            TempName := "";
            chunkstate.cid := NoSuchChannel;
            firstrecipient := "";
            recipientcount := 0;
            Recipients := CreateCombinedRecipientList();
            SPFans1 := SPF_none;
            SPFans2 := SPF_none;
            RelayAllowed := MayRelay;
            whitelisted := OnWhitelist;
            dataOK := TRUE;
            postmasterOK := TRUE;
            softfail := FALSE;
            SkipFiltering0 := FALSE;
            SkipFiltering := FALSE;
        END (*WITH*);
        ResetReturnPath (result);
        RETURN result;
    END CreateItemDescriptor;

(************************************************************************)

PROCEDURE ResetReturnPath (desc: ItemDescriptor);

    (* Resets the sender e-mail address, i.e. the address supplied in   *)
    (* the MAIL FROM command.                                           *)

    BEGIN
        WITH desc^ DO
            returnpath[0] := EmptyReversePathMarker;
            returnpath[1] := Nul;
            SkipFiltering := SkipFiltering0;
            dataOK := TRUE;
            postmasterOK := FALSE;
            softfail := FALSE;
        END (*WITH*);
    END ResetReturnPath;

(************************************************************************)

PROCEDURE ParsePathString (path: ARRAY OF CHAR;
                            VAR (*OUT*) user: UserName;
                            VAR (*OUT*) domain: HostName);  FORWARD;

(************************************************************************)

PROCEDURE ResetItemDescriptor (desc: ItemDescriptor;
                               ReturnPath: ARRAY OF CHAR);

    (* Discards information related to sender and receivers, deletes    *)
    (* message file if one has been created, and sets a new return path.*)

    VAR user: UserName;  dummy: BOOLEAN;

    BEGIN
        IF desc <> NIL THEN
            IF desc^.TempName[0] <> Nul THEN
                FileSys.Remove (desc^.TempName, dummy);
                desc^.TempName[0] := Nul;
            END (*IF*);
            Strings.Assign (ReturnPath, desc^.returnpath);
            ParsePathString (desc^.returnpath, user, desc^.fromdomain);
            ClearCombinedRecipientList (desc^.Recipients);
            desc^.firstrecipient := "";
            desc^.recipientcount := 0;
            desc^.dataOK := TRUE;
            desc^.postmasterOK := FALSE;
            desc^.softfail := FALSE;
            desc^.SkipFiltering := desc^.SkipFiltering0;
            desc^.chunkstate.cid := NoSuchChannel;
        END (*IF*);
    END ResetItemDescriptor;

(************************************************************************)

PROCEDURE DiscardItemDescriptor (VAR (*INOUT*) desc: ItemDescriptor);

    (* Destroys the descriptor, and deletes the message file if one     *)
    (* has been created.                                                *)

    BEGIN
        IF desc <> NIL THEN
            ResetItemDescriptor (desc, "");
            DiscardCombinedRecipientList (desc^.Recipients);
            DEALLOCATE (desc, SIZE(Item));
        END (*IF*);
    END DiscardItemDescriptor;

(************************************************************************)
(*                          HELO/EHLO PROCESSING                        *)
(************************************************************************)

PROCEDURE SPFcheck (desc: ItemDescriptor;  user: ARRAY OF CHAR;
                                VAR (*IN*) domain: DomainName): SPFresult;

    (* Does an SPF check to see whether ipaddr is a valid sender for    *)
    (* the given domain.  The actual SPF string, which may be up to     *)
    (* 450 characters in length, is returned in case the caller wants   *)
    (* to record it.                                                    *)

    VAR SPFstring, message: ARRAY [0..512] OF CHAR;
        IPstring: ARRAY [0..19] OF CHAR;
        status: SPFresult;

    BEGIN
        message := "[spf] Checking address ";
        IPToString (desc^.RealIPAddr, TRUE, IPstring);
        Strings.Append (IPstring, message);
        Strings.Append (", domain ", message);
        Strings.Append (domain, message);
        LogTransaction (desc^.LogID, message);

        status := DoSPFLookup (desc^.RealIPAddr, domain,
                                 desc^.HELOname, user, domain, SPFstring);

        message := "[spf] SPF string = ";
        Strings.Append (SPFstring, message);
        LogTransaction (desc^.LogID, message);
        message := "[spf] SPF result = ";
        SPFresultToString (status, SPFstring);
        Strings.Append (SPFstring, message);
        LogTransaction (desc^.LogID, message);

        RETURN status;

    END SPFcheck;

(************************************************************************)

PROCEDURE SetClaimedSendingHost (desc: ItemDescriptor;
                                 VAR (*IN*) ClaimedName: HostName): BOOLEAN;

    (* ClaimedName is the sending host's name as supplied in the HELO   *)
    (* command.  Returns TRUE if name is acceptable.                    *)

    BEGIN
        Strings.Assign (ClaimedName, desc^.HELOname);
        IF desc^.whitelisted THEN
            RETURN TRUE;
        ELSIF BannedHost(ClaimedName, desc^.LogID, desc^.watchID) THEN
            RETURN FALSE;
        ELSIF SPFenabled THEN
            desc^.SPFans1 := SPFcheck (desc, "postmaster", ClaimedName);
            RETURN (desc^.SPFans1 <> SPF_fail);
        END (*IF*);
        RETURN TRUE;
    END SetClaimedSendingHost;

(************************************************************************)
(*                         RCPT TO: PROCESSING                          *)
(************************************************************************)

PROCEDURE ParsePathString (path: ARRAY OF CHAR;
                            VAR (*OUT*) user: UserName;
                            VAR (*OUT*) domain: HostName);

    (* Extracts the user and domain from a path string, i.e. the sort   *)
    (* of string that is used in the MAIL FROM and RCPT TO commands.    *)

    VAR j, k, pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        j := 0;  k := Strings.Length(path);

        (* Our goal is to increase j and decrease k, as appropriate *)
        (* so that the address we want is path[j..k-1].             *)

        (* Skip leading and trailing spaces. *)

        WHILE (j <= HIGH(path)) AND (path[j] = ' ') DO
            INC (j);
        END (*WHILE*);

        WHILE (k > 0) AND (path[k-1] = ' ') DO
            DEC (k);
        END (*WHILE*);

        (* Remove the enclosing <>.  Because of a possible AUTH     *)
        (* argument in the MAIL FROM string, the '>' we want is     *)
        (* not necessarily right at the end of the string.          *)

        IF (j <= HIGH(path)) AND (path[j] = '<') THEN
            INC (j);
            Strings.FindNext ('>', path, j, found, pos);
            IF found AND (pos > 0) AND (pos <= k) THEN
                k := pos;
            END (*IF*);
        END (*IF*);

        (* There shouldn't be any more leading and trailing spaces, *)
        (* but I've been told of a case where space followed '<'.   *)

        WHILE (j <= HIGH(path)) AND (path[j] = ' ') DO
            INC (j);
        END (*WHILE*);

        WHILE (k > 0) AND (path[k-1] = ' ') DO
            DEC (k);
        END (*WHILE*);

        (* Skip everything up to ':', if present. *)

        Strings.FindNext (':', path, j, found, pos);
        IF found THEN
            j := pos+1;
        END (*IF*);

        (* Now we're looking at path[j..k-1].  Look for an '@'. *)

        Strings.FindNext ('@', path, j, found, pos);
        IF NOT found THEN
            pos := k;
        END (*IF*);

        (* Username is path[j..pos-1]. *)

        IF pos > j THEN
            Strings.Extract (path, j, pos-j, user);
        ELSE
            user[0] := Nul;
        END (*IF*);

        (* Domain is path[pos+1..k-1]. *)

        IF k > pos+1 THEN
            Strings.Extract (path, pos+1, k-pos-1, domain);
        ELSE
            domain[0] := Nul;
        END (*IF*);

    END ParsePathString;

(************************************************************************)

PROCEDURE ProcessRCPTAddress (desc: ItemDescriptor;
                                 VAR (*IN*) ToAddress: ARRAY OF CHAR;
                                 VAR (*OUT*) user: UserName;
                                 VAR (*OUT*) domainname: HostName);

    (* Takes note of the destination address as supplied in a RCPT TO:  *)
    (* command, also decomposes it into user and domain components.     *)

    BEGIN
        WITH desc^ DO
            IF recipientcount = 0 THEN
                Strings.Assign (ToAddress, firstrecipient);
            END (*IF*);
            INC (recipientcount);
        END (*WITH*);
        ParsePathString (ToAddress, user, domainname);
    END ProcessRCPTAddress;

(************************************************************************)
(*                        MAIL FROM: PROCESSING                         *)
(*  The "from" address has already been stored, in ResetItemDescriptor  *)
(************************************************************************)

PROCEDURE FromAddressAcceptable (desc: ItemDescriptor;  S: Socket;
                               VAR (*OUT*) TempFailure: BOOLEAN): BOOLEAN;

    (* Returns TRUE if we're satisfied with the sender's address as     *)
    (* supplied in MAIL FROM.  If we're not satisfied, we clear that    *)
    (* address as well as returning FALSE.  A result of FALSE with      *)
    (* TempFailure also FALSE means a soft failure, such that the       *)
    (* address might become acceptable on a future attempt.             *)

    (* This procedure is not called if the client is whitelisted.       *)

    CONST max = 32;

    VAR user: UserName;
        SenderDomain: Domain;
        E1, E2, OK, IsBanned, whitelisted, MayRelay, MXOK: BOOLEAN;
        address: ARRAY [0..max] OF CARDINAL;
        message: ARRAY [0..127] OF CHAR;
        domain, OurDomainName: DomainName;

    BEGIN
        IF desc^.whitelisted THEN
            RETURN TRUE;
        END (*IF*);

        TempFailure := FALSE;
        ParsePathString (desc^.returnpath, user, domain);
        desc^.fromdomain := domain;
        E1 := user[0] = Nul;
        E2 := domain[0] = Nul;
        desc^.postmasterOK := TRUE;

        (* The SMTP standard says that a completely empty FROM address  *)
        (* is acceptable.  (In which case all of the following checks   *)
        (* are redundant.)  Otherwise, both user and domain must be     *)
        (* nonempty.                                                    *)

        IF E1 AND E2 THEN
            RETURN TRUE;
        END (*IF*);

        OK := (E1 = E2);

        (* Check whether the claimed domain is a nonexistent domain. *)

        MXOK := DoMXLookup (domain, address) = 0;
        IF CheckNoDNS AND ((NOT MXOK) OR (address[0] = 0)) THEN
            RETURN FALSE;
        END (*IF*);

        (* Skip the next couple of checks if the domain matches the     *)
        (* HELO name, because in that case we have already done those   *)
        (* checks.                                                      *)

        IF NOT StringMatch(domain, desc^.HELOname) THEN

            KickWatchdog (desc^.watchID);

            (* Do the "banned hosts" checks if the  *)
            (* MAILFROMcheck option has been set.   *)

            IF OK AND MAILFROMcheck THEN
                OK := NOT BannedHost(domain, desc^.LogID, desc^.watchID);

                (* To avoid excessive time delays, we should only look  *)
                (* at the primary MX host for this domain.              *)

                IF OK AND MXOK THEN
                    IF address[0] <> 0 THEN
                        KickWatchdog (desc^.watchID);
                        CheckHost (address[0], IsBanned, whitelisted, MayRelay);
                        IF IsBanned THEN
                            OK := FALSE;
                        ELSIF NOT MayRelay THEN
                            KickWatchdog (desc^.watchID);
                            OK := NOT OnBlacklist (address[0], desc^.LogID, desc^.watchID, message);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*IF*);

            (* Now the SPF check.  *)

            IF SPFenabled THEN
                IF OK THEN
                    KickWatchdog (desc^.watchID);
                    desc^.SPFans2 := SPFcheck (desc, user, domain);
                    OK := desc^.SPFans2 <> SPF_fail;
                ELSE
                    desc^.SPFans2 := SPF_none;
                END (*IF*);
            END (*IF*);

        END (*IF*);

        (* Now the postmaster check, unless it's disabled.  We give an  *)
        (* exemption to a completely empty FROM address and to          *)
        (* whitelisted clients, both cases handled earlier in this      *)
        (* procedure.  We also exempt local domains, for obvious        *)
        (* reasons, and the username "postmaster".                      *)

        IF OK AND (pmchecklevel <> disabled)
                AND NOT StringMatch (user, "postmaster")
                    AND NOT DomainIsLocal (domain, SenderDomain) THEN
            NameOfFirstDomain (OurDomainName);
            IF OurDomainName[0] = Nul THEN
                Strings.Assign (desc^.OurHostname, OurDomainName);
            END (*IF*);
            KickWatchdog (desc^.watchID);
            desc^.postmasterOK := PostmasterCheck (domain, desc^.OurHostname,
                                     OurDomainName, desc^.LogID,
                                      desc^.watchID, TempFailure);
            desc^.softfail := TempFailure;
            IF TempFailure THEN
                LogTransactionL (desc^.LogID, "[pc]Soft failure of postmaster check");
            ELSIF NOT desc^.postmasterOK THEN
                LogTransactionL (desc^.LogID, "[pc]Failed postmaster check");
            END (*IF*);
            IF pmchecklevel = blockpmfailures THEN
                OK := desc^.postmasterOK;
            END (*IF*);
        END (*IF*);

        IF NOT OK THEN
            desc^.returnpath[0] := EmptyReversePathMarker;
        END (*IF*);
        RETURN OK;

    END FromAddressAcceptable;

(************************************************************************)

PROCEDURE SenderNotSpecified (desc: ItemDescriptor): BOOLEAN;

    (* Returns TRUE iff the reverse path is empty. *)

    BEGIN
        RETURN desc^.returnpath[0] = EmptyReversePathMarker;
    END SenderNotSpecified;

(************************************************************************)

PROCEDURE NoRecipients (desc: ItemDescriptor): BOOLEAN;

    (* Returns TRUE iff the list of recipients is empty. *)

    BEGIN
        RETURN EmptyRecipientList (desc^.Recipients);
    END NoRecipients;

(************************************************************************)
(*                   UPDATING LISTS OF MAIL RECIPIENTS                  *)
(************************************************************************)

(************************************************************************)
(*  NOTE: Procedures AddLocalRecipient and AddRelayRecipient are the    *)
(*  only places where the caller (in module SMTPCommands) needs to make *)
(*  a local/remote decision.                                            *)
(************************************************************************)

PROCEDURE AddLocalRecipient (desc: ItemDescriptor;  D: Domain;
                           VAR (*IN*) name, host: ARRAY OF CHAR): BOOLEAN;

    (* Adds one local recipient to the list of recipients. *)
    (* Returns FALSE if it's not possible.                 *)

    BEGIN
        RETURN AddToLocalList (desc^.Recipients, name, host, D,
                               desc^.RelayAllowed, FALSE, TRUE, desc^.LogID);
    END AddLocalRecipient;

(************************************************************************)

PROCEDURE AddRelayRecipient (desc: ItemDescriptor;
                             VAR (*IN*) name: ARRAY OF CHAR);

    (* Adds one non-local recipient to the list of recipients. *)

    BEGIN
        AddToRelayList (desc^.Recipients, name, TRUE);
    END AddRelayRecipient;

(************************************************************************)
(*                            ITEM LOGGING                              *)
(************************************************************************)

PROCEDURE WriteLogItem (desc: ItemDescriptor);

    (* Writes the summary for this item to the user log. *)

    VAR cid: ChanId;  datetime: ARRAY [0..31] OF CHAR;

    BEGIN
        Obtain (LogFileLock);
        cid := OpenAtEnd (SMTPLogName);
        CurrentTimeToString (datetime);
        FWriteString (cid, datetime);
        FWriteString (cid, " ");
        FWriteString (cid, desc^.RealName);
        FWriteCard (cid, desc^.charcount, 10);
        FWriteString (cid, " ");
        WriteRecipientList (cid, desc^.Recipients, FALSE, FALSE);
        FWriteLn (cid);
        CloseFile (cid);
        Release (LogFileLock);
    END WriteLogItem;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                IF N = 0 THEN
                    NextName := "00000000";
                ELSE
                    NextName[N] := '0';
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (NextNameLock);
        Strings.Assign (NextName, name);
        Increment (7);
        Release (NextNameLock);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR UName: FilenameString;

    BEGIN
        REPEAT
            MakeUniqueName (UName);
            Strings.Assign (BaseName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenNewOutputFile (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, NewName);
            cid := OpenNewFile1 (NewName, duplication);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*               ACCEPTING AND FILING AN INCOMING MESSAGE               *)
(************************************************************************)

PROCEDURE WriteEOL (cid: ChanId);

    (* Writes CRLF to a file. *)

    BEGIN
        WriteRaw (cid, CRLF, 2);
    END WriteEOL;

(************************************************************************)

PROCEDURE NoChunksYet (desc: ItemDescriptor): BOOLEAN;

    (* Returns TRUE if there is not yet any chunked data. *)

    BEGIN
        (*RETURN desc^.firstchunk = NIL;*)
        RETURN desc^.chunkstate.cid = NoSuchChannel;
    END NoChunksYet;

(************************************************************************)

PROCEDURE AcceptOneLine (SB: SBuffer;
                             VAR (*OUT*) Buffer: ARRAY OF CHAR): CARDINAL;

    (* Receives one line of an incoming message.  Returns the number of *)
    (* characters read, or MAX(CARDINAL) for a reception failure.       *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR ch: CHAR;  success: BOOLEAN;
        pos: CARDINAL;

    BEGIN
        success := TRUE;  pos := 0;
        LOOP
            ch := Getch (SB);

            IF ch = Nul THEN

                (* Connection lost, give up. *)

                success := FALSE;  EXIT(*LOOP*);

            ELSIF ch = CR THEN

                (* Line terminator should be CRLF, but I suspect that   *)
                (* some unix software sends only LF.  To handle both    *)
                (* cases, ignore the CR and use only the LF.            *)

            ELSIF ch = LF THEN

                (* End of line. *)

                EXIT (*LOOP*);

            ELSIF pos <= HIGH(Buffer) THEN

                (* Normal case, store character.  In this version we    *)
                (* wrap a long line after 1000 characters, or 1001      *)
                (* characters if the first character is a '.'.          *)

                Buffer[pos] := ch;  INC(pos);
                IF pos = 1000 THEN
                    IF Buffer[0] <> '.' THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                ELSIF pos > 1000 THEN
                    EXIT (*LOOP*);
                END (*IF*);

            END (*IF*);

        END (*LOOP*);

        (* Make sure string is properly terminated. *)

        IF pos <= HIGH(Buffer) THEN
            Buffer[pos] := Nul;
        END (*IF*);
        IF success THEN
            RETURN pos;
        ELSE
            RETURN MAX(CARDINAL);
        END (*IF*);

    END AcceptOneLine;

(************************************************************************)

PROCEDURE InsertMessageID (cid: ChanId;  LocalHost: HostName): CARDINAL;

    (* Writes a "Message-ID" line to the file.  Returns the number of   *)
    (* characters written.                                              *)

    VAR Buffer: ARRAY [0..1023] OF CHAR;
        uname: ARRAY [0..7] OF CHAR;

    BEGIN
        Strings.Assign ("Message-ID: <", Buffer);
        AppendTimeString (Buffer);
        Strings.Append ('.', Buffer);
        MakeUniqueName (uname);
        Strings.Append (uname, Buffer);
        Strings.Append ('@', Buffer);
        Strings.Append (LocalHost, Buffer);
        Strings.Append (">", Buffer);
        WriteRaw (cid, Buffer, LENGTH(Buffer));
        WriteEOL (cid);
        RETURN LENGTH(Buffer) + 2;
    END InsertMessageID;

(************************************************************************)

PROCEDURE InsertDate (cid: ChanId;  LocalHost: HostName): CARDINAL;

    (* Writes a "Date" header line to the file.  Returns the number of  *)
    (* characters written.                                              *)

    VAR nchars: CARDINAL;  Buffer: ARRAY [0..1023] OF CHAR;

    BEGIN
        Strings.Assign ("Date: ", Buffer);
        nchars := LENGTH(Buffer);
        WriteRaw (cid, Buffer, nchars);
        CurrentDateAndTime (Buffer);
        WriteRaw (cid, Buffer, LENGTH(Buffer));
        WriteEOL (cid);
        INC (nchars, LENGTH(Buffer) + 2);

        Strings.Assign ("X-Date-Added: Date header was added by ", Buffer);
        Strings.Append (LocalHost, Buffer);
        WriteRaw (cid, Buffer, LENGTH(Buffer));
        WriteEOL (cid);
        INC (nchars, LENGTH(Buffer) + 2);

        RETURN nchars;

    END InsertDate;

(************************************************************************)

PROCEDURE KeywordMatch (kwd: ARRAY OF CHAR;
                         VAR (*IN*) Line: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff Line starts with kwd followed by ':'. *)
    (* Character case is ignored.                             *)

    VAR j: CARDINAL;  match: BOOLEAN;

    BEGIN
        j := 0;  match := TRUE;
        LOOP
            IF (j > HIGH(kwd)) OR (kwd[j] = Nul) THEN
                EXIT (*LOOP*);
            ELSIF (j > HIGH(Line)) OR (CAP(kwd[j]) <> CAP(Line[j])) THEN
                match := FALSE;
                EXIT (*LOOP*);
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
        IF match THEN
            WHILE Line[j] = ' ' DO
                INC (j);
            END (*WHILE*);
            match := Line[j] = ':';
        END (*IF*);
        RETURN match;
    END KeywordMatch;

(************************************************************************)
(*                      RECEIVING THE MESSAGE TEXT                      *)
(************************************************************************)

PROCEDURE ReceiveMessage0 (SB: SBuffer;  cid: ChanId;  LocalHost: HostName;
                             watchID: WatchdogID;  StoreIt: BOOLEAN;
                              VAR (*OUT*) FromPresent,
                                          TooManyHops: BOOLEAN): CARDINAL;

    (* Receives an incoming message, stores it to a previously opened   *)
    (* file.  We periodically signal on sem to confirm that the         *)
    (* operation has not timed out.  The returned value is a character  *)
    (* count, or MAX(CARDINAL) if the transfer failed.                  *)
    (* If StoreIt = FALSE then we don't write to the file.              *)
    (* FromPresent = TRUE iff "From:" header is present.                *)

    VAR LineBuffer: ARRAY [0..1023] OF CHAR;

    (********************************************************************)

    PROCEDURE ExtractSendingHost;

        (* Processes LineBuffer, which we have already found is a  *)
        (* Received: line, to get the "from" host address.         *)

        BEGIN
        END ExtractSendingHost;

    (********************************************************************)

    VAR EndOfMessage, MessageIDPresent, DatePresent, InHeader: BOOLEAN;
        amount, total, ReceivedCount: CARDINAL;

    BEGIN
        EndOfMessage := FALSE;
        MessageIDPresent := FALSE;
        DatePresent := FALSE;
        FromPresent := FALSE;
        InHeader := TRUE;
        ReceivedCount := 0;
        total := 0;
        REPEAT
            KickWatchdog (watchID);
            amount := AcceptOneLine (SB, LineBuffer);
            IF amount = MAX(CARDINAL) THEN
                total := amount;
            ELSE
                EndOfMessage := (LineBuffer[0] = '.') AND (LineBuffer[1] = Nul);

                (* Remark: if StoreIt is FALSE, we don't really care about  *)
                (* header line checks.                                      *)

                IF StoreIt AND InHeader THEN

                    IF EndOfMessage OR (LineBuffer[0] = Nul) THEN

                        (* We have just come to the end of the headers.  Add a  *)
                        (* "Message-ID" and/or "Date" header line if none was   *)
                        (* received.                                            *)

                        InHeader := FALSE;
                        IF NOT MessageIDPresent THEN
                            INC (total, InsertMessageID (cid, LocalHost));
                        END (*IF*);
                        IF NOT DatePresent THEN
                            INC (total, InsertDate (cid, LocalHost));
                        END (*IF*);

                    ELSIF KeywordMatch ("From", LineBuffer) THEN

                        FromPresent := TRUE;

                    ELSIF KeywordMatch ("Received", LineBuffer) THEN
                        INC (ReceivedCount);
                        IF ReceivedCount = ReceivedOffset THEN
                            ExtractSendingHost;
                        END (*IF*);
                    ELSE
                        MessageIDPresent := MessageIDPresent OR
                                         KeywordMatch ("Message-ID", LineBuffer);
                        DatePresent := MessageIDPresent OR
                                         KeywordMatch ("Date", LineBuffer);
                    END (*IF*);

                END (*IF*);

                IF StoreIt AND NOT EndOfMessage THEN
                    IF amount > 0 THEN
                        WriteRaw (cid, LineBuffer, amount);
                    END (*IF*);
                    WriteEOL (cid);
                END (*IF*);
                INC (total, amount+2);
                IF total > LimitOnMessageSize() THEN
                    (* Keep receiving message, but stop storing it. *)
                    StoreIt := FALSE;
                END (*IF*);
            END (*IF*);

        UNTIL EndOfMessage OR (total = MAX(CARDINAL));

        TooManyHops := ReceivedCount > 25;
        RETURN total;

    END ReceiveMessage0;

(************************************************************************)

PROCEDURE ReceiveMessage (SB: SBuffer;  cid: ChanId;  LocalHost: HostName;
                              watchID: WatchdogID;
                              VAR (*OUT*) FromPresent,
                                          TooManyHops: BOOLEAN): CARDINAL;

    (* Receives an incoming message, stores it to a previously opened   *)
    (* file.  We periodically signal on sem to confirm that the         *)
    (* operation has not timed out.  The returned value is a character  *)
    (* count, or MAX(CARDINAL) if the transfer failed.                  *)

    (* TooManyHops = TRUE means that the message has too many header    *)
    (* lines saying "Received:".                                        *)
    (* FromPresent = TRUE iff "From:" header is present.                *)
    (* Invariant: called only if itemdata^.dataOK = TRUE.               *)

    BEGIN
        RETURN ReceiveMessage0 (SB, cid, LocalHost, watchID, TRUE,
                                                FromPresent, TooManyHops);
    END ReceiveMessage;

(************************************************************************)

PROCEDURE SkipMessage (SB: SBuffer;  watchID: WatchdogID);

    (* Like ReceiveMessage, but the incoming data are discarded rather  *)
    (* than being written to a file.                                    *)
    (* Invariant: called only if itemdata^.dataOK = FALSE.              *)

    VAR dummy1, dummy2: BOOLEAN;  dummyhost: HostName;

    BEGIN
        EVAL (ReceiveMessage0 (SB, NoSuchChannel, dummyhost, watchID,
                                                  FALSE, dummy1, dummy2));
    END SkipMessage;

(************************************************************************)

PROCEDURE StartNewMessage (itemdata: ItemDescriptor): ChanId;

    (* Creates a temporary file whose name is recorded in itemdata, and *)
    (* inserts the first few header lines.  The number of characters    *)
    (* inserted is stored in itemdata^.charcount.                       *)

    CONST MaxPosInLine = 80;

    VAR PosInLine: CARDINAL;

    (********************************************************************)

    PROCEDURE AddString (cid: ChanId;  str: ARRAY OF CHAR);

        (* Same as FWriteString, except that we wrap to a new line      *)
        (* if PosInLine is too big.                                     *)

        VAR length: CARDINAL;

        BEGIN
            length := LENGTH(str);
            IF PosInLine + length > MaxPosInLine THEN
                FWriteLn (cid);  FWriteString (cid, ' ');
                INC (itemdata^.charcount, 3);
                PosInLine := 1;
            END (*IF*);
            FWriteString (cid, str);
            INC (PosInLine, length);
            INC (itemdata^.charcount, length);
        END AddString;

    (********************************************************************)

    VAR cid: ChanId;
        BaseName: FilenameString;
        StringBuffer: ARRAY [0..255] OF CHAR;
        success: BOOLEAN;

    BEGIN
        itemdata^.charcount := 0;

        (* Create a temporary file in the mailbox of the first recipient, *)
        (* or in the Forward directory if there are no local recipients.  *)

        ChooseIncomingFileDirectory (itemdata^.Recipients, BaseName);
        cid := OpenNewOutputFile (BaseName, ".###", itemdata^.TempName);
            success := cid <> NoSuchChannel;
            IF success THEN

                (* Create a "Return-Path:" header line. *)

                PosInLine := 0;
                AddString (cid, "Return-Path: ");
                AddString (cid, itemdata^.returnpath);
                FWriteLn (cid);
                INC (itemdata^.charcount, 2);

                (* The above parts of the file will be removed if this  *)
                (* message is relayed to another server.                *)

                itemdata^.offset := CurrentPosition(cid);

                (* Create an "Authentication-Results" header line, but only *)
                (* if we're using SPF.  The RFC for this header says that   *)
                (* this header should come ahead of any other trace field,  *)
                (* but the examples in that document do not include any     *)
                (* Return-Path header lines.  In my opinion the             *)
                (* Return-Path should be displayed only on final delivery,  *)
                (* while the Authentication-Results should be preserved     *)
                (* through all intermediate servers.                        *)

                IF SPFenabled THEN
                    PosInLine := 0;
                    AddString (cid, "Authentication-Results: ");
                    Strings.Assign (itemdata^.OurHostname, StringBuffer);
                    Strings.Append ("; ", StringBuffer);
                    AddString (cid, StringBuffer);
                    AddString (cid, "spf=");
                    SPFresultToString (itemdata^.SPFans1, StringBuffer);
                    AddString (cid, StringBuffer);
                    IF itemdata^.whitelisted THEN
                        AddString (cid, " (whitelisted)");
                    END (*IF*);
                    Strings.Append ("; ", StringBuffer);
                    AddString (cid, " smtp.helo=");
                    AddString (cid, itemdata^.HELOname);

                    IF itemdata^.SPFans2 <> itemdata^.SPFans1 THEN
                        AddString (cid, "; spf=");
                        SPFresultToString (itemdata^.SPFans2, StringBuffer);
                        AddString (cid, StringBuffer);
                    END (*IF*);
                    IF (itemdata^.fromdomain[0] <> Nul)
                            OR (itemdata^.SPFans2 <> itemdata^.SPFans1) THEN
                        AddString (cid, " smtp.mailfrom=");
                        AddString (cid, itemdata^.fromdomain);
                    END (*IF*);
                    FWriteLn (cid);
                    INC (itemdata^.charcount, 2);
                END (*IF*);

                (* Create a "Received:" header line. *)

                PosInLine := 0;
                AddString (cid, "Received: from ");
                AddString (cid, itemdata^.HELOname);
                AddString (cid, " (" );
                AddString (cid, itemdata^.RealName);
                AddString (cid, " ");
                IPToString (itemdata^.RealIPAddr, TRUE, StringBuffer);
                AddString (cid, StringBuffer);
                FWriteChar (cid, ')');  INC(PosInLine);
                INC (itemdata^.charcount);
                AddString (cid, " by " );
                AddString (cid, itemdata^.OurHostname);
                AddString (cid, " (Weasel v");
                AddString (cid, WV.version);
                AddString (cid, ")");
                IF itemdata^.recipientcount = 1 THEN
                    AddString (cid, " for ");
                    AddString (cid, itemdata^.firstrecipient);
                END (*IF*);
                AddString (cid, "; ");
                CurrentDateAndTime (StringBuffer);
                AddString (cid, StringBuffer);
                FWriteLn (cid);
                INC (itemdata^.charcount, 2);

                (* Add an X-PostmasterCheck header if needed. *)

                IF (NOT itemdata^.postmasterOK) AND (pmchecklevel = marksuspectfiles) THEN
                    IF itemdata^.softfail THEN
                        FWriteString (cid, "X-PostmasterCheck: DEFERRED");
                        INC (itemdata^.charcount, 27);
                    ELSE
                        FWriteString (cid, "X-PostmasterCheck: FAIL");
                        INC (itemdata^.charcount, 23);
                    END (*IF*);
                    FWriteLn (cid);
                    INC (itemdata^.charcount, 2);
                END (*IF*);

            END (*IF*);

        RETURN cid;

    END StartNewMessage;

(************************************************************************)

PROCEDURE AcceptMessage (SB: SBuffer;  itemdata: ItemDescriptor;
                         VAR (*OUT*) FailureReason: ARRAY OF CHAR): BOOLEAN;

    (* Receives an incoming message, stores it in a temporary file      *)
    (* whose name is recorded in itemdata.  We periodically kick the    *)
    (* watchdog to confirm that the reception has not timed out.        *)

    VAR success, dummy, TooManyHops, FromPresent: BOOLEAN;
        count: CARDINAL;
        cid: ChanId;

    BEGIN
        success := NOT NoRecipients(itemdata);
        itemdata^.dataOK := success;

        IF success THEN

            cid := StartNewMessage (itemdata);
            success := cid <> NoSuchChannel;

            IF success THEN

                (* Read the new message into the temporary file. *)

                count := ReceiveMessage (SB, cid, itemdata^.OurHostname,
                                         itemdata^.watchID, FromPresent, TooManyHops);
                success := count <> MAX(CARDINAL);
                IF success THEN
                    INC (itemdata^.charcount, count);
                END (*IF*);
                CloseFile (cid);
                KickWatchdog (itemdata^.watchID);

                IF TooManyHops THEN
                    Strings.Assign ("554 too many hops (max 25)", FailureReason);
                    success := FALSE;
                ELSIF NOT FromPresent THEN
                    Strings.Assign ("554 From: header line is missing", FailureReason);
                    success := FALSE;
                ELSE
                    IF NOT success THEN
                        Strings.Assign ("554 connection lost", FailureReason);
                        itemdata^.dataOK := FALSE;
                    ELSIF itemdata^.charcount > LimitOnMessageSize() THEN
                        Strings.Assign ("552 message size exceeds fixed maximum message size",
                                                            FailureReason);
                        success := FALSE;
                    END (*IF*);
                END (*IF*);

            ELSE
                itemdata^.dataOK := FALSE;
                Strings.Assign ("554 could not create message file", FailureReason);
                SkipMessage (SB, itemdata^.watchID);
                itemdata^.TempName[0] := Nul;
            END (*IF*);

        ELSE
            itemdata^.dataOK := FALSE;
            Strings.Assign ("554 no recipients", FailureReason);
            SkipMessage (SB, itemdata^.watchID);
        END (*IF*);

        IF NOT success AND (itemdata^.TempName[0] <> Nul) THEN
            FileSys.Remove (itemdata^.TempName, dummy);
            itemdata^.TempName[0] := Nul;
        END (*IF*);

        IF success AND LogSMTPItems THEN
            WriteLogItem (itemdata);
        END (*IF*);

        KickWatchdog (itemdata^.watchID);
        RETURN success;

    END AcceptMessage;

(************************************************************************)
(*                           CHUNKED MESSAGES                           *)
(************************************************************************)

PROCEDURE CheckHeader (cid: ChanId;
                           VAR (*OUT*) FromPresent, TooManyHops: BOOLEAN);

    (* This is the equivalent of AcceptMessage for the case of chunked  *)
    (* data.  The difference is that AcceptMessage processes the        *)
    (* message while it is being received, while with chunked data we   *)
    (* receive the entire message before calling this procedure.        *)
    (* We do some checks on the headers, but do not alter the headers   *)
    (* or touch the message body.  This means, unlike the non-chunked   *)
    (* case, that we trust the sender to have included a Message-ID.    *)

    CONST CtrlZ = CHR(26);

    VAR LineBuffer: ARRAY [0..1023] OF CHAR;

    VAR EndOfMessage, InHeader: BOOLEAN;
        ReceivedCount: CARDINAL;

    BEGIN
        FromPresent := FALSE;
        InHeader := TRUE;
        ReceivedCount := 0;
        SetPosition (cid, StartPosition(cid));
        WHILE InHeader DO
            ReadLine (cid, LineBuffer);
            EndOfMessage := LineBuffer[0] = CtrlZ;

            IF EndOfMessage OR (LineBuffer[0] = Nul) THEN
                InHeader := FALSE;

            ELSIF KeywordMatch ("From", LineBuffer) THEN

                FromPresent := TRUE;

            ELSIF KeywordMatch ("Received", LineBuffer) THEN
                INC (ReceivedCount);
            END (*IF*);

        END (*WHILE*);

        TooManyHops := ReceivedCount > 25;

    END CheckHeader;

(************************************************************************)

PROCEDURE IgnoreChunk (SB: SBuffer;  size: CARDINAL);

    (* Reads 'size' bytes from the input channel without storing them.  *)
    (* This is for the case where there has been an error but the       *)
    (* sender does not yet know that anything is wrong.                 *)

    CONST blocksize = 1024;

    VAR dummy: ARRAY [0..blocksize-1] OF CHAR;
        amount: CARDINAL;

    BEGIN
        WHILE size > 0 DO
            IF size >= blocksize THEN
                amount := blocksize;
            ELSE
                amount := size;
            END (*IF*);
            EVAL (GetRaw (SB, amount, ADR(dummy)));
            DEC (size, amount);
        END (*WHILE*);
    END IgnoreChunk;

(************************************************************************)

PROCEDURE AcceptChunk (SB: SBuffer;  itemdata: ItemDescriptor;
                         chunksize: CARDINAL;
                         finalchunk: BOOLEAN;
                         VAR (*OUT*) FailureReason: ARRAY OF CHAR): BOOLEAN;

    (* Receives one chunk of an incoming message.  The message is       *)
    (* stored in a temporary file whose name is recorded in itemdata.   *)
    (* We periodically kick the watchdog to confirm that the reception  *)
    (* has not timed out.                                               *)

    TYPE ThreeChar = ARRAY [0..2] OF CHAR;

    CONST
        CRLFdot = ThreeChar {CR, LF, '.'};
        subchunksize = 8192;

    VAR cid: ChanId;
        amount, CharsLeft, k, pos, gap: CARDINAL;
        dot: CHAR;
        success, FromPresent, TooManyHops, dummy, found, HaveCR, HaveCRLF: BOOLEAN;
        buffer: ARRAY [0..subchunksize] OF CHAR;

    BEGIN
        dot := '.';
        FailureReason[0] := Nul;
        buffer[subchunksize] := Nul;   (* guard against search overflow *)
        cid := itemdata^.chunkstate.cid;
        IF cid = NoSuchChannel THEN

            (* This is the first chunk in the message, so open a new    *)
            (* message file.                                            *)

            itemdata^.dataOK := TRUE;
            cid := StartNewMessage (itemdata);
            itemdata^.chunkstate.cid := cid;
            HaveCRLF := FALSE;  HaveCR := FALSE;

        ELSE
            HaveCRLF := itemdata^.chunkstate.HaveCRLF;
            HaveCR := itemdata^.chunkstate.HaveCR;
        END (*IF*);

        (* Receive the chunk, insert stuffed dots where needed, and     *)
        (* append it to the file.  Note that we must keep accepting     *)
        (* chunks even if there has been a previous error, but in that  *)
        (* case there is no point in storing them.                      *)

        WHILE chunksize > 0 DO
            (* Pick up next subchunk. *)

            IF chunksize >= subchunksize THEN
                amount := subchunksize;
            ELSE
                amount := chunksize;
                buffer[amount] := Nul;
            END (*IF*);
            IF GetRaw (SB, amount, ADR(buffer)) THEN
                INC (itemdata^.charcount, amount);
            ELSE
                itemdata^.dataOK := FALSE;
            END (*IF*);
            DEC (chunksize, amount);

            IF itemdata^.dataOK THEN
                (* Process this block. *)

                CharsLeft := amount;  k := 0;

                (* Special case: check for a "CR LF ." that crosses *)
                (* block boundaries.                                *)

                IF HaveCR THEN
                    IF (CharsLeft > 0) AND (buffer[0] = LF) THEN
                        WriteRaw (cid, buffer, 1);
                        k := 1;
                        DEC (CharsLeft);
                        HaveCRLF := TRUE;
                    END (*IF*);
                END (*IF*);

                IF HaveCRLF THEN
                    IF (CharsLeft > 0) AND (buffer[k] = '.') THEN
                        WriteRaw (cid, dot, 1);
                        INC (itemdata^.charcount);
                    END (*IF*);
                END (*IF*);

                (* Look at the end of the block to set the flags    *)
                (* for the next block.                              *)

                HaveCR := (amount > 0) AND (buffer[amount-1] = CR);
                HaveCRLF := (amount > 1) AND (buffer[amount-2] = CR)
                                            AND (buffer[amount-1] = LF);

                (* Now search for the pattern CRLFdot in what remains   *)
                (* of the buffer.  Note that the last character in the  *)
                (* buffer is permanently a Nul, to stop the search      *)
                (* from overrunning the buffer.                         *)

                WHILE CharsLeft > 0 DO
                    Strings.FindNext (CRLFdot, buffer, k, found, pos);
                    IF found THEN
                        gap := pos - k + 3;
                        WriteRaw (cid, buffer[k], gap);
                        INC (k, gap);
                        DEC (CharsLeft, gap);
                        WriteRaw (cid, dot, 1);
                        INC (itemdata^.charcount);
                    ELSE
                        WriteRaw (cid, buffer[k], CharsLeft);
                        CharsLeft := 0;
                    END (*IF*);
                END (*WHILE*);

            ELSE

                (* Ignore this block.  We are going to reject the   *)
                (* message, so there is no point in storing the     *)
                (* rest of it.                                      *)

            END (*IF*);

            KickWatchdog (itemdata^.watchID);

        END (*WHILE*);

        (* Save some flags for use by the next chunk. *)

        itemdata^.chunkstate.HaveCRLF := HaveCRLF;
        itemdata^.chunkstate.HaveCR := HaveCR;

        success := itemdata^.dataOK;

        IF finalchunk THEN

            (* We have now received the entire message, so do the   *)
            (* final cleaning up.                                   *)

            FromPresent := TRUE;
            TooManyHops := FALSE;
            CheckHeader (cid, FromPresent, TooManyHops);
            CloseFile (cid);
            itemdata^.chunkstate.cid := NoSuchChannel;
            KickWatchdog (itemdata^.watchID);

            IF NOT success THEN
                Strings.Assign ("554 Data transfer failed", FailureReason);
            ELSIF itemdata^.charcount > LimitOnMessageSize() THEN
                Strings.Assign ("552 message size exceeds limit on this server", FailureReason);
                success := FALSE;
            ELSE
                IF TooManyHops THEN
                    Strings.Assign ("554 too many hops (max 25)", FailureReason);
                    success := FALSE;
                ELSIF NOT FromPresent THEN
                    Strings.Assign ("554 From: header line is missing", FailureReason);
                    success := FALSE;
                END (*IF*);
            END (*IF*);

            IF success THEN
                FailureReason[0] := Nul;
                IF LogSMTPItems THEN
                    WriteLogItem (itemdata);
                END (*IF*);
            ELSE
                IF itemdata^.TempName[0] <> Nul THEN
                    FileSys.Remove (itemdata^.TempName, dummy);
                    itemdata^.TempName[0] := Nul;
                END (*IF*);
            END (*IF*);

        END (*IF*);

        RETURN success;

    END AcceptChunk;

(************************************************************************)
(*                               FILTERS                                *)
(************************************************************************)

PROCEDURE MakeRecipientListFile (stage: CARDINAL;  desc: ItemDescriptor;
                                 VAR (*OUT*) filename: FilenameString);

    (* Creates the "namefile" for a filter.  In the present version,    *)
    (* the file contents are:                                           *)
    (*    line 1:   client's IP address and hostname, in the form       *)
    (*              [nnn.nnn.nnn.nnn] hostname                          *)
    (*              with a single space between the numeric name and    *)
    (*              the textual hostname.  If the reverse DNS operation *)
    (*              fails to find a hostname, the second part is just   *)
    (*              a repeat of the first.                              *)
    (*    line 2:   the argument of the HELO or EHLO command (stage>0)  *)
    (*    line 3:   the argument of the MAIL FROM command    (stage>1)  *)
    (*    line 4:   an empty line                            (stage>2)  *)
    (*    remaining lines:  the recipients, one per line.    (stage>2)  *)
    (*                                                                  *)
    (* All of the above lines are present for a stage 3 or stage 4      *)
    (* filter.  For earlier stages, some of the lines are missing.  At  *)
    (* stage 0 we have nothing but line 1.  At stage 1 we have only     *)
    (* lines 1 and 2, and at stage 2 we have only lines 1 to 3.  The    *)
    (* reason for the empty line 4, which occurs only at stages 3 and 4,*)
    (* is to allow for changes in future versions.  If we change the    *)
    (* format of the leading lines, we can still ensure that the list   *)
    (* of recipients starts after the first blank line, making it       *)
    (* easier to update the final stage filters if the rules change.    *)
    (* Filters for earlier stages might have to change, but that is     *)
    (* inevitable given that the earlier-stage filters are relying on   *)
    (* information that might change from version to version.           *)

    VAR cid: ChanId;  BaseName: FilenameString;
        IPBuffer: ARRAY [0..20] OF CHAR;

    BEGIN
        ChooseIncomingFileDirectory (desc^.Recipients, BaseName);

        (* Make the file name reflect numeric part of the transaction   *)
        (* log ID.                                                      *)


        GetLogPrefix (desc^.LogID, IPBuffer);
        WHILE IPBuffer[1] = ' ' DO
            Strings.Delete (IPBuffer, 1, 1);
        END (*WHILE*);
        IPBuffer[0] := '.';
        cid := OpenNewOutputFile (BaseName, IPBuffer, filename);

        IF cid <> NoSuchChannel THEN

            (* Write the client IP address and hostname. *)

            IPToString (desc^.RealIPAddr, TRUE, IPBuffer);
            FWriteString (cid, IPBuffer);  FWriteChar (cid, ' ');
            FWriteString (cid, desc^.RealName);  FWriteLn (cid);

            IF stage > 0 THEN

                (* Write the HELO or EHLO name. *)

                FWriteString (cid, desc^.HELOname);  FWriteLn (cid);
                IF stage > 1 THEN

                    (* Write the MAIL FROM address. *)

                    FWriteString (cid, desc^.returnpath);  FWriteLn (cid);
                    IF stage > 2 THEN
                        FWriteLn (cid);

                        (* Write the list of recipients. *)

                        WriteRecipientList (cid, desc^.Recipients, TRUE, FALSE);
                    END (*IF*);
                END (*IF*);
            END (*IF*);

            CloseFile (cid);

        END (*IF*);
    END MakeRecipientListFile;

(************************************************************************)

PROCEDURE RebuildRecipientList (desc: ItemDescriptor;
                                filename: FilenameString);

    (* Discards the current recipients for desc^, and creates a new set *)
    (* of recipients from "filename".  The first two lines in the file  *)
    (* (if present) are skipped.  The third line is used to update      *)
    (* the "MAIL FROM" information.  Then we skip everything up to and  *)
    (* including the first empty line in the file.  All remaining lines *)
    (* if any are used to rebuild the recipient list.                   *)

    CONST CtrlZ = CHR(26);

    VAR cid: ChanId;  MoreToGo: BOOLEAN;
        name: UserName;

    BEGIN
        IF filename[0] <> Nul THEN
            ClearCombinedRecipientList (desc^.Recipients);
            cid := OpenOldFile (filename, FALSE, FALSE);
            IF cid <> NoSuchChannel THEN

                (* Read header section. *)

                ReadLine (cid, name);     (* line 1 *)
                ReadLine (cid, name);     (* line 2 *)
                ReadLine (cid, name);     (* line 3 *)
                IF name[0] <> CtrlZ THEN
                    Strings.Assign (name, desc^.returnpath);
                END (*IF*);

                (* Skip to first empty line. *)

                REPEAT
                    ReadLine (cid, name);
                    MoreToGo := name[0] <> CtrlZ;
                UNTIL (NOT MoreToGo) OR (name[0] = Nul);

                (* Finally, the updated list of recipients. *)

                WHILE MoreToGo DO
                    ReadLine (cid, name);
                    IF name[0] = CtrlZ THEN
                        MoreToGo := FALSE;
                    ELSIF name[0] <> Nul THEN
                        AddRecipient (desc^.Recipients, name,
                                      desc^.RelayAllowed, TRUE, desc^.LogID);
                    END (*IF*);
                END (*WHILE*);

                CloseFile (cid);

            END (*IF*);
        END (*IF*);

    END RebuildRecipientList;

(************************************************************************)

PROCEDURE ReadFailureMessage (filename: FilenameString;
                              VAR (*OUT*) Message: ARRAY OF CHAR);

    (* Returns the first line of the file.  The remainder of the file   *)
    (* is ignored.                                                      *)

    VAR cid: ChanId;

    BEGIN
        Message[0] := Nul;
        cid := OpenOldFile (filename, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            ReadLine (cid, Message);
            CloseFile (cid);
        END (*IF*);
    END ReadFailureMessage;

(************************************************************************)
(*  The following two procedures are no longer used, but are retained   *)
(*  in case I need to go back to using DosStartSession.                 *)
(************************************************************************)

(*
PROCEDURE IncName (VAR (*INOUT*) name: ARRAY OF CHAR;  pos: CARDINAL): BOOLEAN;

    BEGIN
        IF name[pos] < '9' THEN
            INC (name[pos]);
            RETURN TRUE;
        ELSIF pos > 0 THEN
            name[pos] := '0';
            RETURN IncName (name, pos-1);
        ELSE
            RETURN FALSE;
        END (*IF*);
    END IncName;

(************************************************************************)

PROCEDURE ExecProg (VAR (*IN*) ProgName, Params: ARRAY OF CHAR): CARDINAL;

    (* This procedure executes the specified program on behalf of the   *)
    (* client.  The client thread remains blocked until the command     *)
    (* completes.                                                       *)

    CONST ONLength = 256;

    VAR DataLength, result: CARDINAL;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        StartData: OS2.STARTDATA;
        idSession: CARDINAL;  pid: OS2.PID;
        hq: OS2.HQUEUE;
        QueueName: FilenameString;
        RequestData: OS2.REQUESTDATA;
        ElemPriority: CARD8;
        DataAddress: POINTER TO
                         RECORD
                             childid, ResultCode: CARD16;
                         END (*RECORD*);
        NextName: ARRAY [0..5] OF CHAR;

    BEGIN
        (* Create a queue that lets the new session return a result. *)

        NextName := "000000";
        LOOP
            Strings.Assign ("\QUEUES\RQ", QueueName);
            Strings.Append (NextName, QueueName);
            Strings.Append (".QUE", QueueName);
            IF OS2.DosCreateQueue(hq,
                    OS2.QUE_FIFO + OS2.QUE_CONVERT_ADDRESS, QueueName) = 0 THEN
                EXIT (*LOOP*);      (* Success! *)
            END (*IF*);
            IF NOT IncName (NextName, 5) THEN
                RETURN 0;
            END (*IF*);
        END (*LOOP*);

        WITH StartData DO
            Length     :=  SIZE(OS2.STARTDATA);
            Related    :=  OS2.SSF_RELATED_CHILD;
            FgBg       :=  OS2.SSF_FGBG_BACK;
            TraceOpt   :=  OS2.SSF_TRACEOPT_NONE;
            PgmTitle   :=  NIL;
            PgmName    :=  ADR(ProgName);
            IF Params[0] = Nul THEN
                PgmInputs := NIL;
            ELSE
                PgmInputs := ADR(Params);
            END (*IF*);
            TermQ      :=  ADR(QueueName);
            Environment:=  NIL;
            InheritOpt :=  OS2.SSF_INHERTOPT_PARENT;
            SessionType:=  OS2.SSF_TYPE_DEFAULT;
            IconFile   :=  NIL;
            PgmHandle  :=  0;
            PgmControl :=  OS2.SSF_CONTROL_MINIMIZE + OS2.SSF_CONTROL_INVISIBLE;
            InitXPos   :=  30;
            InitYPos   :=  40;
            InitXSize  :=  200;
            InitYSize  :=  140;
            Reserved   :=  0;
            ObjectBuffer  :=  ADR(FailureObjectName);
            ObjectBuffLen :=  ONLength;
        END (*WITH*);

        result := OS2.DosStartSession (StartData, idSession, pid);

        (* Starting in background (code 457) is not an error. *)

        IF (result = 0) OR (result = 457) THEN
            IF OS2.DosReadQueue (hq, RequestData, DataLength,
                        DataAddress, 0, FALSE, ElemPriority, 0) = 0 THEN
                result := DataAddress^.ResultCode;
                OS2.DosFreeMem (DataAddress);
            ELSE
                result := 0;
            END (*IF*);

        ELSE
            result := 0;
        END (*IF*);

        OS2.DosCloseQueue (hq);
        RETURN result;

    END ExecProg;
*)

(************************************************************************)

PROCEDURE RunFilter03 (stage: CARDINAL;  itemdata: ItemDescriptor;
                     VAR (*OUT*) FailureMessage: ARRAY OF CHAR): CARDINAL;

    (* This procedure may be invoked at several stages:                 *)
    (*                                                                  *)
    (*    0    on initial connection                                    *)
    (*    1    after the HELO or EHLO command                           *)
    (*    2    after the MAIL FROM command                              *)
    (*    3    when the DATA command has been received, but before the  *)
    (*           message has been transmitted.                          *)
    (*    4    handled by a separate procedure.                         *)
    (*                                                                  *)
    (* The filter is allowed to return the following codes:             *)
    (*                                                                  *)
    (*    0    continue processing normally.                            *)
    (*    1    like 0, but the filter has modified the list of          *)
    (*         recipients.                                              *)
    (*    2    should not occur at this stage.                          *)
    (*    3    reject the message.                                      *)
    (*    4    like 3, but the filter has placed a failure message into *)
    (*           the first line of the 'recipients' file.               *)
    (*  5-15   unused, reserved for future use.                         *)
    (*   16    like 0, but in addition future filtering steps will be   *)
    (*           bypassed for this item of mail.                        *)
    (*                                                                  *)
    (* As a guard against errors in the filters, unused codes are       *)
    (* converted to 0.                                                  *)
    (*                                                                  *)
    (* Note that cases 1, 4, and 16 are dealt with internally in        *)
    (* this procedure, so this procedure will only ever return a        *)
    (* result of 0, 2, or 3.                                            *)
    (*                                                                  *)
    (* In case 3, FailureMessage holds the reply (starting with a       *)
    (* three-digit code) to be returned to the sender.                  *)

    CONST ONLength = 256;  Tab = CHR(9);  MaxResultCode = 16;
    TYPE ResultCodes = SET OF [0..MaxResultCode];
    CONST LegalResultCodes = ResultCodes {0..4, 16};
    TYPE ArgStringIndex = [0..3*(MAX(FilenameIndex)+1) + 12];

    VAR j, k, result: CARDINAL;  found, Serialise: BOOLEAN;
        FilterName, recipients, newfile: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        cid, cid2: ChanId;
        LineBuffer, RPBuffer: ARRAY [0..1023] OF CHAR;
        ArgString: ARRAY ArgStringIndex OF CHAR;

    BEGIN
        IF itemdata^.SkipFiltering THEN
            RETURN 0;
        END (*IF*);

        Obtain (FilterProgLock);
        FilterName := FilterProg[stage];
        Release (FilterProgLock);

        (* If no filter specified, do nothing. *)

        IF FilterName[0] = Nul THEN
            RETURN 0;
        END (*IF*);

        MakeRecipientListFile (stage, itemdata, recipients);
        ArgString := "CMD /C ";
        Strings.Append (FilterName, ArgString);
        Strings.Append (" ", ArgString);
        Strings.Append (recipients, ArgString);
        IF itemdata^.TempName[0] <> Nul THEN
            Strings.Append (" ", ArgString);
            Strings.Append (itemdata^.TempName, ArgString);
        END (*IF*);

        (* Special rule for ArgString: it must be terminated by two Nul *)
        (* characters, and the program name and arguments must also be  *)
        (* separated by a Nul.  We have to insert the separating Nul    *)
        (* after everything else has been done, otherwise it would mess *)
        (* up the Strings.Append operation.                             *)

        (* The following piece of code contains redundancies, because   *)
        (* I've made ArgString big enough to fit the two Nuls even in   *)
        (* the worst case, and the first one will already have been     *)
        (* put there by Strings.Append; but the belt-and-braces         *)
        (* approach is a minor cost, and makes the code easier to read. *)

        j := LENGTH(ArgString);
        IF j <= MAX(ArgStringIndex) THEN
            ArgString[j] := Nul;
            INC (j);
            IF j <= MAX(ArgStringIndex) THEN
                ArgString[j] := Nul;
            END (*IF*);
        END (*IF*);
        ArgString[3] := Nul;

        Obtain (FilterProgLock);
        Serialise := SerialiseFilters;
        Release (FilterProgLock);
        IF Serialise THEN
            Obtain(FilterAccess);
        END (*IF*);
        ReleaseScreen;
        result := OS2.DosExecPgm (FailureObjectName, ONLength,
                                  OS2.EXEC_SYNC, ArgString, NIL,
                                  ExitStatus, "CMD.EXE");
        RegainScreen;
        IF Serialise THEN
            Release(FilterAccess);
        END (*IF*);

        (* Starting in background (code 457) is not an error. *)

        IF (result = 0) OR (result = 457) THEN
            result := ExitStatus.codeResult;
            IF (result > MaxResultCode) OR NOT (result IN LegalResultCodes) THEN
                result := 0;
            END (*IF*);
        ELSE
            result := 0;
        END (*IF*);

        FailureMessage[0] := Nul;
        IF result = 1 THEN
            RebuildRecipientList (itemdata, recipients);
            result := 0;
        ELSIF result = 4 THEN
            ReadFailureMessage (recipients, FailureMessage);
            DEC (result);
        ELSIF result = 16 THEN
            itemdata^.SkipFiltering := TRUE;
            IF stage = 0 THEN
                itemdata^.SkipFiltering0 := TRUE;
            END (*IF*);
            result := 0;
        END (*IF*);
        IF (result >= 3) AND (FailureMessage[0] = Nul) THEN

            (* The duplication below could be replaced                  *)
            (* by more efficient code, but I'm leaving options open for *)
            (* future changes.  Note also that, to ensure conformance   *)
            (* with the SMTP standard, the numeric code is different at *)
            (* different stages.                                        *)

            IF stage = 0 THEN
                Strings.Assign ("421 Spammers not welcome here", FailureMessage);
            ELSIF stage = 1 THEN
                Strings.Assign ("421 Spammers not welcome here", FailureMessage);
            ELSIF stage = 2 THEN
                Strings.Assign ("421 Spammers not welcome here", FailureMessage);
            ELSIF stage = 3 THEN
                Strings.Assign ("554 Mail rejected by filter", FailureMessage);
            ELSE
                Strings.Assign ("554 Mail rejected by filter", FailureMessage);
            END (*IF*);

        END (*IF*);

        FileSys.Remove (recipients, found);

        IF (stage > 3) AND (result <= 2) THEN

            (* The filter might have altered the sender address and/or  *)
            (* the Return-Path header, so we should ensure (by altering *)
            (* the Return-Path if necessary) that these agree with      *)
            (* each other.                                              *)

            RPBuffer := "Return-Path: ";
            Strings.Append (itemdata^.returnpath, RPBuffer);
            Strings.Append (CRLF, RPBuffer);
            j := Strings.Length (RPBuffer);

            (* The first j characters of the message file should  *)
            (* match RPBuffer.                                    *)

            cid := OpenOldFile (itemdata^.TempName, FALSE, TRUE);
            IF cid <> NoSuchChannel THEN
                ReadRaw (cid, LineBuffer, j, k);
                IF (j <> k) OR NOT Strings.Equal (LineBuffer, RPBuffer) THEN

                    (* Mismatch.  Skip over the Return-Path. *)

                    SetPosition (cid, StartPosition(cid));
                    ReadLine (cid, LineBuffer);
                    IF KeywordMatch ("Return-Path", LineBuffer) THEN

                        (* Note that the Return-Path line might have continuation lines. *)

                        REPEAT
                            ReadLine (cid, LineBuffer);
                        UNTIL (LineBuffer[0] <> ' ') AND (LineBuffer[0] <> Tab);

                    END (*IF*);

                    (* At this stage RPBuffer holds the true desired    *)
                    (* new Return-Path line, LineBuffer holds the line  *)
                    (* after the Return-Path.  Open a new file, copy    *)
                    (* these two lines to it, then continue copying     *)
                    (* from the old file.                               *)

                    (* Remark: if we fail to create the new file, we    *)
                    (* simply stay with the old file as it is.          *)

                    FilterName := itemdata^.TempName;
                    Strings.FindPrev ('\', FilterName, LENGTH(FilterName)-1, found, k);
                    IF found THEN
                        FilterName[k+1] := Nul;
                    END (*IF*);
                    cid2 := OpenNewOutputFile (FilterName, ".###", newfile);
                    IF cid2 <> NoSuchChannel THEN
                        WriteRaw (cid2, RPBuffer, j);
                        itemdata^.offset := CurrentPosition(cid2);
                        FWriteString (cid2, LineBuffer);
                        FWriteLn (cid2);
                        WHILE j > 0 DO
                            ReadRaw (cid, LineBuffer, 1024, j);
                            IF j > 0 THEN
                                WriteRaw (cid2, LineBuffer, j);
                            END (*IF*);
                        END (*WHILE*);
                        CloseFile (cid2);
                    END (*IF*);
                    CloseFile (cid);

                    (* Copying complete.  Delete the old file, replace  *)
                    (* it with the new file.                            *)

                    DeleteFile (itemdata^.TempName);
                    itemdata^.TempName := newfile;

                ELSE
                    CloseFile (cid);
                END (*IF*);

            END (*IF*);

        END (*IF*);

        RETURN result;

    END RunFilter03;

(************************************************************************)

PROCEDURE RunFinalFilter (itemdata: ItemDescriptor;
                             VAR (*IN*) FilterName: FilenameString;
                     VAR (*OUT*) FailureMessage: ARRAY OF CHAR): CARDINAL;

    (* This procedure may be invoked at stage 4 of reception:           *)
    (*                                                                  *)
    (*    4    after a mail item has been received but before it has    *)
    (*           been distributed to the addressees.                    *)
    (*                                                                  *)
    (* The filter is allowed to return the following codes:             *)
    (*                                                                  *)
    (*    0    continue processing normally, i.e. deliver mail          *)
    (*    1    like 0, but the filter has modified the list of          *)
    (*         recipients.                                              *)
    (*    2    item has now been dealt with, report success to sender   *)
    (*    3    reject the message                                       *)
    (*    4    like 3, but the filter has placed a failure message into *)
    (*           the first line of the 'recipients' file.               *)
    (*  5-15   unused, reserved for future use.                         *)
    (*   16    only used in stages 0-3.                                 *)
    (*                                                                  *)
    (* As a guard against errors in the filters, unused codes are       *)
    (* converted to 0.                                                  *)
    (*                                                                  *)
    (* Note that cases 1 and 4 are dealt with internally in             *)
    (* this procedure, so this procedure will only ever return a        *)
    (* result of 0, 2, or 3.                                            *)
    (*                                                                  *)
    (* In case 3, FailureMessage holds the reply (starting with a       *)
    (* three-digit code) to be returned to the sender.                  *)
    (* This procedure is called only from module SMTPCommands.          *)

    CONST stage = 4;  ONLength = 256;  Tab = CHR(9);  MaxResultCode = 16;
    TYPE ResultCodes = SET OF [0..MaxResultCode];
    CONST LegalResultCodes = ResultCodes {0..4};

    VAR j, k, result: CARDINAL;  found, Serialise: BOOLEAN;
        recipients, ArgString, newfile: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        cid, cid2: ChanId;
        LineBuffer, RPBuffer: ARRAY [0..1023] OF CHAR;

    BEGIN
        IF itemdata^.SkipFiltering OR (FilterName[0] = Nul) THEN
            RETURN 0;
        END (*IF*);

        MakeRecipientListFile (stage, itemdata, recipients);
        ArgString := "CMD /C ";
        Strings.Append (FilterName, ArgString);
        Strings.Append (" ", ArgString);
        Strings.Append (recipients, ArgString);
        IF itemdata^.TempName[0] <> Nul THEN
            Strings.Append (" ", ArgString);
            Strings.Append (itemdata^.TempName, ArgString);
        END (*IF*);

        (* Special rule for ArgString: it must be terminated by two Nul *)
        (* characters, and the program name and arguments must also be  *)
        (* separated by a Nul.  We have to insert the separating Nul    *)
        (* after everything else has been done, otherwise it would mess *)
        (* up the Strings.Append operation.                             *)

        j := LENGTH(ArgString) + 1;
        IF j <= MAX(FilenameIndex) THEN
            ArgString[j] := Nul;
        END (*IF*);
        ArgString[3] := Nul;

        Obtain (FilterProgLock);
        Serialise := SerialiseFilters;
        Release (FilterProgLock);
        IF Serialise THEN
            Obtain(FilterAccess);
        END (*IF*);
        ReleaseScreen;
        result := OS2.DosExecPgm (FailureObjectName, ONLength,
                                  OS2.EXEC_SYNC, ArgString, NIL,
                                  ExitStatus, "CMD.EXE");
        RegainScreen;
        IF Serialise THEN
            Release(FilterAccess);
        END (*IF*);

        (* Starting in background (code 457) is not an error. *)

        IF (result = 0) OR (result = 457) THEN
            result := ExitStatus.codeResult;
            IF (result > MaxResultCode) OR NOT (result IN LegalResultCodes) THEN
                result := 0;
            END (*IF*);
        ELSE
            result := 0;
        END (*IF*);

        FailureMessage[0] := Nul;
        IF result = 1 THEN
            RebuildRecipientList (itemdata, recipients);
            result := 0;
        ELSIF result = 4 THEN
            ReadFailureMessage (recipients, FailureMessage);
            DEC (result);
        END (*IF*);
        IF (result >= 3) AND (FailureMessage[0] = Nul) THEN

            Strings.Assign ("554 Mail rejected by filter", FailureMessage);

        END (*IF*);

        FileSys.Remove (recipients, found);

        IF result <= 2 THEN

            (* The filter might have altered the sender address and/or  *)
            (* the Return-Path header, so we should ensure (by altering *)
            (* the Return-Path if necessary) that these agree with      *)
            (* each other.                                              *)

            RPBuffer := "Return-Path: ";
            Strings.Append (itemdata^.returnpath, RPBuffer);
            Strings.Append (CRLF, RPBuffer);
            j := Strings.Length (RPBuffer);

            (* The first j characters of the message file should  *)
            (* match RPBuffer.                                    *)

            cid := OpenOldFile (itemdata^.TempName, FALSE, TRUE);
            IF cid <> NoSuchChannel THEN
                ReadRaw (cid, LineBuffer, j, k);
                IF (j <> k) OR NOT Strings.Equal (LineBuffer, RPBuffer) THEN

                    (* Mismatch.  Skip over the Return-Path. *)

                    SetPosition (cid, StartPosition(cid));
                    ReadLine (cid, LineBuffer);
                    IF KeywordMatch ("Return-Path", LineBuffer) THEN

                        (* Note that the Return-Path line might have continuation lines. *)

                        REPEAT
                            ReadLine (cid, LineBuffer);
                        UNTIL (LineBuffer[0] <> ' ') AND (LineBuffer[0] <> Tab);

                    END (*IF*);

                    (* At this stage RPBuffer holds the true desired    *)
                    (* new Return-Path line, LineBuffer holds the line  *)
                    (* after the Return-Path.  Open a new file, copy    *)
                    (* these two lines to it, then continue copying     *)
                    (* from the old file.                               *)

                    (* Remark: if we fail to create the new file, we    *)
                    (* simply stay with the old file as it is.          *)

                    FilterName := itemdata^.TempName;
                    Strings.FindPrev ('\', FilterName, LENGTH(FilterName)-1, found, k);
                    IF found THEN
                        FilterName[k+1] := Nul;
                    END (*IF*);
                    cid2 := OpenNewOutputFile (FilterName, ".###", newfile);
                    IF cid2 <> NoSuchChannel THEN
                        WriteRaw (cid2, RPBuffer, j);
                        itemdata^.offset := CurrentPosition(cid2);
                        FWriteString (cid2, LineBuffer);
                        FWriteLn (cid2);
                        WHILE j > 0 DO
                            ReadRaw (cid, LineBuffer, 1024, j);
                            IF j > 0 THEN
                                WriteRaw (cid2, LineBuffer, j);
                            END (*IF*);
                        END (*WHILE*);
                        CloseFile (cid2);
                    END (*IF*);
                    CloseFile (cid);

                    (* Copying complete.  Delete the old file, replace  *)
                    (* it with the new file.                            *)

                    DeleteFile (itemdata^.TempName);
                    itemdata^.TempName := newfile;

                ELSE
                    CloseFile (cid);
                END (*IF*);

            END (*IF*);

        END (*IF*);

        RETURN result;

    END RunFinalFilter;

(************************************************************************)
(*                   DISTRIBUTING A RECEIVED MESSAGE                    *)
(************************************************************************)

PROCEDURE DistributeMessage (itemdata: ItemDescriptor);

    (* This procedure can be called after AcceptMessage has read the    *)
    (* whole message.  Now we put it into the local mailboxes, and/or   *)
    (* relay it, depending on the recipients.                           *)
    (* This procedure is called only internally.                        *)

    BEGIN
        WITH itemdata^ DO
            CopyToRecipients (TempName, returnpath, offset, LogID,
                              Recipients, FALSE);
            TempName[0] := Nul;
        END (*WITH*);
    END DistributeMessage;

(************************************************************************)

PROCEDURE SingleFilterAndDistribute (itemdata: ItemDescriptor;
                                        VAR (*IN*) filter: FilenameString;
                                                    SB: SBuffer): BOOLEAN;

    (* Runs the final filter on this message, and then delivers it      *)
    (* unless the filter says not to.  Returns FALSE if the filter has  *)
    (* rejected the message.  This version is for the case where we     *)
    (* know we will be using the same filter for all recipients.        *)
    (* Sends the success or failure reply to the client.                *)

    VAR result, sent: CARDINAL;
        success: BOOLEAN;
        ReplyString: ARRAY [0..127] OF CHAR;

    BEGIN
        IF itemdata^.SkipFiltering THEN
            result := 0;
        ELSE
            result := RunFinalFilter (itemdata, filter, ReplyString);
        END (*IF*);
        success := result < 3;
        IF success AND (result <> 2) THEN
            DistributeMessage (itemdata);
        END (*IF*);
        IF success THEN
            Strings.Assign ("250 OK", ReplyString);
        ELSIF result > 3 THEN
            Strings.Assign ("554 Server error, please report to postmaster", ReplyString);
        END (*IF*);

        (* It is possible that the next few lines are redundant,        *)
        (* because the caller replies and logs the reply.               *)

        EVAL (SendLine (SB, ReplyString, sent));
        EVAL (FlushOutput (SB));
        Strings.Insert ("> ", 0, ReplyString);
        LogTransaction (itemdata^.LogID, ReplyString);

        RETURN success;

    END SingleFilterAndDistribute;

(************************************************************************)

PROCEDURE FilterAndDistribute (itemdata: ItemDescriptor;  SB: SBuffer): BOOLEAN;

    (* Runs the final (stage 4) filter on this message, and then        *)
    (* delivers it unless the filter says not to.  Returns FALSE if the *)
    (* filter has rejected the message for all recipients.              *)

    VAR list, next: ListOfRecipients;
        OriginalFile, NewDir, NewFile: FilenameString;
        cid: ChanId;
        success: BOOLEAN;

    BEGIN
        (* Sort the recipients by filter name. *)

        NEW (list);
        Obtain (FilterProgLock);
        list^.filtername := FilterProg[4];
        Release (FilterProgLock);
        list^.this := itemdata^.Recipients;
        list^.next := NIL;
        IF NOT itemdata^.SkipFiltering THEN
            SortByFilter (list);
        END (*IF*);

        (* Save the message file. *)

        OriginalFile := itemdata^.TempName;

        (* Filter and deliver all the subjobs.  Remark: we do this by   *)
        (* giving values to the Recipients and TempName fields of       *)
        (* itemdata, reusing the same itemdata each time.  By now some  *)
        (* itemdata fields, specifically the firstrecipient and         *)
        (* recipientcount information, are incorrect, but that does not *)
        (* matter because that information was needed only at an        *)
        (* earlier stage of processing the message.                     *)

        success := FALSE;
        WHILE list <> NIL DO
            itemdata^.Recipients := list^.this;

            (* Make a copy of the message file. *)

            ChooseIncomingFileDirectory (itemdata^.Recipients, NewDir);
            cid := OpenNewOutputFile (NewDir, ".###", NewFile);
            CloseFile (cid);
            EVAL (OS2.DosCopy (OriginalFile,
                                    NewFile, OS2.DCPY_EXISTING));
            itemdata^.TempName := NewFile;
            IF SingleFilterAndDistribute (itemdata, list^.filtername, SB) THEN
                success := TRUE;
            END (*IF*);
            next := list^.next;
            DISPOSE (list);
            list := next;
        END (*WHILE*);

        (* Remove the original message file.  (The copies have been *)
        (* removed during the delivery.)                            *)

        DeleteFile (OriginalFile);

        RETURN success;
    END FilterAndDistribute;

(************************************************************************)

PROCEDURE LimitOnMessageSize(): CARDINAL;

    (* Max allowable number of bytes in a message. *)

    VAR result: CARDINAL;

    BEGIN
        Obtain (MaxMessageSizeLock);
        result := MaxMessageSize;
        Release (MaxMessageSizeLock);
        RETURN result;
    END LimitOnMessageSize;

(************************************************************************)
(*                        MODULE INITIALISATION                         *)
(************************************************************************)

PROCEDURE UpdateINIData;

    (* Re-reads our INI file to get the options that can be changed     *)
    (* while Weasel is still running.                                   *)

    VAR hini: INIData.HINI;  j: [0..4];
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        app := "$SYS";
        hini := OpenINI();
        IF INIData.INIValid (hini) THEN

            IF NOT INIGet (hini, app, "CheckNoDNS", CheckNoDNS) THEN
                CheckNoDNS := FALSE;
            END (*IF*);
            IF NOT INIGet (hini, app, "CheckNoRDNS", CheckNoRDNS) THEN
                CheckNoRDNS := TRUE;
            END (*IF*);

            Obtain (FilterProgLock);
            key := "SerialiseFilters";
            EVAL (INIGet (hini, app, key, SerialiseFilters));
            FOR j := 0 TO 4 DO
                key := "FilterProg";
                key[10] := CHR(j + ORD('0'));
                key[11] := Nul;
                EVAL (INIGetString (hini, app, key, FilterProg[j]));
                WHILE FilterProg[j][0] = ' ' DO
                    Strings.Delete (FilterProg[j], 0, 1);
                END (*WHILE*);
            END (*FOR*);
            Release (FilterProgLock);

            key := "LogSMTPItems";
            EVAL (INIGet (hini, app, key, LogSMTPItems));
            key := "SMTPLogName";
            IF NOT INIGetString (hini, app, key, SMTPLogName) THEN
                SMTPLogName := "SMTP.LOG";
            END (*IF*);

            Obtain (MaxMessageSizeLock);
            key := "MaxMessageSize";
            IF NOT INIGet (hini, app, key, MaxMessageSize) THEN
                MaxMessageSize := MAX(CARDINAL);
            END (*IF*);
            Release (MaxMessageSizeLock);

            key := "pmchecklevel";
            IF NOT INIGet (hini, app, key, pmchecklevel) THEN
                pmchecklevel := marksuspectfiles;
            END (*IF*);
            key := "MAILFROMcheck";
            EVAL (INIGet (hini, app, key, MAILFROMcheck));
            key := "SPFenabled";
            EVAL (INIGet (hini, app, key, SPFenabled));

            CloseINI;

        END (*IF*);
    END UpdateINIData;

(************************************************************************)

PROCEDURE LoadSMTPINIData;

    (* Initial load of configuration data for this module. *)

    TYPE CharSet = SET OF CHAR;

    CONST Alphanumeric = CharSet {'0'..'9', 'A'..'Z'};

    VAR hini: INIData.HINI;  j: [0..7];
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        app := "$SYS";
        hini := OpenINI();
        IF INIData.INIValid (hini) THEN

            (* For the filters, convert from old version if necessary. *)

            key := "FilterProg4";
            IF NOT INIGetString (hini, app, key, FilterProg[4]) THEN
                key := "FilterProg2";
                EVAL (INIGetString (hini, app, key, FilterProg[4]));
                key := "FilterProg2";
                INIPutString (hini, app, key, FilterProg[2]);
                key := "FilterProg4";
                INIPutString (hini, app, key, FilterProg[4]);
            END (*IF*);

            (* NextName, which is stored in the INI file with key       *)
            (* UName, is used when creating unique file names.          *)

            key := "UName";
            IF INIGet (hini, app, key, NextName) THEN

                (* Ensure that NextName has the right format. *)

                FOR j := 0 TO 7 DO
                    IF NOT (NextName[j] IN Alphanumeric) THEN
                        NextName[j] := '0';
                    END (*IF*);
                END (*FOR*);
            ELSE
                NextName := "00000000";
            END (*IF*);

            CloseINI;
        END (*IF*);
        UpdateINIData;
    END LoadSMTPINIData;

(************************************************************************)

VAR hini: INIData.HINI;
    app: ARRAY [0..4] OF CHAR;
    key: ARRAY [0..10] OF CHAR;

BEGIN
    CRLF[0] := CR;  CRLF[1] := LF;
    MaxMessageSize := MAX(CARDINAL);
    CreateLock (MaxMessageSizeLock);
    CreateLock (LogFileLock);
    LogSMTPItems := FALSE;
    SPFenabled := FALSE;
    CheckNoRDNS := FALSE;
    CheckNoDNS := FALSE;
    pmchecklevel := marksuspectfiles;
    SerialiseFilters := TRUE;
    CreateLock (FilterAccess);
    CreateLock (FilterProgLock);
    CreateLock (NextNameLock);
FINALLY
    hini := OpenINI();
    IF INIData.INIValid (hini) THEN
        Obtain (NextNameLock);
        app := "$SYS";  key := "UName";
        INIPut (hini, app, key, NextName);
        Release (NextNameLock);
        CloseINI;
    END (*IF*);
END SMTPData.

