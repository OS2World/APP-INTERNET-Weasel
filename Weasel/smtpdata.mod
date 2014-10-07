(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2014   Peter Moylan                                     *)
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
        (*  Last edited:        30 July 2012                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CARD8, CARD16, ADDRESS, CAST, ADR;

IMPORT WV, Strings, OS2, FileSys, INIData;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos,
    (* proc *)  OpenOldFile, OpenAtEnd, OpenNewFile1, CloseFile,
                StartPosition, CurrentPosition, SetPosition, ReadLine, ReadRaw,
                WriteRaw, FWriteChar, FWriteString, FWriteCard, FWriteLn,
                DeleteFile;

FROM Names IMPORT
    (* type *)  UserName, HostName, FilenameIndex, FilenameString, PathString;

FROM Domains IMPORT
    (* type *)  Domain;

FROM Hosts IMPORT
    (* proc *)  CheckHost, OnBlacklist;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SocketOf, Getch;

FROM MXCheck IMPORT
    (* proc *)  DoMXLookup;

FROM MyClock IMPORT
    (* proc *)  CurrentDateAndTime, AppendTimeString, CurrentTimeToString;

FROM INIData IMPORT
    (* proc *)  OpenINIFile, INIGet, INIGetString, INIPut, INIPutString,
                CloseINIFile;

FROM InetUtilities IMPORT
    (* proc *)  WriteCard;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString, AddressToHostName;

FROM Delivery IMPORT
    (* type *)  CombinedRecipientList,
    (* proc *)  CreateCombinedRecipientList, DiscardCombinedRecipientList,
                ClearCombinedRecipientList, EmptyRecipientList,
                AddToLocalList, AddToRelayList, WriteRecipientList,
                ChooseIncomingFileDirectory, GetOurHostName,
                AddRecipient, CopyToRecipients, SeparateFilter;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  GetLogPrefix;

FROM SplitScreen IMPORT
    (* proc *)  ReleaseScreen, RegainScreen;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
    EmptyReversePathMarker = CHR(1);

    (* Feature not yet implemented.  This will later become a variable. *)

    ReceivedOffset = 0;

TYPE
    (* An ItemDescriptor record keeps track of the information needed   *)
    (* to send one item of mail.  The fields are                        *)
    (*   RealIPAddr        the sender's real IP address as determined   *)
    (*                        from the socket binding                   *)
    (*   RealName          the sender's hostname as determined by       *)
    (*                        nameserver lookup                         *)
    (*   HELOname          the sender's hostname as supplied in the     *)
    (*                        HELO command                              *)
    (*   TempName          name of a file where the incoming item is    *)
    (*                       stored before being distributed to all     *)
    (*                       recipients.                                *)
    (*   offset            effective starting point of the file if it   *)
    (*                       has to be relayed.                         *)
    (*   charcount         bytes received                               *)
    (*   LogID             ID used for transaction logging              *)
    (*   returnpath        path supplied by MAIL FROM:                  *)
    (*   firstrecipient    as supplied in first RCPT TO:                *)
    (*   recipientcount    number of RCPT TO: recipients                *)
    (*   Recipients        list of destination mailboxes                *)
    (*   RelayAllowed      TRUE iff the sending host is one that is     *)
    (*                       allowed to relay through us, and allowed   *)
    (*                       access to non-public aliases.              *)
    (*   SkipFiltering     TRUE iff we want to bypass filtering.        *)
    (*   SkipFiltering0    The value of SkipFiltering after stage 0.    *)

    ItemDescriptor = POINTER TO Item;
    Item = RECORD
               RealIPAddr: CARDINAL;
               RealName: HostName;
               HELOname: HostName;
               LogID: TransactionLogID;
               TempName: FilenameString;
               offset: FilePos;
               charcount: CARDINAL;
               returnpath, firstrecipient: PathString;
               recipientcount: CARDINAL;
               Recipients: CombinedRecipientList;
               RelayAllowed: BOOLEAN;
               SkipFiltering0, SkipFiltering: BOOLEAN;
           END (*RECORD*);

VAR
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

    (* Flag to say that we should apply "unacceptable host" checks to   *)
    (* the domain in the MAIL FROM command.                             *)

    MAILFROMcheck: BOOLEAN;

    (* A flag that's true for filters prior to version 1.643. *)

    OldFilterRules: BOOLEAN;

    (* Flag to say that we get our INI data from TNI files. *)

    UseTNI: BOOLEAN;

    (* A useful "constant". *)

    CRLF: ARRAY [0..1] OF CHAR;

(************************************************************************)
(*                KEEPING TRACK OF THE ITEM INFORMATION                 *)
(************************************************************************)

PROCEDURE CreateItemDescriptor (SB: SBuffer;  ClientIPAddress: CARDINAL;
                                 ID: TransactionLogID;
                                  MayRelay: BOOLEAN): ItemDescriptor;

    (* Creates a descriptor for a new mail item.  ID is for *)
    (* transaction logging.                                 *)

    VAR result: ItemDescriptor;

    BEGIN
        NEW (result);
        WITH result^ DO
            RealIPAddr := ClientIPAddress;
            AddressToHostName (ClientIPAddress, RealName);
            LogID := ID;
            HELOname := "";
            TempName := "";
            firstrecipient := "";
            recipientcount := 0;
            Recipients := CreateCombinedRecipientList();
            RelayAllowed := MayRelay;
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
        END (*WITH*);
    END ResetReturnPath;

(************************************************************************)

PROCEDURE ResetItemDescriptor (desc: ItemDescriptor;
                               ReturnPath: ARRAY OF CHAR);

    (* Discards information related to sender and receivers, deletes    *)
    (* message file if one has been created, and sets a new return path.*)

    VAR dummy: BOOLEAN;

    BEGIN
        IF desc <> NIL THEN
            IF desc^.TempName[0] <> Nul THEN
                FileSys.Remove (desc^.TempName, dummy);
                desc^.TempName[0] := Nul;
            END (*IF*);
            Strings.Assign (ReturnPath, desc^.returnpath);
            ClearCombinedRecipientList (desc^.Recipients);
            desc^.firstrecipient := "";
            desc^.recipientcount := 0;
            desc^.SkipFiltering := desc^.SkipFiltering0;
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

PROCEDURE SetClaimedSendingHost (desc: ItemDescriptor;
                                 VAR (*IN*) ClaimedName: ARRAY OF CHAR);

    (* ClaimedName is the sending host's name as supplied in the HELO command. *)

    BEGIN
        Strings.Assign (ClaimedName, desc^.HELOname);
    END SetClaimedSendingHost;

(************************************************************************)

PROCEDURE ParsePathString (path: ARRAY OF CHAR;
                            VAR (*OUT*) user: UserName;
                            VAR (*OUT*) domain: HostName);

    (* Extracts the user and domain from a path string, i.e. the sort   *)
    (* of string that is used in the MAIL FROM and RCPT TO commands.    *)

    VAR j, k, pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        j := 0;  k := Strings.Length(path);

        (* Skip leading and trailing spaces. *)

        WHILE (j <= HIGH(path)) AND (path[j] = ' ') DO
            INC (j);
        END (*WHILE*);

        WHILE (k > 0) AND (path[k-1] = ' ') DO
            DEC (k);
        END (*WHILE*);

        (* Remove the enclosing <>. *)

        IF (j <= HIGH(path)) AND (path[j] = '<') THEN
            INC (j);
            IF (k > 0) AND (path[k-1] = '>') THEN
                DEC (k);
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

PROCEDURE FromAddressAcceptable (desc: ItemDescriptor): BOOLEAN;

    (* Returns TRUE if we're satisfied with the sender's address as     *)
    (* supplied in MAIL FROM.  If we're not satisfied, we clear that    *)
    (* address as well as returning FALSE.                              *)

    CONST max = 32;

    VAR user: UserName;  domain: HostName;
        E1, E2, OK, IsBanned, MayRelay: BOOLEAN;
        j: CARDINAL;
        address: ARRAY [0..max] OF CARDINAL;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        ParsePathString (desc^.returnpath, user, domain);
        E1 := user[0] = Nul;
        E2 := domain[0] = Nul;

        (* The SMTP standard says that a completely empty FROM address  *)
        (* is acceptable.                                               *)

        IF E1 AND E2 THEN
            RETURN TRUE;
        END (*IF*);

        (* Start with a simple check: user and domain must be nonempty. *)
        (* (This test temporarily disabled, and I'm considering         *)
        (* disabling it permanently.                                    *)

        OK := NOT (E1 OR E2);

        (* If we pass that check, do the "banned hosts" checks if       *)
        (* the MAILFROMcheck option has been set.                       *)

        IF OK AND MAILFROMcheck THEN
            IF DoMXLookup (domain, address) = 0 THEN
                j := 0;
                WHILE OK AND (j <= max) AND (address[j] <> 0) DO
                    CheckHost (address[j], IsBanned, MayRelay);
                    IF IsBanned THEN
                        OK := FALSE;
                    ELSIF NOT MayRelay THEN
                        OK := NOT OnBlacklist (desc^.LogID, address[j],
                                                            message);
                    END (*IF*);
                    INC(j);
                END (*WHILE*);
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

PROCEDURE InsertMessageID (cid: ChanId;  LocalHost: HostName);

    (* Writes a "Message-ID" line to the file. *)

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
    END InsertMessageID;

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

PROCEDURE ReceiveMessage0 (SB: SBuffer;  cid: ChanId;  LocalHost: HostName;
                             sem: Semaphore;  StoreIt: BOOLEAN;
                              VAR (*OUT*) TooManyHops: BOOLEAN): CARDINAL;

    (* Receives an incoming message, stores it to a previously opened   *)
    (* file.  We periodically signal on sem to confirm that the         *)
    (* operation has not timed out.  The returned value is a character  *)
    (* count, or MAX(CARDINAL) if the transfer failed.                  *)
    (* If StoreIt = FALSE then we don't write to the file.              *)

    VAR LineBuffer: ARRAY [0..1023] OF CHAR;

    (********************************************************************)

    PROCEDURE ExtractSendingHost;

        (* Processes LineBuffer, which we have already found is a  *)
        (* Received: line, to get the "from" host address.         *)

        BEGIN
        END ExtractSendingHost;

    (********************************************************************)

    VAR EndOfMessage, MessageIDPresent, InHeader: BOOLEAN;
        amount, total, ReceivedCount: CARDINAL;

    BEGIN
        EndOfMessage := FALSE;
        MessageIDPresent := FALSE;
        InHeader := TRUE;
        ReceivedCount := 0;
        total := 0;
        REPEAT
            Signal (sem);
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
                        (* "Message-ID" header line if none was received.       *)

                        InHeader := FALSE;
                        IF NOT MessageIDPresent THEN
                            InsertMessageID (cid, LocalHost);
                        END (*IF*);

                    ELSIF KeywordMatch ("Received", LineBuffer) THEN
                        INC (ReceivedCount);
                        IF ReceivedCount = ReceivedOffset THEN
                            ExtractSendingHost;
                        END (*IF*);
                    ELSE
                        MessageIDPresent := MessageIDPresent OR
                                         KeywordMatch ("Message-ID", LineBuffer);
                    END (*IF*);

                END (*IF*);

                IF StoreIt AND NOT EndOfMessage THEN
                    IF amount > 0 THEN
                        WriteRaw (cid, LineBuffer, amount);
                    END (*IF*);
                    WriteEOL (cid);
                END (*IF*);
                INC (total, amount+2);
            END (*IF*);

        UNTIL EndOfMessage OR (total = MAX(CARDINAL));

        TooManyHops := ReceivedCount > 25;
        RETURN total;

    END ReceiveMessage0;

(************************************************************************)

PROCEDURE ReceiveMessage (SB: SBuffer;  cid: ChanId;  LocalHost: HostName;
                              sem: Semaphore;
                              VAR (*OUT*) TooManyHops: BOOLEAN): CARDINAL;

    (* Receives an incoming message, stores it to a previously opened   *)
    (* file.  We periodically signal on sem to confirm that the         *)
    (* operation has not timed out.  The returned value is a character  *)
    (* count, or MAX(CARDINAL) if the transfer failed.                  *)

    (* TooManyHops = TRUE means that the message has too many header    *)
    (* lines saying "Received:".                                        *)

    BEGIN
        RETURN ReceiveMessage0 (SB, cid, LocalHost, sem, TRUE, TooManyHops);
    END ReceiveMessage;

(************************************************************************)

PROCEDURE SkipMessage (SB: SBuffer;  sem: Semaphore);

    (* Like ReceiveMessage, but the incoming data are discarded rather  *)
    (* than being written to a file.                                    *)

    VAR dummy: BOOLEAN;  dummyhost: HostName;

    BEGIN
        EVAL (ReceiveMessage0 (SB, NoSuchChannel, dummyhost, sem, FALSE, dummy));
    END SkipMessage;

(************************************************************************)

PROCEDURE AcceptMessage (SB: SBuffer;  itemdata: ItemDescriptor;
                         sem: Semaphore;
                         VAR (*OUT*) FailureReason: ARRAY OF CHAR): BOOLEAN;

    (* Receives an incoming message, stores it in a temporary file      *)
    (* whose name is recorded in itemdata.  We periodically signal on   *)
    (* sem to confirm that the reception has not timed out.             *)

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
                PosInLine := 1;
            END (*IF*);
            FWriteString (cid, str);
            INC (PosInLine, length);
        END AddString;

    (********************************************************************)

    VAR success, dummy, TooManyHops: BOOLEAN;
        cid: ChanId;
        BaseName: FilenameString;
        StringBuffer: ARRAY [0..255] OF CHAR;
        LocalHost: HostName;

    BEGIN
        success := NOT NoRecipients(itemdata);

        IF success THEN

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
                itemdata^.offset := CurrentPosition(cid);

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
                AddString (cid, " by " );
                LocalHost := "[127.0.0.0]";      (* fallback default *)
                GetOurHostName (SocketOf(SB), LocalHost);
                AddString (cid, LocalHost);
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

                (* Read the new message into the temporary file. *)

                itemdata^.charcount := ReceiveMessage (SB, cid, LocalHost,
                                                        sem, TooManyHops);
                CloseFile (cid);
                Signal (sem);

                IF TooManyHops THEN
                    Strings.Assign ("554 too many hops (max 25)", FailureReason);
                    success := FALSE;
                ELSE
                    success := itemdata^.charcount <> MAX(CARDINAL);
                    IF NOT success THEN
                        Strings.Assign ("554 connection lost", FailureReason);
                    END (*IF*);
                END (*IF*);

            ELSE
                Strings.Assign ("554 could not create message file", FailureReason);
                SkipMessage (SB, sem);
                itemdata^.TempName[0] := Nul;
            END (*IF*);

        ELSE
            Strings.Assign ("554 no recipients", FailureReason);
            SkipMessage (SB, sem);
        END (*IF*);

        IF NOT success AND (itemdata^.TempName[0] <> Nul) THEN
            FileSys.Remove (itemdata^.TempName, dummy);
            itemdata^.TempName[0] := Nul;
        END (*IF*);

        IF success AND LogSMTPItems THEN
            WriteLogItem (itemdata);
        END (*IF*);

        Signal (sem);
        RETURN success;

    END AcceptMessage;

(************************************************************************)

PROCEDURE OldMakeRecipientListFile (stage: CARDINAL;  desc: ItemDescriptor;
                                 VAR (*OUT*) filename: FilenameString);

    VAR cid: ChanId;  BaseName: FilenameString;

    BEGIN
        ChooseIncomingFileDirectory (desc^.Recipients, BaseName);
        cid := OpenNewOutputFile (BaseName, ".REC", filename);
        IF cid <> NoSuchChannel THEN
            IF stage = 0 THEN
                IPToString (desc^.RealIPAddr, TRUE, desc^.returnpath);
            END (*IF*);
            FWriteString (cid, desc^.returnpath);  FWriteLn (cid);
            WriteRecipientList (cid, desc^.Recipients, TRUE, FALSE);
            CloseFile (cid);
            IF stage = 0 THEN
                ResetReturnPath (desc);
            END (*IF*);
        END (*IF*);
    END OldMakeRecipientListFile;

(************************************************************************)

PROCEDURE NewMakeRecipientListFile (stage: CARDINAL;  desc: ItemDescriptor;
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
    END NewMakeRecipientListFile;

(************************************************************************)

PROCEDURE OldRebuildRecipientList (desc: ItemDescriptor;
                                filename: FilenameString);

    (* Discards the current recipients for desc^, and creates a new set *)
    (* of recipients from "filename".  The first line in that file is   *)
    (* ignored.                                                         *)

    CONST CtrlZ = CHR(26);

    VAR cid: ChanId;  MoreToGo: BOOLEAN;
        name: UserName;

    BEGIN
        IF filename[0] <> Nul THEN
            ClearCombinedRecipientList (desc^.Recipients);
            cid := OpenOldFile (filename, FALSE, FALSE);
            IF cid <> NoSuchChannel THEN
                ReadLine (cid, desc^.returnpath);
                MoreToGo := desc^.returnpath[0] <> CtrlZ;
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
    END OldRebuildRecipientList;

(************************************************************************)

PROCEDURE NewRebuildRecipientList (desc: ItemDescriptor;
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

    END NewRebuildRecipientList;

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

(********************************************************************************)

PROCEDURE ExtractSeparateFilterUsers (itemdata: ItemDescriptor;
                             VAR (*INOUT*) FilterName: FilenameString);

    (* Pulls out those local recipients whose filter is different from  *)
    (* that required for the remaining recipients, and submits them     *)
    (* as "recirculate" jobs so that they will be filtered on the       *)
    (* second time around.  On return, all items require the same       *)
    (* filter.  This is not necessarily the same filter as when this    *)
    (* procedure was called, because it might turn out that all         *)
    (* recipients require the same separate filter.                     *)

    VAR NewCRL: CombinedRecipientList;
        cid: ChanId;
        found: BOOLEAN;
        OriginalFilter, NewDir, NewFile: FilenameString;

    BEGIN
        OriginalFilter := FilterName;
        REPEAT
            found := SeparateFilter (itemdata^.Recipients, NewCRL,
                                                           FilterName);
            IF found THEN
                ChooseIncomingFileDirectory (NewCRL, NewDir);

                (* Make a copy of the message file.  By opening and     *)
                (* closing the new file, we duck around critical        *)
                (* section problems by ensuring that another thread     *)
                (* can't choose the same file name before we get a      *)
                (* chance to do the copy.                               *)

                cid := OpenNewOutputFile (NewDir, ".###", NewFile);
                CloseFile (cid);
                EVAL (OS2.DosCopy (itemdata^.TempName,
                                        NewFile, OS2.DCPY_EXISTING));

                (* QUESTION: What do we need to do about the firstrecipient *)
                (* and recipientcount fields in the itemdata^ record?  My   *)
                (* guess is that they're no longer relevant by the time     *)
                (* we're up to filtering, but I should confirm this.        *)

                IF EmptyRecipientList (itemdata^.Recipients) THEN
                    DiscardCombinedRecipientList (itemdata^.Recipients);
                    itemdata^.Recipients := NewCRL;
                    DeleteFile (itemdata^.TempName);
                    itemdata^.TempName := NewFile;
                    found := FALSE;
                ELSE
                    CopyToRecipients (NewFile, itemdata^.returnpath,
                         itemdata^.offset, itemdata^.LogID, NewCRL, TRUE);
                    DiscardCombinedRecipientList (NewCRL);
                    FilterName := OriginalFilter;
                END (*IF*);

            END (*IF*);
        UNTIL NOT found;

    END ExtractSeparateFilterUsers;

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

    VAR j, k, result: CARDINAL;  found, Serialise: BOOLEAN;
        FilterName, recipients, ArgString, newfile: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        cid, cid2: ChanId;
        LineBuffer, RPBuffer: ARRAY [0..1023] OF CHAR;

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

        IF OldFilterRules THEN
            OldMakeRecipientListFile (stage, itemdata, recipients);
        ELSE
            NewMakeRecipientListFile (stage, itemdata, recipients);
        END (*IF*);
        ArgString := "CMD /C ";
        Strings.Append (FilterName, ArgString);
        Strings.Append (" ", ArgString);
        IF OldFilterRules THEN
            Strings.Append (itemdata^.TempName, ArgString);
            Strings.Append (" ", ArgString);
            Strings.Append (recipients, ArgString);
        ELSE
            Strings.Append (recipients, ArgString);
            IF itemdata^.TempName[0] <> Nul THEN
                Strings.Append (" ", ArgString);
                Strings.Append (itemdata^.TempName, ArgString);
            END (*IF*);
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
            IF OldFilterRules THEN
                OldRebuildRecipientList (itemdata, recipients);
            ELSE
                NewRebuildRecipientList (itemdata, recipients);
            END (*IF*);
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

(********************************************************************************)

PROCEDURE RunFinalFilter (itemdata: ItemDescriptor;
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
    (*   16    not used at this stage.                                  *)
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

    CONST stage = 4;  ONLength = 256;  Tab = CHR(9);  MaxResultCode = 16;
    TYPE ResultCodes = SET OF [0..MaxResultCode];
    CONST LegalResultCodes = ResultCodes {0..4};

    VAR j, k, result: CARDINAL;  found, Serialise: BOOLEAN;
        FilterName, recipients, ArgString, newfile: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        cid, cid2: ChanId;
        LineBuffer, RPBuffer: ARRAY [0..1023] OF CHAR;

    BEGIN
        IF itemdata^.SkipFiltering THEN
            RETURN 0;
        END (*IF*);

        Obtain (FilterProgLock);
        FilterName := FilterProg[stage];
        Release (FilterProgLock);

        (* Some of the users might require different filters from the   *)
        (* default, or even no filter.  ExtractSeparateFilterUsers      *)
        (* trims down the list of users (and might change the filter    *)
        (* name) so that what we have left is one or more users all of  *)
        (* whom require the same filter.  The same procedure creates    *)
        (* separate copies of the mail for the trimmed-out users, and   *)
        (* recirculates those jobs as separate jobs.                    *)

        (*CheckFilterOverride (itemdata^.Recipients, FilterName);*)

        ExtractSeparateFilterUsers (itemdata, FilterName);

        (* If no filter specified, do no filtering. *)

        IF FilterName[0] = Nul THEN
            RETURN 0;
        END (*IF*);

        IF OldFilterRules THEN
            OldMakeRecipientListFile (stage, itemdata, recipients);
        ELSE
            NewMakeRecipientListFile (stage, itemdata, recipients);
        END (*IF*);
        ArgString := "CMD /C ";
        Strings.Append (FilterName, ArgString);
        Strings.Append (" ", ArgString);
        IF OldFilterRules THEN
            Strings.Append (itemdata^.TempName, ArgString);
            Strings.Append (" ", ArgString);
            Strings.Append (recipients, ArgString);
        ELSE
            Strings.Append (recipients, ArgString);
            IF itemdata^.TempName[0] <> Nul THEN
                Strings.Append (" ", ArgString);
                Strings.Append (itemdata^.TempName, ArgString);
            END (*IF*);
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
            IF OldFilterRules THEN
                OldRebuildRecipientList (itemdata, recipients);
            ELSE
                NewRebuildRecipientList (itemdata, recipients);
            END (*IF*);
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
(*                                                                      *)
(* The following procedure has been replaced by separate procedures     *)
(* RunFilter03 and RunFinalFilter, but I'm retaining the original code  *)
(* here in case I decide to reverse the split.                          *)
(*                                                                      *)
(************************************************************************)

(*
PROCEDURE RunFilter (stage: CARDINAL;  itemdata: ItemDescriptor;
                     VAR (*OUT*) FailureMessage: ARRAY OF CHAR): CARDINAL;

    (* This procedure may be invoked at several stages:                 *)
    (*                                                                  *)
    (*    0    on initial connection                                    *)
    (*    1    after the HELO or EHLO command                           *)
    (*    2    after the MAIL FROM command                              *)
    (*    3    when the DATA command has been received, but before the  *)
    (*           message has been transmitted.                          *)
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
    (*   16    like 0, but in addition future filtering steps will be   *)
    (*           bypassed for this item of mail.                        *)
    (*                                                                  *)
    (* As a guard against errors in the filters, unused codes are       *)
    (* converted to 0.                                                  *)
    (*                                                                  *)
    (* Note that cases 1, 4, and 16 are dealt with internally in        *)
    (* this procedure, so this procedure will only ever return a        *)
    (* result of 0, 2, or 3.  Furthermore, code 2 should never occur    *)
    (* at stage 0, 1, 2, or 3.                                          *)
    (*                                                                  *)
    (* In case 3, FailureMessage holds the reply (starting with a       *)
    (* three-digit code) to be returned to the sender.                  *)

    CONST ONLength = 256;  Tab = CHR(9);  MaxResultCode = 16;
    TYPE ResultCodes = SET OF [0..MaxResultCode];
    CONST LegalResultCodes = ResultCodes {0..4, 16};

    VAR j, k, result: CARDINAL;  found, Serialise: BOOLEAN;
        FilterName, recipients, ArgString, newfile: FilenameString;
        FailureObjectName: ARRAY [0..ONLength-1] OF CHAR;
        ExitStatus: OS2.RESULTCODES;
        cid, cid2: ChanId;
        LineBuffer, RPBuffer: ARRAY [0..1023] OF CHAR;

    BEGIN
        IF itemdata^.SkipFiltering THEN
            RETURN 0;
        END (*IF*);

        Obtain (FilterProgLock);
        FilterName := FilterProg[stage];
        Release (FilterProgLock);

        (* If stage = 4, we can override the default filter iff there   *)
        (* is a unique local user and the override is enabled for       *)
        (* that user.                                                   *)

        IF stage = 4 THEN
            CheckFilterOverride (itemdata^.Recipients, FilterName);
        END (*IF*);

        (* If no filter specified, do nothing. *)

        IF FilterName[0] = Nul THEN
            RETURN 0;
        END (*IF*);

        IF OldFilterRules THEN
            OldMakeRecipientListFile (stage, itemdata, recipients);
        ELSE
            NewMakeRecipientListFile (stage, itemdata, recipients);
        END (*IF*);
        ArgString := "CMD /C ";
        Strings.Append (FilterName, ArgString);
        Strings.Append (" ", ArgString);
        IF OldFilterRules THEN
            Strings.Append (itemdata^.TempName, ArgString);
            Strings.Append (" ", ArgString);
            Strings.Append (recipients, ArgString);
        ELSE
            Strings.Append (recipients, ArgString);
            IF itemdata^.TempName[0] <> Nul THEN
                Strings.Append (" ", ArgString);
                Strings.Append (itemdata^.TempName, ArgString);
            END (*IF*);
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
            IF OldFilterRules THEN
                OldRebuildRecipientList (itemdata, recipients);
            ELSE
                NewRebuildRecipientList (itemdata, recipients);
            END (*IF*);
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

            (* The apparent duplication below could probably be removed *)
            (* by more efficient code, but I'm leaving options open for *)
            (* future expansion.  Note also that, to ensure conformance *)
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

            cid := OpenOldFile (itemdata^.TempName, FALSE);
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

        (* The following code is obsolete, but I'm temporarily          *)
        (* saving it until the new code has been tested.                *)

        (*
        IF (stage > 3) AND (result <= 2) THEN

            (* Since the filter might have altered or removed the   *)
            (* Return-Path header line, we need to recompute the    *)
            (* offset.                                              *)

            cid := OpenOldFile (itemdata^.TempName, FALSE);
            IF cid <> NoSuchChannel THEN
                itemdata^.offset := StartPosition(cid);
                SetPosition (cid, itemdata^.offset);
                ReadLine (cid, LineBuffer);
                IF KeywordMatch ("Return-Path", LineBuffer) THEN
                    itemdata^.offset := CurrentPosition(cid);

                    (* Note that the Return-Path line might have continuation lines. *)

                    REPEAT
                        ReadLine (cid, LineBuffer);
                        found := (LineBuffer[0] = ' ') OR (LineBuffer[0] = Tab);
                        IF found THEN
                            itemdata^.offset := CurrentPosition(cid);
                        END (*IF*);
                    UNTIL NOT found;

                END (*IF*);
                CloseFile (cid);
            END (*IF*);

        END (*IF*);
        *)

        RETURN result;

    END RunFilter;
*)

(************************************************************************)

PROCEDURE DistributeMessage (itemdata: ItemDescriptor);

    (* This procedure can be called after AcceptMessage has read the    *)
    (* whole message.  Now we put it into the local mailboxes, and/or   *)
    (* relay it, depending on the recipients.                           *)

    BEGIN
        WITH itemdata^ DO
            CopyToRecipients (TempName, returnpath, offset, LogID,
                              Recipients, FALSE);
            TempName[0] := Nul;
        END (*WITH*);
    END DistributeMessage;

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
        key := "weasel.ini";
        hini := OpenINIFile (key, UseTNI);
        IF INIData.INIValid (hini) THEN

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
            CloseINIFile (hini);
        END (*IF*);
    END UpdateINIData;

(************************************************************************)

PROCEDURE LoadSMTPINIData (TNImode: BOOLEAN);

    (* Initial load of configuration data for this module. *)

    TYPE CharSet = SET OF CHAR;

    CONST Alphanumeric = CharSet {'0'..'9', 'A'..'Z'};

    VAR hini: INIData.HINI;  j: [0..7];
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        app := "$SYS";
        key := "weasel.ini";
        UseTNI := TNImode;
        hini := OpenINIFile (key, UseTNI);
        IF INIData.INIValid (hini) THEN

            key := "SerialiseFilters";
            EVAL (INIGet (hini, app, key, SerialiseFilters));
            FOR j := 0 TO 4 DO
                FilterProg[j] := "";
            END (*FOR*);

            (* Transition rules to compensate for the new filter rules. *)

            OldFilterRules := FALSE;
            key := "FilterProg";
            IF INIGetString (hini, app, key, FilterProg[4]) THEN
                OldFilterRules := TRUE;
            ELSE
                key := "FilterProg4";
                IF NOT INIGetString (hini, app, key, FilterProg[4]) THEN
                    key := "FilterProg2";
                    EVAL (INIGetString (hini, app, key, FilterProg[4]));
                    key := "FilterProg2";
                    INIPutString (hini, app, key, FilterProg[2]);
                    key := "FilterProg4";
                    INIPutString (hini, app, key, FilterProg[4]);
                END (*IF*);
            END (*IF*);

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

            key := "MAILFROMcheck";
            EVAL (INIGet (hini, app, key, MAILFROMcheck));
            CloseINIFile (hini);
        END (*IF*);
        UpdateINIData;
    END LoadSMTPINIData;

(************************************************************************)

VAR hini: INIData.HINI;
    app: ARRAY [0..4] OF CHAR;
    key: ARRAY [0..10] OF CHAR;

BEGIN
    CRLF[0] := CR;  CRLF[1] := LF;
    UseTNI := FALSE;
    CreateLock (LogFileLock);
    LogSMTPItems := FALSE;
    SerialiseFilters := TRUE;
    CreateLock (FilterAccess);
    CreateLock (FilterProgLock);
    CreateLock (NextNameLock);
FINALLY
    app := "$SYS";
    key := "weasel.ini";
    hini := OpenINIFile (key, UseTNI);
    IF INIData.INIValid (hini) THEN
        Obtain (NextNameLock);
        app := "$SYS";  key := "UName";
        INIPut (hini, app, key, NextName);
        Release (NextNameLock);
        CloseINIFile (hini);
    END (*IF*);
END SMTPData.

