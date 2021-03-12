(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2021   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE POPData;

        (********************************************************)
        (*                                                      *)
        (*        Main data operations on a POP mailbox         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            22 April 1998                   *)
        (*  Last edited:        11 February 2021                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CAST, LOC, CARD8, ADR;

IMPORT Strings, FileSys, OS2, INIData;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SendChar, SendString, SendEOL, SendLine, SendRaw, FlushOutput;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Watchdog IMPORT
    (* type *)  WatchdogID,
    (* proc *)  KickWatchdog;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM MiscFuncs IMPORT
    (* proc *)  ToLower;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos, DirectoryEntry,
    (* proc *)  FirstDirEntry, NextDirEntry, DirSearchDone,
                OpenNewFile, OpenNewFile1, OpenOldFile, CloseFile,
                DeleteFile, ReadLine, SetPosition, ReadRaw, WriteRaw;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM Names IMPORT
    (* type *)  UserName, PassString, FilenameString;

FROM MailAccounts IMPORT
    (* type *)  DomainList,
    (* proc *)  BuildDomainList, MakeSingletonList, DiscardDomainList,
                NonEmptyList, StepToNextDomain, CurrentDomain,
                CurrentPassword;

FROM Domains IMPORT
    (* type *)  Domain,
    (* const*)  LockFileName,
    (* proc *)  MailDirectoryFor(*, POPFetchLatestOption*);

(************************************************************************)

(*
VAR debugid: TransactionLogID;

PROCEDURE SetDebugID (ID: TransactionLogID);
    BEGIN
        debugid := ID;
    END SetDebugID;
*)

CONST
    Nul = CHR(0);
    CacheSize = 64;
    DescrFileName = "MSGLIST.DAT";

TYPE

    (* File name without full path or extension.                        *)

    ShortFilename = ARRAY [0..7] OF CHAR;

    DescrPointer = POINTER TO DescriptorRecord;

    (* We have one of these records for each message in a mailbox.      *)
    (* The fields are:                                                  *)
    (*      size         size in characters of this message             *)
    (*      name         full filename of this message                  *)
    (*      shortname    relative filename of this message,             *)
    (*                        without the ".MSG"                        *)
    (*      ToBeDeleted  TRUE iff this message is marked for deletion   *)

    DescriptorRecord = RECORD
                           size: CARDINAL;
                           shortname: ShortFilename;
                           ToBeDeleted: BOOLEAN;
                       END (*RECORD*);

    DescriptorCache = ARRAY [1..CacheSize] OF DescriptorRecord;

    (* A mailbox is the collection of waiting messages for one user.    *)
    (* The fields are:                                                  *)
    (*   name             the user's login name                         *)
    (*   domains          the list of domains to which this user might  *)
    (*                        belong                                    *)
    (*   directory        file filename of the user's mailbox directory *)
    (*   TotalMessages    the number of messages in the mailbox,        *)
    (*                       including the ones marked for deletion     *)
    (*   NumberOfMessages the number of messages in the mailbox, not    *)
    (*                       including the ones marked for deletion     *)
    (*   UndeletedBytes   the sum of file sizes for all messages in the *)
    (*                       mailbox, except those marked for deletion  *)
    (*   MessageNoOffset  (Message number - 1) of the first entry in    *)
    (*                       the descriptor cache                       *)
    (*   DescrCache       the descriptors currently in main memory      *)
    (*   descrcid         handle for the descriptor file                *)
    (*   CacheChanged     TRUE iff the cache contents have changed      *)
    (*                         since the last write to disk.            *)
    (*   HaveLock         TRUE iff we have obtained exclusive access to *)
    (*                       this mailbox.                              *)
    (*   usingdiskfile    TRUE iff there are more messages than the     *)
    (*                       descriptor cache can hold.                 *)
    (*   overflow         TRUE iff we have had to ignore some messages  *)
    (*                       because UndeletedBytes would have          *)
    (*                       exceeded MAX(CARDINAL).                    *)

    Mailbox = POINTER TO MailboxRecord;
    MailboxRecord = RECORD
                        name: UserName;
                        domains: DomainList;
                        directory: FilenameString;
                        TotalMessages: CARDINAL;
                        NumberOfMessages: CARDINAL;
                        UndeletedBytes: CARDINAL;
                        MessageNoOffset: CARDINAL;
                        DescrCache: DescriptorCache;
                        descrcid: CARDINAL;
                        CacheChanged: BOOLEAN;
                        HaveLock: BOOLEAN;
                        usingdiskfile: BOOLEAN;
                        overflow: BOOLEAN;
                    END (*RECORD*);

(************************************************************************)

VAR UseTNI: BOOLEAN;

(************************************************************************)

PROCEDURE AlignCache (M: Mailbox;  N: CARDINAL): BOOLEAN;

    (* Reloads the descriptor cache, if necessary, to ensure that the   *)
    (* Nth descriptor is in the cache.  Returns FALSE if there is no    *)
    (* mailbox or if the message number is too large.                   *)

    VAR desiredoffset, amount: CARDINAL;
        pos: FilePos;

    BEGIN
        IF (M = NIL) OR (N < 1) OR (N > M^.TotalMessages) THEN
            RETURN FALSE;
        END (*IF*);

        IF M^.usingdiskfile THEN
            DEC (N);
            desiredoffset := N - (N MOD CacheSize);
            IF M^.MessageNoOffset <> desiredoffset THEN
                amount := SIZE (DescriptorCache);

                IF M^.CacheChanged THEN

                    (* Write back current cache contents.  *)

                    pos.high := 0;
                    pos.low := (M^.MessageNoOffset DIV CacheSize) * amount;
                    SetPosition (M^.descrcid, pos);
                    WriteRaw (M^.descrcid, M^.DescrCache, amount);
                END (*IF*);

                (* Reload the cache. *)

                pos.high := 0;
                pos.low := (desiredoffset DIV CacheSize) * amount;
                SetPosition (M^.descrcid, pos);
                ReadRaw (M^.descrcid, M^.DescrCache, amount, amount);
                M^.MessageNoOffset := desiredoffset;
                M^.CacheChanged := FALSE;

            END (*IF*);
        ELSE
            (* no need to do anything. *)
        END (*IF*);
        RETURN TRUE;

    END AlignCache;

(************************************************************************)

PROCEDURE MessageDescriptor (M: Mailbox;  N: CARDINAL): DescrPointer;

    (* Returns a pointer to the Nth message in mailbox M; the result is *)
    (* NIL if there is no Nth message.                                  *)

    VAR p: DescrPointer;

    BEGIN
        IF AlignCache (M, N) THEN
            p := ADR (M^.DescrCache[(N-1) MOD CacheSize + 1]);
        ELSE
            p := NIL;
        END (*IF*);
        RETURN p;
    END MessageDescriptor;

(************************************************************************)

PROCEDURE ClearMessageInfo (VAR (*INOUT*) M: Mailbox);

    (* Discards the cached message information, without committing any  *)
    (* changes.  We retain the domain list and the directory name,      *)
    (* also the lock on the user directory.                             *)

    VAR fname: FilenameString;

    BEGIN
        WITH M^ DO
            IF usingdiskfile THEN
                CloseFile (descrcid);
                fname := directory;
                Strings.Append (DescrFileName, fname);
                DeleteFile (fname);
            END (*IF*);
            descrcid := NoSuchChannel;
            usingdiskfile := FALSE;
            overflow := FALSE;
            CacheChanged := FALSE;
            TotalMessages := 0;
            NumberOfMessages := 0;
            UndeletedBytes := 0;
            MessageNoOffset := 0;
        END (*WITH*);
    END ClearMessageInfo;

    (* Fields not affected by the above code:

                      name: UserName;
                      domains: DomainList;
                      directory: FilenameString;
                      DescrCache: DescriptorCache;
                      HaveLock: BOOLEAN;
    *)

(************************************************************************)

PROCEDURE InitialCacheLoad (M: Mailbox;
                            VAR (*IN*) D: DirectoryEntry): CARDINAL;

    (* Loads one cache-load of message descriptors, and updates the     *)
    (* counts for number of messages and total bytes.   On entry D has  *)
    (* information about the first message.                             *)

    VAR count, pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        count := 0;
        REPEAT

            IF M^.UndeletedBytes > MAX(CARDINAL) - D.size.low THEN
                M^.overflow := TRUE;
            ELSE
                INC (count);
                WITH M^.DescrCache[count] DO
                    size := D.size.low;
                    Strings.Assign (D.name, shortname);
                    Strings.FindNext (".", shortname, 0, found, pos);
                    IF found THEN
                        shortname[pos] := Nul;
                    END (*IF*);
                    ToBeDeleted := FALSE;
                END (*WITH*);
                INC (M^.NumberOfMessages);
                INC (M^.UndeletedBytes, M^.DescrCache[count].size);
            END (*IF*);

        UNTIL M^.overflow OR (count >= CacheSize) OR NOT NextDirEntry (D);

        RETURN count;

    END InitialCacheLoad;

(************************************************************************)

PROCEDURE BuildDescriptorArray (M: Mailbox);

    (* Assumption: the M^.directory is already set up, and the message  *)
    (* list of M is empty.  This procedure fills the message list, and  *)
    (* updates the counts for number of messages and total bytes.       *)

    VAR SearchString: FilenameString;  D: DirectoryEntry;
        count : CARDINAL;
        MoreToGo, duplicate: BOOLEAN;
        name: FilenameString;

    BEGIN
        SearchString := M^.directory;
        Strings.Append ("*.MSG", SearchString);
        M^.MessageNoOffset := 0;

        M^.usingdiskfile := FALSE;
        MoreToGo := FirstDirEntry (SearchString, FALSE, FALSE, FALSE, D);
        WHILE MoreToGo DO
            count := InitialCacheLoad (M, D);
            MoreToGo := (count = CacheSize) AND NextDirEntry (D);
            IF MoreToGo OR (M^.usingdiskfile AND (count > 0)) THEN

                (* There are more entries than can fit in the cache,    *)
                (* so we have to write the current data out to disk.    *)

                IF NOT M^.usingdiskfile THEN

                    (* Create the file. *)

                    name := M^.directory;
                    Strings.Append (DescrFileName, name);
                    M^.descrcid := OpenNewFile1 (name, duplicate);
                    IF duplicate THEN
                        M^.descrcid := OpenOldFile (name, TRUE, TRUE);
                    END (*IF*);
                    M^.usingdiskfile := TRUE;
                END (*IF*);

                WriteRaw (M^.descrcid, M^.DescrCache, SIZE(DescriptorCache));
                IF MoreToGo THEN
                    INC (M^.MessageNoOffset, CacheSize);
                END (*IF*);

                (* Remark: if the number of messages is not an exact    *)
                (* multiple of CacheSize, the last record written out   *)
                (* will contain trailing rubbish.  I consider this to   *)
                (* be an acceptable overhead - we just have to ignore   *)
                (* the rubbish when reading back from the file.         *)

                IF M^.overflow THEN MoreToGo := FALSE END(*IF*);

            END (*IF*);

        END (*WHILE*);

        DirSearchDone (D);
        M^.TotalMessages := M^.NumberOfMessages;
        M^.CacheChanged := FALSE;

    END BuildDescriptorArray;

(************************************************************************)

PROCEDURE LoadUserData (VAR (*IN*) M: Mailbox;
                                  VAR (*IN*) username: ARRAY OF CHAR;
                                         OurIPAddress: CARDINAL): BOOLEAN;

    (* Checks that the username is valid, and if so initialises the     *)
    (* user data part of M.  The caller has already set username to     *)
    (* lower case.  On entry username could be in the form user@domain  *)
    (* or user'domain or user%domain, but the 'domain' part is stripped *)
    (* off during processing.                                           *)

    VAR success: BOOLEAN;

    BEGIN
        M^.descrcid := NoSuchChannel;
        ClearMessageInfo (M);

        (* Build the list of domains to which this username might belong. *)

        M^.domains := BuildDomainList (username, OurIPAddress);

        (* We have a username match iff this list is nonempty. *)

        WITH M^ DO
            success := NonEmptyList(domains);
            IF success THEN
                Strings.Assign (username, name);
                directory[0] := Nul;
                HaveLock := FALSE;
            END (*IF*);
        END (*WITH*);

        RETURN success;

    END LoadUserData;

(************************************************************************)

PROCEDURE OpenMailbox (VAR (*INOUT*) M: Mailbox;
                               VAR (*IN*) username: ARRAY OF CHAR;
                                         OurIPAddress: CARDINAL): BOOLEAN;

    (* Creates a new Mailbox structure, throwing away any existing      *)
    (* user data in M.  Returns TRUE iff successful.                    *)

    BEGIN
        ToLower (username);
        IF M <> NIL THEN
            DiscardMailbox (M);
        END (*IF*);
        NEW (M);
        IF LoadUserData (M, username, OurIPAddress) THEN
            RETURN TRUE;
        ELSE
            DiscardMailbox (M);
            RETURN FALSE;
        END (*IF*);
    END OpenMailbox;

(************************************************************************)

PROCEDURE DiscardMailbox (VAR (*INOUT*) M: Mailbox);

    (* Throws away all data belonging to this Mailbox, and sets the     *)
    (* argument to NILMailbox.                                          *)

    VAR filename: FilenameString;
        dummy: BOOLEAN;

    BEGIN
        IF M <> NIL THEN
            DiscardDomainList (M^.domains);
            IF M^.usingdiskfile THEN
                CloseFile (M^.descrcid);
                filename := M^.directory;
                Strings.Append (DescrFileName, filename);
                FileSys.Remove (filename, dummy);
            END (*IF*);
            IF M^.HaveLock THEN
                filename := M^.directory;
                Strings.Append (LockFileName, filename);
                FileSys.Remove (filename, dummy);
            END (*IF*);
            DEALLOCATE (M, SIZE(MailboxRecord));
        END (*IF*);
    END DiscardMailbox;

(************************************************************************)

PROCEDURE LockMailbox (M: Mailbox;  LogID: TransactionLogID): CARDINAL;

    (* Attempts to lock the mailbox.  The possible results are          *)
    (*       0     OK, you have exclusive access to the mailbox         *)
    (*       1     not used by this procedure                           *)
    (*       2     can't access mailbox, it's already locked            *)
    (*       3     user directory does not exist                        *)
    (* We discard the domain list, which was only needed for password   *)
    (* checking.                                                        *)

    CONST MaxTries = 12;

    VAR filename: FilenameString;  tries: CARDINAL;  cid: ChanId;

    BEGIN
        filename := M^.directory;
        Strings.Append (LockFileName, filename);

        (* If mailbox is locked, keep checking for a short time in the  *)
        (* hope that the lock will soon be released.                    *)

        tries := 0;
        WHILE FileSys.Exists (filename) DO
            INC (tries);
            IF tries >= MaxTries THEN
                RETURN 2;
            END (*IF*);
            Sleep (400);
        END (*WHILE*);

        cid := OpenNewFile (filename, FALSE);
        IF cid <> NoSuchChannel THEN
            CloseFile (cid);
            M^.HaveLock := TRUE;
            DiscardDomainList (M^.domains);
            BuildDescriptorArray (M);
            IF M^.overflow THEN
                LogTransactionL (LogID, "Mailbox too big to list all files.");
            END (*IF*);
            RETURN 0;
        ELSE
            RETURN 3;
        END (*IF*);

    END LockMailbox;

(************************************************************************)

PROCEDURE ClaimMailbox (VAR (*INOUT*) M: Mailbox;
                               LogID: TransactionLogID;
                               VAR (*IN*) username: ARRAY OF CHAR;
                               D: Domain): CARDINAL;

    (* Like OpenMailbox followed by LockMailbox, but for the case where *)
    (* we've already authenticated the user.                            *)

    BEGIN

        ToLower (username);
        IF M <> NIL THEN
            DiscardMailbox (M);
        END (*IF*);

        NEW (M);
        M^.usingdiskfile := FALSE;
        ClearMessageInfo (M);

        WITH M^ DO
            Strings.Assign (username, name);
            domains := MakeSingletonList (D);
            HaveLock := FALSE;
            MailDirectoryFor (D, directory);
            Strings.Append (name, directory);
            Strings.Append ('\', directory);
        END (*WITH*);
        RETURN LockMailbox (M, LogID);

    END ClaimMailbox;

(************************************************************************)

PROCEDURE PasswordOK (M: Mailbox;  VAR (*IN*) password: ARRAY OF CHAR;
                                   VAR (*OUT*) D: Domain;
                                   LogID: TransactionLogID): CARDINAL;

    (* Locks the mailbox if the password is correct.  The possible      *)
    (* results are                                                      *)
    (*       0     OK, you have exclusive access to the mailbox         *)
    (*       1     password not acceptable                              *)
    (*       2     can't access mailbox, it's already locked            *)
    (*       3     password is OK but user directory does not exist     *)

    VAR head: DomainList;  pwd: PassString;

    BEGIN
        D := NIL;
        IF M = NIL THEN
            RETURN 1;
        ELSE
            head := M^.domains;
            IF NOT NonEmptyList(head) THEN
                RETURN 1;
            END (*IF*);
            LOOP
                CurrentPassword (M^.domains, pwd);
                IF Strings.Equal (password, pwd) THEN
                    WITH M^ DO
                        D := CurrentDomain(domains);
                        MailDirectoryFor (D, directory);
                        Strings.Append (name, directory);
                        Strings.Append ('\', directory);
                    END (*WITH*);
                    RETURN LockMailbox (M, LogID);
                ELSE
                    StepToNextDomain (M^.domains);
                    IF M^.domains = head THEN
                        RETURN 1;
                    END (*IF*)
                END (*IF*);
            END (*LOOP*);
        END (*IF*);
    END PasswordOK;

(************************************************************************)

PROCEDURE APOPCheck (M: Mailbox;  LogID: TransactionLogID;
                       VAR (*IN*) digeststring, TimeStamp: ARRAY OF CHAR;
                       VAR (*OUT*) D: Domain): CARDINAL;

    (* Locks the mailbox if the MD5 digest string is correct.  The      *)
    (* possible results are                                             *)
    (*       0     OK, you have exclusive access to the mailbox         *)
    (*       1     digest string not acceptable                         *)
    (*       2     can't access mailbox, it's already locked            *)
    (*       3     password is OK but user directory does not exist     *)

    (********************************************************************)

    PROCEDURE CodeOf (hexchar: CHAR): CARDINAL;

        (* Converts a one-digit hexadecimal number. *)

        TYPE CharSet = SET OF CHAR;
        CONST Digits = CharSet {'0'..'9'};

        BEGIN
            IF hexchar IN Digits THEN
                RETURN ORD(hexchar) - ORD('0');
            ELSE
                RETURN ORD(CAP(hexchar)) - ORD('A') + 10;
            END (*IF*);
        END CodeOf;

    (********************************************************************)

    PROCEDURE ConvertDigest (VAR (*OUT*) result: ARRAY OF LOC);

        (* Converts the hexadecimal string digeststring to a *)
        (* 16-byte array result.                             *)

        VAR j: [0..15];

        BEGIN
            FOR j := 0 TO 15 DO
                result[j] := CAST (LOC, 16*CodeOf(digeststring[2*j])
                                        + CodeOf(digeststring[2*j+1]));
            END (*FOR*);
        END ConvertDigest;

    (********************************************************************)

    VAR ctx: MD5_CTX;
        supplied, computed: MD5_DigestType;
        head: DomainList;
        pwd: PassString;

    BEGIN
        D := NIL;
        IF (M = NIL) OR (LENGTH(digeststring) <> 32) THEN
            LogTransactionL (LogID, "Digest length error");
            RETURN 1;
        END (*IF*);
        head := M^.domains;
        IF NOT NonEmptyList(head) THEN
            RETURN 1;
        END (*IF*);
        ConvertDigest (supplied);
        LOOP
            ctx := MD5Init();
            MD5Update (ctx, TimeStamp, LENGTH(TimeStamp));
            CurrentPassword (M^.domains, pwd);
            MD5Update (ctx, pwd, LENGTH(pwd));
            MD5Final (ctx, computed);
            IF (supplied[0] = computed[0]) AND (supplied[1] = computed[1])
                                AND (supplied[2] = computed[2])
                                AND (supplied[3] = computed[3]) THEN
                WITH M^ DO
                    D := CurrentDomain(domains);
                    MailDirectoryFor (D, directory);
                    Strings.Append (name, directory);
                    Strings.Append ('\', directory);
                END (*WITH*);
                RETURN LockMailbox (M, LogID);
            ELSE
                StepToNextDomain (M^.domains);
                IF M^.domains = head THEN
                    RETURN 1;
                END (*IF*)
            END (*IF*);
        END (*LOOP*);
    END APOPCheck;

(********************************************************************************)

PROCEDURE NumberAndSize (M: Mailbox;  VAR (*OUT*) N, size: CARDINAL);

    (* Sets N to the number of messages in the mailbox, and size to     *)
    (* the total number of bytes in the messages.                       *)

    BEGIN
        IF M = NIL THEN
            N := 0;  size := 0;
        ELSE
            N := M^.NumberOfMessages;
            size := M^.UndeletedBytes;
        END (*IF*);
    END NumberAndSize;

(************************************************************************)

PROCEDURE MaxMessageNumber (M: Mailbox): CARDINAL;

    (* Returns the message number of the last message that is not  *)
    (* marked for deletion.                                        *)

    VAR N: CARDINAL;  current: DescrPointer;

    BEGIN
        N := M^.TotalMessages;
        LOOP
            current := MessageDescriptor (M, N);
            IF current = NIL THEN
                N := 0;
                EXIT(*LOOP*);
            END(*IF*);
            IF NOT current^.ToBeDeleted THEN
                EXIT(*LOOP*);
            END(*IF*);
            DEC (N);
        END (*LOOP*);
        RETURN N;
    END MaxMessageNumber;

(************************************************************************)

PROCEDURE SizeOfMessage (M: Mailbox;  MessageNumber: CARDINAL;
                                   VAR (*OUT*) size: CARDINAL): BOOLEAN;

    (* If message MessageNumber exists, sets size to its size and       *)
    (* returns TRUE.  Otherwise result is FALSE and size is undefined.  *)

    VAR p: DescrPointer;

    BEGIN
        p := MessageDescriptor (M, MessageNumber);
        IF (p = NIL) OR p^.ToBeDeleted THEN
            RETURN FALSE;
        ELSE
            size := p^.size;
            RETURN TRUE;
        END (*IF*);
    END SizeOfMessage;

(************************************************************************)

PROCEDURE GetUID (M: Mailbox;  MessageNumber: CARDINAL;
                                   VAR (*OUT*) UID: MD5_DigestType;
                                   lognames: BOOLEAN;
                                   ID: TransactionLogID): BOOLEAN;

    (* If message MessageNumber exists, sets UID to a persistent and    *)
    (* unique identifier for this message, and returns TRUE.  Otherwise *)
    (* result is FALSE and UID is undefined.                            *)

    VAR p: DescrPointer;  ctx: MD5_CTX;
        D: DirectoryEntry;
        name: FilenameString;

    BEGIN
        p := MessageDescriptor (M, MessageNumber);
        IF (p = NIL) OR p^.ToBeDeleted THEN
            RETURN FALSE;
        ELSE
            name := M^.directory;
            Strings.Append (p^.shortname, name);
            Strings.Append (".MSG", name);
            IF lognames THEN
                LogTransaction (ID, name);
            END (*IF*);
            EVAL (FirstDirEntry(name, FALSE, FALSE, FALSE, D));
            DirSearchDone (D);
            ctx := MD5Init();
            MD5Update (ctx, name, LENGTH(name));
            MD5Update (ctx, D.timePkd, 2);
            MD5Update (ctx, D.datePkd, 2);
            MD5Final (ctx, UID);
            RETURN TRUE;
        END (*IF*);
    END GetUID;

(************************************************************************)

PROCEDURE SendFile (SB: SBuffer;  watchID: WatchdogID;
                    VAR (*IN*) filename: ARRAY OF CHAR;
                    VAR (*OUT*) bytessent: CARDINAL;
                    lognames: BOOLEAN;
                      id: TransactionLogID): BOOLEAN;

    (* Sends the contents of a file via SB.  *)

    CONST
        CR = CHR(13);  LF = CHR(10);
        BufferSize = 8192;

    VAR success, MoreToGo, AtEOL: BOOLEAN;  amount, actual: CARDINAL;
        cid: ChanId;
        buffer: ARRAY [0..BufferSize-1] OF CHAR;

    BEGIN
        IF lognames THEN
            LogTransaction (id, filename);
        END (*IF*);
        cid := OpenOldFile (filename, FALSE, TRUE);
        success := cid <> NoSuchChannel;
        MoreToGo := TRUE;  AtEOL := TRUE;
        bytessent := 0;
        WHILE success AND MoreToGo DO
            KickWatchdog (watchID);
            ReadRaw (cid, buffer, BufferSize, amount);
            IF amount = 0 THEN

                MoreToGo := FALSE;

            ELSE

                AtEOL := (amount > 1) AND (buffer[amount-2] = CR)
                                       AND (buffer[amount-1] = LF);
                success := SendRaw (SB, buffer, amount, actual);
                INC (bytessent, actual);

            END (*IF*);

        END (*WHILE*);
        CloseFile (cid);

        IF success THEN
            IF NOT AtEOL THEN
                success := SendEOL(SB, actual);
                INC (bytessent, actual);
            END (*IF*);
            amount := 0;  actual := 0;
            success := success AND SendChar(SB, '.', amount) AND SendEOL(SB, actual);
            INC (bytessent, amount+actual);
        END (*IF*);
        INC (bytessent, FlushOutput (SB));

        RETURN success;

    END SendFile;

(************************************************************************)

PROCEDURE SendPartFile (SB: SBuffer;  watchID: WatchdogID;
                            VAR (*IN*) filename: ARRAY OF CHAR;
                            MaxLines: CARDINAL;
                            lognames: BOOLEAN;
                              id: TransactionLogID): BOOLEAN;

    (* Sends the header, plus MaxLines of the body, via SB.     *)

    CONST CtrlZ = CHR(26);

    VAR success, MoreToGo, PastHeader: BOOLEAN;
        cid: ChanId;
        buffer: ARRAY [0..2047] OF CHAR;
        lines, sent: CARDINAL;

    BEGIN
        IF lognames THEN
            LogTransaction (id, filename);
        END (*IF*);
        lines := 0;  PastHeader := FALSE;
        cid := OpenOldFile (filename, FALSE, FALSE);
        success := cid <> NoSuchChannel;
        MoreToGo := TRUE;
        WHILE success AND MoreToGo DO
            KickWatchdog (watchID);
            ReadLine (cid, buffer);
            IF buffer[0] = CtrlZ THEN

                MoreToGo := FALSE;

            ELSE

                success := SendString (SB, buffer, sent) AND SendEOL(SB, sent);

                IF PastHeader THEN
                    INC (lines);
                    MoreToGo := lines < MaxLines;
                ELSIF buffer[0] = Nul THEN
                    PastHeader := TRUE;
                    MoreToGo := MaxLines > 0;
                END (*IF*);

            END (*IF*);

        END (*WHILE*);
        CloseFile (cid);

        IF success THEN
            buffer[0] := '.';  buffer[1] := Nul;
            success := SendLine (SB, buffer, sent);
        END (*IF*);
        EVAL (FlushOutput (SB));

        RETURN success;

    END SendPartFile;

(************************************************************************)

PROCEDURE SendMessage (SB: SBuffer;  watchID: WatchdogID;  M: Mailbox;
                               N, MaxLines: CARDINAL;
                               VAR (*OUT*) bytessent: CARDINAL;
                               lognames: BOOLEAN;
                                         id: TransactionLogID): BOOLEAN;

    (* Sends message N in mailbox N via SB.  The caller must            *)
    (* already have confirmed that this message exists.                 *)
    (* MaxLines refers to the number of non-header lines to be sent.    *)
    (* We must call KickWatchdog every so often to ensure that the      *)
    (* operation does not time out.                                     *)
    (* A FALSE result means a communications failure.                   *)

    VAR success: BOOLEAN;  p: DescrPointer;
        name: FilenameString;

    BEGIN
        p := MessageDescriptor (M, N);
        IF (p = NIL) OR p^.ToBeDeleted THEN
            success := FALSE;
        ELSE
            name := M^.directory;
            Strings.Append (p^.shortname, name);
            Strings.Append (".MSG", name);
            IF MaxLines = MAX(CARDINAL) THEN
                success := SendFile (SB, watchID, name, bytessent, lognames, id);
            ELSE
                success := SendPartFile (SB, watchID, name, MaxLines, lognames, id);
                bytessent := 0;         (* dummy info *)
            END (*IF*);
        END (*IF*);
        RETURN success;
    END SendMessage;

(************************************************************************)

PROCEDURE MarkForDeletion (M: Mailbox;  N: CARDINAL;
                              VAR (*OUT*) MessageSize: CARDINAL): BOOLEAN;

    (* Marks message number N for deletion.  (The actual deletion       *)
    (* won't happen until a clean logout from the client.)  A return    *)
    (* value of FALSE means "no such message".                          *)

    VAR p: DescrPointer;

    BEGIN
        p := MessageDescriptor (M, N);
        IF p = NIL THEN
            MessageSize := 0;
            RETURN FALSE;
        ELSE
            MessageSize := p^.size;
            IF NOT p^.ToBeDeleted THEN
                p^.ToBeDeleted := TRUE;
                DEC (M^.NumberOfMessages);
                DEC (M^.UndeletedBytes, MessageSize);
                M^.CacheChanged := TRUE;
            END (*IF*);
            RETURN TRUE;
        END (*IF*);
    END MarkForDeletion;

(************************************************************************)

PROCEDURE UndeleteAll (M: Mailbox);

    (* Any messages in M that are marked for deletion are unmarked.     *)

    VAR N: CARDINAL;  p: DescrPointer;

    BEGIN
        IF M <> NIL THEN
            FOR N := 1 TO M^.TotalMessages DO
                p := MessageDescriptor (M, N);
                IF (p <> NIL) AND (p^.ToBeDeleted) THEN
                    p^.ToBeDeleted := FALSE;
                    INC (M^.NumberOfMessages);
                    INC (M^.UndeletedBytes, p^.size);
                    M^.CacheChanged := TRUE;
                END (*IF*);
            END (*FOR*);
        END (*IF*);
    END UndeleteAll;

(************************************************************************)

PROCEDURE CommitChanges (M: Mailbox);

    (* Deletes all files that have been marked for deletion.   *)

    VAR p: DescrPointer;
        N: CARDINAL;
        name: FilenameString;
        dummy: BOOLEAN;

    BEGIN
        IF M <> NIL THEN
            FOR N := 1 TO M^.TotalMessages DO
                p := MessageDescriptor (M, N);
                IF (p <> NIL) AND (p^.ToBeDeleted) THEN
                    name := M^.directory;
                    Strings.Append (p^.shortname, name);
                    Strings.Append (".MSG", name);
                    FileSys.Remove (name, dummy);
                END (*IF*);
            END (*FOR*);
        END (*IF*);
    END CommitChanges;

(************************************************************************)

BEGIN
    UseTNI := FALSE;
END POPData.

