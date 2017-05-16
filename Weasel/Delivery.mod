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

IMPLEMENTATION MODULE Delivery;

        (********************************************************)
        (*                                                      *)
        (* Part of the SMTP server - makes the final delivery   *)
        (*                   of the mail                        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 May 1998                     *)
        (*  Last edited:        7 May 2017                      *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT CAST, ADR, ADDRESS, CARD8, CARD16, CARD32;

IMPORT Strings, OS2, FileSys, INIData;

FROM SMTPLogin IMPORT
    (* proc *)  DoLogin, DoPOPLogin;

FROM Sockets IMPORT
    (* const*)  AF_INET, SOCK_STREAM, AF_UNSPEC, NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  socket, bind, connect, getsockname, soclose, sock_errno;

FROM Internet IMPORT
    (* const*)  Zero8,
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* type *)  HostEntPtr, AddressPointerArrayPointer,
    (* proc *)  gethostbyname;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, SendLine, SendChar,
                SendString, SendEOL, SendRaw, PositiveResponse, GetLastLine,
                SetTimeout, FlushOutput;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  RefreshMasterDomainList, DomainIsLocal, MailDirectoryFor,
                AddressIsLocal, RecomputeLocalDomainNames,
                SetPrincipalIPAddress, RefreshOurIPAddresses, OpenDomainINI,
                EnableDomainExtraLogging, NameOfDomain;

FROM MailAccounts IMPORT
    (* type *)  LocalUser,
    (* proc *)  LoadLocalUser, UnloadLocalUser, LockLocalUser, UnlockLocalUser,
                IsActiveUser, SetFilterOverride, NewMessageFilename;

FROM Hosts IMPORT
    (* proc *)  RefreshHostLists;

FROM Names IMPORT
    (* type *)  UserName, PassString, HostName, DomainName, CardArray,
                CardArray2, CardArray3,
                ServiceType, PathString, FilenameString;

FROM MXCheck IMPORT
    (* proc *)  DoMXLookup;

FROM Extra IMPORT
    (* proc *)  FullAddress, UserAndDomain;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  SetProcname, SetSyslogHost,
                StartTransactionLogging, CreateLogID, DiscardLogID,
                LogTransaction, LogTransactionL, UpdateTopScreenLine;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilePos, DirectoryEntry,
    (* proc *)  OpenOldFile, OpenNewFile1, OpenNewHiddenFile,
                OpenAtEnd, CloseFile, GetFileSize,
                HideFile, SetPosition, CurrentPosition,
                ReadRaw, ReadLine, WriteRaw, FWriteString, FWriteLn,
                FWriteChar, FirstDirEntry, NextDirEntry, DirSearchDone,
                DeleteFile;

FROM INIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  OpenINIFile, CloseINIFile, INIGet, INIGetString, INIPut,
                GetStringList, NextString, CloseStringList, INIValid,
                ItemSize, INIGetTrusted;

FROM InetUtilities IMPORT
    (* proc *)  Swap2, ConvertCardZ, AppendCard, ToLower,
                NameIsNumeric;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  IPToString, AddressToHostName, StringMatch, ConvertCard;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM MyClock IMPORT
    (* proc *)  CurrentDateAndTime, CurrentTimeToString;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached, ClearScreen, SetBoundary, WriteStringAt;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM RandCard IMPORT
    (* proc *)  RandInt;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM Types IMPORT
    (* type *)  CARD64, INT64;

FROM LONGLONG IMPORT
    (* proc *)  Diff64, Sub64, DEC64;

FROM TaskControl IMPORT
    (* type *)  Lock, NameString,
    (* proc *)  CreateTask, CreateTask1, CreateLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND, AddOffset, Copy;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE(*, SayHeapCount*);

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
    MaxVarRetries = 10;
    DefaultNumberOfDaemons = 16;
    MaxNumberOfDaemons = 64;
    JobCountLimitForRetry = 25;
    JobCountLimitLow = 50;
    JobCountLimitHigh = 200;
    OutboundRecipientLimit = 1000;
    RetryRecipientLimit = 200;
    semName = "\SEM32\WEASEL\RECEIVED";
    LoopbackAddress = 16777343;      (* 127.0.0.1 *)

TYPE
    CharSet = SET OF CHAR;
    Interval = ARRAY [0..MaxVarRetries] OF CARDINAL;

CONST
    Digits = CharSet {'0'..'9'};

    (* Time between retries, in minutes.  The table below gives the     *)
    (* interval for the first few retries.  After that, the last table  *)
    (* entry is used as a constant interval.                            *)

    RetryInterval = Interval {1, 5, 10, 20, 30, 40, 50, 60, 60, 150, 300};

    (* While testing the retry interval is shorter. *)

(*  RetryInterval = Interval {1,  1,  1,  1,  1,  1, 1, 300};  *)

TYPE
    (********************************************************************)
    (*                                                                  *)
    (*                         RECIPIENT LISTS                          *)
    (*                                                                  *)
    (* For each mail item, we keep separate lists of local and remote   *)
    (* recipients.                                                      *)
    (*                                                                  *)
    (********************************************************************)

    (* Descriptor for the mailbox of a local user.  This is not a       *)
    (* global record; for each incoming mail item, a new list of        *)
    (* recipients is built.                                             *)
    (*                                                                  *)
    (*     next         next local recipient for the same item          *)
    (*     U            the INI file data for this user                 *)
    (*     user         username - needed for detecting duplicates      *)
    (*     domainname   the domain name that was used in addressing     *)
    (*                      this user.                                  *)
    (*     domain       the domain corresponding to domainname          *)
    (*     DirName      name of the mailbox directory.                  *)
    (*     failuremessage  error message saying what went wrong, if the *)
    (*                     item couldn't be delivered to this recipient *)
    (*     Original     this is an address as supplied by the sender,   *)
    (*                     as distinct from one generated by an         *)
    (*                     internal process such as alias expansion.    *)
    (*     Skip         ignore this entry except when checking for      *)
    (*                     duplicates                                   *)

    LocalRecipientList = POINTER TO LocalRecipientRecord;
    LocalRecipient = LocalRecipientList;
    LocalRecipientRecord = RECORD
                               next: LocalRecipientList;
                               U: LocalUser;
                               user: UserName;
                               domainname: HostName;
                               domain: Domain;
                               DirName: FilenameString;
                               failuremessage: ARRAY [0..255] OF CHAR;
                               Original: BOOLEAN;
                               Skip: BOOLEAN;
                           END (*RECORD*);

    (* A list of remote recipients for a single mail item. *)

    RelayListPointer = POINTER TO RelayListEntry;
    RelayList = RelayListPointer;

    (* The fields in a relay list entry have the following meaning.     *)
    (*                                                                  *)
    (*   next            next recipient for this item                   *)
    (*   username, domain  destination e-mail address, separated into   *)
    (*                     its components                               *)
    (*   failuremessage  error message saying what went wrong, if the   *)
    (*                     item couldn't be delivered to this recipient *)
    (*   Original        this is an address as supplied by the sender,  *)
    (*                     as distinct from one generated by an         *)
    (*                     internal process such as alias expansion.    *)
    (*   KeepTrying      TRUE if we haven't exhausted the possibilities *)
    (*                     for delivery to this recipient.              *)
    (*   flag            temporary flag used in list processing         *)

    RelayListEntry = RECORD
                         next: RelayListPointer;
                         username: UserName;
                         domain: HostName;
                         failuremessage: ARRAY [0..255] OF CHAR;
                         Original: BOOLEAN;
                         KeepTrying, flag: BOOLEAN;
                     END (*RECORD*);

    (* A list of recipients for one mail item.  Note that there are     *)
    (* separate sublists for local and remote recipients.  Note also    *)
    (* that the remote list will sometimes also contain local           *)
    (* recipients, either because we haven't yet determined that they   *)
    (* are local or because we want to use the 'Recirculate' option.    *)

    (* RemoteCount is the total number of entries on the 'remote' list. *)
    (* We keep track of it because we want to put limits on the size    *)
    (* of a couple of internal queues.                                  *)

    (* After a 'send' attempt, we can end up with some recipients for   *)
    (* whom delivery was successful (so we remove them from the list);  *)
    (* some for whom delivery was unsuccessful but for which a retry    *)
    (* makes sense (so we keep them on the list); and some for whom a   *)
    (* retry would be pointless.  For a recipient in that last group,   *)
    (* we set KeepTrying to FALSE, and keep the record on the list only *)
    (* long enough to let us compose and send a failure message to the  *)
    (* sender.                                                          *)

    CombinedRecipientList = POINTER TO CRLHeader;
    CRLHeader = RECORD
                    local: LocalRecipientList;
                    remote: RelayList;
                    RemoteCount: CARDINAL;
                END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (*                 JOB DESCRIPTOR FOR A MAIL ITEM                   *)
    (*                                                                  *)
    (* An "OutJob" record is created for a mail item when it enters the *)
    (* delivery system, then it moves through various queues (see       *)
    (* below) as it passes through the system.  By the time it has      *)
    (* passed the Sorter module, a job has no local recipients (i.e.    *)
    (* the local deliveries have already been done), and either all     *)
    (* recipients are in the same domain, or they all have to be sent   *)
    (* through the same relay host.                                     *)
    (*                                                                  *)
    (********************************************************************)

    (* A list of mail items waiting to be sent.                         *)
    (*   next             next item on the list                         *)
    (*   sendtime         time that the item is due to be sent          *)
    (*   RetryNumber      number of times this has been tried already   *)
    (*   size             the size of the file (calculated but          *)
    (*                                          not yet used)           *)
    (*   ID               ID for transaction log                        *)
    (*   sender           the MAIL FROM parameter                       *)
    (*   domain           the mail domain and port to which to send     *)
    (*   SMTPport             this item.  Not meaningful until the      *)
    (*                        Sorter sets them.                         *)
    (*   LocalHost        our own hostname.  This can change as the     *)
    (*                        job proceeds, because we might use        *)
    (*                        different local interfaces for different  *)
    (*                        recipients.                               *)
    (*   file             name of the message file                      *)
    (*   offset           starting position within the file for the     *)
    (*                     data to be sent to the recipient.  (There    *)
    (*                     could be bookkeeping data before that,       *)
    (*                     which is not to be sent.)                    *)
    (*   sendto           list of recipients.  On initial job           *)
    (*                     submission this should contain only remote   *)
    (*                     recipients, but jobs that get recycled       *)
    (*                     within this module might have a mixture of   *)
    (*                     local and remote recipients.                 *)
    (*   NotifyOnFailure  TRUE if a failure to send this item should    *)
    (*                     cause a notification to be mailed back       *)
    (*                     to the sender.                               *)
    (*   Recirculate      TRUE if we want to relay this job back        *)
    (*                     through our own SMTP port, so that it gets   *)
    (*                     filtered.  This option is to support the     *)
    (*                     special cases where we have bypassed the     *)
    (*                     filtering step the first time around.  This  *)
    (*                     could lead to infinite looping if we're      *)
    (*                     not careful, but we're OK if the caller      *)
    (*                     ensures that this option is specified only   *)
    (*                     for a subset of the recipients, never the    *)
    (*                     entire list.  In that case convergence is    *)
    (*                     guaranteed in a finite number of passes.     *)

    OutJobPtr = POINTER TO OutJob;
    OutJob = RECORD
                 next: OutJobPtr;
                 sendtime: CARDINAL;
                 RetryNumber: CARDINAL;
                 size: CARD64;
                 ID: TransactionLogID;
                 sender: PathString;
                 domain: HostName;
                 SMTPport: CARDINAL;
                 LocalHost: HostName;
                 file: FilenameString;
                 offset: FilePos;
                 sendto: CombinedRecipientList;
                 NotifyOnFailure: BOOLEAN;
                 Recirculate: BOOLEAN;
             END (*RECORD*);

    (********************************************************************)
    (*                                                                  *)
    (*               MAIL SITTING IN THE FORWARD DIRECTORY              *)
    (*                                                                  *)
    (* Outgoing mail might have to wait if the internal processing      *)
    (* queues are too long.  It is also possible that there will still  *)
    (* be unprocessed mail when the system shuts down.  To ensure that  *)
    (* nothing is lost in such cases, each file in the forward          *)
    (* directory is stored with a preamble containing the information   *)
    (* that will be needed to put that mail back into the mail sack.    *)
    (*                                                                  *)
    (********************************************************************)

    (* While relay mail is waiting to be sent it is stored as a file    *)
    (* which starts with the following details.                         *)
    (*   4 bytes   format version, value 'V000'                         *)
    (*   4 bytes   send time                                            *)
    (*   1 byte    retry number                                         *)
    (*   1 byte    Flags byte                                           *)
    (*                bits 7-2  unused, should be 0                     *)
    (*                bit 1     the Recirculate flag                    *)
    (*                bit 0     notify-on-failure flag                  *)
    (*   variable  sender (character string)                            *)
    (*   variable  recipient list, bounded by () and comma-separated    *)
    (*                                                                  *)
    (* The message content starts immediately after this.               *)

    PreambleType = RECORD
                       version: ARRAY [0..3] OF CHAR;
                       sendtime: CARDINAL;
                       RetryNumber: CARD8;
                       Flags: CARD8;
                   END (*RECORD*);

(************************************************************************)
(*                                                                      *)
(*   PROCESSING ORDER                                                   *)
(*      Mail passed to this module for delivery goes through            *)
(*      several queues:                                                 *)
(*       1. A file is created in the 'forward' directory, containing    *)
(*          all information needed for delivery.                        *)
(*       2. The mail item also goes into the mail sack, provided that   *)
(*          the mail sack is not too full.  If the mail sack is too     *)
(*          full, the item will be picked up on a later directory scan  *)
(*          by the task called OnlineChecker, which will put it in      *)
(*          the mail sack.                                              *)
(*       3. The task called Sorter takes mail out of the mail sack,     *)
(*          possibly splits it into multiple jobs, and puts one or      *)
(*          more jobs onto the output queue.  (If Sorter does split     *)
(*          the item into multiple jobs, this implies the replacement   *)
(*          of the file in the 'forward' directory by multiple files.)  *)
(*          If there's any local mail left at this stage then the       *)
(*          Sorter delivers it immediately.                             *)
(*       4. The task called MailerTask then delivers the non-local      *)
(*          mail, provided that it can be delivered.  Undelivered items *)
(*          are discarded if they will never be delivered (and this     *)
(*          causes a rejection letter to be sent to the sender).  If    *)
(*          this task judges that a retry will be worth trying, the     *)
(*          undelivered item is moved onto the retry queue.             *)
(*                                                                      *)
(*      The task called RetryTask picks up items from the retry queue   *)
(*      when their 'time to deliver' arrives.  (Anything on the retry   *)
(*      queue is there for delayed delivery rather than immediate       *)
(*      delivery.)  When the item is ready to be sent, it is put back   *)
(*      into the mail sack.  Note that there is a counter on such       *)
(*      items that puts a limit on how many retries will be attempted.  *)
(*                                                                      *)
(************************************************************************)

VAR
    (* A flag saying whether we may write to the screen. *)

    ScreenEnabled: BOOLEAN;

    (* A flag to say whether our INI data is in a TNI file. *)

    UseTNI: BOOLEAN;

    (* A flag saying that we want more detail written to the log file. *)

    ExtraLogging: BOOLEAN;

    (* Even more detail: do we want job counts displayed on the screen? *)

    DisplayCounts: BOOLEAN;

    (* Critical section protection on the log of outgoing mail. *)

    LogFileLock: Lock;

    (* "Enable" flag for logging the outgoing mail items to the SMTPOUT *)
    (* item log.  Note: this is not the same as LogSMTPout, which       *)
    (* controls whether we send details to the transaction log.         *)

    LogOutgoing: BOOLEAN;

    (* The filename for the log of outgoing mail. *)

    LogFileName: FilenameString;

    (* The method we use for deciding when we are online.               *)
    (*        0    check whether a file called ONLINE exists            *)
    (*        1    check to see whether we have a dial-up connection    *)
    (*        2    remain online always                                 *)

    OnlineOption: CARDINAL;

    (* A flag to say that we're offline and can't send any mail out. *)

    WeAreOffline: BOOLEAN;

    (* A count of items that are held up because we're offline. *)

    OfflineData: RECORD
                     access: Lock;
                     PendingCount: CARDINAL;
                 END (*RECORD*);

    (* A semaphore to say that we should check again to see whether     *)
    (* we are now online.                                               *)

    CheckIfOnline: Semaphore;

    (* Event semaphore by which external software can force a Signal    *)
    (* on semaphore CheckIfOnline.                                      *)

    ForceOnlineCheck: OS2.HEV;

    (* Flag to ensure that an online check will include a check of      *)
    (* the "forward" directory.                                         *)

    ForceForwardDirCheck: BOOLEAN;

    (* A semaphore to tell the mailer task that it should check the     *)
    (* list of mail waiting to be sent.                                 *)

    SomethingToSend: Semaphore;

    (* A semaphore to wake up the "retry" task. *)

    Retry: Semaphore;

    (* The number of bytes of a message to bounce back to the sender if *)
    (* the message couldn't be delivered.  If this is zero, we send     *)
    (* back the entire rejected message.                                *)

    BounceBytes: CARDINAL;

    (* Number of threads handling outbound mail (max 64). *)

    NumberOfDaemons: CARDINAL;
    DesiredNumberOfDaemons: CARDINAL;
    NumberOfDaemonsLock: Lock;
    DaemonActive: ARRAY [1..MaxNumberOfDaemons] OF BOOLEAN;

    (* The mail items being sent right now. *)

    InProgress: ARRAY [1..99] OF FilenameString;

    (* List of mail waiting to be sorted before being sent out.  *)

    MailSack: RECORD
                  access: Lock;
                  count: Semaphore;
                  head, tail: OutJobPtr;
              END (*RECORD*);

    (* List of mail waiting to be sent (without delay).   *)

    OutboundMail: RECORD
                      access: Lock;
                      head, tail: OutJobPtr;
                      RecipientCount: RECORD
                                          count, limit: CARDINAL;
                                      END;
                  END (*RECORD*);

    (* Maximum number of recipients per outgoing mail item.  If *)
    (* there are more than this, we have to break the list of   *)
    (* recipients into batches.                                 *)

    MaxRecipientsPerLetter: CARDINAL;

    (* Number of retries before a warning is sent to the sender, *)
    (* and number of retries before giving up.                   *)

    WarnRetries, MaxRetries: CARDINAL;

    (* List of jobs waiting to be reattempted (with a delay).   *)

    RetryList: RECORD
                   access: Lock;
                   head: OutJobPtr;
                   RecipientCount: RECORD
                                       count, limit: CARDINAL;
                                   END;
               END (*RECORD*);

    (* The directory used to hold mail waiting to be forwarded. *)

    ForwardDirName: FilenameString;

    (* String used in creating a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

    (* The interface we use for sending mail.  It may be zero, in       *)
    (* which case we let the connect() call choose the address.         *)

    BindAddr: CARDINAL;

    (* The name we are currently using as our own host name. *)

    OurHostName: HostName;

    (* If UseFixedLocalName is TRUE, we use the hostname above as our   *)
    (* name under all circumstances.  Otherwise, we allow for the       *)
    (* possibility of different names at our end for different          *)
    (* interfaces, and we work out our local name by checking which IP  *)
    (* address we are using at our end.                                 *)

    UseFixedLocalName: BOOLEAN;

    (* Relay host for our outbound mail. *)

    ForwardRelayHost: HostName;
    ForwardRelayPort: CARDINAL;

    (* SMTP port for recycling mail back through the server. *)

    OurSMTPPort: CARDINAL;

    (* POP3 host for POP-before-SMTP authentication. *)

    AuthPOPhost: HostName;
    AuthPOPport: CARDINAL;

    (* Option to use ForwardRelayHost for outbound mail. *)
    (*      0     don't use it                           *)
    (*      1     use it if direct route fails           *)
    (*      2     always use it                          *)

    ForwardRelayOption: CARDINAL;

    (* Authentication for outbound relay mail.  AuthOption has  *)
    (* the values:                                              *)
    (*      0    don't use authentication                       *)
    (*      1    use the SMTP AUTH command                      *)
    (*      2    use POP-before-SMTP                            *)
    (* In cases 1 and 2, AuthUser and AuthPass are the username *)
    (* and password used for authentication.                    *)

    AuthOption: CARDINAL;
    AuthUser: UserName;
    AuthPass: PassString;

    (* Counter for number of queued jobs.  To avoid running out *)
    (* of memory, we limit the number of jobs in the system.    *)
    (* The counter is the total number of jobs in one of the    *)
    (* queues (MailSack, OutboundMail, RetryList), plus those   *)
    (* in the InProgress array.  Jobs omitted from the queues   *)
    (* remain in the 'forward' directory, and will be picked up *)
    (* later on one of our periodic scans of that directory.    *)

    JobCount: RECORD
                  access: Lock;
                  count, limit: CARDINAL;
              END (*RECORD*);

    (* For debugging purposes: a breakdown of the two preceding *)
    (* counts.                                                  *)

    (*
    DetailedCount: RECORD
                       access: Lock;
                       jsack, joutbound, jretry: CARDINAL;  (* jobs *)
                       rrsack, rroutbound, rrretry: CARDINAL;  (* relay recipients *)
                   END (*RECORD*);
    *)

    (* Event semaphore to signal that new local mail has arrived. *)

    newmailhev: OS2.HEV;

    (* Number of extra threads that are now running. *)

    TaskCount: CARDINAL;

    (* A semaphore used to delay the start of the tasks in this *)
    (* module until INI data have been initialised.             *)

    SystemUp: Semaphore;

    (* A flag and semaphore used in shutdown processing. *)

    ShutdownRequest: BOOLEAN;
    TaskDone: Semaphore;

(************************************************************************)
(*                          DEBUGGING CODE                              *)
(************************************************************************)

(*
PROCEDURE ShowCounts;

    CONST base1 = 10;  size = 4;  baseinc = 4;

    VAR pos: CARDINAL;
        count1: CARDINAL;
        count1sack, count1outbound, count1retry: CARDINAL;
        count2sack, count2outbound, count2retry: CARDINAL;
        buffer: ARRAY [0..size] OF CHAR;

    BEGIN
        IF DisplayCounts THEN
            WITH JobCount DO
                Obtain (access);
                count1 := count;
                Release (access);
            END (*WITH*);
            WITH DetailedCount DO
                Obtain (access);
                count1sack := jsack;
                count1outbound := joutbound;
                count1retry := jretry;
                count2sack := rrsack;
                count2outbound := rroutbound;
                count2retry := rrretry;
                Release (access);
            END (*WITH*);

            pos := base1;
            CardinalToString (count1, buffer, size);
            buffer[size] := Nul;
            WriteStringAt (0, pos, buffer);
            INC (pos, size+baseinc);

            CardinalToString (count1sack, buffer, size);
            WriteStringAt (0, pos, buffer);
            INC (pos, size);
            CardinalToString (count2sack, buffer, size);
            buffer[size] := Nul;
            WriteStringAt (0, pos, buffer);
            INC (pos, size+baseinc);

            CardinalToString (count1outbound, buffer, size);
            WriteStringAt (0, pos, buffer);
            INC (pos, size);
            CardinalToString (count2outbound, buffer, size);
            buffer[size] := Nul;
            WriteStringAt (0, pos, buffer);
            INC (pos, size+baseinc);

            CardinalToString (count1retry, buffer, size);
            WriteStringAt (0, pos, buffer);
            INC (pos, size);
            CardinalToString (count2retry, buffer, size);
            buffer[size] := Nul;
            WriteStringAt (0, pos, buffer);

            (*Assert (count1 = count1sack + count1outbound + count1retry);*)
            (*Assert (count2 = count2sack + count2outbound + count2retry);*)

        END (*IF*);

    END ShowCounts;
*)

(************************************************************************)
(*                       MANAGING RECIPIENT LISTS                       *)
(************************************************************************)

PROCEDURE CreateCombinedRecipientList(): CombinedRecipientList;

    (* Creates a new recipient list. *)

    VAR result: CombinedRecipientList;

    BEGIN
        NEW (result);
        result^.local := NIL;
        result^.remote := NIL;
        result^.RemoteCount := 0;
        RETURN result;
    END CreateCombinedRecipientList;

(************************************************************************)

PROCEDURE ClearCombinedRecipientList (RL: CombinedRecipientList);

    (* Removes all entries from a recipient list, without destroying    *)
    (* the list itself.                                                 *)

    VAR nextlocal: LocalRecipientList;
        this, next: RelayListPointer;
        total: CARDINAL;

    BEGIN
        total := 0;
        IF RL <> NIL THEN
            WHILE RL^.local <> NIL DO
                nextlocal := RL^.local^.next;
                UnloadLocalUser (RL^.local^.U);
                DEALLOCATE (RL^.local, SIZE(LocalRecipientRecord));
                RL^.local := nextlocal;
            END (*WHILE*);

            this := RL^.remote;
            WHILE this <> NIL DO
                next := this^.next;
                DEALLOCATE (this, SIZE(RelayListEntry));
                INC (total);
                this := next;
            END (*WHILE*);
            RL^.remote := NIL;
            RL^.RemoteCount := 0;

            (*ShowCounts;*)

        END (*IF*);

    END ClearCombinedRecipientList;

(************************************************************************)

(*
PROCEDURE RRCount (p: OutJobPtr): CARDINAL;

    (* For debugging: returns the total number of relay recipients. *)

    BEGIN
        IF (p = NIL) OR (p^.sendto = NIL) THEN
            RETURN 0;
        ELSE
            RETURN p^.sendto^.RemoteCount;
        END (*IF*);
    END RRCount;
*)

(************************************************************************)

PROCEDURE DiscardCombinedRecipientList (VAR (*INOUT*) RL: CombinedRecipientList);

    (* Destroys a recipient list. *)

    BEGIN
        IF RL <> NIL THEN
            ClearCombinedRecipientList (RL);
            DEALLOCATE (RL, SIZE(CRLHeader));
        END (*IF*);
    END DiscardCombinedRecipientList;

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
                NextName[N] := '0';
                IF N > 0 THEN
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
                       VAR (*OUT*) NewName: FilenameString;
                                               Visible: BOOLEAN): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, NewName);
            IF Visible THEN
                cid := OpenNewFile1 (NewName, duplication);
            ELSE
                cid := OpenNewHiddenFile (NewName, duplication);
            END (*IF*);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*              ADDING A JOB TO THE QUEUE OF OUTBOUND MAIL              *)
(************************************************************************)

PROCEDURE AppendBody (srcname: ARRAY OF CHAR;  offset: FilePos;
                      dstcid: ChanId;  NumBytes: CARDINAL);

    (* Copies from a source file, starting at position "offset" and     *)
    (* continuing to the end of the file, to the destination.  The      *)
    (* caller should have already opened the destination file, but      *)
    (* not the source file.                                             *)

    (* Special case: If NumBytes>0 then we append only that many bytes. *)

    VAR srccid: ChanId;  NumberRead, amount: CARDINAL;
        buffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        (* Open the source file and position it at the right place. *)

        srccid := OpenOldFile (srcname, FALSE, TRUE);
        SetPosition (srccid, offset);

        (* Copy across the message body. *)

        LOOP
            amount := SIZE(buffer);
            IF (NumBytes > 0) AND (NumBytes < SIZE(buffer)) THEN
                amount := NumBytes;
            END (*IF*);
            ReadRaw (srccid, buffer, amount, NumberRead);
            IF NumberRead = 0 THEN EXIT(*LOOP*) END(*IF*);
            WriteRaw (dstcid, buffer, NumberRead);
            IF (NumBytes > 0) THEN
                DEC (NumBytes, NumberRead);
                IF NumBytes = 0 THEN EXIT(*LOOP*) END(*IF*);
            END (*IF*);
        END (*LOOP*);

        CloseFile (srccid);

    END AppendBody;

(************************************************************************)

PROCEDURE DiscardJob (VAR (*INOUT*) p: OutJobPtr;  DecCount: BOOLEAN);

    (* Deletes the job record.  The message file, if any, is not        *)
    (* deleted by this procedure, but it loses its hidden status.       *)

    (* If DecCount is TRUE then we also decrement the count of the      *)
    (* total number of jobs in the system.  If DecCount is FALSE then   *)
    (* we assume that the caller has already taken care of the count.   *)

    BEGIN
        IF p <> NIL THEN
            DiscardCombinedRecipientList (p^.sendto);
            IF p^.file[0] <> Nul THEN
                HideFile (p^.file, FALSE);
            END (*IF*);
            DEALLOCATE (p, SIZE(OutJob));
            IF DecCount THEN
                Obtain (JobCount.access);
                DEC (JobCount.count);
                IF JobCount.count < JobCount.limit THEN
                    JobCount.limit := JobCountLimitHigh;
                END (*IF*);
                Release (JobCount.access);
            END (*IF*);
        END (*IF*);
    END DiscardJob;

(************************************************************************)

PROCEDURE AddToMailSack (p: OutJobPtr);

    (* Adds a new job to the queue MailSack.  If it doesn't fit it      *)
    (* is not put on the queue, but that doesn't mean that the job is   *)
    (* lost; all the necessary details are in the job file itself, and  *)
    (* will be reconstructed on a later scan of the forward directory.  *)

    (* This procedure doesn't put a limit on the number of recipients,  *)
    (* because for mail with too many recipients I'd like it to get as  *)
    (* far as the Sorter task, in order that it can be broken up.       *)

    VAR overflow: BOOLEAN;

    BEGIN
        Obtain (JobCount.access);
        overflow := JobCount.count >= JobCount.limit;
        IF overflow THEN
            JobCount.limit := JobCountLimitLow;
        END (*IF*);
        INC (JobCount.count);
        Release (JobCount.access);

        IF overflow THEN
            DiscardJob (p, TRUE);
        ELSE
            (*
            WITH DetailedCount DO
                Obtain (access);
                INC (jsack);
                INC (rrsack, RRCount(p));
                Release (access);
            END (*WITH*);
            ShowCounts;
            *)
            Signal (CheckIfOnline);
            p^.next := NIL;
            Obtain (MailSack.access);
            IF MailSack.head = NIL THEN
                MailSack.head := p;
            ELSE
                MailSack.tail^.next := p;
            END (*IF*);
            MailSack.tail := p;
            Release (MailSack.access);
            LogTransactionL (p^.ID, "Added a letter to the mail sack");
            Signal (MailSack.count);
        END (*IF*);

    END AddToMailSack;

(************************************************************************)

PROCEDURE AddToOutQueue (p: OutJobPtr);

    (* Adds a new job to the queue OutboundMail.  If it doesn't fit it  *)
    (* is not put on the queue, but that doesn't mean that the job is   *)
    (* lost; all the necessary details are in the job file itself, and  *)
    (* will be reconstructed on a later scan of the forward directory.  *)

    VAR overflow: BOOLEAN;

    BEGIN
        Obtain (JobCount.access);
        overflow := JobCount.count >= JobCount.limit;
        IF overflow THEN
            JobCount.limit := JobCountLimitLow;
        END (*IF*);
        INC (JobCount.count);
        Release (JobCount.access);

        (* Put a limit on the number of recipients in the jobs  *)
        (* queued on this list.                                 *)

        IF NOT overflow THEN
            Obtain (OutboundMail.access);
            WITH OutboundMail.RecipientCount DO
                overflow := count + p^.sendto^.RemoteCount >= limit;
                IF NOT overflow THEN
                    INC (count, p^.sendto^.RemoteCount);
                END (*IF*);
            END (*WITH*);
            Release (OutboundMail.access);
        END (*IF*);

        IF overflow THEN
            DiscardJob (p, TRUE);
        ELSE
            (*
            WITH DetailedCount DO
                Obtain (access);
                INC (joutbound);
                INC (rroutbound, RRCount(p));
                Release (access);
            END (*WITH*);
            *)
            Signal (CheckIfOnline);
            p^.next := NIL;
            Obtain (OutboundMail.access);
            IF OutboundMail.head = NIL THEN
                OutboundMail.head := p;
            ELSE
                OutboundMail.tail^.next := p;
            END (*IF*);
            OutboundMail.tail := p;
            Release (OutboundMail.access);
            LogTransactionL (p^.ID, "Moved a job to the output queue");
            (*ShowCounts;*)
            Signal (SomethingToSend);
        END (*IF*);

    END AddToOutQueue;

(************************************************************************)

PROCEDURE AddToRetryList (p: OutJobPtr);

    (* Adds a new job to the list of jobs that have to be retried.      *)
    (* This list differs from the OutboundMail queue in that its        *)
    (* entries are ordered by time.                                     *)

    VAR previous, current: OutJobPtr;
        overflow: BOOLEAN;
        Message: ARRAY [0..79] OF CHAR;
        HH, MM, SS, pos: CARDINAL;

    BEGIN
        Obtain (JobCount.access);
        overflow := JobCount.count >= JobCount.limit;
        IF overflow THEN
            JobCount.limit := JobCountLimitLow;
        ELSE
            overflow := JobCount.count >= JobCountLimitForRetry;
        END (*IF*);
        INC (JobCount.count);
        Release (JobCount.access);

        (* Put a limit on the number of recipients in the jobs  *)
        (* queued on this list.                                 *)

        IF NOT overflow THEN
            Obtain (RetryList.access);
            WITH RetryList.RecipientCount DO
                overflow := (count + p^.sendto^.RemoteCount) >= limit;
                IF NOT overflow THEN
                    INC (count, p^.sendto^.RemoteCount);
                END (*IF*);
            END (*WITH*);
            Release (RetryList.access);
        END (*IF*);

        IF overflow THEN
            DiscardJob (p, TRUE);
        ELSE
            (*
            WITH DetailedCount DO
                Obtain (access);
                INC (jretry);
                INC (rrretry, RRCount(p));
                Release (access);
            END (*WITH*);
            *)
            p^.next := NIL;
            Obtain (RetryList.access);
            previous := NIL;  current := RetryList.head;
            WHILE (current <> NIL) AND (p^.sendtime > current^.sendtime) DO
                previous := current;  current := current^.next;
            END (*WHILE*);

            (* The new item has to be inserted between previous and current. *)

            p^.next := current;
            IF previous = NIL THEN
                RetryList.head := p;
            ELSE
                previous^.next := p;
            END (*IF*);

            Release (RetryList.access);

            Strings.Assign ("Retry scheduled for ", Message);
            pos := Strings.Length (Message);
            SS := time();
            IF p^.sendtime <= SS THEN
                SS := 0;
            ELSE
                SS := p^.sendtime - SS;
            END (*IF*);
            MM := SS DIV 60;  SS := SS MOD 60;
            HH := MM DIV 60;  MM := MM MOD 60;
            ConvertCardZ (HH, Message, 2, pos);
            Message[pos] := ':';  INC(pos);
            ConvertCardZ (MM, Message, 2, pos);
            Message[pos] := ':';  INC(pos);
            ConvertCardZ (SS, Message, 2, pos);
            Message[pos] := Nul;
            Strings.Append (" from now", Message);
            LogTransaction (p^.ID, Message);
            (*ShowCounts;*)

            Signal (Retry);
        END (*IF*);

    END AddToRetryList;

(************************************************************************)
(*            STORING A MESSAGE FILE IN THE FORWARD DIRECTORY           *)
(************************************************************************)

PROCEDURE WriteLocalNameList (fid: ChanId;  list: LocalRecipientList;
                              separator: CHAR;  FinalExpansion: BOOLEAN);

    (* Writes a list of names, with adjacent entries separated by       *)
    (* the separator character.  If separator is CHR(0) then we write   *)
    (* <CRLF> as the separator, including a final <CRLF> at the end of  *)
    (* the list.  FinalExpansion=TRUE means that the list will not hold *)
    (* any aliases, only the final addresses for delivery.              *)
    (* FinalExpansion=FALSE means that the list should be as specified  *)
    (* by the sender, before alias expansion.                           *)

    VAR p: LocalRecipient;  IncludeIt, SeparatorNeeded: BOOLEAN;

    BEGIN
        p := list;
        SeparatorNeeded := FALSE;
        WHILE p <> NIL DO
            IF FinalExpansion THEN
                IncludeIt := NOT p^.Skip;
            ELSE
                IncludeIt := p^.Original;
            END (*IF*);
            IF IncludeIt THEN
                IF SeparatorNeeded THEN
                    FWriteChar (fid, separator);
                    SeparatorNeeded := FALSE;
                END (*IF*);
                WriteRaw (fid, p^.user, LENGTH(p^.user));
                IF p^.domainname[0] <> Nul THEN
                    FWriteChar (fid, '@');
                    WriteRaw (fid, p^.domainname, LENGTH(p^.domainname));
                END (*IF*);
                IF separator = Nul THEN
                    FWriteLn (fid);
                ELSE
                    SeparatorNeeded := TRUE;
                END (*IF*);
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
    END WriteLocalNameList;

(************************************************************************)

PROCEDURE WriteRemoteNameList (fid: ChanId;  list: RelayList;
                               separator: CHAR;  FinalExpansion: BOOLEAN);

    (* Writes a list of names, with adjacent entries separated by       *)
    (* the separator character.  If separator is CHR(0) then we write   *)
    (* <CRLF> as the separator, including a final <CRLF> at the end of  *)
    (* the list.  FinalExpansion = FALSE means we should                *)
    (* omit names not marked as Original.                               *)

    VAR p: RelayListPointer;  SeparatorNeeded: BOOLEAN;

    BEGIN
        p := list;
        SeparatorNeeded := FALSE;
        WHILE p <> NIL DO
            IF FinalExpansion OR p^.Original THEN
                IF SeparatorNeeded THEN
                    FWriteChar (fid, separator);
                    SeparatorNeeded := FALSE;
                END (*IF*);
                WriteRaw (fid, p^.username, LENGTH(p^.username));
                IF p^.domain[0] <> Nul THEN
                    FWriteChar (fid, '@');
                    WriteRaw (fid, p^.domain, LENGTH(p^.domain));
                END (*IF*);
                IF separator = Nul THEN
                    FWriteLn (fid);
                ELSE
                    SeparatorNeeded := TRUE;
                END (*IF*);
            END (*IF*);
            p := p^.next;
        END (*WHILE*);
    END WriteRemoteNameList;

(************************************************************************)

PROCEDURE WriteCombinedNameList (fid: ChanId;  list: CombinedRecipientList;
                                 separator: CHAR;  FinalExpansion: BOOLEAN);

    (* Writes a list of names, with adjacent entries separated by       *)
    (* the separator character.  If separator is CHR(0) then we write   *)
    (* <CRLF> as the separator.  FinalExpansion=TRUE means that         *)
    (* the list will not hold any aliases, only the final addresses     *)
    (* for delivery.  FinalExpansion=FALSE means that the list should   *)
    (* be as specified by the sender, before alias expansion.           *)

    BEGIN
        WriteLocalNameList (fid, list^.local, separator, FinalExpansion);
        IF (list^.local <> NIL) AND (list^.remote <> NIL) THEN
            IF separator = Nul THEN
                FWriteLn (fid);
            ELSE
                FWriteChar (fid, separator);
            END (*IF*);
        END (*IF*);
        WriteRemoteNameList (fid, list^.remote, separator, FinalExpansion);
    END WriteCombinedNameList;

(************************************************************************)

PROCEDURE WriteNameList (fid: ChanId;  list: CombinedRecipientList);

    (* Writes a comma-separated list of names, enclosed in parentheses. *)
    (* Aliases are not included, but their expansions are.              *)

    BEGIN
        FWriteChar (fid, '(');
        WriteCombinedNameList (fid, list, ',', TRUE);
        FWriteChar (fid, ')');
    END WriteNameList;

(************************************************************************)

PROCEDURE StartNewFile (p: OutJobPtr): ChanId;

    (* Creates a new HIDDEN file, opens it, and writes the preamble     *)
    (* details for job p^.  On return p^.offset has been set, the file  *)
    (* is still open, and the function result is the file handle.       *)

    VAR cid: ChanId;
        preamble: PreambleType;

    BEGIN
        cid := OpenNewOutputFile (ForwardDirName, ".FWD", p^.file, FALSE);

        IF cid <> NoSuchChannel THEN

            (* Write the preamble to the new file. *)

            WITH preamble DO
                version := "V000";
                sendtime := p^.sendtime;
                RetryNumber := p^.RetryNumber;
                Flags := 0;
                IF p^.NotifyOnFailure THEN INC(Flags) END(*IF*);
                IF p^.Recirculate THEN INC(Flags, 2) END(*IF*);
            END (*WITH*);

            WriteRaw (cid, preamble, SIZE(preamble));
            WriteRaw (cid, p^.sender, LENGTH(p^.sender));
            WriteNameList (cid, p^.sendto);

            (* Set the size and offset. *)

            p^.offset := CurrentPosition(cid);
            p^.size := p^.offset;
        END (*IF*);

        RETURN cid;

    END StartNewFile;

(************************************************************************)

PROCEDURE StoreMessageFile (p: OutJobPtr);

    (* On entry p^ should have all fields filled in except p^.size.     *)
    (* This procedure creates a new file (whose name and size are       *)
    (* returned in p^.file) with a preamble reflecting the details in   *)
    (* p^ and with a body being the body of the original p^.file.  The  *)
    (* preamble details are given in the comments near the beginning of *)
    (* this module. Important assumption: p^.sendto is a non-empty list.*)

    (* The newly created file is a hidden file, and it will remain      *)
    (* hidden as long as it is on one of the outgoing queues.  When it  *)
    (* leaves the system of queues, it is either deleted (if it is      *)
    (* leaving the system permanently) or made visible, so that it can  *)
    (* be picked up later on a directory scan.                          *)

    VAR oldfilename: FilenameString;
        oldoffset: FilePos;
        newfid: ChanId;

    BEGIN
        oldfilename := p^.file;
        oldoffset := p^.offset;
        newfid := StartNewFile(p);
        IF newfid <> NoSuchChannel THEN
            AppendBody (oldfilename, oldoffset, newfid, 0);
            CloseFile (newfid);
            p^.size := GetFileSize(p^.file);
        END (*IF*);
    END StoreMessageFile;

(************************************************************************)
(*                     THE MAIN CLIENT PROCEDURES                       *)
(************************************************************************)

PROCEDURE NoLocalRecipients (RL: LocalRecipientList): BOOLEAN;

    (* Returns TRUE iff the list of recipients is empty. *)

    BEGIN
        (* Watch out for the special case where all local recipient     *)
        (* records are "Skip" records.                                  *)

        WHILE (RL <> NIL) AND RL^.Skip DO
            RL := RL^.next;
        END (*WHILE*);

        RETURN RL = NIL;

    END NoLocalRecipients;

(************************************************************************)

PROCEDURE RemoveInitialSkipRecords (VAR (*INOUT*) RL: LocalRecipientList);

    (* Disposes of the leading 'Skip' records in RL.  Interior 'Skip'   *)
    (* records are not touched, since the main point of this procedure  *)
    (* is to guard against the case where the list consists entirely    *)
    (* of 'Skip' records.                                               *)

    VAR next: LocalRecipientList;

    BEGIN
        WHILE (RL <> NIL) AND RL^.Skip DO
            next := RL^.next;
            UnloadLocalUser (RL^.U);
            DEALLOCATE (RL, SIZE(LocalRecipientRecord));
            RL := next;
        END (*WHILE*);
    END RemoveInitialSkipRecords;

(************************************************************************)

PROCEDURE EmptyRecipientList (RL: CombinedRecipientList): BOOLEAN;

    (* Returns TRUE iff the list of recipients is empty. *)

    BEGIN
        RETURN (RL = NIL) OR
                    (NoLocalRecipients(RL^.local) AND (RL^.remote = NIL));
    END EmptyRecipientList;

(************************************************************************)

PROCEDURE AddToRelayList (VAR (*INOUT*) CRL: CombinedRecipientList;
                             VAR (*IN*) to: ARRAY OF CHAR;
                             MarkAsOriginal: BOOLEAN);

    (* Adds one non-local recipient to the list of recipients. *)

    VAR RLP, tail: RelayListPointer;  duplicate: BOOLEAN;
        newuser: UserName;  newdomain: HostName;

    BEGIN
        IF CRL = NIL THEN
            NEW (CRL);
            CRL^.local := NIL;  CRL^.remote := NIL;
            CRL^.RemoteCount := 0;
        END (*IF*);

        (* To avoid complications due to alias expansion during mail    *)
        (* sorting, I prefer to keep this list in FIFO order.  Most     *)
        (* commonly it's going to be short, so I judge that it's better *)
        (* to search for the list tail each time rather than keep the   *)
        (* overhead of an explicit tail pointer.  Besides, this         *)
        (* approach allows me to check for duplicates.                  *)

        UserAndDomain (to, newuser, newdomain);
        duplicate := FALSE;
        tail := CRL^.remote;
        IF tail <> NIL THEN
            duplicate := Strings.Equal (newuser, tail^.username)
                            AND Strings.Equal (newdomain, tail^.domain);
            WHILE (tail^.next <> NIL) AND NOT duplicate DO
                tail := tail^.next;
                duplicate := Strings.Equal (newuser, tail^.username)
                            AND Strings.Equal (newdomain, tail^.domain);
            END (*WHILE*);
        END (*IF*);

        IF NOT duplicate THEN
            NEW (RLP);
            WITH RLP^ DO
                next := NIL;
                username := newuser;  domain := newdomain;
                failuremessage := "";
                Original := MarkAsOriginal;
                KeepTrying := TRUE;
            END (*WITH*);

            IF tail = NIL THEN
                CRL^.remote := RLP;
            ELSE
                tail^.next := RLP;
            END (*IF*);

            INC (CRL^.RemoteCount);

        END (*IF*);

    END AddToRelayList;

(************************************************************************)

PROCEDURE AddRecipient (RL: CombinedRecipientList;
                        VAR (*IN*) name: ARRAY OF CHAR;
                        AllowPrivateAliases, MarkAsOriginal: BOOLEAN;
                        ID: TransactionLogID);

    (* Checks whether "name" is local or non-local, and adds it to the  *)
    (* appropriate list.  It is possible to add invalid recipients, but *)
    (* this will be caught at the time of delivery.                     *)

    VAR user: UserName;  domain: HostName;  D: Domain;

    BEGIN
        ToLower (name);
        UserAndDomain (name, user, domain);
        IF DomainIsLocal (domain, D) THEN
            EVAL (AddToLocalList (RL, user, domain, D,
                                  AllowPrivateAliases, TRUE, MarkAsOriginal, ID));
        ELSE
            FullAddress (user, domain, name);
            AddToRelayList (RL, name, MarkAsOriginal);
        END (*IF*);
    END AddRecipient;

(************************************************************************)

PROCEDURE WriteRecipientList (cid: ChanId;  p: CombinedRecipientList;
                                       NewLines, FinalExpansion: BOOLEAN);

    (* Writes a list of e-mail addresses to a file.  If NewLines is     *)
    (* TRUE then each address goes on a new line; otherwise, the        *)
    (* addresses are space-separated.  FinalExpansion=TRUE means that   *)
    (* the list will not hold any aliases, only the final addresses     *)
    (* for delivery.  FinalExpansion=FALSE means that the list should   *)
    (* be as specified by the sender, before alias expansion.           *)

    VAR separator: CHAR;

    BEGIN
        IF NewLines THEN
            separator := Nul;
        ELSE
            separator := ' ';
        END (*IF*);
        WriteCombinedNameList (cid, p, separator, FinalExpansion);
    END WriteRecipientList;

(************************************************************************)

PROCEDURE SendRelayMail (VAR (*IN*) filename, from: ARRAY OF CHAR;
                         VAR (*INOUT*) RL: RelayList;
                         RecipientCount: CARDINAL;
                         offset: FilePos;  ID: TransactionLogID;
                         recirculate: BOOLEAN);

    (* Takes a copy of the file, and queues it to be sent to the        *)
    (* recipients on RL; then deletes RL.  We do not delete the         *)
    (* original file, but we will ultimately delete the copy we have    *)
    (* taken.                                                           *)
    (* (Actually we don't immediately delete RL; we take over the list  *)
    (* internally, but the caller sees RL=NIL.  The list data are kept  *)
    (* as long as needed, but are ultimately discarded.)                *)

    VAR p: OutJobPtr;

    BEGIN
        IF RL <> NIL THEN
            NEW (p);
            Strings.Assign (filename, p^.file);
            Strings.Assign (from, p^.sender);
            p^.domain := "";
            p^.SMTPport := 25;
            p^.LocalHost := OurHostName;
            NEW (p^.sendto);
            p^.sendto^.local := NIL;
            p^.sendto^.remote := RL;
            p^.sendto^.RemoteCount := RecipientCount;
            p^.ID := ID;
            p^.Recirculate := recirculate;
            p^.NotifyOnFailure := TRUE;
            p^.offset := offset;
            p^.sendtime := time();
            p^.RetryNumber := 0;
            StoreMessageFile (p);         (* hidden *)
            IF WeAreOffline THEN
                DiscardJob (p, FALSE);
            ELSE
                LogTransaction (ID, p^.file);
                AddToMailSack (p);
            END (*IF*);
            RL := NIL;
        END (*IF*);
    END SendRelayMail;

(************************************************************************)
(*                MAINTAINING A LIST OF LOCAL RECIPIENTS                *)
(************************************************************************)

PROCEDURE IsADuplicate (RL: LocalRecipientList;  D: Domain;
                             VAR (*IN*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if name is already on the list. *)

    BEGIN
        WHILE (RL <> NIL) AND NOT ((RL^.domain = D)
                                   AND Strings.Equal (RL^.user, name)) DO
            RL := RL^.next;
        END (*WHILE*);
        RETURN RL <> NIL;
    END IsADuplicate;

(************************************************************************)

PROCEDURE ExpandAlias (VAR (*INOUT*) CRL: CombinedRecipientList;
                             VAR (*IN*) alias: ARRAY OF CHAR;
                             D: Domain;
                             AllowPrivate: BOOLEAN;  ID: TransactionLogID): BOOLEAN;

    (* If "alias" is a valid alias, puts the expansion onto the lists   *)
    (* in CRL and returns TRUE.  Otherwise returns FALSE.  Private      *)
    (* aliases are not recognised unless AllowPrivate is TRUE.          *)

    VAR hini: INIData.HINI;  success, found: BOOLEAN;
        j, j2, k, size, size2, bestindex, index2: CARDINAL;
        bufptr, bufptr2: POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;
        name: PathString;
        searchstate: StringReadState;
        bestmatch, candidate: UserName;
        OurDomainName: DomainName;
        aliasstr: ARRAY [0..6] OF CHAR;
        key: ARRAY [0..0] OF CHAR;

    BEGIN

        (*SayHeapCount (ID, "entering ExpandAlias");*)

        NameOfDomain (D, OurDomainName);
        size := 0;  bufptr := NIL;  j := 0;  j2 := 0;
        bestindex := MAX(CARDINAL);  bestmatch := "";
        hini := OpenDomainINI (D);
        success := INIValid(hini);
        IF success THEN

            (* Because of the possibility of wildcards, there might be  *)
            (* several alias entries that match the name we have.  We   *)
            (* have to choose the matching one of lowest index, and     *)
            (* unfortunately they're not necessarily stored in the      *)
            (* INI file in index order.                                 *)

            (* The best candidate so far name "bestmatch", its index is *)
            (* "bestindex", and its data are in bufptr^[j] onwards,     *)
            (* where the buffer has size "size".                        *)

            (* Each time we test a new candidate its name will be       *)
            (* "candidate", its index is index2, and its data are in    *)
            (* bufptr2^, where the buffer has size "size2".             *)

            aliasstr := "$ALIAS";
            key[0] := Nul;
            GetStringList (hini, aliasstr, key, searchstate);
            LOOP
                NextString (searchstate, candidate);
                IF candidate[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);
                IF WildMatch(alias, candidate) THEN

                    success := ItemSize (hini, aliasstr, candidate, size2);

                    IF (NOT success) OR (size2 = 0) THEN

                        bufptr2 := NIL;
                        index2 := MAX(CARDINAL);
                        success := AllowPrivate;
                        j2 := 0;

                    ELSE

                        ALLOCATE (bufptr2, size2);
                        success := INIGetTrusted (hini, aliasstr, candidate,
                                                             bufptr2^, size2);
                        index2 := MAX(CARDINAL);
                        success := success AND
                                     (AllowPrivate OR (bufptr2^[0] = CHR(1)));
                        IF success THEN
                            IF (size2 < 4) OR (bufptr2^[1] <> CHR(3)) THEN
                                j2 := 1;
                                index2 := MAX(CARD16);
                            ELSE
                                j2 := 4;
                                index2 := ORD(bufptr2^[2]) + 256*ORD(bufptr2^[3]);
                            END (*IF*);
                        END (*IF*);

                    END (*IF*);

                    success := success AND (index2 < bestindex);

                    (* We now have enough information to decide whether *)
                    (* to replace the best candidate so far with the    *)
                    (* new one.                                         *)

                    IF success THEN
                        IF bufptr <> NIL THEN
                            DEALLOCATE (bufptr, size);
                        END (*IF*);
                        bufptr := bufptr2;
                        bufptr2 := NIL;
                        j := j2;  size := size2;  bestindex := index2;
                        bestmatch := candidate;
                    ELSIF bufptr2 <> NIL THEN
                        DEALLOCATE (bufptr2, size2);
                    END (*IF*);

                END (*IF WildMatch*);

            END (*LOOP*);

            CloseStringList (searchstate);
            CloseINIFile (hini);

        END (*IF*);

        success := bestmatch[0] <> Nul;

        IF success THEN

            (* Extract the names in the expansion. *)

            LOOP
                IF (j >= size) OR (bufptr^[j] = Nul) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                k := 0;
                REPEAT
                    name[k] := bufptr^[j];
                    INC (k);  INC(j);
                UNTIL name[k-1] = Nul;
                IF k > 1 THEN
                    Strings.FindNext ('@', name, 0, found, j2);
                    IF NOT found THEN
                        Strings.Assign (name, candidate);
                        FullAddress (candidate, OurDomainName, name);
                    END (*IF*);
                    AddRecipient (CRL, name, AllowPrivate, FALSE, ID);
                END (*IF*);
            END (*LOOP*);

        END (*IF*);

        IF bufptr <> NIL THEN
            DEALLOCATE (bufptr, size);
        END (*IF*);

        (*SayHeapCount (ID, "leaving ExpandAlias");*)

        RETURN success;

    END ExpandAlias;

(************************************************************************)

PROCEDURE AddToLocalList (VAR (*INOUT*) CRL: CombinedRecipientList;
                           VAR (*IN*) name, host: ARRAY OF CHAR;
                           D: Domain;
                           AllowPrivateAlias, AcceptIllegal,
                           MarkAsOriginal: BOOLEAN;  ID: TransactionLogID): BOOLEAN;

    (* Adds one local recipient to the list of recipients.  If          *)
    (* AcceptIllegal = TRUE, we'll add the recipient to the list and    *)
    (* return TRUE regardless of whether this is a legitimate local     *)
    (* user.  If AcceptIllegal = FALSE, we reject the recipient and     *)
    (* return FALSE if it's not a legal local recipient.                *)

    (* MarkAsOriginal = TRUE means that this is an address as supplied  *)
    (* by the sender, as distinct from one that was produced by         *)
    (* alias expansion or forwarding.                                   *)

    VAR RL: LocalRecipientList;  exists: BOOLEAN;
        active: CARDINAL;
        ForwardTo: ARRAY [0..511] OF CHAR;

    BEGIN

        (*SayHeapCount (ID, "entering AddToLocalList");*)

        ForwardTo[0] := Nul;
        ToLower (name);

        (* Start by putting a new record at the head of CRL^.local.     *)
        (* (Whether that list is LIFO or FIFO doesn't really matter to  *)
        (* us.)  That is, the default assumption is that this record    *)
        (* should be added.  If we're right, we'll fill in RL^.DirName  *)
        (* below.  If not, we'll remove the record again.               *)

        NEW (RL);
        WITH RL^ DO
            next := CRL^.local;
            Strings.Assign (name, user);
            Strings.Assign (host, domainname);
            domain := D;
            U := LoadLocalUser (name, D);
            MailDirectoryFor (D, DirName);
            Original := MarkAsOriginal;
            Skip := FALSE;
        END (*WITH*);
        CRL^.local := RL;

        exists := TRUE;
        IF IsADuplicate (CRL^.local^.next, D, name) THEN
            IF MarkAsOriginal THEN
                RL^.Skip := TRUE;
            ELSE
                CRL^.local := RL^.next;
                UnloadLocalUser (RL^.U);
                DEALLOCATE (RL, SIZE(LocalRecipientRecord));
            END (*IF*);

        ELSIF IsActiveUser (RL^.U, active, ForwardTo) THEN

            (* Possible values for 'active' are:                        *)
            (*   0   this user is temporarily deactivated, i.e. we      *)
            (*       should act as if the user didn't exist.            *)
            (*   1   normal user.                                       *)
            (*   2   don't deliver to the user's account, instead       *)
            (*       forward the mail to the ForwardTo address.         *)
            (*   3   deliver one copy to user, another to ForwardTo.    *)

            IF active = 0 THEN
                exists := FALSE;
                CRL^.local := RL^.next;
                UnloadLocalUser (RL^.U);
                DEALLOCATE (RL, SIZE(LocalRecipientRecord));
            ELSIF active = 1 THEN
                Strings.Append (name, RL^.DirName);
                Strings.Append ('\', RL^.DirName);
            ELSIF active = 2 THEN
                (* Keep record so that duplicates can be detected. *)
                RL^.Skip := TRUE;
            ELSIF active = 3 THEN
                (* Deliver local copy, second copy will be handled below. *)
                Strings.Append (name, RL^.DirName);
                Strings.Append ('\', RL^.DirName);
            END (*IF*);

        ELSIF ExpandAlias (CRL, name, D, AllowPrivateAlias, ID) THEN
            RL^.Skip := TRUE;
        ELSIF AcceptIllegal THEN

            (* Not a valid username, but we add it to the list anyway,  *)
            (* so that it will be mentioned in the error message that   *)
            (* goes back to the sender.                                 *)

            RL^.DirName := "";

        ELSE

            exists := FALSE;
            CRL^.local := RL^.next;
            UnloadLocalUser (RL^.U);
            DEALLOCATE (RL, SIZE(LocalRecipientRecord));

        END (*IF*);

        (* Check whether we have to forward the mail to someone else    *)
        (* as a side-effect of the above decisions.                     *)

        IF ForwardTo[0] <> Nul THEN
            AddRecipient (CRL, ForwardTo, AllowPrivateAlias, FALSE, ID);
        END (*IF*);

        (*SayHeapCount (ID, "leaving AddToLocalList");*)

        RETURN exists;

    END AddToLocalList;

(************************************************************************)

PROCEDURE ChooseIncomingFileDirectory (CRL: CombinedRecipientList;
                                      VAR (*OUT*) BaseName: FilenameString);

    (* Sets BaseName to the mailbox of the first local recipient, or to *)
    (* the name of the Forward directory if the first local recipient   *)
    (* has no maibox.  In either case the name includes a trailing '\'. *)

    VAR LocalRL: LocalRecipientList;

    BEGIN
        LocalRL := CRL^.local;
        IF (LocalRL = NIL) OR (LocalRL^.DirName[0] = Nul) THEN
            BaseName := ForwardDirName;
        ELSE
            BaseName := LocalRL^.DirName;
        END (*IF*);
    END ChooseIncomingFileDirectory;

(************************************************************************)

PROCEDURE PublicNotification;

    (* Posts a public event semaphore to say that there has been a      *)
    (* new mail item received.                                          *)

    VAR count: CARDINAL;

    BEGIN
        (* According to the manual event semaphores are edge-triggered, *)
        (* so we ought to be able to reset the semaphore immediately    *)
        (* after posting it.  In fact, this seems to be the method      *)
        (* that works most reliably.  Logically the waiter should       *)
        (* do the reset, but that doesn't seem to work as well.         *)

        OS2.DosPostEventSem (newmailhev);
        OS2.DosResetEventSem (newmailhev, count);

    END PublicNotification;

(************************************************************************)

PROCEDURE CopyToRecipients (messagefile: FilenameString;
                            returnpath: PathString;
                            offset: FilePos;
                            LogID: TransactionLogID;
                            CRL: CombinedRecipientList;
                            recirculate: BOOLEAN);

    (* This procedure is to be called after a mail item has been        *)
    (* received.  We assume that messagefile is a temporary file in     *)
    (* the mailbox of the first local recipient, if any, and is a       *)
    (* temporary file in the 'forward' directory otherwise.  This       *)
    (* procedure makes copies for all local recipients, queues the mail *)
    (* for the remote recipients, and then deletes the message file.    *)

    VAR done: BOOLEAN;  result, tries, localcount: CARDINAL;
        current: LocalRecipient;
        DestName: FilenameString;

    BEGIN
        (* Send off the copies to be forwarded. *)

        SendRelayMail (messagefile, returnpath,
                          CRL^.remote, CRL^.RemoteCount,
                               offset, LogID, recirculate);

        (* There are now no nonlocal recipients.  The relay operations  *)
        (* are likely to be still in progress, but that's not our       *)
        (* concern.  If necessary, SendRelayMail has taken a copy of    *)
        (* messagefile and a copy of the list of relay recipients, and  *)
        (* passed that problem to another thread.  We can now do what   *)
        (* we like with our own copy.                                   *)

        RemoveInitialSkipRecords (CRL^.local);
        localcount := 0;

        IF CRL^.local = NIL THEN

            (* If no local recipients, delete the temporary file. *)

            FileSys.Remove (messagefile, done);

        ELSE
            (* Copy the file to the mailboxes of all local recipients   *)
            (* except the first.  Watch out for the special case where  *)
            (* the first local recipient is invalid, because in that    *)
            (* case the ChooseIncomingFileDirectory would have chosen   *)
            (* to put the file in the forward directory even if there   *)
            (* were valid local users.  (To avoid the overhead of       *)
            (* having to search past all invalid and 'Skip' users.)     *)

            current := CRL^.local;
            IF current^.DirName[0] <> Nul THEN
                current := current^.next;
            END (*IF*);
            WHILE current <> NIL DO
                IF NOT current^.Skip THEN
                    IF current^.DirName[0] = Nul THEN
                        Strings.Assign ("Unknown user ", DestName);
                        Strings.Append (current^.user, DestName);
                        Strings.Append (" in alias expansion", DestName);
                        LogTransaction (LogID, DestName);
                    ELSE
                        tries := 0;
                        REPEAT
                            LockLocalUser (current^.U);
                            NewMessageFilename (current^.U, DestName);
                            result := OS2.DosCopy (messagefile, DestName, 0);
                            UnlockLocalUser (current^.U);
                            INC (tries);
                        UNTIL (result <> 32) OR (tries > 50);
                        IF result = 32 THEN
                            LogTransactionL (LogID, "Couldn't copy to destination file");
                        ELSE
                            LogTransaction (LogID, DestName);
                            INC (localcount);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
                current := current^.next;
            END (*WHILE*);

            (* Finally, rename the original so that it's no longer a    *)
            (* temporary file.  Again, watch out for the special case   *)
            (* where the first local recipient is invalid.              *)

            IF CRL^.local^.DirName[0] = Nul THEN
                FileSys.Remove (messagefile, done);
            ELSE
                tries := 0;
                REPEAT
                    LockLocalUser (CRL^.local^.U);
                    NewMessageFilename (CRL^.local^.U, DestName);
                    FileSys.Rename (messagefile, DestName, done);
                    UnlockLocalUser (CRL^.local^.U);
                    INC (tries);
                UNTIL done OR (tries > 50);
                IF done THEN
                    LogTransaction (LogID, DestName);
                    INC (localcount);
                ELSE
                    LogTransactionL (LogID, "Failed to rename message file");
                END (*IF*);
            END (*IF*);
        END (*IF*);

        IF localcount > 0 THEN
            PublicNotification;
        END (*IF*);

        ClearCombinedRecipientList (CRL);

    END CopyToRecipients;

(************************************************************************)
(*                           FILTER OVERRIDES                           *)
(************************************************************************)

(*
PROCEDURE SeparateFilter (VAR (*INOUT*) CRL: CombinedRecipientList;
                          VAR (*OUT*) NewCRL: CombinedRecipientList;
                          VAR (*INOUT*) FilterName: FilenameString): BOOLEAN;

    (* Finds the first user in CRL that has a filter override.  If      *)
    (* found, moves that user - and any other user with an identical    *)
    (* override - to NewCRL, and sets FilterName to the override        *)
    (* filter name (which could be the empty string, since an override  *)
    (* may specify "no filtering").  In this case the function result   *)
    (* is TRUE.  If there is no user in CRL with an override, the       *)
    (* result is FALSE, CRL is unaltered, and NewCRL and FilterName     *)
    (* do not contain any meaningful information.                       *)

    (* Note also: we search only the local users in CRL, but the ones   *)
    (* we move to NewCRL are redefined to be remote users so that we    *)
    (* can recirculate them.                                            *)

    VAR found, HaveResult: BOOLEAN;
        previous, current, temp: LocalRecipientList;
        tail: RelayListPointer;
        NewFilterName: FilenameString;

    BEGIN
        found := FALSE;  HaveResult := FALSE;
        previous := NIL;  current := CRL^.local;
        tail := NIL;

        WHILE (current <> NIL) AND NOT found DO

            (* Find the next user on list with an override. *)

            WHILE (current <> NIL) AND NOT found DO
                IF NOT current^.Skip THEN
                    found := SetFilterOverride (current^.U, NewFilterName)
                             AND (StringMatch(FilterName, NewFilterName) = HaveResult);
                END (*IF*);
                IF NOT found THEN
                    previous := current;
                    current := current^.next;
                END (*IF*);
            END (*WHILE*);

            IF found THEN
                IF HaveResult THEN
                    NEW (tail^.next);
                    tail := tail^.next;
                ELSE
                    HaveResult := TRUE;
                    FilterName := NewFilterName;
                    NewCRL := CreateCombinedRecipientList();
                    NEW (NewCRL^.remote);
                    tail := NewCRL^.remote;
                END (*IF*);

                (* Put current user onto new list.  Note that the new     *)
                (* list has to be a 'remote' list to force recirculation. *)

                WITH tail^ DO
                    next := NIL;
                    username := current^.user;
                    domain := current^.domainname;
                    failuremessage[0] := Nul;
                    Original := TRUE;
                    KeepTrying := TRUE;
                END (*WITH*);
                INC (NewCRL^.RemoteCount);

                (* Remove current user from old list. *)

                temp := current;
                current := current^.next;
                IF previous = NIL THEN
                    CRL^.local := current;
                ELSE
                    previous^.next := current;
                END (*IF*);
                UnloadLocalUser (temp^.U);
                DEALLOCATE (temp, SIZE(LocalRecipientRecord));

                (* By continuing to loop at this stage, we continue *)
                (* searching the old list for entries with the same *)
                (* override filter.                                 *)

            END (*IF*);
        END (*WHILE*);

        RETURN HaveResult;

    END SeparateFilter;
*)

(************************************************************************)

PROCEDURE SortByFilter (VAR (*INOUT*) list: ListOfRecipients);

    (* On entry the list has one entry, with the default filter in the  *)
    (* filtername field, next=NIL, and this=the list of all recipients. *)
    (* On exit the recipients have been sorted such that all those in   *)
    (* a "CombinedRecipientList" have the same filter specified.        *)
    (* Assumption: list <> NIL.                                         *)

    VAR p, remainder, remlast, listend: ListOfRecipients;
        previous, current, next: LocalRecipientList;
        newentry: CombinedRecipientList;
        newnode: ListOfRecipients;
        NewFilterName: FilenameString;
        found: BOOLEAN;

    BEGIN
        (* Pass 1: Divide the list into two lists, those that use the   *)
        (* default filter and those that have a separate filter.        *)

        remainder := NIL;  remlast := NIL;

        (* We are now searching the "local" sublist of the (at this     *)
        (* stage unique) CombinedRecipientList.  We don't need to look  *)
        (* at the remote sublist, because only local addresses can have *)
        (* filter overrides.                                            *)

        previous := NIL;
        current := list^.this^.local;

        (* Find the next user on list with an override. *)

        WHILE current <> NIL DO
            found := (NOT current^.Skip)
                        AND SetFilterOverride (current^.U, NewFilterName)
                         AND (StringMatch(list^.filtername, NewFilterName) = FALSE);
            IF found THEN

                (* Move this user to the "remainder" list. *)

                NEW (newentry);
                newentry^.remote := NIL;
                newentry^.RemoteCount := 0;
                newentry^.local := current;
                next := current^.next;
                IF previous = NIL THEN
                    list^.this^.local := next;
                ELSE
                    previous^.next := next;
                END (*IF*);
                current^.next := NIL;

                (* Set current back to where we were working on *)
                (* the mail list.                               *)

                current := next;

                NEW (newnode);
                newnode^.filtername := NewFilterName;
                newnode^.next := NIL;
                newnode^.this := newentry;
                IF remlast = NIL THEN
                    remainder := newnode;
                ELSE
                    remlast^.next := newnode;
                END (*IF*);
                remlast := newnode;
            ELSE
                previous := current;
                current := current^.next;
            END (*IF*);
        END (*WHILE*);

        (* Pass 2: break the second list into sublists.  Our processing *)
        (* is simplified by the fact that, because of the way pass 1    *)
        (* was done, there is exactly one user per node of the          *)
        (* remainder list.                                              *)

        listend := list;
        WHILE remainder <> NIL DO

            (* Move the first node to a new list. *)

            listend^.next := remainder;
            listend := remainder;
            remainder := remainder^.next;
            listend^.next := NIL;

            NewFilterName := listend^.filtername;

            (* Search the rest of the remainder list for a match on     *)
            (* filter name.                                             *)

            previous := listend^.this^.local;
            p := remainder;
            WHILE p <> NIL DO
                WHILE (p <> NIL) AND
                        (StringMatch(p^.filtername, NewFilterName) = FALSE) DO
                    p := p^.next;
                END (*WHILE*);
                IF p <> NIL THEN

                    (* Move one user from the remainder list to the *)
                    (* listend^.this^.local list.                   *)

                    current := p^.this^.local;
                    previous^.next := current;
                    previous := current;
                    remlast := p;
                    p := p^.next;
                    DISPOSE (remlast);
                END (*IF*);
            END (*WHILE*);
        END (*WHILE*);

        (* Final check: remove the list head if it has no users.  We    *)
        (* don't need to check any other list entries, because they     *)
        (* are nonempty by construction.                                *)

        IF (list^.this^.remote = NIL) AND (list^.this^.local = NIL) THEN
            remainder := list^.next;
            DISPOSE (list);
            list := remainder;
        END (*IF*);

    END SortByFilter;

(************************************************************************)
(*              SENDING A "SORRY, IT DIDN'T WORK" MESSAGE               *)
(************************************************************************)

PROCEDURE WriteRejectionLetter (cid: ChanId;  job: OutJobPtr;
                                                          final: BOOLEAN);

    (* On entry the file has already been opened but it has no content. *)
    (* We write the header and body, then close the file.               *)

    CONST Blast = 24;
    VAR Boundary: ARRAY [0..Blast] OF CHAR;
        AttachBody: BOOLEAN;

    (********************************************************************)

    PROCEDURE GenerateBoundaryCode;

        (* Sets Boundary to a pseudo-random string. *)

        CONST max = 61;
        VAR j, k: CARDINAL;  ch: CHAR;

        BEGIN
            FOR j := 0 TO Blast DO
                k := RandInt (0, max);
                IF k < 10 THEN
                    ch := CHR (ORD('0') + k);
                ELSIF k < 36 THEN
                    ch := CHR (ORD('A') + k-10);
                ELSE
                    ch := CHR (ORD('a') + k-36);
                END (*IF*);
                Boundary[j] := ch;
            END (*FOR*);
        END GenerateBoundaryCode;

    (********************************************************************)

    VAR TimeBuffer: ARRAY [0..31] OF CHAR;
        LLP: LocalRecipientList;  RLP: RelayListPointer;

    BEGIN
        AttachBody := BounceBytes = 0;

        (* Fill in the header details. *)

        CurrentDateAndTime (TimeBuffer);
        FWriteString (cid, "Date: ");
        FWriteString (cid, TimeBuffer);
        FWriteLn (cid);
        FWriteString (cid, "From: Mailer daemon <postmaster@");
        FWriteString (cid, job^.LocalHost);
        FWriteString (cid, ">");
        FWriteLn (cid);
        FWriteString (cid, "To: ");
        FWriteString (cid, job^.sender);
        FWriteLn (cid);
        FWriteString (cid, "Subject: Delivery Notification: Delivery has ");
        IF final THEN
            FWriteString (cid, "failed");
        ELSE
            FWriteString (cid, "been delayed");
        END (*IF*);
        FWriteLn (cid);
        GenerateBoundaryCode;
        FWriteString (cid, "Mime-Version: 1.0");
        FWriteLn (cid);
        IF AttachBody THEN
            FWriteString (cid, 'Content-Type: multipart/mixed; boundary="Boundary_(');
            FWriteString (cid, Boundary);
            FWriteString (cid, ')"');
        ELSE
            FWriteString (cid, 'Content-Type: text/plain');
        END (*IF*);
        FWriteLn (cid);
        FWriteLn (cid);

        IF AttachBody THEN
            (* MIME boundary. *)

            FWriteString (cid, "This is a multipart message in MIME format.");
            FWriteLn (cid);
            FWriteLn (cid);
            FWriteString (cid, "--Boundary_(");
            FWriteString (cid, Boundary);
            FWriteString (cid, ')');
            FWriteLn (cid);
            FWriteString (cid, "Content-type: text/plain");
            FWriteLn (cid);
            FWriteString (cid, "Content-transfer-encoding: 7bit");
            FWriteLn (cid);
            FWriteLn (cid);
        END (*IF*);

        (* Fill in the message body. *)

        FWriteString (cid, "Your mail was not delivered");
        FWriteString (cid, " to the following recipients.");
        FWriteLn (cid);

        (* Local failures. *)

        LLP := job^.sendto^.local;
        WHILE LLP <> NIL DO
            FWriteString (cid, LLP^.user);
            IF LLP^.domainname[0] <> Nul THEN
                FWriteChar (cid, '@');
                FWriteString (cid, LLP^.domainname);
            END (*IF*);
            FWriteString (cid, ": ");
            FWriteString (cid, LLP^.failuremessage);
            FWriteLn (cid);
            LLP := LLP^.next;
        END (*WHILE*);

        (* Remote failures. *)

        RLP := job^.sendto^.remote;
        WHILE RLP <> NIL DO
            FWriteString (cid, RLP^.username);
            IF RLP^.domain[0] <> Nul THEN
                FWriteChar (cid, '@');
                FWriteString (cid, RLP^.domain);
            END (*IF*);
            FWriteString (cid, ": ");
            FWriteString (cid, RLP^.failuremessage);
            FWriteLn (cid);
            RLP := RLP^.next;
        END (*WHILE*);

        FWriteLn (cid);
        IF final THEN
            FWriteString (cid, "There will be no further attempts to deliver this mail.");
        ELSE
            FWriteString (cid, "You do not need to re-send this mail.  The mail system");
            FWriteLn (cid);
            FWriteString (cid, "will keep trying to deliver it for several days.");
        END (*IF*);
        FWriteLn (cid);  FWriteLn (cid);

        (* MIME boundary. *)

        IF AttachBody THEN
            FWriteString (cid, "--Boundary_(");
            FWriteString (cid, Boundary);
            FWriteString (cid, ')');
            FWriteLn (cid);
            FWriteString (cid, "Content-type: message/rfc822");
        ELSE
            FWriteString (cid, "==== Beginning of the undeliverable message ====");
        END (*IF*);
        FWriteLn (cid);
        FWriteLn (cid);

        (* Append a copy of the message that couldn't be delivered. *)

        IF job^.file[0] <> Nul THEN
            AppendBody (job^.file, job^.offset, cid, BounceBytes);
        END (*IF*);

        IF AttachBody THEN
            (* Final boundary. *)

            FWriteLn (cid);
            FWriteString (cid, "--Boundary_(");
            FWriteString (cid, Boundary);
            FWriteString (cid, ')--');
            FWriteLn (cid);
        END (*IF*);
        CloseFile (cid);

    END WriteRejectionLetter;

(************************************************************************)

PROCEDURE SendRejectionLetter (job: OutJobPtr;  ID: TransactionLogID;
                                                        final: BOOLEAN);

    (* Sends an e-mail to the sender to say that the message described  *)
    (* by "job" could not be delivered.  If "final" is TRUE, we tell    *)
    (* the sender that there will be no further attempts.               *)

    (* The job descriptor and message file are not altered by this      *)
    (* procedure - we create our own copies of what we need.            *)

    VAR p: OutJobPtr;  cid: ChanId;

    BEGIN
        (* Create a new job descriptor. *)

        NEW (p);
        NEW (p^.sendto);
        p^.sendto^.local := NIL;
        NEW (p^.sendto^.remote);
        p^.sendto^.RemoteCount := 1;

        WITH p^.sendto^.remote^ DO
            next := NIL;
            UserAndDomain (job^.sender, username, domain);
            KeepTrying := TRUE;
        END (*WITH*);

        (* Beware of the case where the sender is <>.  I think it's     *)
        (* sufficient to check for an empty username, because I can     *)
        (* think of no valid scenario where the sender would be of the  *)
        (* form <@domain>.                                              *)

        IF p^.sendto^.remote^.username[0] = Nul THEN

            DEALLOCATE (p^.sendto^.remote, SIZE(RelayListEntry));
            DEALLOCATE (p^.sendto, SIZE(CRLHeader));
            DEALLOCATE (p, SIZE(OutJob));

        ELSE
            p^.Recirculate := FALSE;
            p^.NotifyOnFailure := FALSE;  p^.sender := "<>";
            p^.domain := "";
            p^.SMTPport := 25;
            p^.ID := ID;
            p^.sendtime := time();
            p^.RetryNumber := 0;
            p^.LocalHost := job^.LocalHost;

            (* Create a new message file. *)

            cid := StartNewFile (p);
            IF cid <> NoSuchChannel THEN
                WriteRejectionLetter (cid, job, final);
            END (*IF*);

            (* Put our message on the output queue.  *)

            AddToMailSack (p);
        END (*IF*);

    END SendRejectionLetter;

(************************************************************************)
(*            THE PROCEDURES THAT DELIVER THE OUTGOING MAIL             *)
(************************************************************************)

PROCEDURE WriteError (LogID: TransactionLogID);

    VAR LogLine: ARRAY [0..255] OF CHAR;

    BEGIN
        Strings.Assign ("Socket error ", LogLine);
        AppendCard (sock_errno(), LogLine);
        LogTransaction (LogID, LogLine);
    END WriteError;

(********************************************************************************)

PROCEDURE ConnectToHost (IPaddress: CARDINAL;  SMTPport: CARDINAL;
                      LogID: TransactionLogID;
                      VAR (*INOUT*) FailureReason: ARRAY OF CHAR): Socket;

    (* Tries to open a connection to the specified host.  Returns the   *)
    (* value NotASocket if we don't succeed; in this case, the string   *)
    (* FailureReason is given a value.                                  *)
    (* IPaddress is in network byte order.                              *)

    VAR s: Socket;  peer: SockAddr;

    BEGIN
        IF IPaddress <> 0 THEN
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);

            IF s = NotASocket THEN
                Strings.Assign ("Can't allocate socket", FailureReason);
            ELSE

                (* Bind to an address at our end if we're using a       *)
                (* specific address.                                    *)

                IF BindAddr <> 0 THEN
                    WITH peer DO
                        family := AF_INET;
                        WITH in_addr DO
                            port := 0;
                            addr := BindAddr;
                            zero := Zero8;
                        END (*WITH*);
                    END (*WITH*);

                    IF bind (s, peer, SIZE(peer)) THEN
                        WriteError (LogID);
                        LogTransactionL (LogID, "Cannot bind to local interface");
                    END (*IF*);
                END (*IF*);

                (* Socket open, connect to the client. *)

                WITH peer DO
                    family := AF_INET;
                    WITH in_addr DO
                        port := Swap2(SMTPport);
                        addr := IPaddress;
                        zero := Zero8;
                    END (*WITH*);
                END (*WITH*);

                IF connect (s, peer, SIZE(peer)) THEN

                    Strings.Assign ("Failed to connect", FailureReason);
                    soclose(s);
                    s := NotASocket;

                END (*IF*);
            END (*IF*);

        ELSE

            Strings.Assign ("500 Unknown host", FailureReason);
            s := NotASocket;

        END (*IF*);

        RETURN s;

    END ConnectToHost;

(************************************************************************)

PROCEDURE SendCommandOnly (SB: SBuffer;  ID: TransactionLogID;
                                    command: ARRAY OF CHAR;
                                    VAR (*OUT*) ConnectionLost: BOOLEAN);

    (* Sends and logs a command, but does not wait for a response.      *)

    VAR logline: ARRAY [0..511] OF CHAR;  sent: CARDINAL;

    BEGIN
        Strings.Assign ("> ", logline);
        Strings.Append (command, logline);
        LogTransaction (ID, logline);
        ConnectionLost := NOT SendLine (SB, command, sent);
        EVAL (FlushOutput (SB));
    END SendCommandOnly;

(************************************************************************)

PROCEDURE SendCommand (SB: SBuffer;  ID: TransactionLogID;  command: ARRAY OF CHAR;
                         VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* Sends a command, returns TRUE if the command was sent OK and     *)
    (* a positive response was returned.                                *)

    VAR result: BOOLEAN;
        logline: ARRAY [0..511] OF CHAR;

    BEGIN
        SendCommandOnly (SB, ID, command, ConnectionLost);
        IF ConnectionLost THEN
            Strings.Assign ("Connection lost", logline);
            result := FALSE;
        ELSE
            result := PositiveResponse(SB, ConnectionLost);
            GetLastLine (SB, logline);
            Strings.Insert ("< ", 0, logline);
        END (*IF*);
        LogTransaction (ID, logline);
        RETURN result;
    END SendCommand;

(************************************************************************)

PROCEDURE SendFile (SB: SBuffer;  name: FilenameString;  offset: FilePos;
                         VAR (*OUT*) ConnectionLost: BOOLEAN;
                         VAR (*OUT*) sent: CARDINAL): BOOLEAN;

    (* Sends the file, returns TRUE if it was successfully transmitted  *)
    (* and a positive response was returned.  Output parameter 'sent'   *)
    (* returns the number of bytes transmitted.                         *)

    CONST CtrlZ = CHR(26);

    VAR success, MoreToGo: BOOLEAN;  sent1, sent2: CARDINAL;
        cid: ChanId;
        buffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        sent := 0;  sent1 := 0;  sent2 := 0;
        cid := OpenOldFile (name, FALSE, FALSE);
        success := cid <> NoSuchChannel;
        IF success THEN
            SetPosition (cid, offset);
        END (*IF*);
        MoreToGo := TRUE;
        WHILE success AND MoreToGo DO
            ReadLine (cid, buffer);
            IF buffer[0] = CtrlZ THEN

                MoreToGo := FALSE;

            ELSE

                success := SendString (SB, buffer, sent1) AND SendEOL(SB, sent2);
                INC (sent, sent1+sent2);

            END (*IF*);

        END (*WHILE*);
        CloseFile (cid);

        success := success AND SendChar (SB, '.', sent1) AND SendEOL (SB, sent2);
        INC (sent, sent1+sent2);
        ConnectionLost := NOT success;
        INC (sent, FlushOutput (SB));
        RETURN success AND PositiveResponse (SB, ConnectionLost);

    END SendFile;

(************************************************************************)
(*                            ITEM LOGGING                              *)
(************************************************************************)

PROCEDURE WriteLogItem (job: OutJobPtr;  IPAddress: CARDINAL;
                                         recipients: RelayList);

    (* Writes the summary for this item to the user log. *)

    VAR cid: ChanId;  datetime: ARRAY [0..31] OF CHAR;
        IPbuffer: ARRAY [0..15] OF CHAR;

    BEGIN
        IPToString (IPAddress, FALSE, IPbuffer);
        Obtain (LogFileLock);
        cid := OpenAtEnd (LogFileName);
        CurrentTimeToString (datetime);
        FWriteString (cid, datetime);
        FWriteString (cid, " ");
        FWriteString (cid, job^.file);
        FWriteString (cid, " via ");
        FWriteString (cid, IPbuffer);
        FWriteLn (cid);
        FWriteString (cid, "  From: ");
        FWriteString (cid, job^.sender);
        FWriteLn (cid);
        FWriteString (cid, "  To:   ");
        WriteRemoteNameList (cid, recipients, ',', TRUE);
        FWriteLn (cid);
        FWriteLn (cid);
        CloseFile (cid);
        Release (LogFileLock);
    END WriteLogItem;

(************************************************************************)
(*                      SOME MISCELLANEOUS PROCEDURES                   *)
(************************************************************************)

PROCEDURE SetFailureMessage (job: OutJobPtr;  message: ARRAY OF CHAR);

    (* Stores the same failure message for all of job^.sendto^.remote.  *)
    (* Exception: we don't change the failure message when the          *)
    (* KeepTrying flag is FALSE, because in that case we know that a    *)
    (* more appropriate error message has already been set.             *)
    (* (We assume that the failure message for the local failures has   *)
    (* already been set.)                                               *)

    VAR pr: RelayListPointer;

    BEGIN
        pr := job^.sendto^.remote;
        WHILE pr <> NIL DO
            IF pr^.KeepTrying THEN
                Strings.Assign (message, pr^.failuremessage);
            END (*IF*);
            pr := pr^.next;
        END (*WHILE*);
    END SetFailureMessage;

(********************************************************************)

PROCEDURE RequestForwardDirectoryCheck;

    (* Forces a rescan of the 'forward' directory. *)

    BEGIN
        ForceForwardDirCheck := TRUE;
        Signal (CheckIfOnline);
    END RequestForwardDirectoryCheck;

(************************************************************************)

PROCEDURE AppendHostName (IPaddress: CARDINAL;
                          VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Converts IPaddress to a HostName, and appends it to buffer. *)

    VAR name: HostName;

    BEGIN
        AddressToHostName (IPaddress, name);
        Strings.Append (name, buffer);
    END AppendHostName;

(************************************************************************)

PROCEDURE GetOurHostName (S: Socket;  VAR (*OUT*) name: HostName);

    (* Sets name to what we are currently calling our host name for our *)
    (* end of the connection using socket S.  The answer might or might *)
    (* not depend on S, depending on the options currently in force. If *)
    (* we can't get a reasonable answer then we leave name unchanged.   *)

    VAR myaddr: SockAddr;  size: CARDINAL;

    BEGIN
        size := SIZE(myaddr);
        IF UseFixedLocalName THEN
            name := OurHostName;
        ELSIF NOT getsockname (S, myaddr, size) THEN
            AddressToHostName (myaddr.in_addr.addr, name);
        END (*IF*);
    END GetOurHostName;

(************************************************************************)
(*                                                                      *)
(*                    SENDING ONE MESSAGE OUT                           *)
(*                                                                      *)
(*  The following procedures are called by the mailer task.  By the     *)
(*  time a job has reached this point, there are no local recipients.   *)
(*  Furthermore, either all recipients are in the same domain, or they  *)
(*  have to be sent through the same relay host.                        *)
(*                                                                      *)
(************************************************************************)

PROCEDURE SendDataFile (SB: SBuffer;  job: OutJobPtr;  UseChunking: BOOLEAN;
                            VAR (*OUT*) ConnectionLost: BOOLEAN;
                            VAR (*OUT*) failuremessage: ARRAY OF CHAR): BOOLEAN;

    (* This implements the "send the message" part of the transaction,  *)
    (* after we've dealt with logging in, specifying recipients, etc.   *)

    VAR cid: ChanId;
        p: CharArrayPointer;
        bytesleft: CARD64;
        HaveCR, HaveCRLF: BOOLEAN;
        message: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE SendNextChunk (maxsize: CARDINAL;  final: BOOLEAN): BOOLEAN;

        (* Sends up to maxsize more bytes of the file.  The actual      *)
        (* chunk size might be less because of removal of '.' fillers   *)
        (* at the beginning of lines.  We update bytesleft.             *)

        VAR size, CharsLeft: CARDINAL;

        (****************************************************************)

        PROCEDURE RemoveByte (pos: CARDINAL);

            (* Deletes the character at p^[pos].  *)

            BEGIN
                Copy (ADR (p^[pos+1]), ADR (p^[pos]), CharsLeft);
                DEC (CharsLeft);
                DEC (size);
                DEC64 (bytesleft);
            END RemoveByte;

        (****************************************************************)

        TYPE ThreeChar = ARRAY [0..2] OF CHAR;

        CONST CRLFdot = ThreeChar {CR, LF, '.'};

        VAR cmd: ARRAY [0..31] OF CHAR;
            pos, checkpos, NumberRead, sent: CARDINAL;
            success, found: BOOLEAN;

        BEGIN
            size := maxsize;

            (* Read a chunk from the file, then strip out any   *)
            (* superfluous dots at the beginning of lines.      *)

            ReadRaw (cid, p^, size, NumberRead);
            IF NumberRead <> size THEN
                RETURN FALSE;
            END (*IF*);
            p^[size] := Nul;    (* to prevent running off the end *)
            checkpos := 0;
            CharsLeft := size;

            (* Special case: check for a "CR LF ." that crosses *)
            (* chunk boundaries.                                *)

            IF HaveCR THEN
                IF (CharsLeft > 0) AND (p^[0] = LF) THEN
                    checkpos := 1;
                    DEC (CharsLeft);
                    HaveCRLF := TRUE;
                END (*IF*);
            END (*IF*);

            IF HaveCRLF THEN
                IF (CharsLeft > 0) AND (p^[checkpos] = '.') THEN
                    RemoveByte (checkpos);
                END (*IF*);
            END (*IF*);

            HaveCR := (size > 0) AND (p^[size-1] = CR);
            HaveCRLF := (size > 1) AND (p^[size-2] = CR)
                                        AND (p^[size-1] = LF);

            (* Now the main scan of the buffer. *)

            WHILE CharsLeft > 0 DO
                Strings.FindNext (CRLFdot, p^, checkpos, found, pos);
                IF found THEN
                    DEC (CharsLeft, pos - checkpos + 3);
                    checkpos := pos + 2;
                    RemoveByte (checkpos);
                ELSE
                    CharsLeft := 0;
                END (*IF*);
            END (*WHILE*);

            (* Create a suitable BDAT command. *)

            Strings.Assign ("BDAT ", cmd);
            pos := 5;
            ConvertCard (size, cmd, pos);
            cmd[pos] := Nul;
            IF final THEN
                Strings.Append (" LAST", cmd);
            END (*IF*);

            (* We cannot use SendCommand here, because the chunk    *)
            (* must be send immediately after the command, without  *)
            (* waiting for a reply.                                 *)

            SendCommandOnly (SB, job^.ID, cmd, ConnectionLost);

            IF size = 0 THEN
                success := TRUE;
            ELSE
                success := SendRaw (SB, p^, size, sent) AND (sent = size)
                                  AND PositiveResponse (SB, ConnectionLost);
                Sub64 (bytesleft, sent);
            END (*IF*);

            Strings.Assign ('< ', message);
            GetLastLine (SB, failuremessage);
            Strings.Append (failuremessage, message);
            LogTransaction (job^.ID, message);
            IF success THEN
                failuremessage[0] := Nul;
            END (*IF*);
            RETURN success;

        END SendNextChunk;

    (********************************************************************)

    CONST MaxChunkSize = 32767;

    VAR sent: CARDINAL;         (* a dummy in this version. *)
        success: BOOLEAN;

    BEGIN
        ConnectionLost := FALSE;
        IF UseChunking THEN
            bytesleft := CAST(CARD64, Diff64(GetFileSize(job^.file), job^.offset));
            cid := OpenOldFile (job^.file, FALSE, TRUE);
            success := cid <> NoSuchChannel;
            IF success THEN
                SetPosition (cid, job^.offset);
            ELSE
                Strings.Assign ("Failed to open data file", failuremessage);
                LogTransaction (job^.ID, failuremessage);
                RETURN FALSE;
            END (*IF*);
            ALLOCATE (p, MaxChunkSize+1);
            HaveCR := FALSE;  HaveCRLF := FALSE;
            WHILE (bytesleft.high > 0) OR (bytesleft.low > MaxChunkSize) DO
                success := SendNextChunk (MaxChunkSize, FALSE);
                IF NOT success THEN
                    (* Abandon the attempt. *)
                    Strings.Assign ("Failed to send data chunk", failuremessage);
                    LogTransaction (job^.ID, failuremessage);
                    DEALLOCATE (p, MaxChunkSize+1);
                    CloseFile (cid);
                    RETURN FALSE;
                END (*IF*);
            END (*WHILE*);

            (* Now send the final chunk, which might have size 0. *)

            success := SendNextChunk (bytesleft.low, TRUE);
            DEALLOCATE (p, MaxChunkSize+1);
            CloseFile (cid);
        ELSE
            (* The non-chunked case. *)

            success := SendCommand (SB, job^.ID, "DATA", ConnectionLost)
                AND SendFile (SB, job^.file, job^.offset, ConnectionLost, sent);
            IF NOT ConnectionLost THEN
                Strings.Assign ('< ', message);
                GetLastLine (SB, failuremessage);
                Strings.Append (failuremessage, message);
                LogTransaction (job^.ID, message);
                IF success THEN
                    failuremessage[0] := Nul;
                END (*IF*);
            END (*IF*);
        END (*IF*);

        IF ConnectionLost THEN
            LogTransactionL (job^.ID, "Connection lost");
        END (*IF*);
        RETURN success;
    END SendDataFile;

(************************************************************************)

(*
PROCEDURE OldSendDataFile (SB: SBuffer;  job: OutJobPtr;  UseChunking: BOOLEAN;
                            VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* This implements the "send the message" part of the transaction,  *)
    (* after we've dealt with logging in, specifying recipients, etc.   *)

    VAR cid: ChanId;
        p: ADDRESS;
        message, message2: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE OldSendNextChunk (size: CARDINAL;  final: BOOLEAN): BOOLEAN;

        VAR cmd: ARRAY [0..31] OF CHAR;
            pos: CARDINAL;  success: BOOLEAN;

        BEGIN
            (* Create a suitable BDAT command. *)

            Strings.Assign ("BDAT ", cmd);
            pos := 5;
            ConvertCard (size, cmd, pos);
            cmd[pos] := Nul;
            IF final THEN
                Strings.Append (" LAST", cmd);
            END (*IF*);

            (* We cannot use SendCommand here, because the chunk    *)
            (* must be send immediately after the command, without  *)
            (* waiting for a reply.                                 *)

            SendCommandOnly (SB, job^.ID, cmd, ConnectionLost);
            success := OldSendChunk (cid, SB, size, p)
                        AND PositiveResponse (SB, ConnectionLost);
            Strings.Assign ('< ', message);
            GetLastLine (SB, message2);
            Strings.Append (message2, message);
            LogTransaction (job^.ID, message);
            RETURN success;
        END OldSendNextChunk;

    (********************************************************************)

    CONST ChunkSize = 16384;

    VAR bytesleft: CARD64;
        sent: CARDINAL;         (* a dummy in this version. *)
        success: BOOLEAN;

    BEGIN
        ConnectionLost := FALSE;
        IF UseChunking THEN
            bytesleft := CAST(CARD64, Diff64(GetFileSize(job^.file), job^.offset));
            cid := OpenOldFile (job^.file, FALSE, TRUE);
            success := cid <> NoSuchChannel;
            IF success THEN
                SetPosition (cid, job^.offset);
            ELSE
                LogTransactionL (job^.ID, "Failed to open data file");
                RETURN FALSE;
            END (*IF*);
            ALLOCATE (p, ChunkSize);
            WHILE (bytesleft.high > 0) OR (bytesleft.low > ChunkSize) DO
                success := OldSendNextChunk (ChunkSize, FALSE);
                IF NOT success THEN
                    (* Abandon the attempt. *)
                    RETURN FALSE;
                END (*IF*);
                Sub64 (bytesleft, ChunkSize);
            END (*WHILE*);

            (* Now send the final chunk. *)

            IF bytesleft.low > 0 THEN
                success := OldSendNextChunk (bytesleft.low, TRUE);
            END (*IF*);
            DEALLOCATE (p, ChunkSize);
            CloseFile (cid);
        ELSE
            (* The non-chunked case. *)

            success := SendCommand (SB, job^.ID, "DATA", ConnectionLost)
                AND SendFile (SB, job^.file, job^.offset, ConnectionLost, sent);
            IF NOT ConnectionLost THEN
                Strings.Assign ('< ', message);
                GetLastLine (SB, message2);
                Strings.Append (message2, message);
                LogTransaction (job^.ID, message);
            END (*IF*);
        END (*IF*);

        IF ConnectionLost THEN
            LogTransactionL (job^.ID, "Connection lost");
        END (*IF*);
        RETURN success;
    END OldSendDataFile;
*)

(************************************************************************)

PROCEDURE DeliverDeLetter (job: OutJobPtr;  IPaddress, port: CARDINAL;
                           UseAuth: BOOLEAN;  AuthName: UserName;
                           AuthPass: PassString;
                           VAR (*OUT*) TryNextOption: BOOLEAN;
                           VAR (*OUT*) RetryNeeded: BOOLEAN):  BOOLEAN;

    (* Sends one mail item to multiple recipients via the server at the *)
    (* specified IP address.  On return the successful deliveries have  *)
    (* been discarded from job^.sendto.  For all remaining recipients,  *)
    (* the failuremessage field is updated.  The message file is not    *)
    (* deleted.   If UseAuth = TRUE then we attempt to use the SMTP     *)
    (* AUTH command to authenticate ourselves, with username AuthName   *)
    (* and password AuthPass.  Note that local recipients are ignored;  *)
    (* they are handled elsewhere.  Precondition: job <> NIL.           *)

    (* The function result is TRUE iff we manage to send the message to *)
    (* at least one of the recipients.  Any failures, which are on the  *)
    (* job^.sendto^.remote list, are left for the caller to handle.     *)
    (* On exit RetryNeeded is TRUE iff there is at least one recipient  *)
    (* on that list for which a retry could be justified.               *)

    (* NOTE: RetryNeeded = TRUE means that there are some items that    *)
    (* deserve another try.                                             *)
    (* TryNextOption = TRUE means that the entire job failed, and that  *)
    (* we should move on to the relay host if that option exists.       *)

    VAR success, ConnectionLost, ChunkingAvailable, CreateSuccessList: BOOLEAN;
        s: Socket;
        Buffer, Buffer2, failuremessage: ARRAY [0..511] OF CHAR;
        SB: SBuffer;
        p, previous, current, successlist, prevsuccess: RelayListPointer;
        RecipientCount, j, code, sent: CARDINAL;
        ch: CHAR;

    BEGIN
        failuremessage := "";
        ChunkingAvailable := FALSE;
        Buffer := "Trying ";
        IPToString (IPaddress, TRUE, Buffer2);
        Strings.Append (Buffer2, Buffer);
        LogTransaction (job^.ID, Buffer);

        (* Ensure that the "from" address is delimited by angle brackets. *)

        IF job^.sender[0] <> '<' THEN
            Strings.Insert ('<', 0, job^.sender);
            Strings.Append ('>', job^.sender);
        END (*IF*);
        RecipientCount := 0;

        (* Connect to a host and send the EHLO or HELO command.  At     *)
        (* this stage we also assign a value to job^.LocalHost.         *)

        job^.LocalHost := OurHostName;
        s := ConnectToHost (IPaddress, port, job^.ID, failuremessage);
        SB := CreateSBuffer (s, TRUE);
        SetTimeout (SB, 150);
        success := (CAST(ADDRESS,SB) <> NIL) AND (s <> NotASocket)
                       AND PositiveResponse(SB, ConnectionLost);
        IF s = NotASocket THEN
            Strings.Assign (failuremessage, Buffer);
        ELSIF CAST(ADDRESS,SB) = NIL THEN
            Strings.Assign ("Out of memory, abandoning connection attempt", failuremessage);
            Strings.Assign (failuremessage, Buffer);
        ELSE
            Strings.Assign ('< ', Buffer);
            GetLastLine (SB, failuremessage);
            Strings.Append (failuremessage, Buffer);
        END (*IF*);
        LogTransaction (job^.ID, Buffer);
        IF success THEN
            failuremessage[0] := Nul;
            GetOurHostName (s, job^.LocalHost);
            SetTimeout (SB, 900);
            success := DoLogin (SB, job^.LocalHost, UseAuth,
                                AuthName, AuthPass, ConnectionLost,
                                ChunkingAvailable, job^.ID);
            IF NOT success THEN
                GetLastLine (SB, failuremessage);
                LogTransactionL (job^.ID, "Login failed");
            END (*IF*);
        END (*IF*);

        RetryNeeded := NOT success;
        TryNextOption := RetryNeeded;

        IF success THEN

            (* We are now logged in. *)

            Buffer := "Connected to ";
            AppendHostName (IPaddress, Buffer);
            LogTransaction (job^.ID, Buffer);

            Buffer := "MAIL FROM: ";
            Strings.Append (job^.sender, Buffer);
            success := SendCommand (SB, job^.ID, Buffer, ConnectionLost);
            RetryNeeded := NOT success;
            IF success THEN

                p := job^.sendto^.remote;
                (*failuremessage := "500 No valid recipients";*)

                WHILE success AND (p <> NIL) DO
                    p^.flag := FALSE;
                    IF p^.KeepTrying THEN
                        Buffer := "RCPT TO: <";
                        Strings.Append (p^.username, Buffer);
                        IF p^.domain[0] <> Nul THEN
                            Strings.Append ('@', Buffer);
                            Strings.Append (p^.domain, Buffer);
                        END (*IF*);
                        Strings.Append ('>', Buffer);
                        Strings.Assign ("> ", Buffer2);
                        Strings.Append (Buffer, Buffer2);
                        LogTransaction (job^.ID, Buffer2);
                        ConnectionLost := NOT SendLine (SB, Buffer, sent);
                        EVAL (FlushOutput (SB));
                        Strings.Delete (Buffer, 0, 9);
                        success := NOT ConnectionLost;
                        IF success THEN
                            p^.flag := PositiveResponse(SB, ConnectionLost);
                            GetLastLine (SB, Buffer2);
                            Strings.Insert ("< ", 0, Buffer2);
                            LogTransaction (job^.ID, Buffer2);
                            IF p^.flag THEN
                                INC (RecipientCount);
                                Strings.Append (" - accepted", Buffer);
                            ELSE
                                GetLastLine (SB, p^.failuremessage);
                                code := 0;
                                FOR j := 0 TO 2 DO
                                    ch := p^.failuremessage[j];
                                    IF ch IN Digits THEN
                                        code := 10*code + (ORD(ch) - ORD('0'));
                                    END (*IF*);
                                END (*FOR*);
                                p^.KeepTrying := (code < 550) OR (code > 553);
                                RetryNeeded := RetryNeeded OR p^.KeepTrying;
                                Strings.Append (" - ", Buffer);
                                Strings.Append (p^.failuremessage, Buffer);
                            END (*IF*);
                            LogTransaction (job^.ID, Buffer);
                        END (*IF*);
                    END (*IF*);
                    p := p^.next;
                END (*WHILE*);

            END (*IF*);

            success := success AND (RecipientCount > 0);
            IF success THEN
                success := SendDataFile (SB, job, ChunkingAvailable,
                                         ConnectionLost, failuremessage);
            ELSE
                LogTransactionL (job^.ID, "Failure before we could call SendDataFile");
            END (*IF*);

            (* We should try to log out even if the above failed. *)

            EVAL (SendCommand (SB, job^.ID, "QUIT", ConnectionLost));

        END (*IF*);

        CloseSBuffer (SB);

        (* If 'success' is now TRUE, we have managed to send the mail   *)
        (* to all recipients for whom p^.flag is set.  Strip those out  *)
        (* so that p^.sendto holds only the failures.  If LogOutgoing   *)
        (* is TRUE then we also build a list of successes, which will   *)
        (* be discarded as soon as we've written the log item.          *)

        CreateSuccessList := LogOutgoing;
        successlist := NIL;  prevsuccess := NIL;
        IF success THEN
            IF RecipientCount > 0 THEN
                RecipientCount := 0;
                previous := NIL;  p := job^.sendto^.remote;
                WHILE p <> NIL DO
                    IF p^.flag THEN
                        current := p;  p := p^.next;
                        IF previous = NIL THEN
                            job^.sendto^.remote := p;
                        ELSE
                            previous^.next := p;
                        END (*IF*);
                        IF CreateSuccessList THEN
                            current^.next := NIL;
                            IF prevsuccess = NIL THEN
                                successlist := current;
                            ELSE
                                prevsuccess^.next := current;
                            END (*IF*);
                            prevsuccess := current;
                        ELSE
                            DEALLOCATE (current, SIZE(RelayList));
                        END (*IF*);
                        INC (RecipientCount);
                        DEC (job^.sendto^.RemoteCount);
                    ELSE
                        previous := p;  p := previous^.next;
                    END (*IF*);
                END (*WHILE*);
                IF CreateSuccessList THEN
                    WriteLogItem (job, IPaddress, successlist);
                    WHILE successlist <> NIL DO
                        prevsuccess := successlist;
                        successlist := successlist^.next;
                        DEALLOCATE (prevsuccess, SIZE(RelayListEntry));
                    END (*WHILE*);
                END (*IF*);
            END (*IF*);

        ELSE
            SetFailureMessage (job, failuremessage);
            RetryNeeded := RetryNeeded AND (job^.sendto <> NIL)
                                       AND (job^.sendto^.RemoteCount > 0);
        END (*IF*);

        RETURN success;

    END DeliverDeLetter;

(************************************************************************)

PROCEDURE SendToAllRecipients (job: OutJobPtr);

    (* Sends one mail item to a list of recipients.  On successful      *)
    (* delivery the corresponding record is removed from job^.sendto.   *)
    (* For failed deliveries, the failuremessage field is updated.  The *)
    (* message file is not deleted.  Precondition: job <> NIL.          *)

    (* Remark: we only have to deal with non-local mail, because the    *)
    (* local mail has already been delivered by the Sorter task.  Any   *)
    (* local mail still left on job^.sendto is a failed delivery.       *)

    VAR TryNextOption, RetryNeeded: BOOLEAN;  message: ARRAY [0..511] OF CHAR;
        HostUsed: HostName;

    (********************************************************************)

    PROCEDURE SendViaRelay (host: HostName;  port: CARDINAL;
                           AuthOption: CARDINAL;
                           AuthUser: UserName;  AuthPass: PassString)
                                                                : BOOLEAN;

        (* Try to send the mail via the given host, using host lookup   *)
        (* rather than MX lookup.  If necessary and we get multiple IP  *)
        (* addresses for the host, we try the multiple addresses in the *)
        (* order that gethostbyname returns them.                       *)
        (* AuthOption = 0 for no authentication, 1 for SMTP AUTH, and   *)
        (* 2 for POP-before-SMTP authentication.                        *)

        (* This procedure is invoked when we want to send the mail via  *)
        (* a relay host, a case where authentication should be used     *)
        (* as specified in the relay options.                           *)

        (* The function result is TRUE iff we manage to send the        *)
        (* message to at least one of the recipients.  Any failures,    *)
        (* which are left on the job^.sendto^.remote list, are left for *)
        (* the caller to handle.                                        *)

        CONST Max = 31;

        VAR success, abort: BOOLEAN;
            HostInfo: HostEntPtr;
            p: AddressPointerArrayPointer;
            k: CARDINAL;
            message: ARRAY [0..511] OF CHAR;

        BEGIN
            Strings.Assign ("Trying to send via host ", message);
            Strings.Append (host, message);
            LogTransaction (job^.ID, message);
            success := FALSE;
            IF NameIsNumeric(host) THEN
                k := inet_addr(host);
                IF AuthOption = 2 THEN
                    DoPOPLogin (inet_addr(AuthPOPhost), AuthPOPport,
                                            AuthUser, AuthPass, ExtraLogging, job^.ID);
                END (*IF*);
                success := DeliverDeLetter (job, k, port,
                                            AuthOption = 1,
                                            AuthUser, AuthPass,
                                            TryNextOption, RetryNeeded);
            ELSE
                HostInfo := gethostbyname (host);
                IF HostInfo = NIL THEN p := NIL
                ELSE p := HostInfo^.h_addr_list
                END (*IF*);
                IF p = NIL THEN
                    SetFailureMessage (job, "400 domain name lookup failure");
                ELSE
                    k := 0;
                    abort := FALSE;
                    REPEAT
                        IF ShutdownRequest OR WeAreOffline THEN
                            SetFailureMessage (job, "400 delivery delayed by server going offline");
                            abort := TRUE;
                        ELSE
                            IF AuthOption = 2 THEN
                                DoPOPLogin (p^[k]^, AuthPOPport, AuthUser,
                                             AuthPass, ExtraLogging, job^.ID);
                            END (*IF*);
                            success := DeliverDeLetter (job, p^[k]^, port,
                                                        AuthOption = 1,
                                                        AuthUser, AuthPass,
                                                        TryNextOption, RetryNeeded);
                            INC(k);
                        END (*IF*);
                    UNTIL success OR abort OR (NOT RetryNeeded) OR (p^[k] = NIL);
                END (*IF*);
            END (*IF*);

            IF success THEN
                HostUsed := host;
            ELSE
                IF job^.sendto^.remote = NIL THEN

                    (* Can this case ever occur?  I have a feeling that *)
                    (* this check was done at an earlier stage, but we  *)
                    (* need to check whether the list has been pruned   *)
                    (* since then.                                      *)

                    LogTransactionL (job^.ID, "500 no recipients specified");
                    RetryNeeded := FALSE;
                ELSE
                    (* No need to log failure message, because it was   *)
                    (* already logged at the point of failure.          *)
                END (*IF*);
            END (*IF*);

            RETURN success;

        END SendViaRelay;

    (********************************************************************)

    PROCEDURE SendToDomain (domain: HostName;  port: CARDINAL): BOOLEAN;

        (* Try to send the mail to the given domain.  If necessary and  *)
        (* we get multiple possible addresses for that domain, we try   *)
        (* the multiple addresses in order of preference.               *)

        (* This procedure is invoked when we want to send the mail      *)
        (* directly, i.e. not using a relay host.  Authentication is    *)
        (* inappropriate in this case.                                  *)

        (* The function result is TRUE iff we manage to send the        *)
        (* message to at least one of the recipients.  Any failures,    *)
        (* which are left on the job^.sendto^.remote list, are left for *)
        (* the caller to handle.                                        *)

        CONST Max = 31;

        VAR success: BOOLEAN;  j: CARDINAL;
            address: ARRAY [0..Max] OF CARDINAL;
            message: ARRAY [0..511] OF CHAR;
            IPstr: ARRAY [0..21] OF CHAR;

        BEGIN
            success := FALSE;  j := 0;
            Strings.Assign ("Looking up domain name ", message);
            Strings.Append (domain, message);
            LogTransaction (job^.ID, message);
            CASE DoMXLookup (domain, address) OF
              |  0: REPEAT
                        IPToString (address[j], TRUE, IPstr);
                        Strings.Assign ("MX lookup result ", message);
                        Strings.Append (IPstr, message);
                        LogTransaction (job^.ID, message);
                        IF ShutdownRequest OR WeAreOffline THEN
                            SetFailureMessage (job, "400 delivery delayed by server going offline");
                            address[j] := 0;
                        ELSIF AddressIsLocal(address[j]) THEN
                            SetFailureMessage (job, "400 no relay path to destination");
                            address[j] := 0;
                        ELSE
                            success := DeliverDeLetter (job, address[j], port,
                                                        FALSE, '', '',
                                                        TryNextOption, RetryNeeded);
                            INC (j);
                        END (*IF*);
                    UNTIL (NOT RetryNeeded) OR (j > Max) OR (address[j] = 0);
              |  1:
                    SetFailureMessage (job, "500 unknown host or domain");
                    RetryNeeded := FALSE;
              |  2:
                    SetFailureMessage (job, "400 domain name lookup failure");
              |  3:
                    SetFailureMessage (job, "400 nameserver not responding");
            END (*CASE*);

            IF success THEN
                AddressToHostName (address[j-1], HostUsed);
            ELSE
                IF job^.sendto^.remote = NIL THEN

                    (* Can this case ever occur?  I have a feeling that *)
                    (* this check was done at an earlier stage, but we  *)
                    (* need to check whether the list has been pruned   *)
                    (* since then.                                      *)

                    LogTransactionL (job^.ID, "500 no recipients specified");
                    RetryNeeded := FALSE;
                ELSE
                    (* No need to log failure message, because it was   *)
                    (* already logged at the point of failure.          *)
                END (*IF*);
            END (*IF*);

            RETURN success;

        END SendToDomain;

    (********************************************************************)

    VAR success: BOOLEAN;
        p: RelayListPointer;

    BEGIN
        Strings.Assign ("Attempting to send to ", message);
        Strings.Append (job^.sendto^.remote^.username, message);
        IF job^.sendto^.remote^.domain[0] <> CHR(0) THEN
            Strings.Append ('@', message);
            Strings.Append (job^.sendto^.remote^.domain, message);
        END (*IF*);
        IF job^.sendto^.RemoteCount > 1 THEN
            Strings.Append (" and others", message);
        END (*IF*);
        LogTransaction (job^.ID, message);
        RetryNeeded := TRUE;
        IF job^.Recirculate THEN

            (* Send to the loopback interface, bypassing authentication. *)

            AddressToHostName (LoopbackAddress, HostUsed);
            success := DeliverDeLetter (job, LoopbackAddress, OurSMTPPort,
                                           FALSE, '', '', TryNextOption, RetryNeeded)
                              AND NOT RetryNeeded;

        ELSIF ForwardRelayOption = 2 THEN
            success := SendViaRelay (ForwardRelayHost, ForwardRelayPort,
                                    AuthOption, AuthUser, AuthPass);
        ELSE
            success := SendToDomain (job^.domain, job^.SMTPport);
        END (*IF*);

        (* If ForwardRelayOption is 1, the failures get a second        *)
        (* chance: we send them via the backup relay.                   *)

        IF (ForwardRelayOption = 1)
                         AND NOT(job^.Recirculate
                                 OR ShutdownRequest OR WeAreOffline) THEN

            (* If the entire job failed, try sending via the relay host.*)

            IF TryNextOption THEN
                LogTransactionL (job^.ID, "Direct send failed, now trying the backup relay");
                job^.domain := ForwardRelayHost;
                job^.SMTPport := ForwardRelayPort;
                success := SendViaRelay (ForwardRelayHost, ForwardRelayPort,
                                        AuthOption, AuthUser, AuthPass);
            END (*IF*);

            (* If there are still failures, work out whether any of     *)
            (* them have the KeepTrying flag set.                       *)

            p := job^.sendto^.remote;
            RetryNeeded := FALSE;
            WHILE (p <> NIL) AND NOT RetryNeeded DO
                RetryNeeded := p^.KeepTrying;
                p := p^.next;
            END (*WHILE*);

            (* For the failed items, a send via the relay host is       *)
            (* justified if we have items whose KeepTrying flag is set; *)
            (* but as a policy matter we don't do the retry until the   *)
            (* main host has been tried enough times.                   *)

            IF RetryNeeded AND (job^.RetryNumber > 4) THEN
                LogTransactionL (job^.ID, "Direct send failed, now trying the backup relay");
                job^.domain := ForwardRelayHost;
                job^.SMTPport := ForwardRelayPort;
                success := SendViaRelay (ForwardRelayHost, ForwardRelayPort,
                                        AuthOption, AuthUser, AuthPass);
            END (*IF*);

        END (*IF*);

        IF success THEN
            Strings.Assign ("Delivered ", message);
        ELSE
            Signal(CheckIfOnline);
            Strings.Assign ("Failed to deliver ", message);
        END (*IF*);
        Strings.Append (job^.file, message);
        IF success THEN
            Strings.Append (" via ", message);
            Strings.Append (HostUsed, message);
        END (*IF*);
        LogTransaction (job^.ID, message);

    END SendToAllRecipients;

(************************************************************************)

PROCEDURE MailOneMessage (VAR (*INOUT*) p: OutJobPtr);

    (* Sends one mail item, possibly to multiple recipients.  On        *)
    (* return, p^.sendto lists the recipients for whom the operation    *)
    (* failed and for whom no retry should be attempted.  (Special case:*)
    (* after a shutdown request, p^.sendto might also contain intended  *)
    (* recipients that we didn't get time to deal with.)  Retries are   *)
    (* handled inside this procedure by generating a new job descriptor.*)

    (* The message file is not deleted, regardless of success or        *)
    (* failure; we leave that decision up to the caller.                *)
    (* Precondition: p <> NIL.                                          *)

    VAR ToRetry: RelayList;
        previous, current, next: RelayListPointer;
        q: OutJobPtr;
        RetryCount, minutes: CARDINAL;

    BEGIN
        IF FileSys.Exists (p^.file) THEN
            SendToAllRecipients (p);
        ELSE
            p^.file := "";
            SetFailureMessage (p, "500 Message file has been lost");
        END (*IF*);

        (* At this stage p^.sendto contains the recipients for which    *)
        (* delivery failed.  We now separate out the ones for which     *)
        (* a retry is justified.  Note that we only check the remote    *)
        (* recipients, because a retry is never going to work for       *)
        (* failed local deliveries.                                     *)

        ToRetry := NIL;  RetryCount := 0;

        IF p^.RetryNumber < MaxRetries THEN
            previous := NIL;  current := p^.sendto^.remote;
            WHILE current <> NIL DO
                next := current^.next;
                IF current^.KeepTrying AND (current^.failuremessage[0] <> '5') THEN
                    IF previous = NIL THEN
                        p^.sendto^.remote := next;
                    ELSE
                        previous^.next := next;
                    END (*IF*);
                    current^.next := ToRetry;
                    ToRetry := current;
                    INC (RetryCount);
                ELSE
                    previous := current;
                END (*IF*);
                current := next;
            END (*WHILE*);
        END (*IF*);

        IF RetryCount > 0 THEN

            (* Make a new copy of the job details, and a copy of the    *)
            (* message file, because we want to preserve the state of   *)
            (* p for the caller.                                        *)

            NEW (q);  q^ := p^;
            NEW (q^.sendto);
            q^.sendto^.local := NIL;  q^.sendto^.remote := ToRetry;
            q^.sendto^.RemoteCount := RetryCount;
            DEC (p^.sendto^.RemoteCount, RetryCount);
            IF q^.RetryNumber <= MaxVarRetries THEN
                minutes := RetryInterval[q^.RetryNumber];
            ELSE
                minutes := RetryInterval[MaxVarRetries];
            END (*IF*);
            INC (q^.sendtime, 60*minutes);
            INC (q^.RetryNumber);
            StoreMessageFile (q);         (* hidden *)
            IF q^.NotifyOnFailure AND (q^.RetryNumber = WarnRetries) THEN
                SendRejectionLetter (q, q^.ID, FALSE);
            END (*IF*);
            AddToRetryList (q);

        END (*IF*);

    END MailOneMessage;

(************************************************************************)
(*                 THE TASK THAT HANDLES OUTGOING MAIL                  *)
(************************************************************************)

VAR LogMessage: ARRAY [0..511] OF CHAR;

PROCEDURE MailerTask (tasknum: ADDRESS);

    (* Takes the mail on the OutQueue, and sends it.  We could be       *)
    (* running multiple copies of this task.                            *)

    VAR p: OutJobPtr;  LogID: TransactionLogID;
        TaskName: NameString;
        filename: FilenameString;
        TaskNumber, N, status: CARDINAL;
        AllSent: BOOLEAN;

        (* while testing *)
        (*ptib: OS2.PTIB;  ppib: OS2.PPIB;*)

    BEGIN
        Wait (SystemUp);
        Signal (SystemUp);
        TaskNumber := CAST(CARDINAL, tasknum);

        Obtain (NumberOfDaemonsLock);
        INC (NumberOfDaemons);
        INC (TaskCount);
        Release (NumberOfDaemonsLock);

        TaskName := "Send_  ";
        TaskName[5] := CHR(ORD('0') + TaskNumber DIV 10);
        TaskName[6] := CHR(ORD('0') + TaskNumber MOD 10);
        TaskName[7] := CHR(0);
        LogID := CreateLogID (WCtx, TaskName);
        Strings.Assign ("Starting task ", LogMessage);
        Strings.Append (TaskName, LogMessage);
        LogTransaction (LogID, LogMessage);
        InProgress[TaskNumber][0] := Nul;

        (* Test code: report the priority of this thread. *)

        (*
        LogMessage := "Our priority is ";
        OS2.DosGetInfoBlocks (ptib, ppib);
        AppendCard (ptib^.tib_ptib2^.tib2_ulpri, LogMessage);
        LogTransaction (LogID, LogMessage);
        *)

        LOOP
            Obtain (NumberOfDaemonsLock);

            (* Terminate a task if too many copies are running. *)

            IF NumberOfDaemons > DesiredNumberOfDaemons THEN
                IF TaskNumber > DesiredNumberOfDaemons THEN
                    DaemonActive[TaskNumber] := FALSE;
                    DEC (NumberOfDaemons);
                    DEC (TaskCount);
                    Release (NumberOfDaemonsLock);
                    TaskName[5] := CHR(ORD('0') + TaskNumber DIV 10);
                    TaskName[6] := CHR(ORD('0') + TaskNumber MOD 10);
                    Strings.Assign ("Ending task ", LogMessage);
                    Strings.Append (TaskName, LogMessage);
                    (*Strings.Append (", Signal(SomethingToSend)", LogMessage);*)
                    LogTransaction (LogID, LogMessage);
                    DiscardLogID (LogID);
                    Signal (SomethingToSend);
                    Signal (TaskDone);
                    RETURN;
                ELSE
                    Signal (SomethingToSend);
                END (*IF*);
            END (*IF*);

            (* Create one more task if not enough copies are running. *)

            IF NumberOfDaemons < DesiredNumberOfDaemons THEN
                N := 0;
                REPEAT
                    INC (N);
                    IF N = TaskNumber THEN
                        INC (N);
                    END (*IF*);
                UNTIL (N > DesiredNumberOfDaemons) OR NOT DaemonActive[N];
                IF N <= DesiredNumberOfDaemons THEN
                    TaskName[5] := CHR(ORD('0') + N DIV 10);
                    TaskName[6] := CHR(ORD('0') + N MOD 10);
                    DaemonActive[N] :=
                         CreateTask1 (MailerTask, 4, TaskName, CAST(ADDRESS,N));
                END (*IF*);
            END (*IF*);

            Release (NumberOfDaemonsLock);

            (* End of check on changing the number of running tasks. *)

            Obtain (OutboundMail.access);
            InProgress[TaskNumber][0] := Nul;
            Release (OutboundMail.access);
            Wait (SomethingToSend);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);

            IF WeAreOffline THEN

                WITH OfflineData DO
                    Obtain(access);
                    INC (PendingCount);
                    Release(access);
                    p := NIL;
                END (*WITH*);

            ELSE
                (* Take the first element of OutboundMail, if any. *)

                Obtain (OutboundMail.access);
                p := OutboundMail.head;
                IF p <> NIL THEN
                    OutboundMail.head := p^.next;
                    p^.next := NIL;
                    IF p^.sendto <> NIL THEN
                        DEC (OutboundMail.RecipientCount.count, p^.sendto^.RemoteCount);
                    END (*IF*);
                    IF OutboundMail.head = NIL THEN
                        OutboundMail.tail := NIL;

                        (* We've just taken the last item, so force a   *)
                        (* rescan of the forward directory.             *)

                        ForceForwardDirCheck := TRUE;
                        Signal (CheckIfOnline);
                    END (*IF*);
                    InProgress[TaskNumber] := p^.file;
                END (*IF*);
                Release (OutboundMail.access);
            END (*IF*);

            (* This next section of code is skipped if there is nothing *)
            (* to send.                                                 *)

            IF p <> NIL THEN
                (*
                WITH DetailedCount DO
                    Obtain (access);
                    DEC (joutbound);
                    DEC (rroutbound, RRCount(p));
                    Release (access);
                END (*WITH*);
                *)
                p^.ID := LogID;
                filename := p^.file;

                WITH JobCount DO
                    Obtain (access);
                    DEC (count);
                    IF count < JobCountLimitLow THEN
                        limit := JobCountLimitHigh;
                    END (*IF*);
                    Release (access);
                END (*WITH*);

                IF p^.sendto^.remote <> NIL THEN
                    MailOneMessage (p);
                END (*IF*);
                (*ShowCounts;*)

                (* On return from MailOneMessage, p^.sendto lists only  *)
                (* the failures.  If the failure was because of a       *)
                (* shutdown request then we should preserve the message *)
                (* for a reattempt on the next startup.  Otherwise this *)
                (* is a hard failure, one for which we give up trying.  *)
                (* (Soft failures, for which a retry is appropriate,    *)
                (* have already been handled by MailOneMessage.)        *)

                AllSent := (p^.sendto^.remote = NIL)
                                            AND (p^.sendto^.local = NIL);
                IF NOT AllSent AND NOT ShutdownRequest THEN
                    IF p^.NotifyOnFailure THEN
                        SendRejectionLetter (p, LogID, TRUE);
                    END (*IF*);
                    AllSent := TRUE;
                END (*IF*);

                (* Delete the job record and message file, except in    *)
                (* the special case of shutdown.  Even then we can      *)
                (* delete the job record - but not the message file -   *)
                (* because the job will be re-created the next time     *)
                (* this program is run.                                 *)

                IF AllSent THEN
                    IF filename[0] <> Nul THEN
                        status := OS2.DosDelete (filename);
                        p^.file[0] := Nul;
                        IF status <> 0 THEN
                             Strings.Assign ("Can't delete ", LogMessage);
                             Strings.Append (filename, LogMessage);
                             Strings.Append (", error code ", LogMessage);
                             AppendCard (status, LogMessage);
                             LogTransaction (LogID, LogMessage);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
                DiscardCombinedRecipientList (p^.sendto);
                DEALLOCATE (p, SIZE(OutJob));

            END (*IF*);

        END (*LOOP*);

        (* For neatness, and to make it easier to check the program for *)
        (* correctness, discard any pending jobs that we didn't get to  *)
        (* because of shutdown.                                         *)

        Obtain (OutboundMail.access);
        p := OutboundMail.head;
        WHILE p <> NIL DO
            OutboundMail.head := p^.next;
            p^.next := NIL;
            DiscardJob (p, TRUE);
            p := OutboundMail.head;
        END (*WHILE*);
        Release (OutboundMail.access);

        Signal (TaskDone);

    END MailerTask;

(************************************************************************)
(*                THE TASK THAT SORTS THE OUTGOING MAIL                 *)
(************************************************************************)

PROCEDURE RebuildMessageFile (job: OutJobPtr);

    (* To be called after the contents of job^.sendto might have        *)
    (* changed.  We copy the file with an updated preamble, and delete  *)
    (* the original.  If there are no recipients left, we simply        *)
    (* delete the file and return with job^.file = "".                  *)
    (* If a file is created, it is created as hidden.                   *)

    VAR oldfile: FilenameString;

    BEGIN
        oldfile := job^.file;
        IF (job^.sendto = NIL)
                   OR ((job^.sendto^.local = NIL)
                        AND (job^.sendto^.remote = NIL)) THEN
            job^.file[0] := Nul;
        ELSE
            StoreMessageFile(job);         (* hidden *)
        END (*IF*);

        DeleteFile (oldfile);

    END RebuildMessageFile;

(************************************************************************)

PROCEDURE DistributeLocal (job: OutJobPtr);

    (* Sends one mail item to one or more local recipients.  The file   *)
    (* is not deleted.  On return, job^.sendto^.local is a list of      *)
    (* recipients for whom the delivery failed, and job^.sendto^.remote *)
    (* is unchanged.                                                    *)

    VAR cid: ChanId;
        TempName, FinalName, CopyTo: FilenameString;  success: BOOLEAN;
        tries, result, count, active: CARDINAL;
        previous, current, next: LocalRecipientList;

    BEGIN
        count := 0;
        cid := OpenNewOutputFile (ForwardDirName, ".###", TempName, TRUE);
        success := cid <> NoSuchChannel;
        IF success THEN
            IF job^.file[0] <> Nul THEN
                AppendBody (job^.file, job^.offset, cid, 0);
            END (*IF*);
            CloseFile (cid);

            (* Now make a copy for each recipient. *)

            previous := NIL;  current := job^.sendto^.local;
            WHILE current <> NIL DO
                IF current^.Skip THEN
                    success := TRUE;
                ELSIF IsActiveUser (current^.U, active, CopyTo) THEN

                    (* The 'active' and 'CopyTo' values can be ignored, *)
                    (* because we've already accounted for the special  *)
                    (* cases when building the recipient list.          *)

                    success := TRUE;  tries := 0;
                    REPEAT
                        (*FinalName := current^.DirName;*)
                        LockLocalUser (current^.U);
                        NewMessageFilename (current^.U, FinalName);
                        result := OS2.DosCopy (TempName, FinalName, 0);
                        UnlockLocalUser (current^.U);
                        INC (tries);
                    UNTIL (result <> 32) OR (tries > 50);
                    IF result <> 0 THEN
                        LogTransactionL (job^.ID, "Could not copy message file");
                        Strings.Assign ("450 unable to store message file", current^.failuremessage);
                        success := FALSE;
                    END (*IF*);
                    IF success THEN
                        Strings.Assign ("Local delivery to ", FinalName);
                        Strings.Append (current^.user, FinalName);
                        LogTransaction (job^.ID, FinalName);
                        INC (count);
                    END (*IF*);
                ELSE
                    Strings.Assign ("550 unknown user ", current^.failuremessage);
                    Strings.Append (current^.user, current^.failuremessage);
                    success := FALSE;
                END (*IF*);

                next := current^.next;
                IF success THEN
                    IF previous = NIL THEN
                        job^.sendto^.local := next;
                    ELSE
                        previous^.next := next;
                    END (*IF*);
                    UnloadLocalUser (current^.U);
                    DEALLOCATE (current, SIZE(LocalRecipientRecord));
                ELSE
                    previous := current;
                END (*IF*);
                current := next;

            END (*WHILE*);
        END (*IF*);

        DeleteFile (TempName);
        RebuildMessageFile (job);         (* hidden *)

        IF count > 0 THEN
            PublicNotification;
        END (*IF*);

    END DistributeLocal;

(************************************************************************)

PROCEDURE Split (p: OutJobPtr;  VAR (*OUT*) q: OutJobPtr);

    (* Separates the list of addressees for p so that on exit the p     *)
    (* list is all for the same address, and the q list is what         *)
    (* remains.  Assumption: p <> NIL.                                  *)

    (* This procedure can result in changes to the message file and     *)
    (* to the creation of new message files, but all files created      *)
    (* are hidden files.                                                *)

    VAR current, ptail, qhead, qtail: RelayListPointer;
        count: CARDINAL;

    BEGIN
        ptail := p^.sendto^.remote;  current := ptail^.next;
        p^.domain := ptail^.domain;
        qhead := NIL;  qtail := NIL;
        count := 1;
        REPEAT
            WHILE (current <> NIL) AND (count < MaxRecipientsPerLetter)
                           AND Strings.Equal(current^.domain, p^.domain) DO
                ptail := current;  current := ptail^.next;
                INC (count);
            END (*WHILE*);

            IF current <> NIL THEN
                (* Move current to q list *)
                IF qhead = NIL THEN qhead := current
                ELSE qtail^.next := current
                END (*IF*);
                REPEAT
                    qtail := current;  current := qtail^.next;
                UNTIL (current = NIL) OR ((count < MaxRecipientsPerLetter)
                               AND Strings.Equal(current^.domain, p^.domain));
            END (*IF*);

            ptail^.next := current;

        UNTIL current = NIL;

        IF qhead = NIL THEN
            q := NIL;
        ELSE
            qtail^.next := NIL;
            NEW (q);
            q^ := p^;
            NEW (q^.sendto);
            q^.sendto^.local := NIL;
            q^.sendto^.remote := qhead;
            q^.sendto^.RemoteCount := p^.sendto^.RemoteCount - count;
            p^.sendto^.RemoteCount := count;
            StoreMessageFile (q);           (* hidden *)
            RebuildMessageFile (p);         (* hidden *)
        END (*IF*);

    END Split;

(************************************************************************)

PROCEDURE LimitRecipients (p: OutJobPtr;  VAR (*OUT*) q: OutJobPtr;
                                               VAR (*INOUT*) N: CARDINAL);

    (* If p has too many remote recipients, creates a q list and moves  *)
    (* the surplus to the q list.                                       *)

    (* On entry, N is the number of recipients on the p list.  On exit, *)
    (* it is the number of recipients on the q list.                    *)

    (* This procedure can result in changes to the message file and     *)
    (* to the creation of new message files, but all files created      *)
    (* are hidden files.                                                *)

    VAR current, ptail: RelayListPointer;
        count: CARDINAL;

    BEGIN
        IF N <= MaxRecipientsPerLetter THEN
            q := NIL;  N := 0;
        ELSE
            count := 1;
            ptail := p^.sendto^.remote;  current := ptail^.next;
            WHILE count < MaxRecipientsPerLetter DO
                ptail := current;  current := ptail^.next;
                INC (count);
            END (*WHILE*);
            ptail^.next := NIL;
            IF current = NIL THEN
                q := NIL;
            ELSE
                NEW (q);
                q^ := p^;
                NEW (q^.sendto);
                q^.sendto^.local := NIL;
                q^.sendto^.remote := current;
                q^.sendto^.RemoteCount := p^.sendto^.RemoteCount - count;
                p^.sendto^.RemoteCount := count;
                StoreMessageFile (q);           (* hidden *)
                DEC (N, count);
                RebuildMessageFile (p);         (* hidden *)
            END (*IF*);
        END (*IF*);
    END LimitRecipients;

(************************************************************************)

PROCEDURE Sorter;

    (* Takes the mail in the MailSack, sorts it, and moves the jobs to  *)
    (* OutQueue.  All message files remain hidden as long as they are   *)
    (* on one of this module's queues.  A message file becomes unhidden *)
    (* either when it is delivered to a local mailbox, or when it fails *)
    (* to fit on the queues and has to be set aside to be picked up     *)
    (* on a later scan of the visible files in the 'forward' directory. *)

    VAR p, q: OutJobPtr;  previous, current, next: RelayListPointer;
        D: Domain;  local, nonlocal: CARDINAL;
        LogID: TransactionLogID;

    BEGIN
        Wait (SystemUp);
        Signal (SystemUp);
        LogID := CreateLogID (WCtx, "Sorter ");
        LOOP
            Wait (MailSack.count);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);

            (* Take the first element of MailSack, if any. *)

            Obtain (MailSack.access);
            p := MailSack.head;
            IF p <> NIL THEN
                MailSack.head := p^.next;
                p^.next := NIL;
                IF MailSack.head = NIL THEN
                    MailSack.tail := NIL;
                END (*IF*);
            END (*IF*);
            Release (MailSack.access);

            (* Decrement the job count.  We are about to do the local   *)
            (* deliveries here and then deal with the non-local ones    *)
            (* by calling AddToOutQueue.  Each call to AddToOutQueue    *)
            (* will increment the job count again.                      *)

            IF p <> NIL THEN
                (*
                WITH DetailedCount DO
                    Obtain (access);
                    DEC (jsack);
                    DEC (rrsack, RRCount(p));
                    Release (access);
                END (*WITH*);
                *)
                WITH JobCount DO
                    Obtain (access);
                    DEC (count);
                    IF count < JobCountLimitLow THEN
                        limit := JobCountLimitHigh;
                    END (*IF*);
                    Release (access);
                END (*WITH*);
                (*ShowCounts;*)
            END (*IF*);

            (* Deal with local recipients.  A job that reached us via   *)
            (* SMTP won't have any local recipients, because they were  *)
            (* handled before the mail even reached the mail sack.      *)
            (* There might, however, be local recipients for mail that  *)
            (* another program put into the 'forward' directory for     *)
            (* direct delivery, or for rejection letters that were      *)
            (* generated by this module.  In these cases the recipients *)
            (* have not necessarily been classified into 'local' and    *)
            (* 'remote', so we should first check the 'remote' list     *)
            (* for local recipients.                                    *)

            (* While we're running through the list anyway, we can      *)
            (* count the nonlocal recipients.                           *)

            local := 0;  nonlocal := 0;
            IF p <> NIL THEN
                p^.ID := LogID;
                IF p^.sendto = NIL THEN
                    IF p^.file[0] <> Nul THEN
                        DeleteFile (p^.file);
                        p^.file[0] := Nul;
                    END (*IF*);
                    DiscardJob (p, FALSE);
                ELSIF p^.Recirculate THEN
                    nonlocal := p^.sendto^.RemoteCount;
                ELSE
                    previous := NIL;
                    current := p^.sendto^.remote;
                    WHILE current <> NIL DO
                        IF DomainIsLocal (current^.domain, D) THEN
                            INC (local);
                            EVAL (AddToLocalList (p^.sendto, current^.username,
                                                  current^.domain, D, TRUE,
                                                  TRUE, TRUE, LogID));
                            next := current^.next;
                            IF previous = NIL THEN
                                p^.sendto^.remote := next;
                            ELSE
                                previous^.next := next;
                            END (*IF*);
                            DEALLOCATE (current, SIZE(RelayListEntry));
                            DEC (p^.sendto^.RemoteCount);
                            current := next;
                        ELSE
                            previous := current;
                            current := current^.next;
                            INC (nonlocal);
                        END (*IF*);
                    END (*WHILE*);
                END (*IF*);

                (* Deliver the local mail, if any. *)

                RemoveInitialSkipRecords (p^.sendto^.local);
                IF NOT NoLocalRecipients(p^.sendto^.local) THEN
                    DistributeLocal (p);
                END (*IF*);

                (* Check for the case where there's nothing further     *)
                (* to do.                                               *)

                IF (p^.sendto^.remote = NIL)
                                 AND (p^.sendto^.local = NIL) THEN
                    IF p^.file[0] <> Nul THEN
                        DeleteFile (p^.file);
                    END (*IF*);
                    DiscardJob (p, FALSE);
                END (*IF*);

            END (*IF*);

            IF p <> NIL THEN
                p^.ID := LogID;
                LogTransaction (LogID, p^.file);

                (* It's possible that at this stage there are no remote *)
                (* recipients, but we still have to pass on the job     *)
                (* so that the failed local deliveries will result in   *)
                (* a 'bounce' message.                                  *)

                IF nonlocal = 0 THEN

                    AddToOutQueue (p);

                (* The value of ForwardRelayOption controls whether we  *)
                (* use ForwardRelayHost for outbound mail.              *)
                (*      0     don't use it                              *)
                (*      1     use it if direct route fails              *)
                (*      2     always use it                             *)

                ELSIF (ForwardRelayOption < 2) AND NOT p^.Recirculate THEN

                    (* Sort mail by receiver's destination domain. *)

                    p^.SMTPport := 25;
                    WHILE p <> NIL DO
                        Split (p, q);
                        AddToOutQueue (p);
                        p := q;
                    END (*WHILE*);

                ELSE
                    (* Go via relay host. *)
                    IF p^.Recirculate THEN
                        p^.domain := "127.0.0.1";
                        p^.SMTPport := OurSMTPPort;
                    ELSE
                        p^.domain := ForwardRelayHost;
                        p^.SMTPport := ForwardRelayPort;
                    END (*IF*);
                    REPEAT
                        LimitRecipients (p, q, nonlocal);
                        AddToOutQueue (p);
                        p := q;
                    UNTIL p = NIL;

                END (*IF*);

            END (*IF*);

        END (*LOOP*);

        (* For neatness, and to make it easier to check the program for *)
        (* correctness, discard any pending jobs that we didn't get to  *)
        (* because of shutdown.  Note that the files associated with    *)
        (* these jobs don't get deleted.                                *)

        Obtain (MailSack.access);
        p := MailSack.head;
        WHILE p <> NIL DO
            MailSack.head := p^.next;
            p^.next := NIL;
            DiscardJob (p, TRUE);
            p := MailSack.head;
        END (*WHILE*);
        MailSack.tail := NIL;
        Release (MailSack.access);

        Signal (TaskDone);

    END Sorter;

(************************************************************************)
(*            THE TASK THAT HANDLES RETRANSMISSION ATTEMPTS             *)
(************************************************************************)

PROCEDURE RetryTask;

    (* Runs forever, taking mail off the retry list and putting each    *)
    (* item on the output queue as it becomes time to send it.          *)
    (* We give up after about five days.                                *)

    (* This task does not create or destroy data, it merely moves       *)
    (* jobs from the retry list to the output queue.                    *)

    CONST DefaultCheckInterval = 30*1000;    (* thirty seconds *)

    VAR TimedOut: BOOLEAN;  CheckInterval, Now: CARDINAL;
        p: OutJobPtr;  LogID: TransactionLogID;

    BEGIN
        Wait (SystemUp);
        Signal (SystemUp);
        LogID := CreateLogID (WCtx, "Retry  ");
        CheckInterval := DefaultCheckInterval;
        LOOP
            TimedWait (Retry, CheckInterval, TimedOut);
            IF ShutdownRequest THEN
                EXIT (*LOOP*);
            END (*IF*);
            Obtain (RetryList.access);
            LOOP
                p := RetryList.head;
                Now := time();
                IF (p = NIL) OR (p^.sendtime > Now) THEN
                    EXIT (*LOOP*);
                END (*IF*);

                (* It's time to resend this item. *)

                RetryList.head := p^.next;
                p^.ID := LogID;
                (*
                WITH DetailedCount DO
                    Obtain (access);
                    DEC (jretry);
                    DEC (rrretry, RRCount(p));
                    Release (access);
                END (*WITH*);
                *)
                DEC (RetryList.RecipientCount.count);
                WITH JobCount DO
                    Obtain (access);
                    DEC (count);
                    IF count < JobCountLimitLow THEN
                        limit := JobCountLimitHigh;
                    END (*IF*);
                    Release (access);
                END (*WITH*);
                (*ShowCounts;*)
                AddToMailSack (p);

            END (*LOOP*);
            Release (RetryList.access);

            IF p = NIL THEN
                CheckInterval := DefaultCheckInterval;
            ELSE
                CheckInterval := 1000*(p^.sendtime - Now);
            END (*IF*);

        END (*LOOP*);

        (* For neatness, and to make it easier to check the program for *)
        (* correctness, discard any pending jobs that we didn't get to  *)
        (* because of shutdown.                                         *)

        Obtain (RetryList.access);
        p := RetryList.head;
        WHILE p <> NIL DO
            RetryList.head := p^.next;
            p^.next := NIL;
            DiscardJob (p, TRUE);
            p := RetryList.head;
        END (*WHILE*);
        Release (RetryList.access);

        Signal (TaskDone);

    END RetryTask;

(************************************************************************)
(*                    FILE INPUT/OUTPUT UTILITIES                       *)
(************************************************************************)

PROCEDURE FReadChar (cid: ChanId;  VAR (*OUT*) ch: CHAR): BOOLEAN;

    (* Reads a single character from a file. *)

    VAR NumberRead: CARDINAL;

    BEGIN
        ReadRaw (cid, ch, 1, NumberRead);
        IF NumberRead = 1 THEN
            RETURN TRUE;
        ELSE
            ch := Nul;  RETURN FALSE;
        END (*IF*);
    END FReadChar;

(************************************************************************)

PROCEDURE FReadString (cid: ChanId;
                                VAR (*OUT*) string: ARRAY OF CHAR;
                                Stoppers: CharSet;
                                VAR (*INOUT*) NextChar: CHAR): BOOLEAN;

    (* Reads a string from a file, stopping when NextChar is one of the *)
    (* characters in Stoppers, or when we reach the end of the file.    *)

    VAR j: CARDINAL;  success: BOOLEAN;

    BEGIN
        success := TRUE;  j := 0;
        WHILE success AND NOT (NextChar IN Stoppers) DO
            string[j] := NextChar;  INC(j);
            success := FReadChar (cid, NextChar);
        END (*WHILE*);
        string[j] := Nul;
        RETURN success;
    END FReadString;

(************************************************************************)

PROCEDURE ReadNameList (cid: ChanId;  VAR (*OUT*) count: CARDINAL;
                                 VAR (*INOUT*) NextChar: CHAR): RelayList;

    (* Reads a comma-separated list of names enclosed in parentheses. *)

    VAR p, last: RelayListPointer;  result: RelayList;
        forwardpath: PathString;

    BEGIN
        result := NIL;
        last := NIL;
        count := 0;
        IF NextChar = '(' THEN
            LOOP
                INC (count);
                NEW(p);
                p^.next := NIL;  p^.failuremessage := "";
                p^.KeepTrying := TRUE;
                EVAL( FReadChar(cid, NextChar)
                          AND FReadString (cid, forwardpath,
                                           CharSet{',', ')'}, NextChar));
                UserAndDomain (forwardpath, p^.username, p^.domain);
                IF last = NIL THEN result := p
                ELSE last^.next := p
                END (*IF*);
                last := p;
                IF NextChar <> ',' THEN EXIT(*LOOP*) END(*IF*);
            END (*LOOP*);
        END (*IF*);
        RETURN result;
    END ReadNameList;

(************************************************************************)
(*            CHECKING FOR NEW FILES IN THE FORWARD DIRECTORY           *)
(************************************************************************)

PROCEDURE CheckUnsentMail (LogID: TransactionLogID);

    (* Checks all *.FWD files in the "forward" directory, and adds them *)
    (* to our list of jobs to be done if they're not already there.     *)
    (* Normally they will already be there, so this procedure doesn't   *)
    (* often need to do anything; but by calling it periodically we     *)
    (* are able to deal with mail that's put in the "forward" directory *)
    (* by other programs.                                               *)

    VAR cid: ChanId;
        p: OutJobPtr;  NextChar: CHAR;  Now, CharsRead: CARDINAL;
        filesize: CARD64;
        mask, filename: FilenameString;
        preamble: PreambleType;
        D: DirectoryEntry;
        found: BOOLEAN;

    BEGIN
        mask := ForwardDirName;
        Strings.Append ("*.FWD", mask);
        found := FirstDirEntry (mask, FALSE, FALSE, D);
        filesize := D.size;
        WHILE found AND NOT ShutdownRequest DO
            filename := ForwardDirName;
            Strings.Append (D.name, filename);
            HideFile (filename, TRUE);
            cid := OpenOldFile (filename, FALSE, FALSE);
            IF cid <> NoSuchChannel THEN
                ReadRaw (cid, preamble, SIZE(preamble), CharsRead);
                IF (CharsRead = SIZE(preamble)) AND Strings.Equal(preamble.version, "V000") THEN
                    NEW (p);
                    p^.file := filename;
                    p^.ID := LogID;
                    WITH preamble DO
                        p^.sendtime := sendtime;
                        p^.RetryNumber := RetryNumber;
                        p^.NotifyOnFailure := ODD(Flags);
                        p^.Recirculate := ODD(Flags DIV 2);
                    END (*WITH*);
                    EVAL( FReadChar(cid, NextChar)
                          AND FReadString (cid, p^.sender, CharSet{'('}, NextChar));
                    p^.domain := "";
                    p^.SMTPport := 25;
                    p^.LocalHost := OurHostName;
                    NEW (p^.sendto);
                    p^.sendto^.local := NIL;
                    p^.sendto^.remote := ReadNameList (cid,
                                             p^.sendto^.RemoteCount, NextChar);

                    (* A <CRLF> after the namelist is optional. *)

                    IF NextChar = CR THEN
                        EVAL (FReadChar (cid, NextChar));
                        IF NextChar = LF THEN
                            EVAL (FReadChar (cid, NextChar));
                        END (*IF*);
                    END (*IF*);

                    p^.offset := CurrentPosition (cid);
                    p^.size := filesize;
                    CloseFile (cid);

                    Now := time();
                    IF p^.sendto = NIL THEN
                        DEALLOCATE(p, SIZE(OutJob));
                    ELSE
                        LogTransaction (LogID, p^.file);
                        IF p^.sendtime <= Now THEN
                            p^.sendtime := Now;
                            AddToMailSack (p);
                        ELSE
                            AddToRetryList (p);
                        END (*IF*);
                    END (*IF*);

                ELSE
                    CloseFile (cid);
                END (*IF*);
            END (*IF file opened*);

            WITH JobCount DO
                Obtain (access);
                IF count < limit THEN
                    found := NextDirEntry(D);
                    filesize := D.size;
                ELSE
                    found := FALSE;
                END (*IF*);
                Release (access);
            END (*WITH*);
        END (*WHILE*);
        DirSearchDone (D);

    END CheckUnsentMail;

(************************************************************************)
(*                      THE ONLINE/OFFLINE CHECKER                      *)
(************************************************************************)

PROCEDURE OnlineChecker;

    (* Runs forever, periodically checking whether we are online or     *)
    (* offline.  While online, we enable the sending of outbound mail.  *)

    CONST InitialDelay = 4*1000;              (* four seconds  *)
          DefaultCheckInterval = 15*1000;     (* 15 seconds    *)

    VAR TimedOut, WeWereOffline: BOOLEAN;
        CheckInterval, loopcount, pos, DisplayAddr: CARDINAL;
        txtbuf: ARRAY [0..16] OF CHAR;
        LogID: TransactionLogID;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        loopcount := 0;
        LogID := CreateLogID (WCtx, "Online ");
        CheckInterval := InitialDelay;
        LOOP
            WeWereOffline := WeAreOffline;
            TimedWait (CheckIfOnline, CheckInterval, TimedOut);
            IF ShutdownRequest THEN
                WeAreOffline := TRUE;
                EXIT (*LOOP*);
            END (*IF*);

            CASE OnlineOption OF
                |  0:  WeAreOffline := NOT FileSys.Exists ('ONLINE');
                |  1:  WeAreOffline := NOT RefreshOurIPAddresses();
                |  2:  WeAreOffline := FALSE;
            END (*CASE*);

            IF WeAreOffline <> WeWereOffline THEN
                RecomputeLocalDomainNames (DisplayAddr, ExtraLogging);
                IF NOT UseFixedLocalName THEN
                    AddressToHostName (DisplayAddr, OurHostName);
                END (*IF*);
                IF WeAreOffline THEN

                    (* We've just gone off-line. *)

                    LogTransactionL (LogID, "Going off-line");
                    IF ScreenEnabled THEN
                        UpdateTopScreenLine (60, "Offline          ");
                    END (*IF*);
                ELSE

                    (* We've just come on-line. *)

                    LogTransactionL (LogID, "Going on-line");
                    IF ScreenEnabled THEN
                        IPToString (DisplayAddr, TRUE, txtbuf);
                        UpdateTopScreenLine (60, txtbuf);
                    END (*IF*);
                    Strings.Assign ("We are using ", message);
                    pos := 13;
                    ConvertCardZ (NumberOfDaemons, message, 3, pos);
                    message[pos] := Nul;
                    Strings.Append (" SMTP output threads.", message);
                    LogTransaction (LogID, message);
                    WITH OfflineData DO
                        Obtain (access);
                        WHILE PendingCount > 0 DO
                            Signal (SomethingToSend);
                            DEC (PendingCount);
                        END (*WHILE*);
                        Release (access);
                    END (*WITH*);
                    CheckUnsentMail (LogID);
                END (*IF*);

            ELSIF NOT WeAreOffline THEN

                (* While we're on-line, check the forward directory for *)
                (* new additions every so often.                        *)

                IF ForceForwardDirCheck OR (loopcount >= 20) THEN
                    loopcount := 0;
                    WITH JobCount DO
                        Obtain (access);
                        ForceForwardDirCheck := count < limit;
                        Release (access);
                    END (*WITH*);
                    IF ForceForwardDirCheck THEN
                        CheckUnsentMail (LogID);
                    END (*IF*);
                    ForceForwardDirCheck := FALSE;
                ELSE
                    INC (loopcount);
                END (*IF*);

            END (*IF*);

            CheckInterval := DefaultCheckInterval;

        END (*LOOP*);

        Signal (TaskDone);

    END OnlineChecker;

(********************************************************************************)

PROCEDURE StartOnlineChecker;

    (* Allows the OnlineChecker task to start working. *)

    BEGIN
        IF CreateTask (OnlineChecker, 2, "online check") THEN
            INC (TaskCount);
        END (*IF*);
    END StartOnlineChecker;

(********************************************************************************)
(*                  TASK TO RESPOND TO AN EXTERNAL EVENT FLAG                   *)
(********************************************************************************)

PROCEDURE CheckForwardMailFlag;

    (* Runs as a separate task.  Forces a re-check of the online status and     *)
    (* the forward directory each time a public event semaphore tells us that   *)
    (* there's been a change.                                                   *)

    CONST semName = "\SEM32\WEASEL\FORWARDMAIL";

    VAR count: CARDINAL;

    BEGIN
        ForceOnlineCheck := 0;
        IF OS2.DosOpenEventSem (semName, ForceOnlineCheck) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ForceOnlineCheck, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownRequest DO
            OS2.DosWaitEventSem (ForceOnlineCheck, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (ForceOnlineCheck, count);
            RequestForwardDirectoryCheck;
        END (*WHILE*);

        OS2.DosCloseEventSem(ForceOnlineCheck);
        Signal (TaskDone);

    END CheckForwardMailFlag;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE UnhidePendingFiles;

    (* Ensures that *.FWD files in the "forward" directory are not      *)
    (* hidden.  We need this procedure only at server startup, in case  *)
    (* there were hidden files left from an improper shutdown of        *)
    (* Weasel.  Note: ForwardDirName must be given a value before this  *)
    (* procedure is called.                                             *)

    VAR mask, filename: FilenameString;
        D: DirectoryEntry;
        found: BOOLEAN;

    BEGIN
        mask := ForwardDirName;
        Strings.Append ("*.FWD", mask);
        found := FirstDirEntry (mask, FALSE, TRUE, D);
        WHILE found DO
            filename := ForwardDirName;
            Strings.Append (D.name, filename);
            HideFile (filename, FALSE);
            found := NextDirEntry (D);
        END (*WHILE*);
        DirSearchDone (D);

    END UnhidePendingFiles;

(************************************************************************)

(*
PROCEDURE EnableExtraLogging (enable: BOOLEAN);

    (* Enables the option of putting extra detail into the log file.    *)
    (* This function is obsolescent; ExtraLogging can be enabled from   *)
    (* the Setup notebook, so there's no point in having the extra      *)
    (* 'X' command-line parameter.                                      *)

    BEGIN
        ForceExtraLogging := enable;
        IF enable AND NOT ExtraLogging THEN
            ExtraLogging := ExtraLogging OR ForceExtraLogging;
            EnableDomainExtraLogging (ExtraLogging);
        END (*IF*);
        (*DisplayCounts := enable AND ScreenEnabled;*)
        DisplayCounts := FALSE;
    END EnableExtraLogging;
*)

(************************************************************************)

PROCEDURE ProcessForwardRelayHostNames;

    (* Separates the "port" information from ForwardRelayHost and AuthPOPhost. *)

    (********************************************************************)

    PROCEDURE Split (VAR (*INOUT*) name: HostName;
                                       defaultport: CARDINAL): CARDINAL;

        VAR port, pos: CARDINAL;  found: BOOLEAN;

        BEGIN
            WHILE name[0] = ' ' DO
                Strings.Delete (name, 0, 1);
            END (*WHILE*);
            Strings.FindNext (':', name, 0, found, pos);
            IF found THEN
                port := 0;
                name[pos] := Nul;
                INC (pos);
                WHILE name[pos] IN Digits DO
                    port := 10*port + ORD(name[pos]) - ORD('0');
                    INC (pos);
                END (*WHILE*);
            ELSE
                port := defaultport;
            END (*IF*);
            RETURN port;
        END Split;

    (********************************************************************)

    BEGIN
        ForwardRelayPort := Split (ForwardRelayHost, 25);
        AuthPOPport := Split (AuthPOPhost, 110);
    END ProcessForwardRelayHostNames;

(************************************************************************)

PROCEDURE ReloadDeliveryINIData (FirstTime: BOOLEAN;
                                    AddressToBindTo: CARDINAL): BOOLEAN;

    (* Loads that part of the INI data, for the Delivery module, that   *)
    (* can be updated "on the run" without restarting the server.       *)
    (* Returns the value of ExtraLogging.                               *)

    VAR hini: INIData.HINI;
        TransLevel, RetryHours, minutes: CARDINAL;
        temp16: CARD16;
        TransLogName: FilenameString;
        SyslogHost: HostName;
        key: ARRAY [0..20] OF CHAR;
        SYSapp: ARRAY [0..4] OF CHAR;

    BEGIN
        SyslogHost := "";
        TransLogName := "WEASEL.LOG";
        TransLevel := 2;
        RetryHours := 4*24;
        ForwardRelayOption := 0;
        BindAddr := AddressToBindTo;
        Obtain (LogFileLock);
        key := "weasel.ini";
        hini := OpenINIFile (key, UseTNI);
        SYSapp := "$SYS";
        IF INIValid(hini) THEN
            key := "SyslogHost";
            EVAL (INIGetString (hini, SYSapp, key, SyslogHost));
            key := "LogOutgoing";
            IF NOT INIGet (hini, SYSapp, key, LogOutgoing) THEN
                LogOutgoing := FALSE;
            END (*IF*);
            key := "OutgoingLogFile";
            IF NOT INIGetString (hini, SYSapp, key, LogFileName) THEN
                LogFileName := "SMTPOUT.LOG";
            END (*IF*);
            key := "TransLevel";
            IF NOT INIGet (hini, SYSapp, key, TransLevel) THEN
                TransLevel := 2;
            END (*IF*);
            key := "RetryHours";
            IF NOT INIGet (hini, SYSapp, key, RetryHours) THEN
                RetryHours := 4*24;     (* 4 days *)
            END (*IF*);
            key := "TransLogName";
            EVAL (INIGetString (hini, SYSapp, key, TransLogName));
            key := "MaxRecipientsPerItem";
            EVAL (INIGet (hini, SYSapp, key, MaxRecipientsPerLetter));
            key := "UseFixedLocalName";
            IF NOT INIGet (hini, SYSapp, key, UseFixedLocalName) THEN
                UseFixedLocalName := FALSE;
            END (*IF*);
            IF UseFixedLocalName THEN
                key := "OurHostName";
                EVAL (INIGetString (hini, SYSapp, key, OurHostName));
            END (*IF*);
            key := "BounceBytes";
            IF NOT INIGet (hini, SYSapp, key, BounceBytes) THEN
                BounceBytes := 0;
            END (*IF*);
            key := "RelayOption";
            IF NOT INIGet (hini, SYSapp, key, ForwardRelayOption) THEN
                ForwardRelayOption := 0;
            END (*IF*);
            key := "AuthOption";
            IF NOT INIGet (hini, SYSapp, key, AuthOption) THEN
                AuthOption := 0;
            END (*IF*);
            key := "AuthUser";
            IF NOT INIGetString (hini, SYSapp, key, AuthUser) THEN
                AuthUser := '';
            END (*IF*);
            key := "AuthPass";
            IF NOT INIGetString (hini, SYSapp, key, AuthPass) THEN
                AuthPass := '';
            END (*IF*);
            key := "ForwardRelay";
            IF NOT INIGetString (hini, SYSapp, key,
                                       ForwardRelayHost) THEN
                ForwardRelayHost := "";
            END (*IF*);
            key := "AuthPOPhost";
            IF NOT INIGetString (hini, SYSapp, key,
                                             AuthPOPhost) THEN
                AuthPOPhost := "";
            END (*IF*);
            IF (AuthUser[0] = Nul) OR
                        ((AuthOption = 2) AND (AuthPOPhost[0] = Nul)) THEN
                AuthOption := 0;
            END (*IF*);
            key := "OutputThreads";
            IF INIGet (hini, SYSapp, key, temp16) THEN
                IF temp16 = 0 THEN temp16 := 1
                ELSIF temp16 > MaxNumberOfDaemons THEN temp16 := 64;
                END (*IF*);
                DesiredNumberOfDaemons := temp16;
            ELSE
                DesiredNumberOfDaemons := DefaultNumberOfDaemons;
            END (*IF*);
            key := "OnlineOption";
            IF NOT INIGet (hini, SYSapp, key, OnlineOption) THEN
                OnlineOption := 2;
            END (*IF*);
        END (*IF*);
        CloseINIFile (hini);
        Release (LogFileLock);
        ExtraLogging := TransLevel > 15;
        EnableDomainExtraLogging (ExtraLogging);
        TransLevel := TransLevel MOD 16;
        IF NOT NotDetached() THEN
            TransLevel := IAND(TransLevel, 5);
        END (*IF*);

        ProcessForwardRelayHostNames;
        IF ForwardRelayHost[0] = Nul THEN
            ForwardRelayOption := 0;
        END (*IF*);

        (* From RetryHours compute WarnRetries and MaxRetries. *)

        WarnRetries := 0;  minutes := 0;
        REPEAT
            IF WarnRetries <= MaxVarRetries THEN
                INC (minutes, RetryInterval[WarnRetries]);
            ELSE
                INC (minutes, RetryInterval[MaxVarRetries]);
            END (*IF*);
            INC (WarnRetries);
        UNTIL minutes >= 15*RetryHours;
        MaxRetries := WarnRetries;
        REPEAT
            IF MaxRetries <= MaxVarRetries THEN
                INC (minutes, RetryInterval[WarnRetries]);
            ELSE
                INC (minutes, RetryInterval[MaxVarRetries]);
            END (*IF*);
            INC (MaxRetries);
        UNTIL minutes >= 60*RetryHours;

        SetProcname ("Weasel", 2);
        IF SyslogHost[0] <> Nul THEN
            SetSyslogHost (SyslogHost);
        END (*IF*);
        SetPrincipalIPAddress (BindAddr);
        StartTransactionLogging (WCtx, TransLogName, TransLevel);
        RefreshMasterDomainList (FirstTime);
        RefreshHostLists (FirstTime, UseTNI);

        (* Now that we know how many Send_NN tasks to run, wake up      *)
        (* one that is already running, so that it can decide whether   *)
        (* to exit or to start more tasks.                              *)

        Signal (SomethingToSend);

        RETURN ExtraLogging;

    END ReloadDeliveryINIData;

(************************************************************************)

PROCEDURE LoadDeliveryINIData (TNImode: BOOLEAN);

    (* Loads the values of this module's global variables that reside   *)
    (* in the main INI file.  Also starts the transaction logging and   *)
    (* lets this module's internal tasks start.                         *)

    CONST DefaultPort = 25;
          Alphanumeric = CharSet {'0'..'9', 'A'..'Z'};

    VAR hini: INIData.HINI;
        j: [0..7];
        ServerPort: CardArray;
        ServerPort2: CardArray2;  ServerPort3: CardArray3;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..12] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        OnlineOption := 0;
        key := "weasel.ini";
        hini := OpenINIFile (key, UseTNI);
        SYSapp := "$SYS";
        IF INIValid(hini) THEN
            key := "ServerPort";
            IF INIGet (hini, SYSapp, key, ServerPort) THEN
                OurSMTPPort := ServerPort[SMTP];
            ELSE
                IF INIGet (hini, SYSapp, key, ServerPort3) THEN
                    OurSMTPPort := ServerPort3[SMTP];
                ELSIF INIGet (hini, SYSapp, key, ServerPort2) THEN
                    OurSMTPPort := ServerPort2[SMTP];
                ELSE
                    OurSMTPPort := DefaultPort;
                END (*IF*);
            END (*IF*);
            key := "MailRoot";
            IF NOT INIGetString (hini, SYSapp, key,
                                       ForwardDirName) THEN
                ForwardDirName := "\MPTN\ETC\MAIL\";
            END (*IF*);
            Strings.Append ("Forward\", ForwardDirName);
            UnhidePendingFiles;
            key := "VName";
            IF INIGet (hini, SYSapp, key, NextName) THEN

                (* Ensure that NextName has the right format. *)

                FOR j := 0 TO 7 DO
                    IF NOT (NextName[j] IN Alphanumeric) THEN
                        NextName[j] := '0';
                    END (*IF*);
                END (*FOR*);
            ELSE
                NextName := "00000000";
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);

        ExtraLogging := ReloadDeliveryINIData (TRUE, 0);
        Signal (SystemUp);
    END LoadDeliveryINIData;

(************************************************************************)

VAR hini: INIData.HINI;  j: CARDINAL;
    SYSapp: ARRAY [0..4] OF CHAR;
    name: ARRAY [0..10] OF CHAR;

BEGIN
    UseTNI := FALSE;
    UseFixedLocalName := FALSE;
    MaxRecipientsPerLetter := 100;
    MaxRetries := 26;  WarnRetries := 9;
    BindAddr := 0;
    OurHostName := "localhost";
    ExtraLogging := FALSE;
    LogFileName := "SMTPOUT.LOG";
    DisplayCounts := FALSE;
    CreateLock (LogFileLock);
    LogOutgoing := FALSE;
    ScreenEnabled := NotDetached();
    ShutdownRequest := FALSE;  TaskCount := 0;
    IF ScreenEnabled THEN
        ClearScreen;  SetBoundary (2, 30);
        UpdateTopScreenLine (60, "Offline          ");
    END (*IF*);
    newmailhev := 0;
    IF OS2.DosOpenEventSem (semName, newmailhev) = OS2.ERROR_SEM_NOT_FOUND THEN
        OS2.DosCreateEventSem (semName, newmailhev, OS2.DC_SEM_SHARED, FALSE);
    END (*IF*);
    CreateLock (NextNameLock);
    WeAreOffline := TRUE;
    WITH OfflineData DO
        CreateLock (access);
        PendingCount := 0;
    END (*WITH*);
    WITH MailSack DO
        CreateLock (access);
        CreateSemaphore (count, 0);
        head := NIL;
        tail := NIL;
    END (*WITH*);
    WITH OutboundMail DO
        CreateLock (access);
        head := NIL;
        tail := NIL;
        WITH RecipientCount DO
            count := 0;  limit := OutboundRecipientLimit;
        END (*WITH*);
    END (*WITH*);
    WITH JobCount DO
        CreateLock (access);
        count := 0;
        limit := JobCountLimitHigh;
    END (*WITH*);
    CreateSemaphore (CheckIfOnline, 0);
    CreateSemaphore (SomethingToSend, 0);
    CreateSemaphore (TaskDone, 0);
    WITH RetryList DO
        head := NIL;
        CreateLock (access);
        WITH RecipientCount DO
            count := 0;  limit := RetryRecipientLimit;
        END (*WITH*);
    END (*WITH*);
    CreateSemaphore (SystemUp, 0);
    CreateSemaphore (Retry, 0);
    IF CreateTask (RetryTask, 3, "retry send") THEN
        INC (TaskCount);
    END (*IF*);
    IF CreateTask (Sorter, 3, "mail sorter") THEN
        INC (TaskCount);
    END (*IF*);

    (* New approach: create only one outbound task initially.   *)
    (* Let that one create others.  However, those others       *)
    (* will not be started until the INI data are loaded.       *)

    NumberOfDaemons := 0;
    CreateLock (NumberOfDaemonsLock);
    FOR j := 1 TO MaxNumberOfDaemons DO
        DaemonActive[j] := FALSE;
    END (*FOR*);
    DesiredNumberOfDaemons := 1;
    j := 1;
    DaemonActive[1] :=
        CreateTask1 (MailerTask, 4, "MailerTask", CAST(ADDRESS,j));

    ForceForwardDirCheck := FALSE;
    IF CreateTask (CheckForwardMailFlag, 2, "update") THEN
        INC (TaskCount);
    END (*IF*);
FINALLY
    ShutdownRequest := TRUE;  Signal(Retry);  Signal(CheckIfOnline);
    Signal (MailSack.count);
    Signal(SomethingToSend);   (* One extra for good luck *)
    OS2.DosPostEventSem(ForceOnlineCheck);
    OS2.DosResetEventSem(ForceOnlineCheck,j);
    REPEAT
        Signal(SomethingToSend);
        Wait (TaskDone);  DEC(TaskCount);
    UNTIL TaskCount = 0;
    UnhidePendingFiles;
    name := "weasel.ini";
    hini := OpenINIFile (name, UseTNI);
    IF INIValid(hini) THEN
        Obtain (NextNameLock);
        SYSapp := "$SYS";
        name := "VName";
        INIPut (hini, SYSapp, name, NextName);
        Release (NextNameLock);
        CloseINIFile (hini);
    END (*IF*);
    OS2.DosCloseEventSem(newmailhev);
END Delivery.

