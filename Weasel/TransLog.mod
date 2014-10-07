(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE TransLog;

        (********************************************************)
        (*                                                      *)
        (*               Transaction logging                    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 March 1999                   *)
        (*  Last edited:        25 May 2014                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*     Now working on the concept of having different   *)
        (*     log contexts, the context being defined by the   *)
        (*     log file name.                                   *)
        (*                                                      *)
        (*     Policy:                                          *)
        (*      All contexts share the same syslog connection.  *)
        (*      All contexts share the same pipe.               *)
        (*      All contexts share the same screen.             *)
        (*      Different contexts MAY have different disk      *)
        (*         files, but may also share them.              *)
        (*      A FileShareGroup is a set of contexts sharing   *)
        (*         the same disk file.                          *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  ADDRESS, CARD16,
    (* proc *)  ADR;

IMPORT Strings, OS2;

FROM MyClock IMPORT
    (* proc *)  CurrentTimeToString, AppendSyslogDateTimeString;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenAtEnd, FWriteString, FWriteLn, CloseFile;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, AddressFamily, SocketType, SockAddr,
    (* proc *)  socket, connect, getsockname, soclose, gethostid, send;

FROM Internet IMPORT
    (* const*)  Zero8,
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* type *)  HostEntPtr, AddressPointerArrayPointer,
    (* proc *)  gethostbyname;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, Swap4, AddressToHostName, ConvertCard, StringMatch,
                NameIsNumeric;

FROM SplitScreen IMPORT
    (* proc *)  LockScreen, UnlockScreen, WriteStringAt,
                WriteString, WriteLn, WriteChar, NotDetached;

FROM Names IMPORT
    (* type *)  FilenameString, HostName;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Copy;

FROM Timer IMPORT
    (* proc *)  Sleep, TimedWait;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                CreateTask, CreateTask1;

(************************************************************************)
(*                      GLOBAL TYPES AND VARIABLES                      *)
(************************************************************************)

TYPE
    TargetType = (todisk, toscreen, topipe, tosyslog);

(************************************************************************)
(*                    TYPES RELEVANT TO A LOG CONTEXT                   *)
(************************************************************************)

TYPE
    (* A ContextList is a linear list of contexts.  *)

    ContextList = POINTER TO RECORD
                      next: ContextList;
                      this: LogContext;
                  END (*RECORD*);

    FileShareGroup = POINTER TO ContextSet;

    ListOfGroups = POINTER TO RECORD
                       next: ListOfGroups;
                       this: FileShareGroup;
                   END (*RECORD*);

    (* A ContextSet is a record keeping track of all contexts that *)
    (* share a disk log file.                                      *)

    ContextSet = RECORD
                     GroupLock: Lock;
                     ctxlist: ContextList;
                     update: Semaphore;
                     closing: BOOLEAN;
                     InterimDiskFileOpen: BOOLEAN;
                     UpdateTaskRunning: BOOLEAN;

                     (* Transaction log file on disk. *)

                     TransactionLogChannel: ChanId;
                     TransactionLogName, InterimLogName: FilenameString;
                 END (*RECORD*);

    (* A LogContext is the support for a collection of log IDs. *)

    FlagArray = ARRAY TargetType OF BOOLEAN;

    LogContext = POINTER TO
                     RECORD
                         group: FileShareGroup;
                         level: CARDINAL;
                         Target: FlagArray;
                     END (*RECORD*);

    (* A TransactionLogID is a context plus a text label. *)

    TransactionLogID = POINTER TO
                           RECORD
                               context: LogContext;
                               prefix: ARRAY [0..127] OF CHAR;
                           END (*RECORD*);

(************************************************************************)

CONST
    Nul = CHR(0);
    PreambleSize = 29;      (* typical, but give this a re-think. *)
    MaxLineLength = 256+PreambleSize;
    BlankLine = "                                                                                ";

TYPE
    LogLinePtr = POINTER TO ARRAY [0..MaxLineLength-1] OF CHAR;

VAR
    (* Log ID for internal admin messages. *)

    DefaultCtx: LogContext;
    DummyID: TransactionLogID;

    (* Critical section protection for the transaction log. *)

    GlobalLock: Lock;

    (* Linear list of all currently existing groups. *)

    AllGroups: ListOfGroups;

    (* Critical section protection for the AllGroups list. *)

    AllGroupsLock: Lock;

    (* A copy of the text that should be on the top line of the screen. *)

    TopLine: ARRAY [0..79] OF CHAR;

    (* Name of the pipe for the case where we are logging to a pipe. *)

    PipeName: FilenameString;

    (* Socket for syslog logging. *)

    LogSocket: Socket;

    (* IP address (network byte order) of syslog host. *)

    syslogaddress: CARDINAL;

    (* Critical section protection for LogSocket. *)

    LogSocketLock: Lock;

    (* Number of contexts currently using pipe and syslog. *)

    PipeUsers, SyslogUsers: CARDINAL;

    (* A tag (process name) for syslog messages. *)

    procname: ARRAY [0..31] OF CHAR;

    (* Host name for syslog messages. *)

    OurHostname: HostName;

    (* Facility code for syslog messages. *)

    Facility: CARDINAL;

    (* Syslog currently in use. *)

    SyslogActive: BOOLEAN;

    (* Flag to say that this process has access to the screen. *)

    ScreenAvailable: BOOLEAN;

    (* Flag to say that shutdown processing has commenced. *)

    ShuttingDown: BOOLEAN;

(********************************************************************************)
(*                               PIPE OPERATIONS                                *)
(********************************************************************************)

CONST
    PipeSize = 8192;
    PipeBufferSize = 32;

TYPE
    BufferIndex = [0..PipeBufferSize-1];
    StrDesc = RECORD
                  addr: LogLinePtr;
                  length: CARDINAL;
              END (*RECORD*);

    PipeState = RECORD
                    access: Lock;
                    handle: OS2.HPIPE;
                    InIndex, OutIndex: BufferIndex;
                    stuck: CARDINAL;
                    PipeTaskRunning: BOOLEAN;
                    open: BOOLEAN;
                    count: Semaphore;
                    Buffer: ARRAY BufferIndex OF StrDesc;
                END (*RECORD*);

VAR
    Pipe: PipeState;
    PipeInUse: BOOLEAN;
    PipeOpenRequest: Semaphore;

    CRLF: ARRAY [0..1] OF CHAR;

(********************************************************************************)

PROCEDURE PipeTask;

    (* Feeds data from the pipe circular buffer to the pipe itself. *)

    VAR rc, actual: CARDINAL;
        success: BOOLEAN;
        towrite: StrDesc;

    BEGIN
        REPEAT
            Wait (PipeOpenRequest);

            (* Open the pipe.  We don't want critical section protection here   *)
            (* because the DosConnectNPipe request below could block all        *)
            (* logging if we still held Pipe.access; and anyway the protection  *)
            (* is not needed because nobody else will try to use the pipe       *)
            (* until we set Pipe.open = TRUE.                                   *)

            rc := OS2.DosCreateNPipe (PipeName, Pipe.handle, OS2.NP_ACCESS_OUTBOUND,
                                      (*OS2.NP_NOWAIT*) + 1, PipeSize, 0, 0);
            success := rc = 0;
            IF success THEN
                rc := OS2.DosConnectNPipe (Pipe.handle);
                success := (rc = 0) OR (rc = 233);
            END (*IF*);
            Pipe.open := success;

            (* Transfer data while we want the pipe open, or until the pipe is  *)
            (* closed at the client end.  If the pipe is closed at the other    *)
            (* end, we'll close it and then attempt to reopen it, in case the   *)
            (* client program is restarted.                                     *)

            rc := 0;
            WHILE PipeInUse AND (rc = 0) DO

                WITH Pipe DO
                    Wait (count);
                    Obtain (access);
                    towrite := Buffer[OutIndex];
                    IF towrite.addr <> NIL THEN
                        WITH Buffer[OutIndex] DO
                            addr := NIL;
                            length := 0;
                        END (*WITH*);
                        OutIndex := (OutIndex + 1) MOD PipeBufferSize;
                    END (*IF*);
                    WHILE stuck > 0 DO
                        Signal (count);  DEC(stuck);
                    END (*WHILE*);
                    Release (access);
                END (*WITH*);
                rc := 0;
                WITH towrite DO
                    IF addr <> NIL THEN
                        IF length > 1 THEN
                            rc := OS2.DosWrite (Pipe.handle, addr, length-1, actual);
                        END (*IF*);
                        IF rc = 0 THEN
                            rc := OS2.DosWrite (Pipe.handle, ADR(CRLF), 2, actual);
                        END (*IF*);
                        DEALLOCATE (addr, length);
                        IF rc = OS2.ERROR_BROKEN_PIPE THEN
                            Signal (PipeOpenRequest);
                        END (*IF*);
                    END (*IF*);
                END (*WITH*);

            END (*WHILE*);

            (* Close the pipe when PipeInUse becomes FALSE *)
            (* or when a write fails.                      *)

            Obtain (Pipe.access);
            Pipe.open := FALSE;
            OS2.DosDisConnectNPipe (Pipe.handle);
            CloseFile (Pipe.handle);
            Release (Pipe.access);

        UNTIL ShuttingDown;
        Pipe.PipeTaskRunning := FALSE;

    END PipeTask;

(********************************************************************************)

PROCEDURE InitialisePipe;

    (* Creates the data structure associated with the pipe. *)

    VAR j: BufferIndex;

    BEGIN
        CreateLock (Pipe.access);
        FOR j := 0 TO MAX(BufferIndex) DO
            Pipe.Buffer[j].addr := NIL;
            Pipe.Buffer[j].length := 0;
        END (*FOR*);
        Pipe.InIndex := 0;
        Pipe.OutIndex := 0;
        Pipe.stuck := 0;
        Pipe.PipeTaskRunning := FALSE;
        Pipe.open := FALSE;
        PipeInUse := FALSE;
        CreateSemaphore (Pipe.count, 0);
        CreateSemaphore (PipeOpenRequest, 0);
    END InitialisePipe;

(********************************************************************************)

PROCEDURE CopyToPipe (ptext: LogLinePtr);

    (* Adds one entry to the queue of items waiting to be sent to the pipe. *)

    VAR overflow: BOOLEAN;

    BEGIN
        WITH Pipe DO
            IF NOT PipeTaskRunning THEN
                PipeTaskRunning := CreateTask (PipeTask, 5, "pipe output");
            END (*IF*);
            Obtain (access);
            IF open THEN
                WITH Buffer[InIndex] DO
                    overflow := addr <> NIL;
                    IF overflow THEN
                        INC (stuck);
                    ELSE
                        length := Strings.Length(ptext^) + 1;
                        IF length > MaxLineLength THEN
                            length := MaxLineLength;
                        END (*IF*);
                        ALLOCATE (addr, length);
                        IF length > 1 THEN
                            Copy (ptext, addr, length-1);
                        END (*IF*);
                        addr^[length-1] := Nul;
                        InIndex := (InIndex + 1) MOD PipeBufferSize;
                        Signal (count);
                    END (*IF*);
                END (*WITH*);
            END (*IF*);
            Release (access);
        END (*WITH*);
    END CopyToPipe;

(************************************************************************)
(*                      SYSLOG SOCKET OPERATIONS                        *)
(************************************************************************)

PROCEDURE GetOurHostName (S: Socket);

    (* Sets OurHostname to what we are currently calling our host name  *)
    (* for our end of the connection using socket S.  If we can't get a *)
    (* reasonable answer then we use the IP address.  If we do get a    *)
    (* textual hostname then we discard the part after the first '.'.   *)

    VAR myaddr: SockAddr;  size: CARDINAL;  found: BOOLEAN;

    BEGIN
        size := SIZE(myaddr);
        IF NOT getsockname (S, myaddr, size) THEN
            AddressToHostName (myaddr.in_addr.addr, OurHostname);
        END (*IF*);
        IF OurHostname[0] <> '[' THEN
            Strings.FindNext ('.', OurHostname, 0, found, size);
            IF found THEN
                OurHostname[size] := Nul;
            END (*IF*);
        END (*IF*);
    END GetOurHostName;

(************************************************************************)

PROCEDURE StartSyslogging;

    (* Starts logging to syslog, by opening and connecting the necessary   *)
    (* socket.  If this operation fails, we clear Target[tosyslog].  Even  *)
    (* though this is a UDP connection, the connect() saves us the trouble *)
    (* of having to specify an address for each transfer.                  *)

    CONST loopback = 256*256*256*127 + 1;

    VAR addr: SockAddr;

    BEGIN
        Obtain (LogSocketLock);
        LogSocket := socket (AF_INET, SOCK_DGRAM, AF_UNSPEC);

        (* Connect to the syslog service on the specified machine. *)
        (* Port 514 is as specified in RFC3164.                    *)

        WITH addr DO
            family := AF_INET;
            WITH in_addr DO
                port := Swap2 (514);
                addr := syslogaddress;
                zero := Zero8;
            END (*WITH*);
        END (*WITH*);

        EVAL((LogSocket <> NotASocket)
                            AND NOT connect (LogSocket, addr, SIZE(addr)));
        GetOurHostName (LogSocket);
        Release (LogSocketLock);

    END StartSyslogging;

(************************************************************************)

PROCEDURE StopSyslogging;

    (* Closes logging to syslog. *)

    BEGIN
        Obtain (LogSocketLock);
        soclose (LogSocket);
        LogSocket := NotASocket;
        Release (LogSocketLock);
    END StopSyslogging;

(************************************************************************)

PROCEDURE SetSyslogHost (hostname: ARRAY OF CHAR);

    (* Sets syslogaddress to the IP address, in network byte order, of  *)
    (* the specified host.  If the lookup fails, leaves syslogaddress   *)
    (* unchanged.                                                       *)

    VAR HostInfo: HostEntPtr;
        p: AddressPointerArrayPointer;

    BEGIN
        IF NameIsNumeric(hostname) THEN
            syslogaddress := inet_addr(hostname);
        ELSE
            HostInfo := gethostbyname (hostname);
            IF HostInfo <> NIL THEN
                p := HostInfo^.h_addr_list;
                IF p <> NIL THEN
                    syslogaddress := p^[0]^;
                END (*IF*);
            END (*IF*);
        END (*IF*);
        IF SyslogActive THEN
            StopSyslogging;
            StartSyslogging;
        END (*IF*);
    END SetSyslogHost;

(************************************************************************)

PROCEDURE CopyToSyslog (ptext: LogLinePtr);

    (* Sends this line to syslog, with an appropriate header. *)

    VAR message: ARRAY [0..1023] OF CHAR;
        pos: CARDINAL;

    BEGIN
        message := "<";  pos := 1;
        ConvertCard (8*Facility + 6, message, pos);
        message[pos] := '>';  INC(pos);
        message[pos] := Nul;
        AppendSyslogDateTimeString (message);
        Strings.Append (' ', message);
        Strings.Append (OurHostname, message);
        Strings.Append (' ', message);
        Strings.Append (procname, message);
        Strings.Append (": ", message);
        Strings.Append (ptext^, message);
        Obtain (LogSocketLock);
        send (LogSocket, message, LENGTH(message), 0);
        Release (LogSocketLock);

    END CopyToSyslog;

(************************************************************************)
(*                       ADDING A NEW LOG ENTRY                         *)
(************************************************************************)

PROCEDURE WriteTopTwoScreenLines;

    (* Refreshes the top two lines of the screen, provided that the     *)
    (* screen is available to us.                                       *)

    BEGIN
        IF ScreenAvailable THEN
            LockScreen;
            WriteStringAt (0, 0, TopLine);
            WriteStringAt (1, 0, BlankLine);
            UnlockScreen;
        END (*IF*);
    END WriteTopTwoScreenLines;

(************************************************************************)

PROCEDURE AddToTransactionLog (ctx: LogContext;  ptext: LogLinePtr);

    (* Writes the text, preceded by the date and time, to the screen    *)
    (* and/or the transaction log and/or the pipe, depending on Target. *)

    VAR pLogLine: LogLinePtr;  G: FileShareGroup;

        (* For debugging: *)

        testchar: ARRAY [0..0] OF CHAR;
        count: CARD16;

    BEGIN
        IF ctx^.level > 0 THEN

            (* Create a string containing date/time and the text.  *)

            NEW (pLogLine);
            CurrentTimeToString (pLogLine^);
            Strings.Append (" ", pLogLine^);
            Strings.Append (ptext^, pLogLine^);

            (* Write to disk if Target[todisk) is TRUE. *)

            IF ctx^.Target[todisk] THEN
                G := ctx^.group;
                Obtain (G^.GroupLock);
                IF NOT G^.InterimDiskFileOpen THEN
                    G^.TransactionLogChannel := OpenAtEnd (G^.InterimLogName);
                    G^.InterimDiskFileOpen := TRUE;
                END (*IF*);
                FWriteString (G^.TransactionLogChannel, pLogLine^);
                FWriteLn (G^.TransactionLogChannel);
                Release (G^.GroupLock);
            END (*IF*);

            (* Write to the screen if Target[toscreen] is TRUE. *)

            IF ctx^.Target[toscreen] THEN
                LockScreen;
                WriteString (pLogLine^);  WriteLn;
                UnlockScreen;
            END (*IF*);

            (* Do we also want to write to the pipe? *)

            IF ctx^.Target[topipe] THEN
                CopyToPipe (pLogLine);
            END (*IF*);

            (* Write to the syslog socket if that option is active. *)

            IF ctx^.Target[tosyslog] THEN
                CopyToSyslog (pLogLine);
            END (*IF*);

            DISPOSE (pLogLine);

        END (*IF*);

        (* Repair the screen display if row 1 has been overwritten. *)

        IF ScreenAvailable THEN
            count := 1;
            OS2.VioReadCharStr (testchar, count, 1, 0, 0);
            IF testchar[0] <> ' ' THEN
                WriteTopTwoScreenLines;
            END (*IF*);
        END (*IF*);

    END AddToTransactionLog;

(************************************************************************)

PROCEDURE LogTransaction (id: TransactionLogID;
                          VAR (*IN*) text: ARRAY OF CHAR);

    (* Puts id+text in the transaction log (if logging enabled). *)

    VAR bufptr: LogLinePtr;

    BEGIN
        IF (id <> NIL) AND (id^.context^.level > 0) THEN
            NEW (bufptr);
            Strings.Assign (id^.prefix, bufptr^);
            Strings.Append ("  ", bufptr^);
            Strings.Append (text, bufptr^);
            AddToTransactionLog (id^.context, bufptr);
            DISPOSE (bufptr);
        END (*IF*);
    END LogTransaction;

(************************************************************************)

PROCEDURE LogTransactionL (id: TransactionLogID;
                           text: ARRAY OF CHAR);

    (* Like LogTransaction, but for a literal text string. *)

    BEGIN
        LogTransaction (id, text);
    END LogTransactionL;

(************************************************************************)
(*                     OTHER EXPORTED PROCEDURES                        *)
(************************************************************************)

PROCEDURE UpdateTopScreenLine (pos: CARDINAL; newstring: ARRAY OF CHAR);

    (* Puts newstring at position pos in our record of what should go   *)
    (* into the top screen line.  Rewrites that part of the screen if   *)
    (* we are a non-detached process.                                   *)

    BEGIN
        (* On first entry, rewrite the top two lines completely. *)

        IF TopLine[0] = ' ' THEN
            WriteTopTwoScreenLines;
            TopLine[0] := '.';
        END (*IF*);

        Strings.Replace (newstring, pos, TopLine);
        IF ScreenAvailable THEN
            LockScreen;
            WriteStringAt (0, pos, newstring);
            UnlockScreen;
        END (*IF*);

    END UpdateTopScreenLine;

(************************************************************************)

PROCEDURE SetProcname (name: ARRAY OF CHAR;  facility: CARDINAL);

    (* Sets process name and facility number for use in syslog messages *)
    (* and for making the pipe name if we use a pipe.  Must be called   *)
    (* before StartTransactionLogging if you plan to use syslog and/or  *)
    (* a pipe for the log messages.                                     *)

    BEGIN
        Strings.Assign (name, procname);
        Strings.Assign ("\PIPE\", PipeName);
        Strings.Append (name, PipeName);
        Strings.Append ("TransLog", PipeName);
        Facility := facility;
    END SetProcname;

(************************************************************************)

PROCEDURE DiscardLogID (VAR (*INOUT*) id: TransactionLogID);

    (* Discards a previously created logfile ID. *)

    BEGIN
        DISPOSE (id);
    END DiscardLogID;

(************************************************************************)
(*           COPYING INTERIM DISK FILE TO PERMANENT LOG FILE            *)
(************************************************************************)

PROCEDURE CopyTransactionLogUpdates (G: FileShareGroup);

    (* Appends all the data from the interim transaction log to the final       *)
    (* transaction log, then deletes the interim transaction log.  We assume    *)
    (* that caller has obtained the G^.GroupLock.                               *)

    BEGIN
        IF G^.InterimDiskFileOpen THEN
            CloseFile (G^.TransactionLogChannel);
            G^.InterimDiskFileOpen := FALSE;
            OS2.DosCopy (G^.InterimLogName, G^.TransactionLogName,
                                                        OS2.DCPY_APPEND);
            OS2.DosDelete (G^.InterimLogName);
        END (*IF*);
    END CopyTransactionLogUpdates;

(************************************************************************)

PROCEDURE TransactionLogUpdateTask (param: ADDRESS);

    (* A separate task that updates a transaction log disk file  *)
    (* approximately every minute.                               *)

    VAR G: FileShareGroup;  TimedOut: BOOLEAN;

    BEGIN
        G := param;
        REPEAT
            TimedWait (G^.update, 60000, TimedOut);
            Obtain (G^.GroupLock);
            CopyTransactionLogUpdates (G);
            Release (G^.GroupLock);
        UNTIL G^.closing;

        (* There is a risk of exiting the above loop without having     *)
        (* done the final update, so do an extra check.                 *)

        Obtain (G^.GroupLock);
        CopyTransactionLogUpdates (G);
        Release (G^.GroupLock);

        (* Tell the CloseGroup procedure that we're terminating. *)

        G^.closing := FALSE;

    END TransactionLogUpdateTask;

(************************************************************************)
(*                         OTHER DISK OPERATIONS                        *)
(************************************************************************)

PROCEDURE CreateGroup (name: ARRAY OF CHAR): FileShareGroup;

    (* Creates a new group. *)

    VAR G: FileShareGroup;  p: ListOfGroups;
        posOfPattern: CARDINAL;  patternFound: BOOLEAN;

    BEGIN
        (*
        LockScreen;
        WriteString ("Creating group with log file name ");
        WriteString (name);
        WriteLn;
        UnlockScreen;
        *)
        NEW (G);
        WITH G^ DO
            CreateLock (GroupLock);
            CreateSemaphore (update, 0);
            ctxlist := NIL;
            InterimDiskFileOpen := FALSE;
            closing := FALSE;

            (* Store the file name, and work out an interim file name. *)

            Strings.Capitalize (name);
            Strings.Assign (name, G^.TransactionLogName);
            Strings.FindPrev ('.', name, LENGTH(name) - 1,
                                           patternFound, posOfPattern);
            IF patternFound THEN
                name[posOfPattern] := Nul;
            END (*IF*);
            Strings.Append (".$$$", name);
            Strings.Assign (name, G^.InterimLogName);

            (* Start the disk file updater. *)

            UpdateTaskRunning :=
                        CreateTask1 (TransactionLogUpdateTask,
                                         3, "trlog update", G);

        END (*WITH*);

        (* Add this group to the list of all groups. *)

        Obtain (AllGroupsLock);
        NEW (p);
        p^.this := G;  p^.next := AllGroups;
        AllGroups := p;
        Release (AllGroupsLock);

        RETURN G;

    END CreateGroup;

(************************************************************************)

PROCEDURE CloseGroup (VAR (*INOUT*) G: FileShareGroup);

    (* Closes group G.  On entry, the group must be locked.    *)

    VAR p, q: ContextList;  prev, LG: ListOfGroups;

    BEGIN
        (* Remove this group from the list of all groups. *)

        Obtain (AllGroupsLock);
        prev := NIL;  LG := AllGroups;
        WHILE LG^.this <> G DO
            prev := LG;  LG := LG^.next;
        END (*WHILE*);
        IF prev = NIL THEN
            AllGroups := LG^.next;
        ELSE
            prev^.next := LG^.next;
        END (*IF*);
        DISPOSE (LG);
        Release (AllGroupsLock);
        (*
        LockScreen;
        WriteString ("Closing group with log file name ");
        WriteString (G^.TransactionLogName);
        WriteLn;
        UnlockScreen;
        *)
        IF G^.UpdateTaskRunning THEN
            CopyTransactionLogUpdates (G);
            G^.closing := TRUE;
        END (*IF*);
        Release (G^.GroupLock);
        Signal (G^.update);
        WHILE G^.closing DO
            Sleep (10);
        END (*WHILE*);
        (*
        LockScreen;
        WriteString ("G^.closing is now FALSE");
        WriteLn;
        UnlockScreen;
        *)
        p := G^.ctxlist;
        WHILE p <> NIL DO
            q := p^.next;
            DISPOSE (p);
            p := q;
        END (*WHILE*);
        DestroySemaphore (G^.update);
        DestroyLock (G^.GroupLock);
        DISPOSE (G);

    END CloseGroup;

(************************************************************************)

PROCEDURE CloseAllGroups;

    (* For use during shutdown, in case some groups still exist. *)

    VAR G: FileShareGroup;

    BEGIN
        Obtain (AllGroupsLock);
        WHILE AllGroups <> NIL DO
            G := AllGroups^.this;
            Obtain (G^.GroupLock);
            Release (AllGroupsLock);
            CloseGroup (G);
            Obtain (AllGroupsLock);
        END (*WHILE*);
        Release (AllGroupsLock);
    END CloseAllGroups;

(************************************************************************)

PROCEDURE RemoveFromGroup (ctx: LogContext);

    (* Removes a context from its group, if any. *)

    VAR G: FileShareGroup;  prev, p: ContextList;

    BEGIN
        G := ctx^.group;
        IF G <> NIL THEN
            Obtain (G^.GroupLock);
            prev := NIL;  p := G^.ctxlist;
            WHILE p <> NIL DO
                IF p^.this = ctx THEN
                    IF prev = NIL THEN
                        G^.ctxlist := p^.next;
                    ELSE
                        prev^.next := p^.next;
                    END (*IF*);
                    DISPOSE (p);
                ELSE
                    prev := p;  p := p^.next;
                END (*IF*);
            END (*WHILE*);
            ctx^.group := NIL;

            (* If the context list is now empty, close the group; and   *)
            (* if the only context in the list is the default context,  *)
            (* remove it from the group.                                *)
            (* Added later: I now think this second part is wrong, so   *)
            (* I've commented it out.                                   *)

            IF G^.ctxlist = NIL THEN
                CloseGroup (G);
            ELSE
                Release (G^.GroupLock);
            END (*IF*);
        END (*IF*);

    END RemoveFromGroup;

(************************************************************************)

PROCEDURE PutIntoGroup (ctx: LogContext;
                         VAR (*IN*) name: ARRAY OF CHAR): FileShareGroup;

    (* Inserts a context into a group matching its filename, creating  *)
    (* a new group if necessary.                                       *)

    VAR p: ListOfGroups;
        G: FileShareGroup;
        CL: ContextList;

    BEGIN
        Strings.Capitalize (name);
        Obtain (AllGroupsLock);
        p := AllGroups;  G := NIL;
        REPEAT
            IF p = NIL THEN
                G := CreateGroup (name);
            ELSIF Strings.Equal (name, p^.this^.TransactionLogName) THEN
                G := p^.this;
            ELSE
                p := p^.next;
            END (*IF*);
        UNTIL G <> NIL;
        Release (AllGroupsLock);

        Obtain (G^.GroupLock);
        NEW (CL);
        CL^.this := ctx;  CL^.next := G^.ctxlist;
        G^.ctxlist := CL;
        ctx^.group := G;
        Release (G^.GroupLock);
        RETURN G;

    END PutIntoGroup;

(************************************************************************)
(*                     OTHER EXPORTED PROCEDURES                        *)
(************************************************************************)

PROCEDURE StartStopPipeAndSyslog (usepipe, hadpipe,
                                  usesyslog, hadsyslog: BOOLEAN);

    (* Starts or stops pipe and syslog logging, as required. *)

    BEGIN
        Obtain (GlobalLock);

        (* Open or close the pipe, as necessary. *)

        IF usepipe <> hadpipe THEN
            IF hadpipe THEN
                DEC (PipeUsers);
                IF PipeUsers = 0 THEN
                    PipeInUse := FALSE;
                    Signal (Pipe.count);
                END (*IF*);
            ELSE
                IF PipeUsers = 0 THEN
                    Signal (PipeOpenRequest);
                    (*Sleep (2000);*)            (* not sure why I had this *)
                END (*IF*);
                INC (PipeUsers);
                PipeInUse := TRUE;
            END (*IF*);
        END (*IF*);

        (* Open or close the syslog socket, as necessary. *)

        IF usesyslog <> hadsyslog THEN
            IF hadsyslog THEN
                DEC (SyslogUsers);
                IF SyslogUsers = 0 THEN
                    SyslogActive := FALSE;
                    StopSyslogging;
                END (*IF*);
            ELSE
                IF SyslogUsers = 0 THEN
                    StartSyslogging;
                END (*IF*);
                INC (SyslogUsers);
                SyslogActive := TRUE;
            END (*IF*);
        END (*IF*);

        Release (GlobalLock);

    END StartStopPipeAndSyslog;

(************************************************************************)

PROCEDURE StartTransactionLogging (ctx: LogContext;
                            LogfileName: ARRAY OF CHAR;  level: CARDINAL);

    (* Sets the transaction log file name, and enables logging.  The    *)
    (* level parameter means: 0 for none, 1 for disk, 2 for screen, 4   *)
    (* for pipe, 8 for syslog, and sums of these for multiple log       *)
    (* targets.  This can be called more than once, to change the log   *)
    (* file name or level.  On a second or later call the existing log  *)
    (* file is closed and the next log entry will cause it to be        *)
    (* reopened, possibly as a new file with a new name.  Similarly,    *)
    (* the pipe and syslog socket will be opened or closed if the       *)
    (* change in level requires this.                                   *)

    VAR UsedPipe, UsedSyslog, changefile: BOOLEAN;
        target: FlagArray;
        t: TargetType;

    BEGIN
        level := level MOD 16;
        changefile := (ctx^.group = NIL) OR
               NOT StringMatch (LogfileName, ctx^.group^.TransactionLogName);

        IF changefile OR (ctx^.level <> level) THEN

            IF (ctx <> DefaultCtx) AND (DefaultCtx^.level <> level) THEN
                IF level = 0 THEN
                    LogTransactionL (DummyID, "Transaction logging stopped");
                END (*IF*);
                StartTransactionLogging (DefaultCtx, LogfileName, level);
                IF level > 0 THEN
                    LogTransactionL (DummyID, "Transaction logging started or restarted");
                END (*IF*);
            END (*IF*);

            (* Remove this context from its file share group. *)

            IF ctx^.Target[todisk] THEN
                ctx^.Target[todisk] := FALSE;
                RemoveFromGroup (ctx);
            END (*IF*);

            (* Were we already using pipe and/or syslog? *)

            UsedPipe := ctx^.Target[topipe];
            UsedSyslog := ctx^.Target[tosyslog];

            (* Where do we want the logs to go to now? *)

            ctx^.level := level;
            FOR t := MIN(TargetType) TO MAX(TargetType) DO
                target[t] := ODD(level);
                level := level DIV 2;
            END (*FOR*);

            (* If we are now logging to disk, put this context into *)
            (* the group appropriate to the log file name.          *)

            IF target[todisk] THEN
                ctx^.group := PutIntoGroup (ctx, LogfileName);
            END (*IF*);

            (* Open or close the pipe and syslog socket, as necessary. *)

            StartStopPipeAndSyslog (target[topipe], UsedPipe,
                                    target[tosyslog], UsedSyslog);

            (* Set the flags for log targets. We have not committed     *)
            (* them until now because we don't wamt ctx^.Target[todisk] *)
            (* to be set until ctx^.group exists.                       *)

            ctx^.Target := target;

        END (*IF*);

    END StartTransactionLogging;

(************************************************************************)

PROCEDURE OpenLogContext(): LogContext;

    (* Creates a new context in which logfile IDs can then be created. *)

    VAR result: LogContext;
        t: TargetType;

    BEGIN
        NEW (result);
        WITH result^ DO
            group := NIL;
            level := 0;
            FOR t := MIN(TargetType) TO MAX(TargetType) DO
                Target[t] := FALSE;
            END (*FOR*);
        END (*WITH*);
        RETURN result;
    END OpenLogContext;

(************************************************************************)

PROCEDURE CloseLogContext (VAR (*INOUT*) ctx: LogContext);

    (* Closes and discards a log context. *)

    BEGIN
        IF ctx^.Target[todisk] THEN
            RemoveFromGroup (ctx);
        END (*IF*);
        DISPOSE (ctx);
    END CloseLogContext;

(************************************************************************)

PROCEDURE CreateLogID (ctx: LogContext;  code: ARRAY OF CHAR): TransactionLogID;

    (* Creates a new logfile ID within the given log context. *)

    VAR result: TransactionLogID;

    BEGIN
        NEW (result);
        WITH result^ DO
            context := ctx;
            Strings.Assign (code, prefix);
        END (*WITH*);
        RETURN result;
    END CreateLogID;

(************************************************************************)

PROCEDURE GetLogPrefix (LogID: TransactionLogID;  VAR (*OUT*) code: ARRAY OF CHAR);

    (* Returns the code used as a sessin ID prefix in the log. *)

    BEGIN
        Strings.Assign (LogID^.prefix, code);
    END GetLogPrefix;

(************************************************************************)

PROCEDURE DummyLogID (): TransactionLogID;

    (* Returns a NIL result of the correct type. *)

    BEGIN
        RETURN NIL;
    END DummyLogID;

(************************************************************************)
(*                            INITIALISATION                            *)
(************************************************************************)

BEGIN
    CRLF[0] := CHR(13);  CRLF[1] := CHR(10);
    TopLine := BlankLine;
    ShuttingDown := FALSE;
    LogSocket := NotASocket;
    OurHostname := "localhost";
    SetProcname ("", 0);
    AllGroups := NIL;
    CreateLock (AllGroupsLock);
    CreateLock (GlobalLock);
    CreateLock (LogSocketLock);
    SetSyslogHost ("127.0.0.1");
    PipeUsers := 0;
    SyslogUsers := 0;
    SyslogActive := FALSE;
    PipeInUse := FALSE;
    InitialisePipe;
    DefaultCtx := OpenLogContext();
    ScreenAvailable := NotDetached();
    DefaultCtx^.Target[toscreen] := ScreenAvailable;
    DummyID := CreateLogID (DefaultCtx, "*******");
FINALLY
    ShuttingDown := TRUE;
    LogTransactionL (DummyID, "Transaction logging closing down");
    Signal (Pipe.count);
    DiscardLogID (DummyID);
    CloseLogContext (DefaultCtx);
    CloseAllGroups;
END TransLog.

