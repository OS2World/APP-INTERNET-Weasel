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

IMPLEMENTATION MODULE WSession;

        (********************************************************)
        (*                                                      *)
        (*    Session handler for the Weasel POP/SMTP server    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 April 1998                   *)
        (*  Last edited:        12 June 2014                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT WV, LogCtx, Strings, OS2;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Rego IMPORT
    (* proc *)  IsRegistered;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, SetTimeout, GetLine;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr,
    (* proc *)  send, getsockname, soclose, so_cancel;

FROM InetUtilities IMPORT
    (* proc *)  LockScreen, UnlockScreen, IPToString, AddEOL;

FROM Inet2Misc IMPORT
    (* proc *)  ConvertCard, Synch;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Signal;

FROM Timer IMPORT
    (* proc *)  TimedWait, Sleep;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release, CreateTask1, TaskExit;

FROM Hosts IMPORT
    (* proc *)  CheckHost, OnBlacklist;

FROM Names IMPORT
    (* proc *)  CardArray, ServiceType, HostName;

IMPORT Delivery, POPCommands, POPData, SMTPCommands, SMTPData;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction, LogTransactionL;

(************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);
    NilLogID = CAST(TransactionLogID, NIL);
    NilSemaphore = CAST(Semaphore, NIL);
    NilSBuffer = CAST(SBuffer, NIL);

TYPE
    (* Session state record. *)

    Session = RECORD
                  LogID: TransactionLogID;
                  HostIPAddress: CARDINAL;     (* host   - network byte order *)
                  ClientIPAddress: CARDINAL;   (* client - network byte order *)
                  CASE service: ServiceType OF
                     | SMTP:  SS:  SMTPCommands.Session;
                     | POP:   SP:  POPCommands.Session;
                     | IMAP:  SI:  POPCommands.Session;
                     | MSA:   SA:  SMTPCommands.Session;
                  END (*CASE*);
                  isopen: BOOLEAN;
              END (*RECORD*);

    (* Data used in creating a new instance of the session handler task. *)

    NewSessionPointer = POINTER TO NewSessionRecord;
    NewSessionRecord = RECORD
                           service: ServiceType;
                           socket: Socket;
                           IPAddress: CARDINAL;  (* network byte order *)
                       END (*RECORD*);

    (* Data needed by the timeout checker task. *)

    KeepAlivePointer = POINTER TO KeepAliveRecord;
    KeepAliveRecord = RECORD
                          SocketOpen, dying: BOOLEAN;
                          sem: Semaphore;
                          service: ServiceType;
                          socket: Socket;
                          TimedOut: BOOLEAN;
                      END (*RECORD*);

    (* Error message type. *)

    StringArray = ARRAY ServiceType OF ARRAY [0..40] OF CHAR;

CONST
    AccessDenied = StringArray {"421 Access denied",
                                "-ERR Access denied",
                                "* BYE Access denied",
                                "421 Access denied"};
    TooManyUsers = StringArray {"421 User limit exceeded, try again later",
                                "-ERR User limit exceeded, try again later",
                                "* BYE Too many users, try again later",
                                "421 User limit exceeded, try again later"};
    InitialMessage = StringArray {"220 ", "+OK ", "* OK IMAP4rev1 ready", "220 "};

VAR
    (* Critical section protection for these global parameters.  Since  *)
    (* we rarely change them, a single shared lock is sufficient.       *)

    ParamLock: Lock;

    (* Maximum allowed number of simultaneous users. *)

    MaxUsers: CardArray;

    (* Count of active users, and a lock to protect it. *)

    UserCount: CardArray;

    (* Timeout delay, in seconds. *)

    MaxTime: CardArray;

    (* Permission to update title bar. *)

    ScreenLoggingEnabled: BOOLEAN;

    (* Locks to slow down excessive thread creation during a denial     *)
    (* of service attack.                                               *)

    ThreadCreationLock: ARRAY ServiceType OF Lock;

(************************************************************************)
(*                       PARAMETER SETTINGS                             *)
(************************************************************************)

PROCEDURE SetSessionParameters (MaxUserLimit, TimeoutLimit: CardArray;
                                BadPasswordLimit: CARDINAL;
                                AuthTime, SMTPAuthMask: CARDINAL);

    (* Sets some parameters specified by the INI file: maximum nunber   *)
    (* of simultaneous users; how long a session can be idle before it  *)
    (* is forcibly closed; the number of bad login attempts that will   *)
    (* be tolerated before a POP3 session is forcibly terminated; how   *)
    (* long a POP-before-SMTP authorisation remains valid; and a mask   *)
    (* to say which SMTP AUTH options are enabled for incoming mail.    *)
    (* Also gets module SMTPData to reread the filter names.            *)

    VAR j: ServiceType;

    BEGIN
        Obtain (ParamLock);
        MaxUsers := MaxUserLimit;
        FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
            IF TimeoutLimit[j] > MAX(CARDINAL) DIV 1000 THEN
                MaxTime[j] := MAX(CARDINAL) DIV 1000;
            ELSE
                MaxTime[j] := TimeoutLimit[j];
            END (*IF*);
        END (*FOR*);
        Release (ParamLock);
        POPCommands.SetPOPParameters (AuthTime);
        POPCommands.SetBadPasswordLimit (BadPasswordLimit);
        SMTPCommands.SetAuthMask (SMTPAuthMask);
        SMTPData.UpdateINIData;
    END SetSessionParameters;

(************************************************************************)
(*                      OPENING A NEW CLIENT SESSION                    *)
(************************************************************************)

(*
PROCEDURE UpdateTitleBar;

    (* Sets the program title bar to reflect the number of users. *)

    VAR k: CARDINAL;  notechange: BOOLEAN;
        message: ARRAY [0..63] OF CHAR;

    BEGIN
        Obtain (ParamLock);
        notechange := ScreenLoggingEnabled;
        IF notechange THEN
            IF IsRegistered() THEN
                message := "Weasel Pro ";
            ELSE
                message := "Weasel ";
            END (*IF*);
            Strings.Append (WV.version, message);
            Strings.Append ("    POP: ", message);
            k := Strings.Length (message);
            ConvertCard (UserCount[POP], message, k);
            message[k] := Nul;
            Strings.Append (" SMTP: ", message);
            INC (k, 7);
            ConvertCard (UserCount[SMTP], message, k);
            message[k] := Nul;
            SetOurTitle (message);
        END (*IF*);
        Release (ParamLock);
    END UpdateTitleBar;
*)

(************************************************************************)

PROCEDURE UpdateCount (service: ServiceType;  increment: INTEGER): CARDINAL;

    (* Updates the count of the number of users, and returns the new    *)
    (* count.  Special case: if this would take us beyond the MaxUsers  *)
    (* limit, then the count is not updated and the returned value      *)
    (* is zero.                                                         *)

    VAR value: CARDINAL;

    BEGIN
        Obtain (ParamLock);
        IF increment > 0 THEN INC (UserCount[service], increment);
        ELSIF increment < 0 THEN DEC (UserCount[service], -increment)
        END (*IF*);
        value := UserCount[service];
        IF (value > MaxUsers[service]) AND (increment > 0) THEN
            DEC (UserCount[service], increment);  value := 0;
        END (*IF*);
        Release (ParamLock);
        RETURN value;
    END UpdateCount;

(************************************************************************)

PROCEDURE NumberOfUsers(): CARDINAL;

    (* Returns the number of users who are currently logged on. *)

    BEGIN
        RETURN UpdateCount (SMTP, 0) + UpdateCount (MSA, 0)
                                        + UpdateCount (POP, 0);
    END NumberOfUsers;

(************************************************************************)

PROCEDURE TimeoutChecker (arg: ADDRESS);

    (* A new instance of this task is created for each client session.  *)
    (* It kills the corresponding SessionHandler task if more than      *)
    (* MaxTime seconds have passed since the last Signal() on the       *)
    (* session's KeepAlive semaphore.                                   *)

    VAR p: KeepAlivePointer;  patience: CARDINAL;

    BEGIN
        p := arg;
        REPEAT
            Obtain (ParamLock);
            patience := 1000*MaxTime[p^.service];
            Release (ParamLock);
            TimedWait (p^.sem, patience, p^.TimedOut);
        UNTIL p^.TimedOut OR p^.dying;
        IF p^.SocketOpen THEN
            so_cancel (p^.socket);
        END (*IF*);

        (* Wait for the socket to be closed. *)

        WHILE p^.SocketOpen DO
            Sleep (500);
        END (*WHILE*);
        DestroySemaphore (p^.sem);
        DEALLOCATE (p, SIZE(KeepAliveRecord));

    END TimeoutChecker;

(************************************************************************)

PROCEDURE OpenSession (VAR (*INOUT*) session: Session;  SB: SBuffer;
                       KeepAlive: Semaphore;  MayRelay: BOOLEAN): BOOLEAN;

    (* Initialise the data structures for a new session. *)

    VAR success: BOOLEAN;

    BEGIN
        WITH session DO
            IF service = SMTP THEN
                SS := SMTPCommands.OpenSession (SB, HostIPAddress,
                              ClientIPAddress, KeepAlive, LogID, MayRelay,
                               FALSE, success);
            ELSIF service = MSA THEN
                SA := SMTPCommands.OpenSession (SB, HostIPAddress,
                               ClientIPAddress, KeepAlive, LogID, MayRelay,
                                TRUE, success);
            ELSE
                SP := POPCommands.OpenSession (SB, HostIPAddress,
                                       ClientIPAddress, KeepAlive, LogID);
                success := TRUE;
            END (*IF*);
            isopen := success;
        END (*WITH*);
        RETURN success;
    END OpenSession;

(************************************************************************)

PROCEDURE CloseSession (session: Session);

    (* End-of-session tidying up. *)

    BEGIN
        WITH session DO
            IF isopen THEN
                IF service = SMTP THEN
                    SMTPCommands.CloseSession (SS);
                ELSIF service = MSA THEN
                    SMTPCommands.CloseSession (SS);
                ELSE
                    POPCommands.CloseSession (SP);
                END (*IF*);
            END (*IF*);
            IF LogID <> NilLogID THEN
                DiscardLogID (LogID);
            END (*IF*);
        END (*WITH*);
    END CloseSession;

(************************************************************************)

PROCEDURE HandleCommand (session: Session;  VAR (*IN*) command: ARRAY OF CHAR;
                                      VAR (*OUT*) Quit, ServerAbort: BOOLEAN);

    (* Handles a single command from the client.  Returns with Quit     *)
    (* set to TRUE if the client wants to end the session, and with     *)
    (* ServerAbort set to TRUE if the server wants to end the session.  *)

    BEGIN
        WITH session DO
            IF service = SMTP THEN
                SMTPCommands.HandleCommand (session.SS, command,
                                                      Quit, ServerAbort);
            ELSIF service = MSA THEN
                SMTPCommands.HandleCommand (session.SS, command,
                                                      Quit, ServerAbort);
            ELSE
                POPCommands.HandleCommand (session.SP, command, Quit);
                ServerAbort := FALSE;
            END (*IF*);
        END (*WITH*);
    END HandleCommand;

(************************************************************************)

PROCEDURE SessionHandler (arg: ADDRESS);

    (* The task that handles a client session, i.e. this is where all   *)
    (* the real work is done.  There might be several instances of this *)
    (* task running, one for each session that is still open.           *)

    CONST CmdBufferSize = 4096;

    VAR
        pCmdBuffer: POINTER TO ARRAY [0..CmdBufferSize-1] OF CHAR;
        KeepAliveSemaphore: Semaphore;
        sess: Session;
        SB: SBuffer;
        S: Socket;
        UserNumber: CARDINAL;

    (********************************************************************)

    PROCEDURE AbandonSession;

        (* Premature session close. *)

        BEGIN
            IF sess.LogID <> NilLogID THEN
                DiscardLogID (sess.LogID);
            END (*IF*);
            IF KeepAliveSemaphore <> NilSemaphore THEN
                DestroySemaphore (KeepAliveSemaphore);
            END (*IF*);
            IF pCmdBuffer <> NIL THEN
                DEALLOCATE (pCmdBuffer, CmdBufferSize);
            END (*IF*);
            IF SB <> NilSBuffer THEN
                CloseSBuffer (SB);
            END (*IF*);
            IF S <> NotASocket THEN
                EVAL(soclose(S));
            END (*IF*);
            IF UserNumber <> 0 THEN
                EVAL (UpdateCount (sess.service, -1));
            END (*IF*);
            TaskExit;
        END AbandonSession;

    (********************************************************************)

    VAR NSP: NewSessionPointer;
        size: CARDINAL;
        KA: KeepAlivePointer;
        Quit, ServerAbort: BOOLEAN;
        LogFilePrefix: ARRAY [0..6] OF CHAR;
        IPBuffer: ARRAY [0..16] OF CHAR;
        LogMessage: ARRAY [0..127] OF CHAR;
        OurHostName: HostName;
        ServerName: SockAddr;
        IsBanned, MayRelay: BOOLEAN;
        HavePOPbeforeSMTPAuthorisation: BOOLEAN;

    BEGIN                   (* Body of SessionHandler *)

        OS2.DosError (OS2.FERR_DISABLEHARDERR);              (* disable hard error popups *)

        (* Copy the NewSessionPointer^ information. *)

        NSP := arg;
        sess.service := NSP^.service;

        (* Initialise some variables to better keep track of what needs *)
        (* to be tidied up on premature termination.                    *)

        sess.isopen := FALSE;
        sess.LogID := NilLogID;
        pCmdBuffer := NIL;
        KeepAliveSemaphore := NilSemaphore;
        SB := NilSBuffer;

        Obtain (ThreadCreationLock[sess.service]);

        S := NSP^.socket;
        size := SIZE(SockAddr);
        getsockname (S, ServerName, size);
        sess.HostIPAddress := ServerName.in_addr.addr;
        sess.ClientIPAddress := NSP^.IPAddress;
        DEALLOCATE (NSP, SIZE(NewSessionRecord));

        (* Create the log file ID for this session. *)

        CardinalToString (S, LogFilePrefix, 7);
        IF sess.service = SMTP THEN
            LogFilePrefix[0] := 'S';
        ELSIF sess.service = MSA THEN
            LogFilePrefix[0] := 'M';
        ELSE
            LogFilePrefix[0] := 'P';
        END (*IF*);
        sess.LogID := CreateLogID (LogCtx.WCtx, LogFilePrefix);

        (* Log the new session commencement. *)

        Strings.Assign ("New client ", LogMessage);
        IPToString (sess.ClientIPAddress, TRUE, IPBuffer);
        Strings.Append (IPBuffer, LogMessage);
        LogTransaction (sess.LogID, LogMessage);
        NEW (pCmdBuffer);

        (* Check for too many users. *)

        UserNumber := UpdateCount (sess.service, +1);
        IF UserNumber = 0 THEN
            LogTransactionL (sess.LogID, "Too many users");
            Strings.Assign (TooManyUsers[sess.service], pCmdBuffer^);
            size := AddEOL (pCmdBuffer^);
            EVAL (send (S, pCmdBuffer^, size, 0));
            Release (ThreadCreationLock[sess.service]);
            AbandonSession;
        END (*IF*);

        Release (ThreadCreationLock[sess.service]);

        (* Check whether the client is on one of our special lists. *)

        HavePOPbeforeSMTPAuthorisation := FALSE;
        CheckHost (sess.ClientIPAddress, IsBanned, MayRelay);
        IF sess.service = MSA THEN
            MayRelay := FALSE;
        ELSIF (NOT MayRelay) AND (sess.service = SMTP) THEN
            HavePOPbeforeSMTPAuthorisation := POPCommands.POPbeforeSMTP(sess.ClientIPAddress);
            MayRelay := HavePOPbeforeSMTPAuthorisation;
        END (*IF*);

        (* Client on our local blacklist? *)

        IF IsBanned THEN

            (* Pause for 5 seconds to inconvenience the client. *)

            Sleep (5000);
            Strings.Assign (AccessDenied[sess.service], pCmdBuffer^);
            size := AddEOL (pCmdBuffer^);
            EVAL (send (S, pCmdBuffer^, size, 0));
            LogTransactionL (sess.LogID, "Blacklisted client rejected");
            AbandonSession;
        END (*IF*);

        (* Check the realtime blacklists, if this check is enabled. *)

        IF (NOT MayRelay) AND (sess.service = SMTP)
                   AND OnBlacklist(sess.LogID, sess.ClientIPAddress, pCmdBuffer^) THEN
            size := AddEOL (pCmdBuffer^);
            EVAL (send (S, pCmdBuffer^, size, 0));
            AbandonSession;
        END (*IF*);

        (* Special control for POP sessions to reduce simultaneous   *)
        (* logins from the same IP address.                          *)

        IF (sess.service = POP) AND POPData.ThrottlePOP(sess.ClientIPAddress) THEN
            Strings.Assign ("-ERR Too many password errors", pCmdBuffer^);
            LogTransaction (sess.LogID, pCmdBuffer^);
            size := AddEOL (pCmdBuffer^);
            EVAL (send (S, pCmdBuffer^, size, 0));
            AbandonSession;
        END (*IF*);

        (* Create the session information structure.  Note that  *)
        (* OpenSession has not been called up to this point.     *)

        CreateSemaphore (KeepAliveSemaphore, 0);
        SB := CreateSBuffer (S, TRUE);
        IF SB = NilSBuffer THEN
            Strings.Assign ("Out of memory, closing session", pCmdBuffer^);
            LogTransaction (sess.LogID, pCmdBuffer^);
            size := AddEOL (pCmdBuffer^);
            EVAL (send (S, pCmdBuffer^, size, 0));
            AbandonSession;
        END (*IF*);
        Obtain (ParamLock);
        SetTimeout (SB, MaxTime[sess.service]);
        Release (ParamLock);
        IF NOT OpenSession (sess, SB, KeepAliveSemaphore, MayRelay) THEN
            LogTransactionL (sess.LogID, "Client rejected by filter");
            CloseSession (sess);
            AbandonSession;
        END (*IF*);

        (* Create an instance of the TimeoutChecker task. *)

        NEW (KA);
        WITH KA^ DO
            SocketOpen := TRUE;  socket := S;  dying := FALSE;
            service := sess.service;
            sem := KeepAliveSemaphore;
            TimedOut := FALSE;
        END (*WITH*);
        IF NOT CreateTask1 (TimeoutChecker, 3, "weasel timeout", KA) THEN
            DEALLOCATE (KA, SIZE(KeepAliveRecord));
            LogTransactionL (sess.LogID, "Thread limit exceeded");
            CloseSession (sess);
            DestroySemaphore (KeepAliveSemaphore);
            DEALLOCATE (pCmdBuffer, CmdBufferSize);
            TaskExit;
        END (*IF*);

        (* Work out our host name and send the "welcome" message. *)

        OurHostName := "[127.0.0.0]";               (* fallback default *)
        Delivery.GetOurHostName (S, OurHostName);

        Strings.Assign (InitialMessage[sess.service], pCmdBuffer^);
        Strings.Append (OurHostName, pCmdBuffer^);
        Strings.Append (" Weasel ", pCmdBuffer^);
        Strings.Append (WV.version, pCmdBuffer^);
        Strings.Append (" ready", pCmdBuffer^);
        IF sess.service = POP THEN
            Strings.Append (' ', pCmdBuffer^);
            POPCommands.AppendTimeStamp (sess.SP, pCmdBuffer^);
        END (*IF*);
        LogTransaction (sess.LogID, pCmdBuffer^);
        size := AddEOL (pCmdBuffer^);
        Quit := send (S, pCmdBuffer^, size, 0) = MAX(CARDINAL);
        ServerAbort := FALSE;

        (* Here's the main command processing loop.  We leave it when the client  *)
        (* issues a QUIT command, or when socket communications are lost, or      *)
        (* when we deliberately abort the session (via a spam filter, etc.), or   *)
        (* when we get a timeout on the KeepAlive semaphore.                      *)

        LOOP
            IF Quit OR ServerAbort THEN EXIT(*LOOP*) END(*IF*);
            IF GetLine (SB, pCmdBuffer^) THEN
                Signal (KeepAliveSemaphore);
                HandleCommand (sess, pCmdBuffer^, Quit, ServerAbort);
                Synch (S);
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        (* Work out whether the session was terminated by a QUIT, or a timeout, *)
        (* or a communications failure.  In the QUIT case only, renew the       *)
        (* POP-before-SMTP authorisation if we had it.                          *)

        DEALLOCATE (pCmdBuffer, CmdBufferSize);
        IF KA^.TimedOut THEN
            IF sess.service = IMAP THEN
                (*EVAL(SendLineL (SB, "* BYE Autologout; idle for too long"));*)
            END (*IF*);
            LogTransactionL (sess.LogID, "Timed out");
        ELSIF ServerAbort THEN
            LogTransactionL (sess.LogID, "Session aborted by server");
        ELSIF Quit THEN
            IF HavePOPbeforeSMTPAuthorisation THEN
                POPCommands.RefreshSMTPAuthorisation (sess.ClientIPAddress);
            END (*IF*);
        ELSE
            LogTransactionL (sess.LogID, "Session aborted by client");
        END (*IF*);
        LogTransactionL (sess.LogID, "End of session");

        CloseSession (sess);
        KA^.dying := TRUE;  Signal (KA^.sem);
        CloseSBuffer (SB);
        KA^.SocketOpen := FALSE;
        EVAL (UpdateCount (sess.service, -1));

        TaskExit;

    (* Something in the run-time system is not correctly terminating the *)
    (* thread when the exception handler is used, so I've scrapped the   *)
    (* following code.                                                   *)

    (*
    EXCEPT
        LogTransaction (sess.LogID, "SessionHandler detected exception.");
        CloseSession (sess);
        UserNumber := UpdateCount (sess.service, -1);
        soclose(S);
        TaskExit;
    *)

    END SessionHandler;

(********************************************************************************)

PROCEDURE NewSession (serv: ServiceType;  S: Socket;  addr: SockAddr): BOOLEAN;

    (* Starts and runs a client session.  The session runs in a separate        *)
    (* thread; this procedure returns after starting the session, it does not   *)
    (* wait until the session is over.                                          *)

    VAR NSP: NewSessionPointer;
        success: BOOLEAN;

    BEGIN
        Obtain (ThreadCreationLock[serv]);
        NEW (NSP);
        WITH NSP^ DO
            service := serv;
            socket := S;  IPAddress := addr.in_addr.addr;
        END (*WITH*);
        success := CreateTask1 (SessionHandler, 3, "mail session", NSP);
        Release (ThreadCreationLock[serv]);
        IF NOT success THEN
            soclose (S);
            DEALLOCATE (NSP, SIZE(NewSessionRecord));
        END (*IF*);
        RETURN success;
    END NewSession;

(********************************************************************************)
(*                            MODULE INITIALISATION                             *)
(********************************************************************************)

CONST
    many = MAX(CARDINAL);  FifteenMinutes = 15*60*1000;

VAR s: ServiceType;

BEGIN
    CreateLock (ParamLock);
    Obtain (ParamLock);
    MaxUsers := CardArray {many, many, many, many};
    MaxTime := CardArray {FifteenMinutes, FifteenMinutes, 2*FifteenMinutes,
                          FifteenMinutes};
    UserCount := CardArray {0, 0, 0, 0};
    ScreenLoggingEnabled := TRUE;
    Release (ParamLock);
    FOR s := MIN(ServiceType) TO MAX(ServiceType) DO
        CreateLock (ThreadCreationLock[s]);
    END (*FOR*);
FINALLY
    FOR s := MIN(ServiceType) TO MAX(ServiceType) DO
        DestroyLock (ThreadCreationLock[s]);
    END (*FOR*);

    (* Let's not destroy ParamLock, because there could still   *)
    (* be one or more instances of TimeoutChecker running.      *)

END WSession.
