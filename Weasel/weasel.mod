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

MODULE Weasel;

        (********************************************************)
        (*                                                      *)
        (*             Combined POP3/SMTP server                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 April 1998                   *)
        (*  Last edited:        12 February 2021                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT WV, OS2, TextIO, Strings, INIData, TNIData;

FROM SYSTEM IMPORT LOC, CARD8, CARD16;

FROM ProgName IMPORT
    (* proc *)  GetProgramName;

FROM WINI IMPORT
    (* proc *)  SetTNIMode, OpenINI, CloseINI;

FROM INIData IMPORT
    (* proc *)  INIGet, INIGetString,
                INIPut, SetWorkingDirectory;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM FileOps IMPORT
    (* proc *)  GetProgName;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket, SockAddr, AddressFamily, SocketType,
    (* proc *)  sock_init, socket, so_cancel, setsockopt,
                bind, listen, select, accept, soclose, psock_errno,
                getsockname, getpeername, sock_errno;

FROM Internet IMPORT
    (* const*)  Zero8, INADDR_ANY;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask, ThreadCount, NotDetached;

FROM OS2Sem IMPORT
    (* type *)  SemKind,
    (* proc *)  WaitOnSemaphore;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM SplitScreen IMPORT
    (* proc *)  WriteString, WriteLn;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertDecimal, ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, IPToString;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler;

FROM WSession IMPORT
    (* proc *)  SetServerParameters, NewSession, NumberOfUsers;

FROM POPCommands IMPORT
    (* proc *)  SetPopLogName;

FROM SMTPCommands IMPORT
    (* proc *)  SetMaxMessageSize;

FROM SMTPData IMPORT
    (* proc *)  LoadSMTPINIData;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM Delivery IMPORT
    (* proc *)  LoadDeliveryINIData, ReloadDeliveryINIData,
                StartOnlineChecker;

FROM Domains IMPORT
    (* proc *)  ClearMailboxLocks;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM Names IMPORT
    (* type *)  ServiceType, CardArray2, CardArray3, CardArray, FilenameString;

(************************************************************************)

TYPE
    SocketArray = ARRAY ServiceType OF Socket;
    ServiceNameArray = ARRAY ServiceType OF ARRAY [0..3] OF CHAR;

CONST
    Nul = CHR(0);
    ServiceName = ServiceNameArray {"POP", "SMTP", "MSA", "IMAP"};

CONST
    DefaultPort = CardArray {110, 25, 587, 143};
    DefaultMaxUsers = CardArray {10, 10, 10, 10};
    DefaultTimeout = CardArray {900, 900, 900, 1800};             (* seconds   *)

VAR
    WeaselName: ARRAY [0..63] OF CHAR;
    ProgVersion: ARRAY [0..31] OF CHAR;
    InetdSocket: Socket;
    MainSocket: SocketArray;
    ServerPort: CardArray;
    ServerEnabled: CARDINAL;
    CalledFromInetd: BOOLEAN;
    ScreenEnabled: BOOLEAN;
    ExtraLogging: BOOLEAN;
    UseTNI: BOOLEAN;
    ExceptqActive: BOOLEAN;

    (* Event semaphore to trigger updater task. *)

    UpdaterFlag: OS2.HEV;

    (* Variables needed for shutdown processing. *)

    ShutdownSignal: OS2.HEV;
    ShutdownRequest: Semaphore;
    TaskDone: Semaphore;
    ShutdownInProgress, RapidShutdown: BOOLEAN;

(************************************************************************)
(*                           SHUTTING DOWN                              *)
(************************************************************************)

PROCEDURE ShutdownChecker;

    (* A separate task that waits for a shutdown request.  *)

    VAR StillRunning: BOOLEAN;  j: ServiceType;

    BEGIN
        StillRunning := TRUE;
        WHILE NOT RapidShutdown DO
            Wait (ShutdownRequest);
            RapidShutdown := ShutdownInProgress;
            ShutdownInProgress := TRUE;
            IF StillRunning THEN
                FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                    IF MainSocket[j] <> NotASocket THEN
                        so_cancel (MainSocket[j]);
                    END (*IF*);
                END (*FOR*);
                StillRunning := FALSE;
            END (*IF*);
        END (*WHILE*);
        Signal (TaskDone);
    END ShutdownChecker;

(************************************************************************)

PROCEDURE ["C"] ControlCHandler(): BOOLEAN;

    (* Intercepts a Ctrl/C from the keyboard. *)

    BEGIN
        Signal (ShutdownRequest);
        RETURN TRUE;
    END ControlCHandler;

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
(*                         LOADING THE INI DATA                         *)
(************************************************************************)

PROCEDURE LoadUpdateableINIData;

    (* Loads configuration parameters from the INI file.  These are not *)
    (* all of the parameters; only those that may be modified while the *)
    (* server is running.  The parameters that can be set only at       *)
    (* program startup are set by LoadINIData (see below).  The present *)
    (* procedure is called by LoadINIData, but it may also be called    *)
    (* at other times.                                                  *)

    VAR hini: INIData.HINI;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    VAR BadPasswordLimit, AuthMethods, AuthTime, BindAddr: CARDINAL;
        TimeoutLimit, MaxUsers: CardArray;
        TimeoutLimit2, MaxUsers2: CardArray2;
        TimeoutLimit3, MaxUsers3: CardArray3;
        LogPOPusers, NoPOPinTL: BOOLEAN;
        key: ARRAY [0..10] OF CHAR;
        PopLogName: FilenameString;

    BEGIN
        SYSapp := "$SYS";
        BadPasswordLimit := 4;
        AuthMethods := 0;
        AuthTime := 0;
        BindAddr := 0;
        LogPOPusers := FALSE;
        ExtraLogging := FALSE;
        NoPOPinTL := FALSE;
        hini := OpenINI();
        IF INIData.INIValid (hini) THEN
            EVAL(GetItem ("BindAddr", BindAddr));
            IF NOT GetItem ("MaxUsers", MaxUsers) THEN
                IF GetItem ("MaxUsers", MaxUsers3) THEN
                    MaxUsers[POP] := MaxUsers3[POP];
                    MaxUsers[SMTP] := MaxUsers3[SMTP];
                    MaxUsers[MSA] := MaxUsers3[MSA];
                    MaxUsers[IMAP] := DefaultMaxUsers[IMAP];
                ELSIF GetItem ("MaxUsers", MaxUsers2) THEN
                    MaxUsers[POP] := MaxUsers2[POP];
                    MaxUsers[SMTP] := MaxUsers2[SMTP];
                    MaxUsers[MSA] := DefaultMaxUsers[MSA];
                    MaxUsers[IMAP] := DefaultMaxUsers[IMAP];
                ELSE
                    MaxUsers := DefaultMaxUsers;
                END (*IF*);
            END (*IF*);
            IF NOT GetItem ("TimeOut", TimeoutLimit) THEN
                IF GetItem ("TimeoutLimit", TimeoutLimit3) THEN
                    TimeoutLimit[POP] := TimeoutLimit3[POP];
                    TimeoutLimit[SMTP] := TimeoutLimit3[SMTP];
                    TimeoutLimit[MSA] := TimeoutLimit3[MSA];
                    TimeoutLimit[IMAP] := DefaultTimeout[IMAP];
                ELSIF GetItem ("TimeoutLimit", TimeoutLimit2) THEN
                    TimeoutLimit[POP] := TimeoutLimit2[POP];
                    TimeoutLimit[SMTP] := TimeoutLimit2[SMTP];
                    TimeoutLimit[MSA] := DefaultTimeout[MSA];
                    TimeoutLimit[IMAP] := DefaultTimeout[IMAP];
                ELSE
                    TimeoutLimit := DefaultTimeout;
                END (*IF*);
            END (*IF*);
            IF NOT GetItem ("BadPasswordLimit", BadPasswordLimit) THEN
                BadPasswordLimit := 4;
            END (*IF*);
            IF NOT GetItem ("AuthTime", AuthTime) THEN
                AuthTime := 0;
            END (*IF*);
            IF NOT GetItem ('AuthMethods', AuthMethods) THEN
                AuthMethods := 0;
            END (*IF*);

            (* Remark: LogPOPusers controls the POP log, while      *)
            (* NoPOPinTL controls POP session entries in the        *)
            (* transaction log.                                     *)

            EVAL (GetItem ("LogPOPusers", LogPOPusers));
            EVAL (GetItem ("NoPOPinTL", NoPOPinTL));
            key := "PopLogName";
            IF NOT INIGetString (hini, SYSapp, key, PopLogName) THEN
                PopLogName := "POP.LOG";
            END (*IF*);
            CloseINI;
        END (*IF*);
        SetPopLogName (LogPOPusers, PopLogName);
        ExtraLogging := ReloadDeliveryINIData (FALSE, BindAddr);

        (* The SetServerParameters procedure also forces the SMTPData  *)
        (* module to reload its updateable data.                       *)

        SetServerParameters (MaxUsers, TimeoutLimit, BadPasswordLimit,
                                     AuthTime, AuthMethods, NoPOPinTL);
        SetMaxMessageSize;

    END LoadUpdateableINIData;

(************************************************************************)

PROCEDURE LoadINIData(): BOOLEAN;

    (* Loads setup parameters from "weasel.ini" or "weasel.tni". *)

    VAR hini: INIData.HINI;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE GetItem (name: ARRAY OF CHAR;
                            VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

        BEGIN
            RETURN INIGet (hini, SYSapp, name, variable);
        END GetItem;

    (********************************************************************)

    VAR ServerPort2: CardArray2;  ServerPort3: CardArray3;

    BEGIN
        SYSapp := "$SYS";
        hini := OpenINI();
        IF NOT INIData.INIValid (hini) THEN
            RETURN FALSE;
        END (*IF*);

        IF NOT GetItem ("Enable", ServerEnabled) THEN
            ServerEnabled := 1;
        END (*IF*);
        IF NOT GetItem ("ServerPort", ServerPort) THEN
            IF GetItem ("ServerPort", ServerPort3) THEN
                ServerPort[POP] := ServerPort3[POP];
                ServerPort[SMTP] := ServerPort3[SMTP];
                ServerPort[MSA] := ServerPort3[MSA];
                ServerPort[IMAP] := DefaultPort[IMAP];
            ELSIF GetItem ("ServerPort", ServerPort2) THEN
                ServerPort[POP] := ServerPort2[POP];
                ServerPort[SMTP] := ServerPort2[SMTP];
                ServerPort[MSA] := DefaultPort[MSA];
                ServerPort[IMAP] := DefaultPort[IMAP];
            ELSE
                ServerPort := DefaultPort;
            END (*IF*);
        END (*IF*);
        CloseINI;

        LoadSMTPINIData;
        LoadDeliveryINIData;
        LoadUpdateableINIData;
        RETURN TRUE;

    END LoadINIData;

(************************************************************************)
(*                         COMMAND LINE ARGUMENTS                       *)
(************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) result: CARDINAL;
                             VAR (*OUT*) abort: BOOLEAN): BOOLEAN;

    (* Picks up optional program arguments from the command line.  If a *)
    (* numeric argument is present, returns TRUE and returns its value  *)
    (* in result.  This procedure also sets the TNI mode, or            *)
    (* returns with abort=TRUE if no decision can be made.              *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR j, TNIoption: CARDINAL;  ch: CHAR;
        args: ChanId;
        ParameterString: ARRAY [0..79] OF CHAR;
        NumberPresent: BOOLEAN;

    BEGIN
        ExtraLogging := FALSE;
        NumberPresent := FALSE;
        abort := FALSE;
        TNIoption := 2;             (* 2 meaning "no decision" *)
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, ParameterString);
            j := 0;
            LOOP
                ch := ParameterString[j];  INC(j);
                IF ch = Nul THEN
                    EXIT (*LOOP*);
                ELSIF ch = '-' THEN
                    (* Ignored in this version *);
                ELSIF CAP(ch) = 'F' THEN
                    SetWorkingDirectory;
                ELSIF CAP(ch) = 'I' THEN
                    TNIoption := 0;
                ELSIF CAP(ch) = 'T' THEN
                    TNIoption := 1;
                ELSIF ch IN Digits THEN
                    NumberPresent := TRUE;
                    result := 0;
                    REPEAT
                        result := 10*result + ORD(ch) - ORD('0');
                        ch := ParameterString[j];  INC(j);
                    UNTIL NOT (ch IN Digits);
                    DEC (j);
                ELSIF ch <> ' ' THEN
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);

        IF TNIoption < 2 THEN
            UseTNI := TNIoption <> 0;
        ELSE
            abort := NOT INIData.ChooseDefaultINI("Weasel", UseTNI);
        END (*IF*);

        IF NOT abort THEN
            SetTNIMode (UseTNI);
        END (*IF*);

        RETURN NumberPresent;

    END GetParameters;

(********************************************************************************)
(*                               THE MAIN SERVER CODE                           *)
(********************************************************************************)

PROCEDURE WriteError (LogID: TransactionLogID);

    VAR LogLine: ARRAY [0..255] OF CHAR;

    BEGIN
        Strings.Assign ("Socket error ", LogLine);
        AppendCard (sock_errno(), LogLine);
        LogTransaction (LogID, LogLine);
    END WriteError;

(********************************************************************************)

(*
PROCEDURE AppendHostID (ID: ARRAY OF LOC;  VAR (*INOUT*) str: ARRAY OF CHAR);

    VAR result: ARRAY [0..16] OF CHAR;

    BEGIN
        IPToString (ID, TRUE, result);
        Strings.Append (result, str);
    END AppendHostID;
*)

(********************************************************************************)

PROCEDURE RunTheServer;

    (*  OPERATING AS A SERVER                                                       *)
    (*     1. (Compulsory) Call "bind" to bind the socket with a local address.     *)
    (*        You can usually afford to specify INADDR_ANY as the machine           *)
    (*        address, but you'd normally bind to a specific port number.           *)
    (*     2. Call "listen" to indicate your willingness to accept connections.     *)
    (*     3. Call "accept", getting a new socket (say ns) from the client.         *)
    (*     4. Use procedures "send" and "recv" to transfer data, using socket ns.   *)
    (*        (Meanwhile, your original socket remains available to accept          *)
    (*        more connections, so you can continue with more "accept" operations   *)
    (*        in parallel with these data operations.  If so, you should of course  *)
    (*        be prepared to run multiple threads.)                                 *)
    (*     5. Use "soclose(ns)" to terminate the session with that particular       *)
    (*        client.                                                               *)
    (*     6. Use "soclose" on your original socket to clean up at the end.         *)

    TYPE
        TestType = [0..4];
        TestArray = ARRAY TestType OF Socket;

    VAR ns: Socket;  myaddr, client: SockAddr;
        temp: CARDINAL;
        SocketsToTest, DefaultSocketsToTest: TestArray;
        ServiceToTestMap: ARRAY ServiceType OF TestType;
        j: ServiceType;
        k, nservice: TestType;
        Enabled: ARRAY ServiceType OF BOOLEAN;
        StartupSuccessful: BOOLEAN;
        optval: CARDINAL;
        LogID: TransactionLogID;
        message: ARRAY [0..127] OF CHAR;
        exRegRec: OS2.EXCEPTIONREGISTRATIONRECORD;

    BEGIN
        LogID := CreateLogID (WCtx, "       ");
        IF sock_init() <> 0 THEN
            LogTransactionL (LogID, "No network.");
            DiscardLogID (LogID);
            RETURN;
        END (*IF*);

        ExceptqActive := InstallExceptq (exRegRec);
        IF NOT ExceptqActive THEN
            LogTransactionL (LogID, "Failed to load the Exceptq handler.");
        END (*IF*);

        StartOnlineChecker;
        IF UseTNI THEN
            LogTransactionL (LogID, "Getting configuration data from weasel.tni");
        ELSE
            LogTransactionL (LogID, "Getting configuration data from weasel.ini");
        END (*IF*);
        Enabled[POP] := ODD(ServerEnabled);
        Enabled[SMTP] := ODD(ServerEnabled DIV 2);
        Enabled[MSA] := ODD(ServerEnabled DIV 4);
        Enabled[IMAP] := ODD(ServerEnabled DIV 8);

        IF CalledFromInetd THEN

            (* RUNNING FROM INETD *)

            Strings.Assign ("Weasel started from inetd, socket ", message);
            AppendCard (InetdSocket, message);
            LogTransaction (LogID, message);
            temp := SIZE(client);
            getpeername (InetdSocket, client, temp);
            temp := SIZE (myaddr);
            IF getsockname (InetdSocket, myaddr, temp) THEN
                LogTransactionL (LogID, "Can't identify inetd session type");
            ELSE
                temp := Swap2(myaddr.in_addr.port);
                Strings.Assign ("Connection is to port ", message);
                AppendCard (temp, message);
                LogTransaction (LogID, message);
                j := POP;
                IF temp = DefaultPort[SMTP] THEN
                    j := SMTP;
                ELSIF temp = DefaultPort[IMAP] THEN
                    j := IMAP;
                END (*IF*);
                IF NOT NewSession (j, InetdSocket, client) THEN
                    LogTransactionL (LogID, "Failed to create session");
                END (*IF*);
            END (*IF*);

        ELSE

            (* NORMAL RUNNING *)

            IF UseTNI THEN
                message := "[T]";
            ELSE
                message := "[I]";
            END (*IF*);
            Strings.Append (ProgVersion, message);
            LogTransaction (LogID, message);

            IF ScreenEnabled THEN
                Strings.Append ("            ", message);
                UpdateTopScreenLine (0, message);
                (*SetOurTitle (message);*)
                UpdateTopScreenLine (25, "(C) 1998-xxxx Peter Moylan");
                UpdateTopScreenLine (34, WV.year());
                UpdateTopScreenLine (54, "Users:");

                EVAL (SetBreakHandler (ControlCHandler));
            END (*IF*);

            message := "exceptq support is ";
            IF ExceptqActive THEN
                Strings.Append ("present", message);
            ELSE
                Strings.Append ("absent", message);
            END (*IF*);
            LogTransaction (LogID, message);

            StartupSuccessful := FALSE;

            FOR k := 0 TO MAX(TestType) DO
                DefaultSocketsToTest[k] := NotASocket;
            END (*FOR*);
            nservice := 0;

            FOR j := MIN(ServiceType) TO MAX(ServiceType) DO

                MainSocket[j] := NotASocket;

                IF Enabled[j] AND (j <> IMAP) THEN
                    MainSocket[j] := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
                    ServiceToTestMap[j] := nservice;
                    DefaultSocketsToTest[nservice] := MainSocket[j];
                    INC (nservice);

                    (* Allow reuse of the port we're binding to. *)

                    optval := 1;
                    setsockopt (MainSocket[j], 0FFFFH, 4, optval, SIZE(optval));
                ELSE
                    ServiceToTestMap[j] := MAX(TestType);
                END (*IF*);

                Strings.Assign (ServiceName[j], message);
                IF Enabled[j] THEN
                    IF j = IMAP THEN
                        Strings.Append (" will be handled by imapd.exe", message);
                        Enabled[j] := FALSE;
                    ELSE
                        Strings.Append (" listening on all interfaces, port ", message);
                        AppendCard (ServerPort[j], message);
                    END (*IF*);
                ELSE
                    Strings.Append (" disabled.", message);
                END (*IF*);
                LogTransaction (LogID, message);

                IF Enabled[j] THEN

                    (* Now have the socket, bind to our machine. *)

                    (* We bind to all interfaces for incoming connections, and  *)
                    (* only use BindAddr for outgoing mail -- see modules       *)
                    (* Domains and Delivery.                                    *)

                    WITH myaddr DO
                        family := AF_INET;
                        WITH in_addr DO
                            port := Swap2 (ServerPort[j]);
                            addr := INADDR_ANY;
                            zero := Zero8;
                        END (*WITH*);
                    END (*WITH*);

                    IF bind (MainSocket[j], myaddr, SIZE(myaddr)) THEN

                        WriteError (LogID);
                        Strings.Assign ("Cannot bind to server port ", message);
                        AppendCard (ServerPort[j], message);
                        LogTransaction (LogID, message);

                    ELSE

                        (* Go into listening mode. *)

                        IF listen (MainSocket[j], 5) THEN
                            WriteError (LogID);
                        ELSE
                            StartupSuccessful := TRUE;
                        END (*IF*);

                    END (*IF bind*);

                END (*IF Enabled*);

            END (*FOR*);

            IF StartupSuccessful THEN

                Strings.Assign (ProgVersion, message);
                Strings.Append (" started.", message);
                LogTransaction (LogID, message);

                CardinalToStringLJ (ThreadCount(), message);
                Strings.Append (" threads.", message);
                LogTransaction (LogID, message);

                (* Here's the main service loop. *)

                SocketsToTest := DefaultSocketsToTest;
                WHILE select (SocketsToTest, nservice, 0, 0, MAX(CARDINAL)) > 0 DO
                    (*UpdateTitleBar;*)
                    FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                        IF Enabled[j] THEN
                            k := ServiceToTestMap[j];
                            IF SocketsToTest[k] <> NotASocket THEN
                                temp := SIZE(client);
                                ns := accept (MainSocket[j], client, temp);
                                IF ns <> NotASocket THEN
                                    IF NOT NewSession (j, ns, client) THEN
                                        LogTransactionL (LogID,
                                            "Failed to create session");
                                    END (*IF*);
                                END (*IF*);
                            END (*IF*);
                        END (*IF*);
                    END (*FOR*);
                    SocketsToTest := DefaultSocketsToTest;
                END (*WHILE*);

                IF NOT ShutdownInProgress THEN

                    (* select() has failed for some reason other than   *)
                    (* normal termination.                              *)

                    LogTransactionL (LogID, "Unexpected program termination");
                    WriteError (LogID);
                END (*IF*);

                (* Close all open main sockets. *)

                FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
                    IF (MainSocket[j] <> NotASocket) AND soclose(MainSocket[j]) THEN
                        psock_errno ("");
                    END (*IF*);
                END (*FOR*);

            END (*IF*);

        END (*IF (not) CalledFromInetd *);

        (* End of operation, shut down the server. *)

        IF NOT RapidShutdown THEN
            IF CalledFromInetd THEN
                Sleep (3000);
            ELSIF NumberOfUsers() > 0 THEN
                LogTransactionL (LogID, "Waiting for existing users to finish");
            END (*IF*);
            WHILE (NumberOfUsers() > 0) AND NOT RapidShutdown DO
                Sleep (1000);
            END (*WHILE*);
        END (*IF*);
        RapidShutdown := TRUE;

        LogTransactionL (LogID, "Weasel closing down");
        DiscardLogID (LogID);

        UninstallExceptq (exRegRec);

    END RunTheServer;

(********************************************************************************)
(*                    TASK TO CATCH UPDATES TO THE INI DATA                     *)
(********************************************************************************)

PROCEDURE INIChangeDetector;

    (* Runs as a separate task.  Rereads some of the configuration data each    *)
    (* time a public event semaphore tells us that there's been a change.       *)

    VAR semName: ARRAY [0..127] OF CHAR;
        LogID: TransactionLogID;

    BEGIN
        semName := "\SEM32\";
        Strings.Append (WeaselName, semName);
        Strings.Append ("\UPDATED", semName);
        LogID := CreateLogID (WCtx, "INImon ");
        UpdaterFlag := 0;
        IF OS2.DosOpenEventSem (semName, UpdaterFlag) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, UpdaterFlag, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        WHILE NOT ShutdownInProgress DO
            WaitOnSemaphore (event, UpdaterFlag);
            IF NOT ShutdownInProgress THEN
                IF UseTNI THEN
                    LogTransactionL (LogID, "Reloading the TNI data");
                ELSE
                    LogTransactionL (LogID, "Reloading the INI data");
                END (*IF*);
                LoadUpdateableINIData;
            END (*IF*);
        END (*WHILE*);

        OS2.DosCloseEventSem(UpdaterFlag);
        Signal (TaskDone);

    END INIChangeDetector;

(********************************************************************************)
(*                   TASK TO CATCH EXTERNAL SHUTDOWN REQUESTS                   *)
(********************************************************************************)

PROCEDURE ShutdownRequestDetector;

    (* Runs as a separate task, and responds to the global event semaphore      *)
    (* that requests Weasel to shut down.                                       *)

    VAR semName: ARRAY [0..127] OF CHAR;

    BEGIN
        semName := "\SEM32\";
        Strings.Append (WeaselName, semName);
        Strings.Append ("\SHUTDOWN", semName);
        ShutdownSignal := 0;
        IF OS2.DosOpenEventSem (semName, ShutdownSignal) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, ShutdownSignal, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);

        (* A second signal on the same semaphore means that we want a *)
        (* rapid shutdown (or that shutdown has already happened.)    *)

        WHILE NOT RapidShutdown DO
            WaitOnSemaphore (event, ShutdownSignal);
            Signal (ShutdownRequest);
        END (*WHILE*);

        Signal (TaskDone);

    END ShutdownRequestDetector;

(********************************************************************************)
(*           PROCEDURE TO TELL THE OUTSIDE WORLD THAT WE'VE FINISHED            *)
(********************************************************************************)

PROCEDURE NotifyTermination;

    (* Posts the global event semaphore that tells other programs that Weasel   *)
    (* has shut down.                                                           *)

    VAR hev: OS2.HEV;
        semName: ARRAY [0..127] OF CHAR;

    BEGIN
        semName := "\SEM32\";
        Strings.Append (WeaselName, semName);
        Strings.Append ("\FINISHED", semName);

        hev := 0;
        IF OS2.DosOpenEventSem (semName, hev) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, hev, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (hev);
        OS2.DosCloseEventSem(hev);
    END NotifyTermination;

(********************************************************************************)
(*               CORRECTION TO COMPENSATE INI DATA FORMAT CHANGE                *)
(********************************************************************************)

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;
    ThreeCard = ARRAY [0..2] OF CARDINAL;
    FourCard = ARRAY [0..3] OF CARDINAL;

(********************************************************************************)

PROCEDURE AdjustINIData;

    (* The order of some FourCard INI entries changed in Feb 2018 (ver 2.44c).  *)
    (* This procedure corrects the order if we see a discrepancy.               *)

    VAR SYSapp: ARRAY [0..7] OF CHAR;

    (****************************************************************************)

    PROCEDURE LoadTwoToFour (hini: INIData.HINI;
                                    key: ARRAY OF CHAR;  default: FourCard;
                                                VAR (*OUT*) result: FourCard);

        (* Loads a four-cardinal value from the INI file, allowing for the  *)
        (* possibility that an older version of the software might have     *)
        (* left only two or three cardinals there.  Any values not found    *)
        (* are set to the default.                                          *)

        VAR twoval: TwoCard;  threeval: ThreeCard;

        BEGIN
            result := default;
            IF NOT INIGet (hini, SYSapp, key, result) THEN
                IF INIGet (hini, SYSapp, key, threeval) THEN
                    result[0] := threeval[0];
                    result[1] := threeval[1];
                    result[2] := threeval[2];
                ELSIF INIGet (hini, SYSapp, key, twoval) THEN
                    result[0] := twoval[0];
                    result[1] := twoval[1];
                END (*IF*);
            END (*IF*);
        END LoadTwoToFour;

    (****************************************************************************)

    CONST
        DefaultPort = FourCard{110, 25, 587, 143};
        DefaultTimeout = FourCard{120, 120, 120, 0};
        DefaultMaxUsers = FourCard{10, 10, 10, 0};

    VAR cardval, temp: CARDINAL;  val, IMAPdata: FourCard;
        switch, bit0, bit1, bit2, bit3, btemp: BOOLEAN;
        hini: INIData.HINI;

    BEGIN
        SYSapp := "$SYS";
        hini := OpenINI();
        IF INIData.INIValid (hini) THEN

            (* Server ports. *)

            LoadTwoToFour (hini, 'ServerPort', DefaultPort, val);

            (* The order of these values changed in Feb 2018.  Try to   *)
            (* correct the order if we see a discrepancy.               *)

            switch := val[0] = 25;
            IF switch THEN
                temp := val[0];  val[0] := val[1];  val[1] := temp;
                temp := val[2];  val[2] := val[3];  val[3] := temp;
                INIPut (hini, SYSapp, 'ServerPort', val);

                IMAPdata[0] := val[3];

                (* Timeout values. *)

                LoadTwoToFour (hini, 'TimeOut', DefaultTimeout, val);
                temp := val[0];  val[0] := val[1];  val[1] := temp;
                temp := val[2];  val[2] := val[3];  val[3] := temp;
                INIPut (hini, SYSapp, 'TimeOut', val);
                IMAPdata[1] := val[3];

                (* Maximum number of users. *)

                LoadTwoToFour (hini, 'MaxUsers', DefaultMaxUsers, val);
                temp := val[0];  val[0] := val[1];  val[1] := temp;
                temp := val[2];  val[2] := val[3];  val[3] := temp;
                INIPut (hini, SYSapp, 'MaxUsers', val);
                IMAPdata[2] := val[3];

                (* "Service enabled" flags. *)

                IF NOT INIGet (hini, SYSapp, 'Enable', cardval) THEN
                    cardval := 1;
                END (*IF*);
                bit0 := ODD(cardval);
                bit1 := ODD(cardval DIV 2);
                bit2 := ODD(cardval DIV 4);
                bit3 := ODD(cardval DIV 8);
                btemp := bit0;  bit0 := bit1;  bit1 := btemp;
                btemp := bit2;  bit2 := bit3;  bit3 := btemp;
                cardval := ORD(bit0) + 2*(ORD(bit1) + 2*(ORD(bit2) + 2*ORD(bit3)));
                INIPut (hini, SYSapp, 'Enable', cardval);
            END (*IF*);
        END (*IF*);

        CloseINI;

    END AdjustINIData;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

(*
PROCEDURE DeliberateCrash;

    (* For testing of return codes. *)

    VAR p: POINTER TO CARDINAL;  x: CARDINAL;

    BEGIN
        (*
        p := NIL;
        x := p^;
        *)
        x := 5;
        DeliberateCrash;    (* infinite recursion *)
    END DeliberateCrash;
*)

(********************************************************************************)

VAR abort: BOOLEAN;
    j: ServiceType;

BEGIN
    ExtraLogging := FALSE;
    UseTNI := FALSE;
    GetProgName (WeaselName);
    ScreenEnabled := NotDetached();

    CalledFromInetd := GetParameters(InetdSocket, abort);
    IF abort THEN
        WriteString ("Inconsistency between Weasel.INI and Weasel.TNI");
        WriteLn;
        WriteString ("Run ChooseTNI.cmd to fix the problem, then try again");
        WriteLn;
    ELSE

        FOR j := MIN(ServiceType) TO MAX(ServiceType) DO
            MainSocket[j] := NotASocket;
        END (*FOR*);
        AdjustINIData;
        abort := NOT LoadINIData();
    END (*IF*);
    IF abort THEN
        WriteString ("ERROR: can't open INI file.  Please run Setup");
        WriteLn;
    ELSE
        GetProgramName (ProgVersion);
        ClearMailboxLocks;
        ShutdownInProgress := FALSE;  RapidShutdown := FALSE;
        CreateSemaphore (TaskDone, 0);
        CreateSemaphore (ShutdownRequest, 0);
        EVAL(CreateTask (INIChangeDetector, 2, "update"));
        EVAL(CreateTask (ShutdownChecker, 1, "ctrl/c hook"));
        EVAL(CreateTask (ShutdownRequestDetector, 2, "shutdown"));
        RunTheServer;
        (*
        OS2.DosPostEventSem (ShutdownSignal);
        Wait (TaskDone);
        *)
        OS2.DosPostEventSem (UpdaterFlag);
        Wait (TaskDone);
        Signal (ShutdownRequest);
        Wait (TaskDone);
        NotifyTermination;
    END (*IF*);
END Weasel.

