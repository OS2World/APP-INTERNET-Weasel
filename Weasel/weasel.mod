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

MODULE Weasel;

        (********************************************************)
        (*                                                      *)
        (*             Combined POP3/SMTP server                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            12 April 1998                   *)
        (*  Last edited:        13 January 2017                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT WV, OS2, TextIO, Strings, INIData, TNIData;

FROM SYSTEM IMPORT LOC, CARD8, CARD16;

FROM ProgName IMPORT
    (* proc *)  GetProgramName;

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
    (* proc *)  CreateTask, ThreadCount;

FROM Heap IMPORT
    (* proc *)  EnableHeapLogging, StopHeapLogging;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM SplitScreen IMPORT
    (* proc *)  NotDetached;

FROM INIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, INIGet, INIGetString,
                SetWorkingDirectory;

FROM InetUtilities IMPORT
    (* proc *)  ConvertDecimal, Swap2, ConvertCard, IPToString;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction, LogTransactionL,
                UpdateTopScreenLine;

FROM CtrlC IMPORT
    (* proc *)  SetBreakHandler;

FROM WSession IMPORT
    (* proc *)  SetSessionParameters, NewSession, NumberOfUsers;

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
    (* proc *)  CheckRegistration, ClearMailboxLocks;

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
    ServiceName = ServiceNameArray {"SMTP", "POP", "IMAP", "MSA"};

CONST
    DefaultPort = CardArray {25, 110, 143, 587};
    DefaultMaxUsers = CardArray {10, 10, 10, 10};
    DefaultTimeout = CardArray {900, 900, 1800, 900};             (* seconds   *)

VAR
    WeaselName: ARRAY [0..63] OF CHAR;
    ProgVersion: ARRAY [0..31] OF CHAR;
    InetdSocket: Socket;
    MainSocket: SocketArray;
    ServerPort: CardArray;
    ServerEnabled: CARDINAL;
    BindAddr: CARDINAL;
    ProVersion: BOOLEAN;
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

    VAR BadPasswordLimit, AuthMethods, AuthTime: CARDINAL;
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
        IF UseTNI THEN
            key := "weasel.tni";
            hini := OpenINIFile(key, TRUE);
        ELSE
            key := "weasel.ini";
            hini := OpenINIFile(key, FALSE);
        END (*IF*);
        IF INIData.INIValid (hini) THEN
            EVAL(GetItem ("BindAddr", BindAddr));
            IF NOT GetItem ("MaxUsers", MaxUsers) THEN
                IF GetItem ("MaxUsers", MaxUsers3) THEN
                    MaxUsers[SMTP] := MaxUsers3[SMTP];
                    MaxUsers[POP] := MaxUsers3[POP];
                    MaxUsers[IMAP] := MaxUsers3[IMAP];
                    MaxUsers[MSA] := DefaultMaxUsers[MSA];
                ELSIF GetItem ("MaxUsers", MaxUsers2) THEN
                    MaxUsers[SMTP] := MaxUsers2[SMTP];
                    MaxUsers[POP] := MaxUsers2[POP];
                    MaxUsers[IMAP] := DefaultMaxUsers[IMAP];
                    MaxUsers[MSA] := DefaultMaxUsers[MSA];
                ELSE
                    MaxUsers := DefaultMaxUsers;
                END (*IF*);
            END (*IF*);
            IF NOT GetItem ("TimeOut", TimeoutLimit) THEN
                IF GetItem ("TimeoutLimit", TimeoutLimit3) THEN
                    TimeoutLimit[SMTP] := TimeoutLimit3[SMTP];
                    TimeoutLimit[POP] := TimeoutLimit3[POP];
                    TimeoutLimit[IMAP] := TimeoutLimit3[IMAP];
                    TimeoutLimit[MSA] := DefaultTimeout[MSA];
                ELSIF GetItem ("TimeoutLimit", TimeoutLimit2) THEN
                    TimeoutLimit[SMTP] := TimeoutLimit2[SMTP];
                    TimeoutLimit[POP] := TimeoutLimit2[POP];
                    TimeoutLimit[IMAP] := DefaultTimeout[IMAP];
                    TimeoutLimit[MSA] := DefaultTimeout[MSA];
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
            CloseINIFile (hini);
        END (*IF*);
        SetPopLogName (LogPOPusers, PopLogName);
        ExtraLogging := ReloadDeliveryINIData (FALSE, BindAddr);

        (* The SetSessionParameters procedure also forces the SMTPData  *)
        (* module to reload its updateable data.                        *)

        SetSessionParameters (MaxUsers, TimeoutLimit, BadPasswordLimit,
                                     AuthTime, AuthMethods, NoPOPinTL);
        SetMaxMessageSize;

    END LoadUpdateableINIData;

(************************************************************************)

PROCEDURE LoadINIData;

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
        name: ARRAY [0..10] OF CHAR;

    BEGIN
        SYSapp := "$SYS";
        IF UseTNI THEN
            name := "weasel.tni";
            hini := OpenINIFile(name, TRUE);
        ELSE
            name := "weasel.ini";
            hini := OpenINIFile(name, FALSE);
        END (*IF*);
        IF INIData.INIValid (hini) THEN
            IF NOT GetItem ("Enable", ServerEnabled) THEN
                ServerEnabled := 2;
            END (*IF*);
            IF NOT GetItem ("ServerPort", ServerPort) THEN
                IF GetItem ("ServerPort", ServerPort3) THEN
                    ServerPort[SMTP] := ServerPort3[SMTP];
                    ServerPort[POP] := ServerPort3[POP];
                    ServerPort[IMAP] := ServerPort3[IMAP];
                    ServerPort[MSA] := DefaultPort[MSA];
                ELSIF GetItem ("ServerPort", ServerPort2) THEN
                    ServerPort[SMTP] := ServerPort2[SMTP];
                    ServerPort[POP] := ServerPort2[POP];
                    ServerPort[IMAP] := DefaultPort[IMAP];
                    ServerPort[MSA] := DefaultPort[MSA];
                ELSE
                    ServerPort := DefaultPort;
                END (*IF*);
            END (*IF*);
            EVAL(GetItem ("BindAddr", BindAddr));
            CloseINIFile (hini);
        END (*IF*);

        CheckRegistration(UseTNI);
        LoadSMTPINIData(UseTNI);
        LoadDeliveryINIData(UseTNI);
        LoadUpdateableINIData;

    END LoadINIData;

(************************************************************************)
(*                         COMMAND LINE ARGUMENTS                       *)
(************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) result: CARDINAL): BOOLEAN;

    (* Picks up optional program arguments from the command line.  If a *)
    (* numeric argument is present, returns TRUE and returns its value  *)
    (* in result.                                                       *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR j: CARDINAL;  ch: CHAR;
        args: ChanId;
        ParameterString: ARRAY [0..79] OF CHAR;
        NumberPresent: BOOLEAN;

    BEGIN
        ExtraLogging := FALSE;
        NumberPresent := FALSE;
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
                    UseTNI := FALSE;
                ELSIF CAP(ch) = 'T' THEN
                    UseTNI := TRUE;
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

        (*UseTNI := TRUE;*)      (* while testing TNI mode *)

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
        (*
        IF ExtraLogging THEN
            EnableHeapLogging (LogID);
        END (*IF*);
        *)

        (*UseTNI := TRUE ;*)    (* while debugging *)
        StartOnlineChecker;
        IF UseTNI THEN
            LogTransactionL (LogID, "Getting configuration data from weasel.tni");
        ELSE
            LogTransactionL (LogID, "Getting configuration data from weasel.ini");
        END (*IF*);
        Enabled[SMTP] := ODD(ServerEnabled);
        Enabled[POP] := ODD(ServerEnabled DIV 2);
        Enabled[IMAP] := ODD(ServerEnabled DIV 4) AND ProVersion;
        Enabled[MSA] := ODD(ServerEnabled DIV 8);

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
                ELSIF ProVersion AND (temp = DefaultPort[IMAP]) THEN
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
                UpdateTopScreenLine (25, "(C) 1998-2017 Peter Moylan");

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

                IF Enabled[j] THEN
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
                        Strings.Append (" requires imapd.exe", message);
                        Enabled[j] := FALSE;
                    ELSE
                        Strings.Append (" listening on ", message);
                        (*IF BindAddr = 0 THEN*)
                         Strings.Append ("all interfaces", message);
                        (*
                        ELSE AppendHostID (BindAddr, message);
                        END (*IF*);
                        *)
                        Strings.Append (", port ", message);
                        AppendCard (ServerPort[j], message);
                    END (*IF*);
                ELSE
                    Strings.Append (" disabled.", message);
                END (*IF*);
                LogTransaction (LogID, message);

                IF Enabled[j] THEN

                    (* Now have the socket, bind to our machine. *)

                    (* In the present version we bind to all interfaces for     *)
                    (* incoming connections, and only use BindAddr for          *)
                    (* outgoing mail -- see modules Domains and Delivery.       *)

                    WITH myaddr DO
                        family := AF_INET;
                        WITH in_addr DO
                            port := Swap2 (ServerPort[j]);
                            (*addr := BindAddr;*)
                            addr := INADDR_ANY;
                            zero := Zero8;
                        END (*WITH*);
                    END (*WITH*);

                    IF bind (MainSocket[j], myaddr, SIZE(myaddr)) THEN

                        WriteError (LogID);
                        LogTransactionL (LogID, "Cannot bind to server port.");

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
        IF ExtraLogging THEN
            StopHeapLogging;
        END (*IF*);
        DiscardLogID (LogID);

        UninstallExceptq (exRegRec);

    END RunTheServer;

(********************************************************************************)
(*                    TASK TO CATCH UPDATES TO THE INI DATA                     *)
(********************************************************************************)

PROCEDURE INIChangeDetector;

    (* Runs as a separate task.  Rereads some of the configuration data each    *)
    (* time a public event semaphore tells us that there's been a change.       *)

    VAR count: CARDINAL;
        semName: ARRAY [0..127] OF CHAR;
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
            OS2.DosWaitEventSem (UpdaterFlag, OS2.SEM_INDEFINITE_WAIT);
            OS2.DosResetEventSem (UpdaterFlag, count);
            IF NOT ShutdownInProgress THEN
                LogTransactionL (LogID, "Reloading the INI data");
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
            OS2.DosWaitEventSem (ShutdownSignal, OS2.SEM_INDEFINITE_WAIT);
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

(********************************************************************************)

VAR count: CARDINAL;

BEGIN
    ExtraLogging := FALSE;
    UseTNI := FALSE;
    GetProgName (WeaselName);
    ScreenEnabled := NotDetached();

    CalledFromInetd := GetParameters (InetdSocket);
    LoadINIData;
    ProVersion := TRUE;
    GetProgramName (ProgVersion);
    ClearMailboxLocks;
    ShutdownInProgress := FALSE;  RapidShutdown := FALSE;
    CreateSemaphore (TaskDone, 0);
    CreateSemaphore (ShutdownRequest, 0);
    EVAL(CreateTask (INIChangeDetector, 2, "update"));
    EVAL(CreateTask (ShutdownChecker, 1, "ctrl/c hook"));
    EVAL(CreateTask (ShutdownRequestDetector, 2, "shutdown"));
    RunTheServer;
FINALLY
    OS2.DosPostEventSem (ShutdownSignal);
    OS2.DosResetEventSem (ShutdownSignal, count);
    Wait (TaskDone);
    OS2.DosPostEventSem (UpdaterFlag);
    Wait (TaskDone);  Wait (TaskDone);
    NotifyTermination;
END Weasel.

