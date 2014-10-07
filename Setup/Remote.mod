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

IMPLEMENTATION MODULE Remote;

        (************************************************************)
        (*                                                          *)
        (*          PM Setup for my networking applications         *)
        (*                Communication with INIRemote              *)
        (*                                                          *)
        (*    Started:        7 October 1999                        *)
        (*    Last edited:    21 May 2013                           *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)

IMPORT SYSTEM, OS2, DID, Strings, INIData;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM Sockets IMPORT
    (* const *) NotASocket, AF_INET, AF_UNSPEC, SOCK_STREAM,
    (* type *)  Socket, SockAddr,
    (* proc *)  sock_init, soclose, socket, connect, send, recv,
                gethostid;

FROM NetDB IMPORT
    (* type *)  HostEntPtr, AddressPointerArrayPointer,
    (* proc *)  gethostbyname;

FROM Internet IMPORT
    (* const *) Zero8,
    (* proc *)  inet_addr;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CreateINIFile, CloseINIFile, INIValid,
                INIGet, INIGetString, INIPut, INIPutString;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, NameIsNumeric, Swap2, Swap4, ConvertCard;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;

    WindowPosition = RECORD
                         x, y: INTEGER;
                     END (*RECORD*);

VAR
    (* OurAppTitle is the application name to display in messages. *)
    (* OurSetupName.INI is the name of the local INI file.         *)
    (* (Not to be confused with the application's INI file.)       *)

    OurAppTitle, OurSetupName, OurINI: FilenameString;

    (* Language to use for displaying messages.  If none has been  *)
    (* set, NoLanguageSet is TRUE.                                 *)

    ourlang: LangHandle;
    NoLanguageSet: BOOLEAN;

    (* Receive buffer for responses from server. *)

    ReceiveBuffer: ARRAY [0..511] OF CHAR;
    ReceiveCount, RBPos: CARDINAL;

    (* CurrentDir is the directory from which this program is running.  *)
    (* RemoteDir is the remote application directory specified in the   *)
    (* INI file.                                                        *)

    RemoteDir, CurrentDir: FilenameString;

    CommandSocket: Socket;
    ServerPort: CARDINAL;
    ServerAddress: ARRAY [0..511] OF CHAR;
    ServerIPAddress: CARDINAL;
    Password: ARRAY [0..31] OF CHAR;

    (* A flag that's true only while receiving the response to an       *)
    (* 'L' command to the server.                                       *)

    ListingInProgress: BOOLEAN;

    (* A flag to say whether we're in TNI (as opposed to INI) mode.     *)

    UseTNI: BOOLEAN;

(************************************************************************)
(*                   RETURNING OUR CURRENT DIRECTORY                    *)
(************************************************************************)

PROCEDURE OurDirectory (Remote: BOOLEAN;
                        VAR (*OUT*) DirectoryName: ARRAY OF CHAR);

    (* Returns either the remote current directory or the local current *)
    (* directory, depending on the first parameter.                     *)

    BEGIN
        IF Remote THEN
            Strings.Assign (RemoteDir, DirectoryName);
        ELSE
            Strings.Assign (CurrentDir, DirectoryName);
        END (*IF*);
    END OurDirectory;

(************************************************************************)

PROCEDURE OurIPAddress (Remote: BOOLEAN): CARDINAL;

    (* Returns either the IP address of the remote machine, or our own  *)
    (* IP address, depending on the first parameter.  In either case    *)
    (* the address returned is in network byte order.                   *)

    BEGIN
        IF Remote THEN
            RETURN ServerIPAddress;
        ELSE
            RETURN Swap4(gethostid());
        END (*IF*);
    END OurIPAddress;

(************************************************************************)
(*                       TALKING TO INIREMOTE                           *)
(************************************************************************)

PROCEDURE LoadReceiveBuffer;

    (* Loads the receive buffer with whatever the server sends. *)
    (* Updates ReceiveCount, ReceiveCount=0 means failure.      *)

    VAR NullResponseCount: CARDINAL;

    BEGIN
        NullResponseCount := 0;
        REPEAT
            ReceiveCount := recv (CommandSocket, ReceiveBuffer, 512, 0);
            IF ReceiveCount = MAX(CARDINAL) THEN
                ReceiveCount := 0;
            END (*IF*);
            IF ReceiveCount = 0 THEN
                INC (NullResponseCount);
            END (*IF*);
        UNTIL (ReceiveCount > 0) OR (NullResponseCount > 20);
        RBPos := 0;
    END LoadReceiveBuffer;

(************************************************************************)

PROCEDURE ReceiveLine (VAR (*OUT*) buffer: ARRAY OF CHAR): BOOLEAN;

    (* Receives a line of response from the server. *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR pos: CARDINAL;  ch: CHAR;

    BEGIN
        pos := 0;
        LOOP
            (* Make sure there's something in the receive buffer. *)

            IF RBPos >= ReceiveCount THEN
                LoadReceiveBuffer;
                IF ReceiveCount = 0 THEN
                    buffer[0] := Nul;
                    RETURN FALSE;
                END (*IF*);
            END (*IF*);

            (* Transfer data from receive buffer to result buffer. *)

            LOOP
                IF RBPos >= ReceiveCount THEN
                    EXIT (*LOOP*);
                ELSE
                    ch := ReceiveBuffer[RBPos];  INC(RBPos);
                    IF ch = CR THEN
                        IF pos <= HIGH(buffer) THEN
                            buffer[pos] := Nul;
                        END (*IF*);
                        RETURN TRUE;
                    ELSIF ch = LF THEN
                        (* do nothing with line feed *)
                    ELSE
                        IF pos <= HIGH(buffer) THEN
                            buffer[pos] := ch;  INC(pos);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);

        END (*LOOP*);

    END ReceiveLine;

(************************************************************************)

PROCEDURE OKReply(): BOOLEAN;

    (* Checks whether the INIServe reply was positive. *)

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        RETURN ReceiveLine(buffer) AND (buffer[0] = '+');
    END OKReply;

(************************************************************************)

PROCEDURE SendCommand (cmd: ARRAY OF CHAR): BOOLEAN;

    (* Executes an INIServe command. *)

    VAR k, BufferSize: CARDINAL;  result: BOOLEAN;
        bufptr: CharArrayPointer;

    BEGIN
        BufferSize := LENGTH(cmd) + 2;
        ALLOCATE (bufptr, BufferSize);
        k := 0;
        WHILE (k <= HIGH(cmd)) AND (cmd[k] <> Nul) DO
            bufptr^[k] := cmd[k];  INC(k);
        END (*WHILE*);
        bufptr^[k] := CR;  INC(k);
        bufptr^[k] := LF;  INC(k);
        result := send (CommandSocket, bufptr^, k, 0) <> MAX(CARDINAL);
        DEALLOCATE (bufptr, BufferSize);
        RETURN result;
    END SendCommand;

(************************************************************************)

PROCEDURE ExecCommand (cmd: ARRAY OF CHAR): BOOLEAN;

    (* Executes an INIServe command and checks for an acknowledgement. *)

    BEGIN
        RETURN SendCommand (cmd) AND OKReply();
    END ExecCommand;

(************************************************************************)

PROCEDURE ExecCommand2 (part1: ARRAY OF CHAR;
                        VAR (*IN*) part2: ARRAY OF CHAR): BOOLEAN;

    (* Like ExecCommand, but we're allowed to supply the command in     *)
    (* two parts.                                                       *)

    VAR j, k: CARDINAL;  result: BOOLEAN;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;

    BEGIN
        BufferSize := LENGTH(part1) + LENGTH(part2) + 2;
        ALLOCATE (bufptr, BufferSize);
        j := 0;  k := 0;
        WHILE (k <= HIGH(part1)) AND (part1[k] <> Nul) DO
            bufptr^[j] := part1[k];  INC(j);  INC(k);
        END (*WHILE*);
        k := 0;
        WHILE (k <= HIGH(part2)) AND (part2[k] <> Nul) DO
            bufptr^[j] := part2[k];  INC(j);  INC(k);
        END (*WHILE*);
        bufptr^[j] := CR;  INC(j);
        bufptr^[j] := LF;  INC(j);
        result := (send (CommandSocket, bufptr^, j, 0) <> MAX(CARDINAL))
                   AND OKReply();
        DEALLOCATE (bufptr, BufferSize);
        RETURN result;
    END ExecCommand2;

(************************************************************************)

PROCEDURE ConnectToServer (hwnd: OS2.HWND;  statusbox: CARDINAL): BOOLEAN;

    (* Opens the connection to the remote INI server.  The parameters   *)
    (* identify a dialogue window in which we can report progress.      *)

    VAR hep: HostEntPtr;  peer: SockAddr;  rejected: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;
        p: AddressPointerArrayPointer;

    BEGIN
        ServerIPAddress := 0;
        IF CommandSocket <> NotASocket THEN
            soclose (CommandSocket);
        END (*IF*);
        CommandSocket := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
        IF CommandSocket = NotASocket THEN
            IF NoLanguageSet THEN
                message := "Can't create socket";
            ELSE
                StrToBuffer (ourlang, "Remote.nosocket", message);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, statusbox, message);
        END (*IF*);
        IF NameIsNumeric (ServerAddress) THEN
            ServerIPAddress := inet_addr(ServerAddress);
        ELSE
            IF NoLanguageSet THEN
                message := "Looking up name";
            ELSE
                StrToBuffer (ourlang, "Remote.lookingup", message);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, statusbox, message);
            hep := gethostbyname (ServerAddress);
            IF hep = NIL THEN p := NIL
            ELSE p := hep^.h_addr_list
            END (*IF*);
            IF (p = NIL) OR (p^[0] = NIL) THEN
                IF NoLanguageSet THEN
                    message := "Unknown host";
                ELSE
                    StrToBuffer (ourlang, "Remote.unknownhost", message);
                END (*IF*);
                OS2.WinSetDlgItemText (hwnd, statusbox, message);
                EVAL (soclose(CommandSocket));
                CommandSocket := NotASocket;
            ELSE
                ServerIPAddress := p^[0]^;
            END (*IF*);
        END (*IF*);
        IF CommandSocket <> NotASocket THEN
            IF NoLanguageSet THEN
                message := "Attempting to connect";
            ELSE
                StrToBuffer (ourlang, "Remote.trying", message);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, statusbox, message);

            WITH peer DO
                family := AF_INET;
                WITH in_addr DO
                    port := Swap2(ServerPort);
                    addr := ServerIPAddress;
                    zero := Zero8;
                END (*WITH*);
            END (*WITH*);

            IF connect (CommandSocket, peer, SIZE(peer)) THEN
                IF NoLanguageSet THEN
                    message := "Failed to connect";
                ELSE
                    StrToBuffer (ourlang, "Remote.failed", message);
                END (*IF*);
                OS2.WinSetDlgItemText (hwnd, statusbox, message);
                EVAL (soclose(CommandSocket));
                RETURN FALSE;
            END (*IF*);

        END (*IF*);

        IF (CommandSocket <> NotASocket) AND NOT OKReply() THEN
            IF NoLanguageSet THEN
                message := "Server rejected us";
            ELSE
                StrToBuffer (ourlang, "Remote.serverrejected", message);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, statusbox, message);
            EVAL (soclose(CommandSocket));
            CommandSocket := NotASocket;
        END (*IF*);

        IF CommandSocket <> NotASocket THEN

            (* Have a connection, now log in. *)

            rejected := FALSE;
            IF ExecCommand2 ("P", Password) THEN

                (* Set the remote working directory. *)

                IF NOT ExecCommand2 ("C", RemoteDir) THEN
                    IF NoLanguageSet THEN
                        message := "Invalid remote directory";
                    ELSE
                        StrToBuffer (ourlang, "Remote.invaliddir", message);
                    END (*IF*);
                    OS2.WinSetDlgItemText (hwnd, statusbox, message);
                    rejected := TRUE;
                END (*IF*);

            ELSE
                IF NoLanguageSet THEN
                    message := "Password rejected";
                ELSE
                    StrToBuffer (ourlang, "Remote.badpassword", message);
                END (*IF*);
                OS2.WinSetDlgItemText (hwnd, statusbox, message);
                rejected := TRUE;
            END (*IF*);

            IF rejected THEN
                EVAL (ExecCommand('Q'));
                EVAL (soclose(CommandSocket));
                CommandSocket := NotASocket;
            END (*IF*);

        END (*IF*);

        IF CommandSocket <> NotASocket THEN
            IF NoLanguageSet THEN
                message := "Connected";
            ELSE
                StrToBuffer (ourlang, "Remote.connected", message);
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, statusbox, message);
        END (*IF*);

        RETURN CommandSocket <> NotASocket;

    END ConnectToServer;

(************************************************************************)

PROCEDURE SelectRemoteFile (filename: ARRAY OF CHAR): BOOLEAN;

    (* Tells the remote server which INI or TNI file to work on.  The   *)
    (* filename is either an absolute file name, or a name relative to  *)
    (* the current working directory, on the remote server.             *)

    BEGIN
        RETURN ExecCommand2 ('F', filename);
    END SelectRemoteFile;

(************************************************************************)
(*                 POSTING ON A REMOTE EVENT SEMAPHORE                  *)
(************************************************************************)

PROCEDURE PostSemaphore (semName: ARRAY OF CHAR): BOOLEAN;

    (* Posts on a public event semaphore. *)

    BEGIN
        RETURN ExecCommand2 ('E', semName);
    END PostSemaphore;

(************************************************************************)
(*                 GETTING A REMOTE DIRECTORY LISTING                   *)
(************************************************************************)

PROCEDURE StartDirectoryListing (dirname: ARRAY OF CHAR): BOOLEAN;

    (* Assumption: we are already connected to the INIServe server.     *)
    (* Sends a request to the server to give us a listing of            *)
    (* directory "dirname", returns TRUE if a positive response is      *)
    (* received.  The actual entries must be retrieved by repeated      *)
    (* calls to NextDirectoryEntry.                                     *)

    BEGIN
        ListingInProgress := TRUE;
        RETURN ExecCommand2 ('L', dirname);
    END StartDirectoryListing;

(************************************************************************)

PROCEDURE NextDirectoryEntry (VAR (*OUT*) filename: ARRAY OF CHAR): BOOLEAN;

    (* Assumption: StartDirectoryListing has already been called.       *)
    (* Returns the next line of the listing.  Returns FALSE if we       *)
    (* have run off the end of the listing.                             *)

    BEGIN
        IF NOT (ListingInProgress AND ReceiveLine(filename)) THEN
            filename[0] := Nul;
        END (*IF*);
        ListingInProgress := ListingInProgress AND (filename[0] <> Nul);
        RETURN ListingInProgress;
    END NextDirectoryEntry;

(************************************************************************)

PROCEDURE FinishDirectoryListing;

    (* Flushes the remaining lines, if any, of the server response      *)
    (* that was initiated by a StartDirectoryListing call.              *)

    VAR dummy: FilenameString;

    BEGIN
        WHILE NextDirectoryEntry(dummy) DO
        END (*WHILE*);
    END FinishDirectoryListing;

(************************************************************************)
(*                   READING/WRITING A LOCAL INI FILE                   *)
(************************************************************************)

PROCEDURE INIGetDecimal (hini: HINI;  name1, name2: ARRAY OF CHAR): CARDINAL;

    (* Gets a number that's stored in an INI file as a decimal number. *)

    VAR Buffer: ARRAY [0..31] OF CHAR;  result, j: CARDINAL;

    BEGIN
        IF INIGetString (hini, name1, name2, Buffer) THEN
            result := 0;  j := 0;
            WHILE (j < 32) AND (Buffer[j] <> Nul) DO
                result := 10*result + ORD(Buffer[j]) - ORD('0');
                INC(j);
            END (*WHILE*);
        ELSE
            result := MAX(CARDINAL);
        END (*IF*);
        RETURN result;
    END INIGetDecimal;

(************************************************************************)

PROCEDURE INIGetNum (hini: HINI;  name1, name2: ARRAY OF CHAR): CARDINAL;

    (* Gets a number that's stored in an INI file either as a decimal   *)
    (* number or as a one-byte small binary number.  This is to ease    *)
    (* the transition away from using decimal notation.                 *)

    VAR Buffer: ARRAY [0..31] OF CHAR;  result, j: CARDINAL;

    BEGIN
        IF INIGetString (hini, name1, name2, Buffer) THEN
            result := ORD (Buffer[0]);
            IF result >= ORD('0') THEN
                result := 0;  j := 0;
                WHILE (j < 32) AND (Buffer[j] <> Nul) DO
                    result := 10*result + ORD(Buffer[j]) - ORD('0');
                    INC(j);
                END (*WHILE*);
            END (*IF*);
        ELSE
            result := MAX(CARDINAL);
        END (*IF*);
        RETURN result;
    END INIGetNum;

(************************************************************************)

PROCEDURE INIPutDecimal (hini: HINI;  name1, name2: ARRAY OF CHAR;
                                                         value: CARDINAL);

    (* Writes data to an INI file. *)

    VAR Buffer: ARRAY [0..31] OF CHAR;  j: CARDINAL;

    PROCEDURE Convert (val: CARDINAL);

        BEGIN
            IF val > 9 THEN
                Convert (val DIV 10);  val := val MOD 10;
            END (*IF*);
            Buffer[j] := CHR(ORD('0')+val);  INC(j);
        END Convert;

    BEGIN
        j := 0;
        Convert (value);
        Buffer[j] := Nul;
        INIPutString (hini, name1, name2, Buffer);
    END INIPutDecimal;

(************************************************************************)
(*                     SETUP DATA IN LOCAL INI FILE                     *)
(************************************************************************)

PROCEDURE InitialSetup (lang: LangHandle;  AppTitle, SetupName,
                                       DefaultRemoteDir: ARRAY OF CHAR;
                                       textmode: BOOLEAN): BOOLEAN;

    (* Sets the local INI file name to AppName.INI, or to AppName.TNI   *)
    (* if textmode is TRUE; then reads the INIServe access information  *)
    (* from that INI file or TNI file.  The function result is TRUE     *)
    (* iff we're defaulting to remote editing.                          *)

    CONST NilHandle = LangHandle(NIL);

    VAR hini: HINI;  RemoteFlag: BOOLEAN;
        key: ARRAY [0..9] OF CHAR;

    BEGIN
        UseTNI := textmode;
        ourlang := lang;
        NoLanguageSet := lang = NilHandle;
        Strings.Assign (AppTitle, OurAppTitle);
        Strings.Assign (SetupName, OurSetupName);
        Strings.Assign (SetupName, OurINI);
        IF UseTNI THEN
            Strings.Append (".TNI", OurINI);
        ELSE
            Strings.Append (".INI", OurINI);
        END (*IF*);
        hini := OpenINIFile (OurINI, UseTNI);
        IF NOT INIValid(hini) THEN
            hini := CreateINIFile (OurINI, UseTNI);
        END (*IF*);

        (* Set some defaults in case we still can't open INI file. *)

        RemoteFlag := FALSE;
        Strings.Assign (DefaultRemoteDir, RemoteDir);
        CommandSocket := NotASocket;
        ServerPort := 3560;
        ServerAddress := '127.0.0.1';
        Password := "";

        IF INIValid(hini) THEN
            key := "Remote";
            RemoteFlag := INIGetNum (hini, SetupName, key) = 1;
            key := "Directory";
            EVAL (INIGetString (hini, SetupName, key, RemoteDir));
            CommandSocket := NotASocket;
            key := "Port";
            ServerPort := INIGetDecimal (hini, SetupName, key);
            IF ServerPort = MAX(CARDINAL) THEN
                ServerPort := 3560;
            END (*IF*);
            key := "Host";
            EVAL (INIGetString (hini, SetupName, key, ServerAddress));
            key := "Password";
            EVAL (INIGetString (hini, SetupName, key, Password));
            CloseINIFile (hini);
        END (*IF*);
        RETURN RemoteFlag;
    END InitialSetup;

(************************************************************************)

PROCEDURE SaveRemoteFlag (flag: BOOLEAN);

    (* Saves the "remote editing" option in the local INI file.  We     *)
    (* store it in decimal for compatibility with older versions.       *)

    VAR code: CARDINAL;  hini: HINI;

    BEGIN
        IF flag THEN code := 1 ELSE code := 0 END(*IF*);
        hini := OpenINIFile (OurINI, UseTNI);
        INIPutDecimal (hini, OurSetupName, "Remote", code);
        CloseINIFile (hini);
    END SaveRemoteFlag;

(************************************************************************)

PROCEDURE SetInitialWindowPosition (hwnd: OS2.HWND;  label: ARRAY OF CHAR);

    (* If this window has a previously stored position in our INI file, *)
    (* positions the window to that position.  Ditto for font.          *)

    CONST bufsize = 256;

    VAR hini: HINI;  pos: WindowPosition;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        app: ARRAY [0..9] OF CHAR;

    BEGIN
        hini := OpenINIFile (OurINI, UseTNI);
        IF INIValid(hini) THEN
            app := "WindowPos";
            IF INIGet (hini, app, label, pos) THEN
                OS2.WinSetWindowPos (hwnd, 0, pos.x, pos.y, 0, 0, OS2.SWP_MOVE);
            END (*IF*);
            app := "Font";
            IF NOT INIGetString (hini, app, label, FontName)
                          OR (FontName[0] = Nul) THEN
                FontName := "8.Helv";
            END (*IF*);
            CloseINIFile (hini);
        ELSE
            FontName := "8.Helv";
        END (*IF*);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE, bufsize, FontName);
    END SetInitialWindowPosition;

(************************************************************************)

PROCEDURE StoreWindowPosition (hwnd: OS2.HWND;  label: ARRAY OF CHAR;
                                                storefont: BOOLEAN);

    (* Saves the location and (optionally) font of this window in our INI file. *)

    CONST bufsize = 256;

    VAR hini: HINI;  swp: OS2.SWP;
        pos: WindowPosition;
        AttrFound, length: CARDINAL;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        app: ARRAY [0..10] OF CHAR;

    BEGIN
        OS2.WinQueryWindowPos (hwnd, swp);
        pos.x := swp.x;  pos.y := swp.y;
        IF storefont THEN
            length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                         AttrFound,
                                         bufsize, FontName, 0(*OS2.QPF_NOINHERIT*));
            IF length < bufsize THEN
                FontName[length] := Nul;
            END (*IF*);
        END (*IF*);
        hini := OpenINIFile (OurINI, UseTNI);
        IF INIValid(hini) THEN
            app := "WindowPos";
            INIPut (hini, app, label, pos);
            IF storefont THEN
                app := "Font";
                INIPutString (hini, app, label, FontName);
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);
    END StoreWindowPosition;

(************************************************************************)
(*                                                                      *)
(*                         THE SETUP DIALOGUE                           *)
(*                                                                      *)
(*   This runs locally.  Any of the global parameters that it sets      *)
(*   will not be used until the next time we connect to the server.     *)
(*                                                                      *)
(************************************************************************)

PROCEDURE SetLanguage (pagehandle: OS2.HWND);

    (* Relabels this page in the current language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        IF NoLanguageSet THEN
            stringval := "Remote server details";
        ELSE
            StrToBuffer (ourlang, "Remote.title", stringval);
        END (*IF*);
        OS2.WinSetWindowText (pagehandle, stringval);
        IF NoLanguageSet THEN
            stringval := "Hostname";
        ELSE
            StrToBuffer (ourlang, "Remote.HostLabel", stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (pagehandle, DID.RHostLabel, stringval);
        IF NoLanguageSet THEN
            stringval := "INIServe port";
        ELSE
            StrToBuffer (ourlang, "Remote.PortLabel", stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (pagehandle, DID.RPortLabel, stringval);
        IF NoLanguageSet THEN
            stringval := "INIServe password";
        ELSE
            StrToBuffer (ourlang, "Remote.PassLabel", stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (pagehandle, DID.RPassLabel, stringval);
        IF NoLanguageSet THEN
            Strings.Assign (OurAppTitle, stringval);
            Strings.Append (" directory", stringval);
        ELSE
            StrToBufferA (ourlang, "Remote.DirLabel", OurAppTitle, stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (pagehandle, DID.RDirLabel, stringval);
    END SetLanguage;

(**************************************************************************)

PROCEDURE LoadData (hwnd: OS2.HWND);

    (* Fills the dialogue elements with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.        *)

    BEGIN
        OS2.WinSetDlgItemText (hwnd, DID.ISHostname, ServerAddress);
        OS2.WinSetDlgItemShort (hwnd, DID.ISport, ServerPort, FALSE);
        OS2.WinSetDlgItemText (hwnd, DID.ISpassword, Password);
        OS2.WinSetDlgItemText (hwnd, DID.ISWdir, RemoteDir);
    END LoadData;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Updates our global variables from the dialogue, also stores      *)
    (* the updated values in our INI file.                              *)

    VAR hini: HINI;  temp: SYSTEM.INT16;
        key: ARRAY [0..10] OF CHAR;

    BEGIN
        hini := OpenINIFile (OurINI, UseTNI);

        OS2.WinQueryDlgItemText (hwnd, DID.ISHostname, 512, ServerAddress);
        key := "Host";
        INIPutString (hini, OurSetupName, key, ServerAddress);

        OS2.WinQueryDlgItemShort (hwnd, DID.ISport, temp, FALSE);
        ServerPort := SYSTEM.CAST(CARDINAL,temp);
        key := "Port";
        INIPutDecimal (hini, OurSetupName, key, ServerPort);

        OS2.WinQueryDlgItemText (hwnd, DID.ISpassword, 32, Password);
        key := "Password";
        INIPutString (hini, OurSetupName, key, Password);

        OS2.WinQueryDlgItemText (hwnd, DID.ISWdir, 256, RemoteDir);
        temp := LENGTH(RemoteDir);
        IF (temp > 0) THEN
            DEC (temp);
            IF (RemoteDir[temp] = '/') OR (RemoteDir[temp] = '\') THEN
                RemoteDir[temp] := Nul;
            END (*IF*);
        END (*IF*);
        key := "Directory";
        INIPutString (hini, OurSetupName, key, RemoteDir);

        CloseINIFile (hini);

    END StoreData;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR string: ARRAY [0..6] OF CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   string := "Remote";
                   INIData.SetInitialWindowPosition (hwnd, OurINI, string, UseTNI);
                   SetLanguage (hwnd);
                   LoadData (hwnd);
                   RETURN NIL;

           |  OS2.WM_COMMAND:
                   IF OS2.SHORT1FROMMP(mp1) = DID.ISOK THEN
                       OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   StoreData (hwnd);
                   string := "Remote";
                   INIData.StoreWindowPosition (hwnd, OurINI, string, UseTNI);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE OpenSetupDialogue (owner: OS2.HWND);

    (* Creates the remote setup dialogue box. *)

    BEGIN
        IF OS2.WinDlgBox(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.SetupDialogue,                (* dialogue ID *)
                       NIL) = 0                 (* creation parameters *)
        THEN
        ELSE
        END (*IF*);
    END OpenSetupDialogue;

(************************************************************************)

PROCEDURE RememberCurrentDirectory;

    (* Sets the CurrentDir variable to the name of the directory        *)
    (* where the executable resides.                                    *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                        CurrentDir) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (CurrentDir);
            WHILE (j > 0) AND (CurrentDir[j] <> '\') DO
                DEC (j);
            END (*WHILE*);
            CurrentDir[j] := CHR(0);
        ELSE
            CurrentDir := "";
        END (*IF*);

    END RememberCurrentDirectory;

(************************************************************************)

BEGIN
    ReceiveBuffer := "";  ReceiveCount := 0;  RBPos := 0;
    UseTNI := FALSE;
    sock_init();
    RememberCurrentDirectory;
    ListingInProgress := FALSE;
    ourlang := NIL;
    NoLanguageSet := TRUE;
END Remote.

