IMPLEMENTATION MODULE InetUtilities;

        (********************************************************)
        (*                                                      *)
        (* Miscellaneous procedures for networking applications *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            23 August 1997                  *)
        (*  Last edited:        16 April 2014                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC, CARD8, CARD16, CARD32, CAST, ADR;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64,
    (* proc *)  Add64, Mul64;

IMPORT Strings, OS2, SysClock;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenAtEnd, CloseFile, FWriteString, FWriteLn;

FROM Conversions IMPORT
    (* type *)  CardinalToString, Card64ToString;

FROM SplitScreen IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket,
    (* proc *)  sock_errno, psock_errno, send, setsockopt, select;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release, CreateTask;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    Digits = CharSet {'0'..'9'};

VAR
    (* Critical section lock for user log file and common log file. *)

    LogFileLock: Lock;

    (* Critical section protection for screen operations. *)

    ScreenLock: Lock;

    (* Anchor block handle for this application.  *)

    hab: OS2.HAB;

(************************************************************************)

PROCEDURE SwapIt (VAR (*INOUT*) arg: ARRAY OF LOC);

    (* Reverses the byte order of its argument. *)

    VAR j, top: CARDINAL;  temp: LOC;

    BEGIN
        top := HIGH(arg);
        FOR j := 0 TO top DIV 2 DO
            temp := arg[j];  arg[j] := arg[top-j];  arg[top-j] := temp;
        END (*FOR*);
    END SwapIt;

(************************************************************************)

PROCEDURE Swap2 (val: CARD16): CARD16;

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD16;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap2;

(************************************************************************)

PROCEDURE Swap4 (val: CARD32): CARD32;

    (* Like Swap2, but for a four-byte argument. *)

    VAR temp: CARD32;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap4;

(********************************************************************************)

PROCEDURE ToLower (VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts all letters in string to lower case. *)

    TYPE CharSet = SET OF CHAR;

    CONST shift = ORD('a') - ORD('A');

    VAR j, length: CARDINAL;

    BEGIN
        length := LENGTH(string);
        IF length > 0 THEN
            FOR j := 0 TO length-1 DO
                IF string[j] IN CharSet {'A'..'Z'} THEN
                    INC (string[j], shift);
                END (*IF*);
            END (*FOR*);
        END (*IF*);
    END ToLower;

(********************************************************************************)

PROCEDURE AppendString (new: ARRAY OF CHAR;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL);

    (* Puts new into the result array, starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.      *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE (pos <= HIGH(result)) AND (j <= HIGH(new)) AND (new[j] <> Nul) DO
            result[pos] := new[j];  INC(pos);  INC(j);
        END (*WHILE*);
    END AppendString;

(********************************************************************************)

PROCEDURE ConvertCard (number: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL);

    (* Converts number to decimal, left justified starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR j: CARDINAL;  buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        CardinalToString (number, buffer, SIZE(buffer));
        j := 0;
        WHILE buffer[j] = ' ' DO INC(j);  END(*WHILE*);
        WHILE (pos <= HIGH(result)) AND (j < SIZE(buffer)) DO
            result[pos] := buffer[j];  INC(pos);  INC(j);
        END (*WHILE*);
    END ConvertCard;

(********************************************************************************)

PROCEDURE ConvertCard64 (number: CARD64;  VAR (*OUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL);

    (* Converts number to decimal, left justified starting at result[pos].      *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR j: CARDINAL;  buffer: ARRAY [0..20] OF CHAR;

    BEGIN
        Card64ToString (number, buffer, SIZE(buffer));
        j := 0;
        WHILE buffer[j] = ' ' DO INC(j);  END(*WHILE*);
        WHILE (pos <= HIGH(result)) AND (j < SIZE(buffer)) DO
            result[pos] := buffer[j];  INC(pos);  INC(j);
        END (*WHILE*);
    END ConvertCard64;

(********************************************************************************)

PROCEDURE ConvertCardRJ (number: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                             fieldwidth: CARDINAL;  VAR (*INOUT*) pos: CARDINAL);

    (* Like ConvertCard, but right justified with space fill.     *)

    VAR j: CARDINAL;  bufptr: POINTER TO ARRAY [0..31] OF CHAR;

    BEGIN
        ALLOCATE (bufptr, fieldwidth);
        CardinalToString (number, bufptr^, fieldwidth);
        j := 0;
        WHILE (pos <= HIGH(result)) AND (j < fieldwidth) DO
            result[pos] := bufptr^[j];  INC(pos);  INC(j);
        END (*WHILE*);
        DEALLOCATE (bufptr, fieldwidth);
    END ConvertCardRJ;

(********************************************************************************)

PROCEDURE ConvertCard64RJ (number: CARD64;  VAR (*OUT*) result: ARRAY OF CHAR;
                             fieldwidth: CARDINAL;  VAR (*INOUT*) pos: CARDINAL);

    (* Like ConvertCardRJ, but for CARD64 numbers.     *)

    VAR j: CARDINAL;  bufptr: POINTER TO ARRAY [0..31] OF CHAR;

    BEGIN
        ALLOCATE (bufptr, fieldwidth);
        Card64ToString (number, bufptr^, fieldwidth);
        j := 0;
        WHILE (pos <= HIGH(result)) AND (j < fieldwidth) DO
            result[pos] := bufptr^[j];  INC(pos);  INC(j);
        END (*WHILE*);
        DEALLOCATE (bufptr, fieldwidth);
    END ConvertCard64RJ;

(********************************************************************************)

PROCEDURE ConvertCardZ (number: CARDINAL;  VAR (*OUT*) result: ARRAY OF CHAR;
                             fieldwidth: CARDINAL;  VAR (*INOUT*) pos: CARDINAL);

    (* Like ConvertCard, but uses a fixed fieldwidth and leading zero fill.     *)

    VAR j: CARDINAL;  bufptr: POINTER TO ARRAY [0..31] OF CHAR;

    BEGIN
        ALLOCATE (bufptr, fieldwidth);
        CardinalToString (number, bufptr^, fieldwidth);
        j := 0;
        WHILE bufptr^[j] = ' ' DO
            bufptr^[j] := '0';  INC(j);
        END(*WHILE*);
        j := 0;
        WHILE (pos <= HIGH(result)) AND (j < fieldwidth) DO
            result[pos] := bufptr^[j];  INC(pos);  INC(j);
        END (*WHILE*);
        DEALLOCATE (bufptr, fieldwidth);
    END ConvertCardZ;

(********************************************************************************)

PROCEDURE ConvertDecimal (VAR (*IN*) numberstring: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts decimal string to cardinal, starting from numberstring[pos].    *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR result: CARDINAL;

    BEGIN
        result := 0;
        WHILE (pos <= HIGH(numberstring)) AND (numberstring[pos] IN Digits) DO
            result := 10*result + (ORD(numberstring[pos]) - ORD('0'));
            INC (pos);
        END (*WHILE*);
        RETURN result;
    END ConvertDecimal;

(********************************************************************************)

PROCEDURE ConvertDecimal64 (VAR (*IN*) numberstring: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARD64;

    (* Like ConvertDecimal, but gives a 64-bit result.  *)

    VAR result: CARD64;

    BEGIN
        result := Zero64;
        WHILE (pos <= HIGH(numberstring)) AND (numberstring[pos] IN Digits) DO
            result := Mul64 (CARD64{10,0}, result);
            Add64 (result, ORD(numberstring[pos]) - ORD('0'));
            INC (pos);
        END (*WHILE*);
        RETURN result;
    END ConvertDecimal64;

(********************************************************************************)

PROCEDURE AppendCard (number: CARDINAL;  VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts number to decimal, appends the result to the string. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := LENGTH(string);
        ConvertCard (number, string, pos);
        IF pos <= HIGH (string) THEN
            string[pos] := Nul;
        END (*IF*);
    END AppendCard;

(********************************************************************************)

PROCEDURE AddEOL (VAR (*INOUT*) buffer: ARRAY OF CHAR): CARDINAL;

    (* Appends a CRLF to the buffer contents, returns the total string length. *)

    CONST CR = CHR(13);  LF = CHR(10);

    VAR length: CARDINAL;

    BEGIN
        length := Strings.Length (buffer);
        IF length >= HIGH(buffer) THEN
            length := HIGH(buffer)-1;
        END (*IF*);
        buffer[length] := CR;  INC(length);
        buffer[length] := LF;  INC(length);
        IF length <= HIGH(buffer) THEN
            buffer[length] := Nul;
        END (*IF*);
        RETURN length;
    END AddEOL;

(********************************************************************************)

PROCEDURE NameIsNumeric (VAR (*INOUT*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name has the form N.N.N.N or [N.N.N.N] where each N is  *)
    (* a decimal number.  (The present version actually accepts an arbitrary    *)
    (* number of numbers separated by dots.)  As a side-effect, we also strip   *)
    (* the square brackets if they are present.                                 *)

    VAR j: CARDINAL;  result: BOOLEAN;

    (****************************************************************************)

    PROCEDURE ScanNumber(): BOOLEAN;

        (* At least one digit. *)

        BEGIN
            IF name[j] IN Digits THEN
                REPEAT
                    INC(j);
                UNTIL NOT(name[j] IN Digits);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        END ScanNumber;

    (****************************************************************************)

    PROCEDURE ScanNumberString(): BOOLEAN;

        (* Numbers separated by '.' *)

        BEGIN
            LOOP
                IF NOT ScanNumber() THEN RETURN FALSE END(*IF*);
                IF name[j] = '.' THEN INC(j)
                ELSE RETURN TRUE
                END (*IF*);
            END (*LOOP*);
        END ScanNumberString;

    (****************************************************************************)

    BEGIN
        j := 0;
        IF name[0] = '[' THEN j := 1 END(*IF*);
        result := ScanNumberString();
        IF result THEN
            IF name[0] = '[' THEN
                result := name[j] = ']';
                IF result THEN INC(j) END(*IF*);
            END (*IF*);
            result := result AND (name[j] = Nul);
        END (*IF*);
        IF result AND (name[0] = '[') THEN
            name[j-1] := Nul;
            Strings.Delete (name, 0, 1);
        END (*IF*);
        RETURN result;
    END NameIsNumeric;

(********************************************************************************)

PROCEDURE IPToString (IP: ARRAY OF LOC;  EncloseInBrackets: BOOLEAN;
                                VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts a four-byte IP address (in network byte order) to a             *)
    (* human-readable form.  There must be at least 15 character positions      *)
    (* available in the result array, or 17 if EncloseInBrackets is TRUE.       *)

    VAR j, position: CARDINAL;

    BEGIN
        IF EncloseInBrackets THEN
            result[0] := '[';  position := 1;
        ELSE
            position := 0;
        END (*IF*);
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,IP[j]), result, position);
            result[position] := '.';  INC(position);
        END (*FOR*);
        ConvertCard (CAST(CARD8,IP[3]), result, position);
        IF EncloseInBrackets THEN
            result[position] := ']';  INC(position);
        END (*IF*);
        IF position <= HIGH(result) THEN
            result[position] := Nul;
        END (*IF*);
    END IPToString;

(************************************************************************)

PROCEDURE LockScreen;

    (* Critical section protection for writing to the screen. *)

    BEGIN
        Obtain (ScreenLock);
    END LockScreen;

(************************************************************************)

PROCEDURE UnlockScreen;

    (* Critical section protection for writing to the screen. *)

    BEGIN
        Release (ScreenLock);
    END UnlockScreen;

(********************************************************************************)

PROCEDURE TimeToString (Time: SysClock.DateTime;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string.  The result array must have room    *)
    (* for at least 19 characters.                                              *)

    VAR j: CARDINAL;

    (****************************************************************************)

    PROCEDURE Convert1 (value: CARDINAL);

        (* Puts a 1-digit number at result[j], updates j. *)

        BEGIN
            result[j] := CHR(value + ORD('0'));  INC(j);
        END Convert1;

    (****************************************************************************)

    PROCEDURE Convert2 (value: CARDINAL);

        (* Puts a 2-digit number at result[j], updates j. *)

        BEGIN
            Convert1 (value DIV 10);  Convert1 (value MOD 10);
        END Convert2;

    (****************************************************************************)

    BEGIN
        j := 0;  Convert2 (Time.year DIV 100);
        Convert2 (Time.year MOD 100);  result[j] := '-';  INC(j);
        Convert2 (Time.month);  result[j] := '-';  INC(j);
        Convert2 (Time.day);  result[j] := ' ';  INC(j);
        Convert2 (Time.hour);  result[j] := ':';  INC(j);
        Convert2 (Time.minute);  result[j] := ':';  INC(j);
        Convert2 (Time.second);
        IF j <= HIGH(result) THEN
            result[j] := Nul;
        END (*IF*);
    END TimeToString;

(********************************************************************************)

PROCEDURE CurrentTimeToString (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time as a character string.  The string     *)
    (* must have room for at least 19 characters.                               *)

    VAR now: SysClock.DateTime;

    BEGIN
        SysClock.GetClock (now);
        TimeToString (now, result);
    END CurrentTimeToString;

(********************************************************************************)

PROCEDURE WriteCard (number: CARDINAL);

    (* Writes a number to the screen. *)

    BEGIN
        IF number > 9 THEN
            WriteCard (number DIV 10);
            number := number MOD 10;
        END (*IF*);
        WriteChar (CHR(number + ORD("0")));
    END WriteCard;

(********************************************************************************)

(*
PROCEDURE WriteError;

    VAR LogLine: ARRAY [0..255] OF CHAR;  pos: CARDINAL;

    BEGIN
        Strings.Assign ("         Socket error ", LogLine);
        pos := LENGTH(LogLine);
        ConvertCard (sock_errno(), LogLine, pos);
        LogLine[pos] := Nul;
        AddToTransactionLog (LogLine);
    END WriteError;
*)

(************************************************************************)

PROCEDURE GetLastError(): CARDINAL;

    (* Returns the error code from WinGetLastError. *)

    BEGIN
        RETURN OS2.WinGetLastError(hab);
    END GetLastError;

(************************************************************************)

PROCEDURE Synch (S: Socket);

    (* Ensures that outgoing data is sent right now rather than being   *)
    (* buffered.  This reduces performance a little, but is needed to   *)
    (* work around a bug in Netscape and MS FTP Exploder.               *)

    (*VAR OptionValue: CARDINAL;  dummy: CHAR;*)

    BEGIN
        (*
        OptionValue := 1;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
        send (S, dummy, 0, 0);
        OptionValue := 0;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
        *)
    END Synch;

(********************************************************************************)

PROCEDURE WaitForSocket (S: Socket;  timeout: CARDINAL): INTEGER;

    (* Waits until something is available on socket S.  The possible return     *)
    (* codes are +1 for OK, 0 for timeout, -1 for error (or cancel).            *)
    (* Specify timeout=MAX(CARDINAL) if you don't want a timeout.               *)

    VAR SocketArray: ARRAY [0..0] OF Socket;

    BEGIN
        SocketArray[0] := S;
        RETURN select (SocketArray, 1, 0, 0, timeout);
    END WaitForSocket;

(********************************************************************************)

PROCEDURE WaitForDataSocket (output: BOOLEAN;
                             DataSocket, CommandSocket: Socket): BOOLEAN;

    (* Waits until DataSocket is ready or out-of-band data arrives on           *)
    (* CommandSocket.  The first parameter should be TRUE if DataSocket is      *)
    (* being used as an output socket, and FALSE if it is being used as an      *)
    (* input socket.  The function result is TRUE iff DataSocket is ready AND   *)
    (* no out-of-band data has arrived on CommandSocket.                        *)

    VAR SocketArray: ARRAY [0..1] OF Socket;  count: INTEGER;

    BEGIN
        SocketArray[0] := DataSocket;
        SocketArray[1] := CommandSocket;
        IF output THEN
            count := select (SocketArray, 0, 1, 1, MAX(CARDINAL));
        ELSE
            count := select (SocketArray, 1, 0, 1, MAX(CARDINAL));
        END (*IF*);
        IF count > 0 THEN
            RETURN SocketArray[1] = NotASocket;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END WaitForDataSocket;

(********************************************************************************)

PROCEDURE OpenLogFile (name: ARRAY OF CHAR): ChanId;

    (* Opens a log file.  This thread retains exclusive access   *)
    (* until it calls CloseLogFile.                              *)

    BEGIN
        Obtain (LogFileLock);
        RETURN OpenAtEnd (name);
    END OpenLogFile;

(********************************************************************************)

PROCEDURE CloseLogFile (id: ChanId);

    (* Closes the specified file, and releases the exclusive lock. *)

    BEGIN
        CloseFile (id);
        Release (LogFileLock);
    END CloseLogFile;

(********************************************************************************)
(*                            INITIALISATION                                    *)
(********************************************************************************)

BEGIN
    CreateLock (LogFileLock);  CreateLock (ScreenLock);
    hab := OS2.WinInitialize (0);
FINALLY
    IF hab <> OS2.NULLHANDLE THEN
        OS2.WinTerminate (hab);
    END (*IF*);
END InetUtilities.

