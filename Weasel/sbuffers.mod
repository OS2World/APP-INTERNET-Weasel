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

IMPLEMENTATION MODULE SBuffers;

        (********************************************************)
        (*                                                      *)
        (*       Buffers for line-oriented socket input         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 May 1998                     *)
        (*  Last edited:        27 October 2013                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, ADDRESS,
    (* proc *)  ADR;

FROM Sockets IMPORT
    (* const*)  NotASocket,
    (* type *)  Socket,
    (* proc *)  send, recv, soclose;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Copy, AddOffset;

IMPORT Inet2Misc, Strings;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0); CR = CHR(13); LF = CHR(10); CtrlZ = CHR(26);
    InBufferSize = 8192;
    OutBufferSize = 8192;

TYPE
    InputBufferSubscript = [0..InBufferSize-1];
    LineBufferSubscript = [0..4095];
    OutBufferSubscript = [0..OutBufferSize-1];

    (* InputBuffer is a buffer to hold incoming data.  RBpos is the     *)
    (* character position we're up to in InputBuffer,                   *)
    (* and RBlength is the number of characters in InputBuffer.         *)
    (* LineBuffer is a copy of the incoming line.                       *)
    (* Timeout is the time in milliseconds before we decide that a      *)
    (* connection for incoming data has been lost.  We make this        *)
    (* variable so that we can use a relatively short value when making *)
    (* the initial connection, and then increase the value once the     *)
    (* connection has been established.                                 *)
    (* OutBuffer is a buffer to hold outgoing data, and OutAmount is a  *)
    (* count of how many characters are now in OutBuffer.               *)
    (* OutputFailed says that an error occurred on output.              *)

    SBuffer = POINTER TO SBrecord;
    SBrecord = RECORD
                   socket: Socket;
                   InputBuffer:
                          ARRAY InputBufferSubscript OF CHAR;
                   RBpos, RBlength: CARDINAL;
                   Timeout: CARDINAL;
                   LineBuffer:
                          ARRAY LineBufferSubscript OF CHAR;
                   OutBuffer:
                          ARRAY OutBufferSubscript OF CHAR;
                   OutAmount: CARDINAL;
                   PlusMinusResponse: BOOLEAN;
                   OutputFailed: BOOLEAN;
               END (*RECORD*);

VAR
    CRLF: ARRAY [0..1] OF CHAR;
    LineFeed: ARRAY [0..0] OF CHAR;

(************************************************************************)
(*                CREATING AND DESTROYING AN SBuffer                    *)
(************************************************************************)

PROCEDURE CreateSBuffer (s: Socket;  NumericReplyCode: BOOLEAN): SBuffer;

    (* Creates a new SBuffer.  We assume that the connection on socket  *)
    (* s has already been established by the caller.                    *)
    (* The second parameter controls how response codes from the peer   *)
    (* are encoded.  If this parameter is FALSE, we expect a '+' or     *)
    (* '-' status reply code.  If it is TRUE, we expect a three-digit   *)
    (* numeric code.                                                    *)

    VAR result: SBuffer;

    BEGIN
        NEW (result);
        IF result <> NIL THEN
            WITH result^ DO
                socket := s;
                RBpos := 0;  RBlength := 0;
                OutAmount := 0;
                Timeout := 75000;
                PlusMinusResponse := NOT NumericReplyCode;
                OutputFailed := FALSE;
            END (*WITH*);
        END (*IF*);
        RETURN result;
    END CreateSBuffer;

(************************************************************************)

PROCEDURE CloseSBuffer (VAR (*INOUT*) SB: SBuffer);

    (* Releases the buffer space, closes the socket. *)

    BEGIN
        IF SB <> NIL THEN
            IF SB^.socket <> NotASocket THEN
                EVAL (soclose(SB^.socket));
            END (*IF*);
            DEALLOCATE (SB, SIZE(SBrecord));
        END (*IF*);
    END CloseSBuffer;

(************************************************************************)

PROCEDURE SetTimeout (SB: SBuffer;  seconds: CARDINAL);

    (* Sets the timeout value to the given number of seconds. *)

    BEGIN
        SB^.Timeout := 1000*seconds;
    END SetTimeout;

(************************************************************************)

PROCEDURE SocketOf (SB: SBuffer): Socket;

    (* Returns the socket belonging to this SBuffer. *)

    BEGIN
        IF SB = NIL THEN
            RETURN NotASocket;
        ELSE
            RETURN SB^.socket;
        END (*IF*);
    END SocketOf;

(************************************************************************)
(*                            SOCKET OUTPUT                             *)
(************************************************************************)

PROCEDURE FlushOutput (SB: SBuffer);

    (* Sends out any remaining buffered output. *)

    BEGIN
        IF SB <> NIL THEN
            WITH SB^ DO
                IF OutAmount > 0 THEN
                    OutputFailed := send(socket, OutBuffer, OutAmount, 0)
                                                         = MAX(CARDINAL);
                    OutAmount := 0;
                    Inet2Misc.Synch (socket);
                END (*IF*);
            END (*WITH*);
        END (*IF*);
    END FlushOutput;

(************************************************************************)

PROCEDURE AddToBuffer (SB: SBuffer;  VAR (*IN*) data: ARRAY OF LOC;
                                       amount: CARDINAL): BOOLEAN;

    (* Puts 'amount' characters into the output buffer. *)

    VAR place, count: CARDINAL;

    BEGIN
        IF SB = NIL THEN
            RETURN FALSE;
        END (*IF*);
        place := 0;
        WHILE amount > 0 DO
            count := OutBufferSize - SB^.OutAmount;
            IF count <= amount THEN
                Copy (ADR(data[place]), ADR(SB^.OutBuffer[SB^.OutAmount]),
                                        count);
                INC (place, count);
                DEC (amount, count);
                INC (SB^.OutAmount, count);
                FlushOutput (SB);
            ELSE
                Copy (ADR(data[place]), ADR(SB^.OutBuffer[SB^.OutAmount]),
                                        amount);
                INC (SB^.OutAmount, amount);
                amount := 0;
            END (*IF*);
        END (*WHILE*);
        RETURN NOT SB^.OutputFailed;
    END AddToBuffer;

(************************************************************************)

PROCEDURE SendLine (SB: SBuffer;  VAR (*IN*) line: ARRAY OF CHAR): BOOLEAN;

    (* Sends the string, appending a CRLF. *)

    VAR success: BOOLEAN;

    BEGIN
        success := AddToBuffer(SB, line, LENGTH(line))
                          AND AddToBuffer(SB, CRLF, 2);
        IF success THEN
            FlushOutput (SB);
        END (*IF*);
        RETURN success;
    END SendLine;

(************************************************************************)

PROCEDURE SendLineL (SB: SBuffer;  line: ARRAY OF CHAR): BOOLEAN;

    (* Like SendLine, but for a literal string. *)

    BEGIN
        RETURN SendLine (SB, line);
    END SendLineL;

(************************************************************************)

PROCEDURE SendString (SB: SBuffer;  line: ARRAY OF CHAR): BOOLEAN;

    (* Sends the string, without appending a CRLF. *)

    BEGIN
        RETURN AddToBuffer(SB, line, LENGTH(line));
    END SendString;

(************************************************************************)

PROCEDURE SendRaw (SB: SBuffer;  VAR (*IN*) data: ARRAY OF LOC;
                                               amount: CARDINAL): BOOLEAN;

    (* Sends uninterpreted data. *)

    BEGIN
        RETURN AddToBuffer(SB, data, amount);
    END SendRaw;

(************************************************************************)

PROCEDURE SendChar (SB: SBuffer;  ch: CHAR): BOOLEAN;

    (* Sends a single character. *)

    VAR buffer: ARRAY [0..0] OF CHAR;

    BEGIN
        buffer[0] := ch;
        RETURN AddToBuffer(SB, buffer, 1);
    END SendChar;

(************************************************************************)

PROCEDURE SendEOL (SB: SBuffer): BOOLEAN;

    (* Sends a CRLF. *)

    VAR result: BOOLEAN;

    BEGIN
        result := AddToBuffer(SB, CRLF, 2);
        FlushOutput (SB);
        RETURN result;
    END SendEOL;

(************************************************************************)
(*                            SOCKET INPUT                              *)
(************************************************************************)

PROCEDURE Getch (SB: SBuffer): CHAR;

    (* Result is Nul if connection fails. *)
    (* Assumption: SB <> NIL. *)

    VAR result: CHAR;  EmptyResponseCount: CARDINAL;

    BEGIN
        WITH SB^ DO
            IF RBpos >= RBlength THEN
                RBpos := 0;
                EmptyResponseCount := 0;
                REPEAT
                    IF Inet2Misc.WaitForSocket (socket, Timeout) > 0 THEN
                        RBlength := recv (socket, InputBuffer,
                                      MAX(InputBufferSubscript) + 1, 0);
                    ELSE
                        RBlength := MAX(CARDINAL);
                    END (*IF*);
                    IF RBlength = 0 THEN
                        INC (EmptyResponseCount);
                        IF EmptyResponseCount > 20 THEN
                            RBlength := MAX(CARDINAL);
                        END (*IF*);
                    END (*IF*);
                UNTIL RBlength <> 0;
                IF RBlength = MAX(CARDINAL) THEN
                    RBlength := 0;
                    RETURN Nul;
                END (*IF*);
            END (*IF*);
            result := InputBuffer[RBpos];  INC(RBpos);
        END (*WITH*);
        RETURN result;
    END Getch;

(************************************************************************)

PROCEDURE LoadLineBuffer (SB: SBuffer): BOOLEAN;

    (* Loads the next incoming line of text into SB.LineBuffer.         *)
    (* Assumption: a line ends with CRLF.  To avoid tortuous logic, I   *)
    (* take the LF as end of line and skip the CR.  At end of input we  *)
    (* return with line[0] = Ctrl/Z.                                    *)
    (* A function return of FALSE means that the connection failed.     *)

    VAR length, pos, extra, space, LBpos: CARDINAL;
        found: BOOLEAN;
        source, destination: ADDRESS;

    BEGIN
        space := MAX(LineBufferSubscript) + 1;
        LBpos := 0;
        destination := ADR(SB^.LineBuffer);
        REPEAT
            found := FALSE;
            WITH SB^ DO

                (* InputBuffer holds RBlength-RBpos unused characters.  *)
                (* LineBuffer has room for 'space' more characters.     *)

                IF RBpos >= RBlength THEN
                    RBpos := 0;
                    IF Inet2Misc.WaitForSocket (socket, Timeout) > 0 THEN
                        RBlength := recv (socket, InputBuffer,
                                      MAX(InputBufferSubscript) + 1, 0);
                    ELSE
                        RBlength := MAX(CARDINAL);
                    END (*IF*);
                    IF (RBlength = 0) OR (RBlength = MAX(CARDINAL)) THEN
                        RBlength := 0;
                        IF space > 0 THEN
                            LineBuffer[LBpos] := CtrlZ;
                            INC (LBpos);
                            destination := AddOffset (destination, 1);
                            DEC (space);
                        END (*IF*);
                        found := TRUE;
                    END (*IF*);
                END (*IF*);

                IF NOT found THEN
                    source := ADR(InputBuffer[RBpos]);
                    IF RBpos + RBlength <= MAX(InputBufferSubscript) THEN
                        InputBuffer[RBpos+RBlength] := Nul;
                    END (*IF*);
                    Strings.FindNext (LineFeed, InputBuffer, RBpos, found, pos);
                    IF found AND (pos < RBlength) THEN
                        extra := 1;
                    ELSE
                        pos := RBlength;
                        extra := 0;
                    END (*IF*);
                    length := pos - RBpos;
                    IF (length > 0) AND (InputBuffer[pos-1] = CR) THEN
                        DEC (length);  INC (extra);
                    END (*IF*);

                    (* At this point length is the number of characters *)
                    (* up to but not including the CRLF.  The variable  *)
                    (* extra counts the number of bytes, if any, in the *)
                    (* line terminator.                                 *)

                    IF length > space THEN
                        INC (extra, length-space);
                        length := space;
                    END (*IF*);

                    (* By now it's possible that extra includes some    *)
                    (* characters that aren't line terminators but      *)
                    (* which can't fit in the line buffer.              *)

                    IF length > 0 THEN
                        Copy (source, destination, length);
                        DEC (space, length);
                        destination := AddOffset (destination, length);
                        INC (LBpos, length);
                    END (*IF*);
                    INC (length, extra);
                    IF found THEN
                        IF length > 0 THEN

                            (* Skip to end of line in the receive       *)
                            (* buffer, even if we've had to truncate    *)
                            (* the line in LineBuffer.                  *)

                            INC (RBpos, length);
                        END (*IF*);
                    ELSE
                        (* Line terminator not yet found, force a       *)
                        (* further read from the socket.                *)

                        RBlength := 0;
                    END (*IF*);
                END (*IF*);
            END (*WITH*);
        UNTIL found OR (space = 0);

        IF space > 0 THEN
            SB^.LineBuffer[LBpos] := Nul;
        END (*IF*);

        RETURN SB^.LineBuffer[0] <> CtrlZ;

    END LoadLineBuffer;

(************************************************************************)

PROCEDURE GetLine (SB: SBuffer;  VAR (*OUT*) line: ARRAY OF CHAR): BOOLEAN;

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid tortuous logic, I take the LF as end of line and *)
    (* skip the CR.  At end of file we return with line[0] = Ctrl/Z.    *)
    (* A function return of FALSE means that the connection failed.     *)

    VAR success: BOOLEAN;

    BEGIN
        success := LoadLineBuffer (SB);
        Strings.Assign (SB^.LineBuffer, line);
        RETURN success;
    END GetLine;

(************************************************************************)

PROCEDURE GetResponse (SB: SBuffer;  VAR (*OUT*) MoreToCome: BOOLEAN): BOOLEAN;

    (* Returns one line of the server response to a command.            *)
    (* MoreToCome is set if this is part of a multi-line response, and  *)
    (* it's not the last line.   The function result is FALSE if we've  *)
    (* lost the connection.                                             *)
    (* Assumption: SB <> NIL. *)

    TYPE CharSet = SET OF CHAR;

    CONST Digits = CharSet {'0'..'9'};

    VAR success: BOOLEAN;

    BEGIN
        success := LoadLineBuffer (SB);
        MoreToCome := NOT(SB^.LineBuffer[0] IN Digits)
                         OR
                         ((SB^.LineBuffer[1] IN Digits)
                          AND (SB^.LineBuffer[2] IN Digits)
                          AND (SB^.LineBuffer[3] = '-'));
        RETURN success;
    END GetResponse;

(************************************************************************)

PROCEDURE ResponseCode (SB: SBuffer): CARDINAL;

    (* Receives a (possibly multi-line) response from the server, and   *)
    (* returns the first digit of the numeric code.  The values are:    *)
    (*      0  Connection lost                                          *)
    (*      1  OK, another reply still to come                          *)
    (*      2  OK, command done                                         *)
    (*      3  OK, another command expected                             *)
    (*      4  Transient failure, try again later                       *)
    (*      5  Definite failure                                         *)
    (*      6  Reply code is not numeric                                *)
    (*      7  Connection lost or SB=NIL                                *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    VAR active, MoreToCome: BOOLEAN;

    BEGIN
        IF SB = NIL THEN
            RETURN 7;
        END (*IF*);
        REPEAT
            active := GetResponse (SB, MoreToCome);
        UNTIL SB^.PlusMinusResponse OR NOT (MoreToCome AND active);
        IF active THEN
            IF SB^.PlusMinusResponse THEN
                IF SB^.LineBuffer[0] = '+' THEN
                    RETURN 2;
                ELSE
                    RETURN 5;
                END (*IF*);
            ELSIF SB^.LineBuffer[0] IN Digits THEN
                RETURN ORD(SB^.LineBuffer[0]) - ORD('0');
            ELSE
                RETURN 6;
            END (*IF*);
        ELSE
            RETURN 7;
        END (*IF*);
    END ResponseCode;

(************************************************************************)

PROCEDURE PositiveResponse (SB: SBuffer;
                            VAR (*OUT*) LostConnection: BOOLEAN): BOOLEAN;

    (* Returns TRUE if a positive response was returned.  *)

    TYPE ReplySet = SET OF [0..7];

    VAR code: [0..7];

    BEGIN
        code := ResponseCode(SB);
        LostConnection := code IN ReplySet{0, 7};
        RETURN code IN ReplySet{1..3};
    END PositiveResponse;

(************************************************************************)

PROCEDURE GetLastLine (SB: SBuffer;  VAR (*OUT*) line: ARRAY OF CHAR);

    (* Returns a copy of the last line received. *)

    BEGIN
        IF SB = NIL THEN
            line[0] := Nul;
        ELSE
            Strings.Assign (SB^.LineBuffer, line);
        END (*IF*);
    END GetLastLine;

(************************************************************************)

BEGIN
    CRLF[0] := CR;
    CRLF[1] := LF;
    LineFeed[0] := LF;
END SBuffers.

