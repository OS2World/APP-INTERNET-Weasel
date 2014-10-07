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

IMPLEMENTATION MODULE Inet2Misc;

        (********************************************************)
        (*                                                      *)
        (* Miscellaneous procedures for networking applications *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            17 January 2002                 *)
        (*  Last edited:        18 May 2005                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, CARD16, CARD32,
    (* proc *)  CAST;

IMPORT Strings;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM Names IMPORT
    (* type *)  HostName;

FROM Sockets IMPORT
    (* const*)  AF_INET,
    (* type *)  Socket,
    (* proc *)  select, setsockopt, send;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostbyaddr;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

(********************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    DecimalDigits = CharSet{'0'..'9'};
    HexDigits = CharSet{'0'..'9', 'A'..'F', 'a'..'f'};

VAR
    ScreenLock: Lock;

(********************************************************************************)

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

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD32;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap4;

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

PROCEDURE AddressToHostName (address: CARDINAL;
                             VAR (*OUT*) Name: HostName);

    (* Converts a numeric IP address to a name.  *)

    VAR HostInfo: HostEntPtr;

    BEGIN
        Name[0] := Nul;
        HostInfo := gethostbyaddr (address, SIZE(CARDINAL), AF_INET);
        IF HostInfo <> NIL THEN
            IF HostInfo^.h_name <> NIL THEN
                Strings.Assign (HostInfo^.h_name^, Name);
            ELSIF (HostInfo^.h_addr_list <> NIL)
                          AND (HostInfo^.h_addr_list^[0] <> NIL) THEN
                address := HostInfo^.h_addr_list^[0]^;
            END (*IF*);
        END (*IF*);

        (* If we have failed to find the name via nameserver lookup,    *)
        (* convert the address to a string and use that.                *)

        IF Name[0] = Nul THEN
            IPToString (address, TRUE, Name);
        END (*IF*);

    END AddressToHostName;

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

(************************************************************************)

PROCEDURE Synch (S: Socket);

    (* Ensures that outgoing data is sent right now rather than being   *)
    (* buffered.  This reduces performance a little, but is needed to   *)
    (* work around a bug in Netscape and MS FTP Exploder.               *)

    VAR OptionValue: CARDINAL;  dummy: CHAR;

    BEGIN
        OptionValue := 1;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
        send (S, dummy, 0, 0);
        OptionValue := 0;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
    END Synch;

(************************************************************************)

PROCEDURE EVAL (f: ARRAY OF LOC);

    (* A do-nothing procedure - we use it for evaluating a function and *)
    (* ignoring the result.                                             *)

    BEGIN
    END EVAL;

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
            IF name[j] IN DecimalDigits THEN
                REPEAT
                    INC(j);
                UNTIL NOT(name[j] IN DecimalDigits);
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

PROCEDURE StringToIP (name: ARRAY OF CHAR): CARDINAL;

    (* Converts an N.N.N.N string to an address in network byte order.  We      *)
    (* assume that the caller has already checked that the string is in this    *)
    (* format.                                                                  *)

    VAR pos: CARDINAL;

    (****************************************************************************)

    PROCEDURE GetNum(): CARDINAL;

        (* Picks up a number at name[pos], increments pos. *)

        VAR ans: CARDINAL;

        BEGIN
            ans := 0;
            WHILE name[pos] IN DecimalDigits DO
                ans := 10*ans + (ORD(name[pos]) - ORD('0'));
                INC (pos);
            END (*WHILE*);
            RETURN ans;
        END GetNum;

    (****************************************************************************)

    TYPE Arr4 = ARRAY [0..3] OF CARD8;

    VAR k: [0..3];  val: Arr4;

    BEGIN
        pos := 0;  k := 0;
        val := CAST(Arr4, VAL(CARDINAL,0));
        LOOP
            val[k] := GetNum();
            IF (k = 3) OR (name[pos] <> '.') THEN
                EXIT (*LOOP*);
            END (*IF*);
            INC (pos);
            INC (k);
        END (*LOOP*);
        RETURN CAST(CARDINAL, val);
    END StringToIP;

(************************************************************************)

PROCEDURE StringMatch (str1, str2: ARRAY OF CHAR): BOOLEAN;

    (* Checks if str1 and str2 are equal, modulo character case. *)

    BEGIN
        Strings.Capitalize (str1);
        Strings.Capitalize (str2);
        RETURN Strings.Equal (str1, str2);
    END StringMatch;

(********************************************************************************)

PROCEDURE ToLower (VAR (*INOUT*) string: ARRAY OF CHAR);

    (* Converts all letters in string to lower case. *)

    CONST shift = ORD('a') - ORD('A');

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE (j <= HIGH(string)) AND (string[j] <> Nul) DO
            IF string[j] IN CharSet {'A'..'Z'} THEN
                INC (string[j], shift);
            END (*IF*);
            INC (j);
        END (*WHILE*);
    END ToLower;

(********************************************************************************)

PROCEDURE StripSpaces (VAR (*INOUT*) arg: ARRAY OF CHAR);

    (* Removes leading and trailing spaces from arg.  *)

    VAR k: CARDINAL;

    BEGIN
        (* Leading spaces. *)

        k := 0;
        WHILE (k <= HIGH(arg)) AND (arg[k] = ' ') DO
            INC (k);
        END (*WHILE*);
        IF k > 0 THEN
            Strings.Delete (arg, 0, k);
        END (*IF*);

        (* Trailing spaces. *)

        k := LENGTH(arg);
        WHILE (k > 0) AND (arg[k-1] = ' ') DO
            DEC (k);
        END (*WHILE*);
        IF k <= HIGH(arg) THEN
            arg[k] := Nul;
        END (*IF*);

    END StripSpaces;

(********************************************************************************)

PROCEDURE SplitArg (VAR (*OUT*) first: ARRAY OF CHAR;
                    VAR (*INOUT*) arg: ARRAY OF CHAR);

    (* Finds the first space character in arg, and assigns everything   *)
    (* before it to first, and everything after it to arg.  Leading and *)
    (* trailing spaces are stripped.  If there is no internal space,    *)
    (* arg is copied to first and arg then becomes the empty string.    *)

    VAR k: CARDINAL;  found: BOOLEAN;

    BEGIN
        StripSpaces (arg);
        Strings.Assign (arg, first);
        Strings.FindNext (' ', first, 0, found, k);
        IF found THEN
            first[k] := Nul;
            WHILE (k <= HIGH(arg)) AND (arg[k] = ' ') DO
                INC (k);
            END (*WHILE*);
            Strings.Delete (arg, 0, k);
        ELSE
            arg[0] := Nul;
        END (*IF*);
    END SplitArg;

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

PROCEDURE HexEncodeDigit (val: CARDINAL): CHAR;

    (* Converts val to one-digit hexadecimal. *)

    BEGIN
        IF val < 10 THEN
            RETURN CHR(ORD('0') + val);
        ELSE
            RETURN CHR(ORD('A') + val - 10);
        END (*IF*);
    END HexEncodeDigit;

(********************************************************************************)

PROCEDURE HexEncodeByte (V: LOC;  VAR (*OUT*) result: ARRAY OF CHAR;
                                                                  pos: CARDINAL);

    (* Converts val to two-digit hexadecimal, stores the result at      *)
    (* result[pos] and result[pos+1].                                   *)

    VAR val: CARD8;

    BEGIN
        val := CAST(CARD8, V);
        result[pos]   := HexEncodeDigit(val DIV 16);
        result[pos+1] := HexEncodeDigit(val MOD 16);
    END HexEncodeByte;

(********************************************************************************)

PROCEDURE HexEncodeArray (VAR (*IN*) input: ARRAY OF LOC;  N: CARDINAL;
                          VAR (*OUT*) result: ARRAY OF CHAR);

    (* Each byte of input becomes two hexadecimal digits. *)

    VAR j: CARDINAL;

    BEGIN
        IF N > HIGH(input) THEN
            N := HIGH(input) + 1;
        END (*IF*);
        IF 2*N > HIGH(result) THEN
            N := ((HIGH(result) + 1)) DIV 2;
        END (*IF*);
        IF N > 0 THEN
            FOR j := 0 TO N-1 DO
                HexEncodeByte (input[j], result, 2*j);
            END (*FOR*);
        END (*FOR*);
        N := 2*N;
        IF N <= HIGH(result) THEN
            result[N] := Nul;
        END (*IF*);
    END HexEncodeArray;

(********************************************************************************)

PROCEDURE DecodeHexDigit (ch: CHAR): CARDINAL;

    (* Converts a one-digit hexadecimal number to cardinal. *)

    BEGIN
        IF ch IN DecimalDigits THEN
            RETURN ORD(ch) - ORD('0');
        ELSE
            RETURN ORD(CAP(ch)) - ORD('A') + 10;
        END (*IF*)
    END DecodeHexDigit;

(********************************************************************************)

PROCEDURE DecodeHex (input: ARRAY OF CHAR): CARDINAL;

    (* Converts a hexadecimal number to cardinal. *)

    VAR j, result: CARDINAL;

    BEGIN
        j := 0;  result := 0;
        WHILE (j <= HIGH(input)) AND (input[j] IN HexDigits) DO
            result := 16*result + DecodeHexDigit(input[j]);
            INC(j);
        END (*WHILE*);
        RETURN result;
    END DecodeHex;

(********************************************************************************)

PROCEDURE DecodeHexString (VAR (*IN*) input: ARRAY OF CHAR;
                           VAR (*OUT*) result: ARRAY OF LOC);

    (* Each two characters of input is interpreted as a pair of two hexadecimal digits. *)

    VAR j, k: CARDINAL;  ans: CARD8;

    BEGIN
        j := 0;  k := 0;
        LOOP
            IF (j > HIGH(input)) OR NOT(input[j] IN HexDigits) OR (k > HIGH(result)) THEN
                EXIT (*LOOP*);
            END (*IF*);
            ans := DecodeHexDigit(input[j]);  INC(j);
            IF input[j] IN HexDigits THEN
                ans := 16*ans + DecodeHexDigit(input[j]);
                INC(j);
            END (*IF*);
            result[k] := CAST(LOC,ans);
            INC(k);
        END (*LOOP*);
    END DecodeHexString;

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

BEGIN
    CreateLock (ScreenLock);
END Inet2Misc.

