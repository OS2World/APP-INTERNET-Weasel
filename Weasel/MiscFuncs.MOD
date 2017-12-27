(**************************************************************************)
(*                                                                        *)
(*  Support module for a variety of applications                          *)
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

IMPLEMENTATION MODULE MiscFuncs;

        (********************************************************)
        (*                                                      *)
        (*              Miscellaneous procedures for            *)
        (*               a variety of applications              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 May 2017                     *)
        (*  Last edited:        16 June 2017                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8,
    (* proc *)  CAST;

FROM Types IMPORT
    (* type *)  CARD64;

FROM LONGLONG IMPORT
    (* const*)  Zero64,
    (* proc *)  Add64, Mul64;

IMPORT Strings;

FROM STextIO IMPORT
    (* proc *)  WriteChar;

FROM Conversions IMPORT
    (* type *)  CardinalToString, Card64ToString;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenAtEnd, CloseFile;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    DecimalDigits = CharSet{'0'..'9'};
    HexDigits = CharSet{'0'..'9', 'A'..'F', 'a'..'f'};

VAR
    ScreenLock, LogFileLock: Lock;

(************************************************************************)

PROCEDURE EVAL (f: ARRAY OF LOC);

    (* A do-nothing procedure - we use it for evaluating a function and *)
    (* ignoring the result.                                             *)

    BEGIN
    END EVAL;

(************************************************************************)
(*                         STRING OPERATIONS                            *)
(************************************************************************)

PROCEDURE StringMatch (str1, str2: ARRAY OF CHAR): BOOLEAN;

    (* Checks if str1 and str2 are equal, modulo character case. *)

    BEGIN
        Strings.Capitalize (str1);
        Strings.Capitalize (str2);
        RETURN Strings.Equal (str1, str2);
    END StringMatch;

(********************************************************************************)

PROCEDURE HeadMatch (VAR (*IN*) str: ARRAY OF CHAR;
                                template: ARRAY OF CHAR): BOOLEAN;

    (* String equality, with case differences ignored.  We check that   *)
    (* template is a leading substring of str.                          *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF template[j] = CHR(0) THEN RETURN TRUE
            ELSIF CAP(str[j]) <> CAP(template[j]) THEN RETURN FALSE
            ELSE INC(j)
            END (*IF*);
        END (*LOOP*);
    END HeadMatch;

(************************************************************************)

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
(*                         NUMERIC TO STRING CONVERSIONS                        *)
(********************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes N in decimal to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;

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

PROCEDURE ConvertDecimal (VAR (*IN*) numberstring: ARRAY OF CHAR;
                                          VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts decimal string to cardinal, starting from numberstring[pos].    *)
    (* On return pos is updated to the next unused array index.                 *)

    VAR result: CARDINAL;

    BEGIN
        result := 0;
        WHILE (pos <= HIGH(numberstring)) AND (numberstring[pos] IN DecimalDigits) DO
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
        WHILE (pos <= HIGH(numberstring)) AND (numberstring[pos] IN DecimalDigits) DO
            result := Mul64 (CARD64{10,0}, result);
            Add64 (result, ORD(numberstring[pos]) - ORD('0'));
            INC (pos);
        END (*WHILE*);
        RETURN result;
    END ConvertDecimal64;

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

PROCEDURE AppendCard (number: CARDINAL;  VAR (*OUT*) message: ARRAY OF CHAR);

    (* Converts number to decimal, appends it to the existing message.  *)

    VAR pos: CARDINAL;

    BEGIN
        pos := Strings.Length(message);
        ConvertCard (number, message, pos);
        IF pos <= HIGH(message) THEN
            message[pos] := Nul;
        END (*IF*);
    END AppendCard;

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
(*                        STRING TO NUMERIC CONVERSION                          *)
(********************************************************************************)

PROCEDURE GetNum (str: ARRAY OF CHAR;  VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Picks up a decimal number at str[pos], increments pos. *)

    VAR ans: CARDINAL;

    BEGIN
        ans := 0;
        WHILE str[pos] IN DecimalDigits DO
            ans := 10*ans + (ORD(str[pos]) - ORD('0'));
            INC (pos);
        END (*WHILE*);
        RETURN ans;
    END GetNum;

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

(************************************************************************)
(*                    PROTECTED ACCESS TO SCREEN OR FILE                *)
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
(*                           MODULE INITIALISATION                              *)
(********************************************************************************)

BEGIN
    CreateLock (ScreenLock);
    CreateLock (LogFileLock);
END MiscFuncs.

