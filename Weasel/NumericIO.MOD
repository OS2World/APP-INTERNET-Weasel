(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE NumericIO;

        (****************************************************************)
        (*                                                              *)
        (*              Numeric I/O using windows.                      *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        15 May 1998                             *)
        (*  Status:             OK                                      *)
        (*                                                              *)
        (****************************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16, CARD32, ADDRESS;

FROM Windows IMPORT
    (* type *)  Window, ColumnRange,
    (* proc *)  WriteChar, WriteString, ReadChar, LookaheadChar,
                ReadCharWithoutEcho, EditString, SaveCursor, SetCursor;

FROM Conversions IMPORT
    (* type *)  HexDigit, EightChar,
    (* proc *)  CardinalToString, StringToCardinal, StringToHex,
                LongCardToString, HexToChar, HexToString, LongHexToString;

FROM LowLevel IMPORT
    (* proc *)  SEGMENT, OFFSET,
                HighByte, LowByte, HighWord, LowWord,
                IANDB, RSB;

FROM Keyboard IMPORT
    (* proc *)  InKey, PutBack;

(************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST Digits = CharSet {"0".."9"};

(************************************************************************)
(*                              OUTPUT                                  *)
(************************************************************************)

PROCEDURE WriteHexDigit (w: Window;  number: CARD8);

    (* Writes a one-digit hexadecimal number.   *)

    BEGIN
        WriteChar (w, HexToChar(VAL(HexDigit, number)));
    END WriteHexDigit;

(************************************************************************)

PROCEDURE WriteHexByte (w: Window;  number: CARD8);

    (* Writes the second argument as a two-digit hexadecimal number.    *)

    BEGIN

        (* The obscure function names from LowLevel are:        *)
        (*      RSB = right shift       IANDB = logical AND     *)

        WriteHexDigit (w, RSB(number, 4));
        WriteHexDigit (w, IANDB(number,15));

    END WriteHexByte;

(************************************************************************)

PROCEDURE WriteHexWord (w: Window;  number: CARD16);

    (* Writes the second argument as a four-digit hexadecimal number.   *)

    BEGIN
        WriteHexByte (w, HighByte(number));
        WriteHexByte (w, LowByte(number));
    END WriteHexWord;

(************************************************************************)

PROCEDURE WriteHexLongword (w: Window;  number: CARD32);

    (* Writes the second argument as an eight-digit hexadecimal number. *)

    BEGIN
        WriteHexWord (w, HighWord(number));
        WriteHexWord (w, LowWord(number));
    END WriteHexLongword;

(************************************************************************)

PROCEDURE WriteAddress (w: Window;  addr: ADDRESS);

    (* Writes a segmented address to the screen.        *)

    BEGIN
        WriteHexWord (w, SEGMENT(addr));  WriteChar (w, ":");
        WriteHexLongword (w, OFFSET(addr));
    END WriteAddress;

(************************************************************************)

PROCEDURE WriteLongCard (w: Window;  number: CARD32);

    (* Writes the second argument as a decimal number.  *)

    VAR remainder: CARDINAL;

    BEGIN
        IF number > 9 THEN
            WriteLongCard (w, number DIV 10);
        END (*IF*);
        remainder := number MOD 10;
        WriteChar (w, CHR(remainder + ORD("0")));
    END WriteLongCard;

(************************************************************************)

PROCEDURE WriteCard (w: Window;  number: CARDINAL);

    (* Writes the second argument as a decimal number.  *)

    BEGIN
        WriteLongCard (w, number);
    END WriteCard;

(************************************************************************)

PROCEDURE WriteShortCard (w: Window;  number: CARD8);

    (* Writes the second argument as a decimal number.  *)

    BEGIN
        WriteLongCard (w, number);
    END WriteShortCard;

(************************************************************************)

PROCEDURE WriteInt (w: Window;  number: INTEGER);

    (* Writes the second argument as a decimal number.  *)

    BEGIN
        IF number < 0 THEN
            WriteChar (w, '-');  number := -number;
        END (*IF*);
        WriteCard (w, number);
    END WriteInt;

(************************************************************************)

PROCEDURE WriteRJLongCard (w: Window;  number: CARD32; fieldsize: CARDINAL);

    (* Like WriteLongCard, but the result is right justified in a field *)
    (* of fieldsize characters.                                         *)

    VAR buffer: ARRAY [0..79] OF CHAR;

    BEGIN
        LongCardToString (number, buffer, fieldsize);
        buffer[fieldsize] := CHR(0);
        WriteString (w, buffer);
    END WriteRJLongCard;

(************************************************************************)

PROCEDURE WriteRJCard (w: Window;  number, fieldsize: CARDINAL);

    (* Like WriteCard, but the result is right justified in a field     *)
    (* of fieldsize characters.                                         *)

    VAR buffer: ARRAY [0..79] OF CHAR;

    BEGIN
        CardinalToString (number, buffer, fieldsize);
        buffer[fieldsize] := CHR(0);
        WriteString (w, buffer);
    END WriteRJCard;

(************************************************************************)

PROCEDURE WriteRJShortCard (w: Window;  number: CARD8;
                                        fieldsize: CARDINAL);

    (* Like WriteShortCard, but the result is right justified in a      *)
    (* field of fieldsize characters.                                   *)

    BEGIN
        WriteRJCard (w, number, fieldsize);
    END WriteRJShortCard;

(************************************************************************)
(*                      CHECK FOR Esc KEY                               *)
(************************************************************************)

PROCEDURE EditOK (): BOOLEAN;

    (* Returns TRUE unless the Esc key was pressed.     *)

    CONST Esc = CHR(27);

    VAR ch: CHAR;

    BEGIN
        ch := InKey();  PutBack(ch);
        RETURN ch <> Esc;
    END EditOK;

(************************************************************************)
(*                        HEXADECIMAL INPUT                             *)
(************************************************************************)

PROCEDURE EditHexByte (w: Window;  VAR (*INOUT*) value: CARD8);

    (* Screen editing of a 2-digit hexadecimal value *)

    VAR buffer: ARRAY [0..1] OF CHAR;

    BEGIN
        HexToString (value, buffer);
        EditString (w, buffer, 2, 2);
        IF EditOK() THEN
            value := StringToHex (buffer);
        END (*IF*);
    END EditHexByte;

(************************************************************************)

PROCEDURE EditHexWord (w: Window;  VAR (*INOUT*) value: CARD16);

    (* Screen editing of a 4-digit hexadecimal value *)

    VAR buffer: ARRAY [0..3] OF CHAR;

    BEGIN
        HexToString (value, buffer);
        EditString (w, buffer, 4, 4);
        IF EditOK() THEN
            value := StringToHex (buffer);
        END (*IF*);
    END EditHexWord;

(************************************************************************)

PROCEDURE EditHexLongword (w: Window;  VAR (*INOUT*) value: CARD32);

    (* Screen editing of an 8-digit hexadecimal value *)

    VAR buffer: EightChar;

    BEGIN
        LongHexToString (value, buffer);
        EditString (w, buffer, 8, 8);
        IF EditOK() THEN
            value := StringToHex (buffer);
        END (*IF*);
    END EditHexLongword;

(************************************************************************)
(*                          DECIMAL INPUT                               *)
(************************************************************************)

PROCEDURE ReadCard (w: Window;  VAR (*OUT*) number: CARDINAL);

    (* Reads a decimal number, skipping over all non-numeric input.     *)

    VAR ch: CHAR;

    BEGIN
        number := 0;

        (* Skip over any leading non-numeric input.     *)

        WHILE NOT (LookaheadChar(w) IN Digits) DO
            ReadCharWithoutEcho (w, ch);
        END (*WHILE*);

        (* Now read and convert the desired input.      *)

        WHILE LookaheadChar(w) IN Digits DO
            ReadChar (w, ch);
            number := 10*number + ORD(ch) - ORD("0");
        END (*WHILE*);
    END ReadCard;

(*************************************************************************)

PROCEDURE ReadBufferedCardinal (w: Window;  fieldsize: CARDINAL): CARDINAL;

    (* Reads a decimal number.  The difference between this and         *)
    (* ReadCard is that the user is given a reverse-video field of a    *)
    (* fixed width to work in, and is able to use the cursor control    *)
    (* keys to edit within that field.                                  *)

    VAR buffer: ARRAY ColumnRange OF CHAR;
        result: CARDINAL;  row, column: CARDINAL;

    BEGIN
        SaveCursor (w, row, column);
        buffer := "";
        EditString (w, buffer, fieldsize, fieldsize);
        result := StringToCardinal (buffer);
        SetCursor (w, row, column);
        WriteRJCard (w, result, fieldsize);
        RETURN result;
    END ReadBufferedCardinal;

(*************************************************************************)

PROCEDURE EditCardinal (w: Window;  VAR (*INOUT*) value: CARDINAL;
                                                fieldsize: CARDINAL);

    (* Screen editing of a decimal number. *)

    VAR buffer: ARRAY ColumnRange OF CHAR;
        row, column: CARDINAL;

    BEGIN
        SaveCursor (w, row, column);
        CardinalToString (value, buffer, fieldsize);
        EditString (w, buffer, fieldsize, fieldsize);
        IF EditOK() THEN
            value := StringToCardinal (buffer);
        END (*IF*);
        SetCursor (w, row, column);
        WriteRJCard (w, value, fieldsize);
    END EditCardinal;

(*************************************************************************)

PROCEDURE EditShortCard (w: Window;  VAR (*INOUT*) value: CARD8;
                                                fieldsize: CARDINAL);

    (* Screen editing of a decimal number. *)

    VAR temp: CARDINAL;

    BEGIN
        temp := ORD (value);
        EditCardinal (w, temp, fieldsize);
        value := temp;
    END EditShortCard;

(*************************************************************************)

END NumericIO.

