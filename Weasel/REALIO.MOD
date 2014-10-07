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

IMPLEMENTATION MODULE RealIO;

        (********************************************************)
        (*                                                      *)
        (*              Real I/O using windows.                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        13 May 1998                     *)
        (*  Status:             Working                         *)
        (*      More care needed in handling the case where     *)
        (*       the field size is too small.                   *)
        (*      Seems to be a loss of accuracy when writing     *)
        (*       E-format numbers; for example 123.456E7 is     *)
        (*       written as 1234559999.  I'm not yet sure       *)
        (*       where this is happening, but suspect that it   *)
        (*       involves some deep error analysis.             *)
        (*                                                      *)
        (********************************************************)

FROM Windows IMPORT
    (* type *)  Window, ColumnRange,
    (* proc *)  SaveCursor, SetCursor, EditString, ReadChar, LookaheadChar,
                ReadCharWithoutEcho, WriteString;

FROM Conversions IMPORT
    (* proc *)  LongRealToString, StringToLongReal;

FROM Keyboard IMPORT
    (* proc *)  InKey, PutBack;

(************************************************************************)

CONST
    EndMarker = CHR(0);  tab = CHR(9);  CR = CHR(13);

TYPE
    CharSet = SET OF CHAR;

(************************************************************************)
(*                              REAL OUTPUT                             *)
(************************************************************************)

PROCEDURE WriteLongReal (w: Window;  number: LONGREAL;  places: CARDINAL);

    (* Writes the second argument as a decimal number, right-justified  *)
    (* in a field of "places" places.                                   *)

    VAR buffer: ARRAY [0..79] OF CHAR;

    BEGIN
        LongRealToString (number, buffer, places);
        WriteString (w, buffer);
    END WriteLongReal;

(************************************************************************)

PROCEDURE WriteReal (w: Window;  number: REAL;  places: CARDINAL);

    (* Like WriteLongReal, except for argument type.    *)

    BEGIN
        WriteLongReal (w, VAL(LONGREAL,number), places);
    END WriteReal;

(************************************************************************)
(*                      REAL INPUT FROM KEYBOARD                        *)
(************************************************************************)

PROCEDURE ReadRealString (w: Window;  VAR (*OUT*) string: ARRAY OF CHAR);

    (* Reads in a character string from the keyboard, stopping at the   *)
    (* first character which cannot form part of the external           *)
    (* representation of an unsigned real number.  Leading blanks are   *)
    (* skipped and not echoed.                                          *)

    CONST Blanks = CharSet {" ", tab, CR};

    VAR nextchar: CHAR;
        position: CARDINAL;

    (********************************************************************)

    PROCEDURE GetNextChar;

        (* Stores nextchar, reads new nextchar from keyboard.  Returns  *)
        (* result = EndMarker if we run out of space.                   *)

        BEGIN
            IF position <= HIGH(string) THEN
                ReadChar (w, nextchar);
                string[position] := nextchar;  INC (position);
                nextchar := LookaheadChar(w);
            ELSE
                nextchar := EndMarker;
            END (*IF*);
        END GetNextChar;

    (********************************************************************)

    BEGIN
        position := 0;

        (* Skip leading spaces and tabs.        *)

        WHILE LookaheadChar(w) IN Blanks DO
            ReadCharWithoutEcho (w, nextchar);
        END (*WHILE*);
        nextchar := LookaheadChar(w);

        (* Read the sign, if present.  We also permit spaces or tabs    *)
        (* after the sign.                                              *)

        IF (nextchar = '-') OR (nextchar = '+') THEN
            REPEAT
                GetNextChar;
            UNTIL (nextchar <> " ") AND (nextchar <> tab);
        END (*IF*);

        (* Read the part before the decimal point.      *)

        WHILE nextchar IN CharSet {"0".."9"} DO
            GetNextChar;
        END (*WHILE*);

        (* Now the part after the decimal point, if any.        *)

        IF nextchar = "." THEN
            GetNextChar;
            WHILE nextchar IN CharSet {"0".."9"} DO
                GetNextChar;
            END (*WHILE*);
        END (*IF*);

        (* Check for Ennn part. *)

        IF (nextchar = "E") OR (nextchar = "e") THEN
            GetNextChar;
            IF (nextchar = "+") OR (nextchar = "-") THEN
                GetNextChar;
            END (*IF*);
            WHILE nextchar IN CharSet {"0".."9"} DO
                GetNextChar;
            END (*WHILE*);
        END (*IF*);

        (* Ensure that string is properly terminated.  *)

        IF position <= HIGH(string) THEN
            string[position] := EndMarker;
        END (*IF*);

    END ReadRealString;

(************************************************************************)

PROCEDURE ReadLongReal (w: Window): LONGREAL;

    (* Reads and converts an unsigned numeric string from the keyboard. *)

    VAR InputString: ARRAY [0..79] OF CHAR;

    BEGIN
        ReadRealString (w, InputString);
        RETURN StringToLongReal (InputString);
    END ReadLongReal;

(************************************************************************)

PROCEDURE ReadReal (w: Window): REAL;

    (* Like ReadLongReal, except for argument type.     *)

    BEGIN
        RETURN VAL(REAL, ReadLongReal (w));
    END ReadReal;

(************************************************************************)

PROCEDURE ReadBufferedLongReal (w: Window;  fieldsize: CARDINAL): LONGREAL;

    (* Like ReadLongReal, but allows the user to edit within a field of *)
    (* the specified size.                                              *)

    VAR buffer: ARRAY ColumnRange OF CHAR;
        row, column: CARDINAL;
        value: LONGREAL;

    BEGIN
        SaveCursor (w, row, column);
        buffer := "";
        EditString (w, buffer, fieldsize, fieldsize);
        value := StringToLongReal (buffer);
        SetCursor (w, row, column);
        WriteLongReal (w, value, fieldsize);
        RETURN value;
    END ReadBufferedLongReal;

(************************************************************************)

PROCEDURE ReadBufferedReal (w: Window;  fieldsize: CARDINAL): REAL;

    (* Like ReadBufferedLongReal, except for argument type.     *)

    BEGIN
        RETURN VAL(REAL, ReadBufferedLongReal (w, fieldsize));
    END ReadBufferedReal;

(************************************************************************)

PROCEDURE EditLongReal (w: Window;  VAR (*INOUT*) variable: LONGREAL;
                                                        width: CARDINAL);

    (* Displays the current value of "variable" at the current cursor   *)
    (* position in window w, using a field width of "width" characters, *)
    (* and gives the user the option of altering the value.             *)

    CONST Esc = CHR(27);

    VAR buffer: ARRAY ColumnRange OF CHAR;
        row, column: CARDINAL;  ch: CHAR;

    BEGIN
        SaveCursor (w, row, column);
        LongRealToString (variable, buffer, width);
        EditString (w, buffer, width, width);
        ch := InKey();  PutBack(ch);
        IF ch <> Esc THEN
            variable := StringToLongReal (buffer);
        END (*IF*);
        SetCursor (w, row, column);
        WriteLongReal (w, variable, width);
    END EditLongReal;

(************************************************************************)

PROCEDURE EditReal (w: Window;  VAR (*INOUT*) variable: REAL;
                                                        width: CARDINAL);

    (* Like EditLongReal, except for argument type.     *)

    VAR temp: LONGREAL;

    BEGIN
        temp := VAL(LONGREAL,variable);
        EditLongReal (w, temp, width);
        variable := VAL(REAL,temp);
    END EditReal;

(************************************************************************)

END RealIO.

