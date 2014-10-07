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

MODULE Decode64;

        (********************************************************)
        (*                                                      *)
        (*       Utility to translate Base64 to plain text      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            10 January 2008                 *)
        (*  Last edited:        10 January 2008                 *)
        (*  Status:             Completed                       *)
        (*                                                      *)
        (********************************************************)


IMPORT TextIO;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

FROM STextIO IMPORT
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM Base64 IMPORT
    (* type *)  Encode, Decode;

(************************************************************************)

PROCEDURE GetParameter (VAR (*OUT*) result: ARRAY OF CHAR): BOOLEAN;

    (* Picks up the program argument from the command line.  *)

    VAR args: ChanId;
        found: BOOLEAN;

    BEGIN
        args := ArgChan();
        found := IsArgPresent();
        IF found THEN
            TextIO.ReadString (args, result);
        END (*IF*);
        RETURN found;
    END GetParameter;

(************************************************************************)

PROCEDURE WriteStringB (str: ARRAY OF CHAR;  N: CARDINAL);

    (* Like WriteString, but we write out the characters one at a       *)
    (* time, for N characters, and we don't let a Nul stop us.          *)

    VAR j: CARDINAL;

    BEGIN
        IF N > 0 THEN
            FOR j := 0 TO N-1 DO
                WriteChar (str[j]);
            END (*FOR*);
        END (*IF*);
    END WriteStringB;

(************************************************************************)

PROCEDURE DecodeTest (code: ARRAY OF CHAR);

    (* Writes out data, and then its Base64 version. *)

    VAR ans: ARRAY [0..1023] OF CHAR;
        N: CARDINAL;

    BEGIN
        WriteString ('Encoded : ');
        WriteString (code);
        WriteLn;
        N := Decode (code, ans);
        WriteString ('Decoded : ');
        WriteStringB (ans, N);
        WriteLn;
    END DecodeTest;

(********************************************************************************)

PROCEDURE DoTheDecoding;

    (* Gets command-line string and decodes it.  *)

    VAR inputstring: ARRAY [0..255] OF CHAR;

    BEGIN
        IF GetParameter (inputstring) THEN
            DecodeTest (inputstring);
        ELSE
            WriteString ("Usage: Decode64 string");
            WriteLn;
        END (*IF*);
    END DoTheDecoding;

(********************************************************************************)

BEGIN
    WriteLn;
    DoTheDecoding;
END Decode64.

