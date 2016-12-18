(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
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

IMPLEMENTATION MODULE Misc;

        (********************************************************)
        (*                                                      *)
        (*         Miscellaneous definitions for WSU use        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            4 July 1999                     *)
        (*  Last edited:        21 May 2002                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, OS2;

FROM Conversions IMPORT
    (* proc *)  CardinalToString, StringToCardinal;

(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)

PROCEDURE WinSetDlgItemCard (hwnd: OS2.HWND;  idItem, value: CARDINAL);

    (* Sets a cardinal field in a dialogue. *)

    VAR Buffer: ARRAY [0..15] OF CHAR;  j: CARDINAL;

    BEGIN
        CardinalToString (value, Buffer, 15);
        Buffer[15] := Nul;
        j := 0;
        WHILE Buffer[j] = ' ' DO INC(j) END(*WHILE*);
        IF j > 0 THEN
            Strings.Delete (Buffer, 0, j);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, idItem, Buffer);
    END WinSetDlgItemCard;

(**************************************************************************)

PROCEDURE WinQueryDlgItemCard (hwnd: OS2.HWND;  idItem: CARDINAL;
                                 VAR (*OUT*) result: CARDINAL);

    (* Reads back the value in a cardinal field in a dialogue. *)

    VAR Buffer: ARRAY [0..15] OF CHAR;

    BEGIN
        OS2.WinQueryDlgItemText (hwnd, idItem, 15, Buffer);
        result := StringToCardinal (Buffer);
    END WinQueryDlgItemCard;

(********************************************************************************)


END Misc.

