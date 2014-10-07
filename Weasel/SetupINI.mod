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

IMPLEMENTATION MODULE SetupINI;

        (********************************************************)
        (*                                                      *)
        (*            Opens/closes Weasel INI file              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 April 1998                   *)
        (*  Last edited:        14 April 2012                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CloseINIFile;

(********************************************************************************)

VAR hini: HINI;

(********************************************************************************)

PROCEDURE OurINIHandle(): HINI;

    (* Returns the handle for our INI file, which this module has already opened. *)

    BEGIN
        RETURN hini;
    END OurINIHandle;

(********************************************************************************)

VAR name: ARRAY [0..10] OF CHAR;

BEGIN
    name := "Weasel.ini";
    hini := OpenINIFile (name, FALSE);
FINALLY
    CloseINIFile (hini);
END SetupINI.

