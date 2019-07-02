(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE ProgName;

    (****************************************************************)
    (*                                                              *)
    (*       A module that lets low-level modules obtain            *)
    (*       application-specific information.  We achieve          *)
    (*       this by putting this module in a directory             *)
    (*       reserved for application-specific source files,        *)
    (*       while the library modules still live at                *)
    (*       "library" level of the source structure.               *)
    (*                                                              *)
    (*    Last edited:    1 June 2019                               *)
    (*    Status:         OK                                        *)
    (*                                                              *)
    (****************************************************************)


IMPORT Strings;

IMPORT WV;

(************************************************************************)

PROCEDURE GetProgramName (VAR (*OUT*) Version: ARRAY OF CHAR);

    (* Returns a version string.  We compute it in this module so that  *)
    (* exceptq can have a version string before any threads are created.*)

    VAR ProgVersion: ARRAY [0..31] OF CHAR;

    BEGIN
        Strings.Assign ("Weasel ", ProgVersion);
        Strings.Append (WV.version, ProgVersion);
        Strings.Assign (ProgVersion, Version);
    END GetProgramName;

(************************************************************************)

END ProgName.

