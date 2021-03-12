(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2021   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE WV;

        (****************************************************************)
        (*                                                              *)
        (*                    Weasel version number                     *)
        (*                                                              *)
        (*    Started:        6 March 2004                              *)
        (*    Last edited:    12 February 2021                          *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT Strings;

(************************************************************************)

PROCEDURE year(): FourChar;

    (* Returns the year part of the date constant. *)

    VAR result: FourChar;

    BEGIN
        Strings.Assign (date, result);
        RETURN result;
    END year;

(************************************************************************)

END WV.

