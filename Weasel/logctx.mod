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

IMPLEMENTATION MODULE LogCtx;

        (********************************************************)
        (*                                                      *)
        (*      Defines an overall log context for Weasel       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            28 January 2009                 *)
        (*  Last edited:        28 January 2009                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT TransLog;

BEGIN
    WCtx := TransLog.OpenLogContext();
FINALLY
    TransLog.CloseLogContext (WCtx);
END LogCtx.

