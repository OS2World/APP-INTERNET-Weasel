(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
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

IMPLEMENTATION MODULE OneLine;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for Weasel                   *)
        (*            Dialogue to edit a one-line string            *)
        (*                                                          *)
        (*    Started:        11 July 1999                          *)
        (*    Last edited:    29 September 2019                     *)
        (*    Status:         Complete                              *)
        (*                                                          *)
        (************************************************************)

IMPORT SYSTEM, OS2, DID, Strings, INIData;

(************************************************************************)
(*                   THE EDIT-ONE-STRING DIALOGUE                       *)
(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  title: ARRAY OF CHAR;
                       VAR (*INOUT*) item: ARRAY OF CHAR;
                       UseTNI: BOOLEAN);

    (* Edits the one-line string "item". *)

    VAR hwnd: OS2.HWND;
        INIFileName: ARRAY [0..15] OF CHAR;

    BEGIN
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, (* parent *)
                       owner,                    (* owner *)
                       OS2.WinDefDlgProc,  (* dialogue procedure *)
                       0,                  (* use resources in EXE *)
                       DID.GetOneLine,     (* dialogue ID *)
                       NIL);               (* creation parameters *)
        INIData.SetInitialWindowPosition (hwnd, INIFileName, "OneLine");
        OS2.WinSetWindowText (hwnd, title);
        OS2.WinSetDlgItemText (hwnd, DID.NameForAlias, item);

        OS2.WinProcessDlg(hwnd);
        OS2.WinQueryDlgItemText (hwnd, DID.NameForAlias, SIZE(item), item);
        INIFileName := "Setup.";
        IF UseTNI THEN
            Strings.Append ("TNI", INIFileName);
        ELSE
            Strings.Append ("INI", INIFileName);
        END (*IF*);
        INIData.StoreWindowPosition (hwnd, INIFileName, "OneLine");
        OS2.WinDestroyWindow (hwnd);
    END Edit;

(************************************************************************)

END OneLine.

