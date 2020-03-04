(**************************************************************************)
(*                                                                        *)
(*  Weasel mail server                                                    *)
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

IMPLEMENTATION MODULE WINI;

        (************************************************************)
        (*                                                          *)
        (*                    Weasel mail server                    *)
        (*              Opens and closes the INI file               *)
        (*                                                          *)
        (*      Started:        3 December 2019                     *)
        (*      Last edited:    6 December 2019                     *)
        (*      Status:         OK                                  *)
        (*                                                          *)
        (*  At present this deals only with the global INI file.    *)
        (*  I still have to think about how to handle domain INI    *)
        (*  files, given that more than one might be open           *)
        (*  simultaneously.                                         *)
        (*                                                          *)
        (************************************************************)

IMPORT Strings, INIData;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM INIData IMPORT
    (* type *)  HINI;

(************************************************************************)

CONST Nul = CHR(0);

VAR INIFilename, CurrentMailRoot: FilenameString;
    hini: INIData.HINI;
    OpenCount: CARDINAL;
    TNImode: BOOLEAN;

(************************************************************************)
(*                SPECIFYING WHICH INI FILE TO WORK ON                  *)
(************************************************************************)

PROCEDURE SetTNIMode (useTNI: BOOLEAN);

    (* If the parameter is FALSE, we are going to work with INI files.  *)
    (* If it's TRUE, we will use TNI files.                             *)

    BEGIN
        TNImode := useTNI;
        Strings.Assign ("Weasel", INIFilename);
        IF TNImode THEN
            Strings.Append (".TNI", INIFilename);
        ELSE
            Strings.Append (".INI", INIFilename);
        END (*IF*);
    END SetTNIMode;

(************************************************************************)

PROCEDURE INIForDomain (DomainMailRoot: ARRAY OF CHAR;
                            VAR (*OUT*) INIname: ARRAY OF CHAR);

    (* Given the mail root for a domain (assumed to end with a '\'),    *)
    (* returns the name of the INI file.                                *)

    BEGIN
        Strings.Assign (DomainMailRoot, INIname);
        Strings.Append ("DOMAIN", INIname);
        IF TNImode THEN
            Strings.Append (".TNI", INIname);
        ELSE
            Strings.Append (".INI", INIname);
        END (*IF*);
    END INIForDomain;

(************************************************************************)

(*
PROCEDURE SetINIDirectory (GlobalMailRoot, subdir: ARRAY OF CHAR);

    (* If subdir = "", the INI file to be used will be WEASEL.INI in    *)
    (* our working directory.  Otherwise, the subdirectory 'subdir' of  *)
    (* GlobalMailRoot, will be used, and the file name is DOMAIN.INI.   *)
    (* In TNI mode the files are TNI files rather than INI files.       *)

    BEGIN
        Strings.Assign (GlobalMailRoot, CurrentMailRoot);
        IF subdir[0] = Nul THEN
            Strings.Assign ("Weasel", INIFilename);
        ELSE
            Strings.Append (subdir, CurrentMailRoot);
            Strings.Append ('\', CurrentMailRoot);
            Strings.Assign (CurrentMailRoot, INIFilename);
            Strings.Append ("DOMAIN", INIFilename);
        END (*IF*);
        IF TNImode THEN
            Strings.Append (".TNI", INIFilename);
        ELSE
            Strings.Append (".INI", INIFilename);
        END (*IF*);
    END SetINIDirectory;
*)

(************************************************************************)
(*                 OTHER EXTERNALLY CALLABLE PROCEDURES                 *)
(************************************************************************)

PROCEDURE OpenINI(): HINI;

    (* Opens Weasel's global INI file. *)

    BEGIN
        IF OpenCount = 0 THEN
            hini := INIData.OpenINIFile (INIFilename);
            IF INIData.INIValid(hini) THEN
                INC (OpenCount);
            END (*IF*);
        END (*IF*);
        RETURN hini;
    END OpenINI;

(************************************************************************)

PROCEDURE CloseINI;

    (* Closes Weasel's global INI file. *)

    BEGIN
        IF OpenCount > 0 THEN
            DEC (OpenCount);
            IF OpenCount = 0 THEN
               INIData.CloseINIFile(hini);
            END (*IF*);
        END (*IF*);
    END CloseINI;

(************************************************************************)

BEGIN
    CurrentMailRoot := "";
    INIFilename := "Weasel.INI";
    TNImode := FALSE;
    OpenCount := 0;
END WINI.

