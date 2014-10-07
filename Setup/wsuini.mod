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

IMPLEMENTATION MODULE WSUINI;

      (************************************************************)
      (*                                                          *)
      (*                    PM Setup for Weasel                   *)
      (*          This module reads/writes user INI data,         *)
      (*          either locally or remotely as requested         *)
      (*                                                          *)
      (*      Started:        01 July 1999                        *)
      (*      Last edited:    15 July 2013                        *)
      (*      Status:         OK                                  *)
      (*                                                          *)
      (************************************************************)

IMPORT Strings, RINIData;

FROM Names IMPORT
    (* type *)  FilenameString, UserName;

FROM SUDomains IMPORT
    (* proc *)  DeleteUser;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST Nul = CHR(0);

VAR INIFilename, CurrentMailRoot: FilenameString;
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
    END SetTNIMode;

(************************************************************************)

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

(************************************************************************)
(*                 OTHER EXTERNALLY CALLABLE PROCEDURES                 *)
(************************************************************************)

PROCEDURE OpenINIFile;

    BEGIN
        IF OpenCount = 0 THEN
           EVAL (RINIData.OpenINIFile (INIFilename, TNImode));
        END (*IF*);
        INC (OpenCount);
    END OpenINIFile;

(************************************************************************)

PROCEDURE CloseINIFile;

    BEGIN
        DEC (OpenCount);
        IF OpenCount = 0 THEN
           RINIData.CloseINIFile;
        END (*IF*);
    END CloseINIFile;

(************************************************************************)

PROCEDURE RemoveUser (user: UserName);

    (* Deletes one user from the currently active domain.  The user's   *)
    (* mail directory will also be deleted iff it is empty.             *)

    BEGIN
        OpenINIFile;
        DeleteUser (user, CurrentMailRoot);
        CloseINIFile;
    END RemoveUser;

(************************************************************************)

BEGIN
    CurrentMailRoot := "";
    INIFilename := "Weasel.INI";
    TNImode := FALSE;
    OpenCount := 0;
END WSUINI.

