(**************************************************************************)
(*                                                                        *)
(*  Utility for use with the Weasel mail server                           *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

MODULE ChooseTNI;

        (********************************************************)
        (*                                                      *)
        (*  Utility to set the value of UseTNI in Weasel.INI    *)
        (*  and/or Weasel.TNI, if they exist.  The main point   *)
        (*  of this program is to make sure that, in the case   *)
        (*  where both files exist, they are consistent with    *)
        (*  each other in having the same UseTNI value.         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        17 August 2017                  *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


IMPORT TextIO;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  OpenINIFile, CloseINIFile, INIValid, INIPut;

FROM FileSys IMPORT
    (* proc *)  Exists;

FROM IOChan IMPORT
    (* type *)  ChanId;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

(************************************************************************)
(*                         COMMAND LINE ARGUMENT                        *)
(************************************************************************)

PROCEDURE GetParameter(): CHAR;

    (* Returns 'N', 'Y', or '?'.  *)

    TYPE CharSet = SET OF CHAR;

    VAR j: CARDINAL;  ch: CHAR;
        args: ChanId;
        ParameterString: ARRAY [0..79] OF CHAR;

    BEGIN
        args := ArgChan();
        IF IsArgPresent() THEN
            TextIO.ReadString (args, ParameterString);
            j := 0;
            ch := ParameterString[0];
            WHILE ch IN CharSet {' ', '-', '/'} DO
                INC (j);
                ch := ParameterString[j];
            END (*WHILE*);
        ELSE
            ch := CHR(0);
        END (*IF*);

        IF ch IN CharSet {'0', 'n', 'N'} THEN
            ch := 'N';
        ELSIF ch IN CharSet {'1', 'y', 'Y'} THEN
            ch := 'Y';
        ELSE
            ch := '?';
        END (*IF*);
        RETURN ch;

    END GetParameter;

(************************************************************************)
(*                            SETTING THE VALUE                         *)
(************************************************************************)

PROCEDURE SetValue (val: BOOLEAN);

    (* Sets "val" as the value of ($SYS, UseTNI) in both INI files, *)
    (* if they exist.                                               *)

    VAR hini: HINI;
        haveINI, haveTNI: BOOLEAN;
        present, missing: ARRAY [0..3] OF CHAR;
        app, key: ARRAY [0..7] OF CHAR;
        name: ARRAY [0..15] OF CHAR;

    BEGIN
        haveINI := Exists("Weasel.INI");
        haveTNI := Exists("Weasel.TNI");

        IF haveINI OR haveTNI THEN
            WriteString ("Setting UseTNI to ");
            IF val THEN
                WriteString ("TRUE");
            ELSE
                WriteString ("FALSE");
            END (*IF*);
        ELSE
            WriteLn;
            WriteString ("This setting has no effect, because neither WEASEL.INI nor WEASEL.TNI");
            WriteString (" exists.");
        END (*IF*);
        WriteLn;

        (* Extra explanatory message for the case where only one of the *)
        (* files exists.                                                *)

        IF (haveINI <> haveTNI) AND (val <> haveTNI) THEN
            IF haveTNI THEN
                present := "TNI";
                missing := "INI";
            ELSE
                present := "INI";
                missing := "TNI";
            END (*IF*);
            WriteString ("Note that Weasel.");
            WriteString (present);
            WriteString (" will be used anyway, regardless of this setting,");
            WriteLn;
            WriteString ("because WEASEL.");
            WriteString (missing);
            WriteString (" does not exist.");
            WriteLn;
        END (*IF*);

        app := "$SYS";  key := "UseTNI";

        IF haveINI THEN
            name := "Weasel.INI";
            hini := OpenINIFile (name, FALSE);
            IF INIValid(hini) THEN
                INIPut (hini, app, key, val);
                CloseINIFile (hini);
            END (*IF*);
        END (*IF*);

        IF haveTNI THEN
            name := "Weasel.TNI";
            hini := OpenINIFile (name, TRUE);
            IF INIValid(hini) THEN
                INIPut (hini, app, key, val);
                CloseINIFile (hini);
            END (*IF*);
        END (*IF*);

    END SetValue;

(************************************************************************)

VAR code: CHAR;

BEGIN
    code := GetParameter();
    IF code = 'Y' THEN SetValue(TRUE);
    ELSIF code = 'N' THEN SetValue(FALSE);
    ELSE
        WriteLn;
        WriteString ("Usage:");  WriteLn;
        WriteString ("    ChooseTNI Y");  WriteLn;
        WriteString ("          to set a preference for WEASEL.TNI");
        WriteLn;
        WriteString ("    ChooseTNI N");  WriteLn;
        WriteString ("          to set a preference for WEASEL.INI");
        WriteLn;  WriteLn;
        WriteString ("Note however that if only one of those files exists,");
        WriteLn;
        WriteString ("then the one that exists is the one that will be used.");
        WriteLn;
    END (*IF*);
END ChooseTNI.

