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

IMPLEMENTATION MODULE Blacklists;

        (********************************************************)
        (*                                                      *)
        (*      'Blacklists' page in Weasel set-up utility      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            4 November 2001                 *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC, CARD8;

IMPORT Strings;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, WriteString, WriteLn,
                WriteChar, SetCursor, SetActivePage, GetKey, GetScreenSize;

FROM Menus IMPORT
    (* type *)  Menu, ItemText,
    (* proc *)  CreateMenu, MenuColours;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIGetString, INIPut;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  StringField, MenuField, Combine,
                ScreenEdit, DeleteStructure;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen;

FROM Names IMPORT
    (* type *)  FilenameString, HostName;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

(************************************************************************)

TYPE
    DomainNumber = [1..4];
    StringArray = ARRAY DomainNumber OF HostName;

CONST
    DomainLabel = StringArray {'RBLDomain1', 'RBLDomain2', 'RBLDomain3',
                              'RBLDomain4'};
    DefaultDomain = StringArray {"blackholes.mail-abuse.org",
                                 "dialups.mail-abuse.org",
                                 "relays.mail-abuse.org", ""};

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Blacklist checkers that are enabled. *)

    Enabled: ARRAY DomainNumber OF CARDINAL;

    (* The blacklist domains. *)

    Domain: ARRAY DomainNumber OF HostName;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini". *)

    VAR hini: HINI;
        val: CARD8;
        j: DomainNumber;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..8] OF CHAR;

    (****************************************************************)

    PROCEDURE GetStringItem (name: ARRAY OF CHAR;
                         VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, app, name, variable);
        END GetStringItem;

    (****************************************************************)

    BEGIN
        app := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN

            (* Blacklist domains. *)

            key := "RBLcheck";
            IF NOT INIGet (hini, app, key, val) THEN
                val := 0;
            END (*IF*);
            FOR j := MIN(DomainNumber) TO MAX(DomainNumber) DO
                Enabled[j] := val MOD 2 + 1;
                val := val DIV 2;
                IF NOT GetStringItem (DomainLabel[j], Domain[j]) THEN
                    Domain[j] := DefaultDomain[j];
                END (*IF*);
            END (*FOR*);

        END (*IF*);
    END LoadINIData;

(********************************************************************)

PROCEDURE StoreINIData;

    (* Writes data back to the INI file. *)

    VAR hini: HINI;
        app: ARRAY [0..4] OF CHAR;

    (****************************************************************)

    PROCEDURE SpaceStrip (VAR (*INOUT*) str: ARRAY OF CHAR);

        (* Removes leading and trailing spaces from str. *)

        CONST Nul = CHR(0);
        VAR j: CARDINAL;

        BEGIN
            j := 0;
            WHILE str[j] = ' ' DO
               INC(j);
            END(*WHILE*);
            IF j > 0 THEN
                Strings.Delete (str, 0, j);
            END (*IF*);
            j := LENGTH(str);
            WHILE (j > 0) AND (str[j-1] = ' ') DO
               DEC(j);
            END(*WHILE*);
            str[j] := Nul;
        END SpaceStrip;

    (****************************************************************)

    PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: ARRAY OF LOC);

        BEGIN
            INIPut (hini, app, name, variable);
        END PutItem;

    (****************************************************************)

    CONST Nul = CHR(0);
    VAR j: DomainNumber;  flag: CARD8;

    BEGIN
        app := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN

            flag := 0;

            FOR j := MIN(DomainNumber) TO MAX(DomainNumber) DO
                flag := 2*flag + Enabled[j] - 1;
                SpaceStrip (Domain[j]);
                PutItem (DomainLabel[j], Domain[j]);
            END (*FOR*);

        END (*IF*);

        PutItem ('RBLcheck', flag);

    END StoreINIData;

(************************************************************************)

PROCEDURE EditParameters (OurPage: VirtualScreen);

    VAR w: Window;  M1: Menu;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;
        EnableOption: ARRAY [0..2] OF ItemText;
        j: DomainNumber;

    (********************************************************************)

    BEGIN
        OpenWindowHidden (w, white, blue, 2, ScreenRows - 2,
                                                  0, 79, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);

        FOR j := 1 TO MAX(DomainNumber) DO
            SetCursor (w, 3*j+1, 2);  WriteChar (w, '[');
            SetCursor (w, 3*j+1, 78);  WriteChar (w, ']');
        END (*FOR*);

        EnableOption[1] := "Disabled";
        EnableOption[2] := "Enabled";
        CreateMenu (M1, 2, EnableOption, 2);
        MenuColours (M1, white, blue, blue, cyan, yellow, darkgrey);

        R := MenuField (Enabled[1], 3, 2, 1, 20, M1);
        Combine (R, StringField (Domain[1], 4, 3, 75));
        FOR j := 2 TO MAX(DomainNumber) DO
            Combine (R, MenuField (Enabled[j], 3*j, 2, 1, 20, M1));
            Combine (R, StringField (Domain[j], 3*j+1, 3, 75));
        END (*FOR*);

        SetCursor (w, 1, 32);
        LOOP
            ScreenEdit (w, R, abort);
            IF abort THEN EXIT(*LOOP*) END(*IF*);

            (* Consume the character that took us off the edge. *)

            ch := GetKey (w);
            IF ch = CHR(0) THEN
                EVAL (GetKey (w));
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        CloseWindow (w);

    END EditParameters;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

PROCEDURE BlacklistEditor;

    CONST Esc = CHR(1BH);

    VAR w0, bottombar: Window;
        OurGroup: ScreenGroup;
        OurPage: VirtualScreen;

    BEGIN
        OurGroup := CreateScreenGroup (1);
        OurPage := CreateVirtualScreen (OurGroup);
        OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        WriteString (bottombar, " Esc exit");
        SetCursor (bottombar, 0, 55);
        WriteString (bottombar, "F4,F5 previous/next page");
        MapToVirtualScreen (bottombar, OurPage);

        OpenWindowHidden (w0, yellow, red, 0, 1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (w0, OurPage);
        WriteString (w0, "  BLACKLISTS");
        WriteLn (w0);
        WriteString (w0, "   You may enable up to four blacklist checkers");

        EditParameters (OurPage);
        CloseWindow (w0);
        CloseWindow (bottombar);
        SetActivePage (0);
        StuffKeyboardBuffer (CHR(27));

    END BlacklistEditor;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    LoadINIData;
    EVAL(CreateTask (BlacklistEditor, 2, "Blacklist editor"));
FINALLY
    StoreINIData;
END Blacklists.

