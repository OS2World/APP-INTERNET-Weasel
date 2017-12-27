(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

IMPLEMENTATION MODULE SUFilters;

        (********************************************************)
        (*                                                      *)
        (*       'Filters' page in Weasel VIOSetup utility      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 September 2003               *)
        (*  Last edited:        22 May 2017                     *)
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
    (* proc *)  INIValid, INIGet, INIGetString, INIPut, INIPutString, INIDeleteKey;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  StringField, MenuField, CardinalField, Combine,
                ScreenEdit, DeleteStructure;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen;

FROM Names IMPORT
    (* type *)  FilenameString, HostName;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCard;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

(************************************************************************)

CONST
    MaxFilterNum = 4;

TYPE
    FilterNum = [0..MaxFilterNum];
    FilterString = ARRAY [0..15] OF CHAR;
    KeyArray = ARRAY FilterNum OF FilterString;

CONST
    FilterProgKey = KeyArray {'FilterProg0', 'FilterProg1', 'FilterProg2',
                              'FilterProg3', 'FilterProg4'};

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Post-reception filters. *)

    FilterProg: ARRAY FilterNum OF FilenameString;

    (* Option to serialize filter operations. *)

    SerialiseFilters: CARDINAL;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini". *)

    VAR hini: HINI;
        k: FilterNum;
        bool: CARD8;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: FilterString;

    (****************************************************************)

    PROCEDURE GetStringItem (name: ARRAY OF CHAR;
                         VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, SYSapp, name, variable);
        END GetStringItem;

    (****************************************************************)

    BEGIN
        SYSapp := "$SYS";
        SerialiseFilters := 2;
        hini := OurINIHandle();
        IF INIValid(hini) THEN

            (* Upgrade from old filter option. *)

            IF GetStringItem ('FilterProg', FilterProg[4]) THEN
                key := "FilterProg";
                INIDeleteKey (hini, SYSapp, key);
                key := FilterProgKey[4];
                INIPutString (hini, SYSapp, key, FilterProg[4]);
            ELSIF NOT GetStringItem (FilterProgKey[4], FilterProg[4]) THEN
                IF GetStringItem (FilterProgKey[2], FilterProg[4]) THEN
                    key := FilterProgKey[2];
                    INIDeleteKey (hini, SYSapp, key);
                    key := FilterProgKey[4];
                    INIPutString (hini, SYSapp, key, FilterProg[4]);
                END (*IF*);
            END (*IF*);

            (* Filters for each stage. *)

            FOR k := 0 TO MaxFilterNum DO
                IF NOT GetStringItem (FilterProgKey[k], FilterProg[k]) THEN
                    FilterProg[k] := "";
                END (*IF*);
            END (*FOR*);

            (* Serialise filters. *)

            key := "SerialiseFilters";
            EVAL (INIGet (hini, SYSapp, key, bool));
            IF bool = 0 THEN SerialiseFilters := 1 END(*IF*);

        END (*IF*);

    END LoadINIData;

(********************************************************************)

PROCEDURE StoreINIData;

    (* Writes data back to the INI file. *)

    VAR hini: HINI;
        k: FilterNum;
        SYSapp: ARRAY [0..4] OF CHAR;

    (****************************************************************)

    PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: ARRAY OF LOC);

        BEGIN
            INIPut (hini, SYSapp, name, variable);
        END PutItem;

    (****************************************************************)

    CONST Nul = CHR(0);
    VAR j: CARDINAL;  bool: CARD8;

    BEGIN
        SYSapp := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            bool := 0;
            IF SerialiseFilters > 1 THEN bool := 1 END(*IF*);
            PutItem ("SerialiseFilters", bool);
            IF bool = 0 THEN SerialiseFilters := 1 END(*IF*);

            FOR k := 0 TO MaxFilterNum DO
                WHILE FilterProg[k][0] = ' ' DO
                    Strings.Delete (FilterProg[k], 0, 1);
                END (*WHILE*);
                j := Strings.Length (FilterProg[k]);
                LOOP
                    IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
                    DEC (j);
                    IF FilterProg[k][j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                    FilterProg[k][j] := Nul;
                END (*LOOP*);
                PutItem (FilterProgKey[k], FilterProg[k]);
            END (*FOR*);

        END (*IF*);
    END StoreINIData;

(************************************************************************)

PROCEDURE EditParameters (OurPage: VirtualScreen);

    CONST row0 = 4;

    VAR w: Window;  M2: Menu;
        k: FilterNum;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;
        NoYes: ARRAY [0..2] OF ItemText;

    BEGIN
        OpenWindowHidden (w, white, blue, 2, ScreenRows - 2,
                                                  0, 79, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);
        LoadINIData;

        SetCursor (w, 1, 2);  WriteString (w, "Filters for incoming mail (optional)");
        SetCursor (w, row0-1, 2);  WriteString (w, "Filter 0: on initial connection");
        SetCursor (w, row0+2, 2);  WriteString (w, "Filter 1: after receiving HELO address");
        SetCursor (w, row0+5, 2);  WriteString (w, "Filter 2: after receiving MAIL FROM address");
        SetCursor (w, row0+8, 2);  WriteString (w, "Filter 3: after receiving recipient addresses");
        SetCursor (w, row0+11, 2);  WriteString (w, "Filter 4: after receiving message body");
        FOR k := 0 TO MaxFilterNum DO
            SetCursor (w, 3*k+row0, 3);  WriteChar (w, '[');
            SetCursor (w, 3*k+row0, 78);  WriteChar (w, ']');
        END (*FOR*);

        SetCursor (w, 18, 12);  WriteString (w, 'Serialize filter operations');

        NoYes[1] := "No";
        NoYes[2] := "Yes";
        CreateMenu (M2, 2, NoYes, 2);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        R := StringField (FilterProg[0], row0, 4, 74);
        FOR k := 1 TO MaxFilterNum DO
            Combine (R, StringField (FilterProg[k], 3*k+row0, 4, 74));
        END (*FOR*);
        Combine (R, MenuField (SerialiseFilters, 18, 42, 1, 20, M2));

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

PROCEDURE OptionEditor;

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
        WriteString (w0, "  FILTERS");
        WriteLn (w0);
        WriteString (w0, "   See WEASEL.INF for the definition of the different filter stages");

        EditParameters (OurPage);
        CloseWindow (w0);
        CloseWindow (bottombar);
        SetActivePage (0);
        StuffKeyboardBuffer (CHR(27));

    END OptionEditor;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    LoadINIData;
    EVAL(CreateTask (OptionEditor, 2, "Filter editor"));
FINALLY
    StoreINIData;
END SUFilters.

