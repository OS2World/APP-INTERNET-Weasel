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

IMPLEMENTATION MODULE SU2Options;

        (********************************************************)
        (*                                                      *)
        (*     'Options 2' page in Weasel VIOSetup utility      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 September 2003               *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC, CARD8, INT16;

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

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

(************************************************************************)

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* When to go online. *)

    OnlineOption: CARDINAL;

    (* Number of output threads. *)

    OutputThreads: CARDINAL;

    (* Stop on first match when identifying a POP3 user? *)

    SingleMatch: BOOLEAN;
    SingleMatchOption: CARDINAL;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini". *)

    VAR hini: HINI;  int16val: INT16;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..13] OF CHAR;

    BEGIN
        SYSapp := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN

            (* Online option. *)

            key := "OnlineOption";
            EVAL (INIGet (hini, SYSapp, key, OnlineOption));

            (* POP3 SingleMatch option. *)

            key := "SingleMatch";
            EVAL (INIGet (hini, SYSapp, key, SingleMatch));
            SingleMatchOption := ORD(SingleMatch) + 1;

            (* Number of outbound mail threads. *)

            key := "OutputThreads";
            IF NOT INIGet (hini, SYSapp, key, int16val) THEN
                int16val := 16;
            END (*IF*);
            IF int16val <= 0 THEN
                int16val := 1;
            ELSIF int16val > 64 THEN
                int16val := 64;
            END (*IF*);
            OutputThreads := int16val;

        END (*IF*);
        INC (OnlineOption);
    END LoadINIData;

(********************************************************************)

PROCEDURE StoreINIData;

    (* Writes data back to the INI file. *)

    VAR hini: HINI;
        SYSapp: ARRAY [0..4] OF CHAR;


    (****************************************************************)

    PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: ARRAY OF LOC);

        BEGIN
            INIPut (hini, SYSapp, name, variable);
        END PutItem;

    (****************************************************************)

    VAR int16val: INT16;  bool: BOOLEAN;

    BEGIN
        SYSapp := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            DEC (OnlineOption);
            PutItem ("OnlineOption", OnlineOption);
            bool := SingleMatchOption > 1;
            PutItem ("SingleMatch", bool);
            int16val := VAL(INT16,OutputThreads);
            PutItem ("OutputThreads", int16val);
        END (*IF*);
    END StoreINIData;

(************************************************************************)

PROCEDURE EditParameters (OurPage: VirtualScreen);

    VAR w: Window;  M2, M4: Menu;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;
        NoYes: ARRAY [0..2] OF ItemText;
        OnlineChoice: ARRAY [0..3] OF ItemText;

    (********************************************************************)

    BEGIN
        OpenWindowHidden (w, white, blue, 2, ScreenRows - 2,
                                                  0, 79, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);
        LoadINIData;

        SetCursor (w, 4, 2);  WriteString (w, "When to go online");
        SetCursor (w, 11, 2);  WriteString (w, "Identifying a POP3 user");
        SetCursor (w, 12, 4);  WriteString (w, "Accept only the first username/domain match");
        SetCursor (w, 17, 2);  WriteString (w, "Number of outbound mail threads (typical 16, max 64):");

        OnlineChoice[1] := " \ONLINE file";
        OnlineChoice[2] := " when \dialup detected";
        OnlineChoice[3] := " \always";
        CreateMenu (M4, 3, OnlineChoice, 3);
        MenuColours (M4, white, blue, blue, cyan, yellow, darkgrey);
        R := MenuField (OnlineOption, 5, 6, 1, 70, M4);

        NoYes[1] := "No";
        NoYes[2] := "Yes";
        CreateMenu (M2, 2, NoYes, 2);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (SingleMatchOption, 12, 55, 1, 10, M2));
        Combine (R, CardinalField (OutputThreads, 17, 60, 2));

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
        WriteString (w0, "  OPTIONS 2");
        WriteLn (w0);
        WriteString (w0, "   Don't enable any options you don't understand");

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
    EVAL(CreateTask (OptionEditor, 2, "Option 2 editor"));
FINALLY
    StoreINIData;
END SU2Options.

