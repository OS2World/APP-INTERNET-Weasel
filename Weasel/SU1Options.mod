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

IMPLEMENTATION MODULE SU1Options;

        (********************************************************)
        (*                                                      *)
        (*     'Options 1' page in Weasel VIOSetup utility      *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 September 2003               *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC, CARD8;

IMPORT Strings;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND;

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

TYPE
    AuthKind = (cheat, plain, login, crammd5);
    AuthArray = ARRAY AuthKind OF CARDINAL;

CONST
    AuthMask = AuthArray {1, 2, 4, 8};
    DefaultAuthMethods = AuthMask[plain]+AuthMask[login]+AuthMask[crammd5];

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Also apply host tests to MAIL FROM address? *)

    MAILFROMcheckLevel: CARDINAL;

    (* Authentication time (minutes) for POP-before-SMTP. *)

    AuthTime: CARDINAL;

    (* Flags (1=NO, 2=YES) for SMTP AUTH methods. *)

    AuthLevel: ARRAY AuthKind OF CARDINAL;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini". *)

    VAR hini: HINI;
        val: CARDINAL;
        j: AuthKind;
        MAILFROMchecking: BOOLEAN;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..13] OF CHAR;

    BEGIN
        AuthTime := 0;
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            app := "$SYS";
            key := "MAILFROMcheck";
            IF INIGet (hini, app, key, MAILFROMchecking) THEN
                MAILFROMcheckLevel := ORD(MAILFROMchecking) + 1;
            ELSE
                MAILFROMcheckLevel := 1;
            END (*IF*);
            key := "AuthTime";
            EVAL (INIGet (hini, app, key, AuthTime));

            (* SMTP AUTH authentication. *)

            key := "AuthMethods";
            IF NOT INIGet (hini, app, key, val) THEN
                val := DefaultAuthMethods;
            END (*IF*);
            FOR j := MIN(AuthKind) TO MAX(AuthKind) DO
                AuthLevel[j] := ORD(IAND (val, AuthMask[j]) <> 0) + 1;
            END (*FOR*);

        END (*IF*);
    END LoadINIData;

(********************************************************************)

PROCEDURE StoreINIData;

    (* Writes data back to the INI file. *)

    VAR hini: HINI;
        AuthMethods: CARDINAL;
        MAILFROMchecking: BOOLEAN;
        SYSapp: ARRAY [0..4] OF CHAR;

    (****************************************************************)

    PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: ARRAY OF LOC);

        BEGIN
            INIPut (hini, SYSapp, name, variable);
        END PutItem;

    (****************************************************************)

    VAR k: AuthKind;

    BEGIN
        SYSapp := "$SYS";
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            PutItem ("AuthTime", AuthTime);
            MAILFROMchecking := MAILFROMcheckLevel > 1;
            PutItem ("MAILFROMcheck", MAILFROMchecking);

            (* SMTP AUTH authentication. *)

            AuthMethods := 0;
            FOR k := MIN(AuthKind) TO MAX(AuthKind) DO
                IF (AuthLevel[k] > 1) THEN
                    INC (AuthMethods, AuthMask[k]);
                END (*IF*);
            END (*FOR*);
            PutItem ('AuthMethods', AuthMethods);

        END (*IF*);
    END StoreINIData;

(************************************************************************)

PROCEDURE EditParameters (OurPage: VirtualScreen);

    VAR w: Window;  M2: Menu;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;
        NoYes: ARRAY [0..2] OF ItemText;
        k: AuthKind;

    (********************************************************************)

    BEGIN
        OpenWindowHidden (w, white, blue, 2, ScreenRows - 2,
                                                  0, 79, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);
        LoadINIData;

        SetCursor (w, 4, 2);  WriteString (w, "Apply host tests to MAIL FROM address");
        SetCursor (w, 9, 2);  WriteString (w, "Authentication for relay mail (optional)");
        SetCursor (w, 11, 4);  WriteString (w, "POP login authenticates SMTP for xxxxxx minutes");
        SetCursor (w, 13, 4);  WriteString (w, "Allow SMTP AUTH authentication with");
        SetCursor (w, 14, 8);  WriteString (w, "PLAIN");
        SetCursor (w, 15, 8);  WriteString (w, "LOGIN");
        SetCursor (w, 16, 8);  WriteString (w, "CRAM-MD5");

        NoYes[1] := "No";
        NoYes[2] := "Yes";
        CreateMenu (M2, 2, NoYes, 2);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        R := MenuField (MAILFROMcheckLevel, 4, 42, 1, 20, M2);
        Combine (R, CardinalField (AuthTime, 11, 37, 6));
        FOR k := VAL(AuthKind,ORD(MIN(AuthKind))+1) TO MAX(AuthKind) DO
            Combine (R, MenuField (AuthLevel[k], 13+ORD(k), 32, 1, 10, M2));
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
        WriteString (w0, "  OPTIONS 1");
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
    EVAL(CreateTask (OptionEditor, 2, "Option editor"));
FINALLY
    StoreINIData;
END SU1Options.

