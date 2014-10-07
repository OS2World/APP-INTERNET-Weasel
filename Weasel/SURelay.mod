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

IMPLEMENTATION MODULE SURelay;

        (********************************************************)
        (*                                                      *)
        (*       'Relay' page in Weasel VIOSetup utility        *)
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
    (* type *)  FilenameString, HostName, UserName, PassString;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

(************************************************************************)

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Forward relay host. *)

    ForwardRelayHost: HostName;

    (* Should we use the forward relay host? *)

    RelayOption: CARDINAL;

    (* Authentification for relay mail. *)

    AuthOption: CARDINAL;

    (* Username and password for authentication. *)

    AuthUser: UserName;
    AuthPass: PassString;
    AuthPOPhost: HostName;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE LoadINIData;

    (* Loads setup parameters from "weasel.ini". *)

    VAR hini: HINI;
        SYSapp: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..10] OF CHAR;

    (****************************************************************)

    PROCEDURE GetStringItem (name: ARRAY OF CHAR;
                         VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

        BEGIN
            RETURN INIGetString (hini, SYSapp, name, variable);
        END GetStringItem;

    (****************************************************************)

    BEGIN
        SYSapp := "$SYS";
        ForwardRelayHost := "";
        RelayOption := 0;
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            EVAL (GetStringItem ("ForwardRelay", ForwardRelayHost));
            key := "RelayOption";
            EVAL (INIGet (hini, SYSapp, key, RelayOption));

            (* Authentication option for outgoing mail. *)

            key := "AuthOption";
            IF NOT INIGet (hini, SYSapp, key, AuthOption) THEN
                AuthOption := 0;
            END (*IF*);
            INC (AuthOption);

            (* Authentication user, password, and POP hostname. *)

            IF NOT GetStringItem ('AuthUser', AuthUser) THEN
                AuthUser := "";
            END (*IF*);

            IF NOT GetStringItem ('AuthPass', AuthPass) THEN
                AuthPass := "";
            END (*IF*);

            IF NOT GetStringItem ('AuthPOPhost', AuthPOPhost) THEN
                AuthPOPhost := "";
            END (*IF*);
        END (*IF*);
        INC (RelayOption);

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

    CONST Nul = CHR(0);
    VAR j: CARDINAL;

    BEGIN
        SYSapp := "$SYS";
        WHILE ForwardRelayHost[0] = ' ' DO
            Strings.Delete (ForwardRelayHost, 0, 1);
        END (*WHILE*);
        j := Strings.Length (ForwardRelayHost);
        LOOP
            IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
            DEC (j);
            IF ForwardRelayHost[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
            ForwardRelayHost[j] := Nul;
        END (*LOOP*);
        IF RelayOption > 0 THEN
            DEC (RelayOption);
        END (*IF*);
        IF AuthOption > 0 THEN
            DEC (AuthOption);
        END (*IF*);
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            PutItem ("ForwardRelay", ForwardRelayHost);
            PutItem ("RelayOption", RelayOption);
            PutItem ("AuthOption", AuthOption);
            PutItem ("AuthUser", AuthUser);
            PutItem ("AuthPass", AuthPass);
            PutItem ("AuthPOPhost", AuthPOPhost);
        END (*IF*);
    END StoreINIData;

(************************************************************************)

PROCEDURE EditParameters (OurPage: VirtualScreen);

    VAR w: Window;  M4, M5: Menu;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;
        NoYes: ARRAY [0..2] OF ItemText;
        RelayChoice, AuthChoice: ARRAY [0..3] OF ItemText;

    (********************************************************************)

    BEGIN
        OpenWindowHidden (w, white, blue, 2, ScreenRows - 2,
                                                  0, 79, noframe, nodivider);
        MapToVirtualScreen (w, OurPage);
        LoadINIData;

        SetCursor (w, 6, 2);  WriteString (w, "Use relay host for forwarding mail");
        SetCursor (w, 8, 2);  WriteChar (w, '[');
        SetCursor (w, 8, 68);  WriteChar (w, ']');
        SetCursor (w, 11, 2);  WriteString (w, "Authentication for outgoing mail");
        SetCursor (w, 14, 2);  WriteString (w, "Username");
        SetCursor (w, 15, 2);  WriteString (w, "Password");
        SetCursor (w, 16, 2);  WriteString (w, "POP host");

        NoYes[1] := "No";
        NoYes[2] := "Yes";

        RelayChoice[1] := " \never";
        RelayChoice[2] := " as \backup";
        RelayChoice[3] := " \always";
        CreateMenu (M4, 3, RelayChoice, 3);
        MenuColours (M4, white, blue, blue, cyan, yellow, darkgrey);
        R := MenuField (RelayOption, 7, 10, 1, 40, M4);
        Combine (R, StringField (ForwardRelayHost, 8, 3, 65));

        AuthChoice[1] := " \none";
        AuthChoice[2] := " \SMTP AUTH";
        AuthChoice[3] := " \POP before SMTP";
        CreateMenu (M5, 3, AuthChoice, 3);
        MenuColours (M5, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (AuthOption, 12, 10, 1, 60, M5));
        Combine (R, StringField (AuthUser, 14, 14, 65));
        Combine (R, StringField (AuthPass, 15, 14, 32));
        Combine (R, StringField (AuthPOPhost, 16, 14, 65));

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
        WriteString (w0, "  RELAY HOST FOR OUTGOING MAIL");
        WriteLn (w0);
        WriteString (w0, "   You can send mail directly, or relay it through another server");

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
END SURelay.

