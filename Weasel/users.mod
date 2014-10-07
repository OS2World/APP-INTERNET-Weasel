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

IMPLEMENTATION MODULE Users;

        (********************************************************)
        (*                                                      *)
        (*         Editor for the user access rights            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 April 1998                   *)
        (*  Last edited:        24 July 2012                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT ADDRESS, ADR;

IMPORT Strings;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, LBAppend, CursorMovements,
                DestroyListBox, HighlightOn, HighlightOff,
                ClearListBox, LBCurrent, ReplaceCurrent, LBInsertAfter,
                LBDeleteCurrent;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, ItemSize, INIGetString, INIPut, INIPutBinary;

FROM InetUtilities IMPORT
    (* proc *)  ToLower;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  StringField, MenuField, CardinalField, Combine, ScreenEdit,
                DeleteStructure;

FROM Menus IMPORT
    (* type *)  Menu, ItemText,
    (* proc *)  CreateMenu, MenuColours;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, WriteChar, WriteString, WriteLn,
                SaveCursor, SetCursor, GetKey, EditString, PressAnyKey, GetScreenSize;

FROM MultiScreen IMPORT
    (* type *)  VirtualScreen,
    (* proc *)  MapToVirtualScreen, EnableScreenSwitching;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    FileNameSubscript = [0..255];
    FileNameString = ARRAY FileNameSubscript OF CHAR;
    PassString = ARRAY [0..31] OF CHAR;

    (* The fields in a UserPermission record have the following meanings.       *)
    (*      Password     The user's password                                    *)

    User = POINTER TO UserPermission;

    UserPermission = RECORD
                         Password: PassString;
                     END (*RECORD*);

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

(********************************************************************************)
(*                    CHECKING WHETHER A USER EXISTS                            *)
(********************************************************************************)

PROCEDURE UserInINIFile (VAR (*OUT*) hini: HINI;
                                username: ARRAY OF CHAR): BOOLEAN;

    (* If username is in INI file, initialises hini and returns TRUE. *)

    VAR size: CARDINAL;  result: BOOLEAN;
        key: ARRAY [0..0] OF CHAR;

    BEGIN
        hini := OurINIHandle();
        IF NOT INIValid(hini) THEN
            result := FALSE;
        ELSE
            key[0] := Nul;
            result := ItemSize (hini, username, key, size) AND (size <> 0);
        END (*IF*);
        RETURN result;
    END UserInINIFile;

(********************************************************************************)

PROCEDURE FindUser (UserName: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff the user data exists.        *)

    VAR hini: HINI;

    BEGIN
        RETURN UserInINIFile (hini, UserName);
    END FindUser;

(********************************************************************************)

PROCEDURE FindAlias (name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name is an alias in the INI file. *)

    VAR hini: HINI;
        size: CARDINAL;  result: BOOLEAN;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        hini := OurINIHandle();
        app := "$ALIAS";
        IF NOT INIValid(hini) THEN
            result := FALSE;
        ELSE
            result := ItemSize (hini, app, name, size);
        END (*IF*);
        RETURN result;
    END FindAlias;

(********************************************************************************)
(*                                  PARSER                                      *)
(********************************************************************************)

PROCEDURE ReadUserData (name: ARRAY OF CHAR): User;

    (* Fetches the password, etc., for the user whose username is specified     *)
    (* as the argument.                                                         *)

    VAR hini: HINI;  result: User;
        key: ARRAY [0..8] OF CHAR;

    BEGIN

        NEW (result);
        result^.Password := "";

        IF UserInINIFile (hini, name) THEN

            key := "Password";
            IF NOT INIGetString (hini, name, key, result^.Password) THEN
                result^.Password := "";
            END (*IF*);

        END (*IF*);

        RETURN result;

    END ReadUserData;

(********************************************************************************)
(*                      WRITING USER DATA TO THE INI FILE                       *)
(********************************************************************************)

PROCEDURE StoreUserData (U: User;  username: ARRAY OF CHAR);

    (* Writes user data to the INI file. *)

    VAR hini: HINI;
        key: ARRAY [0..8] OF CHAR;

    BEGIN
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            key := "Password";
            INIPut (hini, username, key, U^.Password);
        END (*IF*);
    END StoreUserData;

(********************************************************************************)
(*                        DISCARDING A USER RECORD                              *)
(********************************************************************************)

PROCEDURE DestroyUserData (VAR (*INOUT*) U: User);

    (* Discards the data structure.  *)

    BEGIN
        IF U <> NIL THEN
            DEALLOCATE (U, SIZE(UserPermission));
        END (*IF*);
    END DestroyUserData;

(************************************************************************)

PROCEDURE RemoveUser (name: ARRAY OF CHAR);

    (* Deletes this user's INI file entry. *)

    VAR hini: HINI;
        key: ARRAY [0..0] OF CHAR;

    BEGIN
        hini := OurINIHandle();
        IF INIValid(hini) THEN
            key[0] := Nul;
            INIPutBinary (hini, name, key, hini, 0);
        END (*IF*);
    END RemoveUser;

(********************************************************************************)
(*                          MASTER EDITING PROCEDURE                            *)
(********************************************************************************)

PROCEDURE SkipKey (w: Window);

    (* Discards the next keyboard character. *)

    VAR ch: CHAR;

    BEGIN
        ch := GetKey (w);
        IF ch = CHR(0) THEN
            EVAL (GetKey (w));
        END (*IF*);
    END SkipKey;

(********************************************************************************)

PROCEDURE EditUserData (Screen: VirtualScreen;  VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Top-level editor for the user permission data. *)

    VAR TopBar, MainWindow, bottombar, ErrorWindow: Window;
        UserName: PassString;
        R: Structure;
        abort: BOOLEAN;  ch: CHAR;  error: CARDINAL;
        length, saverow, savecol: CARDINAL;
        U: User;
        TempString: FileNameString;

    BEGIN
        EnableScreenSwitching (FALSE);
        Strings.Assign (name, UserName);

        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, Screen);
        WriteString (TopBar, "Username: ");
        WriteString (TopBar, UserName);

        OpenWindowHidden (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (bottombar, Screen);
        WriteString (bottombar, " Esc exit  ");
        WriteChar (bottombar, CHR(24));  WriteChar (bottombar, CHR(25));
        WriteString (bottombar, " select");

        U := ReadUserData (name);
        IF U = NIL THEN
            NEW (U);
            Strings.Assign ("", U^.Password);
        END (*IF*);

        OpenWindowHidden (MainWindow, white, blue, 2, 7, 1, 70, noframe, nodivider);
        MapToVirtualScreen (MainWindow, Screen);

        (* Create the username/category/password editing structure. *)

        SetCursor (MainWindow, 1, 2);  WriteString (MainWindow, "User name");
        SetCursor (MainWindow, 2, 2);  WriteString (MainWindow, "Password");

        R := StringField (UserName, 1, 20, 32);
        Combine (R, StringField (U^.Password, 2, 20, 32));

        (* Here's the main editing loop. *)

        SetCursor (MainWindow, 1, 1);
        LOOP
            LOOP
                ScreenEdit (MainWindow, R, abort);
                ToLower (UserName);
                IF abort THEN EXIT(*LOOP*) END(*IF*);

                (* Check the character that took us off the edge.  If it's *)
                (* not "cursor up", leave the editing loop.                *)

                ch := GetKey (MainWindow);
                IF ch = CHR(0) THEN
                    ch := GetKey (MainWindow);
                    IF ch <> 'H' THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);

            END (*LOOP*);

            (* Remove leading space and '$' characters from UserName. *)

            SaveCursor (MainWindow, saverow, savecol);
            length := LENGTH(UserName);
            WHILE (UserName[0] = ' ') OR (UserName[0] = '$') DO
                Strings.Delete (UserName, 0, 1);
                DEC (length);
                SetCursor (MainWindow, 1, 20+length);  WriteChar (MainWindow, ' ');
                SetCursor (MainWindow, 1, 20);  WriteString (MainWindow, UserName);
            END (*WHILE*);
            SetCursor (MainWindow, saverow, savecol);

            (* At this point, "name" holds the original username, and "UserName"    *)
            (* holds the new name.  We need to check for the following errors:      *)
            (*    1.  UserName is empty.                                            *)
            (*    2.  UserName duplicates an existing name.                         *)

            IF abort THEN
                name[0] := Nul;
                EXIT (*LOOP*);
            ELSIF UserName[0] = CHR(0) THEN
                OpenWindowHidden (ErrorWindow, white, black, 13, 15, 20, 59, noframe, nodivider);
                MapToVirtualScreen (ErrorWindow, Screen);
                WriteLn (ErrorWindow);
                WriteString (ErrorWindow, "  You have not assigned a user name");
                SkipKey (ErrorWindow);
                CloseWindow (ErrorWindow);
            ELSE
                Strings.Assign (name, TempString);
                ToLower (TempString);
                error := 0;
                IF NOT Strings.Equal (TempString, UserName) THEN
                    IF FindUser (UserName) THEN
                        error := 1;
                    ELSIF FindAlias (UserName) THEN
                        error := 2;
                    END (*IF*);
                END (*IF*);
                IF error = 0 THEN
                    EXIT (*LOOP*);
                ELSE
                    OpenWindowHidden (ErrorWindow, yellow, red, 13, 16, 16, 64, noframe, nodivider);
                    MapToVirtualScreen (ErrorWindow, Screen);
                    WriteLn (ErrorWindow);
                    WriteString (ErrorWindow, " You've duplicated the name of an existing ");
                    IF error = 1 THEN
                        WriteString (ErrorWindow, "user");
                    ELSE
                        WriteString (ErrorWindow, "alias");
                    END (*IF*);
                    WriteLn (ErrorWindow);
                    WriteString (ErrorWindow, "       Please assign a different name");
                    SkipKey (ErrorWindow);
                    CloseWindow (ErrorWindow);
                END (*IF*);
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        CloseWindow (MainWindow);
        CloseWindow (TopBar);

        (* Destroy the old user entry, and create a new one. *)

        IF NOT abort THEN
            IF name[0] <> Nul THEN
                RemoveUser (name);
            END (*IF*);
            Strings.Assign (UserName, name);
            StoreUserData (U, name);
        END (*IF*);
        DestroyUserData (U);

        CloseWindow (bottombar);
        EnableScreenSwitching (TRUE);

    END EditUserData;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
END Users.

