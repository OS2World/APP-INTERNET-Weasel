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

IMPLEMENTATION MODULE EditUsers;

        (********************************************************)
        (*                                                      *)
        (*              Weasel user and alias editor            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 April 1998                   *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2, Strings, FileSys;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen,
                VirtualScreenOf, RemoveVirtualScreen, RemoveScreenGroup;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, CursorMovements, WindowOf,
                LBAppend, LBCurrent, HighlightOn, HighlightOff, LBDeleteCurrent,
                LBInsertAfter, LBInsertAt, ReplaceCurrent, LBSort, ItemNumberOf,
                LBUpdateItemNumbers, LBCurrentItemNumber, LBGoto, CursorForward,
                CursorBackward;

FROM InetUtilities IMPORT
    (* proc *)  ToLower;

FROM Users IMPORT
    (* proc *)  EditUserData, FindUser, RemoveUser;

FROM AliasLists IMPORT
    (* proc *)  EditAliasData, RenameAlias, RemoveAlias, GetNumber,
                SetNumber;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIGet, INIPutBinary, INIGetString, INIPutString,
                INIValid, ItemSize, INIGetTrusted, INIDeleteKey;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor, PutOnTop,
                EditString, EditAborted, WriteString, WriteLn, GetKey,
                GetScreenSize, SetActivePage;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer, PutBack;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal, DestroySemaphore;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    NameStringIndex = [0..31];
    NameString = ARRAY NameStringIndex OF CHAR;
    CharArrayPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Semaphore to signal that tasks have started properly. *)

    ConfirmStartup: Semaphore;

    (* Some useful constant strings. *)

    stralias: ARRAY [0..6] OF CHAR;
    null: ARRAY [0..0] OF CHAR;

(************************************************************************)
(*             ENSURING THAT EVERY USER HAS A MAIL DIRECTORY            *)
(************************************************************************)

PROCEDURE CheckUserDirectories (VAR (*IN*) MailRoot: ARRAY OF CHAR);

    (* Makes sure that MailRoot has a subdirectory for every user in    *)
    (* the user database.  Also creates the compulsory directories, if  *)
    (* they don't already exist.                                        *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        name: NameString;
        j, k: CARDINAL;
        DirName: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE MakeMailDirectory (username: ARRAY OF CHAR);

        (* Try to create a mailbox directory for the given user.  If    *)
        (* the directory already exists the operation will fail anyway. *)

        BEGIN
            Strings.Assign (MailRoot, DirName);
            Strings.Append (username, DirName);
            EVAL (FileSys.CreateDirectory (DirName));
        END MakeMailDirectory;

    (********************************************************************)

    BEGIN
        (* Create the mailroot directory if it doesn't already exist.   *)
        (* Note that we temporarily have to remove the trailing '\'     *)
        (* from the directory name.                                     *)

        j := Strings.Length (MailRoot);
        IF j > 0 THEN
            MailRoot[j-1] := Nul;
            EVAL (FileSys.CreateDirectory(MailRoot));
            Strings.Append ('\', MailRoot);
        END (*IF*);

        (* Get the list of users from the INI file. *)

        hini := OurINIHandle();
        IF INIValid(hini)
                  AND ItemSize (hini, null, null, BufferSize)
                  AND (BufferSize > 0) THEN
            ALLOCATE (bufptr, BufferSize);
                IF INIGetTrusted (hini, null, null, bufptr^, BufferSize) THEN
                j := 0;

                (* Each time around this loop we extract one user name. *)

                WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO
                    k := 0;
                    REPEAT
                        name[k] := bufptr^[j];
                        INC (k);  INC (j);
                    UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                            OR (k > MAX(NameStringIndex));
                    IF name[0] <> "$" THEN
                        MakeMailDirectory (name);
                    END (*IF*);

                END (*WHILE*);
            END (*IF*);

            DEALLOCATE (bufptr, BufferSize);

        END (*IF*);

        (* All users checked.  Now make sure the "postmaster" and  *)
        (* "forward" directories exist.                            *)

        MakeMailDirectory ("postmaster");
        MakeMailDirectory ("forward");

    END CheckUserDirectories;

(************************************************************************)
(*                  DISPLAYING THE LIST OF USERS                        *)
(************************************************************************)

PROCEDURE LoadNames (bufptr: CharArrayPointer;  BufferSize: CARDINAL;
                                         LB: ListBox;  numbered: BOOLEAN;
                                         VAR (*INOUT*) AddWild: BOOLEAN);

    (* Loads a sequence of strings from bufptr^ to LB.  The 'numbered'  *)
    (* option is for aliases where we each name has a sequence number   *)
    (* to show where it should go in the listbox.   The 'AddWild'       *)
    (* option, also for aliases, specifies that we should add a new     *)
    (* alias '*' unless a '*' entry already exists in the listbox.  If  *)
    (* the '*' entry already existed we return with AddWild = FALSE.    *)

    VAR j, k: CARDINAL;
        name: NameString;

    BEGIN
        j := 0;

        (* Each time around this loop we extract one name. *)

        WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO
            k := 0;
            REPEAT
                name[k] := bufptr^[j];
                INC (k);  INC (j);
            UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                    OR (k > MAX(NameStringIndex));
            IF name[0] <> "$" THEN
                IF Strings.Equal (name, '*') THEN
                    AddWild := FALSE;
                END (*IF*);
                IF numbered THEN
                    LBInsertAt (LB, name, GetNumber(name)+1);
                ELSE
                    LBAppend (LB, name);
                END (*IF*);
            END (*IF*);
        END (*WHILE*);

        (* In the LBInsertAt case there could be gaps in the item       *)
        (* number sequence, so we need to fix that.                     *)

        IF numbered THEN
            LBUpdateItemNumbers(LB);
        END (*IF*);

        (* In the special 'AddWild' case we have more work to do.       *)

        IF AddWild THEN
            LBAppend (LB, '*');
        END (*IF*);

    END LoadNames;

(************************************************************************)

PROCEDURE FillUserList (LB: ListBox);

    (* Adds all known users to the listbox. *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        AddWild: BOOLEAN;

    BEGIN
        AddWild := FALSE;

        (* Get the list of users from the INI file. *)

        hini := OurINIHandle();
        IF INIValid(hini)
                  AND ItemSize (hini, null, null, BufferSize)
                  AND (BufferSize > 0) THEN
            ALLOCATE (bufptr, BufferSize);
            IF INIGetTrusted (hini, null, null, bufptr^, BufferSize) THEN
                LoadNames (bufptr, BufferSize, LB, FALSE, AddWild);
            END (*IF*);
            DEALLOCATE (bufptr, BufferSize);
            LBSort (LB);

        END (*IF*);
    END FillUserList;

(************************************************************************)

PROCEDURE FillAliasList (LB: ListBox);

    (* Adds all known aliases to the listbox. *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        name: NameString;
        BufferSize: CARDINAL;
        AddWild: BOOLEAN;
        app: ARRAY [0..7] OF CHAR;
        key: ARRAY [0..13] OF CHAR;

    BEGIN
        AddWild := FALSE;

        (* Get the list of aliases from the INI file. *)

        hini := OurINIHandle();
        IF INIValid(hini) THEN
            app := "$SYS";
            key := "AcceptUnknown";
            IF INIGet (hini, app, key, AddWild) THEN
                INIDeleteKey (hini, app, key);
            END (*IF*);
            IF ItemSize (hini, stralias, null, BufferSize) THEN
                IF BufferSize = 0 THEN
                    bufptr := NIL;
                ELSE
                    ALLOCATE (bufptr, BufferSize);
                    EVAL (INIGetTrusted (hini, stralias, null, bufptr, BufferSize));
                END (*IF*);
                LoadNames (bufptr, BufferSize, LB, TRUE, AddWild);
                IF BufferSize > 0 THEN
                    DEALLOCATE (bufptr, BufferSize);
                END (*IF*);
            END (*IF*);

            (* If AddWild is TRUE, we have to create a new alias. *)

            IF AddWild THEN
                BufferSize := ItemNumberOf(LB, '*');
                IF BufferSize > 0 THEN
                    DEC (BufferSize);
                END (*IF*);
                name := "    unknown";
                name[0] := CHR(1);
                name[1] := CHR(3);
                name[2] := CHR(BufferSize MOD 256);
                name[3] := CHR(BufferSize DIV 256);
                name[11] := CHR(0);
                name[12] := CHR(0);
                key := "*";
                INIPutBinary (hini, stralias, key, name, 13);
                app := "unknown";
                key := "Password";
                IF NOT INIGetString (hini, app, key, name) THEN
                    name[0] := Nul;
                    INIPutString (hini, app, key, name);
                END (*IF*);
            END (*IF*);

        END (*IF*);

    END FillAliasList;

(**************************************************************************)

PROCEDURE UpdateAliasIndices (LB: ListBox);

    (* Updates the indices, in the INI file, of aliases starting with   *)
    (* the current listbox entry and continuing to the end of the list. *)

    TYPE NameIndex = [0..255];

    VAR k, start: CARDINAL;  name: ARRAY NameIndex OF CHAR;

    BEGIN
        start := LBCurrentItemNumber(LB);
        IF start > 0 THEN
            k := start-1;
            LOOP
                LBCurrent (LB, name);
                IF name[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);
                SetNumber (k, name);
                INC (k);
                IF NOT CursorForward (LB) THEN
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
            LBGoto (LB, start);
        END (*IF*);
    END UpdateAliasIndices;

(************************************************************************)
(*                          THE ALIAS EDITOR                            *)
(************************************************************************)

PROCEDURE EditAliasList (LB: ListBox;  BoxWidth: CARDINAL);

    (* Allows delete/add/edit operations on LB. *)

    TYPE NameIndex = [0..255];

    VAR w: Window;  OurPage: VirtualScreen;  ch: CHAR;
        OldName, AliasName: ARRAY NameIndex OF CHAR;

    (********************************************************************)

    PROCEDURE EditNewName;

        (* Edits the variable 'AliasName', checking for duplicate names.*)
        (* An empty string is an acceptable result.                     *)

        VAR err: Window;

        BEGIN
            LOOP
                HighlightOff(LB);
                EditString (WindowOf(LB), AliasName, MAX(NameIndex)+1, BoxWidth);
                HighlightOn(LB);
                ToLower (AliasName);
                IF EditAborted() THEN
                    AliasName := OldName;
                END (*IF*);
                IF Strings.Equal(AliasName, OldName) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                IF NOT FindUser (AliasName)
                         AND (ItemNumberOf(LB, AliasName) = 0) THEN
                    EXIT (*LOOP*);
                END (*IF*);
                OpenWindowHidden (err, yellow, red, 5, 9, 40, 69, noframe, nodivider);
                MapToVirtualScreen (err, OurPage);
                WriteLn (err);
                WriteString (err, "      Duplicate name");
                WriteLn (err);
                WriteString (err, "   Type <Enter> to re-edit");
                WriteLn (err);
                WriteString (err, "      or Esc to abort");
                REPEAT
                    ch := GetKey (err);
                UNTIL (ch = CHR(27)) OR (ch = CHR(13));
                CloseWindow (err);
                IF ch = CHR(27) THEN
                    AliasName := OldName;
                    PutBack(ch);
                END (*IF*);
            END (*LOOP*);
        END EditNewName;

    (********************************************************************)

    BEGIN
        w := WindowOf(LB);
        OurPage := VirtualScreenOf(w);
        HighlightOn (LB);
        LOOP
            EVAL (CursorMovements (LB));
            ch := GetKey(w);
            IF ch = Nul THEN
                ch := GetKey(w);
                IF ch = 'S' THEN                      (* Del = delete *)
                    LBCurrent (LB, AliasName);
                    RemoveAlias (AliasName);
                    LBDeleteCurrent (LB);
                    UpdateAliasIndices (LB);
                END (*IF*);
            ELSIF CAP(ch) = 'A' THEN                       (* A = add *)
                OldName := "";
                LBInsertAfter (LB, OldName);
                AliasName := OldName;
                EditNewName;
                IF AliasName[0] = Nul THEN
                    LBDeleteCurrent (LB);
                ELSE
                    ReplaceCurrent (LB, AliasName);
                    EditAliasData (OurPage, AliasName);
                END (*IF*);
                UpdateAliasIndices (LB);
            ELSIF (CAP(ch) = 'E') OR (ch = CHR(13)) THEN   (* E = edit *)
                LBCurrent (LB, AliasName);
                IF AliasName[0] <> Nul THEN
                    EditAliasData (OurPage, AliasName);
                    ReplaceCurrent (LB, AliasName);
                END (*IF*);
            ELSIF CAP(ch) = 'P' THEN                       (* P = promote *)
                LBCurrent (LB, AliasName);
                IF (AliasName[0] <> Nul) AND CursorBackward(LB) THEN
                    LBCurrent (LB, OldName);
                    ReplaceCurrent (LB, AliasName);
                    EVAL(CursorForward(LB));
                    ReplaceCurrent (LB, OldName);
                    EVAL(CursorBackward(LB));
                    UpdateAliasIndices (LB);
                END (*IF*);
            ELSIF CAP(ch) = 'R' THEN                       (* R = rename *)
                LBCurrent (LB, OldName);
                IF OldName[0] <> Nul THEN
                    AliasName := OldName;
                    EditNewName;
                    IF NOT Strings.Equal(AliasName, OldName) THEN
                        ReplaceCurrent (LB, AliasName);
                        RenameAlias (OldName, AliasName);
                    END (*IF*);
                END (*IF*);
            ELSIF CAP(ch) = 'X' THEN                       (* X = exit *)
                SetActivePage (0);
                StuffKeyboardBuffer (CHR(27));
            END (*IF*);
        END (*LOOP*);
    END EditAliasList;

(************************************************************************)

PROCEDURE AliasEditor;

    (* Runs as an autonomous task. *)

    CONST Esc = CHR(1BH);
        BoxTop = 2;  BoxLeft = 10;  BoxWidth = 35;

    VAR ListWindow, TopBar, BottomBar: Window;  AliasList: ListBox;
        BoxHeight: CARDINAL;
        OurGroup: ScreenGroup;
        OurPage: VirtualScreen;

    BEGIN
        BoxHeight := ScreenRows - BoxTop - 4;

        OurGroup := CreateScreenGroup (1);
        OurPage := CreateVirtualScreen (OurGroup);

        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, OurPage);
        WriteString (BottomBar, " A add E edit P promote R rename Del delete X exit");
        SetCursor (BottomBar, 0, 55);
        WriteString (BottomBar, "F4,F5 previous/next page");

        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);
        WriteString (TopBar, "    ALIASES");
        Signal (ConfirmStartup);

        OpenWindowHidden (ListWindow, black, white, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+1, noframe, nodivider);
        MapToVirtualScreen (ListWindow, OurPage);
        AliasList := CreateListBox (ListWindow, 1, 1, BoxHeight, BoxWidth);
        FillAliasList (AliasList);
        EditAliasList (AliasList, BoxWidth);

    END AliasEditor;

(************************************************************************)
(*                           THE USER EDITOR                            *)
(************************************************************************)

PROCEDURE UserEditor;

    (* Runs as an autonomous task. *)

    CONST Esc = CHR(1BH);
        BoxTop = 2;  BoxLeft = 2;  BoxWidth = 35;

    VAR ListWindow, TopBar, BottomBar: Window;  UserList: ListBox;
        ch: CHAR;
        UserName: ARRAY [0..BoxWidth-1] OF CHAR;
        BoxHeight: CARDINAL;
        OurGroup: ScreenGroup;
        OurPage: VirtualScreen;

    BEGIN

        BoxHeight := ScreenRows - BoxTop - 4;

        OurGroup := CreateScreenGroup (1);
        OurPage := CreateVirtualScreen (OurGroup);

        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, OurPage);
        WriteString (BottomBar, " A add  E edit  Del delete  X exit");
        SetCursor (BottomBar, 0, 55);
        WriteString (BottomBar, "F4,F5 previous/next page");

        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);

        IF CreateTask (AliasEditor, 2, "Alias editor") THEN
            Signal (ConfirmStartup);
            Wait (ConfirmStartup);
        END (*IF*);
        Signal (ConfirmStartup);

        WriteString (TopBar, "    WEASEL USER EDITOR");

        OpenWindowHidden (ListWindow, black, white, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+2, noframe, nodivider);
        MapToVirtualScreen (ListWindow, OurPage);
        UserList := CreateListBox (ListWindow, 1, 1, BoxHeight, BoxWidth);
        FillUserList (UserList);
        HighlightOn (UserList);

        LOOP
            EVAL (CursorMovements (UserList));
            ch := GetKey(ListWindow);
            IF ch = Nul THEN
                ch := GetKey(ListWindow);
                IF ch = 'S' THEN                      (* Del = delete *)
                    LBCurrent (UserList, UserName);
                    RemoveUser (UserName);
                    LBDeleteCurrent (UserList);
                END (*IF*);
            ELSIF CAP(ch) = 'A' THEN                       (* A = add *)
                UserName := "";
                EditUserData (OurPage, UserName);
                IF UserName[0] <> Nul THEN
                    LBInsertAfter (UserList, UserName);
                END (*IF*);
            ELSIF (CAP(ch) = 'E') OR (ch = CHR(13)) THEN   (* E = edit *)
                LBCurrent (UserList, UserName);
                EditUserData (OurPage, UserName);
                IF UserName[0] <> Nul THEN
                    ReplaceCurrent (UserList, UserName);
                END (*IF*);
            ELSIF CAP(ch) = 'X' THEN                       (* X = exit *)
                SetActivePage (0);
                StuffKeyboardBuffer (CHR(27));
            END (*IF*);
        END (*LOOP*);

    END UserEditor;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    null[0] := Nul;
    stralias := "$ALIAS";
    GetScreenSize (ScreenRows, dummy);
    CreateSemaphore (ConfirmStartup, 0);
    IF CreateTask (UserEditor, 2, "User editor") THEN
       Wait (ConfirmStartup);  Signal (ConfirmStartup);
    END (*IF*);
    Wait (ConfirmStartup);
    DestroySemaphore (ConfirmStartup);
END EditUsers.

