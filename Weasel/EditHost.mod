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

IMPLEMENTATION MODULE EditHosts;

        (********************************************************)
        (*                                                      *)
        (*         Editor for list of network hosts             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            10 May 1998                     *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST;

IMPORT Strings, FileSys;

FROM MultiScreen IMPORT
    (* type *)  ScreenGroup, VirtualScreen,
    (* proc *)  CreateScreenGroup, CreateVirtualScreen, MapToVirtualScreen,
                RemoveVirtualScreen, RemoveScreenGroup;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox,
                LBAppend, LBCurrent, HighlightOn, LBDeleteCurrent,
                LBInsertAfter, ReplaceCurrent,
                CursorBackward, CursorForward;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, ItemSize, INIGetTrusted, INIPutBinary;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor,
                WriteString, WriteLn, GetKey, GetScreenSize,
                EditString, SetActivePage;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer;

FROM TaskControl IMPORT
    (* proc *)  CreateTask1;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Names IMPORT
    (* type *)  HostName(*, HostCategory*);

(************************************************************************)

CONST Nul = CHR(0);

TYPE
    HostCategory = (local, mayrelay, relaydest, banned);
    LabelString = ARRAY [0..9] OF CHAR;
    Label = ARRAY HostCategory OF LabelString;

CONST
    INILabel = Label {"Local", "MayRelay", "RelayDest", "Banned"};

TYPE
    NameStringIndex = [0..31];
    NameString = ARRAY NameStringIndex OF CHAR;
    CharArrayPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Footer window. *)

    BottomBar: Window;

TYPE
    HostList = POINTER TO HostRecord;
    HostRecord = RECORD
                     previous, next: HostList;
                     name: HostName;
                 END (*RECORD*);

VAR
    MasterList: ARRAY HostCategory OF HostList;

(************************************************************************)
(*                 MOVING DATA TO AND FROM THE INI FILE                 *)
(************************************************************************)

PROCEDURE LoadList (category: HostCategory): HostList;

    (* Loads a list of hosts from the INI file. *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        j, k: CARDINAL;
        current, result: HostList;
        app: ARRAY [0..4] OF CHAR;
        key: LabelString;

    BEGIN
        current := NIL;
        result := NIL;
        app := "$SYS";
        key := INILabel[category];

        (* Get the list of hosts from the INI file. *)

        hini := OurINIHandle();
        IF INIValid(hini)
                  AND ItemSize (hini, app, key, BufferSize)
                  AND (BufferSize > 0) THEN
            ALLOCATE (bufptr, BufferSize);
            IF NOT INIGetTrusted (hini, app, key, bufptr^, BufferSize) THEN
                bufptr^[0] := Nul;
            END (*IF*);
            j := 0;

            (* Each time around this loop we extract one host name. *)

            WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO

                (* Create a new list entry. *)

                IF result = NIL THEN
                    NEW (result);  current := result;
                    current^.previous := NIL;
                ELSE
                    NEW (current^.next);
                    current^.next^.previous := current;
                    current := current^.next;
                END (*IF*);
                current^.next := NIL;

                (* Copy the name from bufptr^. *)

                k := 0;
                REPEAT
                    current^.name[k] := bufptr^[j];
                    INC (k);  INC (j);
                UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                        OR (k > MAX(NameStringIndex));

                (* Convert old-format wildcards to new format. *)

                IF current^.name[0] = '.' THEN
                    current^.name[0] := '*';
                END (*IF*);

            END (*WHILE*);
            DEALLOCATE (bufptr, BufferSize);

        END (*IF*);
        RETURN result;

    END LoadList;

(************************************************************************)

PROCEDURE StoreList (category: HostCategory;  List: HostList);

    (* Stores a HostList into the INI file. *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        j, k: CARDINAL;
        current: HostList;
        app: ARRAY [0..4] OF CHAR;
        key: LabelString;

    BEGIN
        (* Work out how much buffer space we need. *)

        BufferSize := 0;
        current := List;
        WHILE current <> NIL DO
            INC (BufferSize, LENGTH(current^.name) + 1);
            current := current^.next;
        END (*WHILE*);

        (* Create the string buffer. *)

        IF BufferSize = 0 THEN
            bufptr := NIL;
        ELSE
            ALLOCATE (bufptr, BufferSize);
        END (*IF*);

        (* Store all the strings into the buffer. *)

        current := List;  j := 0;
        WHILE current <> NIL DO
            k := 0;
            REPEAT
                bufptr^[j] := current^.name[k];
                INC (k);  INC (j);
            UNTIL (current^.name[k] = Nul) OR (k = SIZE(HostName));
            bufptr^[j] := Nul;
            INC (j);
            current := current^.next;
        END (*WHILE*);

        (* Write the buffer to the INI file. *)

        hini := OurINIHandle();
        IF INIValid(hini) THEN
            app := "$SYS";
            key := INILabel[category];
            INIPutBinary (hini, app, key, bufptr^, BufferSize);
        END (*IF*);

        (* Deallocate the buffer space. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

    END StoreList;

(************************************************************************)
(*                  DISPLAYING THE LIST OF HOSTS                        *)
(************************************************************************)

PROCEDURE FillList (LB: ListBox;  data: HostList);

    (* Adds all the hosts in data to the listbox. *)

    BEGIN
        WHILE data <> NIL DO
            LBAppend (LB, data^.name);
            data := data^.next;
        END (*WHILE*);
    END FillList;

(************************************************************************)
(*                      ENTERING A NEW HOST NAME                        *)
(************************************************************************)

PROCEDURE EditName (Screen: VirtualScreen;  VAR (*INOUT*) Name: ARRAY OF CHAR);

    VAR w: Window;

    BEGIN
        OpenWindowHidden (w, yellow, red, ScreenRows-2, ScreenRows-2,
                                            1, 78, noframe, nodivider);
        MapToVirtualScreen (w, Screen);
        EditString (w, Name, HIGH(Name) + 1, 77);
        CloseWindow (w);
    END EditName;

(************************************************************************)
(*                          MAIN EDITING TASK                           *)
(************************************************************************)

PROCEDURE HostListEditor (arg: ADDRESS);

    (* Runs as an autonomous task.  We have one instance of this        *)
    (* running for each host category.                                  *)

    TYPE HeadString = ARRAY HostCategory OF ARRAY [0..1] OF ARRAY [0..79] OF CHAR;
    CONST Heading = HeadString {{" LIST OF DOMAIN NAMES FOR THE LOCAL HOST",
                                 "   Mail to these addresses will be accepted here"},
                                {" ACCEPTABLE SOURCES FOR RELAY MAIL",
                                 "   These hosts and domains may send us mail to be relayed"},
                                {" ACCEPTABLE DESTINATIONS FOR RELAY MAIL",
                                 "   We agree to relay mail to these hosts and domains"},
                                {" BANNED HOSTS AND DOMAINS",
                                 "   All mail from these addresses will be refused"}};

    VAR category: HostCategory;
        TopBar: Window;
        CurrentHost: HostList;
        LB: ListBox;

    (********************************************************************)

    PROCEDURE StepBackward;

        BEGIN
            IF (CurrentHost <> NIL) AND (CurrentHost^.previous <> NIL) THEN
                CurrentHost := CurrentHost^.previous;
                EVAL (CursorBackward (LB));
            END (*IF*);
        END StepBackward;

    (********************************************************************)

    PROCEDURE StepForward;

        BEGIN
            IF (CurrentHost <> NIL) AND (CurrentHost^.next <> NIL) THEN
                CurrentHost := CurrentHost^.next;
                EVAL (CursorForward(LB));
            END (*IF*);
        END StepForward;

    (********************************************************************)

    CONST Esc = CHR(1BH);
        BoxTop = 2;  BoxLeft = 0;  BoxWidth = 78;

    VAR OurGroup: ScreenGroup;
        OurPage: VirtualScreen;
        ListWindow: Window;
        ch: CHAR;
        NewName: HostName;
        BoxHeight, j: CARDINAL;
        temp: HostList;

    BEGIN
        category := CAST (HostCategory, arg);
        BoxHeight := ScreenRows - BoxTop - 3;

        OurGroup := CreateScreenGroup (1);
        OurPage := CreateVirtualScreen (OurGroup);
        MapToVirtualScreen (BottomBar, OurPage);

        OpenWindowHidden (TopBar, yellow, red, 0, 1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, OurPage);
        WriteString (TopBar, Heading[category][0]);
        WriteLn (TopBar);
        WriteString (TopBar, Heading[category][1]);

        OpenWindowHidden (ListWindow, black, white, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+1, noframe, nodivider);
        MapToVirtualScreen (ListWindow, OurPage);
        LB := CreateListBox (ListWindow, 1, 1, BoxHeight, BoxWidth);
        MasterList[category] := LoadList (category);
        CurrentHost := MasterList[category];
        FillList (LB, MasterList[category]);
        HighlightOn (LB);

        LOOP
            ch := GetKey (ListWindow);
            IF ch = Nul THEN
                ch := GetKey (ListWindow);
                IF ch = "H" THEN                        (* cursor up *)
                    StepBackward;
                ELSIF ch = "P" THEN                     (* cursor down *)
                    StepForward;
                ELSIF ch = "G" THEN                     (* home *)
                    WHILE (CurrentHost <> NIL) AND (CurrentHost^.previous <> NIL) DO
                        StepBackward;
                    END (*WHILE*);
                ELSIF ch = "O" THEN                     (* end *)
                    WHILE (CurrentHost <> NIL) AND (CurrentHost^.next <> NIL) DO
                        StepForward;
                    END (*WHILE*);
                ELSIF ch = "I" THEN                     (* page up *)
                    FOR j := 1 TO BoxHeight-1 DO
                        StepBackward;
                    END (*FOR*);
                ELSIF ch = "Q" THEN                     (* page down *)
                    FOR j := 1 TO BoxHeight-1 DO
                        StepForward;
                    END (*FOR*);
                ELSIF ch = 'S' THEN                      (* Del = delete *)
                    temp := CurrentHost;
                    IF temp <> NIL THEN
                        CurrentHost := temp^.next;
                        IF CurrentHost <> NIL THEN
                            CurrentHost^.previous := temp^.previous;
                        END (*IF*);
                        IF temp^.previous = NIL THEN
                            MasterList[category] := CurrentHost;
                        ELSE
                            temp^.previous^.next := CurrentHost;
                        END (*IF*);

                        (* Special case: deleting the last element. *)

                        IF CurrentHost = NIL THEN
                            CurrentHost := temp^.previous;
                        END (*IF*);

                        DEALLOCATE (temp, SIZE(HostRecord));
                        LBDeleteCurrent (LB);

                    END (*IF option is delete *);

                END (*IF first keyboard character is CHR(0) *);

            (* End of the ch=Nul section. *)

            ELSIF CAP(ch) = 'A' THEN                       (* A = add *)
                NewName := "";
                EditName (OurPage, NewName);
                IF NewName[0] <> Nul THEN
                    NEW (temp);
                    Strings.Assign (NewName, temp^.name);
                    temp^.previous := CurrentHost;
                    IF CurrentHost = NIL THEN
                        temp^.next := NIL;
                        MasterList[category] := temp;
                    ELSE
                        temp^.next := CurrentHost^.next;
                        IF temp^.next <> NIL THEN
                            temp^.next^.previous := temp;
                        END (*IF*);
                        CurrentHost^.next := temp;
                    END (*IF*);
                    CurrentHost := temp;
                    LBInsertAfter (LB, NewName);
                END (*IF*);
            ELSIF CAP(ch) = 'P' THEN                       (* P = promote *)
                IF (CurrentHost <> NIL) AND (CurrentHost^.previous <> NIL) THEN
                    temp := CurrentHost^.previous;
                    ReplaceCurrent (LB, temp^.name);
                    IF temp^.previous = NIL THEN
                        MasterList[category] := CurrentHost;
                    ELSE
                        temp^.previous^.next := CurrentHost;
                    END (*IF*);
                    IF CurrentHost^.next <> NIL THEN
                        CurrentHost^.next^.previous := temp;
                    END (*IF*);
                    CurrentHost^.previous := temp^.previous;
                    temp^.next := CurrentHost^.next;
                    temp^.previous := CurrentHost;
                    CurrentHost^.next := temp;
                    IF CursorBackward (LB) THEN
                        ReplaceCurrent (LB, CurrentHost^.name);
                    END (*IF*);
                END (*IF*);
            ELSIF CAP(ch) = 'X' THEN                      (* X = exit *)
                SetActivePage (0);
                StuffKeyboardBuffer (CHR(27));
            END (*IF*);
        END (*LOOP*);

    END HostListEditor;

(************************************************************************)
(*                         INITIALISATION                               *)
(************************************************************************)

VAR dummy: CARDINAL;  category: HostCategory;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
    WriteString (BottomBar, " A add  P promote  Del delete  X exit");
    SetCursor (BottomBar, 0, 55);
    WriteString (BottomBar, "F4,F5 previous/next page");
    FOR category := MIN(HostCategory) TO MAX(HostCategory) DO
        EVAL(CreateTask1 (HostListEditor, 2, "Host editor", CAST(ADDRESS, category)));
    END (*FOR*);
FINALLY
    FOR category := MIN(HostCategory) TO MAX(HostCategory) DO
        StoreList (category, MasterList[category]);
    END (*FOR*);
    CloseWindow (BottomBar);
END EditHosts.

