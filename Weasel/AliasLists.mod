(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE AliasLists;

        (********************************************************)
        (*                                                      *)
        (*                Editor for alias lists                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            7 July 1998                     *)
        (*  Last edited:        4 May 2019                      *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

IMPORT SYSTEM, Strings;

FROM LowLevel IMPORT
    (* proc *)  EVAL, Copy, CopyUp, AddOffset;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Keyboard IMPORT
    (* proc *)  StuffKeyboardBuffer, PutBack;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindowHidden, CloseWindow, SetCursor, WriteChar,
                WriteString, GetScreenSize, GetKey, ColourSwap,
                EditString, EditAborted;

FROM MultiScreen IMPORT
    (* type *)  VirtualScreen,
    (* proc *)  MapToVirtualScreen, VirtualScreenOf, EnableScreenSwitching;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, ItemSize, INIGetTrusted, INIPutBinary;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

FROM ListBoxes IMPORT
    (* type *)  ListBox,
    (* proc *)  CreateListBox, HighlightOn, HighlightOff, CursorMovements,
                LBAppend, LBCurrent, LBDeleteCurrent, LBInsertAfter,
                ReplaceCurrent, CursorBackward, WindowOf, LBSort,
                DestroyListBox, DisableScreenOutput;

(************************************************************************)

CONST Nul = CHR(0);  Esc = CHR(1BH);

TYPE
    NameStringIndex = [0..127];
    NameString = ARRAY NameStringIndex OF CHAR;
    CharArrayPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

VAR
    (* The number of display rows on the screen. *)

    ScreenRows: CARDINAL;

    (* The sequence number of the alias being edited. *)

    AliasNumber: CARDINAL;

(************************************************************************)
(*                   MOVING DATA TO/FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadNames (bufptr: CharArrayPointer;  BufferSize: CARDINAL;
                                                LB: ListBox);

    (* Loads a sequence of strings from bufptr^ to LB. *)

    VAR j, k: CARDINAL;
        name: NameString;

    BEGIN
        j := 1;

        (* Extract the alias number, if present. *)

        IF BufferSize = 0 THEN
            AliasNumber := MAX(SYSTEM.CARD16);
        ELSIF bufptr^[1] = CHR(3) THEN
            AliasNumber := ORD(bufptr^[2]) + 256*ORD(bufptr^[3]);
            j := 4;
        ELSE
            AliasNumber := MAX(SYSTEM.CARD16);
        END (*IF*);

        (* Each time around this loop we extract one name. *)

        WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO
            k := 0;
            REPEAT
                name[k] := bufptr^[j];
                INC (k);  INC (j);
            UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                    OR (k > MAX(NameStringIndex));
            IF name[0] <> "$" THEN
                LBAppend (LB, name);
            END (*IF*);
        END (*WHILE*);
    END LoadNames;

(************************************************************************)

PROCEDURE LoadTheList (LB: ListBox;  VAR (*IN*) name: ARRAY OF CHAR;
                                            VAR (*OUT*) Public: BOOLEAN);

    (* Fills the listbox with the contents of alias list "name". *)

    VAR hini: HINI;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        (* Get the list of users from the INI file. *)

        Public := FALSE;
        hini := OurINIHandle();
        app := "$ALIAS";
        IF INIValid(hini)
                  AND ItemSize (hini, app, name, BufferSize)
                  AND (BufferSize > 0) THEN
            ALLOCATE (bufptr, BufferSize);
            IF INIGetTrusted (hini, app, name, bufptr^, BufferSize) THEN
                Public := bufptr^[0] = CHR(1);
                LoadNames (bufptr, BufferSize, LB);
            END (*IF*);
            DEALLOCATE (bufptr, BufferSize);
            LBSort (LB);
        END (*IF*);

    END LoadTheList;

(************************************************************************)

PROCEDURE StoreTheList (LB: ListBox;  VAR (*IN*) name: ARRAY OF CHAR;
                                                       Public: BOOLEAN);

    (* Writes the list of users back to the INI file.  The contents of  *)
    (* LB are destroyed as a side-effect, but this is acceptable; we're *)
    (* going to destroy the list after finishing this job anyway.       *)

    CONST AllocationChunk = 8192;

    VAR hini: HINI;
        bufptr, auxptr: CharArrayPointer;
        BufferSize, UsedSize, j, k, length: CARDINAL;
        NextName: NameString;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        DisableScreenOutput (LB);

        (* Make sure we're at the beginning of the listbox. *)

        WHILE CursorBackward(LB) DO
        END (*WHILE*);

        (* Start with a buffer of arbitrary size; we'll expand it later *)
        (* as needed.                                                   *)

        BufferSize := AllocationChunk;
        ALLOCATE (bufptr, AllocationChunk);

        (* The first byte is the public/private indicator. *)

        IF Public THEN
            bufptr^[0] := CHR(1);
        ELSE
            bufptr^[0] := CHR(0);
        END (*IF*);

        (* The next three bytes are the alias number. *)

        bufptr^[1] := CHR(3);
        bufptr^[2] := CHR(AliasNumber MOD 256);
        bufptr^[3] := CHR(AliasNumber DIV 256);
        UsedSize := 4;  j := 4;

        LOOP
            LBCurrent (LB, NextName);
            IF NextName[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            LBDeleteCurrent (LB);
            length := Strings.Length (NextName);

            IF UsedSize + length + 2 > BufferSize THEN
                (* Expand the result buffer. *)
                ALLOCATE (auxptr, BufferSize+AllocationChunk);
                Copy (bufptr, auxptr, UsedSize);
                DEALLOCATE (bufptr, BufferSize);
                INC (BufferSize, AllocationChunk);
                bufptr := auxptr;
            END (*IF*);

            (* Store the current name in the buffer. *)

            FOR k := 0 TO length-1 DO
                bufptr^[j] := NextName[k];  INC (j);
            END (*FOR*);
            bufptr^[j] := Nul;  INC(j);
            INC (UsedSize, length+1);

        END (*LOOP*);

        (* Add a Nul to terminate the list. *)

        bufptr^[j] := Nul;  INC(j);

        (* We've assembled the data, now write it out. *)

        hini := OurINIHandle();
        IF INIValid(hini) THEN
            app := "$ALIAS";
            INIPutBinary (hini, app, name, bufptr^, j);
        END (*IF*);

        DEALLOCATE (bufptr, BufferSize);

    END StoreTheList;

(************************************************************************)
(*        GETTING AND SETTING THE ORDINAL NUMBER OF THIS LIST           *)
(************************************************************************)

PROCEDURE GetNumber (name: ARRAY OF CHAR): CARDINAL;

    (* Returns the ordinal number of alias 'name'. *)

    VAR BufferSize, result: CARDINAL;
        bufptr: CharArrayPointer;
        hini: HINI;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        hini := OurINIHandle();

        (* Load the alias list from the INI file. *)

        bufptr := NIL;
        app := "$ALIAS";
        IF INIValid(hini) OR NOT
                ItemSize (hini, app, name, BufferSize) THEN
            BufferSize := 0;
        END (*IF*);
        IF BufferSize > 0 THEN
            ALLOCATE (bufptr, BufferSize);
            EVAL(INIGetTrusted (hini, app, name, bufptr^, BufferSize));
        END (*IF*);

        (* Extract the alias number, if present. *)

        IF (BufferSize < 4) OR (bufptr^[1] <> CHR(3)) THEN
            result := MAX(SYSTEM.CARD16);
        ELSE
            result := ORD(bufptr^[2]) + 256*ORD(bufptr^[3]);
        END (*IF*);

        (* Deallocate the buffer and close the INI file. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

        RETURN result;

    END GetNumber;

(**************************************************************************)

PROCEDURE SetNumber (newnumber: CARDINAL;  name: ARRAY OF CHAR);

    (* Updates the ordinal number of alias 'name'. *)

    VAR BufferSize: CARDINAL;
        bufptr, bufptr2: CharArrayPointer;
        hini: HINI;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        hini := OurINIHandle();

        (* Load the alias list from the INI file. *)

        bufptr := NIL;
        app := "$ALIAS";
        IF INIValid(hini)
                    OR NOT ItemSize (hini, app, name, BufferSize) THEN
            BufferSize := 0;
        END (*IF*);
        IF BufferSize > 0 THEN
            ALLOCATE (bufptr, BufferSize);
            EVAL (INIGetTrusted (hini, app, name, bufptr^, BufferSize));
        END (*IF*);

        (* Expand buffer if alias number not already present. *)

        IF BufferSize = 0 THEN
            ALLOCATE (bufptr, 4);
            bufptr^[0] := CHR(1);
            BufferSize := 4;
        ELSIF (BufferSize < 4) OR (bufptr^[1] <> CHR(3)) THEN
            ALLOCATE (bufptr2, BufferSize+3);
            CopyUp (AddOffset(bufptr,1), AddOffset(bufptr2,4), BufferSize);
            DEALLOCATE (bufptr, BufferSize);
            bufptr := bufptr2;
            INC (BufferSize, 3);
        END (*IF*);

        (* Insert the number. *)

        bufptr^[1] := CHR(3);
        bufptr^[2] := CHR(newnumber MOD 256);
        bufptr^[3] := CHR(newnumber DIV 256);

        (* Write the result back, then deallocate the buffer.    *)

        IF INIValid(hini) THEN
            app := "$ALIAS";
            INIPutBinary (hini, app, name, bufptr^, BufferSize);
        END (*IF*);
        DEALLOCATE (bufptr, BufferSize);

    END SetNumber;

(************************************************************************)
(*                      THE MAIN LIST EDITOR                            *)
(************************************************************************)

PROCEDURE EditTheList (LB: ListBox;  BoxWidth: CARDINAL): BOOLEAN;

    (* Allows delete/edit/add in this listbox.  The function result is  *)
    (* TRUE if the user moved off the top of the list, and FALSE if     *)
    (* the operation was terminated with the Esc key.                   *)

    VAR w, BottomBar: Window;  ch: CHAR;
        ListEntry: NameString;

    BEGIN
        w := WindowOf(LB);
        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, VirtualScreenOf(w));
        WriteString (BottomBar, " A add  E edit  Del delete  Esc finished");

        LOOP
            IF CursorMovements (LB) THEN
                CloseWindow (BottomBar);  RETURN TRUE;
            END (*IF*);
            ch := GetKey(w);
            IF ch = Nul THEN
                ch := GetKey(w);
                IF ch = 'S' THEN                      (* Del = delete *)
                    LBCurrent (LB, ListEntry);
                    LBDeleteCurrent (LB);
                END (*IF*);
            ELSIF CAP(ch) = 'A' THEN                       (* A = add *)
                ListEntry := "";
                LBInsertAfter (LB, ListEntry);
                EditString (WindowOf(LB), ListEntry,
                         MAX(NameStringIndex)+1, BoxWidth);
                IF EditAborted() OR (ListEntry[0] = Nul) THEN
                    LBDeleteCurrent (LB);
                ELSE
                    ReplaceCurrent (LB, ListEntry);
                END (*IF*);
            ELSIF (CAP(ch) = 'E') OR (ch = CHR(13)) THEN   (* E = edit *)
                LBCurrent (LB, ListEntry);
                IF ListEntry[0] <> Nul THEN
                    EditString (WindowOf(LB), ListEntry,
                            MAX(NameStringIndex)+1, BoxWidth);
                    IF EditAborted() OR (ListEntry[0] = Nul) THEN
                        LBDeleteCurrent (LB);
                    ELSE
                        ReplaceCurrent (LB, ListEntry);
                    END (*IF*);
                END (*IF*);
            ELSIF ch = Esc THEN                            (* Esc = done *)
                CloseWindow (BottomBar);  RETURN FALSE;
            END (*IF*);
        END (*LOOP*);

    END EditTheList;

(************************************************************************)

PROCEDURE EditAliasData (Screen: VirtualScreen;  VAR (*INOUT*) name: ARRAY OF CHAR);

    (* Editor for one alias. *)

    CONST BoxTop = 2;  BoxLeft = 1;  BoxWidth = 76;

    VAR ListWindow, TopBar, BottomBar: Window;  LB: ListBox;
        ch: CHAR;  Public: BOOLEAN;
        BoxHeight: CARDINAL;

    (********************************************************************)

    PROCEDURE ShowPublic;

        (* Writes "Private" or "Public" in the top bar. *)

        BEGIN
            SetCursor (TopBar, 0, 1);
            IF Public THEN
                WriteString (TopBar, " Public");
            ELSE
                WriteString (TopBar, "Private");
            END (*IF*);
        END ShowPublic;

    (********************************************************************)

    BEGIN
        EnableScreenSwitching (FALSE);
        BoxHeight := ScreenRows - BoxTop - 4;

        OpenWindowHidden (TopBar, yellow, red, 0, 0, 0, 79, noframe, nodivider);
        MapToVirtualScreen (TopBar, Screen);
        WriteString (TopBar, "         alias name: ");
        WriteString (TopBar, name);

        OpenWindowHidden (BottomBar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
        MapToVirtualScreen (BottomBar, Screen);
        WriteChar (BottomBar, ' ');
        WriteChar (BottomBar, CHR(27));
        WriteString (BottomBar, " Private    ");
        WriteChar (BottomBar, CHR(26));
        WriteString (BottomBar, " Public    ");
        WriteChar (BottomBar, CHR(25));
        WriteString (BottomBar, " Edit list    Esc finished");

        OpenWindowHidden (ListWindow, black, white, BoxTop, BoxTop+BoxHeight+1,
                      BoxLeft, BoxLeft+BoxWidth+2, noframe, nodivider);
        MapToVirtualScreen (ListWindow, Screen);
        LB := CreateListBox (ListWindow, 1, 1, BoxHeight, BoxWidth);
        LoadTheList (LB, name, Public);

        LOOP
            HighlightOff (LB);
            LOOP
                ShowPublic;
                ColourSwap (TopBar, 0, 1, 8);
                ch := GetKey (TopBar);
                ColourSwap (TopBar, 0, 1, 8);
                IF ch = Esc THEN
                    PutBack(ch);  EXIT (*LOOP*);
                ELSIF ch = CHR(13) THEN
                    Public := NOT Public;
                ELSIF ch = Nul THEN
                    ch := GetKey (TopBar);
                    IF ch = "P" THEN                     (* cursor down *)
                        EXIT (*LOOP*);
                    ELSIF ch = "Q" THEN                  (* page down *)
                        PutBack(ch);  PutBack(Nul);
                        EXIT (*LOOP*);
                    ELSIF ch = "M" THEN                  (* cursor right *)
                        Public := TRUE;
                    ELSIF ch = "K" THEN                  (* cursor left *)
                        Public := FALSE;
                    END (*IF*);
                END (*IF*);
            END (*LOOP*);
            HighlightOn (LB);
            IF NOT EditTheList (LB, BoxWidth) THEN
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        StoreTheList (LB, name, Public);
        DestroyListBox (LB);  CloseWindow (ListWindow);
        CloseWindow (BottomBar);  CloseWindow(TopBar);
        EnableScreenSwitching (TRUE);

    END EditAliasData;

(************************************************************************)

PROCEDURE RemoveAlias (name: ARRAY OF CHAR);

    (* Deletes this alias from the system. *)

    VAR hini: HINI;  dummy: CHAR;
        app: ARRAY [0..6] OF CHAR;

    BEGIN
        hini := OurINIHandle();
        IF INIValid(hini) THEN
           app := "$ALIAS";
           INIPutBinary (hini, app, name, dummy, 0);
        END (*IF*);
    END RemoveAlias;

(********************************************************************************)

PROCEDURE RenameAlias (oldname, newname: ARRAY OF CHAR);

    (* Changes the name of an alias. *)

    VAR w: Window;  LB: ListBox;  Public: BOOLEAN;

    BEGIN
        (* For the sake of code re-use, we use a screen window and a listbox,   *)
        (* even though these will never appear on the screen.  This is not      *)
        (* very efficient in time, but the overhead is not enough to matter.    *)

        OpenWindowHidden (w, black, white, 0, 1, 0, 1, noframe, nodivider);
        LB := CreateListBox (w, 1, 1, 1, 1);
        Public := FALSE;
        IF oldname[0] <> Nul THEN
            LoadTheList (LB, oldname, Public);
            RemoveAlias (oldname);
        END (*IF*);
        IF newname[0] <> Nul THEN
            StoreTheList (LB, newname, Public);
        END (*IF*);
        DestroyListBox (LB);
        CloseWindow (w);
    END RenameAlias;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
END AliasLists.

