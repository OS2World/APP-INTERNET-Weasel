(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE Mouse;

        (********************************************************)
        (*                                                      *)
        (*                  Mouse driver                        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             Partly written                  *)
        (*                                                      *)
        (*      Faults:                                         *)
        (*          - some procedures still not implemented.    *)
        (*            I might decide to remove them.            *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2;

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16;

FROM Storage IMPORT
    (* proc *)  DEALLOCATE;

FROM Keyboard IMPORT
    (* proc *)  IsFullScreen;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND, ALLOCATE64;

        (* I don't know why I have to import the following constants,   *)
        (* given that I've already imported the enumerated types.       *)
        (* Either this is a compiler bug or it's my misunderstanding    *)
        (* of the standard.                                             *)

FROM Mouse0 IMPORT
    (* const *)  LeftButton, RightButton, MiddleButton, Motion,
                 LeftDown, MiddleDown, RightDown, LeftUp, RightUp, MiddleUp;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateTask, CreateLock, DestroyLock, Obtain, Release;

(************************************************************************)

TYPE ChangeMap = ARRAY Buttons OF Events;

CONST
    (* Mappings for translating button states to events. *)

    Map1 = ChangeMap {LeftDown, RightDown, MiddleDown};
    Map2 = ChangeMap {LeftUp, RightUp, MiddleUp};

VAR
    mouse: OS2.HMOU;
    HaveMouse: BOOLEAN;
    NumberOfButtons: CARDINAL;
    ButtonState: ButtonSet;
    CurrentRow, CurrentCol: CARDINAL;

    (* UserHandler is a user-supplied procedure that we must call when  *)
    (* a mouse event is detected; and EventsToDetect is the set of      *)
    (* events the user wants to know about.  (Events not in this set    *)
    (* will be ignored, except to the extent that this module keeps     *)
    (* a private record of the button state.)                           *)

    UserHandler: EventHandler;
    EventsToDetect: EventSet;

    (* We don't let the mouse position go outside the following limits. *)

    Limits: RECORD
                top, bottom, left, right: CARDINAL;
            END (*RECORD*);

    (* ControlTextCursor is TRUE if this module is supposed to display  *)
    (* the cursor position on the screen, rather than letting the       *)
    (* WPS do it.  The text counter is visible as long as the counter   *)
    (* ShowTextCursor is positive.  CursorLock is a critical section    *)
    (* protection lock for ShowTextCursor.                              *)

    ControlTextCursor: BOOLEAN;
    ShowTextCursor: INTEGER;
    CursorLock: Lock;

(************************************************************************)

PROCEDURE MouseAvailable (): BOOLEAN;

    (* Returns TRUE iff a mouse driver is loaded, a mouse exists, and   *)
    (* mouse operation is permitted in module ConfigurationOptions.     *)

    BEGIN
        RETURN HaveMouse AND (NumberOfButtons > 0);
    END MouseAvailable;

(************************************************************************)

PROCEDURE ColourFlip (row, col: CARDINAL);

    (* Swaps forward and background colours at one screen position. *)

    VAR buffer: ARRAY [0..1] OF CARD8;  temp: CARD16;

    BEGIN
        temp := 2;
        OS2.VioReadCellStr (buffer, temp, row, col, 0);
        temp := 16*(buffer[1] MOD 16) + (buffer[1] DIV 16);
        OS2.VioWrtNAttr (temp, 1, row, col, 0);
    END ColourFlip;

(************************************************************************)

PROCEDURE ResetMouse (VAR (*OUT*) MousePresent: BOOLEAN;
                        VAR (*OUT*) NumberOfButtons: CARDINAL);

    (* Initializes mouse, returning MousePresent as FALSE if no mouse   *)
    (* available and as TRUE if it is, and NumberOfButtons as the       *)
    (* number of buttons for the mouse if installed.                    *)

    VAR Nbuttons: CARD16;

    BEGIN
        MousePresent := HaveMouse;
        OS2.MouGetNumButtons (Nbuttons, mouse);
        NumberOfButtons := Nbuttons;
        SetMouseCursorLimits (0, 24, 0, 79);
        GetTextMousePosition (CurrentRow, CurrentCol);
        Obtain (CursorLock);
        IF ControlTextCursor THEN
            ShowTextCursor := 1;
        ELSE
            ShowTextCursor := 0;
        END (*IF*);
        IF ShowTextCursor > 0 THEN
            ColourFlip (CurrentRow, CurrentCol);
        END (*IF*);
        Release (CursorLock);
    END ResetMouse;

(************************************************************************)

PROCEDURE CheckLimits (VAR (*INOUT*) X, Y: CARDINAL);

    (* Modifies X and Y, if necessary, to ensure that they are within   *)
    (* the currently active limits.                                     *)

    BEGIN
        WITH Limits DO
            IF X < left THEN X := left
            ELSIF X > right THEN X := right
            END (*IF*);
            IF Y < top THEN Y := top
            ELSIF Y > bottom THEN Y := bottom
            END (*IF*);
        END (*WITH*);
    END CheckLimits;

(************************************************************************)

PROCEDURE GetTextMousePosition (VAR (*OUT*) Xposition: CARDINAL;
                                VAR (*OUT*) Yposition: CARDINAL);

    (* Returns the current position of the mouse cursor. *)

    VAR position: POINTER TO OS2.PTRLOC;

    BEGIN
        ALLOCATE64 (position, SIZE(OS2.PTRLOC));
        OS2.MouGetPtrPos (position^, mouse);
        Xposition := position^.col;
        Yposition := position^.row;
        CheckLimits (Xposition, Yposition);
        DISPOSE (position);
    END GetTextMousePosition;

(************************************************************************)

PROCEDURE GetTextMouseStatus (VAR (*OUT*) buttons: ButtonSet;
                                VAR (*OUT*) Xposition: CARDINAL;
                                VAR (*OUT*) Yposition: CARDINAL);

    (* Returns the current mouse position and state of the buttons.     *)

    BEGIN
        GetTextMousePosition (Xposition, Yposition);
        buttons := ButtonState;
    END GetTextMouseStatus;

(************************************************************************)

PROCEDURE SetTextMousePosition (Xposition: CARDINAL; Yposition: CARDINAL);

    (* Initialises the mouse position. *)

    VAR position: POINTER TO OS2.PTRLOC;

    BEGIN
        ALLOCATE64 (position, SIZE(OS2.PTRLOC));
        CheckLimits (Xposition, Yposition);
        position^.row := Yposition;  position^.col := Xposition;
        OS2.MouSetPtrPos (position^, mouse);
        DISPOSE (position);
    END SetTextMousePosition;

(************************************************************************)

PROCEDURE SetTextMousePage (page: CARDINAL);

    (* Sets the hardware screen page where the mouse is visible. *)

    BEGIN
        (* Operation not supported. *)
    END SetTextMousePage;

(************************************************************************)

PROCEDURE SetMouseCursorLimits (top, bottom: CARDINAL;
                                        left, right: CARDINAL);

    (* Specifies a rectangular region outside which the mouse cursor    *)
    (* may not go.                                                      *)

    VAR PPtrArea: POINTER TO OS2.NOPTRRECT;

    BEGIN
        Limits.top := top;
        Limits.bottom := bottom;
        Limits.left := left;
        Limits.right := right;

        (* The following does not seem to have any effect. *)

        ALLOCATE64 (PPtrArea, SIZE(OS2.NOPTRRECT));
        WITH PPtrArea^ DO
            row := top;  col := left;  cRow := bottom;  cCol := right;
        END (*WITH*);
        EVAL (OS2.MouRemovePtr(PPtrArea^, mouse));
        DISPOSE (PPtrArea);

    END SetMouseCursorLimits;

(************************************************************************)

PROCEDURE ShowMouseCursor;

    (* Makes the mouse cursor visible on the screen. *)

    BEGIN
        IF ControlTextCursor THEN
            Obtain (CursorLock);
            IF ShowTextCursor = 0 THEN
                ColourFlip (CurrentRow, CurrentCol);
            END (*IF*);
            INC (ShowTextCursor);
            Release (CursorLock);
        END (*IF*);
    END ShowMouseCursor;

(************************************************************************)

PROCEDURE HideMouseCursor;

    (* Makes the mouse cursor invisible. *)

    BEGIN
        IF ControlTextCursor THEN
            Obtain (CursorLock);
            DEC (ShowTextCursor);
            IF ShowTextCursor = 0 THEN
                ColourFlip (CurrentRow, CurrentCol);
            END (*IF*);
            Release (CursorLock);
        END (*IF*);
    END HideMouseCursor;

(************************************************************************)

PROCEDURE InstallEventHandler (DetectedEvents: EventSet;
                                        Handler: EventHandler);

    (* Nominates the procedure to be called whenever an event in the    *)
    (* set DetectedEvents occurs.                                       *)

    BEGIN
        EventsToDetect := DetectedEvents;
        UserHandler := Handler;
    END InstallEventHandler;

(************************************************************************)

PROCEDURE DummyEventHandler (E: EventSet;  B: ButtonSet;  row, col: CARDINAL);

    (* This is a "do nothing" event handler, that is used only if       *)
    (* there has been no call to InstallEventHandler.                   *)

    BEGIN
    END DummyEventHandler;

(************************************************************************)

PROCEDURE ButtonEvents (oldbuttons, newbuttons: ButtonSet): EventSet;

    (* Calculates the events corresponding to a change in button state  *)
    (* from "oldbuttons" to "newbuttons".                               *)

    VAR B: Buttons;  result: EventSet;

    BEGIN
        result := EventSet {};
        FOR B := LeftButton TO MiddleButton DO
            IF B IN newbuttons-oldbuttons THEN
                INCL (result, Map1[B]);
            ELSIF B IN oldbuttons-newbuttons THEN
                INCL (result, Map2[B]);
            END (*IF*);
        END (*FOR*);
        RETURN result;
    END ButtonEvents;

(************************************************************************)
(*                       TASK TO PICK UP MOUSE EVENTS                   *)
(************************************************************************)

PROCEDURE EventTask;

    VAR BufferPtr: POINTER TO OS2.MOUEVENTINFO;
        WaitOnEmpty: CARD16;
        CurrentFlags, NewRow, NewCol: CARDINAL;
        motion, B1down, B2down, B3down: BOOLEAN;
        NewButtonState: ButtonSet;
        NewEvents: EventSet;

    BEGIN
        ALLOCATE64 (BufferPtr, SIZE(OS2.MOUEVENTINFO));
        WaitOnEmpty := 1;
        LOOP
            OS2.MouReadEventQue (BufferPtr^, WaitOnEmpty, mouse);
            CurrentFlags := BufferPtr^.fs;

            (* These are the flags as reported by the system call.  Now we      *)
            (* have to decode them into the notation that we are using.         *)

            motion := IAND (CurrentFlags, 2BH) <> 0;
            B1down := IAND (CurrentFlags, 06H) <> 0;
            B2down := IAND (CurrentFlags, 18H) <> 0;
            B3down := IAND (CurrentFlags, 60H) <> 0;
            NewButtonState := ButtonSet{};
            IF B1down THEN INCL(NewButtonState, LeftButton) END (*IF*);
            IF B2down THEN INCL(NewButtonState, RightButton) END (*IF*);
            IF B3down THEN INCL(NewButtonState, MiddleButton) END (*IF*);

            (* That tells us the current state of the buttons.  Now work out    *)
            (* which buttons changed state.                                     *)

            NewEvents := ButtonEvents (ButtonState, NewButtonState);
            IF motion THEN INCL (NewEvents, Motion) END(*IF*);
            ButtonState := NewButtonState;

            WITH BufferPtr^ DO
                NewRow := row;  NewCol := col
            END (*WITH*);
            CheckLimits (NewCol, NewRow);

            (* Show the text cursor, if we're currently doing that. *)

            IF (NewRow <> CurrentRow) OR (NewCol <> CurrentCol) THEN
                Obtain (CursorLock);
                IF ShowTextCursor > 0 THEN
                    ColourFlip (CurrentRow, CurrentCol);
                    ColourFlip (NewRow, NewCol);
                END (*IF*);
                Release (CursorLock);
                CurrentRow := NewRow;  CurrentCol := NewCol;
            END (*IF*);

            (* Call the user handler. *)

            IF EventsToDetect*NewEvents <> EventSet{} THEN
                UserHandler (NewEvents, NewButtonState, NewCol, NewRow);
            END (*IF*);

        END (*LOOP*);

    END EventTask;

(************************************************************************)

BEGIN
    ButtonState := ButtonSet{};
    EventsToDetect := EventSet{};
    UserHandler := DummyEventHandler;
    CreateLock (CursorLock);
    ControlTextCursor := IsFullScreen();

    HaveMouse := OS2.MouOpen(NIL, mouse) = 0;
    IF HaveMouse THEN
        ResetMouse (HaveMouse, NumberOfButtons);
        EVAL(CreateTask (EventTask, 6, "Mouse events"));
    ELSE
        NumberOfButtons := 0;
    END (*IF*);

FINALLY
    DestroyLock (CursorLock);
END Mouse.
