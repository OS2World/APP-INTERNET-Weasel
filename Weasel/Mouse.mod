(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2018   Peter Moylan                                     *)
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
        (*  Last edited:        7 January 2018                  *)
        (*  Status:                                             *)
        (*      The USEIOCTL option does not work, because of   *)
        (*      a problem with the MOU_READEVENTQUE function.   *)
        (*      With this option turned off, the module works   *)
        (*      but only two buttons are supported.             *)
        (*                                                      *)
        (*      Faults:                                         *)
        (*          - some procedures still not implemented.    *)
        (*            I might decide to remove them.            *)
        (*                                                      *)
        (********************************************************)

IMPORT OS2;

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16,
    (* proc *)  ADR;

FROM Storage IMPORT
    (* proc *)  DEALLOCATE;

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
    (* proc *)  CreateTask, CreateLock, DestroyLock, Obtain, Release,
                IsFullScreen;

FROM Windows IMPORT         (* for debugging *)
    (* type *)  Window, Colour, FrameType, DividerType,
                RowRange, ColumnRange,
    (* proc *)  OpenWindow, WriteChar, WriteString, WriteLn;

(************************************************************************)

TYPE ChangeMap = ARRAY Buttons OF Events;

CONST
    debugging = FALSE;

    (* Mappings for translating button states to events. *)

    Map1 = ChangeMap {LeftDown, RightDown, MiddleDown};
    Map2 = ChangeMap {LeftUp, RightUp, MiddleUp};

VAR
    debugwin: Window;      (* for debugging *)
    <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
        hMouse: OS2.HFILE;     (* used with IOCTL interface *)
    <* ELSE *>
        mouse: OS2.HMOU;       (* used with MOU interface *)
    <* END *>
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

PROCEDURE WriteCard (w: Window;  N: CARDINAL);

    (* Writes N in decimal to the window. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (w, N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (w, CHR(ORD('0')+N));
    END WriteCard;

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
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ParamLength, DataLength, ulrc: CARDINAL;
        <* END *>

    BEGIN
        MousePresent := HaveMouse;
        Nbuttons := 0;
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            NumberOfButtons := 055AAH;
            ParamLength := 0;
            DataLength := SIZE(CARD16);
            ulrc := OS2.DosDevIOCtl (hMouse, 7, OS2.MOU_GETBUTTONCOUNT,
                             NIL, ParamLength, ParamLength,
                             ADR(Nbuttons), DataLength, DataLength);
            IF debugging THEN
                WriteString (debugwin, "DosDevIOCtl return code = ");
                WriteCard (debugwin, ulrc);
                WriteLn (debugwin);
                IF ulrc = 0 THEN
                    WriteString (debugwin, "The mouse has ");
                    WriteCard (debugwin, Nbuttons);
                    WriteString (debugwin, " buttons");
                    WriteLn (debugwin);
                END (*IF*);
            END (*IF*);
        <* ELSE *>
            OS2.MouGetNumButtons (Nbuttons, mouse);
        <* END *>
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
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ParamLength, DataLength, ulrc: CARDINAL;
        <* END *>

    BEGIN
        ALLOCATE64 (position, SIZE(OS2.PTRLOC));
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ParamLength := 0;
            DataLength := SIZE(OS2.PTRLOC);
            ulrc := OS2.DosDevIOCtl (hMouse, 7, OS2.MOU_GETPTRPOS,
                             NIL, ParamLength, ParamLength,
                             position, DataLength, DataLength);
            IF ulrc <> 0 AND debugging THEN
                WriteString (debugwin, "Error ");
                WriteCard (debugwin, ulrc);
                WriteLn (debugwin);
            END (*IF*);
        <* ELSE *>
            OS2.MouGetPtrPos (position^, mouse);
        <* END *>
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
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ParamLength, DataLength, ulrc: CARDINAL;
        <* END *>

    BEGIN
        ALLOCATE64 (position, SIZE(OS2.PTRLOC));
        CheckLimits (Xposition, Yposition);
        position^.row := Yposition;  position^.col := Xposition;
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ParamLength := SIZE(OS2.PTRLOC);
            DataLength := 0;
            ulrc := OS2.DosDevIOCtl (hMouse, 7, OS2.MOU_SETPTRPOS,
                             position, ParamLength, ParamLength,
                             NIL, DataLength, DataLength);
            IF ulrc <> 0 AND debugging THEN
                WriteString (debugwin, "Error ");
                WriteCard (debugwin, ulrc);
                WriteLn (debugwin);
            END (*IF*);
        <* ELSE *>
            OS2.MouSetPtrPos (position^, mouse);
        <* END *>
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

    (*VAR PPtrArea: POINTER TO OS2.NOPTRRECT;*)

    BEGIN
        Limits.top := top;
        Limits.bottom := bottom;
        Limits.left := left;
        Limits.right := right;

        (* The following does not seem to have any effect. *)

        (*
        ALLOCATE64 (PPtrArea, SIZE(OS2.NOPTRRECT));
        WITH PPtrArea^ DO
            row := top;  col := left;  cRow := bottom;  cCol := right;
        END (*WITH*);
        EVAL (OS2.MouRemovePtr(PPtrArea^, mouse));
        DISPOSE (PPtrArea);
        *)

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
        <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
            ExtData: RECORD
                         w1, w2, w3: CARD16;
                     END (*RECORD*);
            ParamLength, DataLength, ulrc: CARDINAL;
        <* END *>

    BEGIN
        IF debugging THEN
            WriteString (debugwin, "CurrentFlags: ");
        END (*IF*);
        ALLOCATE64 (BufferPtr, SIZE(OS2.MOUEVENTINFO));
        WaitOnEmpty := 1;
        LOOP
            <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
                WaitOnEmpty := 1;
                ParamLength := SIZE(CARD16);
                DataLength := SIZE(OS2.MOUEVENTINFO);
                ulrc := OS2.DosDevIOCtl (hMouse, 7, OS2.MOU_READEVENTQUE,
                                 ADR(WaitOnEmpty), ParamLength, ParamLength,
                                 BufferPtr, DataLength, DataLength);
                IF ulrc <> 0 AND debugging THEN
                    WriteString (debugwin, "Error ");
                    WriteCard (debugwin, ulrc);
                    WriteLn (debugwin);
                END (*IF*);

                (* Try to get the data anyway. *)

                WaitOnEmpty := 0;
                ParamLength := 0;
                DataLength := SIZE(ExtData);
                ulrc := OS2.DosDevIOCtl (hMouse, 7, 077H,
                                 NIL, ParamLength, ParamLength,
                                 ADR(ExtData), DataLength, DataLength);
                IF ulrc <> 0 AND debugging THEN
                    WriteString (debugwin, "Error ");
                    WriteCard (debugwin, ulrc);
                    WriteLn (debugwin);
                END (*IF*);
            <* ELSE *>
                OS2.MouReadEventQue (BufferPtr^, WaitOnEmpty, mouse);
            <* END *>
            CurrentFlags := BufferPtr^.fs;
            IF debugging THEN
                WriteChar (debugwin, ' ');
                WriteCard (debugwin, CurrentFlags);
            END (*IF*);

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
(*                           INITIALISATION                             *)
(************************************************************************)

<* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>

PROCEDURE OpenMouse(): BOOLEAN;

    CONST
        Mode1 = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYNONE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READONLY;

    VAR ulrc, Action: CARDINAL;

    BEGIN
        (* Open mouse device. *)

        Action := 0;
        ulrc := OS2.DosOpen ("MOUSE$", hMouse, Action, 0, 0,
                             OS2.OPEN_ACTION_OPEN_IF_EXISTS, Mode1, NIL);
        IF debugging THEN
            WriteString (debugwin, "DosOpen return code = ");
            WriteCard (debugwin, ulrc);
            WriteLn (debugwin);
        END (*IF*);

        RETURN (ulrc = 0);

    END OpenMouse;

<* END *>

(************************************************************************)

BEGIN
    ButtonState := ButtonSet{};
    EventsToDetect := EventSet{};
    UserHandler := DummyEventHandler;
    CreateLock (CursorLock);
    ControlTextCursor := IsFullScreen();

    IF debugging THEN
        OpenWindow (debugwin, blue, cyan, 17, 24, 0, 55, simpleframe, nodivider);
    END (*IF*);

    <* IF DEFINED(USEIOCTL) & USEIOCTL THEN *>
        IF debugging THEN
            WriteString (debugwin, "Using the IOCTL option");
        END (*IF*);
        HaveMouse := OpenMouse();
    <* ELSE *>
        IF debugging THEN
            WriteString (debugwin, "Using the OS2.Mouxxx functions");
        END (*IF*);
        HaveMouse := OS2.MouOpen(NIL, mouse) = 0;
    <* END *>
    IF debugging THEN
        WriteLn (debugwin);
    END (*IF*);

    IF HaveMouse THEN
        ResetMouse (HaveMouse, NumberOfButtons);
        EVAL(CreateTask (EventTask, 6, "Mouse events"));
    ELSE
        NumberOfButtons := 0;
    END (*IF*);

FINALLY
    DestroyLock (CursorLock);
END Mouse.
