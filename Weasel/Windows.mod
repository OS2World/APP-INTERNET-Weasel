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

IMPLEMENTATION MODULE Windows;

        (********************************************************)
        (*                                                      *)
        (*                Text-mode screen windows              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        22 December 2013                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*    Faults:                                           *)
        (*      1. (fixed)                                      *)
        (*      2. (fixed)                                      *)
        (*                                                      *)
        (*      For further thought at some later stage:        *)
        (*         -    (maybe) extend the set of procedures    *)
        (*              for which w^.access0p5 is locked.       *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16, ADDRESS,
    (* proc *)  ADR, CAST;

IMPORT OS2;

FROM Types IMPORT
    (* proc *)  FarPointer, FarCharPointer;

FROM LowLevel IMPORT
    (* proc *)  Far, MakePointer, FarAddOffset, Copy, FarCopy, CopyUp,
                EVAL, IXORB, HighWord, ALLOCATE64;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateTask, CreateLock, DestroyLock,
                Obtain, Release;

FROM Timer IMPORT
    (* proc *)  TimedWait;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM Keyboard IMPORT
    (* proc *)  InKey, PutBack;

(************************************************************************)
(*      If you want black-and-white operation even though your          *)
(*      display supports colour (e.g. for the case where the colours    *)
(*      are not very readable), set ForcedMonochrome to TRUE.           *)
(*      Otherwise, this module selects colour operation if it           *)
(*      thinks that the hardware can support it.                        *)
(************************************************************************)

CONST ForcedMonochrome = FALSE;

(************************************************************************)

CONST
    BytesPerChar = 2;                   (* # bytes/char in video buffer *)
    DefaultPage = 0;

(************************************************************************)

TYPE
    BufferSubscript = [0..MAX(INTEGER) DIV BytesPerChar - 1];
    ExtendedBufferSubscript = [0..MAX(INTEGER) DIV BytesPerChar];

    CloseHandlerList = POINTER TO CloseHandlerRecord;
    CloseHandlerRecord = RECORD
                            next: CloseHandlerList;
                            proc: CloseHandlerProc;
                         END (*RECORD*);

    PageChangeList = POINTER TO PageChangeRecord;
    PageChangeRecord = RECORD
                          next: PageChangeList;
                          proc: PageChangeHandler;
                       END (*RECORD*);

    ScreenChar = RECORD
                     val: CHAR;
                     attr: CARD8;
                 END (*RECORD*);

    Window = POINTER TO WindowData;

        (****************************************************************)
        (*                                                              *)
        (* WindowData records are linked (by next and previous) as a    *)
        (* doubly linked list, to implement a stack of windows.         *)
        (* There is a separate stack for each display page.             *)
        (* Variable TopWindow[page] points to the top of this stack.    *)
        (*                                                              *)
        (* The access0p5 lock is to control simultaneous access         *)
        (* to a single window by multiple tasks.  This is a new         *)
        (* feature, so the protection is implemented for some but not   *)
        (* all operations at this stage.  I'm still working on whether  *)
        (* access0p5 should be a Lock or a Semaphore.  From the         *)
        (* viewpoint of the external caller it's better to make it a    *)
        (* Lock, but that creates problems during shutdown.             *)
        (*                                                              *)
        (* CloseList points to a linked list of procedures to be        *)
        (* called when this window is closed.                           *)
        (*                                                              *)
        (* The row and column values stored in this record are          *)
        (* window-relative coordinates.                                 *)
        (*                                                              *)
        (* The BufferPosition field is a subscript into the buffer      *)
        (* array (see below).  It can be computed easily from the       *)
        (* "row" and "column" fields, but it is more convenient to      *)
        (* keep this technically redundant variable.                    *)
        (*                                                              *)
        (* A window's InputWaiting flag is set when it is waiting for   *)
        (* a keyboard character.  When the character arrives it is      *)
        (* stored in the InputChar field of this record, and we are     *)
        (* notified of this by a Release(CharAvailable).  Because the   *)
        (* requests for input are not necessarily satisfied in a FIFO   *)
        (* order - because the input focus can change before the input  *)
        (* arrives - we need a separate semaphore and InputChar field   *)
        (* for each window, even though these fields remain unused for  *)
        (* most windows.                                                *)
        (*                                                              *)
        (* A window with the "hidden" flag set is not part of the       *)
        (* stack of windows, and is not visible on the screen.          *)
        (*                                                              *)
        (* The "obscured" field indicates whether this window is wholly *)
        (* or partially obscured by another window on the screen.  By   *)
        (* keeping track of this, we can avoid some unnecessary screen  *)
        (* refreshing.                                                  *)
        (*                                                              *)
        (* The "blockcursor" field specifies what kind of cursor to     *)
        (* display: a block cursor if TRUE, an underline cursor if      *)
        (* FALSE.  Most of the time this is irrelevant, as we display   *)
        (* a cursor only when the window's CursorWanted flag is TRUE.   *)
        (*                                                              *)
        (* The "bufptr^" array holds a copy of what is supposed to be   *)
        (* transferred to the video buffer.  The size of bufptr^        *)
        (* corresponds to the actual window size.                       *)
        (*                                                              *)
        (****************************************************************)

    WindowData = RECORD
                    next, previous: Window;
                    access0p5: Semaphore;
                    CloseList: CloseHandlerList;
                    frame: FrameType;  divider: DividerType;
                    ScrollRegion, DefaultScrollRegion: Rectangle;
                    page: DisplayPage;
                    FirstRow: RowRange;
                    FirstColumn: ColumnRange;
                    Rows, Columns: CARDINAL;
                    row, column: CARDINAL;
                    BufferPosition: ExtendedBufferSubscript;
                    foreground, background: Colour;
                    CurrentAttributes: CARD8;
                    InputChar: CHAR;
                    CharAvailable: Semaphore;
                    InputWaiting, hidden, obscured, InsertMode,
                        CursorWanted, blockcursor, WrapOption: BOOLEAN;
                    bufptr: POINTER TO ARRAY BufferSubscript OF ScreenChar;
                 END (*RECORD*);

(************************************************************************)
(*                 NOTE ON CRITICAL SECTION PROTECTION                  *)
(* The potential deadlock problems in this module are surprisingly      *)
(* subtle, arising from the fact that a procedure incorporating one     *)
(* form of critical section protection may call other procedures which  *)
(* themselves contain critical section protection.  To avoid these      *)
(* problems, we use an ordered resource policy.  Each critical section  *)
(* protection semaphore is given a "level", which for clarity is shown  *)
(* as the last character of its name.  A piece of code is said to be    *)
(* executing at level N if it is inside a critical section protected    *)
(* by a semaphore whose level is N (and not inside a critical section   *)
(* protected by any semaphore of any higher level).  The rule which     *)
(* avoids deadlock is: to lock a semaphore at level N, we must be       *)
(* executing at a level < N.                                            *)
(* For the purposes of this analysis, Locks and Semaphores are treated  *)
(* as being equivalent.                                                 *)
(*                                                                      *)
(*                      LATEST DEVELOPMENT                              *)
(* Because I'm adding a new semaphore, but haven't yet updated the      *)
(* notation, a new level 0p5 (meaning 0.5) has been added.              *)
(************************************************************************)

VAR
    (* BlackAndWhite is true if we have a monochrome display.           *)

    BlackAndWhite: BOOLEAN;

    (* Maximum row and column numbers for the current video mode.       *)
    (* Set during module initialisation.                                *)

    MaxRowNumber, MaxColumnNumber: CARDINAL;

    (* ActivePage is the display page currently visible on the screen.  *)

    ActivePage: DisplayPage;

    (* A list of procedures to call whenever the current display page   *)
    (* is changed, and a lock to control access to this list.           *)

    PageChangeProcs: PageChangeList;
    PageChangeListAccess: Lock;

    (* A semaphore to say that somebody wants some keyboard input.      *)

    InputRequest: Semaphore;

    (* BlankRow is set up by the initialisation code as a row of space  *)
    (* characters.  Note however that the attribute codes need to be    *)
    (* filled in before each use.                                       *)

    BlankRowPtr: POINTER TO ARRAY ColumnRange OF ScreenChar;

    (* Access to BlankRow is a critical section, so we protect it with  *)
    (* a Lock.                                                          *)

    BlankRowAccess1: Lock;

    (* StackAccess2 is used to protect access to the shared data        *)
    (* structure which defines the stacks of windows.                   *)

    StackAccess2: Lock;

    (* TopWindow[p] is the current top of the stack of windows for      *)
    (* display on physical display page p.                              *)

    TopWindow: ARRAY DisplayPage OF Window;

    (* ScreenAccess3 is used to protect access to memory in segment     *)
    (* ScreenSeg, i.e. the memory belonging to the physical screen.     *)

    ScreenAccess3: Lock;

    (* PhysicalCursor keeps track of the blinking screen cursor.  The   *)
    (* CursorWindow field shows which window, if any, currently "owns"  *)
    (* the physical cursor.  CursorVisible[page] shows whether the      *)
    (* cursor should be visible when "page" is the active display page. *)
    (* ScreenPos and Attributes are the position and display attributes *)
    (* that we expect for the next character to be physically written   *)
    (* to the screen.                                                   *)
    (* Lock access4 is used to protect these variables and the hardware *)
    (* operations of turning the cursor on and off.                     *)
    (* For now, access4 also protects alterations to the active         *)
    (* display page.                                                    *)

    PhysicalCursor: RECORD
                        access4: Lock;
                        CursorWindow: ARRAY DisplayPage OF Window;
                        CursorVisible: ARRAY DisplayPage OF BOOLEAN;
                        ScreenPos: CARDINAL;
                        Attributes: CARD8;
                    END (*RECORD*);

(************************************************************************)
(*                 TURNING THE SCREEN CURSOR ON AND OFF                 *)
(************************************************************************)

PROCEDURE CursorOff;

    (* Turns the cursor off.  *)

    VAR CursorInfoBlockPtr: POINTER TO OS2.VIOCURSORINFO;

    BEGIN
        ALLOCATE64 (CursorInfoBlockPtr, SIZE(OS2.VIOCURSORINFO));
        WITH CursorInfoBlockPtr^ DO
            yStart := 0;  cEnd := 0;  cx := 0;  attr := 0FFFFH;
        END (*WITH*);
        OS2.VioSetCurType (CursorInfoBlockPtr^, 0);
        DISPOSE (CursorInfoBlockPtr);
    END CursorOff;

(************************************************************************)

PROCEDURE CursorOn (row, column: CARDINAL;  blockcursor: BOOLEAN);

    (* Displays a blinking screen cursor at the specified position.     *)

    CONST Minus90 = 0FFFFH - 89;  Minus100 = 0FFFFH - 99;

    VAR CursorInfoBlockPtr: POINTER TO OS2.VIOCURSORINFO;

    BEGIN
        ALLOCATE64 (CursorInfoBlockPtr, SIZE(OS2.VIOCURSORINFO));
        WITH CursorInfoBlockPtr^ DO
            IF blockcursor THEN yStart := 0 ELSE yStart := Minus90 END (*IF*);
            cEnd := Minus100;  cx := 1;  attr := 0;
        END (*WITH*);
        OS2.VioSetCurType (CursorInfoBlockPtr^, 0);
        OS2.VioSetCurPos (row, column, 0);
        DISPOSE (CursorInfoBlockPtr);
    END CursorOn;

(************************************************************************)

PROCEDURE UpdatePhysicalCursor;

    (* Turns the physical cursor on or off, as appropriate.  Also       *)
    (* signals a new input request, if a window on the active page is   *)
    (* waiting for input, in case the input task has gone idle.         *)
    (* The caller must be running at level<4.                           *)

    VAR w: Window;

    BEGIN
        WITH PhysicalCursor DO
            Obtain (access4);
            w := CursorWindow[ActivePage];
            IF w <> NIL THEN
                Signal (InputRequest);
            END (*IF*);
            IF CursorVisible[ActivePage] THEN
                WITH w^ DO
                    CursorOn (FirstRow+row, FirstColumn+column, blockcursor);
                END (*WITH*);
            ELSE
                CursorOff;
            END (*IF*);
            Release (access4);
        END (*WITH*);
    END UpdatePhysicalCursor;

(************************************************************************)
(*                  HARDWARE DISPLAY PAGE CHANGES                       *)
(************************************************************************)

PROCEDURE SetActivePage (page: DisplayPage);

    (* Changes the active display page.  Remark: OS/2 apparently does   *)
    (* not support multiple hardware text pages, at least in a VIO      *)
    (* session, so we simulate them by displaying only those windows    *)
    (* for which w^.page=ActivePage.                                    *)

    VAR PCL: PageChangeList;

    BEGIN
        Obtain (PhysicalCursor.access4);
        ActivePage := page;
        Release (PhysicalCursor.access4);
        RefreshDisplay;

        (* Call the procedures which want notification of the change.   *)

        Obtain (PageChangeListAccess);
        PCL := PageChangeProcs;
        WHILE PCL <> NIL DO
            PCL^.proc (page);
            PCL := PCL^.next;
        END (*WHILE*);
        Release (PageChangeListAccess);

        (* Turn the cursor off or on, as appropriate. *)

        UpdatePhysicalCursor;

    END SetActivePage;

(************************************************************************)

PROCEDURE RequestPageChangeNotification (Proc: PageChangeHandler);

    (* Sets up Proc as a procedure to be called on a page change.       *)

    VAR PCL: PageChangeList;

    BEGIN
        NEW (PCL);
        Obtain (PageChangeListAccess);
        WITH PCL^ DO
            next := PageChangeProcs;
            proc := Proc;
        END (*WITH*);
        PageChangeProcs := PCL;
        Release (PageChangeListAccess);
    END RequestPageChangeNotification;

(************************************************************************)

PROCEDURE PageOf (w: Window): DisplayPage;

    (* Returns the display page on which window w resides. *)

    BEGIN
        RETURN w^.page;
    END PageOf;

(************************************************************************)

PROCEDURE CurrentPage(): DisplayPage;

    (* Returns the currently active display page. *)

    BEGIN
        RETURN ActivePage;
    END CurrentPage;

(************************************************************************)
(*              MANIPULATION OF THE STACK OF WINDOWS                    *)
(************************************************************************)

PROCEDURE UnLink (w: Window);

    (* Removes w^ from the stack, but otherwise leaves it unchanged.    *)
    (* Caller must have locked StackAccess2.                            *)

    BEGIN
        WITH w^ DO
            IF previous <> NIL THEN previous^.next := next END (*IF*);
            IF next <> NIL THEN next^.previous := previous END (*IF*);
            IF TopWindow[page] = w THEN TopWindow[page] := next END (*IF*);
            previous := NIL;  next := NIL;
        END (*WITH*);
    END UnLink;

(************************************************************************)

PROCEDURE IdentifyTopWindow (VAR (*OUT*) w: Window;  page: DisplayPage;
                                VAR (*INOUT*) row: RowRange;
                                VAR (*INOUT*) col: ColumnRange): BOOLEAN;

    (* On entry w is unspecified and (page,row,col) describes a         *)
    (* position on the screen.  On exit w is equal to the top window    *)
    (* containing this screen location, and (row,col) have been altered *)
    (* to be window-relative coordinates.  Exception: if there is no    *)
    (* visible window containing the given point, the function result   *)
    (* is FALSE, the returned w is meaningless, and row and col are     *)
    (* unchanged.                                                       *)

    BEGIN
        Obtain (StackAccess2);
        w := TopWindow[page];
        LOOP
            IF w = NIL THEN EXIT(*LOOP*);
            ELSIF (col >= w^.FirstColumn) AND (col < w^.Columns + w^.FirstColumn)
                        AND (row >= w^.FirstRow) AND (row < w^.FirstRow + w^.Rows) THEN
                DEC (row, w^.FirstRow);  DEC (col, w^.FirstColumn);
                EXIT (*LOOP*);
            ELSE
                w := w^.next;
            END (*IF*);
        END (*LOOP*);
        Release (StackAccess2);
        RETURN w <> NIL;
    END IdentifyTopWindow;

(************************************************************************)

PROCEDURE ComputeCursorWindow (page: DisplayPage);

    (* Rechecks which window on this page should have the physical      *)
    (* screen cursor, and displays or turns off the cursor, as          *)
    (* appropriate, if a change is needed.  This procedure should be    *)
    (* called whenever there is a chance that the input focus might     *)
    (* need to be shifted.                                              *)

    VAR w, wtop: Window;  visible: BOOLEAN;
        row: RowRange;  col: ColumnRange;

    BEGIN
        (* Find the top window that's waiting for input. *)

        Obtain (StackAccess2);
        w := TopWindow[page];
        LOOP
            IF w = NIL THEN EXIT(*LOOP*) END(*IF*);
            IF w^.InputWaiting THEN EXIT(*LOOP*) END(*IF*);
            w := w^.next;
        END (*LOOP*);
        Release (StackAccess2);

        (* Check whether the cursor should be visible. *)

        IF w = NIL THEN
            visible := FALSE;
        ELSE
            row := w^.FirstRow+w^.row;  col := w^.FirstColumn+w^.column;
            visible := IdentifyTopWindow (wtop, page, row, col) AND (wtop = w);
        END (*IF*);

        (* Turn the cursor on or off, if necessary. *)

        WITH PhysicalCursor DO
            Obtain (access4);
            IF (w = NIL) OR (CursorWindow[page] <> w) THEN
                CursorWindow[page] := w;
                CursorVisible[page] := visible AND w^.CursorWanted;
                IF page = ActivePage THEN
                    IF CursorVisible[page] THEN
                        CursorOn (w^.FirstRow+w^.row, w^.FirstColumn+w^.column, w^.blockcursor);
                    ELSE
                        CursorOff;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            Release (access4);
        END (*WITH*);
    END ComputeCursorWindow;

(************************************************************************)
(*                         SCREEN REFRESHING                            *)
(************************************************************************)

PROCEDURE PartialRefresh (w: Window;  startrow, endrow: RowRange;
                                        startcol, endcol: ColumnRange);

    (* Re-draws the image of window w on the screen, in the area        *)
    (* bounded by the given absolute screen coordinates.  The ranges    *)
    (* specified are inclusive limits.                                  *)
    (* Caller must be executing at level <3.                            *)

    VAR i: RowRange;  bytecount: CARDINAL;  offset2: BufferSubscript;

    BEGIN
        WITH w^ DO

            (* Work out the overlap between the region and the window.  *)

            IF FirstRow > startrow THEN startrow := FirstRow END (*IF*);
            IF FirstRow + Rows <= endrow THEN endrow := FirstRow + Rows - 1 END (*IF*);
            IF FirstColumn > startcol THEN startcol := FirstColumn END (*IF*);
            IF Columns + FirstColumn <= endcol THEN endcol := Columns + FirstColumn - 1 END (*IF*);

            (* Refresh that region, if it is nonempty.  *)

            IF (startrow <= endrow) AND (startcol <= endcol) THEN
                bytecount := BytesPerChar*(endcol - startcol + 1);
                FOR i := startrow TO endrow DO
                    offset2 := Columns*(i-FirstRow) + startcol-FirstColumn;
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[offset2], bytecount, i, startcol, 0);
                    Release (ScreenAccess3);
                END (*FOR*);
            END (*IF*);
        END (*WITH*);
    END PartialRefresh;

(************************************************************************)

PROCEDURE Refresh (w: Window);

    (* Re-draws the image of window w on the screen.  (But there's no   *)
    (* physical write if w^.page <> ActivePage.)                        *)
    (* Caller must be executing at level <3.                            *)

    VAR i: RowRange;  bytecount: CARDINAL;  offset: BufferSubscript;

    BEGIN
        WITH w^ DO
            IF page = ActivePage THEN
                bytecount := BytesPerChar*Columns;
                FOR i := 0 TO Rows - 1 DO
                    offset := Columns*i;
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[offset], bytecount,
                                           FirstRow+i, FirstColumn, 0);
                    Release (ScreenAccess3);
                END (*FOR*);
            END (*IF*);
            obscured := FALSE;
        END (*WITH*);
    END Refresh;

(************************************************************************)

PROCEDURE ComputeCollisions (w: Window);

    (* Updates the "obscured" field of all windows which are below this *)
    (* one on the stack, and sets w^.obscured to FALSE.  Also updates   *)
    (* the cursor visibility information, based on the assumption that  *)
    (* w is the window on top of its stack.                             *)
    (* Caller must have locked StackAccess2.                            *)
    (* Caller must be executing at level <4.                            *)

    VAR left, right: ColumnRange;
        top, bottom: RowRange;
        w2: Window;  p: DisplayPage;

    BEGIN
        (* Take note of the screen location of this window. *)

        WITH w^ DO
            obscured := FALSE;
            left := FirstColumn;  right := Columns + FirstColumn - 1;
            top := FirstRow;  bottom := FirstRow + Rows - 1;
            w2 := next;  p := page;
        END (*WITH*);

        (* Update the cursor visibility information. *)

        WITH PhysicalCursor DO
            Obtain (access4);
            IF w^.InputWaiting THEN
                CursorWindow[p] := w;
                CursorVisible[p] := w^.CursorWanted;
            ELSIF CursorVisible[p] THEN
                WITH CursorWindow[p]^ DO
                    CursorVisible[p] := (FirstRow+row < top) OR (FirstRow+row > bottom)
                                OR (FirstColumn+column < left) OR (FirstColumn+column > right);
                END (*WITH*);
            END (*IF*);
            Release (access4);
        END (*WITH*);

        (* Update the "obscured" information for all windows under      *)
        (* the current window.  (For those which are already obscured   *)
        (* by some other window, no further check is needed.)           *)

        WHILE w2 <> NIL DO
            WITH w2^ DO
                IF NOT obscured THEN
                    obscured := (Columns + FirstColumn > left) AND (FirstColumn <= right)
                                AND (FirstRow + Rows > top) AND (FirstRow <= bottom);
                END (*IF*);
            END (*WITH*);
            w2 := w2^.next;
        END (*WHILE*);
    END ComputeCollisions;

(************************************************************************)

PROCEDURE PutOnTopI (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen. *)
    (* This procedure does the same job as PutOnTop (see below), but    *)
    (* different entry assumptions.                                     *)
    (* The caller must have locked StackAccess2.                        *)
    (* The caller must be executing at level 2.                         *)

    BEGIN
        UnLink (w);
        IF TopWindow[w^.page] <> NIL THEN
            TopWindow[w^.page]^.previous := w
        END (*IF*);
        w^.next := TopWindow[w^.page];  TopWindow[w^.page] := w;
        Refresh (w);  ComputeCollisions (w);
        IF w^.page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
    END PutOnTopI;

(************************************************************************)

PROCEDURE PutOnTop (w: Window);

    (* Makes w the top of stack, and refreshes its image on the screen. *)
    (* This also unhides w if it was hidden.                            *)
    (* This is the externally callable version.                         *)
    (* The caller must be executing at a level <0.5.                    *)

    BEGIN
        Wait (w^.access0p5);
        Obtain (StackAccess2);
        w^.hidden := FALSE;
        IF TopWindow[w^.page] <> w THEN
            PutOnTopI (w);
        ELSIF w^.page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
        Release (StackAccess2);
        Signal (w^.access0p5);
    END PutOnTop;

(************************************************************************)

PROCEDURE Repaint (page: DisplayPage;  startrow, endrow: RowRange;
                                        startcol, endcol: ColumnRange);

    (* Repaints the specified (inclusive) rectangular region on the     *)
    (* screen, and sets the physical screen cursor as necessary.        *)
    (* The caller must be executing at level <1.                        *)

    CONST NormalVideo = 07H;

    VAR i: RowRange;  k: ColumnRange;
        count: CARDINAL;
        p: Window;

    BEGIN
        (* First, clear the region.     *)

        IF page = ActivePage THEN
            count := endcol - startcol + 1;
            Obtain (BlankRowAccess1);
            FOR k := 0 TO count-1 DO
                BlankRowPtr^[k].attr := NormalVideo;
            END (*FOR*);
            count := BytesPerChar*count;
            FOR i := startrow TO endrow DO
                Obtain (ScreenAccess3);
                OS2.VioWrtCellStr(BlankRowPtr^[0], count, i, startcol, 0);
                Release (ScreenAccess3);
            END (*FOR*);
            Release (BlankRowAccess1);
        END (*IF*);

        (* Now refresh all open windows (or, more precisely, the parts  *)
        (* of them which lie in the affected region).                   *)

        Obtain (StackAccess2);
        WITH PhysicalCursor DO
            Obtain (access4);
            CursorWindow[page] := NIL;
            CursorVisible[page] := FALSE;
            Release (access4);
        END (*WITH*);
        IF TopWindow[page] <> NIL THEN
            p := TopWindow[page];
            WHILE p^.next <> NIL DO  p := p^.next  END (*WHILE*);
            REPEAT
                IF page = ActivePage THEN
                    PartialRefresh (p, startrow, endrow, startcol, endcol);
                END (*IF*);
                ComputeCollisions (p);  p := p^.previous;
            UNTIL p = NIL;
        END (*IF*);
        IF page = ActivePage THEN
            UpdatePhysicalCursor;
        END (*IF*);
        Release (StackAccess2);
    END Repaint;

(************************************************************************)

PROCEDURE RefreshDisplay;

    (* Rewrites every open window.  Should not normally be needed, but  *)
    (* available for use in cases the display is corrupted by, for      *)
    (* example, software which bypasses this module and writes directly *)
    (* to the screen.                                                   *)
    (* The caller must be executing at level <1.                        *)

    VAR p: DisplayPage;

    BEGIN
        FOR p := 0 TO MAX(DisplayPage) DO
            Repaint (p, 0, MaxRowNumber, 0, MaxColumnNumber);
        END (*FOR*);
    END RefreshDisplay;

(************************************************************************)

PROCEDURE Hide (w: Window);

    (* Makes this window invisible on the screen.  It is still possible *)
    (* to write to the window, but the output will not appear until     *)
    (* a PutOnTop(w) is executed.                                       *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        IF NOT w^.hidden THEN
            Wait (w^.access0p5);
            Obtain (StackAccess2);
            w^.hidden := TRUE;
            UnLink (w);
            Release (StackAccess2);
            Signal (w^.access0p5);

            (* Repaint the part of the screen which this window occupied.   *)

            WITH w^ DO
                Repaint (page, FirstRow, FirstRow + Rows - 1,
                               FirstColumn, Columns + FirstColumn - 1);
            END (*WITH*);
        END (*IF*);
    END Hide;

(************************************************************************)

PROCEDURE PutOnPage (w: Window;  p: DisplayPage);

    (* Moves window w to another display page.  The default is to put   *)
    (* every window on page 0 when it is first opened.  To override     *)
    (* the default, call this procedure after opening the window.       *)

    VAR wasvisible: BOOLEAN;

    BEGIN
        wasvisible := NOT w^.hidden;
        Hide (w);

        WITH w^ DO

            (* Change the page. *)

            page := p;
            IF wasvisible THEN
                hidden := FALSE;  PutOnTop (w);
            END(*IF*);

        END (*WITH*);

    END PutOnPage;

(************************************************************************)
(*                      SETTING THE SCROLLING REGION                    *)
(************************************************************************)

PROCEDURE InScrollingRegion (w: Window): BOOLEAN;

    (* Returns TRUE iff the current cursor position of window w is      *)
    (* inside its scrolling region.                                     *)

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                RETURN (row >= top) AND (row <= bottom)
                        AND (column >= left) AND (column <= right);
            END (*WITH*);
        END (*WITH*);
    END InScrollingRegion;

(************************************************************************)

PROCEDURE InExtendedScrollingRegion (w: Window): BOOLEAN;

    (* Similar to InScrollingRegion, but also returns TRUE if we are    *)
    (* just off the right edge of the scrolling region.                 *)

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                RETURN (row >= top) AND (row <= bottom)
                        AND (column >= left) AND (column <= right+1);
            END (*WITH*);
        END (*WITH*);
    END InExtendedScrollingRegion;

(************************************************************************)

PROCEDURE ChangeScrollingRegion (w: Window;  firstline, lastline: RowRange);

    (* Changes the scrolling region of window w to the new line         *)
    (* boundaries given, and sets its cursor to the start of the new    *)
    (* scrolling region.  The line numbers are window-relative.         *)

    VAR horizontal, vertical, leftT, rightT: CHAR;
        j: ColumnRange;
        place: BufferSubscript;

    BEGIN
        (* Work out what characters to use for the frame and divider.   *)

        horizontal := 'Ä';  vertical := '³';
        leftT := 'Ã';  rightT := '´';
        WITH w^ DO
            IF divider = doubledivider THEN
                horizontal := 'Í';
            END (*IF*);
            IF frame = doubleframe THEN
                vertical := 'º';
                IF divider = doubledivider THEN
                    leftT := 'Ì';  rightT := '¹';
                ELSE
                    leftT := '³';  rightT := '³';
                END (*IF*);
            ELSIF divider = doubledivider THEN
                leftT := '³';  rightT := '³';
            END (*IF*);

            (* Clean up the frame. *)

            ScrollRegion := DefaultScrollRegion;
            IF frame <> noframe THEN

                (* Remove the left and right T belonging to the *)
                (* old divider bars, if necessary.              *)

                IF ScrollRegion.top > 1 THEN
                    place := Columns*(ScrollRegion.top - 1);
                    bufptr^[place].val := vertical;
                    bufptr^[place + Columns - 1].val := vertical;
                END (*IF*);

                IF ScrollRegion.bottom < Rows - 2 THEN
                    place := Columns*(ScrollRegion.bottom + 1);
                    bufptr^[place].val := vertical;
                    bufptr^[place + Columns - 1].val := vertical;
                END (*IF*);
            END (*IF*);

            (* Put in the new divider bars.     *)

            IF divider <> nodivider THEN

                (* Put in the top horizontal bar.       *)

                IF firstline > 1 THEN
                    place := Columns*(firstline-1);
                    IF frame <> noframe THEN
                        bufptr^[place].val := leftT;  INC (place);
                    END (*IF*);
                    FOR j := ScrollRegion.left TO ScrollRegion.right DO
                        bufptr^[place].val := horizontal;  INC(place);
                    END (*FOR*);
                    IF frame <> noframe THEN
                        bufptr^[place].val := rightT;
                    END (*IF*);
                END (*IF*);

                (* Put in the bottom horizontal bar.    *)

                IF lastline < Rows - 2 THEN
                    place := Columns*(lastline+1);
                    IF frame <> noframe THEN
                        bufptr^[place].val := leftT;  INC (place);
                    END (*IF*);
                    FOR j := ScrollRegion.left TO ScrollRegion.right DO
                        bufptr^[place].val := horizontal; INC (place);
                    END (*FOR*);
                    IF frame <> noframe THEN
                        bufptr^[place].val := rightT;
                    END (*IF*);
                END (*IF*);

            END (*IF*);

            (* Finally, update the scrolling region parameters. *)

            WITH ScrollRegion DO
                top := firstline;  bottom := lastline;
            END (*WITH*);
            DefaultScrollRegion := ScrollRegion;
            SetCursor (w, firstline, ScrollRegion.left);
            Obtain (StackAccess2);
            IF NOT (hidden OR obscured) THEN
                Refresh (w);
            END (*IF*);
            Release (StackAccess2);
        END (*WITH*);
    END ChangeScrollingRegion;

(************************************************************************)

PROCEDURE NewScrollingRegion (w: Window;  firstline, lastline: RowRange;
                                firstcolumn, lastcolumn: ColumnRange);

    (* Changes the scrolling region of w to be the specified rectangle, *)
    (* but unlike ChangeScrollingRegion this procedure does not redraw  *)
    (* the dividers.  Furthermore the old scrolling region set by       *)
    (* ChangeScrollingRegion is remembered and may be restored by a     *)
    (* call to ResetScrollingRegion.                                    *)

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                top := firstline;  bottom := lastline;
                left := firstcolumn;  right := lastcolumn;
            END (*WITH*);
        END (*WITH*);
    END NewScrollingRegion;

(************************************************************************)

PROCEDURE ResetScrollingRegion (w: Window);

    (* Changes the scrolling region of w back to what it was the last   *)
    (* time ChangeScrollingRegion was called.  If ChangeScrollingRegion *)
    (* was never called, the scrolling region goes back to being the    *)
    (* entire window minus the frame (if any).                          *)

    BEGIN
        w^.ScrollRegion := w^.DefaultScrollRegion;
    END ResetScrollingRegion;

(************************************************************************)
(*                          OPENING A WINDOW                            *)
(************************************************************************)

PROCEDURE ClearWindow (w: Window);

    (* Removes all text from the default scrolling region.   *)

    VAR k, count: CARDINAL;

    BEGIN
        WITH w^ DO
            Wait (access0p5);
            count := DefaultScrollRegion.right + 1 - DefaultScrollRegion.left;
            Obtain (BlankRowAccess1);
            IF count > 0 THEN
                FOR k := 0 TO count-1 DO
                    BlankRowPtr^[k].attr := CurrentAttributes;
                END (*FOR*);
            END (*IF*);
            count := BytesPerChar*count;
            FOR k := DefaultScrollRegion.top TO DefaultScrollRegion.bottom DO
                Copy (BlankRowPtr,
                      ADR(bufptr^[k*Columns+DefaultScrollRegion.left]), count);
            END (*FOR*);
            Release (BlankRowAccess1);
            Signal (access0p5);
        END (*WITH*);
        IF NOT w^.hidden THEN
            Refresh (w);
        END (*IF*);
    END ClearWindow;

(************************************************************************)

PROCEDURE FillInFrame (w: Window);

    (* Puts the box around the window into the window buffer.   *)

    VAR i: RowRange;  j: ColumnRange;
        corner: ARRAY [1..4] OF ScreenChar;
        horizontal, vertical: ScreenChar;
        place, offset: BufferSubscript;

    BEGIN
        IF w^.frame = simpleframe THEN
            corner[1].val := 'Ú';  corner[2].val := '¿';
            corner[3].val := 'À';  corner[4].val := 'Ù';
            horizontal.val := 'Ä';  vertical.val := '³';
        ELSE
            corner[1].val := 'É';  corner[2].val := '»';
            corner[3].val := 'È';  corner[4].val := '¼';
            horizontal.val := 'Í';  vertical.val := 'º';
        END (*IF*);

        WITH w^ DO
            horizontal.attr := CurrentAttributes;
            vertical.attr := CurrentAttributes;
            FOR j := 1 TO 4 DO
                corner[j].attr := CurrentAttributes;
            END (*FOR*);

            place := Columns;
            offset := Columns - 1;

            bufptr^[0] := corner[1];
            bufptr^[offset] := corner[2];

            IF Rows > 2 THEN
                FOR i := 1 TO Rows - 2 DO
                    bufptr^[place] := vertical;
                    bufptr^[place+offset] := vertical;
                    INC (place, Columns);
                END (*FOR*);
            END (*IF*);

            bufptr^[place] := corner[3];
            bufptr^[place+offset] := corner[4];

            offset := Columns*(Rows - 1);
            IF Columns > 2 THEN
                FOR j := 1 TO Columns-2 DO
                    INC (place);
                    bufptr^[place-offset] := horizontal;
                    bufptr^[place] := horizontal;
                END (*FOR*);
            END (*IF*);

        END (*WITH*);
    END FillInFrame;

(************************************************************************)

PROCEDURE MakeMonochrome (VAR (*INOUT*) foreground, background: Colour);

    (* Changes the two given colours to a suitable B/W combination.     *)

    BEGIN
        IF (foreground = black) OR (foreground = darkgrey) THEN
            background := white
        ELSE
            IF foreground > white THEN
                foreground := intensewhite
            ELSE
                foreground := white;
            END (*IF*);
            background := black;
        END (*IF*);
    END MakeMonochrome;

(************************************************************************)

PROCEDURE OpenWindowHidden (VAR (*OUT*) w: Window;
                        ForegroundColour, BackgroundColour: Colour;
                        firstline, lastline: RowRange;
                        firstcol, lastcol: ColumnRange;
                        FrameDesired: FrameType;
                        DividerDesired: DividerType);

    (* Like OpenWindow, but the window does not appear on the screen    *)
    (* until the first PutOnTop(w).                                     *)

    BEGIN
        (* Check for parameters out of range. *)

        IF firstline > MaxRowNumber THEN firstline := MaxRowNumber END(*IF*);
        IF lastline > MaxRowNumber THEN lastline := MaxRowNumber END(*IF*);
        IF firstcol > MaxColumnNumber THEN firstcol := MaxColumnNumber END(*IF*);
        IF lastcol > MaxColumnNumber THEN lastcol := MaxColumnNumber END(*IF*);

        (* Create the new window, and fill in all its fields.   *)

        NEW (w);
        WITH w^ DO
            CreateSemaphore (access0p5, 0);
            CloseList := NIL;
            previous := NIL;  next := NIL;  blockcursor := FALSE;
            page := DefaultPage;  hidden := TRUE;
            InputWaiting := FALSE;  CursorWanted := FALSE;  InsertMode := TRUE;
            WrapOption := TRUE;  obscured := FALSE;
            CreateSemaphore (CharAvailable, 0);
            foreground := ForegroundColour;  background := BackgroundColour;
            IF BlackAndWhite OR ForcedMonochrome THEN
                MakeMonochrome (foreground, background);
            END (*IF*);
            CurrentAttributes := 16*ORD(background) + ORD(foreground);
            frame := FrameDesired;  divider := DividerDesired;
            FirstRow := firstline;  FirstColumn := firstcol;
            Rows := lastline - firstline + 1;
            Columns := lastcol - firstcol + 1;

            (* Allocate a data buffer. *)

            ALLOCATE64 (bufptr, BytesPerChar * Rows * Columns);

            (* Set up a default scrolling region.       *)

            WITH ScrollRegion DO
                top := 0;  bottom := Rows - 1;
                left := 0;  right := Columns - 1;
            END (*WITH*);

            (* Make the frame.  *)

            IF frame <> noframe THEN
                FillInFrame(w);
                WITH ScrollRegion DO
                    INC (top);  INC (left);
                    IF bottom > 0 THEN
                        DEC (bottom);
                    END (*IF*);
                    IF right > 0 THEN
                        DEC (right);
                    END (*IF*);
                END (*WITH*);
            END (*IF*);

            DefaultScrollRegion := ScrollRegion;
            row := ScrollRegion.top;  column := ScrollRegion.left;
            BufferPosition := Columns*row + column;
            Signal (access0p5);

        END (*WITH*);

        ClearWindow (w);

    END OpenWindowHidden;

(************************************************************************)

PROCEDURE OpenWindow (VAR (*OUT*) w: Window;
                        ForegroundColour, BackgroundColour: Colour;
                        firstline, lastline: RowRange;
                        firstcol, lastcol: ColumnRange;
                        FrameDesired: FrameType;
                        DividerDesired: DividerType);

    (* Creates a new window, and makes it the current window, filled    *)
    (* initially with space characters.                                 *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        OpenWindowHidden (w, ForegroundColour, BackgroundColour,
                        firstline, lastline, firstcol, lastcol,
                        FrameDesired, DividerDesired);
        PutOnTop (w);
    END OpenWindow;

(************************************************************************)

PROCEDURE OpenSimpleWindow (VAR (*OUT*) w: Window;
                        firstline, lastline: RowRange;
                        firstcol, lastcol: ColumnRange);

    (* Identical to OpenWindow, except that you don't get any choice    *)
    (* about the colours or frame.  The window is white-on-black with   *)
    (* a simple frame and no dividers for the scrolling region.  This   *)
    (* version of OpenWindow is useful for those with monochrome        *)
    (* displays who don't want to be bothered with importing the types  *)
    (* Colour, FrameType, and DividerType.                              *)

    BEGIN
        OpenWindow (w, white, black, firstline, lastline,
                        firstcol, lastcol, simpleframe, nodivider);
    END OpenSimpleWindow;

(************************************************************************)
(*                 CHANGING THE POSITION OF A WINDOW                    *)
(************************************************************************)

PROCEDURE ShiftWindowRel (w: Window;  rowchange, columnchange: INTEGER);

    (* Moves w on the screen.  The second and third arguments may be    *)
    (* negative.  The amount of move may be reduced to prevent a move   *)
    (* off the edge of the screen.                                      *)
    (* The caller must be executing at level <0.5.                      *)

    VAR wasvisible: BOOLEAN;

    BEGIN
        IF (rowchange <> 0) OR (columnchange <> 0) THEN
            WITH w^ DO
                (* Temporarily remove the window from the stack of windows. *)

                wasvisible := NOT hidden;  Hide(w);

                Wait (access0p5);

                (* Clip the shift amount to avoid going off the screen. *)

                IF VAL(INTEGER,FirstRow)+rowchange < 0 THEN
                    rowchange := - VAL(INTEGER,FirstRow)
                ELSIF VAL(INTEGER,FirstRow + Rows - 1)+rowchange > VAL(INTEGER,MaxRowNumber) THEN
                    rowchange := MaxRowNumber - (FirstRow + Rows - 1)
                END (*IF*);

                IF VAL(INTEGER,FirstColumn)+columnchange < 0 THEN
                    columnchange := -VAL(INTEGER,FirstColumn)
                ELSIF VAL(INTEGER,Columns + FirstColumn - 1)+columnchange > VAL(INTEGER,MaxColumnNumber) THEN
                    columnchange := MaxColumnNumber - (Columns + FirstColumn - 1)
                END (*IF*);

                (* Adjust the affected window parameters.       *)

                IF rowchange > 0 THEN
                    INC (FirstRow, rowchange);
                ELSE
                    rowchange := -rowchange;
                    DEC (FirstRow, rowchange);
                END (*IF*);

                IF columnchange > 0 THEN
                    INC (FirstColumn, columnchange);
                ELSE
                    columnchange := -columnchange;
                    DEC (FirstColumn, columnchange);
                END (*IF*);

                Signal (access0p5);

                (* Put w back onto the stack and onto the screen.       *)

                IF wasvisible THEN
                    hidden := FALSE;  PutOnTop (w);
                END (*IF*);

            END (*WITH*);

        END (*IF*);

    END ShiftWindowRel;

(************************************************************************)

PROCEDURE ShiftWindowAbs (w: Window;  top: RowRange;  left: ColumnRange);

    (* Like ShiftWindowRel, except that we directly specify the target  *)
    (* position of the top left corner in screen coordinates.           *)

    BEGIN
        ShiftWindowRel (w, VAL(INTEGER,top)-VAL(INTEGER,w^.FirstRow),
                                VAL(INTEGER,left)-VAL(INTEGER,w^.FirstColumn));
    END ShiftWindowAbs;

(************************************************************************)

PROCEDURE WindowLocation (w: Window): Rectangle;

    (* Returns the current location of w on the screen. *)

    VAR result: Rectangle;

    BEGIN
        WITH w^ DO
            WITH result DO
                top := FirstRow;  bottom := FirstRow + Rows - 1;
                left := FirstColumn;  right := Columns + FirstColumn - 1;
            END (*WITH*);
        END (*WITH*);
        RETURN result;
    END WindowLocation;

(************************************************************************)
(*                          CLOSING A WINDOW                            *)
(************************************************************************)

PROCEDURE InstallCloseHandler (w: Window;  P: CloseHandlerProc);

    (* Sets up P as a procedure to be called when the window is closed. *)
    (* It is legal to define multiple handlers for the same window.     *)

    VAR HLP: CloseHandlerList;

    BEGIN
        NEW (HLP);
        WITH w^ DO
            Wait (access0p5);
            HLP^.next := CloseList;
            HLP^.proc := P;
            CloseList := HLP;
            Signal (access0p5);
        END (*WITH*);
    END InstallCloseHandler;

(************************************************************************)

PROCEDURE CloseWindow (w: Window);

    (* Reclaims the buffer space used for this window, and removes its  *)
    (* image on the screen.                                             *)
    (* The caller must be executing at level <0.5.                      *)

    VAR p: CloseHandlerList;

    BEGIN
        Hide (w);
        WITH w^ DO
            Wait (access0p5);
            WHILE CloseList <> NIL DO
                p := CloseList^.next;
                CloseList^.proc (w, w^.page);
                DISPOSE (CloseList);
                CloseList := p;
            END (*WHILE*);
            Signal (access0p5);
            DestroySemaphore (access0p5);
            DEALLOCATE (bufptr, BytesPerChar * Rows * Columns);
        END (*WITH*);
        DISPOSE (w);
    END CloseWindow;

(************************************************************************)
(*                        CHANGING OPTIONS                              *)
(************************************************************************)

PROCEDURE SetWrapOption (w: Window;  enabled: BOOLEAN);

    (* If the parameter is TRUE - this is the initial default - then    *)
    (* subsequent text written to the window will wrap to the next      *)
    (* line when it hits the right of the scrolling region.  Setting    *)
    (* the parameter to FALSE disables this feature.                    *)

    BEGIN
        w^.WrapOption := enabled;
    END SetWrapOption;

(************************************************************************)
(*              OPERATIONS ON CHARACTER ATTRIBUTES                      *)
(************************************************************************)

PROCEDURE SetColours (w: Window; r: RowRange; c: ColumnRange;
                                nchar: CARDINAL;  fore, back: Colour);

    (* Sets a field of nchar characters, starting at (row,col), to      *)
    (* the specified foreground and background colours.  The location   *)
    (* is given in window-relative coordinates, not absolute screen     *)
    (* positions.  NOTE: This procedure will not wrap around to a new   *)
    (* row.  The caller must be executing at level <3.                  *)

    VAR k, start: BufferSubscript;  attributes: CARD8;

    BEGIN
        attributes := 16*ORD(back) + ORD(fore);
        WITH w^ DO
            start := Columns*r + c;
            FOR k := start TO start+nchar-1 DO
                bufptr^[k].attr := attributes;
            END (*FOR*);

            INC (r, FirstRow);  INC (c, FirstColumn);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSIF page = ActivePage THEN
                    OS2.VioWrtNAttr(attributes, nchar, r, c, 0);
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END SetColours;

(************************************************************************)

PROCEDURE ColourSwap (w: Window; r: RowRange; c: ColumnRange;
                                                        nchar: CARDINAL);

    (* Switches the foreground and background colours for nchar         *)
    (* characters, starting at location (r,c).  The row and column      *)
    (* numbers are window-relative, not absolute screen coordinates.    *)
    (* This is our colour equivalent of the "reverse video" operation.  *)
    (* NOTE: This procedure will not wrap around to a new row.          *)
    (* The caller must be executing at level <3.                        *)

    VAR k, start: BufferSubscript;  oldattribute: CARD8;

    BEGIN
        WITH w^ DO
            start := Columns*r + c;
            FOR k := start TO start+nchar-1 DO
                oldattribute := bufptr^[k].attr;
                bufptr^[k].attr := 16*(oldattribute MOD 16)
                                        + (oldattribute DIV 16);
            END (*FOR*);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSIF page = ActivePage THEN
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[start], 2*nchar, r+FirstRow, c+FirstColumn, 0);
                    Release (ScreenAccess3);
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END ColourSwap;

(************************************************************************)

PROCEDURE Blink (w: Window; r: RowRange; c: ColumnRange; nchar: CARDINAL);

    (* Toggles the blinking status - that is, turns blinking on if it   *)
    (* was off, and vice versa - for nchar characters, starting at      *)
    (* relative location (r,c) in window w.                             *)
    (* NOTE: This procedure will not wrap around to a new row.          *)
    (* The caller must be executing at level <3.                        *)

    VAR k, start: BufferSubscript;

    BEGIN
        WITH w^ DO
            start := Columns*r + c;
            FOR k := start TO start+nchar-1 DO
                bufptr^[k].attr := IXORB(bufptr^[k].attr, 80H);
            END (*FOR*);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w)
                ELSIF page = ActivePage THEN
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[start], 2*nchar, r+FirstRow, c+FirstColumn, 0);
                    Release (ScreenAccess3);
                END (*IF obscured*);
                Release (StackAccess2);
            END (*IF NOT hidden*);
        END (*WITH*);
    END Blink;

(************************************************************************)
(*                          CURSOR OPERATIONS                           *)
(************************************************************************)

PROCEDURE SetCursor (w: Window; r: RowRange; c: ColumnRange);

    (* Sets the cursor for window w to relative row r, column c.        *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        WITH w^ DO
            Wait (access0p5);
            row := r;  column := c;
            BufferPosition := Columns*r + c;
            Signal (access0p5);
        END (*WITH*);
    END SetCursor;

(************************************************************************)

PROCEDURE SaveCursor (w: Window; VAR (*OUT*) r, c: CARDINAL);

    (* Returns the current cursor position of window w.         *)

    BEGIN
        WITH w^ DO
            r := row;  c := column;
        END (*WITH*);
    END SaveCursor;

(************************************************************************)

PROCEDURE CursorLeft (w: Window);

    (* Moves the window cursor one position left.  If it falls off the  *)
    (* left edge of the window, move to the right edge in the same row. *)

    BEGIN
        WITH w^ DO
            IF column = 0 THEN
                column := Columns - 1;
                BufferPosition := Columns*row + column;
            ELSE
                DEC (column);  DEC (BufferPosition);
            END (*IF*);
        END (*WITH*);
    END CursorLeft;

(************************************************************************)

PROCEDURE CursorRight (w: Window);

    (* Moves the window cursor one position right.  If it falls off the *)
    (* right edge of the window, move to the left edge in the same row. *)

    BEGIN
        WITH w^ DO
            IF column = Columns-1 THEN
                column := 0;
                BufferPosition := Columns*row;
            ELSE
                INC (column);  INC (BufferPosition);
            END (*IF*);
        END (*WITH*);
    END CursorRight;

(************************************************************************)

PROCEDURE CursorUp (w: Window);

    (* Moves the window cursor one position up.  If it falls off the    *)
    (* top edge of the window, it moves to the bottom edge in the same  *)
    (* column.                                                          *)

    BEGIN
        WITH w^ DO
            IF row = 0 THEN
                row := Rows - 1;
                BufferPosition := Columns*row + column;
            ELSE
                DEC (row);  DEC (BufferPosition, Columns);
            END (*IF*);
        END (*WITH*);
    END CursorUp;

(************************************************************************)

PROCEDURE CursorDown (w: Window);

    (* Moves the window cursor one position down.  If it falls off the  *)
    (* bottom edge of the window, it moves to the top edge in the same  *)
    (* column.                                                          *)

    BEGIN
        WITH w^ DO
            IF row = Rows-1 THEN
                row := 0;
                BufferPosition := column;
            ELSE
                INC (row);  INC (BufferPosition, Columns);
            END (*IF*);
        END (*WITH*);
    END CursorDown;

(************************************************************************)

PROCEDURE ScrollUpI (w: Window);

    (* The version of ScrollUp (see below) for internal use.            *)
    (* The caller must be executing at level <1.                        *)

    VAR rownum: RowRange;  count: CARDINAL;
        k: BufferSubscript;
        srcptr, destptr: FarPointer;

    BEGIN
        WITH w^ DO
            WITH ScrollRegion DO
                k := Columns*top + left;
                count := BytesPerChar*(right+1-left);
                destptr := Far(ADR(bufptr^[k]));

                (* Move the contents of the scrolling region up in the buffer. *)

                IF bottom > top THEN
                    FOR rownum := top TO bottom-1 DO
                        srcptr := FarAddOffset (destptr, BytesPerChar*Columns);
                        FarCopy (srcptr, destptr, count);
                        destptr := srcptr;
                    END (*FOR*);
                END (*IF*);
                Obtain (BlankRowAccess1);

                (* Fill in the attributes of BlankRow. *)

                FOR k := 0 TO count DIV 2 DO
                    BlankRowPtr^[k].attr := CurrentAttributes;
                END (*FOR*);

                (* Blank the bottom line of scrolling region in the buffer. *)

                FarCopy (BlankRowPtr, destptr, count);

                (* That's the buffer done, now scroll what's on the screen. *)

                IF NOT(obscured OR hidden) AND (page = ActivePage) THEN
                    Obtain (ScreenAccess3);
                    OS2.VioScrollUp (FirstRow+top, FirstColumn+left,
                                 FirstRow+bottom, FirstColumn+right, 1, BlankRowPtr^[0], 0);
                    Release (ScreenAccess3);
                END (*IF*);

                Release (BlankRowAccess1);

            END (*WITH*);

            IF obscured AND NOT hidden THEN
                Obtain (StackAccess2);
                PutOnTopI(w);
                Release (StackAccess2);
            END (*IF*);

        END (*WITH*);

    END ScrollUpI;

(************************************************************************)

PROCEDURE ScrollUp (w: Window);

    (* Scrolls window w up by one line, both on the screen and in its   *)
    (* buffer.  The last row is filled with spaces.                     *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        Wait (w^.access0p5);
        ScrollUpI (w);
        Signal (w^.access0p5);
    END ScrollUp;

(************************************************************************)

PROCEDURE ScrollDown (w: Window);

    (* Scrolls window w down by one line, both on the screen and in its *)
    (* buffer.  The first row is filled with spaces.                    *)
    (* The caller must be executing at level <0.5.                      *)

    VAR k: BufferSubscript;

    BEGIN
        WITH w^ DO
            Wait (access0p5);
            Obtain (BlankRowAccess1);
            FOR k := 0 TO Columns-1 DO
                BlankRowPtr^[k].attr := CurrentAttributes;
            END (*FOR*);
            WITH ScrollRegion DO
                k := Columns * top;
                IF bottom > top THEN
                    CopyUp (Far(ADR(bufptr^[k])), Far(ADR(bufptr^[k+Columns])),
                                    BytesPerChar*Columns*(bottom-top));
                END (*IF*);
                Copy (BlankRowPtr, ADR(bufptr^[k+left]),
                        BytesPerChar*(right-left+1));
            END (*WITH*);
            Release (BlankRowAccess1);
            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w) ELSE Refresh (w);
                END (*IF*);
                Release (StackAccess2);
            END (*IF*);
            Signal (access0p5);
        END (*WITH*);
    END ScrollDown;

(************************************************************************)
(*                          MAIN OUTPUT ROUTINES                        *)
(************************************************************************)

PROCEDURE WriteLnI (w: Window);

    (* The internal version of WriteLn (see below).     *)
    (* The caller must be executing at level <1.        *)

    BEGIN
        WITH w^ DO
            IF InExtendedScrollingRegion (w) THEN
                column := ScrollRegion.left;
                IF row = ScrollRegion.bottom THEN ScrollUpI (w)
                ELSE INC (row);
                END (*IF*);
            ELSE
                column := DefaultScrollRegion.left;
                IF row >= Rows-1 THEN row := Rows - 1;
                ELSE INC (row);
                END (*IF*);
            END (*IF*);
            BufferPosition := Columns*row + column;
        END (*WITH*);
    END WriteLnI;

(************************************************************************)

PROCEDURE WriteLn (w: Window);

    (* Moves the cursor of window w to the start of the next row.  If   *)
    (* we are already at the last row, the window scrolls up.           *)

    BEGIN
        Wait (w^.access0p5);
        WriteLnI (w);
        Signal (w^.access0p5);
    END WriteLn;

(************************************************************************)

PROCEDURE WriteChar (w: Window; ch: CHAR);

    (* Writes one character to window w, and updates the cursor for     *)
    (* this window.  As a side-effect, this window becomes the          *)
    (* currently active window if it was obscured.  Wraps around to the *)
    (* next line if we are about to run off the end of the current      *)
    (* line.  This procedure does not recognise the concept of a        *)
    (* control character.  Every possible value of ch produces          *)
    (* something readable on the screen.                                *)
    (* The caller must be executing at level <0.5.                      *)

    BEGIN
        WITH w^ DO
            Wait (access0p5);

            (* Wrap to a new line if we about to leave the scrolling    *)
            (* region or if we are outside the legal writing region.    *)

            IF WrapOption AND ((column = ScrollRegion.right + 1)
                           OR (column > DefaultScrollRegion.right)) THEN
                DEC (column);  WriteLnI (w);
            END (*IF*);

            bufptr^[BufferPosition].val := ch;
            bufptr^[BufferPosition].attr := CurrentAttributes;

            IF NOT hidden THEN
                Obtain (StackAccess2);
                IF obscured THEN PutOnTopI(w) END (*IF*);
                IF page = ActivePage THEN
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[BufferPosition], 2,
                                      FirstRow+row, FirstColumn+column, 0);
                    Release (ScreenAccess3);
                END (*IF*);
                Release (StackAccess2);
            END (*IF NOT hidden*);

            (* Note that the following statement may cause column to    *)
            (* go beyond the edge of the window; but this will be       *)
            (* picked up on the next call to WriteChar.  We prefer not  *)
            (* to do a WriteLn just yet, because that could cause an    *)
            (* unintended scroll operation when writing to the bottom   *)
            (* right of the window.                                     *)

            INC (column);  INC (BufferPosition);

            Signal (access0p5);

        END (*WITH*);
    END WriteChar;

(************************************************************************)

PROCEDURE WriteString (w: Window; text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by  *)
    (* the end of the array.                                            *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF ORD (text[j]) = 0 THEN EXIT (*LOOP*)  END (*IF*);
            WriteChar (w, text[j]);  INC (j);
            IF j > HIGH (text) THEN EXIT (*LOOP*)  END (*IF*);
        END (*LOOP*);
    END WriteString;

(************************************************************************)

PROCEDURE WritePartString (w: Window;  VAR (*IN*) text: ARRAY OF CHAR;
                                                        j1, j2: CARDINAL);

    (* Writes text[j1..j2] (or less than this if we hit the end of      *)
    (* the string).                                                     *)

    VAR j: CARDINAL;

    BEGIN
        j := j1;
        WHILE (j <= j2) AND (j <= HIGH(text)) AND (ORD(text[j]) <> 0) DO
            WriteChar (w, text[j]);  INC (j);
        END (*WHILE*);
    END WritePartString;

(************************************************************************)

PROCEDURE Write (w: Window; ch: CHAR);

    (* A version of procedure WriteChar which looks after some of the   *)
    (* control characters.                                              *)

    BEGIN
        IF ch >= " " THEN WriteChar (w, ch)
        ELSIF ORD(ch) = 8 THEN          (* backspace *)
            CursorLeft(w)
        ELSIF ORD(ch) = 9 THEN          (* tab *)
            WITH w^ DO
                REPEAT
                    WriteChar (w, " ");
                UNTIL (column=Columns) OR (column DIV 8 = ScrollRegion.left);
            END (*WITH*);
        ELSIF ORD(ch) = 10 THEN         (* line feed - ignore *)
        ELSIF ORD(ch) = 13 THEN         (* carriage return *)
            WriteLn(w)
        ELSE                            (* other control character *)
            WriteChar (w, "^");  WriteChar (w, CHR(ORD(ch)+64))
        END (*IF*);
    END Write;

(************************************************************************)
(*                              INPUT                                   *)
(************************************************************************)

PROCEDURE ReadBack (w: Window;  r: RowRange;  c: ColumnRange): CHAR;

    (* Returns the character which currently occupies relative location *)
    (* (r,c) on the display of window w.                                *)

    BEGIN
        WITH w^ DO
            RETURN bufptr^[Columns*r + c].val;
        END (*WITH*);
    END ReadBack;

(************************************************************************)

PROCEDURE KeyTask;

    (* Runs as a separate task, getting a character from the keyboard   *)
    (* as needed and making it available to the task which has input    *)
    (* focus.  If no task has input focus, the character is returned    *)
    (* to the keyboard module.                                          *)

    VAR ch: CHAR;  w: Window;  dummy: BOOLEAN;

    BEGIN
        LOOP
            TimedWait (InputRequest, 900, dummy);
            ch := InKey();
            WITH PhysicalCursor DO
                Obtain (access4);
                w := CursorWindow[ActivePage];
                CursorWindow[ActivePage] := NIL;
                CursorVisible[ActivePage] := FALSE;
                Release (access4);
            END (*WITH*);
            IF w = NIL THEN
                PutBack(ch);
            ELSE
                WITH w^ DO
                    InputChar := ch;
                    InputWaiting := FALSE;
                    ComputeCursorWindow (ActivePage);
                    Signal (CharAvailable);
                END (*WITH*);
            END (*IF*);
        END (*LOOP*);
    END KeyTask;

(************************************************************************)

PROCEDURE GetKey (w: Window): CHAR;

    (* Read one character, without any prompt to the user (unless the   *)
    (* caller has already set w^.CursorWanted to TRUE).  The reason for *)
    (* specifying a window parameter is to ensure that keyboard input   *)
    (* comes to us only when this window has input focus.               *)

    BEGIN
        w^.InputWaiting := TRUE;
        ComputeCursorWindow (w^.page);
        Signal (InputRequest);
        Wait (w^.CharAvailable);
        RETURN w^.InputChar;
    END GetKey;

(************************************************************************)

PROCEDURE ReadCharWithoutEcho (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Read one character, with a blinking cursor in window w as a      *)
    (* prompt.                                                          *)

    VAR SaveCursorState: BOOLEAN;

    BEGIN
        SaveCursorState := w^.CursorWanted;
        w^.CursorWanted := TRUE;
        ch := GetKey (w);
        w^.CursorWanted := SaveCursorState;
    END ReadCharWithoutEcho;

(************************************************************************)

PROCEDURE ReadChar (w: Window;  VAR (*OUT*) ch: CHAR);

    (* Like ReadCharWithoutEcho, but the input character is echoed.     *)

    BEGIN
        ReadCharWithoutEcho (w, ch);  Write (w, ch);
    END ReadChar;

(************************************************************************)

PROCEDURE LookaheadChar (w: Window): CHAR;

    (* Reads a character without consuming it.  That is, the character  *)
    (* remains available to be read by ReadChar.  This allows the       *)
    (* caller to check whether the character is really wanted.          *)

    VAR ch: CHAR;

    BEGIN
        ch := GetKey(w);  PutBack (ch);
        RETURN ch;
    END LookaheadChar;

(************************************************************************)

PROCEDURE PressAnyKey (w: Window);

    (* Types a "Press any key to continue" message.     *)

    VAR dummy: CHAR;

    BEGIN
        WriteLn (w);
        WriteString (w, "Press any key to continue.");
        ReadCharWithoutEcho (w, dummy);
        IF ORD(dummy) = 0 THEN ReadCharWithoutEcho (w, dummy) END (*IF*);
        EraseLine (w, 0);
    END PressAnyKey;

(************************************************************************)

PROCEDURE ReadString (w: Window;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Reads a character string, terminated by carriage return.         *)

    VAR j: CARDINAL;  ch: CHAR;

    BEGIN
        FOR j := 0 TO HIGH(result) DO
            result[j] := " ";
        END (*FOR*);
        j := 0;
        LOOP
            ReadChar (w, ch);
            IF ORD(ch) = 13 THEN
                result[j] := CHR(0);  EXIT(*LOOP*)
            ELSIF ORD(ch) = 8 THEN      (* backspace *)
                IF j > 0 THEN
                    CursorLeft(w);  WriteChar(w, " ");  CursorLeft(w);
                    DEC (j);
                END (*IF*);
            ELSE
                result[j] := ch;
                IF j = HIGH(result) THEN EXIT(*LOOP*) END(*IF*);
                INC (j);
            END(*IF*);
        END (*LOOP*);
    END ReadString;

(************************************************************************)

PROCEDURE InsertMode (w: Window;  Insert: BOOLEAN);

    (* Sets the window's cursor type to overstrike (Insert=FALSE) or    *)
    (* insert (Insert=TRUE) mode.  This affects the initial state for   *)
    (* the next call to EditString.  It does not affect WriteChar and   *)
    (* similar screen writing operations.                               *)

    BEGIN
        w^.InsertMode := Insert;
    END InsertMode;

(************************************************************************)

PROCEDURE EditString (w: Window;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                  MaxResultChars, fieldsize: CARDINAL);

    (* Reads a character string, where a default result is supplied by  *)
    (* the caller.  The final result is the state of the string at the  *)
    (* time where the keyboard user types a carriage return or Esc, or  *)
    (* uses a cursor movement key to move out of the displayed field.   *)
    (* The terminating character remains available, via Keyboard.InKey, *)
    (* to the caller.  At most MaxResultChars characters of the string  *)
    (* can be edited, and perhaps fewer if the result array is smaller. *)
    (* The last parameter is the width of the editing field.            *)

    CONST Esc = CHR(01BH);  Space = " ";

    (* Some important variables:                                        *)
    (*   OffLeft  the number of characters that have scrolled off the   *)
    (*            left edge of the editing field.                       *)
    (*   place    current character is result[place].  In the editing   *)
    (*            field it's at position [place-OffLeft].               *)
    (*   limitr   we may edit only result[0..limitr]                   *)
    (*   limits   on screen, editing positions are in range [0..limits] *)

    VAR OffLeft, place, k: CARDINAL;  ch: CHAR;  limitr, limits: CARDINAL;
        SavedAttributes: CARD8;
        startrow, startcolumn: CARDINAL;
        SavedCursorType: BOOLEAN;

    (********************************************************************)

    PROCEDURE RewriteString ();

        BEGIN
            SetCursor (w, startrow, startcolumn);
            WritePartString (w, result, OffLeft, OffLeft+limits);
            SetCursor (w, startrow, startcolumn+place-OffLeft);
        END RewriteString;

    (********************************************************************)

    PROCEDURE GoToEnd;

        (* Puts the cursor just after the last non-blank character.     *)

        BEGIN
            place := limitr+1;
            WHILE (place > 0) AND (result[place-1] = Space) DO
                DEC (place);
            END (*WHILE*);
            IF place > limits + OffLeft THEN
                OffLeft := place - limits - 1;
                RewriteString();
            END (*IF*);
            SetCursor (w, startrow, startcolumn+place-OffLeft);
        END GoToEnd;

    (********************************************************************)

    PROCEDURE HandleControlChar(): BOOLEAN;

        (* Called after detecting the CHR(0) which means that a control *)
        (* character has been typed.  Performs the appropriate actions, *)
        (* returns TRUE iff editing is finished.                        *)

        VAR k: CARDINAL;

        BEGIN
            ch := GetKey (w);
            IF ch = "K" THEN                            (* cursor left *)
                IF place = 0 THEN
                    PutBack(ch);  PutBack(CHR(0));
                    RETURN TRUE;
                ELSIF place <= OffLeft THEN
                    DEC (OffLeft);  DEC (place);
                ELSE
                    CursorLeft(w);  DEC (place);
                END (*IF*);
            ELSIF ch = "M" THEN                         (* cursor right *)
                IF place > limitr THEN
                    PutBack(ch);  PutBack(CHR(0));
                    RETURN TRUE;
                ELSIF place-OffLeft > limits THEN
                    INC (OffLeft);  INC (place);
                ELSE
                    CursorRight(w);  INC (place);
                END (*IF*);
            ELSIF (ch = "H") OR (ch = "P") THEN         (* cursor up/down *)
                PutBack(ch);  PutBack(CHR(0));
                RETURN TRUE;
            ELSIF ch = "G" THEN                         (* home *)
                place := 0;  OffLeft := 0;
                SetCursor (w, startrow, startcolumn);
            ELSIF ch = "O" THEN                         (* end *)
                GoToEnd;
            ELSIF ch = "R" THEN                         (* insert *)
                w^.blockcursor := w^.InsertMode;
                w^.InsertMode := NOT w^.InsertMode;
            ELSIF ch = "S" THEN                         (* delete right *)
                IF place < limitr THEN
                    FOR k := place TO limitr-1 DO
                        result[k] := result[k+1];
                    END (*FOR*);
                END (*IF*);
                IF place <= limitr THEN
                    result[limitr] := Space;
                    RewriteString ();
                END (*IF*);
            END (*IF*);
            RETURN FALSE;
        END HandleControlChar;

    (********************************************************************)

    BEGIN       (* Body of EditString *)

        (* Special case: can't do anything with zero field sizes. *)

        IF (MaxResultChars = 0) OR (fieldsize = 0) THEN
            RETURN;
        END (*IF*);

        SaveCursor (w, startrow, startcolumn);
        SavedCursorType := w^.blockcursor;
        w^.blockcursor := NOT w^.InsertMode;

        (* Compute a limit (limits) which stops us from running off the *)
        (* window, and another limit (limitr) that stops us from        *)
        (* running off the end of the string.                           *)

        limitr := MaxResultChars-1;
        IF limitr > HIGH(result) THEN
            limitr := HIGH(result);
        END (*IF*);
        WITH w^ DO
            IF InScrollingRegion(w) THEN
                limits := ScrollRegion.right;
            ELSE
                limits := DefaultScrollRegion.right;
            END (*IF*);
            DEC (limits, startcolumn);
            SavedAttributes := CurrentAttributes;
        END (*WITH*);
        IF fieldsize <= limits THEN
            limits := fieldsize - 1;
        END (*IF*);

        (* Preprocessing: for a Nul-terminated string, remove the Nul   *)
        (* and pad out the string with spaces at the right.  Otherwise  *)
        (* we get problems if, for example, the Nul is deleted.         *)

        place := 0;
        LOOP
            IF result[place] = CHR(0) THEN
                FOR k := place TO limitr DO
                    result[k] := Space;
                END (*FOR*);
                EXIT (*LOOP*);
            END (*IF*);
            IF place = limitr THEN EXIT(*LOOP*) END(*IF*);
            INC (place);
        END (*LOOP*);

        (* Write the string, using reverse video.       *)

        WritePartString (w, result, 0, limits);
        ColourSwap (w, startrow, startcolumn, limits+1);
        WITH w^ DO
            CurrentAttributes := 16*ORD(foreground) + ORD(background);
        END (*WITH*);
        place := 0;  OffLeft := 0;
        SetCursor (w, startrow, startcolumn);

        (* Now the main editing loop.   *)

        LOOP
            ReadCharWithoutEcho (w, ch);
            IF ORD(ch) = 0 THEN                         (* control char *)
                IF HandleControlChar() THEN
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF (ch = Esc) OR (ORD(ch) = 13) THEN     (* Esc or Return *)
                PutBack(ch);  EXIT(*LOOP*);
            ELSIF ORD(ch) = 8 THEN                      (* delete left *)
                IF place > 0 THEN
                    IF place <= OffLeft THEN
                        DEC (OffLeft);
                    END (*IF*);
                    DEC (place);
                    IF place < limitr THEN
                        FOR k := place TO limitr-1 DO
                            result[k] := result[k+1];
                        END (*FOR*);
                    END (*IF*);
                    result[limitr] := Space;
                END (*IF*);
            ELSIF place > limitr THEN                   (* run off end *)
                PutBack(ch);  EXIT(*LOOP*);
            ELSE                                        (* any other char *)
                IF place-OffLeft > limits THEN
                    INC (OffLeft);
                END (*IF*);
                IF w^.InsertMode THEN
                    FOR k := limitr TO place+1 BY -1 DO
                        result[k] := result[k-1];
                    END (*FOR*);
                END (*IF*);
                result[place] := ch;  INC (place);
            END(*IF*);
            RewriteString ();
        END (*LOOP*);

        (* Postprocessing: remove the trailing spaces. *)

        place := limitr;
        WHILE result[place] = ' ' DO
            result[place] := CHR(0);
            IF place > 0 THEN DEC(place) END(*IF*);
        END (*WHILE*);

        ColourSwap (w, startrow, startcolumn, limits+1);
        w^.blockcursor := SavedCursorType;
        w^.CurrentAttributes := SavedAttributes;
    END EditString;

(************************************************************************)

PROCEDURE EditAborted (): BOOLEAN;

    (* Checks the next keyboard input.  Returns TRUE for Escape, FALSE  *)
    (* for anything else.  Escape or Carriage Return are consumed, any  *)
    (* other character is returned to the Keyboard module.              *)

    CONST Esc = CHR(01BH);  CR = CHR(0DH);

    VAR ch: CHAR;

    BEGIN
        ch := InKey();
        IF ch = Esc THEN RETURN TRUE
        ELSIF ch = CR THEN RETURN FALSE
        ELSE
            PutBack(ch);  RETURN FALSE;
        END (*IF*);
    END EditAborted;

(************************************************************************)
(*                  MISCELLANEOUS CONTROL OPERATIONS                    *)
(************************************************************************)

PROCEDURE GetScreenSize (VAR (*OUT*) rows, columns: CARDINAL);

    (* Returns the number of available screen rows and columns. *)

    BEGIN
        rows := MaxRowNumber + 1;
        columns := MaxColumnNumber + 1;
    END GetScreenSize;

(************************************************************************)

PROCEDURE EraseLine (w: Window;  option: CARDINAL);

    (* Replaces some or all of the current line, except for the border, *)
    (* with space characters.  The window cursor is moved to the        *)
    (* location of the first erased character.  The options are:        *)
    (*          0       the whole of the line, except for the border    *)
    (*          1       from the current cursor position onwards        *)
    (*          2       from the start to just before the cursor        *)
    (* If we are inside a scrolling region, then only that part of the  *)
    (* line inside the scrolling region is affected.                    *)

    VAR first, last: ColumnRange;
        k, firstk, lastk: BufferSubscript;

    BEGIN
        WITH w^ DO
            IF InScrollingRegion(w) THEN
                first := ScrollRegion.left;  last := ScrollRegion.right;
            ELSE
                first := DefaultScrollRegion.left;
                last := DefaultScrollRegion.right;
            END (*IF*);
            IF option = 1 THEN first := column;
            ELSIF option = 2 THEN last := column - 1;
            END (*IF*);
            IF last >= first THEN
                firstk := Columns*row + first;
                lastk := Columns*row + last;
                FOR k := firstk TO lastk DO
                    bufptr^[k].val := " ";
                    bufptr^[k].attr := CurrentAttributes;
                END (*FOR*);
                IF NOT(hidden OR obscured) AND (page = ActivePage) THEN
                    Obtain (ScreenAccess3);
                    OS2.VioWrtCellStr(w^.bufptr^[firstk], BytesPerChar*(last-first+1),
                                         FirstRow+row, FirstColumn+first, 0);
                    Release (ScreenAccess3);
                END (*IF*);
                column := first;  BufferPosition := firstk;
            END (*IF*);
        END (*WITH*);
    END EraseLine;

(************************************************************************)
(*                           TERMINATION                                *)
(************************************************************************)

(*
PROCEDURE DumpWindowLocks;

    (* For debugging: gives the current state of each lock belonging    *)
    (* to this module.                                                  *)

    BEGIN
        DumpString ("PageChangeListAccess: ");  DumpLockState (PageChangeListAccess);
        DumpEOL;
        DumpString ("BlankRowAccess1: ");  DumpLockState (BlankRowAccess1);
        DumpEOL;
        DumpString ("StackAccess2: ");  DumpLockState (StackAccess2);
        DumpEOL;
        DumpString ("ScreenAccess3: ");  DumpLockState (ScreenAccess3);
        DumpEOL;
        DumpString ("PhysicalCursor.access4: ");  DumpLockState (PhysicalCursor.access4);
        DumpEOL;
    END DumpWindowLocks;
*)

(************************************************************************)

PROCEDURE CleanUp;

    (* Phase 1 of module termination.  If termination was caused by an  *)
    (* error, displays the error and waits for the user to press a key. *)
    (* In order to ensure that the error message is not obscured, we    *)
    (* freeze all windows.                                              *)

    VAR w: Window;  p: DisplayPage;

    BEGIN

        (* Close all open windows.       *)

        FOR p := 0 TO MAX(DisplayPage) DO
            LOOP
                Obtain (StackAccess2);
                w := TopWindow[p];
                Release (StackAccess2);
                IF w = NIL THEN EXIT(*LOOP*) END(*IF*);
                CloseWindow (w);
            END (*LOOP*);
        END (*FOR*);

    END CleanUp;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

PROCEDURE SetScreenSize;

    (* Sets MaxRowNumber and MaxColumnNumber.  *)

    VAR vioModeInfoPtr: POINTER TO OS2.VIOMODEINFO;

    BEGIN
        MaxRowNumber := 24;
        MaxColumnNumber := 79;
        ALLOCATE64 (vioModeInfoPtr, SIZE(OS2.VIOMODEINFO));
        vioModeInfoPtr^.cb := SIZE (OS2.VIOMODEINFO);
        OS2.VioGetMode (vioModeInfoPtr^, 0);
        MaxRowNumber := vioModeInfoPtr^.row - 1;
        MaxColumnNumber := vioModeInfoPtr^.col - 1;
        DISPOSE (vioModeInfoPtr);
    END SetScreenSize;

(************************************************************************)

VAR j: BufferSubscript;  p: DisplayPage;
    OriginalAnsiIndicator: CARD16;

BEGIN
    SetScreenSize;
    ALLOCATE64 (BlankRowPtr, BytesPerChar*(MaxColumnNumber+1));
    FOR j := 0 TO MaxColumnNumber DO
        BlankRowPtr^[j].val := " ";
    END (*FOR*);
    CreateLock (BlankRowAccess1);

    FOR p := 0 TO MAX(DisplayPage) DO
        TopWindow[p] := NIL;
        PhysicalCursor.CursorWindow[p] := NIL;
        PhysicalCursor.CursorVisible[p] := FALSE;
    END (*FOR*);
    PhysicalCursor.ScreenPos := 0;
    PhysicalCursor.Attributes := 0;

    CreateLock (StackAccess2);
    CreateLock (ScreenAccess3);
    CreateLock (PhysicalCursor.access4);

    CreateSemaphore (InputRequest, 0);
    EVAL(CreateTask (KeyTask, 15, "keyboard/windows"));

    PageChangeProcs := NIL;
    CreateLock (PageChangeListAccess);

    (* Blank the screen, to erase otherwise annoying background stuff   *)
    (* left by other programs.                                          *)

    OS2.VioGetAnsi (OriginalAnsiIndicator, 0);
    OS2.VioSetAnsi (0, 0);
    SetActivePage (DefaultPage);
    Repaint (DefaultPage, 0,MaxRowNumber,0,MaxColumnNumber);

FINALLY

    CleanUp;
    OS2.VioSetAnsi (OriginalAnsiIndicator, 0);

END Windows.

