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

IMPLEMENTATION MODULE ListBoxes;

        (********************************************************)
        (*                                                      *)
        (*               Scrollable listboxes                   *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            2 December 1997                 *)
        (*  Last edited:        9 February 1999                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings;

FROM Windows IMPORT
    (* type *)  Window, Colour,
    (* proc *)  WriteString, SetCursor, EraseLine, ColourSwap,
                SetColours,
                NewScrollingRegion, ScrollUp, ScrollDown, GetKey;

FROM Keyboard IMPORT
    (* proc *)  PutBack;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    ScreenCols = 120;

TYPE
    ScreenRow = ARRAY [0..ScreenCols-1] OF CHAR;
    TextPointer = POINTER TO ScreenRow;

    RowPointer = POINTER TO
                   RECORD
                       itemnumber: CARDINAL;
                       prev, next: RowPointer;
                       this: TextPointer;
                   END (*RECORD*);

    (* A ListBox record contains the following fields.                  *)
    (*    Access      mutual exclusion lock                             *)
    (*    win         the screen window                                 *)
    (*    top, left   the origin for our display                        *)
    (*    vsize       the number of screen rows we may use              *)
    (*    hsize       the number of screen columns we may use           *)
    (*    head, tail  head and tail of a linked list of row data        *)
    (*    toprow      the list element that occupies the first          *)
    (*                   displayed row.  (There might be other rows     *)
    (*                   that have scrolled out of sight.)              *)
    (*    current     the current selected row                          *)
    (*    cursorrow   screen row of the current selected row            *)
    (*    cursorenabled  TRUE iff we may highlight the current row      *)
    (*    cursoron    TRUE iff the current row is highlighted           *)
    (*    screenenabled  normally TRUE; while it is FALSE, we don't     *)
    (*                   update the screen                              *)

    ListBox = POINTER TO
                   RECORD
                       Access: Lock;
                       win: Window;
                       top, left, vsize, hsize: CARDINAL;
                       head, tail, toprow, current: RowPointer;
                       cursorrow: CARDINAL;
                       cursorenabled, cursoron, screenenabled: BOOLEAN;
                   END (*RECORD*);

    (* An item's screen row number, for any item that is visible on     *)
    (* screen, is given by                                              *)
    (*    rownumber = thisitem^.itemnumber - toprow^.itemnumber + top   *)
    (* In particular, this formula is true for the currently selected   *)
    (* item, with thisitem=current and rownumber=cursorrow, as long as  *)
    (* the list is nonempty.  If the list is empty, then the pointers   *)
    (* head, tail, toprow, and current are all NIL, and cursorrow=top.  *)

(************************************************************************)
(*                        CURSOR MOVEMENTS                              *)
(************************************************************************)

PROCEDURE Highlight (LB: ListBox);

    (* Highlights the current row. *)

    BEGIN
        WITH LB^ DO
            IF cursorenabled AND screenenabled THEN
                ColourSwap (win, cursorrow, left, left+hsize-1);
                cursoron := TRUE;
            END (*IF*);
        END (*WITH*);
    END Highlight;

(************************************************************************)

PROCEDURE Unhighlight (LB: ListBox);

    (* Reverses the action of Highlight. *)

    BEGIN
        WITH LB^ DO
            IF cursoron AND screenenabled THEN
                ColourSwap (win, cursorrow, left, left+hsize-1);
            END (*IF*);
            cursoron := FALSE;
        END (*WITH*);
    END Unhighlight;

(************************************************************************)

PROCEDURE CursorBackward (LB: ListBox): BOOLEAN;

    (* Moves the cursor one row backward.  Returns TRUE if we did       *)
    (* move backward, FALSE if we ran off the top.                      *)

    VAR success: BOOLEAN;  savedch: CHAR;

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            Unhighlight (LB);
            IF current = head THEN
                success := FALSE;
            ELSE

                IF cursorrow = top THEN

                    (* We've run off the top of the window, scroll. *)

                    toprow := toprow^.prev;
                    current := toprow;
                    IF screenenabled THEN
                        ScrollDown (win);
                        SetCursor (win, cursorrow, left);
                        savedch := current^.this^[hsize];
                        current^.this^[hsize] := CHR(0);
                        WriteString (win, current^.this^);
                        current^.this^[hsize] := savedch;
                        EraseLine (win, 1);
                    END (*IF*);

                ELSE
                    IF current = NIL THEN
                        current := tail;
                    ELSE
                        current := current^.prev;
                    END (*IF*);
                    DEC (cursorrow);
                END (*IF*);
                success := TRUE;

            END (*IF*);

            Highlight (LB);
            Release (Access);

        END (*WITH*);
        RETURN success;

    END CursorBackward;

(************************************************************************)

PROCEDURE CursorForward (LB: ListBox): BOOLEAN;

    (* Moves the cursor one row forward.  Returns TRUE if we did  *)
    (* move forward, FALSE if we ran off the bottom.              *)

    VAR CanMove: BOOLEAN;  savedch: CHAR;

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            Unhighlight (LB);
            CanMove := current <> tail;
            IF CanMove THEN

                IF cursorrow = top+vsize-1 THEN

                    (* We've run off the bottom of the window, scroll. *)

                    toprow := toprow^.next;
                    current := current^.next;
                    IF screenenabled THEN
                        ScrollUp (win);
                        SetCursor (win, cursorrow, left);
                        savedch := current^.this^[hsize];
                        current^.this^[hsize] := CHR(0);
                        WriteString (win, current^.this^);
                        current^.this^[hsize] := savedch;
                        EraseLine (win, 1);
                    END (*IF*);

                ELSE
                    current := current^.next;
                    INC (cursorrow);
                END (*IF*);

            END (*IF*);

            Highlight (LB);
            Release (Access);

        END (*WITH*);

        RETURN CanMove;

    END CursorForward;

(************************************************************************)

PROCEDURE LBGoto (LB: ListBox;  N: CARDINAL);

    (* Moves the cursor forward or backward to entry number N. *)

    VAR pos: CARDINAL;

    BEGIN
        pos := LBCurrentItemNumber(LB);
        WHILE (pos > N) AND CursorBackward(LB) DO
            pos := LBCurrentItemNumber(LB);
        END (*WHILE*);
        WHILE (pos < N) AND CursorForward(LB) DO
            pos := LBCurrentItemNumber(LB);
        END (*WHILE*);
    END LBGoto;

(************************************************************************)

PROCEDURE CursorMovements (LB: ListBox): BOOLEAN;

    (* Moves the cursor position in response to keyboard characters     *)
    (* (cursor up/down, Home, End, PgUp, PgDn).  On return, the         *)
    (* first keyboard key that we didn't use remains available to be    *)
    (* read by the caller.  The function result is TRUE iff the user    *)
    (* moved the cursor off the top of the ListBox.                     *)

    VAR ch: CHAR;  OffTop: BOOLEAN;  j: CARDINAL;

    BEGIN
        OffTop := FALSE;
        LOOP
            ch := GetKey(LB^.win);
            IF ch = Nul THEN
                ch := GetKey(LB^.win);
                IF ch = "H" THEN                        (* cursor up *)
                    IF NOT CursorBackward (LB) THEN
                        OffTop := TRUE;
                        EXIT (*LOOP*);
                    END (*IF*);
                ELSIF ch = "P" THEN                     (* cursor down *)
                    EVAL (CursorForward (LB));
                ELSIF ch = "G" THEN                     (* home *)
                    DisableScreenOutput (LB);
                    WHILE CursorBackward (LB) DO
                    END (*WHILE*);
                    Repaint (LB);
                ELSIF ch = "O" THEN                     (* end *)
                    DisableScreenOutput (LB);
                    WHILE LB^.current <> LB^.tail DO
                        EVAL (CursorForward (LB));
                    END (*WHILE*);
                    Repaint (LB);
                ELSIF ch = "I" THEN                     (* page up *)
                    DisableScreenOutput (LB);
                    FOR j := 1 TO LB^.vsize-1 DO
                        EVAL (CursorBackward (LB));
                    END (*FOR*);
                    Repaint (LB);
                    OffTop := FALSE;
                ELSIF ch = "Q" THEN                     (* page down *)
                    DisableScreenOutput (LB);
                    FOR j := 1 TO LB^.vsize-1 DO
                        EVAL (CursorForward (LB));
                    END (*FOR*);
                    Repaint (LB);
                ELSE
                    PutBack(ch);  PutBack(Nul);
                    EXIT (*LOOP*);
                END (*IF*);
            ELSE
                PutBack (ch);
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);
        RETURN OffTop;
    END CursorMovements;

(************************************************************************)
(*                        SCREEN REFRESHING                             *)
(************************************************************************)

PROCEDURE RefreshFrom (LB: ListBox;  RP: RowPointer);

    (* Redisplays the window contents, starting at item RP.  *)

    VAR row, RowsLeft: CARDINAL;  savedch: CHAR;

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            IF screenenabled THEN
                Unhighlight (LB);
                IF RP = NIL THEN
                    IF tail = NIL THEN
                        row := top;
                    ELSE
                        row := tail^.itemnumber - toprow^.itemnumber + top + 1;
                    END (*IF*);
                ELSE
                    IF RP^.itemnumber < toprow^.itemnumber THEN
                        RP := toprow;
                    END (*IF*);
                    row := RP^.itemnumber - toprow^.itemnumber + top;
                END (*IF*);
                IF row > vsize + top THEN
                    RowsLeft := 0;
                ELSE
                    RowsLeft := vsize + top - row;
                END (*IF*);
                WHILE (RP <> NIL) AND (RowsLeft > 0) DO
                    SetCursor (win, row, left);
                    savedch := current^.this^[hsize];
                    current^.this^[hsize] := CHR(0);
                    WriteString (win, RP^.this^);
                    current^.this^[hsize] := savedch;
                    EraseLine (win, 1);
                    DEC (RowsLeft);
                    INC (row);
                    RP := RP^.next;
                END (*WHILE*);

                (* Blank the unused rows. *)

                WHILE RowsLeft > 0 DO
                    SetCursor (win, row, left);
                    EraseLine (win, 0);
                    INC (row);
                    DEC (RowsLeft);
                END (*WHILE*);

                Highlight (LB);
            END (*IF*);
            Release (Access);
        END (*WITH*);

    END RefreshFrom;

(************************************************************************)

PROCEDURE PaintCurrentItem (LB: ListBox;  fore, back: Colour);

    (* Sets the foreground and background colours of the current item. *)

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            IF screenenabled THEN
                IF cursoron THEN
                    SetColours (win, cursorrow, left, hsize, back, fore);
                ELSE
                    SetColours (win, cursorrow, left, hsize, fore, back);
                END (*IF*);
            END (*IF*);
            Release (Access);
        END (*WITH*);
    END PaintCurrentItem;

(************************************************************************)

PROCEDURE Repaint (LB: ListBox);

    (* Rewrites the screen.  This also cancels the effect of a previous *)
    (* DisableScreenOutput call.                                        *)

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            screenenabled := TRUE;
            Release (Access);
        END (*WITH*);
        RefreshFrom (LB, LB^.toprow);
    END Repaint;

(************************************************************************)

PROCEDURE DisableScreenOutput (LB: ListBox);

    (* Disables all screen updating until the next Repaint call. *)

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            screenenabled := FALSE;
            Release (Access);
        END (*WITH*);
    END DisableScreenOutput;

(************************************************************************)
(*                           DELETING ENTRIES                           *)
(************************************************************************)

PROCEDURE ClearListBox (LB: ListBox);

    (* Removes all stored data rows. *)

    VAR p, q: RowPointer;

    BEGIN
        Obtain (LB^.Access);
        p := LB^.head;
        WHILE p <> NIL DO
            DISPOSE (p^.this);
            q := p^.next;
            DISPOSE (p);
            p := q;
        END (*WHILE*);
        WITH LB^ DO
            head := NIL;  tail := NIL;
            toprow := NIL;  current := NIL;
            cursorrow := top;
            Release (Access);
        END (*WITH*);
        RefreshFrom (LB, NIL);
    END ClearListBox;

(************************************************************************)

PROCEDURE LBDeleteNext (LB: ListBox);

    (* Removes the entry after the current entry, if any.  The screen   *)
    (* cursor is not moved.                                             *)

    VAR p: RowPointer;

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            p := current^.next;
            IF p <> NIL THEN
                current^.next := p^.next;
                IF p = tail THEN
                    tail := current;
                ELSE
                    current^.next^.prev := current;
                END (*IF*);
                DISPOSE (p^.this);
                DISPOSE (p);
                p := current^.next;
                WHILE p <> NIL DO
                    DEC (p^.itemnumber);  p := p^.next;
                END (*WHILE*);
            END (*IF*);
            Release (Access);
        END (*WITH*);
        RefreshFrom (LB, LB^.current^.next);
    END LBDeleteNext;

(************************************************************************)

PROCEDURE LBDeleteCurrent (LB: ListBox);

    (* Removes the current entry. *)

    (* REMARK: This can probably be simplified now that I have implemented *)
    (* the LBDeleteNext procedure.  I'd just need to implement something   *)
    (* else to delete the head element, I think.                           *)

    VAR p: RowPointer;

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            p := current;
            IF p <> NIL THEN
                current := p^.next;
                IF p^.prev = NIL THEN
                    head := current;
                ELSE
                    p^.prev^.next := current;
                END (*IF*);
                IF current = NIL THEN
                    tail := p^.prev;
                ELSE
                    current^.prev := p^.prev;
                END (*IF*);
                DISPOSE (p^.this);
                DISPOSE (p);
                p := current;
                WHILE p <> NIL DO
                    DEC (p^.itemnumber);  p := p^.next;
                END (*WHILE*);
                IF current = NIL THEN

                    (* We've just deleted the last item on the list.  Make *)
                    (* the new tail, if any, the current entry.            *)

                    current := tail;
                    IF current = NIL THEN
                        toprow := NIL;  cursorrow := top;
                    ELSE
                        IF cursorrow = top THEN
                            (* The deleted entry was at the top of the  *)
                            (* screen, so we have to scroll.            *)
                            toprow := current;
                        ELSE
                            DEC (cursorrow);
                        END (*IF*);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            Release (Access);
        END (*WITH*);
        RefreshFrom (LB, LB^.current);
    END LBDeleteCurrent;

(************************************************************************)
(*                 ENABLING AND DISABLING THE CURSOR                    *)
(************************************************************************)

PROCEDURE HighlightOn (LB: ListBox);

    (* Enables reverse video for the current item. *)

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            Unhighlight (LB);
            cursorenabled := TRUE;
            Highlight (LB);
            Release (Access);
        END (*WITH*);
    END HighlightOn;

(************************************************************************)

PROCEDURE HighlightOff (LB: ListBox);

    (* Disables reverse video for the current item. *)

    BEGIN
        WITH LB^ DO
            Obtain (Access);
            cursorenabled := FALSE;
            Unhighlight (LB);
            Release (Access);
        END (*WITH*);
    END HighlightOff;

(************************************************************************)
(*                      LISTBOX CREATION, ETC.                          *)
(************************************************************************)

PROCEDURE CreateListBox (w: Window;  firstrow, leftcol,
                                       rows, columns: CARDINAL): ListBox;

    (* Creates a new ListBox.  Window w must already exist, and "rows"  *)
    (* and "columns" should match the number of rows and columns that   *)
    (* will be visible in the window.  The parameters firstrow and      *)
    (* leftcol specify an offset from the window origin.                *)

    VAR result: ListBox;

    BEGIN
        NEW (result);
        WITH result^ DO
            CreateLock (Access);
            win := w;  top := firstrow;  left := leftcol;
            vsize := rows;  hsize := columns;
            NewScrollingRegion (win, top, top+vsize-1, left, left+hsize-1);
            head := NIL;  tail := NIL;
            toprow := NIL;  current := NIL;
            cursorrow := top;
            cursorenabled := FALSE;  cursoron := FALSE;
            screenenabled := TRUE;
        END (*WITH*);
        RETURN result;
    END CreateListBox;

(************************************************************************)

PROCEDURE DestroyListBox (LB: ListBox);

    (* Deletes LB. *)

    BEGIN
        ClearListBox (LB);
        DestroyLock (LB^.Access);
        DISPOSE (LB);
    END DestroyListBox;

(************************************************************************)

PROCEDURE WindowOf (LB: ListBox): Window;

    (* Identifies the window in which this ListBox lives. *)

    BEGIN
        WITH LB^ DO
            SetCursor (win, cursorrow, left);
            RETURN win;
        END (*WITH*);
    END WindowOf;

(************************************************************************)
(*                     ADDING DATA TO A LISTBOX                         *)
(************************************************************************)

PROCEDURE LBAppend (LB: ListBox;  newitem: ARRAY OF CHAR);

    (* Adds a new item after the current last item. *)

    VAR RP: RowPointer;

    BEGIN
        NEW (RP);  NEW (RP^.this);
        Strings.Assign (newitem, RP^.this^);
        RP^.next := NIL;
        Obtain (LB^.Access);
        RP^.prev := LB^.tail;
        IF LB^.tail = NIL THEN
            LB^.head := RP;  LB^.toprow := RP;
            LB^.current := RP;
            RP^.itemnumber := 1;
        ELSE
            LB^.tail^.next := RP;
            RP^.itemnumber := LB^.tail^.itemnumber + 1;
        END (*IF*);
        LB^.tail := RP;
        Release (LB^.Access);
        RefreshFrom (LB, RP);
    END LBAppend;

(************************************************************************)

PROCEDURE LBInsertAt (LB: ListBox;  newitem: ARRAY OF CHAR;
                                    position: CARDINAL);

    (* Adds a new item at sequence number 'position'.  The current item *)
    (* selection is left unchanged.  Does not redisplay the box.        *)
    (* Because calls to this procedure insert items out of sequence,    *)
    (* they can leave gaps and inconsistencies in the item numbers.     *)
    (* To fix this, call LBUpdateItemNumbers after finishing all the    *)
    (* insertions.                                                      *)

    VAR RP, here, next: RowPointer;

    BEGIN
        NEW (RP);  NEW (RP^.this);
        Strings.Assign (newitem, RP^.this^);
        RP^.itemnumber := position;

        Obtain (LB^.Access);

        (* Starting at head of list, find the insertion point. *)

        here := NIL;  next := LB^.head;
        WHILE (next <> NIL) AND (next^.itemnumber < position) DO
            here := next;
            next := here^.next;
        END (*WHILE*);

        (* We now have to insert between here and next. *)

        RP^.prev := here;  RP^.next := next;
        IF here = NIL THEN

            (* Insertion at head of list. *)

            LB^.head := RP;
            IF next = NIL THEN

                (* Insertion into empty list. *)

                LB^.toprow := RP;  LB^.current := RP;

            END (*IF*);

        ELSE
            here^.next := RP;
        END (*IF*);

        IF next = NIL THEN

            (* Insertion at tail of list. *)

            LB^.tail := RP;

        ELSE
            next^.prev := RP;
        END (*IF*);

        Release (LB^.Access);

    END LBInsertAt;

(************************************************************************)

PROCEDURE LBUpdateItemNumbers (LB: ListBox);

    (* Makes a pass through the entire list making sure that the item   *)
    (* numbers are consistent, and then redisplays the listbox.         *)

    VAR p: RowPointer;  N: CARDINAL;

    BEGIN
        Obtain (LB^.Access);
        p := LB^.head;  N := 1;
        WHILE p <> NIL DO
            p^.itemnumber := N;  INC(N);
            p := p^.next;
        END (*WHILE*);
        Release (LB^.Access);
        RefreshFrom (LB, LB^.head);
    END LBUpdateItemNumbers;

(************************************************************************)

PROCEDURE LBInsertAfter (LB: ListBox;  newitem: ARRAY OF CHAR);

    (* Adds a new item after the current item. *)

    VAR RP, p: RowPointer;

    BEGIN
        NEW (RP);  NEW (RP^.this);
        Strings.Assign (newitem, RP^.this^);
        Obtain (LB^.Access);
        IF LB^.current = NIL THEN
            LB^.head := RP;  LB^.tail := RP;
            LB^.toprow := RP;  LB^.current := RP;
            RP^.itemnumber := 1;
            RP^.prev := NIL;  RP^.next := NIL;
        ELSE
            RP^.prev := LB^.current;  RP^.next := LB^.current^.next;
            LB^.current^.next := RP;
            RP^.itemnumber := LB^.current^.itemnumber + 1;
            IF RP^.next = NIL THEN
                LB^.tail := RP;
            ELSE
                p := RP^.next;
                p^.prev := RP;
                REPEAT
                    INC (p^.itemnumber);  p := p^.next;
                UNTIL p = NIL;
            END (*IF*);
        END (*IF*);
        Release (LB^.Access);
        EVAL (CursorForward (LB));
        RefreshFrom (LB, RP);
    END LBInsertAfter;

(************************************************************************)

PROCEDURE LBInsertBefore (LB: ListBox;  newitem: ARRAY OF CHAR);

    (* Adds a new item before the current item, and makes the newly     *)
    (* inserted item the current item.                                  *)

    VAR RP, p: RowPointer;

    BEGIN
        NEW (RP);  NEW (RP^.this);
        Strings.Assign (newitem, RP^.this^);
        Obtain (LB^.Access);
        IF LB^.current = NIL THEN
            LB^.head := RP;  LB^.tail := RP;
            LB^.toprow := RP;  LB^.current := RP;
            RP^.itemnumber := 1;
            RP^.prev := NIL;  RP^.next := NIL;
        ELSE
            RP^.prev := LB^.current^.prev;  RP^.next := LB^.current;
            LB^.current^.prev := RP;
            RP^.itemnumber := LB^.current^.itemnumber;
            IF RP^.prev = NIL THEN
                LB^.head := RP;
            ELSE
                RP^.prev^.next := RP;
            END (*IF*);
            p := LB^.current;
            IF LB^.toprow = p THEN
                LB^.toprow := RP;
            END (*IF*);
            REPEAT
                INC (p^.itemnumber);  p := p^.next;
            UNTIL p = NIL;
        END (*IF*);
        LB^.current := RP;
        Release (LB^.Access);
        RefreshFrom (LB, RP);
    END LBInsertBefore;

(************************************************************************)

PROCEDURE ReplaceCurrent (LB: ListBox;  VAR (*IN*) item: ARRAY OF CHAR);

    (* Updates the string value of the current item. *)

    BEGIN
        IF LB^.current = NIL THEN
            LBAppend (LB, item);
        ELSE
            Strings.Assign (item, LB^.current^.this^);
            RefreshFrom (LB, LB^.current);
        END (*IF*);
    END ReplaceCurrent;

(************************************************************************)
(*                            SORTING                                   *)
(************************************************************************)

PROCEDURE IsLess (p1, p2: RowPointer): BOOLEAN;

    (* Returns TRUE iff p1^.this^ < p2^.this^. *)

    VAR j: CARDINAL;  q1, q2: TextPointer;

    BEGIN
        j := 0;  q1 := p1^.this;  q2 := p2^.this;
        LOOP
            IF j = ScreenCols THEN RETURN FALSE
            ELSIF q1^[j] = Nul THEN RETURN q2^[j] <> Nul
            ELSIF q2^[j] = Nul THEN RETURN FALSE
            ELSIF q1^[j] > q2^[j] THEN RETURN FALSE
            ELSIF q1^[j] < q2^[j] THEN RETURN TRUE
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
    END IsLess;

(************************************************************************)

PROCEDURE SortList (VAR (*INOUT*) first, last: RowPointer);

    (* Sorts a linked list, given the head and tail. *)

    VAR p1, p2, p3: RowPointer;  temp: TextPointer;

    BEGIN
        (* Sorting a linked list can be tricky, so for now I'll use     *)
        (* a simple-minded method.                                      *)

        p1 := first;
        WHILE p1 <> NIL DO

            (* Find the smallest element in p1^..last^.  p2 points to   *)
            (* the smallest element we've found so far.                 *)

            p2 := p1;
            p3 := p1^.next;
            WHILE p3 <> NIL DO
                IF IsLess (p3, p2) THEN
                    p2 := p3;
                END (*IF*);
                p3 := p3^.next;
            END (*WHILE*);

            (* Swap p1^ and p2^. *)

            IF p1 <> p2 THEN
                temp := p1^.this;  p1^.this := p2^.this;  p2^.this := temp;
            END (*IF*);
            p1 := p1^.next;

        END (*WHILE*);

    END SortList;

(************************************************************************)

PROCEDURE LBSort (LB: ListBox);

    (* Sorts the contents of LB, then redisplays it. *)

    VAR oldtoprow, p: RowPointer;  ino, ino2: CARDINAL;

    BEGIN
        Obtain (LB^.Access);
        oldtoprow := LB^.toprow;
        SortList (LB^.head, LB^.tail);

        (* Now we have to get the data structure back into a consistent *)
        (* state.  This requires adjusting all the item numbers, and    *)
        (* giving new values to the toprow, current, and cursorrow      *)
        (* fields of LB.                                                *)

        LB^.toprow := oldtoprow;
        LB^.current := oldtoprow;
        LB^.cursorrow := LB^.top;
        p := LB^.head;  ino := 0;  ino2 := 1;
        WHILE p <> NIL DO
            p^.itemnumber := ino2;
            INC (ino);
            INC (ino2);
            p := p^.next;
        END (*WHILE*);

        Release (LB^.Access);
        RefreshFrom (LB, LB^.toprow);
    END LBSort;

(************************************************************************)
(*                   RETURNING DATA TO THE CLIENT                       *)
(************************************************************************)

PROCEDURE LBCurrentItemNumber (LB: ListBox): CARDINAL;

    (* Returns the ordinal number of the current item.  If the list is  *)
    (* empty then the result is zero.                                   *)

    BEGIN
        IF LB^.current = NIL THEN
            RETURN 0;
        ELSE
            RETURN LB^.current^.itemnumber;
        END (*IF*);
    END LBCurrentItemNumber;

(************************************************************************)

PROCEDURE LBCurrentRowNumber (LB: ListBox): CARDINAL;

    (* Returns the row number, within the window, of the current item.  *)
    (* Note: a result of 0 could mean either that the top item is the   *)
    (* current item, or that there are no items.                        *)

    BEGIN
        WITH LB^ DO
            SetCursor (win, cursorrow, left);
            RETURN cursorrow;
        END (*WITH*);
    END LBCurrentRowNumber;

(************************************************************************)

PROCEDURE LBCurrent (LB: ListBox;  VAR (*OUT*) item: ARRAY OF CHAR);

    (* Returns the (string) value of the current item. *)

    BEGIN
        IF LB^.current = NIL THEN
            Strings.Assign ("", item);
        ELSE
            Strings.Assign (LB^.current^.this^, item);
        END (*IF*);
    END LBCurrent;

(************************************************************************)

PROCEDURE ItemNumberOf (LB: ListBox;  entry: ARRAY OF CHAR): CARDINAL;

    (* Returns the ordinal number of the item that matches 'entry'.     *)
    (* The result is zero iff the list is empty.                        *)

    VAR current: RowPointer;

    BEGIN
        current := LB^.head;
        WHILE (current <> NIL)
                    AND NOT Strings.Equal (entry, current^.this^) DO
            current := current^.next;
        END (*WHILE*);
        IF current = NIL THEN
            RETURN 0;
        ELSE
            RETURN current^.itemnumber;
        END (*IF*);
    END ItemNumberOf;

(************************************************************************)

END ListBoxes.

