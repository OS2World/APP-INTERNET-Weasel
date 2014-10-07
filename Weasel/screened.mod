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

IMPLEMENTATION MODULE ScreenEditor;

        (********************************************************)
        (*                                                      *)
        (*              Screen data capture                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        9 July 2005                     *)
        (*  Status:                                             *)
        (*      Basic features working, but see faults in       *)
        (*      module RowEditor.                               *)
        (*                                                      *)
        (********************************************************)

FROM RowEditor IMPORT
    (* type *)  StructureRow,
    (* proc *)  WriteRow, EditRow, NewRow, CombineRows,
                CopyOfRow, AdjustRow, DeleteRow, NewList, NewMenu,
                StartColumn;

FROM ListEditor IMPORT
    (* type *)  List, ListFormat;

FROM SYSTEM IMPORT
    (* type *)  BYTE, ADDRESS,
    (* proc *)  ADR;

FROM Windows IMPORT
    (* type *)  Window,
    (* proc *)  OpenSimpleWindow, CloseWindow, SetCursor, SaveCursor;

FROM Menus IMPORT
    (* type *)  Menu, MenuOption, MO, OffEdgeOption,
    (* proc *)  SetOptions, OffEdge;

FROM Keyboard IMPORT
    (* proc *)  InKey, PutBack;

FROM FieldEditor IMPORT
    (* var  *)  Byte, Cardinal, Real, String,
    (* type *)  FieldType,
    (* proc *)  DefineFieldType;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST testing = FALSE;

CONST Esc = CHR(27);

TYPE

    Structure = POINTER TO RowHeader;

    (* The fields in a RowHeader record are:                            *)
    (*          pointer:        the row structure for this row          *)
    (*          row:            screen position                         *)
    (*          up, down:       pointers to adjacent rows               *)

    RowHeader = RECORD
                    pointer: StructureRow;
                    row: CARDINAL;
                    up, down: Structure;
                END (*RECORD*);

(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE WriteStructure (w: Window;  S: Structure);

    BEGIN
        WHILE S <> NIL DO
            WITH S^ DO
                WriteRow (w, pointer, row);
            END (*WITH*);
            S := S^.down;
        END (*WHILE*);
    END WriteStructure;

(************************************************************************)
(*                 INTRODUCING A NEW FIELD TO THE SYSTEM                *)
(************************************************************************)

PROCEDURE CreateField (VariableAddress: ADDRESS;  size: CARDINAL;  ftype: FieldType;
                        screenrow, screencolumn, width: CARDINAL): Structure;

    (* Creates a new structure consisting of a single field.  Before    *)
    (* calling this procedure, the caller should make sure, by calling  *)
    (* FieldEditor.DefineFieldType if necessary, that ftype is a type   *)
    (* already known to module FieldEditor.                             *)
    (* Note: size is number of storage locations occupied by variable.  *)
    (*       width is number of screen spaces used to edit it.          *)

    VAR result: Structure;

    BEGIN
        NEW (result);
        WITH result^ DO
            pointer := NewRow (VariableAddress, size, ftype, screencolumn, width);
            row := screenrow;
            up := NIL;  down := NIL;
        END (*WITH*);
        RETURN result;
    END CreateField;

(************************************************************************)

PROCEDURE CardinalField (VAR (*IN*) variable: CARDINAL;
                        screenrow, screencolumn, width: CARDINAL): Structure;

    (* Creates a one-field structure for editing the given CARDINAL     *)
    (* variable.                                                        *)

    BEGIN
        RETURN CreateField (ADR(variable), SIZE(CARDINAL), Cardinal,
                                        screenrow, screencolumn, width);
    END CardinalField;

(************************************************************************)

PROCEDURE ByteField (VAR (*IN*) variable: BYTE;
                        screenrow, screencolumn, width: CARDINAL): Structure;

    (* Creates a one-field structure for editing a BYTE variable.       *)

    BEGIN
        RETURN CreateField (ADR(variable), SIZE(BYTE), Byte,
                                        screenrow, screencolumn, width);
    END ByteField;

(************************************************************************)

PROCEDURE RealField (VAR (*IN*) variable: REAL;
                        screenrow, screencolumn, width: CARDINAL): Structure;

    (* Creates a one-field structure for editing a REAL variable.       *)

    BEGIN
        RETURN CreateField (ADR(variable), SIZE(REAL), Real,
                                        screenrow, screencolumn, width);
    END RealField;

(************************************************************************)

PROCEDURE StringField (VAR (*IN*) variable: ARRAY OF CHAR;
                        screenrow, screencolumn, width: CARDINAL): Structure;

    (* Creates a one-field structure for editing a character string.    *)

    BEGIN
        RETURN CreateField (ADR(variable), HIGH(variable) + 1, String,
                                        screenrow, screencolumn, width);
    END StringField;

(************************************************************************)

PROCEDURE MenuField (VAR (*IN*) variable: CARDINAL;
                screenrow, screencolumn, lines, width: CARDINAL;
                                                M: Menu): Structure;

    (* Creates a one-field structure for editing a cardinal variable    *)
    (* via menu selection.  The caller must ensure that M has already   *)
    (* been defined by a call to Menus.                                 *)

    VAR result: Structure;

    BEGIN
        SetOptions (M, MO{MNoClose,MKeyBack,MNoMouse,MNoTitle,MNoBorder});
        OffEdge (M, return, return, return, return);
        NEW (result);
        WITH result^ DO
            pointer := NewMenu (variable, M, screencolumn, lines, width);
            row := screenrow;
            up := NIL;  down := NIL;
        END (*WITH*);
        RETURN result;
    END MenuField;

(************************************************************************)

PROCEDURE ListField (VAR (*IN*) variable: List;
                                screenrow, screencolumn: CARDINAL;
                                        f: ListFormat): Structure;

    (* Creates a structure for editing a linear list.  The caller must  *)
    (* ensure that f has been defined by a call to module ListEditor.   *)

    VAR result: Structure;

    BEGIN
        NEW (result);
        WITH result^ DO
            pointer := NewList (variable, f, screencolumn);
            row := screenrow;
            up := NIL;  down := NIL;
        END (*WITH*);
        RETURN result;
    END ListField;

(************************************************************************)
(*                          TEST PROCEDURE                              *)
(************************************************************************)

(*
PROCEDURE DumpStructure (S: Structure);

    (* For debugging: writes a representation of S to the screen.       *)

    VAR w: Window;

    BEGIN
        OpenSimpleWindow (w, 0, 10, 0, 79);
        WHILE S <> NIL DO
            WriteLn (w);
            WriteString (w, "Dumping row ");  WriteCard (w, S^.row);
            WriteString (w, " @");  WriteAddress (w, S);
            WriteString (w, ", up = ");  WriteAddress (w, S^.up);
            WriteString (w, ", down = ");  WriteAddress (w, S^.down);
            DumpRow (w, S^.pointer);  S := S^.down;
        END (*WHILE*);
        PressAnyKey(w);
        CloseWindow (w);
    END DumpStructure;
*)

(************************************************************************)
(*                        DELETING A STRUCTURE                          *)
(************************************************************************)

PROCEDURE DeleteStructure (VAR (*INOUT*) S: Structure);

    (* Deallocates the storage which was used in setting up structure   *)
    (* S.  Note that this has nothing to do with the space used by      *)
    (* variables to which S gives access; we delete only the overhead   *)
    (* space which was originally allocated by this module.             *)

    VAR temp: Structure;

    BEGIN
        WHILE S <> NIL DO
            DeleteRow (S^.pointer);  temp := S^.down;
            DISPOSE(S);  S := temp;
        END (*WHILE*);
    END DeleteStructure;

(************************************************************************)
(*                CONSTRUCTING COMPLEX STRUCTURE TYPES                  *)
(************************************************************************)

PROCEDURE Combine (VAR (*INOUT*) A: Structure;  B: Structure);

    (* Strips all of the fields from B and adds them to the existing    *)
    (* fields of A.  Note that B is destroyed in the process.           *)

    VAR previous, current, next, temp: Structure;

    BEGIN
        previous := NIL;  current := A;
        WHILE B <> NIL DO

            (* Find a place to insert the first element on the B list. *)

            LOOP
                IF current = NIL THEN EXIT (*LOOP*) END (*IF*);
                next := current^.down;
                IF B^.row <= current^.row THEN EXIT (*LOOP*) END (*IF*);
                previous := current;  current := next;
            END (*LOOP*);

            IF (current<>NIL) AND (B^.row = current^.row) THEN

                (* Special case: two rows must be merged. *)

                CombineRows (current^.pointer, B^.pointer);
                temp := B;  B := B^.down;  DISPOSE (temp);

            ELSE

                (* Take the whole of the B list, insert it below        *)
                (* previous^, then take what remains of the original    *)
                (* destination list and call it the B list.  Swapping   *)
                (* lists like this is a little unconventional, but it   *)
                (* works, and in many cases it speeds up the merge.     *)

                IF previous = NIL THEN
                    A := B;
                ELSE
                    previous^.down := B;
                END (*IF*);
                B^.up := previous;  previous := B;  B := current;
                current := previous^.down;
            END (*IF*);

        END (*WHILE*);
        (*DumpStructure (A);*)

    END Combine;

(************************************************************************)

PROCEDURE CopyOf (S: Structure): Structure;

    (* Makes a duplicate copy of S.  The variables to be edited are not *)
    (* duplicated; we simply set up a duplicate set of pointers.        *)

    VAR result, newrow: Structure;

    BEGIN
        result := NIL;
        WHILE S <> NIL DO
            NEW (newrow);
            WITH newrow^ DO
                pointer := CopyOfRow (S^.pointer);
                row := S^.row;  up := NIL;  down := NIL;
            END (*WITH*);
            Combine (result, newrow);
            S := S^.down;
        END (*WHILE*);
        RETURN result;
    END CopyOf;

(************************************************************************)

PROCEDURE AdjustAddresses (S: Structure;  addroffset, rowoffset,
                                                coloffset: CARDINAL);

    (* Adjusts the pointer, row, and column fields of all elements of S *)
    (* by adding the specified offsets to those fields.                 *)

    BEGIN
        WHILE S <> NIL DO
            AdjustRow (S^.pointer, addroffset, coloffset);
            INC (S^.row, rowoffset);  S := S^.down;
        END (*WHILE*);
    END AdjustAddresses;

(************************************************************************)

PROCEDURE MakeArray (VAR (*INOUT*) S: Structure;  count: CARDINAL;
                addroffset, rowoffset, coloffset: CARDINAL);

    (* Creates a structure for an array of count elements, where on     *)
    (* entry S is a structure already created for the first array       *)
    (* element.  Parameter addroffset is the difference between         *)
    (* adjacent array elements.  The remaining two parameters give the  *)
    (* offset on the screen between the starting positions of adjacent  *)
    (* array elements.                                                  *)
    (* In this version, I simply replicate item the required number of  *)
    (* times, making appropriate adjustments to the replicated items.   *)
    (* There is probably a more elegant solution which involves the     *)
    (* invention of a new "replicating" type, but that will take more   *)
    (* thought than I have as yet given to the problem.                 *)

    VAR current, copy: Structure;  j: CARDINAL;

    BEGIN
        copy := CopyOf(S);
        FOR j := 2 TO count DO

            current := copy;
            AdjustAddresses (current, addroffset, rowoffset, coloffset);

            (* Save a copy for the next time around the loop, then      *)
            (* incorporate current into the result.                     *)

            copy := CopyOf (current);
            Combine (S, current);

        END (*FOR*);
        (*
        IF testing THEN
            DumpStructure (S);
        END (*IF*);
        *)
        DeleteStructure (copy);
    END MakeArray;

(************************************************************************)
(*                         EDITING A STRUCTURE                          *)
(************************************************************************)

PROCEDURE SetStartingPoint (VAR (*INOUT*) S: Structure;
                                                screenrow: CARDINAL);

    (* On entry, S points to the first row in a structure.  On exit, S  *)
    (* points to the field in that row whose "row" field most           *)
    (* closely matches the second argument to this procedure.           *)

    VAR next: Structure;

    BEGIN
        IF S = NIL THEN RETURN END (*IF*);
        LOOP
            next := S^.down;
            IF next = NIL THEN EXIT(*LOOP*) END (*IF*);
            IF screenrow <= S^.row THEN
                EXIT (*LOOP*);
            END (*IF*);
            S := next;
        END (*LOOP*);
    END SetStartingPoint;

(************************************************************************)

PROCEDURE ScreenEdit (w: Window;  S: Structure;  VAR (*OUT*) abort: BOOLEAN);

    (* Displays structure S in window w, and allows the keyboard user   *)
    (* to edit the components of S.  It is assumed that w is already    *)
    (* open and that S has already been fully defined.  Returns         *)
    (* abort=TRUE if user aborted the editing with the Esc key.         *)

    VAR nextchar: CHAR;  startrow, startcol: CARDINAL;

    BEGIN
        abort := FALSE;
        IF S = NIL THEN
            RETURN;
        END (*IF*);
        SaveCursor (w, startrow, startcol);
        WriteStructure (w, S);
        SetStartingPoint (S, startrow);
        SetCursor (w, S^.row, StartColumn(S^.pointer));
        LOOP
            EditRow (w, S^.pointer, S^.row);
            nextchar := InKey();
            IF nextchar = CHR(0) THEN
                nextchar := InKey();

                (* Remark: in the following we interpret "cursor left"  *)
                (* to mean the same as "cursor up", and "cursor right"  *)
                (* to mean the same as "cursor down".  The reasoning    *)
                (* here is that we have just returned from the row      *)
                (* editor, which will only return those keys if there   *)
                (* is nothing to the left or right, respectively.       *)

                IF ((nextchar = "H") OR (nextchar = "K"))
                                     AND (S^.up <> NIL) THEN
                    S := S^.up;
                ELSIF ((nextchar = "P") OR (nextchar = "M"))
                                        AND (S^.down <> NIL) THEN
                    S := S^.down;
                ELSE
                    PutBack (nextchar);  PutBack (CHR(0));
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF nextchar = CHR(13) THEN       (* <return> *)
                IF S^.down <> NIL THEN
                    S := S^.down;
                    SetCursor (w, S^.row, StartColumn(S^.pointer));
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);
            ELSIF nextchar = Esc THEN
                abort := TRUE;
                EXIT (*LOOP*);
            ELSE
                PutBack (nextchar);
                IF S^.down <> NIL THEN
                    S := S^.down;
                END (*IF*);
            END (*IF*);

        END (*LOOP*);

    END ScreenEdit;

(************************************************************************)

END ScreenEditor.

