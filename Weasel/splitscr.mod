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

IMPLEMENTATION MODULE SplitScreen;

        (********************************************************)
        (*                                                      *)
        (*           Simple screen output routines.             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        14 November 20012               *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*  This module supports a specialised form of split    *)
        (*  screen.  The bottom half supports scrolling, the    *)
        (*  top half does not, and we maintain an internal      *)
        (*  cursor position for the bottom half only.           *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD16;

IMPORT OS2, Strings;

FROM LowLevel IMPORT
    (* proc *)  ALLOCATE64;

FROM Storage IMPORT
    (* proc *)  DEALLOCATE;

(************************************************************************)

CONST
    (* String of blank characters for indenting. *)

    Blanks = "                                                                                                                                ";

VAR
    (* Flag to say whether we have access to the screen. *)

    ProcessIsNotDetached: BOOLEAN;

    (* Flag to say that we can use a split screen.  If this flag is     *)
    (* FALSE then we don't do scrolling and the "write at" operation    *)
    (* doesn't write anything.                                          *)

    SplitEnabled: BOOLEAN;

    (* Screen size. *)

    MaxRowNumber, NoOfColumns: CARDINAL;

    (* CurrentRow is the number of the current screen line.             *)

    CurrentRow: CARDINAL;

    (* CurrentColumn is the number of the current screen column.  The   *)
    (* special case CurrentColumn = NoOfColumns means that we have run  *)
    (* off the end of the current row, and must do a WriteLn or         *)
    (* SetCursor before writing a new character.                        *)

    CurrentColumn: CARDINAL;

    (* The top half of the split screen is rows [FirstBottomRow..MaxRowNumber]. *)

    FirstBottomRow: CARDINAL;

    (* If we run off the end of a line, we indent by 'indent' blanks    *)
    (* at the beginning of the next line.                               *)

    indent: CARDINAL;

    (* Fill character and attribute for creating a blank line. *)

    Fill: ARRAY [0..1] OF CHAR;

    (* Mutex for exclusive access to screen. *)

    ScreenAccess: OS2.HMTX;

(************************************************************************)
(*                     CHECK FOR SCREEN AVAILABILITY                    *)
(************************************************************************)

PROCEDURE NotDetached(): BOOLEAN;

    (* Returns TRUE unless called by a process running detached.        *)
    (* (A detached process may not do keyboard, screen, or mouse I/O.)  *)

    BEGIN
        RETURN ProcessIsNotDetached;
    END NotDetached;

(************************************************************************)
(*                      GETTING EXCLUSIVE ACCESS                        *)
(************************************************************************)

PROCEDURE LockScreen;

    (* Get exclusive access to the screen. *)

    BEGIN
        OS2.DosRequestMutexSem (ScreenAccess, OS2.SEM_INDEFINITE_WAIT);
    END LockScreen;

(************************************************************************)

PROCEDURE UnlockScreen;

    (* End of section started by LockScreen. *)

    BEGIN
        OS2.DosReleaseMutexSem (ScreenAccess);
    END UnlockScreen;

(************************************************************************)
(*                        SPLIT-SCREEN SUPPORT                          *)
(************************************************************************)

PROCEDURE SetBoundary (firstrow, hangingindent: CARDINAL);

    (* Defines the bottom half of the screen to start at row firstrow.  *)
    (* Also defines the number of blank characters to write at the      *)
    (* beginning of a continuation line if we run off the end of a row. *)

    BEGIN
        IF SplitEnabled THEN
            indent := hangingindent;
            FirstBottomRow := firstrow;
            SetCursor (firstrow, 0);
        END (*IF*);
    END SetBoundary;

(************************************************************************)
(*                      SCROLLING AND CURSOR MOVEMENTS                  *)
(************************************************************************)

PROCEDURE SetCursor (row, column: CARDINAL);

    (* Moves the screen cursor to the specified row and column.         *)

    BEGIN
        OS2.VioSetCurPos (row, column, 0);
        CurrentRow := row;  CurrentColumn := column;
    END SetCursor;

(************************************************************************)

PROCEDURE WriteLn;

    (* Moves the screen cursor to the beginning of the next line,       *)
    (* scrolling if necessary.                                          *)

    BEGIN
        IF CurrentRow+1 = FirstBottomRow THEN
            SetCursor (0, 0);
        ELSIF CurrentRow >= MaxRowNumber THEN
            OS2.VioScrollUp (FirstBottomRow, 0, MaxRowNumber, NoOfColumns-1,
                             1, Fill, 0);
            SetCursor (MaxRowNumber, 0);
        ELSE
            SetCursor (CurrentRow+1, 0);
        END (*IF*);
    END WriteLn;

(************************************************************************)

PROCEDURE ClearScreen;

    (* Clears the screen. *)

    BEGIN
        OS2.VioWrtNCell (Fill, (MaxRowNumber+1)*NoOfColumns, 0, 0, 0);
        SetCursor (FirstBottomRow, 0);
    END ClearScreen;

(************************************************************************)

PROCEDURE ReleaseScreen;

    (* Tells the OS about our current cursor position. *)

    BEGIN
        IF ProcessIsNotDetached THEN
            OS2.VioSetCurPos (CurrentRow, CurrentColumn, 0);
        END (*IF*);
    END ReleaseScreen;

(************************************************************************)

PROCEDURE RegainScreen;

    (* Queries the OS for the current cursor position. *)

    VAR row, col: CARD16;

    BEGIN
        IF ProcessIsNotDetached THEN
            OS2.VioGetCurPos (row, col, 0);
            CurrentRow := row;  CurrentColumn := col;
        END (*IF*);
    END RegainScreen;

(************************************************************************)
(*                      CHARACTER AND STRING OUTPUT                     *)
(************************************************************************)

PROCEDURE WriteStringAt (row, col: CARDINAL;  text: ARRAY OF CHAR);

    (* Writes a character string at screen location (row, col). *)
    (* This procedure does not change the cursor position.      *)

    BEGIN
        IF SplitEnabled THEN
            OS2.VioWrtCharStr (text, LENGTH(text), row, col, 0);
        END (*IF*);
    END WriteStringAt;

(************************************************************************)

PROCEDURE WriteString (text: ARRAY OF CHAR);

    (* Writes a sequence of characters, terminated either by NUL or by  *)
    (* the end of the array, and updates the cursor.                    *)

    VAR length, amount: CARDINAL;

    BEGIN
        length := LENGTH(text);
        WHILE length > 0 DO
            IF CurrentColumn = NoOfColumns THEN
                WriteLn;
                IF indent > 0 THEN
                    OS2.VioWrtCharStr (Blanks, indent, CurrentRow, 0, 0);
                    CurrentColumn := indent;
                END (*IF*);
            END (*IF*);
            amount := NoOfColumns - CurrentColumn;
            IF amount > length THEN
                amount := length;
            END (*IF*);
            OS2.VioWrtCharStr (text, amount, CurrentRow, CurrentColumn, 0);
            INC (CurrentColumn, amount);
            DEC (length, amount);
            IF length > 0 THEN
                Strings.Delete (text, 0, amount);
            END (*IF*);
        END (*WHILE*);
    END WriteString;

(************************************************************************)

PROCEDURE WriteChar (ch: CHAR);

    (* Writes one character, and updates the cursor.  This procedure    *)
    (* does not recognise the concept of a control character.  Every    *)
    (* possible value of ch produces something readable on the screen.  *)
    (* If we have run off the end of the current row, wraps to a        *)
    (* new line.                                                        *)

    VAR buffer: ARRAY [0..0] OF CHAR;

    BEGIN
        buffer[0] := ch;
        WriteString (buffer);
    END WriteChar;

(************************************************************************)
(*                      NUMERIC OUTPUT (DECIMAL)                        *)
(************************************************************************)

PROCEDURE WriteCard (number: CARDINAL);

    (* Writes a number to the screen.   *)

    VAR remainder: CARDINAL;

    BEGIN
        IF number > 9 THEN
            WriteCard (number DIV 10);
        END (*IF*);
        remainder := number MOD 10;
        WriteChar (CHR(remainder + ORD("0")));
    END WriteCard;

(************************************************************************)

PROCEDURE WriteInt (number: INTEGER);

    (* Writes a number to the screen.   *)

    BEGIN
        IF number < 0 THEN
            WriteChar ('-');  number := -number;
        END (*IF*);
        WriteCard (VAL(CARDINAL,number));
    END WriteInt;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

PROCEDURE DetachCheck;

    (* Sets the variable ProcessIsNotDetached. *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        ProcessIsNotDetached := pPib^.pib_ultype <> 4;
    END DetachCheck;

(************************************************************************)

PROCEDURE Initialise;

    VAR row, col: CARD16;
        vioModeInfoPtr: POINTER TO OS2.VIOMODEINFO;

    BEGIN
        (* Work out the screen size. *)

        FirstBottomRow := 0;
        MaxRowNumber := 24;
        NoOfColumns := 80;
        row := 0;  col := 0;
        SplitEnabled := TRUE;
        IF ProcessIsNotDetached THEN
            ALLOCATE64 (vioModeInfoPtr, SIZE(OS2.VIOMODEINFO));
            vioModeInfoPtr^.cb := SIZE (OS2.VIOMODEINFO);
            OS2.VioGetMode (vioModeInfoPtr^, 0);
            IF vioModeInfoPtr^.row = 0 THEN
                SplitEnabled := FALSE;
            ELSE
                MaxRowNumber := vioModeInfoPtr^.row - 1;
                NoOfColumns := vioModeInfoPtr^.col;
            END (*IF*);
            OS2.VioGetCurPos (row, col, 0);
            DISPOSE (vioModeInfoPtr);
        END (*IF*);

        (* Set the current row and column. *)

        CurrentRow := row;  CurrentColumn := col;

    END Initialise;

(************************************************************************)

BEGIN
    Fill[0] := ' ';  Fill[1] := CHR(7);
    indent := 0;
    DetachCheck;
    SplitEnabled := ProcessIsNotDetached;
    Initialise;
    OS2.DosCreateMutexSem (NIL, ScreenAccess, 0, FALSE);
END SplitScreen.

