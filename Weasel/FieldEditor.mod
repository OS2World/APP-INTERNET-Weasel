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

IMPLEMENTATION MODULE FieldEditor;

        (********************************************************)
        (*                                                      *)
        (*              Screen editing utilities                *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        14 May 1998                     *)
        (*  Status:             Seems to be OK                  *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, ADDRESS;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM Windows IMPORT
    (* type *)  Window,
    (* proc *)  WriteChar, EditString;

FROM NumericIO IMPORT
    (* proc *)  WriteHexByte, EditHexByte, WriteRJCard, EditCardinal;

FROM RealIO IMPORT
    (* proc *)  WriteReal, EditReal;

(************************************************************************)

TYPE
    BytePtr = POINTER TO CARD8;
    CardPtr = POINTER TO CARDINAL;
    RealPtr = POINTER TO REAL;
    (*
    <* M2EXTENSIONS + *>
    StringPtr = POINTER TO ARRAY OF CHAR;
    <* M2EXTENSIONS - *>
    *)
    StringPtr = POINTER TO ARRAY [0..511] OF CHAR;

    FieldType =  POINTER TO RECORD
                                FieldWriter: WriteProc;
                                FieldEditor: EditProc;
                            END (*RECORD*);

(************************************************************************)
(*              SCREEN OUTPUT FOR THE PREDEFINED TYPES                  *)
(************************************************************************)

PROCEDURE WriteCardinalField (w: Window;  p: ADDRESS;  width: CARDINAL);

    VAR address: CardPtr;

    BEGIN
        address := p;
        WriteRJCard (w, address^, width);
    END WriteCardinalField;

(************************************************************************)

PROCEDURE WriteByteField (w: Window;  p: ADDRESS;  width: CARDINAL);

    VAR address: BytePtr;

    BEGIN
        address := p;
        WriteHexByte (w, address^);
    END WriteByteField;

(************************************************************************)

PROCEDURE WriteRealField (w: Window;  p: ADDRESS;  width: CARDINAL);

    VAR address: RealPtr;

    BEGIN
        address := p;
        WriteReal (w, address^, width);
    END WriteRealField;

(************************************************************************)

PROCEDURE WriteStringField (w: Window;  p: ADDRESS;  width: CARDINAL);

    VAR address: StringPtr;  j: CARDINAL;  NoMore: BOOLEAN;

    BEGIN
        address := p;  NoMore := FALSE;
        FOR j := 0 TO width-1 DO
            NoMore := NoMore OR (address^[j] = CHR(0));
            IF NoMore THEN
                WriteChar (w, ' ');
            ELSE
                WriteChar (w, address^[j]);
            END (*IF*);
        END (*FOR*);
    END WriteStringField;

(************************************************************************)
(*                  EDITORS FOR THE PREDEFINED TYPES                    *)
(************************************************************************)

PROCEDURE EditByteField (w: Window;  VAR (*INOUT*) p: ADDRESS;
                                              dummy, width: CARDINAL);

    VAR address: BytePtr;

    BEGIN
        address := p;
        EditHexByte (w, address^);
    END EditByteField;

(************************************************************************)

PROCEDURE EditCardinalField (w: Window;  VAR (*INOUT*) p: ADDRESS;
                                         dummy, width: CARDINAL);

    VAR address: CardPtr;

    BEGIN
        address := p;
        EditCardinal (w, address^, width);
    END EditCardinalField;

(************************************************************************)

PROCEDURE EditRealField (w: Window;  VAR (*INOUT*) p: ADDRESS;
                                               dummy, width: CARDINAL);

    VAR address: RealPtr;

    BEGIN
        address := p;
        EditReal (w, address^, width);
    END EditRealField;

(************************************************************************)

PROCEDURE EditStringField (w: Window;  VAR (*INOUT*) p: ADDRESS;
                                               varsize, width: CARDINAL);

    VAR address: StringPtr;

    BEGIN
        address := p;
        EditString (w, address^, varsize, width);
    END EditStringField;

(************************************************************************)
(*                         COMPARING TYPES                              *)
(************************************************************************)

PROCEDURE SameType (t1, t2: FieldType): BOOLEAN;

    (* Returns TRUE iff t1 = t2.        *)

    BEGIN
        RETURN t1 = t2;
    END SameType;

(************************************************************************)
(*                      DEFINING A NEW TYPE                             *)
(************************************************************************)

PROCEDURE DefineFieldType (Writer: WriteProc;  Editor: EditProc): FieldType;

    (* Introduces a new field type into the system.  Writer is a        *)
    (* user-supplied procedure to write a variable of the new type.     *)
    (* Editor is the user-supplied procedure for editing a variable of  *)
    (* that type.                                                       *)

    VAR result: FieldType;

    BEGIN
        NEW (result);
        WITH result^ DO
            FieldWriter := Writer;
            FieldEditor := Editor;
        END (*WITH*);
        RETURN result;
    END DefineFieldType;

(************************************************************************)

PROCEDURE DiscardFieldType (type: FieldType);

    (* A notification from the user that this type will not be used     *)
    (* again (unless it is redefined by another call to procedure       *)
    (* DefineFieldType).  Use of this procedure is optional, but is     *)
    (* recommended for the sake of "clean" memory management.           *)

    BEGIN
        DISPOSE (type);
    END DiscardFieldType;

(************************************************************************)
(*                          SCREEN OUTPUT                               *)
(************************************************************************)

PROCEDURE WriteField (w: Window;  address: ADDRESS;  type: FieldType;
                                                        width: CARDINAL);

    (* Writes address^ on the screen at the current cursor position in  *)
    (* window w.  The width parameter specifies how many character      *)
    (* positions to use.  Use width=0 for variable-width fields for     *)
    (* which the write procedure for that type must work out the width. *)

    BEGIN
        WITH type^ DO
            FieldWriter (w, address, width);
        END (*WITH*);
    END WriteField;

(************************************************************************)
(*                           THE EDITOR                                 *)
(************************************************************************)

PROCEDURE EditField (w: Window;  VAR (*INOUT*) address: ADDRESS;
                              type: FieldType;  varsize, width: CARDINAL);

    (* Edits the variable at the given address, and of the given type,  *)
    (* at the current cursor position in window w.  The width parameter *)
    (* specifies how many character positions are to be used on the     *)
    (* screen.  Set width=0 for variable-width fields where the editor  *)
    (* must determine the width.  We leave this procedure on seeing a   *)
    (* keyboard character which does not belong to us.  The cursor is   *)
    (* left just beyond the last character of the field as it is        *)
    (* displayed.  The terminating keystroke is returned to the         *)
    (* keyboard driver so that it can still be read by the caller.      *)

    BEGIN
        WITH type^ DO
            FieldEditor (w, address, varsize, width);
        END (*WITH*);
    END EditField;

(************************************************************************)
(*                      SETTING UP THE PREDEFINED TYPES                 *)
(************************************************************************)

BEGIN
    Byte := DefineFieldType (WriteByteField, EditByteField);
    Cardinal := DefineFieldType (WriteCardinalField, EditCardinalField);
    Real := DefineFieldType (WriteRealField, EditRealField);
    String := DefineFieldType (WriteStringField, EditStringField);
FINALLY
    DiscardFieldType (Byte);
    DiscardFieldType (Cardinal);  DiscardFieldType (Real);
    DiscardFieldType (String);
END FieldEditor.

