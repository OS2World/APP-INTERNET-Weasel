(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2016   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE LowLevel;

        (********************************************************)
        (*                                                      *)
        (*         Miscellaneous low-level procedures           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        15 December 2106                *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, INT8, CARD16, INT16, LOC, ADDRESS,
    (* proc *)  ROTATE, SHIFT, ADDADR, SUBADR, MOVE, FILL, CAST;

FROM Types IMPORT
    (* type *)  FarPointer;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    t01 = [0..1];  t02 = [0..2];  t03 = [0..3];

    Word8 =  RECORD
                CASE :t01 OF
                  | 0:  bits: SET OF [0..7];
                  | 1:  c: CARD8;
                END (*CASE*);
             END (*RECORD*);

    Word16 =  RECORD
                CASE :t02 OF
                  | 0:  bits: BITSET;
                  | 1:  low, high: CARD8;
                  | 2:  c: CARD16;
                END (*CASE*);
              END (*RECORD*);

    Word32 =  RECORD
                CASE :t03 OF
                  | 0:  bits: BITSET;
                  | 1:  low, high, higher, highest: CARD8;
                  | 2:  c: CARDINAL;
                  | 3:  a: ADDRESS;
                END (*CASE*);
              END (*RECORD*);

    Double = RECORD
                CASE :t02 OF
                  | 0:  low, high: CARD16;
                  | 1:  lw: CARDINAL;
                  | 2:  a: ADDRESS;
                END (*CASE*);
             END (*RECORD*);

(************************************************************************)
(*                           MISCELLANEOUS                              *)
(************************************************************************)

PROCEDURE EVAL (f: ARRAY OF LOC);

    (* A do-nothing procedure - we use it for evaluating a function and *)
    (* ignoring the result.                                             *)

    BEGIN
    END EVAL;

(************************************************************************)

PROCEDURE Assert (arg: BOOLEAN);

    (* Crashes the program if arg is FALSE. *)

    VAR ch: CHAR;  p: POINTER TO CHAR;

    BEGIN
        IF NOT arg THEN
            p := NIL;  ch := p^;
        END (*IF*);
    END Assert;

(************************************************************************)
(*                      INTERNAL TYPE CONVERSIONS                       *)
(************************************************************************)

(*
PROCEDURE ByteToWord (b: CARD8): CARDINAL;

    (* Converts a byte to a word (with no sign extension). *)

    VAR result: Word32;

    BEGIN
        result.c := 0;
        result.low := b;
        RETURN result.c;
    END ByteToWord;
*)

(************************************************************************)

(*
PROCEDURE ByteToCard (b: BYTE): CARDINAL;

    (* Converts a byte to a cardinal. *)

    BEGIN
        RETURN VAL(CARDINAL,CAST(CARD8,b));
    END ByteToCard;
*)

(************************************************************************)
(*                         STORAGE ALLOCATION                           *)
(************************************************************************)

PROCEDURE ALLOCATE64 (VAR (*OUT*) p: ADDRESS;  amount: CARDINAL);

    (* Like Storage.ALLOCATE, but this procedure tries to ensure that   *)
    (* the allocated block does not cross a 64K boundary in memory.     *)
    (* This restriction is required when passing data to and from the   *)
    (* 16-bit API procedures (Vio, etc.)                                *)

    (********************************************************************)

    PROCEDURE Wrap64K (q: ADDRESS): BOOLEAN;

        (* Returns TRUE iff q^ crosses a 64Kbyte boundary.  *)

        VAR first: CARDINAL;

        BEGIN
            first := CAST(CARDINAL,q);
            RETURN HighWord(first) <> HighWord(first + amount - 1);
        END Wrap64K;

    (********************************************************************)

    VAR head, q: POINTER TO ADDRESS;

    BEGIN
        IF amount = 0 THEN p := NIL
        ELSIF amount > 65535 THEN
            (* There's no solution in this case *)
            ALLOCATE (p, amount);
        ELSE
            head := NIL;

            (* Keep allocating blocks until one doesn't cross a 64K boundary. *)

            LOOP
                ALLOCATE (q, amount);
                IF Wrap64K (q) THEN
                    q^ := head;
                    head := q;
                ELSE
                    p := q;
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);

            (* Now deallocate the ones that didn't work out. *)

            WHILE head <> NIL DO
                q := head^;
                DEALLOCATE (head, amount);
                head := q;
            END (*WHILE*);

        END (*IF*);

    END ALLOCATE64;

(************************************************************************)
(*                          BITWISE LOGIC                               *)
(************************************************************************)

PROCEDURE SwapIt (VAR (*INOUT*) arg: ARRAY OF LOC);

    (* Reverses the byte order of its argument.  Used for conversion    *)
    (* between BigEndian and LittleEndian format.                       *)

    VAR j, top: CARDINAL;  temp: LOC;

    BEGIN
        top := HIGH(arg);
        FOR j := 0 TO top DIV 2 DO
            temp := arg[j];  arg[j] := arg[top-j];  arg[top-j] := temp;
        END (*FOR*);
    END SwapIt;

(************************************************************************)

PROCEDURE IAND (first, second: CARDINAL): CARDINAL;

    (* Bit-by-bit logical AND.  *)

    VAR a, b, result: Word32;

    BEGIN
        a.c := first;  b.c := second;
        result.bits := a.bits * b.bits;
        RETURN result.c;
    END IAND;

(************************************************************************)

PROCEDURE IANDB (first, second: CARD8): CARD8;

    (* Bit-by-bit logical AND for bytes.        *)

    BEGIN
        RETURN LowByte(IAND(first, second));
    END IANDB;

(************************************************************************)

PROCEDURE IOR (first, second: CARDINAL): CARDINAL;

    (* Bit-by-bit inclusive OR. *)

    VAR a, b, result: Word32;

    BEGIN
        a.c := first;  b.c := second;
        result.bits := a.bits + b.bits;
        RETURN result.c;
    END IOR;

(************************************************************************)

PROCEDURE IORB (first, second: CARD8): CARD8;

    (* Bit-by-bit inclusive OR. *)

    BEGIN
        RETURN LowByte(IOR(first, second));
    END IORB;

(************************************************************************)

PROCEDURE IXOR (first, second: CARDINAL): CARDINAL;

    (* Bit-by-bit exclusive OR. *)

    VAR a, b, result: Word32;

    BEGIN
        a.c := first;  b.c := second;
        result.bits := a.bits / b.bits;
        RETURN result.c;
    END IXOR;

(************************************************************************)

PROCEDURE IXORB (first, second: CARD8): CARD8;

    (* Bit-by-bit exclusive OR. *)

    BEGIN
        RETURN LowByte(IXOR(first, second));
    END IXORB;

(************************************************************************)

PROCEDURE INOT (value: CARDINAL): CARDINAL;

    (* Bit-by-bit Boolean complement.   *)

    BEGIN
        RETURN MAX(CARDINAL)-value;
    END INOT;

(************************************************************************)

PROCEDURE INOTB (value: CARD8): CARD8;

    (* Bit-by-bit Boolean complement.   *)

    BEGIN
        RETURN 0FFH-value;
    END INOTB;

(************************************************************************)

PROCEDURE ROL (value: CARDINAL;  count: CARDINAL): CARDINAL;

    (* Left rotation of "value" by "count" bit positions.       *)

    VAR temp: Word32;

    BEGIN
        count := count MOD 32;
        temp.c := value;
        temp.bits := ROTATE (temp.bits, VAL(INTEGER,count));
        RETURN temp.c;
    END ROL;

(************************************************************************)

PROCEDURE ROLB (value: CARD8;  count: CARDINAL): CARD8;

    (* Left rotation of "value" by "count" bit positions.       *)

    VAR temp: Word8;

    BEGIN
        count := count MOD 8;
        temp.c := value;
        temp.bits := ROTATE (temp.bits, VAL(INTEGER,count));
        RETURN temp.c;
    END ROLB;

(************************************************************************)

PROCEDURE LS (value: CARDINAL;  count: CARDINAL): CARDINAL;

    (* Left shift of "value" by "count" bit positions, with zero fill.  *)

    VAR temp: Word32;

    BEGIN
        temp.c := value;
        IF count > 31 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.c;
    END LS;

(************************************************************************)

PROCEDURE LSB (value: CARD8;  count: CARDINAL): CARD8;

    (* Left shift of "value" by "count" bit positions, with zero fill.  *)

    VAR temp: Word8;

    BEGIN
        temp.c := value;
        IF count > 7 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.c;
    END LSB;

(************************************************************************)

PROCEDURE ROR (value: CARDINAL;  count: CARDINAL): CARDINAL;

    (* Right rotation of "value" by "count" bit positions.      *)

    VAR temp: Word32;

    BEGIN
        count := count MOD 32;
        temp.c := value;
        temp.bits := ROTATE (temp.bits, -VAL(INTEGER,count));
        RETURN temp.c;
    END ROR;

(************************************************************************)

PROCEDURE RORB (value: CARD8;  count: CARDINAL): CARD8;

    (* Right rotation of "value" by "count" bit positions.      *)

    VAR temp: Word8;

    BEGIN
        count := count MOD 8;
        temp.c := value;
        temp.bits := ROTATE (temp.bits, -VAL(INTEGER,count));
        RETURN temp.c;
    END RORB;

(************************************************************************)

PROCEDURE RS (value, count: CARDINAL): CARDINAL;

    (* Right shift of "value" by "count" bit positions, with zero fill. *)

    VAR temp: Word32;

    BEGIN
        temp.c := value;
        IF count > 31 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, -VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.c;
    END RS;

(************************************************************************)

PROCEDURE RSB (value: CARD8;  count: CARDINAL): CARD8;

    (* Right shift of "value" by "count" bit positions, with zero fill. *)

    VAR temp: Word8;

    BEGIN
        temp.c := value;
        IF count > 7 THEN temp.c := 0;
        ELSIF count > 0 THEN
            temp.bits := SHIFT (temp.bits, -VAL(INTEGER,count));
        END (*IF*);
        RETURN temp.c;
    END RSB;

(************************************************************************)
(*                          POINTER OPERATIONS                          *)
(*   Remark: this group of procedures was originally designed for a     *)
(*   segmented memory model.  XDS uses flat 32-bit addresses, and we    *)
(*   handle this by treating the segment part of an address as a dummy. *)
(************************************************************************)

PROCEDURE Far (A: ADDRESS): FarPointer;

    (* Converts a pointer to a far pointer. *)

    BEGIN
        RETURN A;
    END Far;

(************************************************************************)

PROCEDURE MakePointer (segment: CARD16;  offset: CARDINAL): FarPointer;

    (* Creates a pointer, given the segment and offset within segment.  *)

    VAR value: Word32;

    BEGIN
        value.c := offset;
        RETURN value.a;
    END MakePointer;

(************************************************************************)

PROCEDURE SEGMENT (A: ADDRESS): CARD16;

    (* Returns the segment part of an address.  *)

    BEGIN
        RETURN 0;
    END SEGMENT;

(************************************************************************)

PROCEDURE FarSEGMENT (A: FarPointer): CARD16;

    (* Returns the segment part of an address.  *)

    BEGIN
        RETURN 0;
    END FarSEGMENT;

(************************************************************************)

PROCEDURE OFFSET (A: ADDRESS): CARDINAL;

    (* Returns the offset part of an address.   *)

    VAR value: Word32;

    BEGIN
        value.a := A;
        RETURN value.c;
    END OFFSET;

(************************************************************************)

(*
PROCEDURE Virtual (PA: LONGCARD): FarPointer;

    (* Converts a physical address to a virtual address, if possible.   *)
    (* There are no guarantees in the case where there is no such       *)
    (* virtual address.                                                 *)

    CONST Sixteen = (*<FST 16L >*)  (*<~FST*) 16 (*>*);

    VAR value: Double;

    BEGIN
        value.low := LowWord(PA MOD Sixteen);
        value.high := LowWord(PA DIV Sixteen);
        RETURN value.a;
    END Virtual;
*)
(************************************************************************)
(*
PROCEDURE Physical (A: ADDRESS): LONGCARD;

    (* Converts a virtual address to a physical address.  Use with care!*)
    (* WARNING: This is going to give the wrong answer in a protected   *)
    (* mode environment.  I'm still not sure how to fix this.           *)

    VAR value: Double;

    BEGIN
        value.a := A;
        RETURN 16*CARDINAL(value.high) + CARDINAL(value.low);
    END Physical;
*)
(************************************************************************)

PROCEDURE AddOffset (A: ADDRESS;  increment: CARDINAL): ADDRESS;

    (* Returns a pointer to the memory location whose physical address  *)
    (* is Physical(A)+increment.  In the present version, it is assumed *)
    (* that the caller will never try to run off the end of a segment.  *)

    BEGIN
        RETURN ADDADR (A, increment);
    END AddOffset;

(************************************************************************)

PROCEDURE SubtractOffset (A: ADDRESS;  decrement: CARDINAL): ADDRESS;

    (* Like AddOffset, except that we go backwards in memory.  Running  *)
    (* off the beginning of the segment is an undetected error.         *)

    BEGIN
        RETURN SUBADR (A, decrement);
    END SubtractOffset;

(************************************************************************)

PROCEDURE FarAddOffset (A: FarPointer;  increment: CARDINAL): FarPointer;

    (* Like AddOffset, except for the parameter types. *)

    BEGIN
        RETURN ADDADR (A, increment);
    END FarAddOffset;

(************************************************************************)

PROCEDURE FarSubtractOffset (A: FarPointer; decrement: CARDINAL): FarPointer;

    (* Like SubtractOffset, except for the parameter types. *)

    BEGIN
        RETURN SUBADR (A, decrement);
    END FarSubtractOffset;

(************************************************************************)
(*                      BYTE/WORD/LONGCARD CONVERSIONS                  *)
(************************************************************************)

PROCEDURE LowByte (w: CARDINAL): CARD8;

    (* Returns the low-order byte of its argument.      *)

    VAR value: Word32;

    BEGIN
        value.c := w;
        RETURN value.low;
    END LowByte;

(************************************************************************)

PROCEDURE HighByte (w: CARD16): CARD8;

    (* Returns the high-order byte of its argument.     *)

    VAR value: Word16;

    BEGIN
        value.c := w;
        RETURN value.high;
    END HighByte;

(************************************************************************)

PROCEDURE MakeWord (high, low: CARD8): CARD16;

    (* Combines two bytes into a word.  The first argument becomes the  *)
    (* most significant byte of the result.                             *)

    VAR value: Word16;

    BEGIN
        value.low := low;
        value.high := high;
        RETURN value.c;
    END MakeWord;

(************************************************************************)

PROCEDURE SignExtend (val: INT8): INTEGER;

    (* Converts a signed 8-bit number to signed integer. *)

    BEGIN
        RETURN val;
    END SignExtend;

(************************************************************************)

PROCEDURE LowWord (w: CARDINAL): CARD16;

    (* Returns the low-order word of its argument.      *)

    VAR value: Double;

    BEGIN
        value.lw := w;
        RETURN ORD(value.low);
    END LowWord;

(************************************************************************)

PROCEDURE HighWord (w: CARDINAL): CARD16;

    (* Returns the high-order word of its argument.     *)

    VAR value: Double;

    BEGIN
        value.lw := w;
        RETURN ORD(value.high);
    END HighWord;

(************************************************************************)

PROCEDURE MakeLongword (high, low: CARD16): CARDINAL;

    (* Combines two words into a longword.  The first argument becomes  *)
    (* the most significant word of the result.                         *)

    VAR value: Double;

    BEGIN
        value.low := VAL(CARD16,low);
        value.high := VAL(CARD16,high);
        RETURN value.lw;
    END MakeLongword;

(************************************************************************)
(*                      MISCELLANEOUS ARITHMETIC                        *)
(************************************************************************)

PROCEDURE INCV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition   *)
    (* produced a carry.                                                *)

    BEGIN
        IF dest > MAX(CARDINAL) - src THEN
            DEC (dest, MAX(CARDINAL) - src + 1);
            RETURN TRUE;
        ELSE
            INC (dest, src);
            RETURN FALSE;
        END (*IF*);
    END INCV;

(************************************************************************)

PROCEDURE INCVB (VAR (*INOUT*) dest: CARD8;  src: CARD8): BOOLEAN;

    (* Computes dest := dest + src, and returns TRUE iff the addition   *)
    (* produced a carry.                                                *)

    VAR sum: CARDINAL;

    BEGIN
        sum := dest + src;
        dest := LowByte (sum);
        RETURN sum>255;
    END INCVB;

(************************************************************************)

PROCEDURE DECV (VAR (*INOUT*) dest: CARDINAL;  src: CARDINAL): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the            *)
    (* subtraction produced a borrow.                                   *)

    BEGIN
        IF dest < src THEN
            INC (dest, MAX(CARDINAL) - src + 1);  RETURN TRUE;
        ELSE
            DEC (dest, src);  RETURN FALSE;
        END (*IF*);
    END DECV;

(************************************************************************)

PROCEDURE DECVB (VAR (*INOUT*) dest: CARD8;  src: CARD8): BOOLEAN;

    (* Computes dest := dest - src, and returns TRUE iff the            *)
    (* subtraction produced a borrow.                                   *)

    VAR sval, dval: CARDINAL;

    BEGIN
        sval := src;
        dval := dest;
        IF dval < sval THEN
            dest := LowByte(dval + (256 - sval));
            RETURN TRUE;
        ELSE
            dest := LowByte(dval - sval);
            RETURN FALSE;
        END (*IF*);
    END DECVB;

(************************************************************************)

PROCEDURE Mul (A, B: CARD16): CARDINAL;

    (* Same as A*B, except for the type of the result.  We provide this *)
    (* as a general-purpose function since this combination of operands *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN A * B;
    END Mul;

(************************************************************************)

PROCEDURE MulB (A, B: CARD8): CARD16;

    (* Same as A*B, except for the type of the result.  We provide this *)
    (* as a general-purpose function since this combination of operands *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN A * B;
    END MulB;

(************************************************************************)

PROCEDURE IMul (A, B: INT16): INTEGER;

    (* Like Mul, but signed. *)

    BEGIN
        RETURN VAL(INTEGER,A) * VAL(INTEGER,B);
    END IMul;

(************************************************************************)

PROCEDURE IMulB (A, B: INT8): INT16;

    (* Like MulB, but signed. *)

    BEGIN
        RETURN SignExtend(A) * SignExtend(B);
    END IMulB;

(************************************************************************)

PROCEDURE DivB (A: CARD16;  B: CARD8): CARD8;

    (* Same as A DIV B, except for the type of A.  We provide this as   *)
    (* a general-purpose function since this combination of operands    *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN LowByte(A DIV B);
    END DivB;

(************************************************************************)

PROCEDURE Div (A: CARDINAL;  B: CARD16): CARD16;

    (* Same as A DIV B, except for the type of A.  We provide this as   *)
    (* a general-purpose function since this combination of operands    *)
    (* is often precisely what is wanted.                               *)

    BEGIN
        RETURN VAL(CARD16, A DIV VAL(CARDINAL,B));
    END Div;

(************************************************************************)
(*                           BLOCK MOVES                                *)
(************************************************************************)

PROCEDURE Copy (source, destination: ADDRESS;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.  In the case where the two arrays overlap,  *)
    (* the destination address should be lower in physical memory than  *)
    (* the source address.                                              *)

    BEGIN
        MOVE (source, destination, bytecount);
    END Copy;

(************************************************************************)

PROCEDURE FarCopy (source, destination: FarPointer;  bytecount: CARDINAL);

    (* Copies an array of bytes from the source address to the          *)
    (* destination address.  In the case where the two arrays overlap,  *)
    (* the destination address should be lower in physical memory than  *)
    (* the source address.                                              *)

    BEGIN
        MOVE (source, destination, bytecount);
    END FarCopy;

(************************************************************************)

PROCEDURE CopyUp (source, destination: FarPointer;  bytecount: CARDINAL);

    (* A variant of Copy which does the move backwards, in order        *)
    (* to handle the case where the destination address is inside the   *)
    (* source array.  In this special case Copy cannot be used,         *)
    (* because it would overwrite data it was about to copy.            *)

    (* Workaround: for now we don't have access to a suitable SYSTEM    *)
    (* procedure for backwards copying, so I'm doing the job by copying *)
    (* to an intermediate buffer.                                       *)

    VAR p: FarPointer;

    BEGIN
        IF bytecount > 0 THEN
            ALLOCATE (p, bytecount);
            MOVE (source, p, bytecount);
            MOVE (p, destination, bytecount);
            DEALLOCATE (p, bytecount);
        END (*IF*);
    END CopyUp;

(************************************************************************)

PROCEDURE BlockFill (destination: FarPointer;
                                bytecount: CARDINAL;  value: CARD8);

    (* Fills the destination array with the given value.        *)

    BEGIN
        FILL (destination, value, bytecount);
    END BlockFill;

(************************************************************************)

PROCEDURE BlockFillWord (destination: FarPointer;  wordcount: CARDINAL;
                                                        value: CARD16);

    (* Fills the destination array with the given value.        *)

    VAR p: POINTER TO CARD16;  j: CARDINAL;

    BEGIN
        p := destination;
        FOR j := 1 TO wordcount DO
            p^ := value;  p := AddOffset(p, 2);
        END (*FOR*);
    END BlockFillWord;

(************************************************************************)
(*                          INPUT AND OUTPUT                            *)
(************************************************************************)
(*
PROCEDURE OutByte (port: CARDINAL; value: BYTE);

    (* Puts the value out to an output port.    *)

    BEGIN
        (*<TopSpeed*) SYSTEM.Out (port, value); (*>*)
        (*<FST
        ASM
            MOV DX, port
            MOV AL, value
            OUT DX, AL
        END (*ASM*);
        >*)
    END OutByte;

(************************************************************************)

PROCEDURE InByte (port: CARDINAL): BYTE;

    (* Reads a byte from an input port. *)

    (*<FST VAR result: BYTE; >*)

    BEGIN
        RETURN SYSTEM.In (port);
    END InByte;

*)

(************************************************************************)
(*
PROCEDURE InStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
                                                count: CARDINAL);

    (* Reads count words from an input port.    *)

    VAR j: CARDINAL;  p: POINTER TO WORD;
        lobyte, hibyte: BYTE;

    BEGIN
        p := BufferAddress;
        FOR j := 1 TO count DO
            p^ := SYSTEM.InW (port);
            Lib.IncAddr (p, 2);
        END (*FOR*);
    END InStringWord;
*)
(************************************************************************)
(*
PROCEDURE OutStringWord (port: CARDINAL;  BufferAddress: ADDRESS;
                                                count: CARDINAL);

    (* Writes count words to an output port.    *)

    VAR j: CARDINAL;  p: POINTER TO WORD;

    BEGIN
        p := BufferAddress;
        FOR j := 1 TO count DO
            SYSTEM.OutW (port, p^);
            Lib.IncAddr (p, 2);
        END (*FOR*);
    END OutStringWord;
*)
(************************************************************************)

END LowLevel.
