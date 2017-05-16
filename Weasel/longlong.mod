(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE LONGLONG;

        (********************************************************)
        (*                                                      *)
        (*            Support for 64-bit integers               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            17 October 2001                 *)
        (*  Last edited:        7 May 2017                      *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT CARD16;
FROM Types IMPORT CARD64, INT64;

(************************************************************************)

PROCEDURE Compare64 (A, B: CARD64): INTEGER;

    (* Returns >0 if A>B, =0 if A=B, <0 if A<B.  *)

    BEGIN
        IF A.high > B.high THEN
            RETURN +1;
        ELSIF A.high < B.high THEN
            RETURN -1;
        ELSE
            IF A.low > B.low THEN
                RETURN +1;
            ELSIF A.low < B.low THEN
                RETURN -1;
            ELSE
                RETURN 0;
            END (*IF*);
        END (*IF*);
    END Compare64;

(************************************************************************)

PROCEDURE Add64 (VAR (*INOUT*) A: CARD64;  B: CARDINAL);

    (* Computes  A := A + B.  This differs from Sum64 (below) in the    *)
    (* type of B, and in the way the result is returned.                *)

    BEGIN
        IF A.low > MAX(CARDINAL) - B THEN
            A.low := A.low - (MAX(CARDINAL) - B + 1);
            INC (A.high);
        ELSE
            A.low := A.low + B;
        END (*IF*);
    END Add64;

(************************************************************************)

PROCEDURE Sub64 (VAR (*INOUT*) A: CARD64;  B: CARDINAL);

    (* Computes  A := A - B.  This differs from Diff64 (below) in the   *)
    (* type of B, and in the way the result is returned.                *)

    BEGIN
        IF A.low < B THEN
            A.low := A.low + (MAX(CARDINAL) - B + 1);
            DEC (A.high);
        ELSE
            A.low := A.low - B;
        END (*IF*);
    END Sub64;

(************************************************************************)

PROCEDURE DEC64 (VAR (*INOUT*) A: CARD64);

    (* Decrements A by 1.  *)

    BEGIN
        IF A.low = 0 THEN
            A.low := MAX(CARDINAL);
            DEC (A.high);
        ELSE
            DEC (A.low);
        END (*IF*);
    END DEC64;

(************************************************************************)

PROCEDURE Sum64 (A, B: CARD64): CARD64;

    (* Returns A+B. *)

    VAR result: CARD64;

    BEGIN
        result := A;
        IF result.low > MAX(CARDINAL) - B.low THEN
            DEC (result.low, MAX(CARDINAL) - B.low + 1);
            INC (result.high);
        ELSE
            INC (result.low, B.low);
        END (*IF*);
        INC (result.high, B.high);
        RETURN result;
    END Sum64;

(************************************************************************)

PROCEDURE LongSub64 (VAR (*INOUT*) A: CARD64;  B: CARD64);

    (* Computes  A := A - B  *)

    VAR borrow: BOOLEAN;

    BEGIN
        borrow := A.low < B.low;
        IF borrow THEN
            INC (A.low, MAX(CARDINAL) - B.low + 1);
        ELSE
            DEC (A.low, B.low);
        END (*IF*);
        DEC (A.high, B.high);
        IF borrow THEN
            DEC (A.high);
        END (*IF*);
    END LongSub64;

(************************************************************************)

PROCEDURE Diff64 (A, B: CARD64): INT64;

    (* Returns A-B. *)

    VAR result: INT64;  borrow: BOOLEAN;

    BEGIN
        result.high := A.high;
        result.low := A.low;
        borrow := A.low < B.low;
        IF borrow THEN
            INC (result.low, MAX(CARDINAL) - B.low + 1);
        ELSE
            DEC (result.low, B.low);
        END (*IF*);

        DEC (result.high, B.high);
        IF borrow THEN
            DEC (result.high);
        END (*IF*);
        RETURN result;
    END Diff64;

(************************************************************************)

PROCEDURE ShortSub (A, B: CARD64): CARDINAL;

    (* Returns A-B as a CARDINAL value, or MAX(CARDINAL) in the case of *)
    (* overflow.                                                        *)

    BEGIN
        LongSub64 (A, B);
        IF A.high = 0 THEN RETURN A.low
        ELSE RETURN MAX(CARDINAL)
        END (*IF*);
    END ShortSub;

(************************************************************************)

PROCEDURE Mul64 (A, B: CARD64): CARD64;

    (* Returns A*B. *)

    CONST scale = 65536;

    (********************************************************************)

    PROCEDURE ADC (from: CARD16;  VAR (*INOUT*) to: CARD16;
                                  VAR (*INOUT*) carry: BOOLEAN);

        (* Add 'from' to 'to', with carry propagation. *)

        VAR result: CARDINAL;

        BEGIN
            result := from + to;
            IF carry THEN INC(result) END(*IF*);
            carry := result >= scale;
            IF carry THEN DEC(result, scale) END(*IF*);
            to := result;
        END ADC;

    (********************************************************************)

    VAR ans: ARRAY [0..5] OF CARD16;
        AA, BB: ARRAY [0..3] OF CARD16;
        part: CARDINAL;
        result: CARD64;
        j, k, m: [0..5];
        carry: BOOLEAN;

    BEGIN
        (* Clear the ans array. *)

        FOR j := 0 TO 5 DO
           ans[j] := 0;
        END (*FOR*);

        (* Break A and B down into 16-bit sections. *)

        AA[3] := A.high DIV scale;
        AA[2] := A.high MOD scale;
        AA[1] := A.low DIV scale;
        AA[0] := A.low MOD scale;
        BB[3] := B.high DIV scale;
        BB[2] := B.high MOD scale;
        BB[1] := B.low DIV scale;
        BB[0] := B.low MOD scale;

        (* Do the multiplication as a combination of 16-bit     *)
        (* multiplications, since that is what our built-in     *)
        (* multiplication can handle without losing the part    *)
        (* that overflows to the high part.                     *)

        FOR j := 0 TO 3 DO
            FOR k := 0 TO 3-j DO
                part := AA[j]*BB[k];
                carry := FALSE;
                m := j+k;
                ADC (part MOD scale, ans[m], carry);
                ADC (part DIV scale, ans[m+1], carry);
                IF carry THEN INC(ans[m+2]) END(*IF*);
            END (*FOR*);
        END (*FOR*);

        (* Copy the ans array to the final result. *)

        result.low  := scale*ans[1] + ans[0];
        result.high := scale*ans[3] + ans[2];
        RETURN result;

    END Mul64;

(************************************************************************)

PROCEDURE ShortMul64 (A: CARD64;  B: CARDINAL): CARD64;

    (* Returns A*B, for the case where the second operand is a CARDINAL. *)

    VAR BB: CARD64;

    BEGIN
        (* It's probably good enough to do this without *)
        (* trying to optimise.                          *)

        BB.low := B;
        BB.high := 0;
        RETURN Mul64 (A, BB);

    END ShortMul64;

(************************************************************************)

PROCEDURE LongDiv64 (A: CARD64;  B: CARDINAL;  VAR (*OUT*) quotient: CARD64;
                                  VAR (*OUT*) remainder: CARDINAL);

    (* Divides A by B, returns quotient and remainder. *)

    CONST mask = MAX(CARDINAL) DIV 2 + 1;    (* 2^31 *)

    VAR Q, R, BZ: CARD64;  Z: CARDINAL;
        carry: BOOLEAN;

    BEGIN
        (* Initial step: get a first approximation to the quotient Q,   *)
        (* and hence an initial value for the remainder R.              *)

        Q.high := A.high DIV B;
        R.high := A.high MOD B;
        Q.low  := 0;
        R.low  := A.low;

        (* Invariant: A = Q*B + R *)

        (* Notation: Z is our current approximation to R DIV B, and     *)
        (* BZ holds the value of B*Z.  If Z is too large we will do     *)
        (* nothing (but reduce the value of Z for the next iteration).  *)
        (* If Z is not too large then we will decrement R by BZ, and    *)
        (* increment Q by Z.                                            *)

        (*  Z := CARD64{0,1};  BZ := CARD64{0,B};  *)

        (* Observation: at this point in the calculation we             *)
        (* know that R.high < B.  Also, by construction, BZ.high = B.   *)
        (* It follows that R < BZ, so we can pull the first loop        *)
        (* iteration out as a special case.                             *)

        (* Divide Z by 2. *)

        Z := mask;

        (* Divide BZ by 2. *)

        IF ODD(B) THEN
            BZ.low := mask;
        ELSE
            BZ.low := 0;
        END (*IF*);
        BZ.high := B DIV 2;

        (* Now the main reduction loop, where we divide Z by 2 each     *)
        (* time around the loop.                                        *)

        LOOP
            (* Reduction step if R >= BZ.  *)

            IF (R.high > BZ.high) OR
                    ((R.high = BZ.high) AND (R.low >= BZ.low)) THEN
                Add64 (Q, Z);
                LongSub64 (R, BZ);
            END (*IF*);

            (* Once R is down to single precision, we can finish the    *)
            (* job outside the loop.                                    *)

            IF R.high = 0 THEN
                EXIT (*LOOP*);
            END (*IF*);

            (* Divide Z by 2. *)

            Z := Z DIV 2;

            (* Divide BZ by 2. *)

            carry   := ODD (BZ.high);
            BZ.high := BZ.high DIV 2;
            BZ.low  := BZ.low DIV 2;
            IF carry THEN
                INC (BZ.low, mask);
            END (*IF*);

        END (*LOOP*);

        (* Final step: once R.high = 0, we can finish the job using     *)
        (* CARD32 arithmetic.                                           *)

        Add64 (Q, R.low DIV B);
        R.low := R.low MOD B;

        quotient := Q;  remainder := R.low;

    END LongDiv64;

(************************************************************************)

PROCEDURE ShortDiv (A: CARD64;  B: CARDINAL): CARDINAL;

    (* Returns A DIV B as a CARDINAL value, or MAX(CARDINAL) if the     *)
    (* result overflows.                                                *)

    VAR Q: CARD64;  R: CARDINAL;

    BEGIN
        LongDiv64 (A, B, Q, R);
        IF Q.high = 0 THEN RETURN Q.low
        ELSE RETURN MAX(CARDINAL)
        END (*IF*);
    END ShortDiv;

(************************************************************************)

PROCEDURE Div10 (number: CARD64;  VAR (*OUT*) quotient: CARD64;
                                  VAR (*OUT*) remainder: CARDINAL);

    (* Divides number by 10, returns quotient and remainder. *)

    BEGIN
        LongDiv64 (number, 10, quotient, remainder);
    END Div10;

(************************************************************************)

PROCEDURE FLOAT64 (number: CARD64): REAL;

    (* Converts CARD64 to REAL. *)

    CONST scale = FLOAT(MAX(CARDINAL)) + 1.0;

    BEGIN
        RETURN scale*FLOAT(number.high) + FLOAT(number.low);
    END FLOAT64;

(************************************************************************)

PROCEDURE ORDL (number: INT64): CARDINAL;

    (* Converts INT64 to CARDINAL. *)

    BEGIN
        IF number.high <> 0 THEN RETURN MAX(CARDINAL)
        ELSE RETURN number.low;
        END (*IF*)
    END ORDL;

(************************************************************************)

END LONGLONG.

