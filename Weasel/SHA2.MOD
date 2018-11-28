(**************************************************************************)
(*                                                                        *)
(*  SHA-256 digest                                                        *)
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

IMPLEMENTATION MODULE SHA2;

        (********************************************************)
        (*                                                      *)
        (*              SHA-224 and SHA-256 digest              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 December 2016                *)
        (*  Last edited:        28 January 2018                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*  The standard governing this code is Federal         *)
        (*  Information Processing Standards (FIPS) PUB 180-4,  *)
        (*  available from http://csrc.nist.gov                 *)
        (*                                                      *)
        (*  Technically, the SHA2 family also includes SHA-384  *)
        (*  and SHA-512, plus some variants, but I have found   *)
        (*  it convenient to implement those in a separate      *)
        (*  module SHA512, because those versions introduce the *)
        (*  extra complication of 64-bit integer arithmetic.    *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)
(*                                                                      *)
(* Remark: the SHA family of algorithms rely heavily on big-endian      *)
(* arithmetic.  It looks to me, however, as if we can                   *)
(* do all the arithmetic in "native" format, except for three details:  *)
(*  (a) the incoming data, a sequence of bytes, have to be interpreted  *)
(*      as a sequence of 32-bit big-endian numbers, so we have to       *)
(*      interpret each group of 4 bytes as a big-endian number.         *)
(*  (b) the padding at the end includes a 64-bit length, and this must  *)
(*      be converted to big-endian.                                     *)
(*  (c) big-endian arithmetic must be used to convert a digest to hex.  *)
(*                                                                      *)
(************************************************************************)

(************************************************************************)
(*                                                                      *)
(* SHA-224 is identical to SHA-256, except that:                        *)
(*  - the initial hash values h[0] through h[7] are different, and      *)
(*  - the output is constructed by omitting h[7].                       *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)  LOC, ADDRESS, CARD8, CARD32,
    (* proc *)  MOVE, ADR, FILL;

FROM Arith64 IMPORT
    (* const*)  Zero64,
    (* type *)  CARD64,
    (* proc *)  Add64;

FROM BEDigests IMPORT
    (* proc *)  BEDigestToString;

FROM LowLevel IMPORT
    (* proc *)  SwapIt, IXOR, IAND, INOT, LS, ROR, RS;

    (* Remark: we rely rather heavily on the fact that CARDINAL in module *)
    (* LowLevel is the same type as CARD32.                               *)

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    (* Array types used in the calculations. *)

    Block64 = ARRAY [0..63] OF CARDINAL;
    Byte64  = ARRAY [0..63] OF CARD8;

    (* SHA-256 context.  The fields are:                                *)
    (*     state        the state of the algorithm                      *)
    (*     variant      7 for SHA-224, 8 for SHA-256.                   *)
    (*     bytecount    number of data bytes we've processed            *)
    (*     buffer       the input data that we haven't yet processed    *)
    (*                  because the 64-byte block is not yet full.      *)

    SHA2_CTX_RECORD = RECORD
                          state: SHA2_DigestType;
                          variant: CARDINAL;
                          bytecount: CARD64;
                          buffer: Byte64;
                      END (*RECORD*);

    SHA2_CTX = POINTER TO SHA2_CTX_RECORD;

CONST
    (* First 32 bits of the fractional parts of the cube roots of the first 64 primes 2..311 *)

    k = Block64 {
            0428A2F98H, 071374491H, 0B5C0FBCFH, 0E9B5DBA5H, 03956C25BH, 059F111F1H, 0923F82A4H, 0AB1C5ED5H,
            0D807AA98H, 012835B01H, 0243185BEH, 0550C7DC3H, 072BE5D74H, 080DEB1FEH, 09BDC06A7H, 0C19BF174H,
            0E49B69C1H, 0EFBE4786H, 00FC19DC6H, 0240CA1CCH, 02DE92C6FH, 04A7484AAH, 05CB0A9DCH, 076F988DAH,
            0983E5152H, 0A831C66DH, 0B00327C8H, 0BF597FC7H, 0C6E00BF3H, 0D5A79147H, 006CA6351H, 014292967H,
            027B70A85H, 02E1B2138H, 04D2C6DFCH, 053380D13H, 0650A7354H, 0766A0ABBH, 081C2C92EH, 092722C85H,
            0A2BFE8A1H, 0A81A664BH, 0C24B8B70H, 0C76C51A3H, 0D192E819H, 0D6990624H, 0F40E3585H, 0106AA070H,
            019A4C116H, 01E376C08H, 02748774CH, 034B0BCB5H, 0391C0CB3H, 04ED8AA4AH, 05B9CCA4FH, 0682E6FF3H,
            0748F82EEH, 078A5636FH, 084C87814H, 08CC70208H, 090BEFFFAH, 0A4506CEBH, 0BEF9A3F7H, 0C67178F2H};

    (* Initial seed for the SHA-224 case. *)

    seed224 = SHA2_DigestType{
                        0C1059ED8H, 0367CD507H, 03070DD17H, 0F70E5939H,
                        0FFC00B31H, 068581511H, 064F98FA7H, 0BEFA4FA4H};

    (* Initial seed for the SHA-256 case. *)
    (* First 32 bits of the fractional parts of the square roots of the first 8 primes 2..19)  *)

    seed256 = SHA2_DigestType{
                        06A09E667H, 0BB67AE85H, 03C6EF372H, 0A54FF53AH,
                        0510E527FH, 09B05688CH, 01F83D9ABH, 05BE0CD19H};

(********************************************************************************)

VAR
    (* The padding bytes used at the end to round up the data to an integral    *)
    (* number of blocks.  Although this is logically a constant array, it's     *)
    (* easier to do copy operations if it's stored as a variable.               *)

    PADDING: Byte64;

(************************************************************************)
(*                        MISCELLANEOUS UTILITIES                       *)
(************************************************************************)

PROCEDURE Mul8 (VAR (*INOUT*) A: CARD64): CARD64;

    (* Multiplies its argument by 8. *)

    VAR toshift: CARDINAL;  result: CARD64;

    BEGIN
        toshift := IAND(A.low, 0E0000000H);
        result.low := LS(A.low - toshift, 3);
        result.high := LS(A.high,3) + RS(toshift, 32-3);
        RETURN result;
    END Mul8;

(********************************************************************************)
(*                          PROCESSING ONE BLOCK OF DATA                        *)
(********************************************************************************)

PROCEDURE SHA2ProcessBlock (VAR (*INOUT*) state: SHA2_DigestType;  blockaddr: ADDRESS);

    (* This is the main part of the calculation.  The input is exactly 64 bytes *)
    (* long, and the end result of processing it is to update the state.        *)

    VAR w: Block64;  s0, s1, ch, T1, T2, maj: CARDINAL;
        a, b, c, d, e, f, g, h: CARDINAL;
        i: CARDINAL;

    BEGIN
        MOVE (blockaddr, ADR(w), 64);

        (* Note that the SHA-256 standard implicitly requires big-endian    *)
        (* storage, so we have to reverse the bytes of all the CARD32 data. *)

        FOR i := 0 TO 15 DO
            SwapIt (w[i]);
        END (*FOR*);

        <* COVERFLOW- *>   (* The algorithm relies on ignoring integer overflow *)

        (* Extend the first 16 words into the remaining 48 words w[16..63] of   *)
        (* the message schedule array.                                          *)

        FOR i := 16 TO 63 DO
            s0 := IXOR (IXOR (ROR(w[i-15], 7), ROR(w[i-15], 18)), RS(w[i-15], 3));
            s1 := IXOR (IXOR (ROR(w[i-2], 17), ROR(w[i-2], 19)), RS(w[i-2], 10));
            w[i] := w[i-16] + s0 + w[i-7] + s1;
        END (*FOR*);

        (* Initialize working variables to current hash value. *)

        a := state[0];
        b := state[1];
        c := state[2];
        d := state[3];
        e := state[4];
        f := state[5];
        g := state[6];
        h := state[7];

        (* Compression function main loop. *)

        FOR i := 0 TO 63 DO
            s1 := IXOR (IXOR (ROR(e, 6), ROR(e, 11)), ROR(e, 25));
            ch := IXOR (IAND (e, f), IAND (INOT(e), g));
            T1 := h + s1 + ch + k[i] + w[i];
            s0 := IXOR (IXOR (ROR (a, 2), ROR(a, 13)), ROR(a, 22));
            maj := IXOR (IXOR (IAND (a, b), IAND(a, c)), IAND(b, c));
            T2 := s0 + maj;

            h := g;
            g := f;
            f := e;
            e := d + T1;
            d := c;
            c := b;
            b := a;
            a := T1 + T2;

        END (*FOR*);

        (*  Add the compressed chunk to the current hash value:  *)

        INC (state[0], a);
        INC (state[1], b);
        INC (state[2], c);
        INC (state[3], d);
        INC (state[4], e);
        INC (state[5], f);
        INC (state[6], g);
        INC (state[7], h);

        <* COVERFLOW+ *>

    END SHA2ProcessBlock;

(********************************************************************************)
(*                      THE EXTERNALLY CALLABLE PROCEDURES                      *)
(********************************************************************************)

PROCEDURE SHA224Init(): SHA2_CTX;

    (* SHA-224 initialization. Begins a SHA-224 operation, creating *)
    (* a new context.                                               *)

    VAR context: SHA2_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            variant := 7;
            bytecount := Zero64;
            state := seed224;
        END (*WITH*);
        RETURN context;
    END SHA224Init;

(********************************************************************************)

PROCEDURE SHA256Init(): SHA2_CTX;

    (* SHA-256 initialization. Begins the operation, creating a new context. *)

    VAR context: SHA2_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            variant := 8;
            bytecount := Zero64;
            state := seed256;
        END (*WITH*);
        RETURN context;
    END SHA256Init;

(********************************************************************************)

PROCEDURE SHA2Update (context: SHA2_CTX;  data: ARRAY OF LOC;  length: CARDINAL);

    (* SHA-224 and SHA-256 block update operation. Continues a          *)
    (* message-digest operation, adding more data to what has already   *)
    (* been processed for this context.                                 *)

    VAR inpos, index: CARDINAL;

    BEGIN
        IF length > 0 THEN
            (* Work out where we're up to in the buffer. *)

            index := context^.bytecount.low MOD 64;

            (* Update total number of bytes we've processed so far. *)

            Add64 (context^.bytecount, length);

            (* Transform as many times as possible. *)

            inpos := 64 - index;
            IF length >= inpos THEN

                (* Fill the buffer up to capacity.  (It may already contain *)
                (* some data from the last call to SHA2Update.)             *)

                MOVE (ADR(data), ADR(context^.buffer[index]), inpos);
                SHA2ProcessBlock (context^.state, ADR(context^.buffer));

                (* Now process as much of our data as possible, until there's *)
                (* not enough left to make up a full 64-byte block.           *)

                WHILE inpos + 63 < length DO
                    SHA2ProcessBlock (context^.state, ADR(data[inpos]));
                    INC (inpos, 64);
                END (*LOOP*);
                index := 0;
            ELSE
                inpos := 0;
            END (*IF*);

            (* We have no more than 63 bytes of data left to process. *)
            (* Save it in the buffer for next time.                   *)

            IF length > inpos THEN
                MOVE (ADR(data[inpos]), ADR(context^.buffer[index]), length-inpos);
            END (*IF*);

        END (*IF*);

    END SHA2Update;

(********************************************************************************)

PROCEDURE SHA2Final (VAR (*INOUT*) context: SHA2_CTX;
                     VAR (*OUT*) digest: SHA2_DigestType);

    (* SHA-224 or SHA-256 finalization. Ends the message-digest     *)
    (* operation, returning the digest and discarding the context.  *)

    CONST TooBig = MAX(CARD32) DIV 8 + 1;

    VAR index, padLen: CARDINAL;
        LengthInBits: CARD64;

    BEGIN
        (* Calculate total number of data bits we've worked on. *)

        LengthInBits := Mul8 (context^.bytecount);

        (* Pad out to 56 mod 64. *)

        index := context^.bytecount.low MOD 64;
        IF index < 56 THEN
            padLen := 56 - index;
        ELSE
            (* We will have to start a new block.  Note 120 = 56+64. *)
            padLen := 120 - index;
        END (*IF*);
        SHA2Update (context, PADDING, padLen);

        (* Append length (before padding). *)

        SwapIt (LengthInBits.high);
        SHA2Update (context, LengthInBits.high, 4);
        SwapIt (LengthInBits.low);
        SHA2Update (context, LengthInBits.low, 4);

        (* Final result to be given to caller. *)

        digest := context^.state;

        (* Zeroize sensitive information, then dispose of this context. *)

        FILL (ADR(context^), VAL(CARD8,0), SIZE(SHA2_CTX_RECORD));
        DISPOSE (context);

    END SHA2Final;

(********************************************************************************)
(*                         CONVERTING DIGEST TO STRING                          *)
(********************************************************************************)

PROCEDURE SHA224DigestToString (VAR (*IN*) digest: SHA2_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 56-character string.  If there is not enough    *)
    (* space for 64 characters, produces a leading substring of the full result.*)

    BEGIN
        BEDigestToString (7, digest, result);
    END SHA224DigestToString;

(********************************************************************************)

PROCEDURE SHA256DigestToString (VAR (*IN*) digest: SHA2_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 64-character string.  If there is not enough    *)
    (* space for 64 characters, produces a leading substring of the full result.*)

    BEGIN
        BEDigestToString (8, digest, result);
    END SHA256DigestToString;

(********************************************************************************)

PROCEDURE SHA2FinalDigest (VAR (*INOUT*) context: SHA2_CTX;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* SHA-224 or SHA-256 finalization. Ends the message-digest operation,      *)
    (* returning the digest and discarding the context.  This is equivalent to  *)
    (* SHA2Final followed by converting the digest to a string, for callers     *)
    (* who don't need to know the actual digest value.                          *)

    VAR digest: SHA2_DigestType;  variant: CARDINAL;

    BEGIN
        variant := context^.variant;
        SHA2Final (context, digest);
        BEDigestToString (variant, digest, result);
    END SHA2FinalDigest;

(********************************************************************************)
(*                             INITIAL CONSTANTS                                *)
(********************************************************************************)

BEGIN
    PADDING := Byte64 {80H, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
END SHA2.

