(**************************************************************************)
(*                                                                        *)
(*  SHA-512 digest                                                        *)
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

<* WOFF316+ *>

IMPLEMENTATION MODULE SHA512;

        (********************************************************)
        (*                                                      *)
        (*              SHA-384 and SHA-512 digest              *)
        (*                                                      *)
        (*  Processes a data sequence of arbitrary length,      *)
        (*  producing a 48-byte or 64-byte "digest" (96 or 128  *)
        (*  hex characters) as a secure signature of this data  *)
        (*  sequence.                                           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 October 2017                  *)
        (*  Last edited:        28 January 2018                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*  The standard governing this code is Federal         *)
        (*  Information Processing Standards (FIPS) PUB 180-4,  *)
        (*  available from http://csrc.nist.gov                 *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* Remark: the SHA family of algorithms rely heavily on big-endian      *)
(* arithmetic.  It looks to me, however, as if we can                   *)
(* do all the arithmetic in "native" format, except for three details:  *)
(*  (a) the incoming data, a sequence of bytes, have to be interpreted  *)
(*      as a sequence of 64-bit big-endian numbers, so we have to       *)
(*      interpret each group of 8 bytes as a big-endian number.         *)
(*  (b) the padding at the end includes a 16-byte length, and this must *)
(*      be converted to big-endian.                                     *)
(*  (c) big-endian arithmetic must be used to convert a digest to hex.  *)
(*                                                                      *)
(* The big-endian assumption applies only to the scalars of type CARD64 *)
(* and (in the case of message length) CARD128.  Once we start working  *)
(* with arrays of these scalars the standard goes little-endian again,  *)
(* although you have to read between the lines to realise that.  That   *)
(* apparent contradiction caused me some headaches while writing this   *)
(* code.  Armed with this insight I can probably go back and improve    *)
(* module SHA2.                                                         *)
(*                                                                      *)
(************************************************************************)

(************************************************************************)
(*                                                                      *)
(* SHA-384 is identical to SHA-512, except that:                        *)
(*  - the initial hash values h[0] through h[7] are different, and      *)
(*  - the output is constructed by omitting h[7].                       *)
(*                                                                      *)
(* The standard also defines algorithms SHA-512/224 and SHA-512/256,    *)
(* but I'm not bothering with those unless and until I see evidence     *)
(* that I need them.                                                    *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)  LOC, ADDRESS, CARD8, CARD32,
    (* proc *)  MOVE, ADR, FILL;

FROM Arith64 IMPORT
    (* type *)  CARD64;

FROM BEDigests IMPORT
    (* proc *)  BEDigest64ToString;

FROM LowLevel IMPORT
    (* proc *)  SwapIt, IXOR, IAND, INOT, LS, ROR, RS;

    (* Remark: we rely rather heavily on the fact that CARDINAL in module *)
    (* LowLevel is the same type as CARD32.                               *)

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    (* 128-bit cardinal, used here only for message length. *)

    CARD128 = ARRAY [0..3] OF CARD32;

    (* Array types used in the calculations. *)

    Block80 = ARRAY [0..79] OF CARD64;
    Byte128  = ARRAY [0..127] OF CARD8;

    (* SHA-512 context.  The fields are:                                *)
    (*     state        the state of the algorithm                      *)
    (*     variant      12 for SHA-384, 16 for SHA-512.                 *)
    (*     bytecount    the number of bytes we have processed so far    *)
    (*     buffer       the input data that we haven't yet processed    *)
    (*                  because the 128-byte block is not yet full.     *)

    SHA512_CTX_RECORD = RECORD
                            state: SHA512_DigestType;
                            variant: CARDINAL;
                            bytecount: CARD128;
                            buffer: Byte128;
                        END (*RECORD*);

    SHA512_CTX = POINTER TO SHA512_CTX_RECORD;

CONST
    Zero128 = CARD128 {0, 0, 0, 0};

    (* First 64 bits of the fractional parts of the cube roots of the first 80 prime numbers *)

    K = Block80 {
                  CARD64{0428A2F98H, 0D728AE22H}, CARD64{071374491H, 023EF65CDH},
                  CARD64{0B5C0FBCFH, 0EC4D3B2FH}, CARD64{0E9B5DBA5H, 08189DBBCH},
                  CARD64{03956C25BH, 0F348B538H}, CARD64{059F111F1H, 0B605D019H},
                  CARD64{0923F82A4H, 0AF194F9BH}, CARD64{0AB1C5ED5H, 0DA6D8118H},
                  CARD64{0D807AA98H, 0A3030242H}, CARD64{012835B01H, 045706FBEH},
                  CARD64{0243185BEH, 04EE4B28CH}, CARD64{0550C7DC3H, 0D5FFB4E2H},
                  CARD64{072BE5D74H, 0F27B896FH}, CARD64{080DEB1FEH, 03B1696B1H},
                  CARD64{09BDC06A7H, 025C71235H}, CARD64{0C19BF174H, 0CF692694H},
                  CARD64{0E49B69C1H, 09EF14AD2H}, CARD64{0EFBE4786H, 0384F25E3H},
                  CARD64{00FC19DC6H, 08B8CD5B5H}, CARD64{0240CA1CCH, 077AC9C65H},
                  CARD64{02DE92C6FH, 0592B0275H}, CARD64{04A7484AAH, 06EA6E483H},
                  CARD64{05CB0A9DCH, 0BD41FBD4H}, CARD64{076F988DAH, 0831153B5H},
                  CARD64{0983E5152H, 0EE66DFABH}, CARD64{0A831C66DH, 02DB43210H},
                  CARD64{0B00327C8H, 098FB213FH}, CARD64{0BF597FC7H, 0BEEF0EE4H},
                  CARD64{0C6E00BF3H, 03DA88FC2H}, CARD64{0D5A79147H, 0930AA725H},
                  CARD64{006CA6351H, 0E003826FH}, CARD64{014292967H, 00A0E6E70H},
                  CARD64{027B70A85H, 046D22FFCH}, CARD64{02E1B2138H, 05C26C926H},
                  CARD64{04D2C6DFCH, 05AC42AEDH}, CARD64{053380D13H, 09D95B3DFH},
                  CARD64{0650A7354H, 08BAF63DEH}, CARD64{0766A0ABBH, 03C77B2A8H},
                  CARD64{081C2C92EH, 047EDAEE6H}, CARD64{092722C85H, 01482353BH},
                  CARD64{0A2BFE8A1H, 04CF10364H}, CARD64{0A81A664BH, 0BC423001H},
                  CARD64{0C24B8B70H, 0D0F89791H}, CARD64{0C76C51A3H, 00654BE30H},
                  CARD64{0D192E819H, 0D6EF5218H}, CARD64{0D6990624H, 05565A910H},
                  CARD64{0F40E3585H, 05771202AH}, CARD64{0106AA070H, 032BBD1B8H},
                  CARD64{019A4C116H, 0B8D2D0C8H}, CARD64{01E376C08H, 05141AB53H},
                  CARD64{02748774CH, 0DF8EEB99H}, CARD64{034B0BCB5H, 0E19B48A8H},
                  CARD64{0391C0CB3H, 0C5C95A63H}, CARD64{04ED8AA4AH, 0E3418ACBH},
                  CARD64{05B9CCA4FH, 07763E373H}, CARD64{0682E6FF3H, 0D6B2B8A3H},
                  CARD64{0748F82EEH, 05DEFB2FCH}, CARD64{078A5636FH, 043172F60H},
                  CARD64{084C87814H, 0A1F0AB72H}, CARD64{08CC70208H, 01A6439ECH},
                  CARD64{090BEFFFAH, 023631E28H}, CARD64{0A4506CEBH, 0DE82BDE9H},
                  CARD64{0BEF9A3F7H, 0B2C67915H}, CARD64{0C67178F2H, 0E372532BH},
                  CARD64{0CA273ECEH, 0EA26619CH}, CARD64{0D186B8C7H, 021C0C207H},
                  CARD64{0EADA7DD6H, 0CDE0EB1EH}, CARD64{0F57D4F7FH, 0EE6ED178H},
                  CARD64{006F067AAH, 072176FBAH}, CARD64{00A637DC5H, 0A2C898A6H},
                  CARD64{0113F9804H, 0BEF90DAEH}, CARD64{01B710B35H, 0131C471BH},
                  CARD64{028DB77F5H, 023047D84H}, CARD64{032CAAB7BH, 040C72493H},
                  CARD64{03C9EBE0AH, 015C9BEBCH}, CARD64{0431D67C4H, 09C100D4CH},
                  CARD64{04CC5D4BEH, 0CB3E42B6H}, CARD64{0597F299CH, 0FC657E2AH},
                  CARD64{05FCB6FABH, 03AD6FAECH}, CARD64{06C44198CH, 04A475817H} };

    (* Initial seed for SHA-384 takes the first 64 bits of the fractional parts of the square roots of the 9th through 16th prime number. *)

    seed384 = SHA512_DigestType{
                               CARD64{0CBBB9D5DH, 0C1059ED8H},
                               CARD64{0629A292AH, 0367CD507H},
                               CARD64{09159015AH, 03070DD17H},
                               CARD64{0152FECD8H, 0F70E5939H},
                               CARD64{067332667H, 0FFC00B31H},
                               CARD64{08EB44A87H, 068581511H},
                               CARD64{0DB0C2E0DH, 064F98FA7H},
                               CARD64{047B5481DH, 0BEFA4FA4H} };

    (* Initial seed for SHA-512 takes the first 64 bits of the fractional parts of the square roots of the first 8 prime number. *)

    seed512 = SHA512_DigestType{
                               CARD64{06A09E667H, 0F3BCC908H},
                               CARD64{0BB67AE85H, 084CAA73BH},
                               CARD64{03C6EF372H, 0FE94F82BH},
                               CARD64{0A54FF53AH, 05F1D36F1H},
                               CARD64{0510E527FH, 0ADE682D1H},
                               CARD64{09B05688CH, 02B3E6C1FH},
                               CARD64{01F83D9ABH, 0FB41BD6BH},
                               CARD64{05BE0CD19H, 0137E2179H} };

(********************************************************************************)

VAR
    (* The padding bytes used at the end to round up the data to an integral    *)
    (* number of blocks.  Although this is logically a constant array, it is    *)
    (* easier to do copy operations if it is stored as a variable.              *)

    PADDING: Byte128;

(************************************************************************)
(*                    OPERATIONS ON CARD64 VARIABLES                    *)
(************************************************************************)

    (**********)
    (* SHIFTS *)
    (**********)

PROCEDURE SHR1 (VAR (*INOUT*) x: CARD64);

    (* In-place shift right 1 bits.  Bits that fall off are discarded. *)

    CONST topbit = 80000000H;

    VAR carry: BOOLEAN;

    BEGIN
        carry := ODD(x.high);
        x.high := x.high DIV 2;
        x.low := x.low DIV 2;
        IF carry THEN INC(x.low, topbit) END(*IF*);
    END SHR1;

(************************************************************************)

PROCEDURE SHR (x: CARD64;  n: CARDINAL): CARD64;

    (* Shift right n bits.  Bits that fall off are discarded. *)

    VAR result: CARD64;  j: CARDINAL;

    BEGIN
        result := x;
        FOR j := 1 TO n DO
            SHR1 (result);
        END (*FOR*);
        RETURN result;
    END SHR;

(************************************************************************)

PROCEDURE ROTR1 (VAR (*INOUT*) x: CARD64);

    (* In-place rotate right 1 bits.  Bits that fall off are moved  *)
    (* back in to the top end.                                      *)

    CONST topbit = 80000000H;

    VAR carry0, carry1: BOOLEAN;

    BEGIN
        carry1 := ODD(x.high);
        x.high := x.high DIV 2;
        carry0 := ODD(x.low);
        x.low := x.low DIV 2;
        IF carry1 THEN INC(x.low, topbit) END(*IF*);
        IF carry0 THEN INC(x.high, topbit) END(*IF*);
    END ROTR1;

(************************************************************************)

PROCEDURE ROTR (x: CARD64;  n: CARDINAL): CARD64;

    (* Rotate right n bits.  Bits that fall off the right reappear at   *)
    (* left.  This could be made more efficient, but until we get the   *)
    (* entire algorithm working clarity is more important.              *)

    VAR result: CARD64;  j: CARDINAL;

    BEGIN
        result := x;
        FOR j := 1 TO n DO
            ROTR1 (result);
        END (*FOR*);
        RETURN result;
    END ROTR;

(************************************************************************)

    (*******************)
    (* LOGIC FUNCTIONS *)
    (*******************)

PROCEDURE NOT64 (x: CARD64): CARD64;

    (* Bitwise NOT *)

    VAR result: CARD64;

    BEGIN
        result.low := INOT (x.low);
        result.high := INOT (x.high);
        RETURN result;
    END NOT64;

(************************************************************************)

PROCEDURE AND64 (x, y: CARD64): CARD64;

    (* Bitwise AND *)

    VAR result: CARD64;

    BEGIN
        result.low := IAND (x.low, y.low);
        result.high := IAND (x.high, y.high);
        RETURN result;
    END AND64;

(************************************************************************)

PROCEDURE XOR64 (x, y: CARD64): CARD64;

    (* Bitwise XOR *)

    VAR result: CARD64;

    BEGIN
        result.low := IXOR (x.low, y.low);
        result.high := IXOR (x.high, y.high);
        RETURN result;
    END XOR64;

(************************************************************************)

    (*****************************)
    (* MISC ARITHMETIC FUNCTIONS *)
    (*****************************)

PROCEDURE Ch (x, y, z: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64 (AND64(x,y), AND64(NOT64(x),z));
    END Ch;

(************************************************************************)

PROCEDURE Maj (x, y, z: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64 (XOR64(AND64(x,y), AND64(x,z)), AND64(y,z));
    END Maj;

(************************************************************************)

PROCEDURE SIG0 (x: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64(XOR64(ROTR(x,28),ROTR(x,34)),ROTR(x,39));
    END SIG0;

(************************************************************************)

PROCEDURE SIG1 (x: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64(XOR64(ROTR(x,14),ROTR(x,18)),ROTR(x,41));
    END SIG1;

(************************************************************************)

PROCEDURE sigma0 (x: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64(XOR64(ROTR(x,1),ROTR(x,8)),SHR(x,7));
    END sigma0;

(************************************************************************)

PROCEDURE sigma1 (x: CARD64): CARD64;

    (* Defined in section 4.1.3 of standard. *)

    BEGIN
        RETURN XOR64(XOR64(ROTR(x,19),ROTR(x,61)),SHR(x,6));
    END sigma1;

(************************************************************************)

<* COVERFLOW- *>

PROCEDURE Sum64 (A, B: CARD64): CARD64;

    (* Returns A+B, with overflow discarded.  This is the same as       *)
    (* LONGLONG.Sum64, except that we have disabled overflow detection. *)

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

<* COVERFLOW+ *>

(************************************************************************)
(*                   OPERATIONS ON CARD128 VARIABLES                    *)
(************************************************************************)

PROCEDURE Add128 (VAR (*INOUT*) A: CARD128;  B: CARDINAL);

    (* Computes  A := A + B.  We ignore overflow.  *)

    VAR j: CARDINAL;  carry: BOOLEAN;

    BEGIN
        IF A[0] > MAX(CARD32) - B THEN
            A[0] := A[0] - (MAX(CARD32) - B + 1);
            carry := TRUE;
        ELSE
            INC (A[0], B);
            carry := FALSE;
        END (*IF*);
        FOR j := 1 TO 3 DO
            IF carry THEN
                carry := A[j] = MAX(CARD32);
                IF carry THEN A[j] := 0 ELSE INC(A[j]) END(*IF*);
            END (*IF*);
        END (*FOR*);
    END Add128;

(************************************************************************)

PROCEDURE Mul8 (VAR (*INOUT*) A: CARD128);

    (* Multiplies its argument by 8. *)

    VAR j, toshift, carry: CARDINAL;

    BEGIN
        carry := 0;
        FOR j := 0 TO  3 DO
            toshift := IAND(A[j], 0E0000000H);
            A[j] := LS(A[j] - toshift, 3) + carry;
            carry := RS(toshift, 32-3);
        END (*FOR*);
    END Mul8;

(********************************************************************************)
(*                          PROCESSING ONE BLOCK OF DATA                        *)
(********************************************************************************)

PROCEDURE SHA512ProcessBlock (VAR (*INOUT*) state: SHA512_DigestType;  blockaddr: ADDRESS);

    (* This is the main part of the calculation.  The input is exactly 128 bytes *)
    (* long, and the end result of processing it is to update the state.        *)

    VAR w: Block80;  T1, T2: CARD64;
        a, b, c, d, e, f, g, h: CARD64;
        i: CARDINAL;

    BEGIN
        MOVE (blockaddr, ADR(w), 128);

        (* Observe that w is an array of 80 CARD64 variables. 128 bytes of  *)
        (* input is turned into 16 CARD64 quantities, which fills up the    *)
        (* first 16 elements of w.  The rest of w is filled using the       *)
        (* extension calculation below.                                     *)

        (* Note that the SHA-512 standard implicitly requires big-endian    *)
        (* storage, so we have to reverse the bytes of all the CARD64 data. *)

        FOR i := 0 TO 15 DO
            SwapIt (w[i].high);
            SwapIt (w[i].low);
        END (*FOR*);

        <* COVERFLOW- *>   (* The algorithm relies on ignoring integer overflow *)

        (* Extend the first 16 words into the remaining 64 words w[16..79] of   *)
        (* the message schedule array.                                          *)

        FOR i := 16 TO 79 DO
            w[i] := Sum64( Sum64(sigma1(w[i-2]), w[i-7]), Sum64(sigma0(w[i-15]), w[i-16]));
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

        FOR i := 0 TO 79 DO

            T1 := Sum64( Sum64( Sum64(h, SIG1(e)), Ch(e,f,g)), Sum64 (K[i], w[i]));
            T2 := Sum64( SIG0(a), Maj(a,b,c));

            h := g;
            g := f;
            f := e;
            e := Sum64(d, T1);
            d := c;
            c := b;
            b := a;
            a := Sum64( T1, T2);

        END (*FOR*);

        (*  Add the compressed chunk to the current hash value:  *)

        state[0] := Sum64 (state[0], a);
        state[1] := Sum64 (state[1], b);
        state[2] := Sum64 (state[2], c);
        state[3] := Sum64 (state[3], d);
        state[4] := Sum64 (state[4], e);
        state[5] := Sum64 (state[5], f);
        state[6] := Sum64 (state[6], g);
        state[7] := Sum64 (state[7], h);

        <* COVERFLOW+ *>

    END SHA512ProcessBlock;

(********************************************************************************)
(*                      THE EXTERNALLY CALLABLE PROCEDURES                      *)
(********************************************************************************)

PROCEDURE SHA384Init(): SHA512_CTX;

    (* SHA-224 initialization. Begins a SHA-224 operation, creating *)
    (* a new context.                                               *)

    VAR context: SHA512_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            variant := 12;
            bytecount := Zero128;
            state := seed384;
        END (*WITH*);
        RETURN context;
    END SHA384Init;

(********************************************************************************)

PROCEDURE SHA512Init(): SHA512_CTX;

    (* SHA-512 initialization. Begins the operation, creating a new context. *)

    VAR context: SHA512_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            variant := 16;
            bytecount := Zero128;
            state := seed512;
        END (*WITH*);
        RETURN context;
    END SHA512Init;

(********************************************************************************)

PROCEDURE SHA512Update (context: SHA512_CTX;  data: ARRAY OF LOC;  length: CARDINAL);

    (* SHA-384 and SHA-512 block update operation. Continues a          *)
    (* message-digest operation, adding more data to what has already   *)
    (* been processed for this context.                                 *)

    CONST bufsize = 128;

    VAR inpos, index: CARDINAL;

    BEGIN
        IF length > 0 THEN
            (* Work out where we're up to in the buffer. *)

            index := context^.bytecount[0] MOD bufsize;

            (* Update total number of bytes we've processed so far,     *)
            (* including what we process below.                         *)

            Add128 (context^.bytecount, length);

            (* Transform as many times as possible.  Initially inpos    *)
            (* counts the space left in the buffer, but after that it   *)
            (* it is the position in the incoming data stream.          *)

            inpos := bufsize - index;
            IF length >= inpos THEN

                (* Fill the buffer up to capacity.  (It may already contain *)
                (* contain some data from the last call to SHA512Update.)   *)

                MOVE (ADR(data), ADR(context^.buffer[index]), inpos);
                SHA512ProcessBlock (context^.state, ADR(context^.buffer));

                (* Now process as much of our data as possible, until *)
                (* there's not enough left to make up a full block.   *)

                WHILE inpos + bufsize <= length DO
                    SHA512ProcessBlock (context^.state, ADR(data[inpos]));
                    INC (inpos, bufsize);
                END (*LOOP*);
                index := 0;
            ELSE
                inpos := 0;
            END (*IF*);

            (* We have less than a full block of data left to process. *)
            (* Save it in the buffer for next time.                    *)

            IF length > inpos THEN
                MOVE (ADR(data[inpos]), ADR(context^.buffer[index]), length-inpos);
            END (*IF*);

        END (*IF*);

    END SHA512Update;

(********************************************************************************)

PROCEDURE SHA512Final (VAR (*INOUT*) context: SHA512_CTX;
                         VAR (*OUT*) digest: SHA512_DigestType);

    (* SHA-384 or SHA-512 finalization. Ends the message-digest     *)
    (* operation, returning the digest and discarding the context.  *)

    CONST TooBig = MAX(CARD32) DIV 8 + 1;

    VAR LengthInBits: CARD128;
        index, padLen: CARDINAL;

    BEGIN
        (* Calculate total number of data bits we've worked on. *)

        (* NOTE: we are working with a 128-bit byte count, (a 16-byte number),  *)
        (* but the buffer size calculations in this procedure depend on only    *)
        (* the lowest 7 bits of that number, so the only 128-bit arithmetic     *)
        (* we have to do is to calculate the length in bits.                    *)

        LengthInBits := context^.bytecount;
        Mul8 (LengthInBits);
        SwapIt (LengthInBits);                  (* convert to BigEndian *)

        (* The buffer is 128 bytes (1024 bits) long.  Pad it out to 128-16=112  *)
        (* bytes.  This will leave exactly enough space to fill out the rest of *)
        (* the block with a 128-bit (16 byte) length field.                     *)

        (* The standard specifies everything in bits, which complicates the     *)
        (* arithmetic, but we are lucky to be in an environment where all       *)
        (* character codes are integral numbers of 8-bit bytes.                 *)

        index := context^.bytecount[0] MOD 128;
        IF index < 112 THEN
            padLen := 112 - index;
        ELSE
            (* We will have to start a new block.  Worst case padLen=128.  *)
            padLen := (128+112) - index;
        END (*IF*);
        SHA512Update (context, PADDING, padLen);

        (* Append length (before padding). *)

        SHA512Update (context, LengthInBits, 16);

        (* Final result to be given to caller. *)

        digest := context^.state;

        (* Zeroize sensitive information, then dispose of this context. *)

        FILL (ADR(context^), VAL(CARD8,0), SIZE(SHA512_CTX_RECORD));
        DISPOSE (context);

    END SHA512Final;

(********************************************************************************)
(*                         CONVERTING DIGEST TO STRING                          *)
(********************************************************************************)

PROCEDURE SHA384DigestToString (VAR (*IN*) digest: SHA512_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 96-character string.  If there is not enough    *)
    (* space for 96 characters, produces a leading substring of the full result.*)

    BEGIN
        BEDigest64ToString (12, digest, result);
    END SHA384DigestToString;

(********************************************************************************)

PROCEDURE SHA512DigestToString (VAR (*IN*) digest: SHA512_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 128-character string.  If there is not enough    *)
    (* space for 128 characters, produces a leading substring of the full result.*)

    BEGIN
        BEDigest64ToString (16, digest, result);
    END SHA512DigestToString;

(********************************************************************************)

PROCEDURE SHA512FinalDigest (VAR (*INOUT*) context: SHA512_CTX;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* SHA-224 or SHA-256 finalization. Ends the message-digest operation,      *)
    (* returning the digest and discarding the context.  This is equivalent to  *)
    (* SHA2Final followed by converting the digest to a string, for callers     *)
    (* who don't need to know the actual digest value.                          *)

    VAR digest: SHA512_DigestType;  variant: CARDINAL;

    BEGIN
        variant := context^.variant;
        SHA512Final (context, digest);
        BEDigest64ToString (variant, digest, result);
    END SHA512FinalDigest;

(********************************************************************************)
(*                             INITIAL CONSTANTS                                *)
(********************************************************************************)

BEGIN
    PADDING := Byte128 {80H, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
END SHA512.

