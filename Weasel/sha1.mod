(**************************************************************************)
(*                                                                        *)
(*  Encryption library                                                    *)
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

IMPLEMENTATION MODULE SHA1;

        (********************************************************)
        (*                                                      *)
        (*             SECURE HASH ALGORITHM SHA-1              *)
        (*                                                      *)
        (*     Implementation of SHA-1 secure hash standard     *)
        (*             as defined in FIPS PUB 180-1             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 February 2005                *)
        (*  Last edited:        28 January 2018                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* The algorithm used in this module is as specified in FIPS            *)
(* publication 180-1, "Secure Hash Standard", 1995 April 17.  The code  *)
(* follows the computational algorithm explained in that algorithm,     *)
(* and is tested using the test data from the same document.            *)
(*                                                                      *)
(************************************************************************)


FROM STextIO IMPORT                       (* debugging only *)
    (* proc *)  WriteChar, WriteString, WriteLn;

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD32, ADDRESS, LOC,
    (* proc *)  ADR, MOVE, FILL;

FROM Arith64 IMPORT
    (* const*)  Zero64,
    (* type *)  CARD64,
    (* proc *)  Add64;

FROM BEDigests IMPORT
    (* proc *)  BEDigestToString;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR, INOT, IXOR, LS, RS, ROL, ROR;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

TYPE
    Byte64 = ARRAY [0..63] OF CARD8;

    (* SHA-1 context.  The fields are:                                   *)
    (*     H         the H0..H4 of the algorithm                         *)
    (*     bytecount number of data bytes we've processed                *)
    (*     buffer    the input data that we haven't yet processed        *)
    (*               because there's not enough to fill a 64-byte block. *)

    SHA1_CTX_RECORD = RECORD
                          H: SHA1_DigestType;
                          bytecount: CARD64;
                          buffer: Byte64;
                      END (*RECORD*);

    SHA1_CTX = POINTER TO SHA1_CTX_RECORD;

VAR PADDING: Byte64;

CONST
    seed = SHA1_DigestType {067452301H, 0EFCDAB89H, 098BADCFEH,
                                          010325476H, 0C3D2E1F0H};
    K0 = 05A827999H;  K20 = 06ED9EBA1H;  K40 = 08F1BBCDCH;  K60 = 0CA62C1D6H;

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

PROCEDURE SwapIt (VAR (*INOUT*) arg: ARRAY OF LOC);

    (* Reverses the byte order of a 4-byte argument.  We need this because the  *)
    (* SHA-1 standard specifies big-endian storage of integers.                 *)

    VAR temp: LOC;

    BEGIN
        temp := arg[0];  arg[0] := arg[3];  arg[3] := temp;
        temp := arg[1];  arg[1] := arg[2];  arg[2] := temp;
    END SwapIt;

(********************************************************************************)
(*                    PROCESSING ONE 64-BYTE BLOCK OF DATA                      *)
(********************************************************************************)

PROCEDURE SHA1ProcessBlock (VAR (*INOUT*) state: SHA1_DigestType;  blockaddr: ADDRESS);

    (* Encrypts one 16-word block (64 bytes) of input data. *)

    VAR a, b, c, d, e, temp: CARD32;
        t: [0..79];
        W: ARRAY [0..79] OF CARD32;

    BEGIN
        MOVE (blockaddr, ADR(W), 64);

        (* Note that the SHA-1 standard implicitly requires big-endian storage, *)
        (* so we have to reverse the bytes of all the CARD32 data.              *)

        FOR t := 0 TO 15 DO
            SwapIt (W[t]);
        END (*FOR*);

        FOR t := 16 TO 79 DO
            W[t] := ROL (IXOR (IXOR (IXOR (W[t-3] ,W[t-8]) ,W[t-14]) ,W[t-16]), 1);
        END (*FOR*);

        a := state[0];  b := state[1];  c := state[2];  d := state[3];  e := state[4];

        <* COVERFLOW- *>           (* The algorithm relies on ignoring integer overflow *)

        FOR t := 0 TO 19 DO
            temp := ROL(a,5) + IOR(IAND(b,c),IAND(INOT(b),d)) + e + W[t] + K0;
            e := d;  d := c;  c := ROR(b,2);  b := a;  a := temp;
        END (*FOR*);
        FOR t := 20 TO 39 DO
            temp := ROL(a,5) + IXOR(IXOR(b,c),d) + e + W[t] + K20;
            e := d;  d := c;  c := ROR(b,2);  b := a;  a := temp;
        END (*FOR*);
        FOR t := 40 TO 59 DO
            temp := ROL(a,5) + IOR(IOR(IAND(b,c),IAND(b,d)),IAND(c,d)) + e + W[t] + K40;
            e := d;  d := c;  c := ROR(b,2);  b := a;  a := temp;
        END (*FOR*);
        FOR t := 60 TO 79 DO
            temp := ROL(a,5) + IXOR(IXOR(b,c),d) + e + W[t] + K60;
            e := d;  d := c;  c := ROR(b,2);  b := a;  a := temp;
        END (*FOR*);

        INC (state[0], a);
        INC (state[1], b);
        INC (state[2], c);
        INC (state[3], d);
        INC (state[4], e);

        <* COVERFLOW+ *>

    END SHA1ProcessBlock;

(********************************************************************************)
(*                      THE EXTERNALLY CALLABLE PROCEDURES                      *)
(********************************************************************************)

PROCEDURE SHA1Init(): SHA1_CTX;

    (* SHA-1 initialization. Begins a SHA-1 operation, creating a new context. *)

    VAR context: SHA1_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            bytecount := Zero64;

            (* Load magic initialization constants. *)

            H := seed;

        END (*WITH*);
        RETURN context;
    END SHA1Init;

(********************************************************************************)

PROCEDURE SHA1Update (context: SHA1_CTX;  data: ARRAY OF LOC;  length: CARDINAL);

    (* SHA1 block update operation. Continues a SHA-1 message-digest operation, *)
    (* adding more data to what has already been processed for this context.    *)

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
                (* some data from the last call to SHA1Update.)             *)

                MOVE (ADR(data), ADR(context^.buffer[index]), inpos);
                SHA1ProcessBlock (context^.H, ADR(context^.buffer));

                (* Now process as much of our data as possible, until there's *)
                (* not enough left to make up a full 64-byte block.           *)

                WHILE inpos + 63 < length DO
                    SHA1ProcessBlock (context^.H, ADR(data[inpos]));
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

    END SHA1Update;

(********************************************************************************)

PROCEDURE SHA1Final (VAR (*INOUT*) context: SHA1_CTX;
                     VAR (*OUT*) digest: SHA1_DigestType);

    (* SHA-1 finalization. Ends a SHA-1 message-digest operation, returning    *)
    (* the message digest and discarding the context.                          *)

    CONST TooBig = MAX(CARD32) DIV 8 + 1;

    VAR LengthInBits: CARD64;
        index, padLen: CARDINAL;

    BEGIN
        (* Calculate total number of data bits we've worked on. *)

        LengthInBits := Mul8 (context^.bytecount);

        (* Pad out to 56 mod 64. *)

        index := context^.bytecount.low MOD 64;
        IF index < 56 THEN
            padLen := 56 - index;
        ELSE
            padLen := 120 - index;
        END (*IF*);
        SHA1Update (context, PADDING, padLen);

        (* Append length (before padding). *)

        SwapIt (LengthInBits.high);
        SHA1Update (context, LengthInBits.high, 4);
        SwapIt (LengthInBits.low);
        SHA1Update (context, LengthInBits.low, 4);

        (* Final result to be given to caller. *)

        digest := context^.H;

        (* Zeroize sensitive information, then dispose of this context. *)

        FILL (ADR(context^), VAL(CARD8,0), SIZE(SHA1_CTX_RECORD));
        DISPOSE (context);

    END SHA1Final;

(********************************************************************************)
(*                         CONVERTING DIGEST TO STRING                          *)
(********************************************************************************)

PROCEDURE SHA1DigestToString (VAR (*IN*) digest: SHA1_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 40-character string.  If there is not enough    *)
    (* space for 40 characters, produces a leading substring of the full result.*)

    BEGIN
        BEDigestToString (5, digest, result);
    END SHA1DigestToString;

(********************************************************************************)
(*                             INITIAL CONSTANTS                                *)
(********************************************************************************)

BEGIN
    PADDING := Byte64 {80H, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
END SHA1.

