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

<* WOFF316+ *>

IMPLEMENTATION MODULE MD5;

        (********************************************************)
        (*                                                      *)
        (*                  MD5 ENCRYPTION                      *)
        (*                                                      *)
        (*    Implementation of MD5 message-digest algorithm    *)
        (*              as defined in RFC 1321                  *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            14 September 1998               *)
        (*  Last edited:        14 April 2012                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*    Last change: in digest-to-string conversion,      *)
        (*    now use 'a'..'f' instead of 'A'..'F' as hex       *)
        (*    digits.                                           *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(* The algorithm used in this module is as specified in RFC 1321.  The  *)
(* code is derived from the reference implementation given as an        *)
(* appendix to that document.  The code in this module actually departs *)
(* quite a bit from the reference implementation (which is impossible   *)
(* to translate exactly, because it is tied so closely to some          *)
(* unfortunate features of the C language), but mathematically it still *)
(* implements the same algorithm.                                       *)
(*                                                                      *)
(* The reference implementation contains the following copyright notice.*)
(*                                                                      *)
(* MD5C.C - RSA Data Security, Inc., MD5 message-digest algorithm       *)
(*                                                                      *)
(* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All      *)
(* rights reserved.                                                     *)
(*                                                                      *)
(* License to copy and use this software is granted provided that it    *)
(* is identified as the "RSA Data Security, Inc. MD5 Message-Digest     *)
(* Algorithm" in all material mentioning or referencing this software   *)
(* or this function.                                                    *)
(*                                                                      *)
(* License is also granted to make and use derivative works provided    *)
(* that such works are identified as "derived from the RSA Data         *)
(* Security, Inc. MD5 Message-Digest Algorithm" in all material         *)
(* mentioning or referencing the derived work.                          *)
(*                                                                      *)
(* RSA Data Security, Inc. makes no representations concerning either   *)
(* the merchantability of this software or the suitability of this      *)
(* software for any particular purpose. It is provided "as is"          *)
(* without express or implied warranty of any kind.                     *)
(*                                                                      *)
(* These notices must be retained in any copies of any part of this     *)
(* documentation and/or software.                                       *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD32, SET32, ADDRESS, LOC,
    (* proc *)  CAST, ADR, MOVE, FILL, ROTATE;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(********************************************************************************)

TYPE
    Byte64 = ARRAY [0..63] OF CARD8;

    (* MD5 context.  The fields are:                                     *)
    (*     state     the ABCD of the algorithm                           *)
    (*     bytecount number of data bytes we've processed                *)
    (*     buffer    the input data that we haven't yet processed        *)
    (*               because there's not enough to fill a 64-byte block. *)

    MD5_CTX_RECORD = RECORD
                         state: MD5_DigestType;
                         bytecount: CARDINAL;
                         buffer: Byte64;
                     END (*RECORD*);

    MD5_CTX = POINTER TO MD5_CTX_RECORD;

VAR PADDING: Byte64;

(********************************************************************************)
(*                F, G, H and I are basic MD5 functions.                        *)
(********************************************************************************)

PROCEDURE F(x, y, z: CARD32): CARD32;

    BEGIN
        RETURN CAST(CARD32, CAST(SET32,x) * CAST(SET32,y)
                             + CAST(SET32,MAX(CARD32)-x) * CAST(SET32,z));
    END F;

(********************************************************************************)

PROCEDURE G(x, y, z: CARD32): CARD32;

    BEGIN
        RETURN CAST(CARD32, CAST(SET32,x) * CAST(SET32,z)
                             + CAST(SET32,MAX(CARD32)-z) * CAST(SET32,y));
    END G;

(********************************************************************************)

PROCEDURE H(x, y, z: CARD32): CARD32;

    BEGIN
        RETURN CAST(CARD32, CAST(SET32,x) / CAST(SET32,y) / CAST(SET32,z));
    END H;

(********************************************************************************)

PROCEDURE I(x, y, z: CARD32): CARD32;

    BEGIN
        RETURN CAST(CARD32, CAST(SET32,y) /
                              (CAST(SET32,x) + CAST(SET32,MAX(CARD32)-z)));
    END I;

(********************************************************************************)
(*                       THE FOUR MAIN TRANSFORMATIONS                          *)
(********************************************************************************)

<* COVERFLOW- *>           (* The algorithm relies on ignoring integer overflow *)

PROCEDURE FF (VAR (*INOUT*) a: CARD32;  b, c, d, x, s, ac: CARD32);

    BEGIN
        INC (a, F(b, c, d) + x + ac);
        a := CAST(CARD32, ROTATE(CAST(SET32,a), VAL(INTEGER,s)));
        INC (a, b);
    END FF;

(********************************************************************************)

PROCEDURE GG (VAR (*INOUT*) a: CARD32;  b, c, d, x, s, ac: CARD32);

    BEGIN
        INC (a, G(b, c, d) + x + ac);
        a := CAST(CARD32, ROTATE(CAST(SET32,a), VAL(INTEGER,s)));
        INC (a, b);
    END GG;

(********************************************************************************)

PROCEDURE HH (VAR (*INOUT*) a: CARD32;  b, c, d, x, s, ac: CARD32);

    BEGIN
        INC (a, H(b, c, d) + x + ac);
        a := CAST(CARD32, ROTATE(CAST(SET32,a), VAL(INTEGER,s)));
        INC (a, b);
    END HH;

(********************************************************************************)

PROCEDURE II (VAR (*INOUT*) a: CARD32;  b, c, d, x, s, ac: CARD32);

    BEGIN
        INC (a, I(b, c, d) + x + ac);
        a := CAST(CARD32, ROTATE(CAST(SET32,a), VAL(INTEGER,s)));
        INC (a, b);
    END II;

<* COVERFLOW+ *>

(********************************************************************************)
(*                    PROCESSING ONE 64-BYTE BLOCK OF DATA                      *)
(********************************************************************************)

PROCEDURE MD5Transform (VAR (*INOUT*) state: MD5_DigestType;  blockaddr: ADDRESS);

    (* MD5 basic transformation.  Transforms state based on a 64-byte block     *)
    (* of data.                                                                 *)

    CONST
        S11 = 7;  S12 = 12;  S13 = 17;  S14 = 22;
        S21 = 5;  S22 =  9;  S23 = 14;  S24 = 20;
        S31 = 4;  S32 = 11;  S33 = 16;  S34 = 23;
        S41 = 6;  S42 = 10;  S43 = 15;  S44 = 21;

    VAR a, b, c, d: CARD32;
        x: ARRAY [0..15] OF CARD32;

    BEGIN
        a := state[0];  b := state[1];  c := state[2];  d := state[3];
        MOVE (blockaddr, ADR(x), 64);

        (* Round 1 *)

        FF (a, b, c, d, x[ 0], S11, 0D76AA478H); (* 1 *)
        FF (d, a, b, c, x[ 1], S12, 0E8C7B756H); (* 2 *)
        FF (c, d, a, b, x[ 2], S13, 0242070DBH); (* 3 *)
        FF (b, c, d, a, x[ 3], S14, 0C1BDCEEEH); (* 4 *)
        FF (a, b, c, d, x[ 4], S11, 0F57C0FAFH); (* 5 *)
        FF (d, a, b, c, x[ 5], S12, 04787C62AH); (* 6 *)
        FF (c, d, a, b, x[ 6], S13, 0A8304613H); (* 7 *)
        FF (b, c, d, a, x[ 7], S14, 0FD469501H); (* 8 *)
        FF (a, b, c, d, x[ 8], S11, 0698098D8H); (* 9 *)
        FF (d, a, b, c, x[ 9], S12, 08B44F7AFH); (* 10 *)
        FF (c, d, a, b, x[10], S13, 0FFFF5BB1H); (* 11 *)
        FF (b, c, d, a, x[11], S14, 0895CD7BEH); (* 12 *)
        FF (a, b, c, d, x[12], S11, 06B901122H); (* 13 *)
        FF (d, a, b, c, x[13], S12, 0FD987193H); (* 14 *)
        FF (c, d, a, b, x[14], S13, 0A679438EH); (* 15 *)
        FF (b, c, d, a, x[15], S14, 049B40821H); (* 16 *)

        (* Round 2 *)

        GG (a, b, c, d, x[ 1], S21, 0F61E2562H); (* 17 *)
        GG (d, a, b, c, x[ 6], S22, 0C040B340H); (* 18 *)
        GG (c, d, a, b, x[11], S23, 0265E5A51H); (* 19 *)
        GG (b, c, d, a, x[ 0], S24, 0E9B6C7AAH); (* 20 *)
        GG (a, b, c, d, x[ 5], S21, 0D62F105DH); (* 21 *)
        GG (d, a, b, c, x[10], S22,  02441453H); (* 22 *)
        GG (c, d, a, b, x[15], S23, 0D8A1E681H); (* 23 *)
        GG (b, c, d, a, x[ 4], S24, 0E7D3FBC8H); (* 24 *)
        GG (a, b, c, d, x[ 9], S21, 021E1CDE6H); (* 25 *)
        GG (d, a, b, c, x[14], S22, 0C33707D6H); (* 26 *)
        GG (c, d, a, b, x[ 3], S23, 0F4D50D87H); (* 27 *)
        GG (b, c, d, a, x[ 8], S24, 0455A14EDH); (* 28 *)
        GG (a, b, c, d, x[13], S21, 0A9E3E905H); (* 29 *)
        GG (d, a, b, c, x[ 2], S22, 0FCEFA3F8H); (* 30 *)
        GG (c, d, a, b, x[ 7], S23, 0676F02D9H); (* 31 *)
        GG (b, c, d, a, x[12], S24, 08D2A4C8AH); (* 32 *)

        (* Round 3 *)

        HH (a, b, c, d, x[ 5], S31, 0FFFA3942H); (* 33 *)
        HH (d, a, b, c, x[ 8], S32, 08771F681H); (* 34 *)
        HH (c, d, a, b, x[11], S33, 06D9D6122H); (* 35 *)
        HH (b, c, d, a, x[14], S34, 0FDE5380CH); (* 36 *)
        HH (a, b, c, d, x[ 1], S31, 0A4BEEA44H); (* 37 *)
        HH (d, a, b, c, x[ 4], S32, 04BDECFA9H); (* 38 *)
        HH (c, d, a, b, x[ 7], S33, 0F6BB4B60H); (* 39 *)
        HH (b, c, d, a, x[10], S34, 0BEBFBC70H); (* 40 *)
        HH (a, b, c, d, x[13], S31, 0289B7EC6H); (* 41 *)
        HH (d, a, b, c, x[ 0], S32, 0EAA127FAH); (* 42 *)
        HH (c, d, a, b, x[ 3], S33, 0D4EF3085H); (* 43 *)
        HH (b, c, d, a, x[ 6], S34,  04881D05H); (* 44 *)
        HH (a, b, c, d, x[ 9], S31, 0D9D4D039H); (* 45 *)
        HH (d, a, b, c, x[12], S32, 0E6DB99E5H); (* 46 *)
        HH (c, d, a, b, x[15], S33, 01FA27CF8H); (* 47 *)
        HH (b, c, d, a, x[ 2], S34, 0C4AC5665H); (* 48 *)

        (* Round 4 *)

        II (a, b, c, d, x[ 0], S41, 0F4292244H); (* 49 *)
        II (d, a, b, c, x[ 7], S42, 0432AFF97H); (* 50 *)
        II (c, d, a, b, x[14], S43, 0AB9423A7H); (* 51 *)
        II (b, c, d, a, x[ 5], S44, 0FC93A039H); (* 52 *)
        II (a, b, c, d, x[12], S41, 0655B59C3H); (* 53 *)
        II (d, a, b, c, x[ 3], S42, 08F0CCC92H); (* 54 *)
        II (c, d, a, b, x[10], S43, 0FFEFF47DH); (* 55 *)
        II (b, c, d, a, x[ 1], S44, 085845DD1H); (* 56 *)
        II (a, b, c, d, x[ 8], S41, 06FA87E4FH); (* 57 *)
        II (d, a, b, c, x[15], S42, 0FE2CE6E0H); (* 58 *)
        II (c, d, a, b, x[ 6], S43, 0A3014314H); (* 59 *)
        II (b, c, d, a, x[13], S44, 04E0811A1H); (* 60 *)
        II (a, b, c, d, x[ 4], S41, 0F7537E82H); (* 61 *)
        II (d, a, b, c, x[11], S42, 0BD3AF235H); (* 62 *)
        II (c, d, a, b, x[ 2], S43, 02AD7D2BBH); (* 63 *)
        II (b, c, d, a, x[ 9], S44, 0EB86D391H); (* 64 *)

        <* COVERFLOW- *>

        INC (state[0], a);
        INC (state[1], b);
        INC (state[2], c);
        INC (state[3], d);

        <* COVERFLOW+ *>

        (* Overwrite sensitive information. *)

        FILL (ADR(x), VAL(CARD8,0), SIZE(x));

    END MD5Transform;

(********************************************************************************)
(*                      THE EXTERNALLY CALLABLE PROCEDURES                      *)
(********************************************************************************)

PROCEDURE MD5Init(): MD5_CTX;

    (* MD5 initialization. Begins an MD5 operation, creating a new context. *)

    VAR context: MD5_CTX;

    BEGIN
        NEW (context);
        WITH context^ DO
            bytecount := 0;
            (* Load magic initialization constants. *)
            state[0] := 067452301H;
            state[1] := 0EFCDAB89H;
            state[2] := 098BADCFEH;
            state[3] := 010325476H;
        END (*WITH*);
        RETURN context;
    END MD5Init;

(********************************************************************************)

PROCEDURE MD5Update (context: MD5_CTX;  VAR (*IN*) data: ARRAY OF LOC;  length: CARDINAL);

    (* MD5 block update operation. Continues an MD5 message-digest operation,   *)
    (* adding more data to what has already been processed for this context.    *)

    VAR inpos, index: CARDINAL;

    BEGIN
        IF length > 0 THEN
            (* Work out where we're up to in the buffer. *)

            index := context^.bytecount MOD 64;

            (* Update total number of bytes we've processed so far. *)

            INC (context^.bytecount, length);

            (* Transform as many times as possible. *)

            inpos := 64 - index;
            IF length >= inpos THEN

                (* Fill the buffer up to capacity.  (It may already contain *)
                (* some data from the last call to MD5Update.)              *)

                MOVE (ADR(data), ADR(context^.buffer[index]), inpos);
                MD5Transform (context^.state, ADR(context^.buffer));

                (* Now process as much of our data as possible, until there's *)
                (* not enough left to make up a full 64-byte block.           *)

                WHILE inpos + 63 < length DO
                    MD5Transform (context^.state, ADR(data[inpos]));
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

    END MD5Update;

(********************************************************************************)

PROCEDURE MD5Final (VAR (*INOUT*) context: MD5_CTX;
                        VAR (*OUT*) digest: MD5_DigestType);

    (* MD5 finalization. Ends an MD5 message-digest operation, returning    *)
    (* the message digest and discarding the context.                       *)

    CONST TooBig = MAX(CARD32) DIV 8 + 1;

    VAR LengthInBits: ARRAY [0..1] OF CARD32;
        index, padLen: CARDINAL;

    BEGIN
        (* Calculate total number of data bits we've worked on. *)

        index := context^.bytecount;
        LengthInBits[1] := index DIV TooBig;
        DEC (index, TooBig*LengthInBits[1]);
        LengthInBits[0] := 8*index;

        (* Pad out to 56 mod 64. *)

        index := context^.bytecount MOD 64;
        IF index < 56 THEN
            padLen := 56 - index;
        ELSE
            padLen := 120 - index;
        END (*IF*);
        MD5Update (context, PADDING, padLen);

        (* Append length (before padding). *)

        MD5Update (context, LengthInBits, 8);

        (* Final result to be given to caller. *)

        digest := context^.state;

        (* Zeroize sensitive information, then dispose of this context. *)

        FILL (ADR(context^), VAL(CARD8,0), SIZE(MD5_CTX_RECORD));
        DISPOSE (context);

    END MD5Final;

(********************************************************************************)
(*                         CONVERTING DIGEST TO STRING                          *)
(********************************************************************************)

PROCEDURE ConvertHex1 (value: CARD8;  VAR (*INOUT*) string: ARRAY OF CHAR;
                                         VAR (*INOUT*) pos: CARDINAL);

    (* Writes a one-digit hex number to string[pos], then increments pos.  If   *)
    (* there is not enough space, no result is produced.                        *)

    VAR ch: CHAR;

    BEGIN
        IF value < 10 THEN
            ch := CHR(ORD('0') + value);
        ELSE
            ch := CHR(ORD('a') + value - 10);
        END (*IF*);
        IF pos <= HIGH(string) THEN
            string[pos] := ch;  INC(pos);
        END (*IF*);
    END ConvertHex1;

(********************************************************************************)

PROCEDURE ConvertHex2 (value: CARDINAL;  VAR (*INOUT*) string: ARRAY OF CHAR;
                                         VAR (*INOUT*) pos: CARDINAL);

    (* Writes a two-digit hex number to string, starting at string[pos]. *)

    BEGIN
        ConvertHex1 (value DIV 16, string, pos);
        ConvertHex1 (value MOD 16, string, pos);
    END ConvertHex2;

(********************************************************************************)

PROCEDURE ConvertHex8 (value: CARDINAL;  VAR (*INOUT*) string: ARRAY OF CHAR;
                                         VAR (*INOUT*) pos: CARDINAL);

    (* Writes an eight-digit hex number to string, starting at string[pos]. *)

    VAR j: [0..3];

    BEGIN
        FOR j := 0 TO 3 DO
            ConvertHex2 (value MOD 256, string, pos);
            value := value DIV 256;
        END (*FOR*);
    END ConvertHex8;

(********************************************************************************)

PROCEDURE MD5DigestToString (VAR (*IN*) digest: MD5_DigestType;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 32-character string.  If there is not enough    *)
    (* space for 32 characters, produces a leading substring of the full result.*)

    VAR i: [0..3];  pos: CARDINAL;

    BEGIN
        pos := 0;
        FOR i := 0 TO 3 DO
            ConvertHex8 (digest[i], result, pos);
        END (*FOR*);
        IF pos <= HIGH(result) THEN
            result[pos] := CHR(0);
        END (*IF*);
    END MD5DigestToString;

(********************************************************************************)
(*                           DEFINE PADDING STRING                              *)
(********************************************************************************)

BEGIN
    PADDING := Byte64 {80H, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
END MD5.

