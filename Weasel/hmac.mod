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

IMPLEMENTATION MODULE HMAC;

        (********************************************************)
        (*                                                      *)
        (*    HMAC: Keyed hashing for message authentication    *)
        (*                                                      *)
        (*     Implementation of HMAC as defined in RFC2104     *)
        (*                                                      *)
        (*     This module implements the variants HMAC-MD5,    *)
        (*     HMAC-SHA1, HMAC-SHA256, HMAC_SHA384, HMAC_512.   *)
        (*     extended to other variants if desired.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            18 February 2005                *)
        (*  Last edited:        28 January 2018                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*      HMAC-MD5 passes all tests                       *)
        (*      HMAC-SHA1 passes all tests                      *)
        (*      HMAC-SHA256 passes all tests                    *)
        (*      HMAC-SHA384 passes all tests                    *)
        (*      HMAC-SHA512 passes all tests                    *)
        (*                                                      *)
        (********************************************************)


FROM Arith64 IMPORT
    (* type *)  CARD64;

FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

FROM SHA1 IMPORT
    (* type *)  SHA1_CTX, SHA1_DigestType,
    (* proc *)  SHA1Init, SHA1Update, SHA1Final, SHA1DigestToString;

FROM SHA2 IMPORT
    (* type *)  SHA2_CTX, SHA2_DigestType,
    (* proc *)  SHA256Init, SHA2Update, SHA2Final;

FROM SHA512 IMPORT
    (* type *)  SHA512_CTX, SHA512_DigestType,
    (* proc *)  SHA384Init, SHA512Init, SHA512Update, SHA512Final;

FROM LowLevel IMPORT
    (* proc *)  IXORB;

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD32, LOC,
    (* proc *)  ADR, MOVE, FILL;

(************************************************************************)
(*                                                                      *)
(*  REMARK: There is a certain amount of code duplication below.  I did *)
(*  look at the possibility of having a single generic HMAC procedure,  *)
(*  but in the final analysis it seemed better to accept the redundancy *)
(*  for the sake of better run-time efficiency.                         *)
(*                                                                      *)
(************************************************************************)

TYPE
    Byte64 = ARRAY [0..63] OF CARD8;
    Byte128 = ARRAY [0..127] OF CARD8;

(************************************************************************)
(*                              HMAC-MD5                                *)
(************************************************************************)

PROCEDURE HMAC_MD5 (key: ARRAY OF LOC;  keylength: CARDINAL;
                     text: ARRAY OF LOC;  textlength: CARDINAL;
                     VAR (*OUT*) Digest: MD5_DigestType);

    (* Performs the HMAC-MD5 hashing method, with Digest as the result. *)

    VAR context: MD5_CTX;
        k_ipad, k_opad: Byte64;
        j: CARDINAL;

    BEGIN
        IF keylength > 64 THEN

            (* Need initial key transformation if key length > 64. *)

            context := MD5Init();
            MD5Update (context, key, keylength);
            MD5Final (context, Digest);
            MOVE (ADR(Digest), ADR(key), 16);
            keylength := 16;

        END (*IF*);

        (************************************************)
        (* The HMAC_MD5 transform is defined as:        *)
        (*                                              *)
        (* MD5(K XOR opad, MD5(K XOR ipad, text))       *)
        (*                                              *)
        (* where K is an n byte key                     *)
        (* ipad is the byte 0x36 repeated 64 times      *)
        (* opad is the byte 0x5c repeated 64 times      *)
        (* and text is the data being protected         *)
        (************************************************)

        (* Start out by storing key in pads. *)

        FILL (ADR(k_ipad), VAL(CARD8,0), 64);
        MOVE (ADR(key), ADR(k_ipad), keylength);
        k_opad := k_ipad;

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO 63 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner MD5. *)

        context := MD5Init();
        MD5Update (context, k_ipad, 64);
        MD5Update (context, text, textlength);
        MD5Final (context, Digest);

        (* Perform outer MD5. *)

        context := MD5Init();
        MD5Update (context, k_opad, 64);
        MD5Update (context, Digest, 16);
        MD5Final (context, Digest);

    END HMAC_MD5;

(************************************************************************)
(*                         ENDIANNESS ADJUSTMENT                        *)
(*                                                                      *)
(*    We need this because the SHA family of algorithms are machine     *)
(*    dependent and assume big-endian storage of integers.              *)
(************************************************************************)

PROCEDURE Swap4 (VAR (*INOUT*) arg: ARRAY OF LOC);

    (* Reverses the byte order of a 4-byte argument.  *)

    VAR temp: LOC;

    BEGIN
        temp := arg[0];  arg[0] := arg[3];  arg[3] := temp;
        temp := arg[1];  arg[1] := arg[2];  arg[2] := temp;
    END Swap4;

(************************************************************************)

PROCEDURE SwapEndian (VAR (*INOUT*) arg: ARRAY OF CARD32;  N: CARDINAL);

    (* Changes the endianness of an array of N CARD32 numbers. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            Swap4 (arg[j]);
        END (*FOR*);
    END SwapEndian;

(************************************************************************)

PROCEDURE SwapEndian64 (VAR (*INOUT*) arg: ARRAY OF CARD64;  N: CARDINAL);

    (* Changes the endianness of an array of N CARD64 numbers. *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO N-1 DO
            Swap4 (arg[j].low);
            Swap4 (arg[j].high);
        END (*FOR*);
    END SwapEndian64;

(************************************************************************)
(*                              HMAC-SHA1                               *)
(************************************************************************)

PROCEDURE HMAC_SHA1 (key: ARRAY OF LOC;  keylength: CARDINAL;
                     text: ARRAY OF LOC;  textlength: CARDINAL;
                     VAR (*OUT*) Digest: SHA1_DigestType);

    (* Performs the HMAC-SHA1 hashing method, with Digest as the result. *)

    CONST
        BlockSize = 64;
        DigestLength = 20;

    VAR context: SHA1_CTX;
        k_ipad, k_opad: Byte64;
        j: CARDINAL;

    BEGIN
        IF keylength > BlockSize THEN

            (* Need initial key transformation if key length > 64. *)

            context := SHA1Init();
            SHA1Update (context, key, keylength);
            SHA1Final (context, Digest);
            SwapEndian (Digest, DigestLength DIV 4);
            MOVE (ADR(Digest), ADR(key), DigestLength);
            keylength := DigestLength;

        END (*IF*);

        (****************************************************)
        (* The HMAC_SHA1 transform is defined as:           *)
        (*                                                  *)
        (* SHA1(K XOR opad, SHA1(K XOR ipad, text))         *)
        (*                                                  *)
        (* where K is an n byte key                         *)
        (* ipad is the byte 0x36 repeated BlockSize times   *)
        (* opad is the byte 0x5c repeated BlockSize times   *)
        (* and text is the data being protected             *)
        (****************************************************)

        (* Start out by storing key in pads. *)

        FILL (ADR(k_ipad), VAL(CARD8,0), BlockSize);
        MOVE (ADR(key), ADR(k_ipad), keylength);
        k_opad := k_ipad;

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO BlockSize-1 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner SHA1. *)

        context := SHA1Init();
        SHA1Update (context, k_ipad, BlockSize);
        SHA1Update (context, text, textlength);
        SHA1Final (context, Digest);

        (* Convert Digest from BigEndian.   *)

        SwapEndian (Digest, DigestLength DIV 4);

        (* Perform outer SHA1. *)

        context := SHA1Init();
        SHA1Update (context, k_opad, BlockSize);
        SHA1Update (context, Digest, DigestLength);
        SHA1Final (context, Digest);

    END HMAC_SHA1;

(************************************************************************)
(*                             HMAC-SHA256                              *)
(************************************************************************)

PROCEDURE HMAC_SHA256 (key: ARRAY OF LOC;  keylength: CARDINAL;
                     text: ARRAY OF LOC;  textlength: CARDINAL;
                     VAR (*OUT*) Digest: SHA2_DigestType);

    (* Performs the HMAC-SHA256 hashing method, with Digest as the result. *)

    CONST
        BlockSize = 64;
        DigestLength = 32;

    VAR context: SHA2_CTX;
        k_ipad, k_opad: Byte64;
        j: CARDINAL;

    BEGIN
        IF keylength > BlockSize THEN

            (* Need initial key transformation if key length > BlockSize. *)

            context := SHA256Init();
            SHA2Update (context, key, keylength);
            SHA2Final (context, Digest);
            SwapEndian (Digest, DigestLength DIV 4);
            MOVE (ADR(Digest), ADR(key), DigestLength);
            keylength := DigestLength;

        END (*IF*);

        (************************************************)
        (* The HMAC_SHA256 transform is defined as:     *)
        (*                                              *)
        (* SHA256(K XOR opad, SHA256(K XOR ipad, text)) *)
        (*                                              *)
        (* where K is an n byte key                     *)
        (* ipad is the byte 0x36 repeated 64 times      *)
        (* opad is the byte 0x5c repeated 64 times      *)
        (* and text is the data being protected         *)
        (************************************************)

        (* Start out by storing key in pads. *)

        FILL (ADR(k_ipad), VAL(CARD8,0), BlockSize);
        MOVE (ADR(key), ADR(k_ipad), keylength);
        k_opad := k_ipad;

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO BlockSize-1 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner SHA256. *)

        context := SHA256Init();
        SHA2Update (context, k_ipad, BlockSize);
        SHA2Update (context, text, textlength);
        SHA2Final (context, Digest);

        (* Convert Digest from BigEndian.   *)

        SwapEndian (Digest, DigestLength DIV 4);

        (* Perform outer SHA256. *)

        context := SHA256Init();
        SHA2Update (context, k_opad, BlockSize);
        SHA2Update (context, Digest, DigestLength);
        SHA2Final (context, Digest);

    END HMAC_SHA256;

(************************************************************************)

PROCEDURE HMAC_SHA384 (key: ARRAY OF LOC;  keylength: CARDINAL;
                     text: ARRAY OF LOC;  textlength: CARDINAL;
                     VAR (*OUT*) Digest: SHA512_DigestType);

    (* Performs the HMAC-SHA384 hashing method, with Digest as the result. *)

    CONST
        BlockSize = 128;
        DigestLength = 48;

    VAR context: SHA512_CTX;
        k_ipad, k_opad: Byte128;
        j: CARDINAL;

    BEGIN
        IF keylength > BlockSize THEN

            (* Need initial key transformation if key length > BlockSize. *)

            context := SHA384Init();
            SHA512Update (context, key, keylength);
            SHA512Final (context, Digest);
            SwapEndian64 (Digest, DigestLength DIV 8);
            MOVE (ADR(Digest), ADR(key), DigestLength);
            keylength := DigestLength;

        END (*IF*);

        (************************************************)
        (* The HMAC_SHA384 transform is defined as:     *)
        (*                                              *)
        (* SHA384(K XOR opad, SHA384(K XOR ipad, text)) *)
        (*                                              *)
        (* where K is an n byte key                     *)
        (* ipad is the byte 0x36 repeated 128 times     *)
        (* opad is the byte 0x5c repeated 128 times     *)
        (* and text is the data being protected         *)
        (************************************************)

        (* Start out by storing key in pads. *)

        FILL (ADR(k_ipad), VAL(CARD8,0), BlockSize);
        MOVE (ADR(key), ADR(k_ipad), keylength);
        k_opad := k_ipad;

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO BlockSize-1 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner SHA384. *)

        context := SHA384Init();
        SHA512Update (context, k_ipad, BlockSize);
        SHA512Update (context, text, textlength);
        SHA512Final (context, Digest);

        (* Convert Digest from BigEndian.   *)

        SwapEndian64 (Digest, DigestLength DIV 8);

        (* Perform outer SHA256. *)

        context := SHA384Init();
        SHA512Update (context, k_opad, BlockSize);
        SHA512Update (context, Digest, DigestLength);
        SHA512Final (context, Digest);

    END HMAC_SHA384;

(************************************************************************)

PROCEDURE HMAC_SHA512 (key: ARRAY OF LOC;  keylength: CARDINAL;
                     text: ARRAY OF LOC;  textlength: CARDINAL;
                     VAR (*OUT*) Digest: SHA512_DigestType);

    (* Performs the HMAC-SHA512 hashing method, with Digest as the result. *)

    CONST
        BlockSize = 128;
        DigestLength = 64;

    VAR context: SHA512_CTX;
        k_ipad, k_opad: Byte128;
        j: CARDINAL;

    BEGIN
        IF keylength > BlockSize THEN

            (* Need initial key transformation if key length > BlockSize. *)

            context := SHA512Init();
            SHA512Update (context, key, keylength);
            SHA512Final (context, Digest);
            SwapEndian64 (Digest, DigestLength DIV 8);
            MOVE (ADR(Digest), ADR(key), DigestLength);
            keylength := DigestLength;

        END (*IF*);

        (************************************************)
        (* The HMAC_SHA512 transform is defined as:     *)
        (*                                              *)
        (* SHA512(K XOR opad, SHA512(K XOR ipad, text)) *)
        (*                                              *)
        (* where K is an n byte key                     *)
        (* ipad is the byte 0x36 repeated 128 times     *)
        (* opad is the byte 0x5c repeated 128 times     *)
        (* and text is the data being protected         *)
        (************************************************)

        (* Start out by storing key in pads. *)

        FILL (ADR(k_ipad), VAL(CARD8,0), BlockSize);
        MOVE (ADR(key), ADR(k_ipad), keylength);
        k_opad := k_ipad;

        (* XOR key with ipad and opad values. *)

        FOR j := 0 TO BlockSize-1 DO
            k_ipad[j] := IXORB (k_ipad[j], 036H);
            k_opad[j] := IXORB (k_opad[j], 05CH);
        END (*FOR*);

        (* Perform inner SHA512. *)

        context := SHA512Init();
        SHA512Update (context, k_ipad, BlockSize);
        SHA512Update (context, text, textlength);
        SHA512Final (context, Digest);

        (* Convert Digest from BigEndian.   *)

        SwapEndian64 (Digest, DigestLength DIV 8);

        (* Perform outer SHA512. *)

        context := SHA512Init();
        SHA512Update (context, k_opad, BlockSize);
        SHA512Update (context, Digest, DigestLength);
        SHA512Final (context, Digest);

    END HMAC_SHA512;

(************************************************************************)

END HMAC.

