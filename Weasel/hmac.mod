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

IMPLEMENTATION MODULE HMAC;

        (********************************************************)
        (*                                                      *)
        (*    HMAC: Keyed hashing for message authentication    *)
        (*                                                      *)
        (*     Implementation of HMAC as defined in RFC2104     *)
        (*                                                      *)
        (*     This module implements the variants HMAC-MD5     *)
        (*     and HMAC-SHA1.  It could be extended to other    *)
        (*                 variants if desired.                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            18 February 2005                *)
        (*  Last edited:        19 February 2005                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM MD5 IMPORT
    (* type *)  MD5_CTX, MD5_DigestType,
    (* proc *)  MD5Init, MD5Update, MD5Final, MD5DigestToString;

FROM SHA1 IMPORT
    (* type *)  SHA1_CTX, SHA1_DigestType,
    (* proc *)  SHA1Init, SHA1Update, SHA1Final, SHA1DigestToString;

FROM LowLevel IMPORT
    (* proc *)  IXORB;

FROM SYSTEM IMPORT
    (* type *)  CARD8, LOC,
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

(************************************************************************)
(*                              HMAC-MD5                                *)
(************************************************************************)

PROCEDURE HMAC_MD5 (text: ARRAY OF LOC;  textlength: CARDINAL;
                     key: ARRAY OF LOC;  keylength: CARDINAL;
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
(*                              HMAC-SHA1                               *)
(************************************************************************)

PROCEDURE HMAC_SHA1 (text: ARRAY OF LOC;  textlength: CARDINAL;
                     key: ARRAY OF LOC;  keylength: CARDINAL;
                     VAR (*OUT*) Digest: SHA1_DigestType);

    (* Performs the HMAC-SHA1 hashing method, with Digest as the result. *)

    VAR context: SHA1_CTX;
        k_ipad, k_opad: Byte64;
        j: CARDINAL;

    BEGIN
        IF keylength > 64 THEN

            (* Need initial key transformation if key length > 64. *)

            context := SHA1Init();
            SHA1Update (context, key, keylength);
            SHA1Final (context, Digest);
            MOVE (ADR(Digest), ADR(key), 16);
            keylength := 16;

        END (*IF*);

        (************************************************)
        (* The HMAC_SHA1 transform is defined as:       *)
        (*                                              *)
        (* SHA1(K XOR opad, SHA1(K XOR ipad, text))     *)
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

        (* Perform inner SHA1. *)

        context := SHA1Init();
        SHA1Update (context, k_ipad, 64);
        SHA1Update (context, text, textlength);
        SHA1Final (context, Digest);

        (* Perform outer SHA1. *)

        context := SHA1Init();
        SHA1Update (context, k_opad, 64);
        SHA1Update (context, Digest, 16);
        SHA1Final (context, Digest);

    END HMAC_SHA1;

(************************************************************************)

END HMAC.

