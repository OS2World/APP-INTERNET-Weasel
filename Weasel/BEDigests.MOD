(**************************************************************************)
(*                                                                        *)
(*  Part of encryption software                                           *)
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

IMPLEMENTATION MODULE BEDigests;

        (********************************************************)
        (*                                                      *)
        (*  Converting a message digest to a hexidecimal string *)
        (*  for the case where big-endian notation is needed,   *)
        (*  e.g. the SHA class of algorithms.  This module      *)
        (*  deals with the fact that we're executing a          *)
        (*  big-endian algorithm on little-endian hardware.     *)
        (*                                                      *)
        (*  Common code for a variety of encryption procedures. *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            29 September 2017               *)
        (*  Last edited:        28 January 2018                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)



FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD32;

FROM Arith64 IMPORT
    (* type *)  CARD64;

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

    (* Converts an eight-digit hex number to string, starting at string[pos]. *)

    VAR j: [0..3];   byte: ARRAY [0..3] OF CARD8;

    BEGIN
        FOR j := 3 TO 0 BY -1 DO
            byte[j] := value MOD 256;
            value := value DIV 256;
        END (*FOR*);
        FOR j := 0 TO 3 DO
            ConvertHex2 (byte[j], string, pos);
        END (*FOR*);
    END ConvertHex8;

(********************************************************************************)

PROCEDURE BEDigestToString (N: CARDINAL;  VAR (*IN*) digest: ARRAY OF CARD32;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 8*N-character string.  If there is not enough   *)
    (* space for the result, produces a leading substring of the full result.   *)

    VAR i: [0..7];  pos: CARDINAL;

    BEGIN
        pos := 0;
        FOR i := 0 TO N-1 DO
            ConvertHex8 (digest[i], result, pos);
        END (*FOR*);
        IF pos <= HIGH(result) THEN
            result[pos] := CHR(0);
        END (*IF*);
    END BEDigestToString;

(********************************************************************************)

PROCEDURE BEDigest64ToString (N: CARDINAL;  VAR (*IN*) digest: ARRAY OF CARD64;
                             VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts the digest to a 8*N-character string.  If there is not enough   *)
    (* space for the result, produces a leading substring of the full result.   *)

    (* NOTE: Endianness is relevant only when converting each CARD64 number.    *)
    (* We work through the array from element 0 up.                             *)

    VAR i: [0..7];  pos: CARDINAL;

    BEGIN
        N := N DIV 2;
        pos := 0;
        FOR i := 0 TO N-1 DO
            ConvertHex8 (digest[i].high, result, pos);
            ConvertHex8 (digest[i].low, result, pos);

            (* While testing, insert space between sections. *)

            (*result[pos] := ' ';  INC(pos);*)

        END (*FOR*);
        IF pos <= HIGH(result) THEN
            result[pos] := CHR(0);
        END (*IF*);
    END BEDigest64ToString;

(********************************************************************************)

END BEDigests.

