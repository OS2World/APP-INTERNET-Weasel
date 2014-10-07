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

IMPLEMENTATION MODULE Base64;

        (********************************************************)
        (*                                                      *)
        (*         Encoding and decoding Base64 strings         *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            31 January 2003                 *)
        (*  Last edited:        15 April 2012                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, CARD32,
    (* proc *)  CAST;

(********************************************************************************)

CONST
    Nul = CHR(0);

TYPE EncoderTable = ARRAY [0..63] OF CHAR;

CONST Code = EncoderTable {'A','B','C','D','E','F','G','H',
                           'I','J','K','L','M','N','O','P',
                           'Q','R','S','T','U','V','W','X',
                           'Y','Z','a','b','c','d','e','f',
                           'g','h','i','j','k','l','m','n',
                           'o','p','q','r','s','t','u','v',
                           'w','x','y','z','0','1','2','3',
                           '4','5','6','7','8','9','+','/'};

TYPE
    SixBit = CARD8[0..63];

    FourByte = RECORD
                   CASE :BOOLEAN OF
                       FALSE:    w: CARD32;
                     | TRUE:     b: ARRAY [0..3] OF LOC;
                   END (*CASE*);
               END (*RECORD*);

    (************************************************************************)
    (* Warning re the FourByte type: we are running on a machine that uses  *)
    (* big-endian representation of a CARD32, so the bytes must             *)
    (* be processed from right to left.                                     *)
    (************************************************************************)

(********************************************************************************)
(*                                ENCODING                                      *)
(********************************************************************************)

PROCEDURE Encode (VAR (*IN*) data: ARRAY OF LOC;  N: CARDINAL;
                           VAR (*OUT*) result: ARRAY OF CHAR);

    (* Translates N bytes of data to Base64. *)

    VAR k: CARDINAL;

    (****************************************************************************)

    PROCEDURE EncodeGroup (val: CARDINAL);

        (* Converts a 24-bit number to a four-character code at result[k]. *)

        VAR i: CARDINAL;

        BEGIN
            FOR i := 3 TO 0 BY -1 DO
                result[k+i] := Code[val MOD 64];
                val := val DIV 64;
            END (*FOR*);
            INC (k, 4);
        END EncodeGroup;

    (****************************************************************************)

    VAR i, j: CARDINAL;
        v1: CARD8;
        InGroup: FourByte;

    BEGIN
        (* Overflow check: reduce N if there is not enough space in     *)
        (* the result array.                                            *)

        j := (HIGH(result) + 1) DIV 4;
        IF (N + 2) DIV 3 > j THEN
            N := 3*j;
        END (*IF*);

        (* Main part of the calculation. *)

        j := 0;  k := 0;
        WHILE N >= 3 DO
            InGroup.w := 0;
            FOR i := 2 TO 0 BY -1 DO
                InGroup.b[i] := data[j];
                INC(j);  DEC(N);
            END (*FOR*);
            EncodeGroup (InGroup.w);
        END (*WHILE*);

        (* There could be one or two bytes left.  Use '=' padding. *)

        IF N > 0 THEN
            result[k] := Code[CAST(CARD8,data[j]) DIV 4];  INC(k);
            v1 := 16*(CAST(CARD8,data[j]) MOD 4);
            IF N = 1 THEN
                result[k] := Code[v1];  INC(k);
                result[k] := '=';  INC(k);
            ELSE
                result[k] := Code[v1+(CAST(CARD8,data[j+1]) DIV 16)];  INC(k);
                result[k] := Code[4*(CAST(CARD8,data[j+1]) MOD 16)];  INC(k);
            END (*IF*);
            result[k] := '=';  INC(k);
        END (*IF*);

        IF k <= HIGH(result) THEN
            result[k] := Nul;
        END (*IF*);

    END Encode;

(********************************************************************************)
(*                                DECODING                                      *)
(********************************************************************************)

PROCEDURE DecodeChar (ch: CHAR): CARD8;

    (* Converts one Base64 character to a six-bit value. *)
    (* Returns MAX(CARD8) for an illegal character.      *)

    BEGIN
        IF ch = '+' THEN
            RETURN 62;
        ELSIF ch = '/' THEN
            RETURN 63;
        ELSIF ch < '0' THEN
            RETURN MAX(CARD8);
        ELSIF ch <= '9' THEN
            RETURN ORD(ch) - ORD('0') + 52;
        ELSIF ch < 'A' THEN
            RETURN MAX(CARD8);
        ELSIF ch <= 'Z' THEN
            RETURN ORD(ch) - ORD('A');
        ELSIF ch < 'a' THEN
            RETURN MAX(CARD8);
        ELSIF ch <= 'z' THEN
            RETURN ORD(ch) - ORD('a') + 26;
        ELSE
            RETURN MAX(CARD8);
        END (*IF*);
    END DecodeChar;

(********************************************************************************)

PROCEDURE Decode (VAR (*IN*) data: ARRAY OF CHAR;
                      VAR (*OUT*) result: ARRAY OF LOC): CARDINAL;

    (* Translates Base64 to a string of bytes.  The function result is the      *)
    (* number of bytes in the decoded result.                                   *)

    VAR i, j, N: CARDINAL;   val: CARD8;
        current: FourByte;

    BEGIN
        j := 0;  N := 0;

        REPEAT
            current.w := 0;  i := 4;

            (* Pick up 4 characters in big-endian notation. *)

            WHILE (i > 0) AND (j <= HIGH(data))
                        AND (data[j] <> '=') AND (data[j] <> Nul) DO
                val := DecodeChar(data[j]);  INC(j);
                IF val <> MAX(CARD8) THEN
                    current.w := 64*current.w + val;
                    DEC (i);
                END (*IF*);
            END (*WHILE*);

            (* If i > 0, we have come to the end of the data stream. *)

            IF i < 4 THEN
                IF i > 0 THEN
                    j := i;
                    REPEAT
                        current.w := 64*current.w;  DEC (j);
                    UNTIL j = 0;
                END (*IF*);
                result[N] := current.b[2];  INC(N);
                IF i < 3 THEN
                    result[N] := current.b[1];  INC(N);
                    IF i < 2 THEN
                        result[N] := current.b[0];  INC(N);
                    END (*IF*);
                END (*IF*);
            END (*IF*);

        UNTIL i > 0;

        IF N <= HIGH(result) THEN
            result[N] := CAST(LOC,Nul);
        END (*IF*);

        RETURN N;

    END Decode;

(********************************************************************************)

END Base64.

