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

IMPLEMENTATION MODULE RandCard;

        (********************************************************)
        (*                                                      *)
        (*              Random number generator                 *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Last edited:        24 March 2010                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

(************************************************************************)
(*                                                                      *)
(* The algorithm used is Schrage's method, as described in              *)
(*      Stephen K. Park and Keith W. Miller, "Random Number Generators: *)
(*      Good ones are hard to find", CACM 31(10), Oct 1988, 1192-1201.  *)
(* A basic property of this particular implementation is that all       *)
(* intermediate results fit into 32 bits (including sign).              *)
(*                                                                      *)
(************************************************************************)

CONST a = 16807;                  (* 7^5 *)

(************************************************************************)

PROCEDURE RandCardinal (): CARDINAL;

    (* Returns a random number in the range [1..modulus-1],     *)
    (* with a uniform distribution over that range.             *)

    CONST r = modulus MOD a;    (* 2836 *)
          q = modulus DIV a;    (* 127773 *)

    VAR high, low: CARDINAL;  test: INTEGER;

    BEGIN
        high := seed DIV q;  low := seed MOD q;
        test := VAL(INTEGER,a*low) - VAL(INTEGER,r*high);
        IF test > 0 THEN seed := test
        ELSE seed := test + modulus;
        END (*IF*);
        RETURN seed;
    END RandCardinal;

(****************************************************************)

PROCEDURE RandInt (min, max: INTEGER): INTEGER;

    (* Returns a random number in the range [min..max]          *)
    (* (inclusive), with an approximately uniform distribution  *)
    (* over that range.  (In this version I'm not being fussy   *)
    (* about the precise distribution.)                         *)

    BEGIN
        IF min >= max THEN
            RETURN min;
        ELSE
            RETURN VAL(INTEGER,RandCardinal()) MOD (max-min+1) + min;
        END (*IF*);
    END RandInt;

(****************************************************************)

PROCEDURE Randomize;

    (* Changes the random number seed to a new value.           *)

    VAR time: DateTime;

    BEGIN
        GetClock (time);
        WITH time DO
            seed := 60*(24*hour + minute) + second;
        END (*WITH*);
    END Randomize;

(****************************************************************)
(*                      MODULE INITIALISATION                   *)
(****************************************************************)

BEGIN
    seed := 1;
END RandCard.

