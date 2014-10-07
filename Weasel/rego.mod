(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE Rego;

        (********************************************************)
        (*                                                      *)
        (*        Checking a software registration code         *)
        (*        This is the version that works locally        *)
        (*  and does not actually require a registration code   *)
        (*                                                      *)
        (*  That is, this is the dummy version that always      *)
        (*  returns Registered = TRUE.                          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 June 2002                    *)
        (*  Last edited:        28 August 2014                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)

PROCEDURE IsRegistered(): BOOLEAN;

    (* Returns TRUE iff this is a registered copy of the software. *)

    BEGIN
        RETURN TRUE;
    END IsRegistered;

(************************************************************************)

PROCEDURE GenerateCode (string1, string2, string3: ARRAY OF CHAR): CARDINAL;

    (* Generates a registration code.  This can be used to create new   *)
    (* codes, but it is also used internally by this module in          *)
    (* checking existing registration codes.                            *)

    BEGIN
        (* This is the dummy version. *)

        RETURN 0;

    END GenerateCode;

(************************************************************************)

PROCEDURE SetRegistrationParameters (INIFile: ARRAY OF CHAR;
                                     TNImode: BOOLEAN;
                                     NameKey, NumberKey: ARRAY OF CHAR;
                                     code1, code2: ARRAY OF CHAR);

    (* The caller specifies the parameters used in generating a         *)
    (* registration code, and this module makes an internal record of   *)
    (* whether the registration criterion has been satisfied.           *)

    BEGIN
    END SetRegistrationParameters;

(************************************************************************)

PROCEDURE SetExpiryDate (year, month, day: CARDINAL);

    (* This procedure replaces SetRegistrationParameters for the case   *)
    (* where we want to create a time-expired demo.                     *)

    BEGIN
    END SetExpiryDate;

(************************************************************************)

END Rego.

