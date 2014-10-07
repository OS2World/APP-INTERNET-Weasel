(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

IMPLEMENTATION MODULE Extra;

        (********************************************************)
        (*                                                      *)
        (*      Weasel procedures that need to live at a        *)
        (*             lower level than RelayMail               *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            25 April 2001                   *)
        (*  Last edited:        19 March 2009                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM Names IMPORT
    (* type *)  UserName, HostName;

(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)

PROCEDURE FullAddress (user: UserName;  domain: HostName;
                                      VAR (*OUT*) result: ARRAY OF CHAR);

    (* Given user and domain, produces the string <user@domain>. *)

    BEGIN
        Strings.Assign ('<', result);
        Strings.Append (user, result);
        IF domain[0] <> Nul THEN
            Strings.Append ("@", result);
            Strings.Append (domain, result);
        END (*IF*);
        Strings.Append ('>', result);
    END FullAddress;

(************************************************************************)
(*                      PARSING A PATH STRING                           *)
(************************************************************************)

PROCEDURE UserAndDomain (source: ARRAY OF CHAR;
                            VAR (*OUT*) user: UserName;
                            VAR (*OUT*) domain: HostName);

    (* Extracts user and domain from a path string. *)

    TYPE CharSet = SET OF CHAR;

    VAR srcpos: CARDINAL;  Stoppers: CharSet;

    (********************************************************************)

    PROCEDURE CopyString (VAR (*OUT*) dest: ARRAY OF CHAR);

        (* Copies up to (but not including) a character in Stoppers. *)

        VAR dstpos: CARDINAL;

        BEGIN
            dstpos := 0;
            WHILE (srcpos <= HIGH(source))
                           AND NOT (source[srcpos] IN Stoppers) DO
                IF dstpos <= HIGH(dest) THEN
                    dest[dstpos] := source[srcpos];
                    INC (dstpos);
                END (*IF*);
                INC (srcpos);
            END (*WHILE*);
            IF dstpos <= HIGH(dest) THEN
                dest[dstpos] := Nul;
            END (*IF*);
        END CopyString;

    (********************************************************************)

    BEGIN
        srcpos := 0;
        WHILE (srcpos <= HIGH(source)) AND (source[srcpos] = ' ') DO
            INC (srcpos);
        END (*WHILE*);
        IF (srcpos <= HIGH(source)) AND (source[srcpos] = '"') THEN
            INC (srcpos);
            REPEAT
                INC (srcpos);
            UNTIL (srcpos > HIGH(source)) OR (source[srcpos-1] = '"');
        END (*IF*);
        WHILE (srcpos <= HIGH(source)) AND (source[srcpos] = ' ') DO
            INC (srcpos);
        END (*WHILE*);
        IF (srcpos <= HIGH(source)) AND (source[srcpos] = '<') THEN
            INC (srcpos);
        END (*IF*);
        Stoppers := CharSet {Nul, ' ', '@', '>'};
        CopyString (user);
        IF (srcpos <= HIGH(source)) AND (source[srcpos] = '@') THEN
            INC (srcpos);
            EXCL (Stoppers, '@');
            CopyString (domain);
        ELSE
            domain := "";
        END (*IF*);
    END UserAndDomain;

(************************************************************************)

END Extra.

