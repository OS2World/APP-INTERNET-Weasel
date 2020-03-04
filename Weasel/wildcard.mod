(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE WildCard;

        (********************************************************)
        (*                                                      *)
        (*            String matching with wildcards            *)
        (*                                                      *)
        (*   Yet another approach to scanning from both ends    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            16 November 2019                *)
        (*  Last edited:        23 December 2019                *)
        (*  Status:             Seems to be working             *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)

PROCEDURE CleanTemplate (VAR (*IN*) template: ARRAY OF CHAR;
                         j2: CARDINAL;  VAR (*INOUT*) k2: CARDINAL);

    (* Reduces sequences of '*' characters to a single one. *)

    VAR js, jd: CARDINAL;

    BEGIN
        jd := j2;
        IF k2 > j2 THEN
            FOR js := j2 TO k2-1 DO
                IF (template[js] <> '*') OR (template[js+1] <> '*') THEN
                    template[jd] := template[js];
                    INC (jd);
                END (*IF*);
            END (*FOR*);
            IF jd <> k2 THEN
                template[jd] := template[k2];
                template[jd+1] := Nul;
                k2 := jd;
            END (*IF*);
        END (*IF*);
    END CleanTemplate;

(************************************************************************)

PROCEDURE Match (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL): BOOLEAN;
                                                                 FORWARD;

    (* Returns TRUE if input[j1..k1] matches template[j2..k2].          *)

(************************************************************************)

PROCEDURE SubMatch (VAR (*IN*) input: ARRAY OF CHAR;
                                j1, k1: CARDINAL;
                                   VAR (*IN*) template: ARRAY OF CHAR;
                                     j2, k2: CARDINAL): BOOLEAN;  FORWARD;

    (* Succeeds iff a substring of input matches template, i.e. if      *)
    (* input[j..k] matches template[j2..k2], where j1 <= j <= k <= k1.  *)

(************************************************************************)

PROCEDURE HeadMatch (VAR (*IN*) input: ARRAY OF CHAR;
                                j1, k1: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                j2, k2: CARDINAL): BOOLEAN;

    (* Executive overview: succeeds if a LEADING substring of input    *)
    (* matches template.                                                *)

    (* The caller guarantees that the template is nonempty and that     *)
    (* template[j2] <> '*'.                                             *)

    (* Returns TRUE if input[j1..k] matches template[j2..k2], where     *)
    (* j1 <= k <= k1.                                                   *)

    BEGIN
        LOOP
            (* Step through the template until we hit a '*' or we can   *)
            (* return with a definite success or failure.               *)

            IF j2 > k2 THEN

                (* Template all used up, so we have matched a leading substring. *)

                RETURN TRUE;

            ELSIF template[j2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Some template left but no input left. *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?') AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            (* If we have passed all of the above, we have matched a character. *)

            INC (j1);  INC (j2);

        END (*LOOP*);

        (* If we reach this point, then j2 <= k2 and template[j2] = '*'.  *)

        IF j2 = k2 THEN
            RETURN TRUE;
        ELSE
            (* j2 < k2 and template[j2+1] <> '*'.  *)

            RETURN SubMatch (input, j1, k1, template, j2+1, k2);

        END (*IF*);

    END HeadMatch;

(************************************************************************)

PROCEDURE SubMatch (VAR (*IN*) input: ARRAY OF CHAR;
                                j1, k1: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                j2, k2: CARDINAL): BOOLEAN;

    (* Succeeds iff a substring of input matches template, i.e. if      *)
    (* input[j..k] matches template[j2..k2], where j1 <= j <= k <= k1.  *)

    VAR j: CARDINAL;

    BEGIN
        IF j2 > k2 THEN

            (* Empty template matches any empty substring of input. *)

            RETURN TRUE;

        ELSIF j1 > k1 THEN
            RETURN FALSE;
        END (*IF*);

        (* We now have a nonempty template. *)

        IF template[j2] = '*' THEN
            RETURN SubMatch (input, j1, k1, template, j2+1, k2);
        END (*IF*);

        (* Now template[j2] <> '*'.  *)

        j := j1;
        WHILE j <= k1 DO
            IF (template[j2] = '?') OR (CAP(input[j]) = CAP(template[j2]))
                    AND HeadMatch (input, j+1, k1, template, j2+1, k2) THEN
                RETURN TRUE;
            ELSE
                INC (j);
            END (*IF*);

        END (*WHILE*);

        (* If we get here, we have exhausted the input without  *)
        (* exhausting the template, and the template does not   *)
        (* start with '*'.                                      *)

        RETURN FALSE;

    END SubMatch;

(************************************************************************)

PROCEDURE TailMatch (VAR (*IN*) input: ARRAY OF CHAR;
                                j1, k1: CARDINAL;
                                    VAR (*IN*) template: ARRAY OF CHAR;
                                                j2, k2: CARDINAL): BOOLEAN;

    (* Executive overview: succeeds if a TRAILING substring of input    *)
    (* matches template.                                                *)

    (* The caller guarantees that the template is nonempty and that     *)
    (* template[j2] <> '*'.                                             *)

    (* Returns TRUE if input[j..k1] matches template[j2..k2], where     *)
    (* j1 <= j <= k1.                                                   *)

    BEGIN
        LOOP
            (* Step through the template backwards until we hit a '*'   *)
            (* or we can return with a definite success or failure.     *)

            IF j2 > k2 THEN
                RETURN TRUE;
            ELSIF template[k2] = '*' THEN
                EXIT (*LOOP*);
            ELSIF j1 > k1 THEN
                RETURN FALSE;

            ELSIF (template[k2] <> '?') AND (CAP(input[k1]) <> CAP(template[k2])) THEN

                RETURN FALSE;

            (* Watch out for the cases that would drive k1 or k2        *)
            (* negative.  Having to work out what would happen the next *)
            (* time around the loop adds some extra checks, but I can't *)
            (* think of a better way to handle this.                    *)

            ELSIF k1 = 0 THEN

                 (* Empty input; the only thing that can match is an *)
                 (* empty or all-star template.                      *)

                IF j2+1 = k2 THEN RETURN template[j2] = '*'
                ELSE RETURN j2+1 > k2
                END (*IF*);

            ELSIF k2 = 0 THEN

                (* Template exhausted, nonempty input. *)

                RETURN TRUE;

            END (*IF*);

            (* if we reach here we have matched one trailing character. *)

            DEC (k1);  DEC(k2);

        END (*LOOP*);

        (* If we reach this point, then template[k2] = '*'.  *)

        IF j2 = k2 THEN
            RETURN TRUE;
        ELSE
            (* j2 < k2 AND template[k2-1] <> '*'. *)

            RETURN SubMatch (input, j1, k1, template, j2, k2-1);
        END (*IF*);

    END TailMatch;

(************************************************************************)

PROCEDURE Match (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL): BOOLEAN;

    (* Returns TRUE if input[j1..k1] matches template[j2..k2].  *)

    BEGIN
        IF j1 > k1 THEN

            (* Empty input; the only thing that can match is an *)
            (* empty or all-star template.                      *)

            IF j2 = k2 THEN RETURN template[j2] = '*'
            ELSE RETURN j2 > k2
            END (*IF*);

        ELSIF j2 > k2 THEN

            (* Empty template, non-empty input. *)

            RETURN FALSE;

        END (*IF*);

        (* Having disposed of the "empty" cases, we're now comparing    *)
        (* input[j1..k1] with template[j2..k2].                         *)

        LOOP
            (* From the left, input[j1] and template[j2] are the        *)
            (* first characters we haven't yet tested for a match.      *)

            IF j2 > k2 THEN

                (* No more template left; match iff we've also  *)
                (* exhausted the input.                         *)

                RETURN j1 > k1;

            ELSIF template[j2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not '*', so we have a definite mismatch.    *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?') AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j1);  INC (j2);

        END (*LOOP*);

        (* If we reach here, template[j2] = '*'.  *)

        INC (j2);

        IF j2 > k2 THEN

            (* No more template left, so effective template = '*'.  *)

            RETURN TRUE;

        ELSE
            (* Now j2 <= k2 and template[j2] <> '*' *)

            RETURN TailMatch (input, j1, k1, template, j2, k2);

        END (*IF*);

    END Match;

(************************************************************************)

PROCEDURE SetBounds (VAR (*IN*) input: ARRAY OF CHAR;
                          VAR (*OUT*) j1, k1: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          VAR (*OUT*) j2, k2: CARDINAL);

    (* Sets the subscript limits for input and template, also removes   *)
    (* redundant wildcards in template.  This means that the other      *)
    (* procedures in this module may assume that an '*' is never        *)
    (* followed by another '*'.                                         *)

    BEGIN
        j1 := 0;  k1 := LENGTH (input);
        j2 := 0;  k2 := LENGTH (template);

        (* Deal with the "empty" cases in such a way that will not  *)
        (* let the k values go negative.                            *)

        IF k1 = 0 THEN
            j1 := 1;
        ELSE
            DEC (k1);
        END (*IF*);

        IF k2 = 0 THEN
            j2 := 1;
        ELSE
            DEC (k2);
        END (*IF*);

        CleanTemplate (template, j2, k2);

    END SetBounds;

(************************************************************************)
(*                  THE EXTERNALLY CALLABLE PROCEDURES                  *)
(************************************************************************)

PROCEDURE WildMatch (VAR (*IN*) input, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if template and input are equal, with the extra     *)
    (* rules:                                                           *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)

    VAR j1, k1, j2, k2: CARDINAL;

    BEGIN
        SetBounds (input, j1, k1, template, j2, k2);
        RETURN Match (input, j1, k1, template, j2, k2);

    END WildMatch;

(************************************************************************)

PROCEDURE SubstringMatch (VAR (*IN*) input, template: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE if template matches a substring of input, with the  *)
    (* extra rules:                                                     *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)

    VAR j1, k1, j2, k2: CARDINAL;

    BEGIN
        SetBounds (input, j1, k1, template, j2, k2);
        RETURN SubMatch (input, j1, k1, template, j2, k2);

    END SubstringMatch;

(************************************************************************)

END WildCard.

