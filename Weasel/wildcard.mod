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

IMPLEMENTATION MODULE WildCard;

        (********************************************************)
        (*                                                      *)
        (*            String matching with wildcards            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 June 1999                     *)
        (*  Last edited:        28 March 2003                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*   NOTE: There is a certain amount of code            *)
        (*   duplication in this module.  This is because       *)
        (*   WildMatchS adds a rule that can not be implemented *)
        (*   as efficiently as the WildMatch rules, and I did   *)
        (*   not want to slow down the WildMatch case with code *)
        (*   that is not going to be used very often.           *)
        (*                                                      *)
        (********************************************************)


CONST NoStoppers = CharSet{};

(************************************************************************)
(*                         SUBSTRING MATCHING                           *)
(*                                                                      *)
(*  These two procedures will make more sense once you've read the      *)
(*  specification of WildMatchS.                                        *)
(*                                                                      *)
(************************************************************************)

PROCEDURE SubstringMatch (VAR (*IN*) input: ARRAY OF CHAR;
                           j1, k1, minsize: CARDINAL;
                           VAR (*IN*) template: ARRAY OF CHAR;
                           j2, k2: CARDINAL;
                           CheckPercent, leftpercent: BOOLEAN;
                           Stoppers: CharSet): BOOLEAN;           FORWARD;

    (* Executive overview: succeeds if a substring of input matches     *)
    (* template.  (This glosses over some technicalities, but           *)
    (* executives aren't interested in precision.)                      *)

(************************************************************************)

PROCEDURE HeadMatch (VAR (*IN*) input: ARRAY OF CHAR;
                          j1, k1, minsize: CARDINAL;
                          VAR (*IN*) template: ARRAY OF CHAR;
                          j2, k2: CARDINAL;  CheckPercent: BOOLEAN;
                                             Stoppers: CharSet): BOOLEAN;

    (* Executive overview: succeeds if a LEADING substring of input     *)
    (* matches template.                                                *)

    (* Returns TRUE if input[j1..k] matches template[j2..k2], where     *)
    (* j1 <= k <= k1, and if in addition the match would consume        *)
    (* at least minsize characters of input.  If the template is empty  *)
    (* (j2 > k2), we have a match iff minsize = 0.  The Stoppers        *)
    (* parameter is used iff CheckPercent is TRUE and the template      *)
    (* contains a '%'.                                                  *)

    VAR leftpercent: BOOLEAN;

    BEGIN
        leftpercent := FALSE;
        LOOP
            (* From the left, input[j1] and template[j2] are the        *)
            (* first characters we haven't yet tested for a match.      *)

            IF j2 > k2 THEN

                (* No more template left; match by definition iff       *)
                (* minsize is zero.                                     *)

                RETURN minsize = 0;

            ELSIF template[j2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF CheckPercent AND (template[j2] = '%') THEN

                leftpercent := TRUE;
                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not wild, so we have a definite mismatch.   *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?')
                    AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j1);  INC (j2);
            IF minsize > 0 THEN
                DEC (minsize);
            END (*IF*);

        END (*LOOP*);

        (* If we reach here, template[j2] = '*' or template[j2] = '%'. *)

        IF CheckPercent THEN
            WHILE (j2 <= k2) AND ((template[j2] = '*') OR (template[j2] = '%')) DO
                leftpercent := leftpercent AND (template[j2] = '%');
                INC (j2);
            END (*WHILE*);
        ELSE
            REPEAT
                INC (j2);
            UNTIL (j2 > k2) OR (template[j2] <> '*');
        END (*IF*);

        RETURN SubstringMatch (input, j1, k1, minsize, template,
                             j2, k2, CheckPercent, leftpercent, Stoppers);

    END HeadMatch;

(************************************************************************)

PROCEDURE SubstringMatch (VAR (*IN*) input: ARRAY OF CHAR;
                           j1, k1, minsize: CARDINAL;
                           VAR (*IN*) template: ARRAY OF CHAR;
                           j2, k2: CARDINAL;
                           CheckPercent, leftpercent: BOOLEAN;
                           Stoppers: CharSet                   ): BOOLEAN;

    (* Returns TRUE if any contiguous substring of input[j1..k1]        *)
    (* matches template[j2..k2], subject to the conditions:             *)
    (*   - if leftpercent is TRUE, the skipped part at the left of      *)
    (*     input may not contain characters in Stoppers.                *)
    (*   - the matching operation must consume at least minsize         *)
    (*     characters of input.                                         *)
    (* On entry we are guaranteed that j1 <= k1.                        *)

    VAR j: CARDINAL;

    BEGIN
        j := j1;
        LOOP
            IF HeadMatch (input, j, k1, minsize, template, j2, k2,
                                            CheckPercent, Stoppers) THEN
                RETURN TRUE;
            ELSIF j >= k1 THEN
                RETURN FALSE;
            ELSIF leftpercent AND (input[j] IN Stoppers) THEN
                RETURN FALSE;
            END (*IF*);
            INC(j);
            IF minsize > 0 THEN
                DEC (minsize);
            END (*IF*);
        END (*LOOP*);
    END SubstringMatch;

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
        j1 := 0;  k1 := LENGTH (input);
        j2 := 0;  k2 := LENGTH (template);

        IF k1 = 0 THEN

            (* Empty input; the only thing that can match is an *)
            (* empty or all-star template.                      *)

            LOOP
                IF j2 = k2 THEN RETURN TRUE
                ELSIF template[j2] = '*' THEN INC(j2)
                ELSE RETURN FALSE
                END (*IF*);
            END (*LOOP*);

        ELSIF k2 = 0 THEN

            (* Empty template, non-empty input. *)

            RETURN FALSE;

        END (*IF*);

        DEC (k1);  DEC(k2);

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

        (* If we reach here, template[j2] = '*'. *)

        LOOP
            (* From the right, input[k1] and template[k2] are the first *)
            (* characters we haven't yet checked for a match.           *)

            IF template[k2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF k1 < j1 THEN

                (* Input exhausted, last unmatched template char *)
                (* is not '*', so we have a definite mismatch.   *)

                RETURN FALSE;

            ELSIF (template[k2] <> '?')
                      AND (CAP(input[k1]) <> CAP(template[k2])) THEN

                RETURN FALSE;

            END (*IF*);

            (* Special case: if k1=0 then we have to record that we've  *)
            (* exhausted the input without decrementing k1.  The same   *)
            (* problem doesn't arise for k2 because at this point we    *)
            (* know that we'll hit a '*' before exhausting the template.*)

            IF k1 = 0 THEN INC(j1) ELSE DEC (k1) END(*IF*);
            DEC (k2);

        END (*LOOP*);

        (* If we reach here, k2 >= j2, template[j2] = '*', and          *)
        (* template[k2] = '*'.  If we have several '*'s in a row, here  *)
        (* is where we reduce them down.                                *)

        REPEAT
            INC (j2);
        UNTIL (j2 > k2) OR (template[j2] <> '*');
        WHILE (j2 <= k2) AND (template[k2] = '*') DO
            DEC (k2);
        END (*WHILE*);

        RETURN SubstringMatch (input, j1, k1, 0, template, j2, k2,
                                             FALSE, FALSE, NoStoppers);

    END WildMatch;

(************************************************************************)

PROCEDURE WildMatchS (VAR (*IN*) input, template: ARRAY OF CHAR;
                                              Stoppers: CharSet): BOOLEAN;

    (* Returns TRUE if template and input are equal, with the extra     *)
    (* rules:                                                           *)
    (*   1. Character case is not significant.                          *)
    (*   2. A '?' in template matches any single character.             *)
    (*   3. A '*' in template matches any string of zero or more        *)
    (*      characters.                                                 *)
    (*   4. A '%' in template matches any string of zero or more        *)
    (*      characters, provided they are not in Stoppers.              *)

    VAR j1, k1, j2, k2, minsize, wstart: CARDINAL;
        CheckPercent, leftpercent, rightpercent: BOOLEAN;

    BEGIN
        CheckPercent := Stoppers <> NoStoppers;
        leftpercent := FALSE;
        rightpercent := FALSE;
        j1 := 0;  k1 := LENGTH (input);
        j2 := 0;  k2 := LENGTH (template);

        IF k1 = 0 THEN

            (* Empty input; the only thing that can match is an *)
            (* empty or all-wild template.                      *)

            LOOP
                IF j2 = k2 THEN RETURN TRUE
                ELSIF template[j2] = '*' THEN INC(j2)
                ELSIF template[j2] = '%' THEN INC(j2)
                ELSE RETURN FALSE
                END (*IF*);
            END (*LOOP*);

        ELSIF k2 = 0 THEN

            (* Empty template, non-empty input. *)

            RETURN FALSE;

        END (*IF*);

        DEC (k1);  DEC(k2);

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

            ELSIF CheckPercent AND (template[j2] = '%') THEN

                leftpercent := TRUE;
                EXIT (*LOOP*);

            ELSIF j1 > k1 THEN

                (* Input exhausted, first unmatched template char *)
                (* is not wild, so we have a definite mismatch.   *)

                RETURN FALSE;

            ELSIF (template[j2] <> '?') AND (CAP(input[j1]) <> CAP(template[j2])) THEN

                RETURN FALSE;

            END (*IF*);

            INC (j1);  INC (j2);

        END (*LOOP*);

        (* If we reach here, template[j2] = '*' or '%'. *)

        LOOP
            (* From the right, input[k1] and template[k2] are the first *)
            (* characters we haven't yet checked for a match.           *)

            IF template[k2] = '*' THEN

                EXIT (*LOOP*);

            ELSIF CheckPercent AND (template[j2] = '%') THEN

                rightpercent := TRUE;
                EXIT (*LOOP*);

            ELSIF k1 < j1 THEN

                (* Input exhausted, last unmatched template char *)
                (* is not wild, so we have a definite mismatch.  *)

                RETURN FALSE;

            ELSIF (template[k2] <> '?')
                      AND (CAP(input[k1]) <> CAP(template[k2])) THEN

                RETURN FALSE;

            END (*IF*);

            (* Special case: if k1=0 then we have to record that we've  *)
            (* exhausted the input without decrementing k1.  The same   *)
            (* problem doesn't arise for k2 because at this point we    *)
            (* know that template[j2] is wild and that we haven't yet   *)
            (* met a wildcard character while working from the right;   *)
            (* so we must hit the wildcard before exhausting the template.*)

            IF k1 = 0 THEN INC(j1) ELSE DEC (k1) END(*IF*);
            DEC (k2);

        END (*LOOP*);

        (* If we reach here, k2 >= j2, and both template[j2] and        *)
        (* template[k2] are wild.  (With one possibility being that     *)
        (* j2 = k2, i.e. we have a single wildcard rather than one at   *)
        (* each end.)  If we have several wildcard characters in a row, *)
        (* here is where we reduce them down, using the rules:          *)
        (*        **  =>  *                *%  =>  *                    *)
        (*        %*  =>  *                %%  =>  %                    *)
        (* Note, by the way, that we don't call '?' a wildcard for our  *)
        (* present purposes, because it is checked separately.          *)

        IF CheckPercent THEN
            WHILE (j2 <= k2) AND ((template[j2] = '*') OR (template[j2] = '%')) DO
                leftpercent := leftpercent AND (template[j2] = '%');
                INC (j2);
            END (*WHILE*);
            WHILE (j2 <= k2) AND ((template[k2] = '*') OR (template[k2] = '%')) DO
                rightpercent := rightpercent AND (template[k2] = '%');
                DEC (k2);
            END (*WHILE*);
        ELSE
            REPEAT
                INC (j2);
            UNTIL (j2 > k2) OR (template[j2] <> '*');
            WHILE (j2 <= k2) AND (template[k2] = '*') DO
                DEC (k2);
            END (*WHILE*);
        END (*IF*);

        (* At this point, we have a match provided a substring of input *)
        (* matches template, with the constraint that the parts of that *)
        (* input before and after the substring should not contain a    *)
        (* character in Stoppers if leftpercent/rightpercent, as        *)
        (* appropriate, is TRUE.                                        *)

        (* If our right wildcard is '%', find a wstart such that        *)
        (* input[wstart..k1] does not contain a character in Stoppers.  *)

        IF rightpercent THEN
            wstart := k1 + 1;
            minsize := wstart - j1;
            WHILE (minsize > 0) AND NOT (input[wstart-1] IN Stoppers) DO
                DEC (minsize);
                DEC (wstart);
            END (*WHILE*);
        ELSE
            minsize := 0;
        END (*IF*);

        RETURN SubstringMatch (input, j1, k1, minsize, template, j2, k2,
                                             TRUE, leftpercent, Stoppers);

    END WildMatchS;

(************************************************************************)

END WildCard.

