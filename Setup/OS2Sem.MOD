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

IMPLEMENTATION MODULE OS2Sem;

        (****************************************************************)
        (*                                                              *)
        (*           WAIT ON MUTEX SEMAPHORE OR EVENT SEMAPHORE         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    17 June 2019                            *)
        (*      Status:         OK                                      *)
        (*                                                              *)
        (****************************************************************)


<* M2EXTENSIONS+ *>

IMPORT OS2, SYSTEM;

FROM SplitScreen IMPORT
    (* proc *)  LockScreen, UnlockScreen,
                WriteChar, WriteString, WriteLn;

(************************************************************************)

TYPE CardPtr = POINTER TO CARDINAL;
CONST NilPtr = SYSTEM.CAST(CardPtr, 0);

(************************************************************************)
(*                      TERMINATION ON FATAL ERROR                      *)
(*                                                                      *)
(*  Note that the message output will be useless for a detached         *)
(*  program, but then I expect to use this message output only when     *)
(*  debugging.                                                          *)
(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes unsigned integer to standard output. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0')+N));
    END WriteCard;

(************************************************************************)

PROCEDURE SemError (kind: SemKind;  errornum: CARDINAL);

    (* Puts an error message to standard output, then crashes. *)

    BEGIN
        LockScreen;
        CASE kind OF
            mutex: WriteString ("Mutex");
          |
            event: WriteString ("Event");
        ELSE
                   WriteString ("Unknown");
        END (*CASE*);
        WriteString (" semaphore error ");
        WriteCard (errornum);
        WriteLn;
        UnlockScreen;
        NilPtr^ := 0;
    END SemError;

(************************************************************************)
(*              WAIT ON MUTEX SEMAPHORE OR EVENT SEMAPHORE              *)
(************************************************************************)

PROCEDURE TimedWaitOnSemaphore (kind: SemKind;  sem, TimeLimit: CARDINAL): BOOLEAN;

    (* Waits on an event semaphore or a mutex semaphore.  Returns TRUE  *)
    (* iff the operation timed out.                                     *)

    VAR status, statusr, postcount, t1, t2, diff: CARDINAL;
        done, TimedOut: BOOLEAN;

    BEGIN
        t1 := 0;  t2 := 0;
        done := FALSE;  TimedOut := FALSE;

        IF TimeLimit <> OS2.SEM_INDEFINITE_WAIT THEN

            (* Remember the starting time, in case we need it. *)

            OS2.DosQuerySysInfo (OS2.QSV_MS_COUNT, OS2.QSV_MS_COUNT,
                                               SYSTEM.ADR(t1), SIZE(t1));
        END (*IF*);

        REPEAT
            IF kind = mutex THEN
                status := OS2.DosRequestMutexSem (sem, TimeLimit);
            ELSE
                (* I use event semaphores in such a way that the best   *)
                (* time to reset them is just after returning from a    *)
                (* wait.  This is not necessarily the best strategy for *)
                (* all uses of event semaphores, but it makes sense in  *)
                (* situations where we know that at any time there will *)
                (* be at most one thread waiting on the semaphore.      *)

                status := OS2.DosWaitEventSem (sem, TimeLimit);

                (* There will be a task switch here, unless the event   *)
                (* semaphore has already been posted.                   *)

                statusr := OS2.DosResetEventSem (sem, postcount);
                IF (statusr <> OS2.NO_ERROR) AND (statusr <> OS2.ERROR_ALREADY_RESET) THEN
                    (* Fatal error - SemError will not return. *)
                    SemError (event, status);
                END (*IF*);

            END (*IF*);

            IF status = OS2.ERROR_TIMEOUT THEN
                TimedOut := TRUE;
            ELSIF status = OS2.NO_ERROR THEN
                done := TRUE;
            ELSIF status = OS2.ERROR_INTERRUPT THEN

                IF TimeLimit <> OS2.SEM_INDEFINITE_WAIT THEN

                    (* This is the tricky case.  We are apparently      *)
                    (* partway through the wait, so we have to adjust   *)
                    (* the time limit and wait again.  No adjustment is *)
                    (* needed in the "indefinite wait" case, of course. *)

                    OS2.DosQuerySysInfo(OS2.QSV_MS_COUNT, OS2.QSV_MS_COUNT,
                                                SYSTEM.ADR(t2), SIZE(t2));

                    (* Calculate t2 - t1, allowing for possible wrap.   *)

                    IF t2 >= t1 THEN
                        diff := t2 - t1;
                    ELSE
                        diff := MAX(CARDINAL) - t1 + t2 + 1;
                    END (*IF*);
                    t1 := t2;
                    IF diff >= TimeLimit THEN
                        TimedOut := TRUE;
                    ELSIF diff <> 0 THEN
                        DEC (TimeLimit, diff);
                    END (*IF*);
                END (*IF*);

            ELSE
                (* Fatal error - SemError will not return. *)
                SemError (kind, status);
            END (*IF*);

        UNTIL TimedOut OR done;

        RETURN TimedOut;

    END TimedWaitOnSemaphore;

(************************************************************************)

PROCEDURE WaitOnSemaphore (kind: SemKind;  sem: CARDINAL);

    (* Waits indefinitely on an event semaphore or a mutex semaphore. *)

    VAR dummy: BOOLEAN;

    BEGIN
        dummy := TimedWaitOnSemaphore (kind, sem, OS2.SEM_INDEFINITE_WAIT);
    END WaitOnSemaphore;

(************************************************************************)

END OS2Sem.

