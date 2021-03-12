(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE Semaphores;

        (********************************************************)
        (*                                                      *)
        (*      Implements the Wait and Signal operations on    *)
        (*      semaphores.                                     *)
        (*                                                      *)
        (*      Programmer:     P. Moylan                       *)
        (*      Last edited:    22 June 2020                    *)
        (*      Status:         OK                              *)
        (*                                                      *)
        (*      Observation: the kernel overheads in semaphore  *)
        (*      and Lock operations are somewhat higher than    *)
        (*      in the original PMOS, largely due to the        *)
        (*      repeated calculation of the current task ID.    *)
        (*      I should look at ways to improve this.          *)
        (*                                                      *)
        (********************************************************)


FROM STextIO IMPORT WriteString, WriteLn;

FROM SYSTEM IMPORT
    (* proc *)  CAST;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM TaskControl IMPORT
    (* type *)  Lock, TaskID,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release,
                CurrentTaskID, SuspendMe, ResumeTask, Crash;

FROM OS2 IMPORT
    (* const *) SEM_INDEFINITE_WAIT;

(************************************************************************)

CONST MarkerVal = 1;

TYPE
    BlockedListPointer = POINTER TO
                             RECORD
                                 ThreadID: TaskID;
                                 next: BlockedListPointer;
                             END;

    Semaphore = POINTER TO
                    RECORD
                        value: INTEGER;
                        marker: CARDINAL;
                        access: Lock;
                        BlockedList: RECORD
                                         head, tail: BlockedListPointer;
                                     END (*RECORD*);
                    END (*RECORD*);

(************************************************************************)

PROCEDURE CreateSemaphore (VAR (*OUT*) s: Semaphore;
                                        InitialValue: CARDINAL);

    (* Creates semaphore s, with the given initial value and an empty   *)
    (* queue.                                                           *)

    BEGIN
        NEW(s);
        WITH s^ DO
            CreateLock (access);
            value := InitialValue;
            marker := MarkerVal;
            WITH BlockedList DO
                head := NIL;  tail := NIL;
            END (*WITH*);
        END (*WITH*);
    END CreateSemaphore;

(************************************************************************)

PROCEDURE DestroySemaphore (VAR (*INOUT*) s: Semaphore);

    (* Reclaims any space used by semaphore s.  Remark:  It is not at   *)
    (* all obvious what should be done with any tasks which happen to   *)
    (* be blocked on this semaphore (should they be unblocked, or       *)
    (* killed?).  At present we take the easy way out and assume that   *)
    (* there are no pending operations on s at the time that it is      *)
    (* destroyed.                                                       *)

    BEGIN
        IF s <> NIL THEN
            WITH s^ DO
                DestroyLock (access);
            END (*WITH*);
            DISPOSE (s);
        END (*IF*);
    END DestroySemaphore;

(************************************************************************)

PROCEDURE IsNilSemaphore (s: Semaphore): BOOLEAN;

    (* Returns TRUE iff s has been destroyed. *)

    BEGIN
        RETURN s = NIL;
    END IsNilSemaphore;

(************************************************************************)

PROCEDURE SemVal (s: Semaphore): INTEGER;

    (* Returns the semaphore value.  Not intended for production use,   *)
    (* but can be useful while debugging.                               *)

    BEGIN
        RETURN s^.value;
    END SemVal;

(************************************************************************)

PROCEDURE Wait (s: Semaphore);

    (* Decrements the semaphore value.  If the value goes negative, the *)
    (* calling task is blocked and there is a task switch.              *)

    VAR p: BlockedListPointer;  ThreadID: TaskID;

    BEGIN
        IF s = NIL THEN Crash ("Wait on nonexistent semaphore"); END(*IF*);
        IF s^.marker <> MarkerVal THEN
            WriteString ("In Wait, Semaphore corrupted");  WriteLn;
        END (*IF*);
        WITH s^ DO
            Obtain (access);
            DEC (value);
            IF value < 0 THEN
                ThreadID := CurrentTaskID();
                NEW (p);
                p^.next := NIL;  p^.ThreadID := ThreadID;
                WITH BlockedList DO
                    IF tail = NIL THEN
                        head := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*WITH*);
                Release (access);
                IF SuspendMe (ThreadID, SEM_INDEFINITE_WAIT) THEN
                    Crash ("Semaphore Wait failure");
                END (*IF*);

                (* We resume here after some other task unblocks us. *)

                Obtain (access);
            END (*IF*);
            Release (access);
        END (*WITH*);
    END Wait;

(************************************************************************)

PROCEDURE TimedWaitInternal (s: Semaphore;  TimeLimit: INTEGER;
                               ConsumeSurplus: BOOLEAN;
                                 VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like procedure Wait, except that it returns with TimedOut TRUE   *)
    (* if the corresponding Signal does not occur within TimeLimit      *)
    (* clock ticks.  Note that this procedure is not recommended for    *)
    (* general use, because "clock ticks" is not a convenient unit of   *)
    (* time for most callers.  For a more useful version, see procedure *)
    (* TimedWait in module Timer.                                       *)
    (* If ConsumeSurplus is true, we cancel all the semaphore "credit"  *)
    (* that has been built up by possibly multiple Signal operations.   *)

    VAR p, previous, current: BlockedListPointer;  ThreadID: TaskID;

    BEGIN
        IF TimeLimit <= 0 THEN TimeLimit := 1; END(*IF*);

        (* Possible OS/2 bug: I'm not sure that the case TimeLimit=0    *)
        (* works correctly; but changing 0 to 1 should have little      *)
        (* impact on most tasks.                                        *)

        IF s = NIL THEN Crash ("Wait on nonexistent semaphore"); END(*IF*);
        IF s^.marker <> MarkerVal THEN
            WriteString ("In TimedWaitInternal, Semaphore corrupted");  WriteLn;
        END (*IF*);
        WITH s^ DO
            Obtain (access);
            DEC (value);
            IF value < 0 THEN
                ThreadID := CurrentTaskID();
                NEW (p);
                p^.next := NIL;  p^.ThreadID := ThreadID;
                WITH BlockedList DO
                    IF tail = NIL THEN
                        head := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*WITH*);
                Release (access);

                TimedOut := SuspendMe (ThreadID, TimeLimit);

                Obtain (access);
                IF TimedOut THEN
                    INC(value);
                    previous := NIL;  current := BlockedList.head;
                    WHILE (current <> NIL) AND (current <> p) DO
                        previous := current;  current := current^.next;
                    END (*WHILE*);
                    IF current = p THEN
                        IF previous = NIL THEN
                            BlockedList.head := p^.next;
                        ELSE
                            previous^.next := p^.next;
                        END (*IF*);
                        IF p^.next = NIL THEN
                            BlockedList.tail := previous;
                        END (*IF*);
                        DISPOSE (p);
                    END (*IF*);
                END (*IF*);
            ELSE                     (* value >= 0 *)
                TimedOut := FALSE;
                IF ConsumeSurplus THEN
                    value := 0;
                END (*IF*);
            END (*IF*);
            Release (access);
        END (*WITH*);
    END TimedWaitInternal;

(************************************************************************)

PROCEDURE Signal (s: Semaphore);

    (* Increments the semaphore value.  Unblocks one task, if there was *)
    (* one waiting on this semaphore.                                   *)

    VAR p: BlockedListPointer;  ThreadToUnblock: TaskID;

    BEGIN
        IF s = NIL THEN Crash ("Signal on nonexistent semaphore"); END(*IF*);
        IF s^.marker <> MarkerVal THEN
            WriteString ("In Signal, Semaphore corrupted");  WriteLn;
        END (*IF*);
        WITH s^ DO
            Obtain (access);
            INC (value);
            IF value <= 0 THEN
                WITH BlockedList DO
                    p := head;  head := p^.next;
                    IF head = NIL THEN tail := NIL END(*IF*);
                    ThreadToUnblock := p^.ThreadID;
                    DISPOSE (p);
                END (*WITH*);

                (* The recursion below is to handle the possibility     *)
                (* that we're trying to resume a task that no longer    *)
                (* exists.  This is not a common situation, but it      *)
                (* does occur during program shutdown.                  *)

                Release (access);
                IF NOT ResumeTask (ThreadToUnblock) THEN
                    Signal (s);
                END (*IF*);
            ELSE
                Release (access);
            END (*IF*);
        END (*WITH*);
    END Signal;

(************************************************************************)

PROCEDURE SSignal (text: ARRAY OF CHAR;  s: Semaphore);

    (* A special version of Signal where we record the calls.   *)

    VAR p: BlockedListPointer;  ThreadToUnblock: TaskID;

    BEGIN
        WriteString (text);  WriteString (" SSignal");  WriteLn;
        IF s = NIL THEN Crash ("Signal on nonexistent semaphore"); END(*IF*);
        IF s^.marker <> MarkerVal THEN
            WriteString ("In Signal, Semaphore corrupted");  WriteLn;
        END (*IF*);
        WITH s^ DO
            Obtain (access);
            INC (value);
            IF value <= 0 THEN
                WITH BlockedList DO
                    p := head;  head := p^.next;
                    IF head = NIL THEN tail := NIL END(*IF*);
                    ThreadToUnblock := p^.ThreadID;
                    DISPOSE (p);
                END (*WITH*);

                (* The recursion below is to handle the possibility     *)
                (* that we're trying to resume a task that no longer    *)
                (* exists.  This is not a common situation, but it      *)
                (* does occur during program shutdown.                  *)

                Release (access);
                IF NOT ResumeTask (ThreadToUnblock) THEN
                    Signal (s);
                END (*IF*);
            ELSE
                Release (access);
            END (*IF*);
        END (*WITH*);
    END SSignal;

(************************************************************************)

END Semaphores.

