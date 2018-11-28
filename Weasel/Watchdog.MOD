(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

IMPLEMENTATION MODULE Watchdog;

        (********************************************************)
        (*                                                      *)
        (*             Times out inactive sessions              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            27 December 2017                *)
        (*  Last edited:        14 September 2018               *)
        (*  Status:             Appears to be working           *)
        (*                                                      *)
        (********************************************************)


<* IF WATCHTEST THEN *>

    IMPORT Strings;

    FROM SYSTEM IMPORT CAST;

    FROM LogCtx IMPORT
        (* var  *)  WCtx;

    FROM TransLog IMPORT
        (* type *)  TransactionLogID,
        (* proc *)  CreateLogID, LogTransaction, LogTransactionL;

    FROM MiscFuncs IMPORT
        (* proc *)  AppendCard;

<* END *>

FROM SYSTEM IMPORT ADDRESS;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, DestroySemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release, CreateTask;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)
(*                          GLOBAL DEFINITIONS                          *)
(************************************************************************)

TYPE
    WatchListPointer = POINTER TO WatchListEntry;
    WatchListEntry = RECORD
                         previous, next: WatchListPointer;
                         deltatime, timeout: CARDINAL;       (* seconds *)
                         tocall: PROC1;
                         arg: ADDRESS;
                         onlist: BOOLEAN;
                     END (*RECORD*);

    (* The list is doubly linked, and sorted by "time to expiry".  The  *)
    (* expiry time is not, however, stored explicitly.  Instead we      *)
    (* store deltatime, which is the difference between the expiry time *)
    (* of this element and the one ahead of it.  Thus, the expiry time  *)
    (* is the sum of all deltatime fields up to and including the       *)
    (* element in question.  The timeout field is the initial value of  *)
    (* the expiry time.  On timeout we call tocall(arg).                *)

    WatchdogID = WatchListPointer;

    (* We identify an element by pointing to it, so that we can go      *)
    (* directly to it rather than search the list.                      *)

VAR WatchList:  RECORD
                    access: Lock;
                    head, tail: WatchListPointer;
                END (*RECORD*);

    (* This list is kept sorted by the expiry time value, which says    *)
    (* when this entry is due to be removed from the list.  As noted    *)
    (* above, however, we do not explicitly store the expiry time       *)
    (* except for the tail element of the queue.                        *)

    LastTime: CARDINAL;

    (* LastTime is the sum of all deltatime values in the list.  That   *)
    (* means that it is the expiry time of WatchList.tail.              *)

    sleepstart: RECORD
                    access: Lock;
                    t: CARDINAL;
                END (*ROCORD*);

    (* sleepstart.t is the time that a Sleep() operation started.  It   *)
    (* is zero when there is no sleep in progress.  We need this to     *)
    (* correctly set the deltatime value of a newly introduced list     *)
    (* entry.                                                           *)

    TaskDone: Semaphore;

    ShutDown: BOOLEAN;

(************************************************************************)
(*                           DEBUGGING CODE                             *)
(************************************************************************)

<* IF WATCHTEST THEN *>

VAR LogID: TransactionLogID;

PROCEDURE Hex1 (val: CARDINAL): CHAR;

    (* Converts val to 1-digit hex. *)

    BEGIN
        IF val > 9 THEN
            RETURN CHR(ORD('A') - 10 + val);
        ELSE
            RETURN CHR(ORD('0') + val);
        END (*IF*);
    END Hex1;

(************************************************************************)

PROCEDURE FormatHex (val, N: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes N-digit hex number to text[pos], updates pos. *)

    BEGIN
        IF N > 1 THEN
            FormatHex (val DIV 16, N-1, text, pos);
        END (*IF*);
        text[pos] := Hex1(val MOD 16);
        INC (pos);
    END FormatHex;

(************************************************************************)

PROCEDURE FormatHex32 (val: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes 8-digit hex number to text[pos], updates pos. *)

    BEGIN
        FormatHex (val, 8, text, pos);
    END FormatHex32;

(************************************************************************)

PROCEDURE FormatDec (val: CARDINAL;  VAR (*INOUT*) text: ARRAY OF CHAR;
                                        VAR (*INOUT*) pos: CARDINAL);

    (* Writes decimal number to text[pos], updates pos. *)

    BEGIN
        IF val > 9 THEN
            FormatDec (val DIV 10, text, pos);
            val := val MOD 10;
        END (*IF*);
        text[pos] := CHR(ORD('0') + val);  INC (pos);
    END FormatDec;

(************************************************************************)

PROCEDURE FormatRecord (p: WatchListPointer;
                                        VAR (*OUT*) text: ARRAY OF CHAR);

    VAR pos: CARDINAL;

    BEGIN
        Strings.Assign ("   ", text);
        pos := LENGTH(text);
        FormatHex32 (CAST(CARDINAL,p), text, pos);
        text[pos] := ' ';  INC(pos);
        FormatDec (p^.deltatime, text, pos);
        text[pos] := ' ';  INC(pos);
        FormatDec (p^.timeout, text, pos);
        text[pos] := CHR(0);
    END FormatRecord;

(************************************************************************)

PROCEDURE DumpList (label: ARRAY OF CHAR);

    (* Dumps the WatchList to the transaction log.  We assume that the  *)
    (* caller has exclusive access.                                     *)

    VAR current: WatchListPointer;
        message: ARRAY [0..79] OF CHAR;

    BEGIN
        LogTransaction (LogID, label);
        current := WatchList.head;
        IF current = NIL THEN
            LogTransactionL (LogID, "   <empty>");
        ELSE
            WHILE current <> NIL DO
                FormatRecord (current, message);
                LogTransaction (LogID, message);
                current := current^.next;
            END (*WHILE*);
            LogTransactionL (LogID, "   ----- end of list");
        END (*IF*);
    END DumpList;

(************************************************************************)

PROCEDURE DumpWatchdogState (sleepsecs, overshoot: CARDINAL);

    (* Dumps the state of the watchdog thread.  We assume that the      *)
    (* caller has exclusive access to the WatchList.                    *)

    VAR label: ARRAY [0..79] OF CHAR;
        pos: CARDINAL;

    BEGIN
        label := "Watchdog: sleepsecs = ";
        pos := LENGTH(label);
        FormatDec (sleepsecs, label, pos);
        label[pos] := CHR(0);
        Strings.Append (", overshoot = ", label);
        pos := LENGTH(label);
        FormatDec (overshoot, label, pos);
        label[pos] := CHR(0);
        DumpList (label);
    END DumpWatchdogState;

(************************************************************************)

PROCEDURE HangIfDiscrepancy (msg: ARRAY OF CHAR);

    (* Assumes the caller has obtained WatchList.access.  Writes msg to *)
    (* the log and then crashes if there is a LastTime discrepancy.     *)

    VAR total: CARDINAL;  p: WatchListPointer;
        logline: ARRAY [0..79] OF CHAR;

    BEGIN
        total := 0;  p := WatchList.head;
        WHILE p <> NIL DO
            INC (total, p^.deltatime);
            p := p^.next;
        END (*WHILE*);
        IF total <> LastTime THEN
            Strings.Assign (msg, logline);
            Strings.Append (", LastTime discrepancy", logline);
            LogTransaction (LogID, logline);
            p := NIL;
            p^.deltatime := 0;
        END (*IF*);
    END HangIfDiscrepancy;

(************************************************************************)

<* END *>

(************************************************************************)
(*                   ADDING AND REMOVING LIST ENTRIES                   *)
(************************************************************************)

PROCEDURE ReinsertI (p: WatchdogID);

    (* The internal version of Reinsert (see below) without the         *)
    (* critical section protection.                                     *)

    VAR before, after: WatchdogID;
        T, thisT: CARDINAL;
        <* IF WATCHTEST THEN *>
            msg: ARRAY [0..63] OF CHAR;
        <* END *>

    BEGIN
        <* IF WATCHTEST THEN *>
            HangIfDiscrepancy ("Entering ReinsertI");
        <* END *>

        (* Find the right insertion point. *)

        T := LastTime;   thisT := p^.timeout;
        Obtain (sleepstart.access);
        IF sleepstart.t > 0 THEN
            INC (thisT, time() - sleepstart.t);
        END (*IF*);
        Release (sleepstart.access);
        before := WatchList.tail;  after := NIL;

        (* Throughout this loop, T is the total time    *)
        (* for the "before" element.                    *)

        WHILE (before <> NIL) AND (T > thisT) DO
            after := before;
            before := before^.previous;
            IF T < after^.deltatime THEN
                T := 0;         (* should never happen *)
            ELSE
                DEC (T, after^.deltatime);
            END (*IF*);
        END (*WHILE*);

        (* Insert between before^ and after^.  *)

        IF before = NIL THEN
            p^.deltatime := thisT;
            WatchList.head := p;
        ELSE
            p^.deltatime := thisT - T;
            before^.next := p;
        END (*IF*);
        p^.previous := before;
        p^.next := after;
        IF after = NIL THEN
            INC (LastTime, p^.deltatime);
            WatchList.tail := p;
        ELSE
            IF after^.deltatime >= p^.deltatime THEN
                DEC (after^.deltatime, p^.deltatime);
            ELSE
                (* This case should not happen, so I should re-check    *)
                (* the assumptions in the code above.                   *)
                after^.deltatime := 0;
            END (*IF*);
            after^.previous := p;
        END (*IF*);
        p^.onlist := TRUE;

        <* IF WATCHTEST THEN *>
            msg := "After ReinsertI, LastTime = ";
            AppendCard (LastTime, msg);
            DumpList (msg);
            HangIfDiscrepancy ("Leaving ReinsertI");
        <* END *>

    END ReinsertI;

(************************************************************************)

PROCEDURE Reinsert (p: WatchdogID);

    (* Assumption: the item to be inserted is not currently in the      *)
    (* list.  We also assume that the insertion position is likely to   *)
    (* be towards the end of the list, but that assumption affects only *)
    (* efficiency, not correctness.                                     *)

    BEGIN
        Obtain (WatchList.access);
        ReinsertI (p);
        Release (WatchList.access);
    END Reinsert;

(************************************************************************)

PROCEDURE AddToWatches (timeout: CARDINAL;
                        handler: PROC1;  arg: ADDRESS): WatchdogID;

    (* Adds a new thread to be watched.  If the thread times out,   *)
    (* handler(arg) is called.                                      *)

    VAR result: WatchdogID;

    BEGIN
        NEW (result);
        result^.previous := NIL;  result^.next := NIL;
        result^.timeout := timeout;  result^.onlist := FALSE;
        result^.tocall := handler;  result^.arg := arg;
        Reinsert (result);
        RETURN result;
    END AddToWatches;

(************************************************************************)

PROCEDURE DetachFromListI (ID: WatchdogID);

    (* The internal version of DetachFromList (see below) without   *)
    (* critical section protection.                                 *)

    VAR before, after: WatchdogID;
        <* IF WATCHTEST THEN *>
            msg: ARRAY [0..63] OF CHAR;
        <* END *>

    BEGIN
        <* IF WATCHTEST THEN *>
            msg := "Before DetachFromListI, LastTime = ";
            AppendCard (LastTime, msg);
            DumpList (msg);
            HangIfDiscrepancy ("Entering DetachFromListI");
        <* END *>
        before := ID^.previous;  ID^.previous := NIL;
        after := ID^.next;  ID^.next := NIL;
        IF before = NIL THEN
            WatchList.head := after;
        ELSE
            before^.next := after;
        END (*IF*);
        IF after = NIL THEN
            WatchList.tail := before;
            IF LastTime < ID^.deltatime THEN
                LastTime := 0;          (* should never happen *)
            ELSE
                DEC (LastTime, ID^.deltatime);
            END (*IF*);
        ELSE
            after^.previous := before;
            INC (after^.deltatime, ID^.deltatime);
        END (*IF*);
        ID^.onlist := FALSE;
        <* IF WATCHTEST THEN *>
            msg := "After DetachFromListI, LastTime = ";
            AppendCard (LastTime, msg);
            DumpList (msg);
            HangIfDiscrepancy ("Leaving DetachFromListI");
        <* END *>
    END DetachFromListI;

(************************************************************************)

PROCEDURE DetachFromList (ID: WatchdogID);

    (* Removes this entry from the watchdog list, without   *)
    (* destroying it.                                       *)

    (* Possible race: this procedure can be called in a user thread     *)
    (* just when the watchdog thread is in the process of removing this *)
    (* ID from the WatchList.  If we get in first there is no problem,  *)
    (* because then this procedure will complete execution and the      *)
    (* watchdog thread will never see this ID.  Otherwise, we will      *)
    (* initially be blocked on WatchList.access; then, when we pass the *)
    (* Obtain(), we will find that ID^.onlist is FALSE.  It is the      *)
    (* "onlist" flag that resolves the conflict.                        *)

    BEGIN
        Obtain (WatchList.access);
        IF ID^.onlist THEN
            DetachFromListI (ID);
        END (*IF*);
        Release (WatchList.access);
    END DetachFromList;

(************************************************************************)

PROCEDURE DetachFirstFromList;

    (* Removes the head entry from the watchdog list, without           *)
    (* destroying it.  This is an internal version that assumes that    *)
    (* the caller already has exclusive access to the list.             *)

    VAR p, after: WatchdogID;

    BEGIN
        p := WatchList.head;
        after := p^.next;  p^.next := NIL;
        WatchList.head := after;
        IF after = NIL THEN
            WatchList.tail := NIL;
            LastTime := 0;
        ELSE
            after^.previous := NIL;
            INC (after^.deltatime, p^.deltatime);
        END (*IF*);
        p^.onlist := FALSE;
    END DetachFirstFromList;

(************************************************************************)

PROCEDURE RemoveFromWatches (VAR (*INOUT*) ID: WatchdogID;  destroy: BOOLEAN);

    (* Removes this entry from the watchdog list if it is still on  *)
    (* the list. If destroy is TRUE, we also dispose of the record. *)

    BEGIN
        IF (ID <> NIL)  THEN

            (* There is potentially a critical race if more than one    *)
            (* thread wants to remove this ID from the WatchList.       *)
            (* DetachFromList resolves this problem by checking the     *)
            (* flag ID^.onlist inside a protected critical section.     *)

            IF ID^.onlist THEN
                DetachFromList (ID);
            END (*IF*);
            IF destroy THEN
                DISPOSE (ID);
            END (*IF*);
        END (*IF*);
    END RemoveFromWatches;

(************************************************************************)
(*                           KICKING THE WATCHDOG                       *)
(************************************************************************)

PROCEDURE KickWatchdog (ID: WatchdogID);

    (* Tells the watchdog that this entry is still active.  We do not   *)
    (* affect the watchdog thread; we simply reset the time-to-go of    *)
    (* this entry.                                                      *)

    BEGIN
        <* IF WATCHTEST THEN *>
            LogTransactionL (LogID, "KickWatchdog called");
        <* END *>

        (* The removal from the Watchlist and the reinsertion must be   *)
        (* indivisible, to avoid a critical race.                       *)

        Obtain (WatchList.access);
        DetachFromListI (ID);
        ReinsertI (ID);
        Release (WatchList.access);
    END KickWatchdog;

(************************************************************************)
(*                           THE "KILL" QUEUE                           *)
(*                                                                      *)
(* A problem with the watchdog timing is that we cannot predict how     *)
(* long it will take to shut down a thread that has timed out.  To      *)
(* resolve this, we break the timeout checker into two parts.  The      *)
(* thread called WatchdogThread keeps track of the timing, including    *)
(* the detection of threads that have timed out.  The thread called     *)
(* KillerThread executes the timeout code.  That ensures that, even if  *)
(* the timeout code takes a long time to execute, WatchdogThread does   *)
(* not lose track of the timing.  Information is passed between the     *)
(* two threads via the KillQ queue structure.                           *)
(*                                                                      *)
(************************************************************************)

TYPE
    KillQptr = POINTER TO
                    RECORD
                        next: KillQptr;
                        this: WatchdogID;
                    END (* RECORD *);

VAR KillQ:  RECORD
                access: Lock;
                count: Semaphore;
                head, tail: KillQptr;
            END (*RECORD*);

(************************************************************************)

PROCEDURE GetFromKillQ(): WatchdogID;

    (* Removes the first element from KillQ. *)

    VAR p: KillQptr;  result: WatchdogID;

    BEGIN
        Wait (KillQ.count);
        IF ShutDown THEN
            RETURN NIL;
        END (*IF*);
        Obtain (KillQ.access);
        p := KillQ.head;
        KillQ.head := p^.next;
        IF KillQ.head = NIL THEN
            KillQ.tail := NIL;
        END (*IF*);
        Release (KillQ.access);
        result := p^.this;
        DISPOSE (p);
        RETURN result;
    END GetFromKillQ;

(************************************************************************)

PROCEDURE AddToKillQ (ID: WatchdogID);

    (* Adds a new element to the tail of KillQ. *)

    VAR p: KillQptr;

    BEGIN
        NEW (p);
        p^.next := NIL;
        p^.this := ID;
        Obtain (KillQ.access);
        IF KillQ.tail = NIL THEN
            KillQ.head := p;
        ELSE
            KillQ.tail^.next := p;
        END (*IF*);
        KillQ.tail := p;
        Release (KillQ.access);
        Signal (KillQ.count);
    END AddToKillQ;

(************************************************************************)
(*                         THE WATCHDOG THREADS                         *)
(************************************************************************)

PROCEDURE KillerThread;

    (* This is the thread that invokes the timeout code of a timed-out  *)
    (* thread.                                                          *)

    VAR p: WatchdogID;

    BEGIN
        WHILE NOT ShutDown DO
            p := GetFromKillQ();
            IF p <> NIL THEN
                p^.tocall (p^.arg);
            END (*IF*);
        END (*WHILE*);
        Signal (TaskDone);
    END KillerThread;

(************************************************************************)

PROCEDURE WatchdogThread;

    (* This is the thread that does the timeout checks.  It keeps       *)
    (* checking whether any entry on the WatchList should be declared   *)
    (* "timed out".                                                     *)

    VAR p: WatchListPointer;
        sleepsecs, overshoot: CARDINAL;

    BEGIN
        sleepsecs := 0;  overshoot := 0;
        WHILE NOT ShutDown DO

            (* sleepsecs is the time (in seconds) that we should sleep  *)
            (* this time around the loop.  There are some situations,   *)
            (* including the first execution of this loop, when the     *)
            (* sleep value is zero, but that is not a problem; it will  *)
            (* resolved once we have dealt with all the pending timeout *)
            (* cases.                                                   *)

            (* overshoot measures how far we are behind schedule.       *)
            (* Ideally it should always be zero, but if not we should   *)
            (* sleep for (sleepsecs - overshoot) seconds.  The          *)
            (* following code allows for the possibility that we will   *)
            (* have to reduce overshoot gradually.                      *)

            IF overshoot > 0 THEN
                IF sleepsecs >= overshoot THEN
                    DEC (sleepsecs, overshoot);
                    overshoot := 0;
                ELSIF sleepsecs > 0 THEN
                    DEC (overshoot, sleepsecs);
                    sleepsecs := 0;
                END (*IF*);
            END (*IF*);

            (* Now we do the sleep, if required. *)

            IF sleepsecs > 0 THEN
                <* IF WATCHTEST THEN *>
                    IF sleepsecs > 1 THEN
                        LogTransactionL (LogID, "Before sleep");
                    END (*IF*);
                <* END *>
                Obtain (sleepstart.access);
                sleepstart.t := time();
                Release (sleepstart.access);

                Sleep (1000*sleepsecs);

                Obtain (sleepstart.access);
                sleepstart.t := 0;
                Release (sleepstart.access);
                <* IF WATCHTEST THEN *>
                    IF sleepsecs > 1 THEN
                        LogTransactionL (LogID, "After sleep");
                    END (*IF*);
                <* END *>
            END (*IF*);

            (* sleepsecs is the duration of our last sleep.  If the     *)
            (* WatchList were static then it would (except for the      *)
            (* first time through this loop, or after a timeout)        *)
            (* exactly match the deltatime value of the first element   *)
            (* on the list.  The list is, however, frequently updated   *)
            (* by the other procedures in this module, so there is no   *)
            (* guarantee that the first element on the list is the same *)
            (* one as the one we used to calculate sleepsecs.           *)

            (* Remark: we only check the first element on the list for  *)
            (* timeout.  It is possible that several list elements are  *)
            (* due to time out, but if so we will catch that on the     *)
            (* next pass through this loop body.                        *)

            Obtain (WatchList.access);
            p := WatchList.head;
            IF p = NIL THEN
                (* While the list is empty, we check once per second. *)
                sleepsecs := 1;
            ELSE
                <* IF WATCHTEST THEN *>
                    DumpWatchdogState (sleepsecs, overshoot);
                <* END *>
                IF p^.deltatime > sleepsecs THEN
                    IF sleepsecs > 0 THEN
                        DEC (p^.deltatime, sleepsecs);
                        IF LastTime < sleepsecs THEN
                            LastTime := 0;          (* should never happen *)
                        ELSE
                            DEC (LastTime, sleepsecs);
                        END (*IF*);
                    END (*IF*);
                    sleepsecs := p^.deltatime;
                ELSE
                    (* TIMEOUT. Remove the first element from the list. *)

                    IF sleepsecs > p^.deltatime THEN
                        INC (overshoot, sleepsecs - p^.deltatime);
                    END (*IF*);
                    DetachFirstFromList;
                    AddToKillQ (p);
                    sleepsecs := 0;

                    (* sleepsecs will be recalculated on the next pass  *)
                    (* through this loop body.                          *)

                END (*IF*);
            END (*IF*);
            Release (WatchList.access);
        END (*WHILE*);
        Signal (TaskDone);

    END WatchdogThread;

(************************************************************************)

PROCEDURE FlushList;

    (* Shutdown processing.  Destroys the WatchList.  *)

    VAR p, next: WatchListPointer;

    BEGIN
        Obtain (WatchList.access);
        p := WatchList.head;
        WHILE p <> NIL DO
            next := p^.next;
            DISPOSE (p);
            p := next;
        END (*WHILE*);
        WatchList.head := NIL;
        WatchList.tail := NIL;
        Release (WatchList.access);
        DestroyLock (WatchList.access);
    END FlushList;

(************************************************************************)

BEGIN
    <* IF WATCHTEST THEN *>
        LogID := CreateLogID (WCtx, "watch  ");
    <* END *>
    CreateLock (WatchList.access);
    WatchList.head := NIL;
    WatchList.tail := NIL;
    LastTime := 0;
    CreateLock (sleepstart.access);
    sleepstart.t := 0;
    ShutDown := FALSE;
    WITH KillQ DO
        CreateLock (access);
        CreateSemaphore (count, 0);
        head := NIL; tail := NIL;
    END (*WITH*);
    CreateSemaphore (TaskDone, 0);
    EVAL (CreateTask (WatchdogThread, 0, "watchdog"));
    EVAL (CreateTask (KillerThread, 0, "killer"));
FINALLY
    ShutDown := TRUE;
    Signal (KillQ.count);

    (* Remainder of module termination code removed, because there      *)
    (* appears to be a deadlock that I can't track down.                *)

    (*
    Wait (TaskDone);  Wait (TaskDone);
    DestroySemaphore (TaskDone);
    FlushList;
    WITH KillQ DO
        DestroyLock (access);
        DestroySemaphore (count);
    END (*WITH*);
    DestroyLock (sleepstart.access);
    DestroyLock (WatchList.access);
    *)
END Watchdog.

