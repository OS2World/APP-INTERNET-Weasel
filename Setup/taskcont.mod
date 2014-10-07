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

IMPLEMENTATION MODULE TaskControl;

<* IF NOT multithread THEN *>
  "This module needs the multithread model"
  END TaskControl.
<* END *>

        (****************************************************************)
        (*                                                              *)
        (*   Data structures internal to the kernel of the operating    *)
        (*     system; the dispatcher of the operating system; and      *)
        (*                  related procedures.                         *)
        (*                                                              *)
        (*      Programmer:     P. Moylan                               *)
        (*      Last edited:    22 December 2013                        *)
        (*      Status:         OK                                      *)
        (*                                                              *)
        (*    Note that most of the PMOS kernel is missing from this    *)
        (*    version, so we don't have features like priority          *)
        (*    inheritance or true real-time scheduling.  Instead, all   *)
        (*    of the thread switching is done by the OS/2 kernel, and   *)
        (*    we retain only those features needed by the higher-level  *)
        (*    modules like Semaphores.                                  *)
        (*                                                              *)
        (****************************************************************)

<* M2EXTENSIONS+ *>

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  CAST, ADR;

IMPORT OS2, Processes;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Assert, EVAL;

FROM Exceptq IMPORT
    (* proc *)  InstallExceptq, UninstallExceptq;

FROM SplitScreen IMPORT
    (* proc *)  LockScreen, UnlockScreen,
                WriteChar, WriteString, WriteLn;

(*
FROM TCdebug IMPORT
    (* type *)  SemOpKind, ThrOpKind,
    (* proc *)  StartDebugLogging, NoteSemOperation, NoteThreadOperation;
*)

(************************************************************************)

CONST StackSize = 262144    (* 65536 seems to be too small for thunking *);

TYPE

    (********************************************************************)
    (*                                                                  *)
    (* Descriptor for a task.  The fields have the following meaning.   *)
    (*                                                                  *)
    (*   next        pointer to the next descriptor on the master task  *)
    (*                list                                              *)
    (*   exRegRec    for use by exceptq                                 *)
    (*   eqValid     TRUE iff the exRegRec component is meaningful      *)
    (*   active      FALSE iff this task is blocked on WakeUp.          *)
    (*                    (debugging aid, now obsolete)                 *)
    (*   name        identifier for testing purposes                    *)
    (*   WakeUp      event semaphore used in blocking a task            *)
    (*   threadnum   thread identifier                                  *)
    (*                                                                  *)
    (* I'm not sure whether exceptq handling is going to have problems  *)
    (* with the default record alignment (1 byte), so to be on the      *)
    (* safe side I'll set the alignment to 4 for task descriptor        *)
    (* records.  This should cause only a minor waste of heap space.    *)
    (*                                                                  *)
    (********************************************************************)

    Task = POINTER TO
               RECORD
                   next: Task;
                   exRegPtr: POINTER TO OS2.EXCEPTIONREGISTRATIONRECORD;
                   (*active: BOOLEAN;*)
                   eqValid: BOOLEAN;
                   name: NameString;
                   WakeUp: OS2.HEV;
                   threadnum: TaskID;
               END (*RECORD*);

(************************************************************************)

VAR
    (* The number of tasks now in the system, and the maximum allowed. *)

    NumberOfThreads, MaxNumberOfThreads: CARDINAL;

    (* The list of all tasks known to us. *)

    MasterTaskList: Task;

    (* Mutual exclusion semaphore.  We must lock this for any access to *)
    (* the task list.  For simplicity we also use this as the mutex     *)
    (* protecting the variables NumberOfThreads and MaxNumberOfThreads. *)

    TaskListAccess: OS2.HMTX;

    (* A copy of the last TaskID <-> Task match made by DescriptorOf.   *)
    (* This sometimes speeds up task identification.                    *)

    (*
    lastthread: TaskID;
    lastTask: Task;
    *)

(************************************************************************)
(*                      TERMINATION ON FATAL ERROR                      *)
(*                                                                      *)
(*  Note that the message output will be useless for a detached         *)
(*  program, but then I expect to use this message output only when     *)
(*  debugging.                                                          *)
(************************************************************************)

TYPE CardPtr = POINTER TO CARDINAL;
CONST PtrTo0 = CAST(CardPtr, 0);

PROCEDURE Crash (message: ARRAY OF CHAR);

    (* Aborts the program in a way that will let us do a postmortem check. *)

    BEGIN
        LockScreen;
        WriteString (message);  WriteLn;
        UnlockScreen;
        PtrTo0^ := 0;
    END Crash;

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

TYPE SemKind = (mutexsem, eventsem);

PROCEDURE SemError (kind: SemKind;  errornum: CARDINAL);

    BEGIN
        LockScreen;
        CASE kind OF
            mutexsem: WriteString ("Mutex");
          |
            eventsem: WriteString ("Event");
        ELSE
                      WriteString ("Unknown");
        END (*CASE*);
        WriteString (" semaphore error ");
        WriteCard (errornum);
        WriteLn;
        UnlockScreen;
        PtrTo0^ := 0;
    END SemError;

(************************************************************************)
(*                 KERNEL CRITICAL SECTION PROTECTION                   *)
(************************************************************************)

PROCEDURE LockTaskList;

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosRequestMutexSem (TaskListAccess, OS2.SEM_INDEFINITE_WAIT);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END LockTaskList;

(************************************************************************)

PROCEDURE UnlockTaskList;

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosReleaseMutexSem (TaskListAccess);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END UnlockTaskList;

(************************************************************************)
(*                            TASK CREATION                             *)
(************************************************************************)

PROCEDURE NewTaskDescriptor (taskname: NameString): Task;

    (* Creates a descriptor for a new task, and adds it to the master   *)
    (* task list.  Note that this does not fill in all fields of the    *)
    (* descriptor.  A lot of the information is filled in by the        *)
    (* task wrapper that runs when the task starts.                     *)

    VAR result: Task;

    BEGIN
        NEW (result);
        result^.name := taskname;
        result^.eqValid := FALSE;
        result^.threadnum := 0;
        result^.WakeUp := 0;

        LockTaskList;
        result^.next := MasterTaskList;
        MasterTaskList := result;
        UnlockTaskList;

        RETURN result;

    END NewTaskDescriptor;

(************************************************************************)

TYPE TaskStartInfo = POINTER TO
                          RECORD
                              HasParameter: BOOLEAN;
                              TaskCode0: PROC;
                              TaskCode1: PROC1;
                              parameter: ADDRESS;
                              descriptor: Task;
                          END;

(************************************************************************)

PROCEDURE Dummy0;

    (* Dummy code, should never be run. *)

    BEGIN
        Crash ("Dummy0 entered");
    END Dummy0;

(************************************************************************)

PROCEDURE Dummy1 (param: ADDRESS);

    (* Dummy code, should never be run. *)

    BEGIN
        Crash ("Dummy1 entered");
        IF param <> NIL THEN  (* Pointless code to suppress a compiler warning *)
            HALT;
        END (*IF*)
    END Dummy1;

(************************************************************************)

PROCEDURE TaskWrapper;

    (* This is the task that runs the user's task code. *)

    VAR StartInfo: TaskStartInfo;
        T: Task;
        errno: CARDINAL;
        UseParameter: BOOLEAN;
        Proc0: PROC;
        Proc1: PROC1;  param: ADDRESS;
        exRegRec: OS2.EXCEPTIONREGISTRATIONRECORD;

    BEGIN
        (* Before starting the task, adjust its stack so that the       *)
        (* bottom of the stack is near a 64K boundary.  This wastes     *)
        (* some stack space, but helps to avoid a bug that occurs when  *)
        (* a system call requires a thunk to 16-bit code.  (At the time *)
        (* of the thunking, we don't want ESP to be near a 64K          *)
        (* boundary.)  To the best of my knowledge we don't need to     *)
        (* readjust the stack when the task exits, because the TaskExit *)
        (* call should leave the remainder of the stack irrelevant.     *)

        (* Assumptions for the assembly language code:                  *)
        (*   1.  DS=ES=SS, i.e. we are running with a flat memory       *)
        (*       model.  It is known that the compiler requires any     *)
        (*       procedure to satisfy ED=DS on exit.                    *)
        (*   2.  The stack is aligned on a 4K boundary.  This is true   *)
        (*       if the thread has been created by DosCreateThread.     *)
        (*   3.  It is not known whether the stack space has been       *)
        (*       pre-committed.                                         *)

        ASM
            PUSH ECX                 (* save three registers            *)
            PUSH ESI
            PUSH EDI
            MOV ESI, ESP             (* make two copies of              *)
            MOV EDI, ESP             (*     original stack pointer      *)
            MOV ECX, ESP             (* work out how many 4K blocks     *)
            SHR ECX, 12              (*    to move the stack by         *)
            INC ECX                  (*    to align it with a           *)
            AND ECX, 0FH             (*    64K boundary                 *)
            JCXZ L0002
            CMP ECX, 0DH             (* if already close to a 64k       *)
            JAE L0002                (* boundary, skip the correction   *)

            (* We execute the following loop ECX times, moving EDI by   *)
            (* 4K bytes (one page) each time.                           *)

        L0001:
            SUB EDI, 1000H           (* back 4K bytes                   *)
            MOV [EDI], ECX           (* touch new stack page            *)
            LOOP L0001               (* do this as often as needed      *)

            (* Now EDI is where we want the new stack pointer to be.    *)

            MOV ESP, EDI             (* change stack pointer            *)
            CLD
            MOV ECX, 22
            DB -13   (*REP*)         (* move old stack contents down    *)
            MOVSD
        L0002:
            POP EDI                  (* all done, restore registers     *)
            POP ESI
            POP ECX
        END;

        (* Copy the start parameter record.   *)

        StartInfo := Processes.MyParam();
        WITH StartInfo^ DO
            UseParameter := HasParameter;
            Proc0 := TaskCode0;
            Proc1 := TaskCode1;
            param := parameter;
            T := descriptor;
        END (*WITH*);

        DISPOSE (StartInfo);

        (* Fill in the rest of the task descriptor, including           *)
        (* discovering our thread ID.                                   *)

        LockTaskList;
        T^.threadnum := CurrentTaskID();
        (*NoteThreadOperation (thr_start, T^.threadnum, T^.name);*)
        errno := OS2.DosCreateEventSem (NIL, T^.WakeUp, 0, FALSE);
        (*NoteSemOperation (sem_creat, T^.WakeUp, T^.threadnum, errno);*)
        IF errno <> 0 THEN
            SemError (eventsem, errno);
        END (*IF*);

        (* Enable exceptq tracking for this thread.  The corresponding  *)
        (* unload is done inside procedure TaskExit.                    *)

        T^.eqValid := InstallExceptq (exRegRec);
        T^.exRegPtr := ADR(exRegRec);
        UnlockTaskList;

        (* Call the user's task code. *)

        IF UseParameter THEN
            Proc1 (param);
        ELSE
            Proc0;
        END (*IF*);

        (* If the task exits by falling out of the bottom of its        *)
        (* code, terminate it with an explicit TaskExit.                *)

        TaskExit;

    END TaskWrapper;

(************************************************************************)

PROCEDURE CreateTask0 (StartAddress0: PROC;  StartAddress1: PROC1;
                          taskpriority: PriorityLevel;
                          taskname: NameString;  param: ADDRESS): BOOLEAN;

    (* The common code for CreateTask and CreateTask1, below.           *)

    VAR StartInfo: TaskStartInfo;  T: Task;
        id: Processes.ProcessId;
        success: BOOLEAN;

    BEGIN
        LockTaskList;
        success := NumberOfThreads < MaxNumberOfThreads;
        IF success THEN
            INC (NumberOfThreads);
        END (*IF*);
        UnlockTaskList;
        IF NOT success THEN
            RETURN FALSE;
        END (*IF*);
        T := NewTaskDescriptor (taskname);
        success := T <> NIL;
        IF NOT success THEN
            RETURN FALSE;
        END (*IF*);

        NEW (StartInfo);
        WITH StartInfo^ DO
            HasParameter := StartAddress1 <> NIL;
            IF HasParameter THEN
                TaskCode0 := Dummy0;
                TaskCode1 := StartAddress1;
            ELSE
                TaskCode0 := StartAddress0;
                TaskCode1 := Dummy1;
            END (*IF*);
            parameter := param;
            descriptor := T;
        END (*WITH*);

        Processes.Start (TaskWrapper, StackSize, taskpriority, StartInfo, id);
        RETURN success;

    END CreateTask0;

(************************************************************************)

PROCEDURE CreateTask (StartAddress: PROC;  taskpriority: PriorityLevel;
                                           taskname: NameString): BOOLEAN;

    (* Must be called to introduce a task to the system. The first      *)
    (* parameter, which should be the name of a procedure containing    *)
    (* the task code, gives the starting address.  The second parameter *)
    (* is the task's base priority.  If this task has a higher priority *)
    (* than its creator, it will run immediately.  Otherwise, it        *)
    (* becomes ready.                                                   *)

    BEGIN
        RETURN CreateTask0 (StartAddress, NIL, taskpriority, taskname, NIL);
    END CreateTask;

(************************************************************************)

PROCEDURE CreateTask1 (StartAddress: PROC1;  taskpriority: PriorityLevel;
                          taskname: NameString;  param: ADDRESS): BOOLEAN;

    (* Like CreateTask, but allows the passing of a single parameter    *)
    (* "param" to the task.                                             *)

    BEGIN
        RETURN CreateTask0 (NIL, StartAddress, taskpriority, taskname, param);
    END CreateTask1;

(************************************************************************)

PROCEDURE ThreadCount(): CARDINAL;

    (* Returns the number of currently running threads. *)

    BEGIN
        RETURN NumberOfThreads;
    END ThreadCount;

(************************************************************************)
(*                           TASK TERMINATION                           *)
(************************************************************************)

PROCEDURE TaskExit;

    (* Removes the currently running task from the system, and performs *)
    (* a task switch to the next ready task.                            *)
    (* (In this OS/2 version we don't actually do the task switch; we   *)
    (* let the operating system do it for us.)                          *)

    VAR MyID: TaskID;  previous, current: Task;
        errno, postcount: CARDINAL;

    BEGIN
        MyID := CurrentTaskID();

        LockTaskList;
        previous := NIL;  current := MasterTaskList;
        WHILE (current <> NIL) AND (current^.threadnum <> MyID) DO
            previous := current;  current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN

            (*current^.active := FALSE;*)
            IF current^.eqValid THEN
                UninstallExceptq (current^.exRegPtr^);
            END (*IF*);

            (* Remark: the descriptor for the main task does not have   *)
            (* the eqValid flag set, because the exceptq handler for    *)
            (* the main program is installed in the main program, not   *)
            (* by this module.  That means that if the main thread      *)
            (* calls TaskExit the UninstallExceptq operation will not   *)
            (* be done.  I doubt that this matters, because in that     *)
            (* case we're terminating the program anyway.  In any case, *)
            (* it would be most unusual for the main thread to call     *)
            (* TaskExit.                                                *)

            IF previous = NIL THEN
                MasterTaskList := current^.next;
            ELSE
                previous^.next := current^.next;
            END (*IF*);

            (* For some strange reason, OS/2 will not let you close an  *)
            (* event semaphore that has an outstanding post count.      *)
            (* Of course for a dying task the state of the semaphore    *)
            (* is of no importance.                                     *)

            errno := OS2.DosResetEventSem (current^.WakeUp, postcount);
            (*NoteSemOperation (sem_reset, current^.WakeUp, MyID, errno);*)
            IF (errno <> 0) AND (errno <> OS2.ERROR_ALREADY_RESET) THEN
                SemError (eventsem, errno);
            END (*IF*);
            errno := OS2.DosCloseEventSem (current^.WakeUp);
            (*NoteSemOperation (sem_close, current^.WakeUp, MyID, errno);*)

            (* The retry in the statement below doesn't seem to do      *)
            (* anything to solve the problem.                           *)

            IF errno = OS2.ERROR_SEM_BUSY THEN
                (* Try again. *)
                errno := OS2.DosCloseEventSem (current^.WakeUp);
                (*NoteSemOperation (sem_close, current^.WakeUp, MyID, errno);*)
            END (*IF*);
            IF errno <> 0 THEN
                SemError (eventsem, errno);
            END (*IF*);

            (*
            IF current = lastTask THEN
               lastthread := 0;  lastTask := NIL;
            END (*IF*);
            *)
            (*NoteThreadOperation (thr_exit, current^.threadnum, current^.name);*)
            DISPOSE (current);

        END (*IF*);

        DEC (NumberOfThreads);
        UnlockTaskList;

        Processes.StopMe;

    END TaskExit;

(************************************************************************)
(*                        IDENTIFYING A TASK                            *)
(************************************************************************)

PROCEDURE CurrentTaskID(): TaskID;

    (* Returns the TaskID of the calling task. *)

    VAR ptib: OS2.PTIB;  ppib: OS2.PPIB;

    BEGIN
        OS2.DosGetInfoBlocks (ptib, ppib);
        RETURN ptib^.tib_ptib2^.tib2_ultid;
        (*RETURN ptib^.tib_ordinal;*)
    END CurrentTaskID;

(************************************************************************)

PROCEDURE DescriptorOf (thread: TaskID): Task;

    (* Returns the task descriptor corresponding to the given TaskID. *)

    VAR result: Task;

    BEGIN
        LockTaskList;
        (*
        IF thread = lastthread THEN
            result := lastTask;
        ELSE
        *)
            result := MasterTaskList;
            WHILE (result <> NIL) AND (result^.threadnum <> thread) DO
                result := result^.next;
            END (*WHILE*);
            (*
            lastthread := thread;
            lastTask := result;
        END (*IF*);
        *)
        UnlockTaskList;
        RETURN result;
    END DescriptorOf;

(************************************************************************)
(*                LOCKS FOR CRITICAL SECTION PROTECTION                 *)
(************************************************************************)

PROCEDURE CreateLock (VAR (*OUT*) L: Lock);

    (* Creates a new lock. *)

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosCreateMutexSem (NIL, L, 0, FALSE);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END CreateLock;

(************************************************************************)

PROCEDURE DestroyLock (VAR (*INOUT*) L: Lock);

    (* Disposes of a lock. *)

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosCloseMutexSem (L);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END DestroyLock;

(************************************************************************)

PROCEDURE Obtain (L: Lock);

    (* Obtains lock L, waiting if necessary. *)

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosRequestMutexSem (L, OS2.SEM_INDEFINITE_WAIT);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END Obtain;

(************************************************************************)

PROCEDURE Release (L: Lock);

    (* Releases lock L - which might unblock some other task. *)

    VAR errno: CARDINAL;

    BEGIN
        errno := OS2.DosReleaseMutexSem (L);
        IF errno <> 0 THEN
            SemError (mutexsem, errno);
        END (*IF*);
    END Release;

(************************************************************************)
(*                 SUSPENDING AND RESUMING A TASK                       *)
(************************************************************************)

PROCEDURE SuspendMe (id: TaskID;  TimeLimit: CARDINAL): BOOLEAN;

    (* Suspends the caller.  A TRUE result indicates that the time      *)
    (* limit expired without the task being woken up.                   *)

    VAR T: Task;  status: CARDINAL;  (* PostCount: CARDINAL; *)
        TimedOut: BOOLEAN;

    BEGIN
        T := DescriptorOf (id);
        IF T = NIL THEN
            TimedOut := FALSE;
            Processes.StopMe;
        ELSE
            (*T^.active := FALSE;*)
            (*NoteSemOperation (sem_wait, T^.WakeUp, T^.threadnum, 0);*)
            status := OS2.DosWaitEventSem (T^.WakeUp, TimeLimit);
            (*NoteThreadOperation (thr_awake, T^.threadnum, T^.name);*)
            (*T^.active := TRUE;*)
            TimedOut := status = OS2.ERROR_TIMEOUT;
            IF NOT TimedOut THEN

                (*
                (* NOTE THAT THIS CODE SECTION IS NOW COMMENTED OUT *)

                IF status = 0 THEN

                    (* This reset should now be redundant, because we   *)
                    (* are resetting as soon as we post.                *)

                    status := OS2.DosResetEventSem (T^.WakeUp, PostCount);
                    IF status = OS2.ERROR_ALREADY_RESET THEN
                        PostCount := 0;
                        status := 0;
                    END (*IF*);
                    IF PostCount > 1 THEN
                        LockScreen;
                        WriteString ("Post count = ");
                        WriteCard (PostCount);
                        WriteString (" for wakeup semaphore in SuspendMe");
                        WriteLn;
                        UnlockScreen;
                        PtrTo0^ := 0;
                    END (*IF*);
                END (*IF*);
                *)

                IF status = 95 THEN
                    (* Interrupt during the wait. I don't see why this  *)
                    (* should be called an error.                       *)

                    status := 0;
                END (*IF*);

                IF status <> 0 THEN
                    SemError (eventsem, status);
                END (*IF*);

            END (*IF*);
        END (*IF*);
        RETURN TimedOut;
    END SuspendMe;

(************************************************************************)

PROCEDURE ResumeTask (id: TaskID): BOOLEAN;

    (* Resumes a task specified by its thread ID.                       *)
    (* The function result is normally TRUE, but is FALSE if the task   *)
    (* couldn't be resumed (usually because that task no longer exists).*)

    VAR T: Task;  (*Me: CARDINAL;*)  status, PostCount: CARDINAL;

    BEGIN
        LockTaskList;
        T := DescriptorOf (id);
        (*Me := CurrentTaskID();*)
        IF T = NIL THEN
            UnlockTaskList;
            RETURN FALSE;
            (*
        ELSIF T^.active THEN

            (* This condition is occasionally occurring, and I can't    *)
            (* work out why.  Should I wake up the task anyway, or      *)
            (* should I ignore the "resume"?                            *)

            LockScreen;
            WriteString ("ResumeTask called by ");
            WriteString (Me^.name);
            WriteString (" to resume ");
            WriteString (T^.name);
            WriteLn;
            UnlockScreen;
            (*Crash ("Attempt to resume active task");*)
            *)

        END (*IF*);
        status := OS2.DosPostEventSem (T^.WakeUp);
        (*NoteSemOperation (sem_post, T^.WakeUp, Me, status);*)
        IF status <> 0 THEN
            SemError (eventsem, status);
        END (*IF*);
        status := OS2.DosResetEventSem (T^.WakeUp, PostCount);
        (*NoteSemOperation (sem_reset, T^.WakeUp, Me, status);*)
        IF status = OS2.ERROR_ALREADY_RESET THEN
            PostCount := 0;
            status := 0;
        END (*IF*);

        (* We probably don't need to be doing this check on the post    *)
        (* count, because we are no longer having a problem of this     *)
        (* nature, but I'll leave the code in for a while.              *)

        IF PostCount > 1 THEN
            LockScreen;
            WriteString ("Post count = ");
            WriteCard (PostCount);
            WriteString (" for wakeup semaphore in ResumeTask");
            WriteLn;
            UnlockScreen;
            PtrTo0^ := 0;
        END (*IF*);
        IF status <> 0 THEN
            SemError (eventsem, status);
        END (*IF*);
        UnlockTaskList;
        RETURN TRUE;
    END ResumeTask;

(************************************************************************)
(*                        MODULE INITIALISATION                         *)
(************************************************************************)

PROCEDURE CreateMainTaskDescriptor;

    (* Creates a task descriptor for the caller, which is of course     *)
    (* the main task.  Because of difficulties related to the order     *)
    (* of execution of startup code, we do not enable exceptq support   *)
    (* for the main task.                                               *)

    VAR T: Task;  errno: CARDINAL;

    BEGIN
        T := NewTaskDescriptor ("*MAIN*");
        LockTaskList;
        T^.threadnum := CurrentTaskID();
        errno := OS2.DosCreateEventSem (NIL, T^.WakeUp, 0, FALSE);
        (*NoteSemOperation (sem_creat, T^.WakeUp, T^.threadnum, errno);*)
        IF errno <> 0 THEN
            SemError (eventsem, errno);
        END (*IF*);
        INC (NumberOfThreads);
        UnlockTaskList;
    END CreateMainTaskDescriptor;

(************************************************************************)

VAR errno: CARDINAL;

BEGIN
    (*StartDebugLogging (FALSE, TRUE);*)
    errno := OS2.DosCreateMutexSem (NIL, TaskListAccess, 0, FALSE);
    IF errno <> 0 THEN
        SemError (mutexsem, errno);
    END (*IF*);
    MaxNumberOfThreads := 256;
    NumberOfThreads := 0;
    MasterTaskList := NIL;
    CreateMainTaskDescriptor;
END TaskControl.

