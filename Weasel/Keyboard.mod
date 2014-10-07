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

IMPLEMENTATION MODULE Keyboard;

        (****************************************************************)
        (*                                                              *)
        (*                      Keyboard Input                          *)
        (*                                                              *)
        (*  Programmer:         P. Moylan                               *)
        (*  Last edited:        22 December 2013                        *)
        (*  Status:             Working                                 *)
        (*                                                              *)
        (****************************************************************)

(************************************************************************)
(*                                                                      *)
(*  To allow the keyboard user to type ahead, this module contains a    *)
(*  task which puts characters into a circular buffer, where they are   *)
(*  kept until picked up by a call to InKey.  (There are already        *)
(*  type-ahead facilities in the operating system, and also in the      *)
(*  keyboard hardware itself; but doing things this way makes it        *)
(*  easier to provide a "hot key" facility.)                            *)
(*                                                                      *)
(*  As a protection against deadlock, there is a timeout on the         *)
(*  "circular buffer full" condition.  If the buffer remains full for   *)
(*  too long, the oldest character in the buffer is discarded to make   *)
(*  room for the newest character.                                      *)
(*                                                                      *)
(************************************************************************)

FROM SYSTEM IMPORT
    (* type *)  CARD8,
    (* proc *)  ADR;

IMPORT OS2;

FROM Storage IMPORT
    (* proc *)  DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL, IAND, IOR, IANDB, IORB, ALLOCATE64;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM CircularBuffers IMPORT
    (* type *)  CircularBuffer,
    (* proc *)  CreateBuffer, PutBufferImpatient, GetBuffer, BufferEmpty,
                InsertAtFront;

(************************************************************************)
(*                      THE 'HOT KEY' TABLES                            *)
(************************************************************************)

TYPE CharSet = SET OF CHAR;

VAR HotKeys, HotFunctionKeys: CharSet;
    HotKeySemaphore: ARRAY CHAR OF Semaphore;
    HotFunctionKeySemaphore: ARRAY CHAR OF Semaphore;

(************************************************************************)
(*                      THE CHARACTER BUFFER                            *)
(************************************************************************)

CONST
    CharBufferSize = 8;

VAR
    (* CharBuffer is a circular buffer holding characters.      *)

    CharBuffer: CircularBuffer;

    (* Flag that indicates that this process is running full-screen. *)

    FullScreen: BOOLEAN;

    (* A variable that is set unless the process is detached. *)

    ProcessIsNotDetached: BOOLEAN;

(************************************************************************)
(*         PUTTING KEYBOARD CHARACTERS INTO THE CIRCULAR BUFFER         *)
(************************************************************************)

PROCEDURE PutCode (FunctionKey: BOOLEAN;  code: CHAR);

    (* Puts a code into CharBuffer - unless it is a hot key, in which   *)
    (* case it is dealt with immediately.                               *)

    BEGIN
        IF FunctionKey THEN
            IF code IN HotFunctionKeys THEN
                Signal (HotFunctionKeySemaphore[code]);
            ELSE
                PutBufferImpatient (CharBuffer, CHR(0), 2000);
                PutBufferImpatient (CharBuffer, code, 2000);
            END (*IF*);
        ELSE
            IF code IN HotKeys THEN
                Signal (HotKeySemaphore[code]);
            ELSE
                PutBufferImpatient (CharBuffer, code, 2000);
            END (*IF*);
        END (*IF*);
    END PutCode;

(************************************************************************)

PROCEDURE InputTask;

    (* This procedure, which is run as a separate task, picks up the    *)
    (* keyboard input and stores it in CharBuffer.                      *)

    VAR KeyData: POINTER TO OS2.KBDKEYINFO;
        result: CHAR;

    BEGIN
        ALLOCATE64 (KeyData, SIZE(OS2.KBDKEYINFO));
        LOOP
            OS2.KbdCharIn (KeyData^, 0, 0);
            result := KeyData^.chChar;
            IF (result = CHR(0)) OR (result = CHR(224)) THEN
                PutCode (TRUE, KeyData^.chScan);
            ELSE
                PutCode (FALSE, result);
            END (*IF*);
        END (*LOOP*);
    END InputTask;

(************************************************************************)
(*              THE EXTERNALLY CALLABLE INPUT PROCEDURES                *)
(************************************************************************)

PROCEDURE KeyPressed(): BOOLEAN;

    (* Returns TRUE iff a character is available. *)

    BEGIN
        RETURN NOT BufferEmpty(CharBuffer);
    END KeyPressed;

(************************************************************************)

PROCEDURE InKey(): CHAR;

    (* Reads one key from the keyboard, or from the putback      *)
    (* buffer if any characters have been put back.              *)

    BEGIN
        RETURN GetBuffer (CharBuffer);
    END InKey;

(************************************************************************)

PROCEDURE PutBack (ch: CHAR);

    (* This is an "un-read" operation, i.e. the character ch will       *)
    (* re-appear on the next call to InKey.  This facility is provided  *)
    (* for the use of software which can overshoot by one character     *)
    (* when reading its input - a situation which can often occur.      *)

    BEGIN
        InsertAtFront (CharBuffer, ch);
    END PutBack;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer (ch: CHAR);

    (* Stores ch as if it had come from the keyboard, so that a         *)
    (* subsequent InKey() will pick it up.                              *)

    BEGIN
        PutCode (FALSE, ch);
    END StuffKeyboardBuffer;

(************************************************************************)

PROCEDURE StuffKeyboardBuffer2 (ch: CHAR);

    (* Like StuffKeyboardBuffer, but stores a two-byte sequence: Nul    *)
    (* followed by ch.                                                  *)

    BEGIN
        PutCode (TRUE, ch);
    END StuffKeyboardBuffer2;

(************************************************************************)

PROCEDURE SetLocks (code: CARDINAL);

    (* Set/clear the caps lock, num lock, and scroll lock conditions.   *)
    (* The codes are defined at the beginning of this module.           *)

    VAR KeyState: ARRAY [0..255] OF CARD8;

    (********************************************************************)

    PROCEDURE UpdateKeyState (VirtualKey: CARD8;  mask: CARDINAL);

        (* Sets or clears the low-order bit of KeyState[VirtualKey],    *)
        (* depending on the value of code.                              *)

        BEGIN
            IF IAND (code, mask) <> 0 THEN
                KeyState[VirtualKey] := IORB (KeyState[VirtualKey], 1);
            ELSE
                KeyState[VirtualKey] := IANDB (KeyState[VirtualKey], 0FEH);
            END (*IF*);
        END UpdateKeyState;

    (********************************************************************)

    CONST LockStatusMask = MAX(CARDINAL) - (CapsLock + NumLock + ScrollLock);

    VAR Action, Length: CARDINAL;
        ss: OS2.SHIFTSTATE;
        handle: OS2.HFILE;

    BEGIN
        OS2.DosOpen("KBD$", handle, Action, 0, 0, OS2.FILE_OPEN,
               OS2.OPEN_ACCESS_READONLY + OS2.OPEN_SHARE_DENYNONE, NIL);
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_GETSHIFTSTATE,
                   NIL, 0, Action, ADR(ss), SIZE(ss), Length);
        ss.fsState := IOR (code, IAND (ss.fsState, LockStatusMask));
        Action := SIZE(ss);
        Length := 0;
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_SETSHIFTSTATE,
                   ADR(ss), SIZE(ss), Action, NIL, 0, Length);

        (* The following code handles the case of a windowed VIO session. *)

        OS2.WinSetKeyboardStateTable (OS2.HWND_DESKTOP, KeyState, FALSE);
        UpdateKeyState (OS2.VK_NUMLOCK, NumLock);
        UpdateKeyState (OS2.VK_CAPSLOCK, CapsLock);
        UpdateKeyState (OS2.VK_SCRLLOCK, ScrollLock);
        OS2.WinSetKeyboardStateTable (OS2.HWND_DESKTOP, KeyState, TRUE);

        OS2.DosClose(handle);

    END SetLocks;

(************************************************************************)

PROCEDURE LockStatus (): CARDINAL;

    (* Returns the current state of the caps lock, num lock, and scroll *)
    (* lock conditions.                                                 *)
    (* The codes are defined at the beginning of this module.           *)

    CONST LockStatusMask = CapsLock + NumLock + ScrollLock;

    VAR Action, Length, result: CARDINAL;
        ss: OS2.SHIFTSTATE;
        handle: OS2.HFILE;

    BEGIN
        OS2.DosOpen("KBD$", handle, Action, 0, 0, OS2.FILE_OPEN,
               OS2.OPEN_ACCESS_READONLY + OS2.OPEN_SHARE_DENYNONE,
               NIL);
        Action := 0;
        Length := SIZE(ss);
        OS2.DosDevIOCtl(handle, OS2.IOCTL_KEYBOARD, OS2.KBD_GETSHIFTSTATE,
                   NIL, 0, Action, ADR(ss), SIZE(ss), Length);
        result := IAND (ss.fsState, LockStatusMask);
        OS2.DosClose(handle);
        RETURN result;
    END LockStatus;

(************************************************************************)

PROCEDURE HotKey (FunctionKey: BOOLEAN;  code: CHAR;  S: Semaphore);

    (* After this procedure is called, typing the key combination for   *)
    (* 'code' will cause a Signal(S).  Set FunctionKey=TRUE to trap one *)
    (* of the two-character special function keys, and FALSE otherwise. *)
    (* The character is consumed; if it should be passed on, then the   *)
    (* user's hot key handler can do a PutBack().  Note: there is no    *)
    (* provision for having multiple hot key handlers for the same key; *)
    (* any existing hot key mapping will be overridden.                 *)

    BEGIN
        IF FunctionKey THEN
            INCL (HotFunctionKeys, code);
            HotFunctionKeySemaphore[code] := S;
        ELSE
            INCL (HotKeys, code);
            HotKeySemaphore[code] := S;
        END (*IF*);
    END HotKey;

(************************************************************************)
(*                      CHECK FOR PROCESS MODE                          *)
(************************************************************************)

PROCEDURE IsFullScreen(): BOOLEAN;

    (* Returns TRUE if this is a full-screen OS/2 session. *)

    BEGIN
        RETURN FullScreen;
    END IsFullScreen;

(************************************************************************)

PROCEDURE NotDetached(): BOOLEAN;

    (* Returns TRUE unless called by a process running detached.        *)
    (* (A detached process may not do keyboard, screen, or mouse I/O.)  *)

    BEGIN
        RETURN ProcessIsNotDetached;
    END NotDetached;

(************************************************************************)

PROCEDURE DetachCheck;

    (* Sets the variable ProcessIsNotDetached. *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        FullScreen := pPib^.pib_ultype = 0;
        ProcessIsNotDetached := pPib^.pib_ultype <> 4;
    END DetachCheck;

(************************************************************************)
(*                          INITIALISATION                              *)
(************************************************************************)

BEGIN
    HotKeys := CharSet{};
    HotFunctionKeys := CharSet{};
    CreateBuffer (CharBuffer, CharBufferSize);
    DetachCheck;
    IF ProcessIsNotDetached THEN
        (*SetLocks (0);*)
        EVAL(CreateTask (InputTask, 4, "Keyboard main"));
    END (*IF*);
END Keyboard.

