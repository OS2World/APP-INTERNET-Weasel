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

IMPLEMENTATION MODULE CircularBuffers;

        (********************************************************)
        (*                                                      *)
        (*      Circular Buffers for passing character data     *)
        (*      between a pair of tasks.                        *)
        (*                                                      *)
        (*      Author:         P. Moylan                       *)
        (*      Last edited:    1 March 1998                    *)
        (*      Status:         OK                              *)
        (*                                                      *)
        (********************************************************)

FROM Storage IMPORT
    (* proc *)  ALLOCATE;

FROM Timer IMPORT
    (* proc *)  TimedWait;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  CreateSemaphore, Wait, Signal;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

(************************************************************************)

CONST MaxChars = 65536;

TYPE
    BufferTemplate = RECORD
                        SlotAvailable, DataPresent: Semaphore;
                        lastlocation: CARDINAL;
                        mutex: Lock;
                        GetPlace, PutPlace: CARDINAL;
                        empty: BOOLEAN;
                        data: ARRAY [0..MaxChars-1] OF CHAR
                     END;

    CircularBuffer = POINTER TO BufferTemplate;

(************************************************************************)

PROCEDURE CreateBuffer (VAR (*OUT*) B: CircularBuffer;  size: CARDINAL);

    (* Allocates space for a circular buffer, and initializes it.  The  *)
    (* caller specifies how many characters the buffer will hold.       *)

    BEGIN
        ALLOCATE (B, SIZE(BufferTemplate) - SIZE(CHAR) * (MaxChars - size));
        WITH B^ DO
            lastlocation := size - 1;
            CreateSemaphore (SlotAvailable, size);
            CreateSemaphore (DataPresent, 0);
            CreateLock (mutex);
            GetPlace := 0;  PutPlace := 0;
            empty := TRUE;
        END (*WITH*);
    END CreateBuffer;

(************************************************************************)

PROCEDURE PutBuffer (B: CircularBuffer; item: CHAR);

    (* Puts item into the circular buffer, waiting for space available  *)
    (* if necessary.                                                    *)

    BEGIN
        WITH B^ DO
            Wait (SlotAvailable);
            data[PutPlace] := item;
            IF PutPlace = lastlocation THEN PutPlace := 0
            ELSE INC (PutPlace);
            END (*IF*);
            empty := FALSE;
            Signal (DataPresent);
        END (*WITH*);
    END PutBuffer;

(************************************************************************)

PROCEDURE PutBufferImpatient (B: CircularBuffer;  item: CHAR;
                                                TimeLimit: CARDINAL);

    (* Like PutBuffer, but waits no longer than TimeLimit milliseconds  *)
    (* for a buffer slot to become available.  If the time limit        *)
    (* expires, the oldest item in the buffer is overwritten by the     *)
    (* new data.                                                        *)

    VAR TimedOut: BOOLEAN;

    BEGIN
        WITH B^ DO
            TimedWait (SlotAvailable, TimeLimit, TimedOut);
            IF TimedOut THEN
                Obtain (mutex);
                IF GetPlace = lastlocation THEN GetPlace := 0
                ELSE INC (GetPlace);
                END (*IF*);
                Release (mutex);
            END (*IF*);
            data[PutPlace] := item;
            IF PutPlace = lastlocation THEN PutPlace := 0
            ELSE INC (PutPlace);
            END (*IF*);
            empty := FALSE;
            IF NOT TimedOut THEN
                Signal (DataPresent);
            END (*IF*);
        END (*WITH*);
    END PutBufferImpatient;

(************************************************************************)

PROCEDURE InsertAtFront (B: CircularBuffer;  item: CHAR);

    (* Inserts an item at the head of the queue, rather than at the     *)
    (* tail as usual.  If the buffer is full, the most recently added   *)
    (* item is lost.                                                    *)

    VAR SpaceWasAvailable: BOOLEAN;

    BEGIN
        WITH B^ DO
            Obtain (mutex);
            SpaceWasAvailable := empty OR (GetPlace <> PutPlace);
            IF GetPlace = 0 THEN GetPlace := lastlocation
            ELSE DEC (GetPlace);
            END (*IF*);
            data[GetPlace] := item;
            empty := FALSE;
            Release (mutex);
            IF SpaceWasAvailable THEN
                Signal (DataPresent);
            END (*IF*);
        END (*WITH*);
    END InsertAtFront;

(************************************************************************)

PROCEDURE GetBuffer (B: CircularBuffer) : CHAR;

    (* Gets one character from the circular buffer, waiting when        *)
    (* necessary for a character to become available.                   *)

    VAR result: CHAR;

    BEGIN
        WITH B^ DO
            Wait (DataPresent);
            Obtain (mutex);
            result := data[GetPlace];
            IF GetPlace = lastlocation THEN GetPlace := 0
            ELSE INC (GetPlace);
            END (*IF*);
            empty := GetPlace = PutPlace;
            Release (mutex);
            Signal (SlotAvailable);
        END (*WITH*);
        RETURN result;
    END GetBuffer;

(************************************************************************)

PROCEDURE BufferEmpty (B: CircularBuffer): BOOLEAN;

    (* Returns TRUE iff the buffer is empty. *)

    (*VAR TimedOut: BOOLEAN;*)

    BEGIN
        RETURN B^.empty;
        (*
        WITH B^ DO
            TimedWait (DataPresent, 0, TimedOut);
            IF TimedOut THEN RETURN TRUE;
            ELSE
                Signal (DataPresent);
                RETURN FALSE;
            END (*IF*);
        END (*WITH*);
        *)
    END BufferEmpty;

(************************************************************************)

END CircularBuffers.

