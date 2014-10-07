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

IMPLEMENTATION MODULE Timer;

        (********************************************************)
        (*                                                      *)
        (*                 Timed operations                     *)
        (*                                                      *)
        (*      Author:         P. Moylan                       *)
        (*      Last edited:    21 March 2014                   *)
        (*      Status:         Working                         *)
        (*                                                      *)
        (********************************************************)

FROM OS2 IMPORT
    (* proc *)  DosSleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  TimedWaitInternal;

(************************************************************************)
(*                      "PUT-ME-TO-SLEEP" PROCEDURE                     *)
(************************************************************************)

PROCEDURE Sleep (milliseconds: CARDINAL);

    (* Puts the caller to sleep for approximately the given number of   *)
    (* milliseconds.                                                    *)

    BEGIN
        DosSleep (milliseconds);
    END Sleep;

(************************************************************************)
(*                      SEMAPHORE WAIT WITH TIMEOUT                     *)
(************************************************************************)

PROCEDURE TimedWait (s: Semaphore;  TimeLimit: INTEGER;
                                        VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like a semaphore Wait, except that it returns with TimedOut TRUE *)
    (* if the corresponding Signal does not occur within TimeLimit      *)
    (* milliseconds.                                                    *)

    BEGIN
        TimedWaitInternal (s, TimeLimit, FALSE, TimedOut);
    END TimedWait;

(************************************************************************)

PROCEDURE TimedWaitSpecial (s: Semaphore;  TimeLimit: INTEGER;
                                        VAR (*OUT*) TimedOut: BOOLEAN);

    (* Like TimedWait, but uses up all "credit" that the semaphore has  *)
    (* accumulated by possible multiple Signal operations.              *)

    (* This violates the usual semaphore property that Wait and Signal  *)
    (* operations are, in effect, paired. Thus this procedure should    *)
    (* not normally be used, although it is helpful for implementing    *)
    (* things like watchdog threads.                                    *)

    BEGIN
        TimedWaitInternal (s, TimeLimit, TRUE, TimedOut);
    END TimedWaitSpecial;

(************************************************************************)

END Timer.

