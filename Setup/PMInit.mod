(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE PMInit;

        (****************************************************************)
        (*                                                              *)
        (*        Initialisation code for a PM application              *)
        (*                                                              *)
        (*        Started:        17 January 2002                       *)
        (*        Last edited:    17 January 2002                       *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2;

(**************************************************************************)

VAR
    hab           : OS2.HAB;      (* Main anchor block handle     *)
    hmq           : OS2.HMQ;      (* Main message queue handle    *)

(**************************************************************************)

PROCEDURE OurHab(): OS2.HAB;

    (* Returns this program's anchor block handle. *)

    BEGIN
        RETURN hab;
    END OurHab;

(**************************************************************************)

PROCEDURE [OS2.APIENTRY] MessageBox(hwndOwner : OS2.HWND
                                   ;szText    : ARRAY OF CHAR
                                   ;fsStyle   : OS2.ULONG
                                   ;fBeep     : OS2.BOOL
                                   ): OS2.ULONG;

    BEGIN

        IF fBeep THEN
           OS2.WinAlarm(OS2.HWND_DESKTOP, OS2.WA_ERROR);
        END;

        RETURN OS2.WinMessageBox (OS2.HWND_DESKTOP, hwndOwner, szText,
                                  NIL, 0, fsStyle);
    END MessageBox;

(**************************************************************************)

PROCEDURE WarningBox (owner: OS2.HWND;  text: ARRAY OF CHAR);

    (* Pops up a warning message, with beep. *)

    BEGIN
        OS2.WinAlarm(OS2.HWND_DESKTOP, OS2.WA_ERROR);
        OS2.WinMessageBox (OS2.HWND_DESKTOP, owner, text,
                                  "Warning", 0, OS2.MB_INFORMATION);
    END WarningBox;

(**************************************************************************)
(*                             FINALISATION                               *)
(**************************************************************************)

PROCEDURE ["SysCall"] ExitProc (ulTermCode : OS2.ULONG);

    BEGIN
        OS2.WinDestroyMsgQueue (hmq);
        OS2.WinTerminate (hab);
        OS2.DosExitList (OS2.EXLST_EXIT, NIL);    (* termination complete *)
    END ExitProc;

(************************************************************************)
(*                             INITIALISATION                           *)
(************************************************************************)

CONST
    BEEP_WARN_FREQ =  60; (* frequency of warning beep *)
    BEEP_WARN_DUR  = 100; (* duration of warning beep *)

BEGIN
    hab := OS2.WinInitialize(0);

    IF hab = OS2.NULLHANDLE THEN
       OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
       HALT(1);
    END;

    hmq := OS2.WinCreateMsgQueue (hab, 0);

    IF hmq = OS2.NULLHANDLE THEN
        OS2.DosBeep(BEEP_WARN_FREQ, BEEP_WARN_DUR);
        OS2.WinTerminate(hab);
        HALT(1);
    END (*IF*);

    (* Add ExitProc to the exit list to handle the exit processing.  If *)
    (* there is an error, then terminate the process since there have   *)
    (* not been any resources allocated yet.                            *)

    IF OS2.DosExitList(OS2.EXLST_ADD, ExitProc) <> 0 THEN
        MessageBox (OS2.HWND_DESKTOP ,"Cannot load exit list",
                               OS2.MB_OK + OS2.MB_ERROR, TRUE);
        OS2.DosExit(OS2.EXIT_PROCESS, 1);
    END (*IF*);

END PMInit.
