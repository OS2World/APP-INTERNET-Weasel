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

IMPLEMENTATION MODULE Exceptq;

        (********************************************************************)
        (*                                                                  *)
        (*             Translation to Modula-2 of exceptq.h                 *)
        (*                                                                  *)
        (*  Programmer:         P. Moylan                                   *)
        (*  Started:            15 July 2012                                *)
        (*  Last edited:        27 December 2012                            *)
        (*  Status:             Complete, needs more testing                *)
        (*                                                                  *)
        (*                                                                  *)
        (*  Parts of this file are                                          *)
        (*    Copyright (c) 2000-2010 Steven Levine and Associates, Inc.    *)
        (*    Copyright (c) 2010-2011 Richard L Walsh                       *)
        (*                                                                  *)
        (********************************************************************)


FROM SYSTEM IMPORT CAST, ADDRESS, ADR;

IMPORT Strings;

FROM OS2 IMPORT
    (* const*)  CCHMAXPATH, APIENTRY,
    (* type *)  APIRET, HMODULE, PPIB, PTIB, PFN, EXCEPTIONREGISTRATIONRECORD,
    (* proc *)  DosLoadModule, DosGetInfoBlocks, DosQueryModuleName,
                DosQueryProcAddr, DosFreeModule, DosUnsetExceptionHandler;

FROM ProgName IMPORT
    (* proc *)  GetProgramName;

(********************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    EXCEPTIONREGISTRATIONPTR = POINTER TO EXCEPTIONREGISTRATIONRECORD;

    INSTEXQ = PROCEDURE [APIENTRY] (EXCEPTIONREGISTRATIONPTR,
                                    ADDRESS, ADDRESS): APIRET;

CONST
    NilHandler = CAST(INSTEXQ, NIL);

VAR
    LoadTried: BOOLEAN;
    fnInstall: INSTEXQ;

    (* Exceptq options. *)

    OptStr: ARRAY [0..15] OF CHAR;

    (* Program identification string. *)

    ProgName: ARRAY [0..79] OF CHAR;

(********************************************************************************)

PROCEDURE EqSetInfo (Opts: ARRAY OF CHAR;  ID: ARRAY OF CHAR);

    (* Sets two string parameters: the exceptq options, and the program         *)
    (* identification.  This procedure must be called by the lowest-level       *)
    (* module whose initialisation code starts a thread.                        *)

    BEGIN
        Strings.Assign (Opts, OptStr);
        Strings.Assign (ID, ProgName);
    END EqSetInfo;

(********************************************************************************)

PROCEDURE InstallExceptq (VAR (*IN*) ExRegRec: EXCEPTIONREGISTRATIONRECORD)
                                                                        : BOOLEAN;

    (* Installs Exceptq dynamically, for the current thread, so that the        *)
    (* application can use it without being dependent on its presence.  By      *)
    (* design, it will fail if it finds a version of Exceptq earlier than v7.0. *)

    (* Important: you must call this function on thread entry and call          *)
    (* UninstallExceptq() on thread exit, for EVERY thread including the main   *)
    (* thread.                                                                  *)

    BEGIN
        IF fnInstall = NilHandler THEN
            RETURN FALSE;

        ELSIF fnInstall (ADR(ExRegRec), ADR(OptStr), ADR(ProgName)) <> 0 THEN

            (* That call shouldn't ever fail once we know that the DLL has      *)
            (* been loaded; but to be on the safe side we clear out the         *)
            (* function address on failure to prevent future calls.             *)

            fnInstall := NilHandler;
            RETURN FALSE;

        ELSE
            RETURN TRUE;
        END (*IF*);

    END InstallExceptq;

(********************************************************************************)

PROCEDURE UninstallExceptq (VAR (*IN*) ExRegRec: EXCEPTIONREGISTRATIONRECORD);

    (* Uninstalls the exceptq handler. *)

    BEGIN
        DosUnsetExceptionHandler(ExRegRec);
    END UninstallExceptq;

(********************************************************************************)
(*                           MODULE INITIALISATION                              *)
(********************************************************************************)

PROCEDURE LoadExceptqDLL;

    (* Loads EXCEPTQ.DLL, from the LIBPATH if possible and otherwise from the   *)
    (* current executable's directory. This is an initialisation option that we *)
    (* will try only once.  If the load fails, we give the global variable      *)
    (* fnInstall the value NilHandler.                                          *)

    VAR hmod: HMODULE;
        szFailName: ARRAY [0..15] OF CHAR;
        ppib: PPIB;
        ptib: PTIB;
        szPath: ARRAY [0..CCHMAXPATH-1] OF CHAR;
        found: BOOLEAN;
        pos: CARDINAL;
        func: PFN;

    BEGIN
        (* Make only one attempt to load the dll & resolve the proc address. *)

        IF NOT LoadTried THEN

            LoadTried := TRUE;
            fnInstall := NilHandler;

            IF DosLoadModule(szFailName, SIZE(szFailName), "EXCEPTQ", hmod) > 0 THEN

                (* DLL was not found on the LIBPATH, so look for it in the     *)
                (* exe's directory (which may not be the current directory).   *)

                DosGetInfoBlocks(ptib, ppib);
                IF DosQueryModuleName (ppib^.pib_hmte, CCHMAXPATH, szPath) > 0 THEN
                    RETURN;
                END (*IF*);
                Strings.FindPrev ('\', szPath, LENGTH(szPath), found, pos);
                IF NOT found THEN
                    RETURN;
                END (*IF*);
                szPath[pos+1] := Nul;
                Strings.Append ("EXCEPTQ.DLL", szPath);
                IF (DosLoadModule(szFailName, SIZE(szFailName), szPath, hmod)) > 0 THEN
                    RETURN;
                END (*inner IF DosLoadModule*);

            END (*outer IF DosLoadModule*);

            (* If the proc address isn't found (possibly because an older   *)
            (* version of exceptq.dll was loaded), unload the dll & exit.   *)

            IF DosQueryProcAddr(hmod, 0, "InstallExceptq", func) > 0 THEN
                DosFreeModule(hmod);
                RETURN;
            END (*IF*);
            fnInstall := CAST(INSTEXQ, func);

        END (*IF*);

    END LoadExceptqDLL;

(********************************************************************************)

BEGIN
    OptStr := "I";
    ProgName := "";
    LoadTried := FALSE;
    LoadExceptqDLL;

    (* Set the program identification string.  *)

    GetProgramName (ProgName);

END Exceptq.

