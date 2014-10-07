(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
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

IMPLEMENTATION MODULE Filter;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*               The filter page of the notebook                *)
        (*                                                              *)
        (*        Started:        19 June 2003                          *)
        (*        Last edited:    24 June 2004                          *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


(*
FROM Debug IMPORT DebugLine;
FROM TNIData IMPORT DebugDump;
*)

FROM SYSTEM IMPORT INT16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings, Strings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIPut, INIFetch, INIPutString, INIGetString, INIDeleteKey;

(**************************************************************************)

CONST
    MaxFilterNum = 4;

TYPE
    NameString = ARRAY [0..511] OF CHAR;
    KeyArray = ARRAY [0..MaxFilterNum] OF ARRAY [0..11] OF CHAR;
    IDArray = ARRAY [0..MaxFilterNum] OF CARDINAL;

CONST
    FilterProgKey = KeyArray {'FilterProg0', 'FilterProg1', 'FilterProg2',
                              'FilterProg3', 'FilterProg4'};
    FilterProgID = IDArray {DID.FilterProg0, DID.FilterProg1, DID.FilterProg2,
                            DID.FilterProg3, DID.FilterProg4};

VAR
    ChangeInProgress: BOOLEAN;
    OldSerialiseFilters: BOOLEAN;
    OurPageID: CARDINAL;
    OurPageHandle, notebookhandle: OS2.HWND;
    OldFilterProg: ARRAY [0..MaxFilterNum] OF NameString;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Filter.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Filter.FilterBox", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.FilterBox, stringval);
        StrToBuffer (lang, "Filter.Filter0label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Filter0label, stringval);
        StrToBuffer (lang, "Filter.Filter1label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Filter1label, stringval);
        StrToBuffer (lang, "Filter.Filter2label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Filter2label, stringval);
        StrToBuffer (lang, "Filter.Filter3label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Filter3label, stringval);
        StrToBuffer (lang, "Filter.Filter4label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Filter4label, stringval);
        StrToBuffer (lang, "Filter.Serialise", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.SerialiseFilters, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file, *)
    (* or loads default values if they're not in the INI file.            *)

    (**********************************************************************)

    PROCEDURE LoadCheckbox (boxid: CARDINAL;  VAR (*OUT*) val: BOOLEAN;
                                                   INIlabel: ARRAY OF CHAR);

        BEGIN
            IF NOT INIFetch ('$SYS', INIlabel, val) THEN
                val := TRUE;
            END (*IF*);
            OS2.WinSendDlgItemMsg (hwnd, boxid, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(val)), NIL);
        END LoadCheckbox;

    (**********************************************************************)

    VAR stringval: NameString;
        k: CARDINAL;

    BEGIN
        (*
        DebugLine ("Entering Filter.LoadValues");
        DebugDump ("$SYS", "FilterProg4");
        *)
        OpenINIFile;

        (*DebugDump ("$SYS", "FilterProg4");*)

        (* Upgrade from old filter option. *)

        IF INIGetString ('$SYS', 'FilterProg', stringval) THEN
            INIPutString ('$SYS', 'Debug', stringval);
            INIDeleteKey ('$SYS', 'FilterProg');
            INIPutString ('$SYS', FilterProgKey[4], stringval);
        END (*IF*);

        (*
        DebugLine ("Done handling old filter option");
        DebugDump ("$SYS", "FilterProg4");
        *)

        (* Filter program. *)

        LoadCheckbox (DID.SerialiseFilters, OldSerialiseFilters, 'SerialiseFilters');

        FOR k := 0 TO MaxFilterNum DO
            IF NOT INIGetString ('$SYS', FilterProgKey[k], stringval) THEN
                stringval := "";
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, FilterProgID[k], stringval);
            OldFilterProg[k] := stringval;
        END (*FOR*);

        CloseINIFile;
        (*DebugLine ("Leaving Filter.LoadValues");*)

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData;

    (* Stores the values on this page back into the INI file. *)

    VAR hwnd: OS2.HWND;

    (**********************************************************************)

    PROCEDURE StoreCheckbox (boxid: CARDINAL;  val: BOOLEAN;
                                                   INIlabel: ARRAY OF CHAR);

        VAR bool: BOOLEAN;

        BEGIN
            bool := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, boxid,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0;
            IF bool <> val THEN
                INIPut ('$SYS', INIlabel, bool);
            END (*IF*);
        END StoreCheckbox;

    (**********************************************************************)

    CONST Nul = CHR(0);

    VAR stringval: NameString;  j, m: CARDINAL;

    BEGIN
        hwnd := OurPageHandle;
        OpenINIFile;

        (* Filter program. *)

        StoreCheckbox (DID.SerialiseFilters, OldSerialiseFilters, 'SerialiseFilters');
        FOR m := 0 TO MaxFilterNum DO
            OS2.WinQueryDlgItemText (hwnd, FilterProgID[m], 512, stringval);
            WHILE stringval[0] = ' ' DO
                Strings.Delete (stringval, 0, 1);
            END (*WHILE*);
            j := Strings.Length (stringval);
            LOOP
                IF j = 0 THEN EXIT(*LOOP*) END(*IF*);
                DEC (j);
                IF stringval[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                stringval[j] := Nul;
            END (*LOOP*);
            IF NOT Strings.Equal (stringval, OldFilterProg[m]) THEN
                INIPutString ('$SYS', FilterProgKey[m], stringval);
            END (*IF*);
        END (*FOR*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadValues (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL);

    (* Creates the filter page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.FilterPage,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OurPageID := PageID;
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,OurPageHandle));
    END CreatePage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (OurPageHandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    OurPageHandle := OS2.NULLHANDLE;
END Filter.

