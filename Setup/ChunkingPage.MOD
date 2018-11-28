(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
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

IMPLEMENTATION MODULE ChunkingPage;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                 Chunking page of the notebook                *)
        (*                                                              *)
        (*        Started:        10 August 2018                        *)
        (*        Last edited:    10 August 2018                        *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIFetch, INIPut, INIDeleteKey;

FROM Misc IMPORT
    (* proc *)  WinSetDlgItemCard, WinQueryDlgItemCard;

(**************************************************************************)

VAR
    OurLang: LangHandle;
    OurHandle, notebookhandle: OS2.HWND;
    ChangeInProgress: BOOLEAN;
    PageID: CARDINAL;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Chunking.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Chunking.max.Label", stringval);
        OS2.WinSetDlgItemText (OurHandle, DID.maxchunksizelabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                 MOVING DATA TO AND FROM THE INI FILE                 *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on this page with data from the INI  *)
    (* file, or loads default values if they're not in the INI file.    *)

    VAR size: CARDINAL;

    BEGIN
        OpenINIFile;

        (* The local "strict checking" checkbox, and the chunking special field. *)

        IF NOT INIFetch ("$SYS", "maxchunksize", size) THEN
            size := 8;
        END (*IF*);
        WinSetDlgItemCard (hwnd, DID.maxchunksize, size);

        CloseINIFile;

    END LoadValues;

(************************************************************************)

PROCEDURE StoreData;

    (* Stores back data for this page. *)

    VAR size: CARDINAL;

    BEGIN
        OpenINIFile;

        (* Save the max chunk size. *)

        WinQueryDlgItemCard (OurHandle, DID.maxchunksize, size);
        INIPut ("$SYS", "maxchunksize", size);

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
        END (*IF*);

        IF msg = OS2.WM_PRESPARAMCHANGED THEN

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

PROCEDURE RemoveObsoleteINIdata;

    (* Deletes some INI data that are no longer relevant. *)

    BEGIN
        INIDeleteKey ("$SYS", "Chunking");
    END RemoveObsoleteINIdata;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) pageID: CARDINAL);

    (* Creates the chunking page and adds it to the notebook. *)

    VAR pagehandle: OS2.HWND;
        Label: ARRAY [0..127] OF CHAR;

    BEGIN
        RemoveObsoleteINIdata;
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.Chunking,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
        OurHandle := pagehandle;
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        StrToBuffer (OurLang, "Chunking.tab", Label);
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        pageID := PageID;
    END CreatePage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (OurHandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    OurHandle := OS2.NULLHANDLE;
    ChangeInProgress := FALSE;
END ChunkingPage.

