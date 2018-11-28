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

IMPLEMENTATION MODULE BlackLists;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*            The 'BlackLists' page of the notebook             *)
        (*                                                              *)
        (*        Started:        29 July 2001                          *)
        (*        Last edited:    16 August 2018                        *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD8, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIPut, INIFetch, INIPutString, INIGetString;

(**************************************************************************)

CONST
    NumberOfBlackLists = 8;

TYPE
    NameString = ARRAY [0..511] OF CHAR;
    BlacklistType = [1..NumberOfBlackLists];
    idArray = ARRAY BlacklistType OF CARDINAL;
    StringArray = ARRAY BlacklistType OF NameString;

CONST
    Checkboxid = idArray {DID.blenable1, DID.blenable2, DID.blenable3,
                          DID.blenable4, DID.blenable5, DID.blenable6,
                          DID.blenable7, DID.blenable8};
    DBLCheckboxid = idArray {DID.dblenable1, DID.dblenable2, DID.dblenable3,
                          DID.dblenable4, DID.dblenable5, DID.dblenable6,
                          DID.dblenable7, DID.dblenable8};
    Domainid = idArray {DID.blackdomain1, DID.blackdomain2,
                        DID.blackdomain3, DID.blackdomain4,
                         DID.blackdomain5, DID.blackdomain6,
                          DID.blackdomain7, DID.blackdomain8};
    DBLDomainid = idArray {DID.DBLdomain1, DID.DBLdomain2,
                        DID.DBLdomain3, DID.DBLdomain4,
                         DID.DBLdomain5, DID.DBLdomain6,
                          DID.DBLdomain7, DID.DBLdomain8};
    DomainName = StringArray {'RBLDomain1', 'RBLDomain2', 'RBLDomain3',
                              'RBLDomain4', 'RBLDomain5', 'RBLDomain6',
                              'RBLDomain7', 'RBLDomain8'};
    DBLDomainName = StringArray {'DBLDomain1', 'DBLDomain2', 'DBLDomain3',
                              'DBLDomain4', 'DBLDomain5', 'DBLDomain6',
                              'DBLDomain7', 'DBLDomain8'};
    DefaultDomain = StringArray {"blackholes.mail-abuse.org",
                                 "dialups.mail-abuse.org",
                                 "relays.mail-abuse.org",
                                 "", "", "", "", ""};
    DefaultDBLDomain = StringArray {"dbl.spamhaus.org",
                                 "", "", "", "", "", "", ""};

VAR
    ChangeInProgress: BOOLEAN;
    OurPageHandle, notebookhandle: OS2.HWND;
    OldBLcheck, OldDBLcheck: CARD8;
    OldDomain, OldDBLDomain: StringArray;
    PageID: CARDINAL;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Blacklists.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Blacklists.explain", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BLHead, stringval);
        StrToBuffer (lang, "Blacklists.Note1", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BLNote1, stringval);
        StrToBuffer (lang, "Blacklists.Head1", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BLHead1, stringval);
        StrToBuffer (lang, "Blacklists.Head2", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BLHead2, stringval);
        StrToBuffer (lang, "Blacklists.Note2", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BLNote2, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR stringval: NameString;  j: BlacklistType;  val: CARD8;

    BEGIN
        OpenINIFile;

        (* Blacklist domains. *)

        IF NOT INIFetch ('$SYS', 'RBLcheck', OldBLcheck) THEN
            OldBLcheck := 0;
        END (*IF*);
        val := OldBLcheck;
        FOR j := MIN(BlacklistType) TO MAX(BlacklistType) DO
            IF ODD (val) THEN
                OS2.WinSendDlgItemMsg (hwnd, Checkboxid[j], OS2.BM_SETCHECK,
                                                 OS2.MPFROMSHORT(1), NIL);
            END (*IF*);
            val := val DIV 2;
            IF NOT INIGetString ('$SYS', DomainName[j], stringval) THEN
                stringval := "";
            END (*IF*);
            OldDomain[j] := stringval;
            IF stringval = "" THEN
                stringval := DefaultDomain[j];
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, Domainid[j], stringval);
        END (*FOR*);

        (* Domain blacklist checkers. *)

        IF NOT INIFetch ('$SYS', 'DBLcheck', OldDBLcheck) THEN
            OldDBLcheck := 0;
        END (*IF*);
        val := OldDBLcheck;
        FOR j := MIN(BlacklistType) TO MAX(BlacklistType) DO
            IF ODD (val) THEN
                OS2.WinSendDlgItemMsg (hwnd, DBLCheckboxid[j], OS2.BM_SETCHECK,
                                                 OS2.MPFROMSHORT(1), NIL);
            END (*IF*);
            val := val DIV 2;
            IF NOT INIGetString ('$SYS', DBLDomainName[j], stringval) THEN
                stringval := "";
            END (*IF*);
            OldDBLDomain[j] := stringval;
            IF stringval = "" THEN
                stringval := DefaultDBLDomain[j];
            END (*IF*);
            OS2.WinSetDlgItemText (hwnd, DBLDomainid[j], stringval);
        END (*FOR*);

        CloseINIFile;

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on the blacklist page back into the INI file. *)

    CONST Nul = CHR(0);

    VAR stringval: NameString;
        BLcheck, DBLcheck: CARD8;  k: BlacklistType;

    BEGIN
        OpenINIFile;

        (* Realtime blacklist checking. *)

        BLcheck := 0;
        FOR k := MAX(BlacklistType) TO MIN(BlacklistType) BY -1 DO
            BLcheck := 2*BLcheck;
            OS2.WinQueryDlgItemText (hwnd, Domainid[k], 512, stringval);
            IF (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, Checkboxid[k],
                                   OS2.BM_QUERYCHECK, NIL, NIL)) > 0)
                               AND (stringval[0] <> Nul) THEN
                INC (BLcheck);
            END (*IF*);
            IF NOT Strings.Equal (stringval, OldDomain[k]) THEN
                INIPutString ('$SYS', DomainName[k], stringval);
            END (*IF*);
        END (*FOR*);
        IF BLcheck <> OldBLcheck THEN
            INIPut ('$SYS', 'RBLcheck', BLcheck);
        END (*IF*);

        (* Domain name blacklist checking. *)

        DBLcheck := 0;
        FOR k := MAX(BlacklistType) TO MIN(BlacklistType) BY -1 DO
            DBLcheck := 2*DBLcheck;
            OS2.WinQueryDlgItemText (hwnd, DBLDomainid[k], 512, stringval);
            IF (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DBLCheckboxid[k],
                                   OS2.BM_QUERYCHECK, NIL, NIL)) > 0)
                               AND (stringval[0] <> Nul) THEN
                INC (DBLcheck);
            END (*IF*);
            IF NOT Strings.Equal (stringval, OldDBLDomain[k]) THEN
                INIPutString ('$SYS', DBLDomainName[k], stringval);
            END (*IF*);
        END (*FOR*);
        IF DBLcheck <> OldDBLcheck THEN
            INIPut ('$SYS', 'DBLcheck', DBLcheck);
        END (*IF*);

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

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) pgID: CARDINAL): OS2.HWND;

    (* Creates the blacklist page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,         (* dialogue procedure *)
                       0,                    (* use resources in EXE *)
                       DID.blacklists,       (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,OurPageHandle));
        pgID := PageID;
        RETURN OurPageHandle;
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
    notebookhandle := OS2.NULLHANDLE;
END BlackLists.

