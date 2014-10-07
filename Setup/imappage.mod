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

IMPLEMENTATION MODULE IMAPPage;

        (********************************************************)
        (*                                                      *)
        (*                  PM Setup for Weasel                 *)
        (*            The IMAP page of the notebook             *)
        (*                                                      *)
        (*        Started:        25 March 2005                 *)
        (*        Last edited:    10 July 2013                  *)
        (*        Status:         OK                            *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT ADDRESS, INT16, CAST, ADR;

IMPORT OS2, OS2RTL;

IMPORT DID, CommonSettings, Strings;

FROM SUPage1 IMPORT
    (* proc *)  GetIMAPParameters, SetIMAPParameters;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

IMPORT RINIData;

FROM RINIData IMPORT
    (* proc *)  INIPut, INIFetch, INIGetCard, INIGetString, INIPutString;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL;

(************************************************************************)

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;
    ThreeCard = ARRAY [0..2] OF CARDINAL;
    FourCard = ARRAY [0..3] OF CARDINAL;

VAR
    ChangeInProgress: BOOLEAN;
    FirstTime: BOOLEAN;
    OldIMAPTransLevel: CARDINAL;
    OurPageID: CARDINAL;
    OldIMAPLogFileName: FilenameString;

    (* Handle to the window that belongs to this page.  We can save     *)
    (* this as a static value because there is never more than one      *)
    (* instance of this page.                                           *)

    OurPageHandle, hwndParent: OS2.HWND;
    notebookhandle: OS2.HWND;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "IMAP.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Page1.Port", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPPortLabel, stringval);
        StrToBuffer (lang, "Page1.Timeout", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPTimeoutLabel, stringval);
        StrToBuffer (lang, "Page1.MaxUsers", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPMaxLabel, stringval);
        StrToBuffer (lang, "Page1.Enabled", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPEnabled, stringval);
        (*OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPLogEnable, stringval);*)
        StrToBuffer (lang, "IMAP.LogGroup", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPLogGroup, stringval);
        StrToBuffer (lang, "Logging.Disk", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPLogToDisk, stringval);
        StrToBuffer (lang, "Logging.Screen", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPLogToScreen, stringval);
        StrToBuffer (lang, "Logging.TransLogName", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPLogLabel, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE LoadBasicParameters (hwnd: OS2.HWND);

    VAR port, timeout, maxusers: CARDINAL;
        enable: BOOLEAN;

    BEGIN
        GetIMAPParameters (port, timeout, maxusers, enable);
        OS2.WinSetDlgItemShort (hwnd, DID.IMAPPortField, port, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.IMAPTimeout, timeout, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.IMAPMaxUsers, maxusers, FALSE);
        OS2.WinSendDlgItemMsg (hwnd, DID.IMAPEnabled, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(enable)), NIL);
    END LoadBasicParameters;

(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on this page with data from the INI  *)
    (* file, or loads default values if they're not in the INI file.    *)

    VAR val0: CARDINAL;
        name: FilenameString;

    BEGIN
        LoadBasicParameters (hwnd);

        OpenINIFile;

        (* IMAP transaction logging. *)

        IF INIGetCard ('$SYS', 'IMAPTransLevel', val0) THEN
            OldIMAPTransLevel := val0;
        ELSE
            val0 := 2;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.IMAPLogToDisk, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.IMAPLogToScreen, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);

        IF INIGetString ('$SYS', 'IMAPLogFileName', name) THEN
            OldIMAPLogFileName := name;
        ELSE
            OldIMAPLogFileName := '';
            name := 'IMAP4.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.IMAPLogFileName, name);

        CloseINIFile;

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(************************************************************************)

PROCEDURE StoreBasicParameters (hwnd: OS2.HWND);

    (* Stores the information that has to be passed to page 1. *)

    VAR port, timeout, maxusers: CARDINAL;  temp: INT16;
        enable: BOOLEAN;

    BEGIN
        (* Server port. *)

        OS2.WinQueryDlgItemShort (hwnd, DID.IMAPPortField, temp, FALSE);
        port := temp;

        (* Timeout values. *)

        OS2.WinQueryDlgItemShort (hwnd, DID.IMAPTimeout, temp, FALSE);
        timeout := temp;

        (* Maximum number of users. *)

        OS2.WinQueryDlgItemShort (hwnd, DID.IMAPMaxUsers, temp, FALSE);
        maxusers := temp;

        (* "Service enabled" flags. *)

        enable := QueryButton (hwnd, DID.IMAPEnabled) > 0;

        (* Send the above information to another module. *)

        SetIMAPParameters (port, timeout, maxusers, enable);

    END StoreBasicParameters;

(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on this page back into the INI file.  Some of  *)
    (* this data is not stored directly; we pass it on to the           *)
    (* Page1 module so that it can be combined with its information.    *)

    VAR val: CARDINAL;
        filename: FilenameString;

    BEGIN
        StoreBasicParameters (hwnd);

        (* IMAP transaction logging. *)

        OpenINIFile;

        val := QueryButton (hwnd, DID.IMAPLogToDisk)
                  + 2 * QueryButton (hwnd, DID.IMAPLogToScreen);
        IF val <> OldIMAPTransLevel THEN
            INIPut ('$SYS', 'IMAPTransLevel', val);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.IMAPLogFileName, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldIMAPLogFileName) THEN
            INIPutString ('$SYS', 'IMAPLogFileName', filename);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode: CARDINAL;
        ItemID: CARDINAL;

    BEGIN

        IF msg = OS2.WM_INITDLG THEN

            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadValues (hwnd);
            StoreBasicParameters (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ItemID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF (NotificationCode = OS2.EN_CHANGE) AND
                  ((ItemID = DID.IMAPPortField) OR (ItemID = DID.IMAPTimeout)
                             OR (ItemID = DID.IMAPMaxUsers)) THEN
                StoreBasicParameters (hwnd);
            ELSIF (NotificationCode = OS2.BN_CLICKED)
                                    AND (ItemID = DID.IMAPEnabled) THEN
                StoreBasicParameters (hwnd);
            END (*IF*);
            RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, CommonSettings.MainNotebook);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSE    (* default *)
            RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;
                            VAR (*INOUT*) AfterPage: CARDINAL): OS2.HWND;

    (* Creates this page and adds it to the notebook, also returns *)
    (* the page ID of the new page in the second parameter.        *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        notebookhandle := notebook;
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.IMAPPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)

        IF AfterPage = 0 THEN
            OurPageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        ELSE
            OurPageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         CAST (ADDRESS, AfterPage),
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_NEXT)));
        END (*IF*);

        Label := "IMAP";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,OurPageID), CAST(ADDRESS,OurPageHandle));
        AfterPage := OurPageID;
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

PROCEDURE Close (hwnd: OS2.HWND);

    (* Shuts down this window and removes it from the notebook. *)

    BEGIN
        OS2.WinSendMsg (notebookhandle, OS2.BKM_DELETEPAGE,
                        CAST(ADDRESS, OurPageID),
                        OS2.MPFROMLONG (OS2.BKA_SINGLE));
        StoreData(hwnd);
        OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
    END Close;

(**************************************************************************)
(*                          MODULE INITIALISATION                         *)
(**************************************************************************)

PROCEDURE MigrateOldSettings;

    (* Updates obsolete settings. *)

    VAR NextUIDValidity: CARDINAL;

    BEGIN
        IF RINIData.OpenINIFile ("IMAP.ini", FALSE) THEN
            IF INIFetch ("$SYS", "NextUIDValidity", NextUIDValidity) THEN
                RINIData.INIDeleteKey ('$SYS', 'NextUIDValidity');
                RINIData.CloseINIFile;
                OpenINIFile;
                INIPut ("$SYS", "NextUIDValidity", NextUIDValidity);
                CloseINIFile;
            ELSE
                RINIData.CloseINIFile;
            END (*IF*);
            EVAL (RINIData.DeleteFile ("IMAP.ini"));
        END (*IF*);
    END MigrateOldSettings;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    FirstTime := FALSE;
    OldIMAPTransLevel := 0;
    OurPageHandle := OS2.NULLHANDLE;
    hwndParent := OS2.NULLHANDLE;
    MigrateOldSettings;
END IMAPPage.

