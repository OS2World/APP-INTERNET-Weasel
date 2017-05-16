(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
(*  Copyright (C) 2017   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE OptionP1;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                 Option page 1 of the notebook                *)
        (*                                                              *)
        (*        Started:        30 June 1999                          *)
        (*        Last edited:    24 March 2017                         *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT INT16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings, Strings;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  ServerIPAddress, INIPut, INIFetch,
                INIPutString, INIGetString, INIGetCard;

FROM Names IMPORT
    (* type *)  HostName, HostNameIndex;

FROM Inet2Misc IMPORT
    (* proc *)  AddressToHostName;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(**************************************************************************)

TYPE
    AuthKind = (cheat, plain, login, crammd5);
    AuthArray = ARRAY AuthKind OF CARDINAL;

CONST
    AuthDID = AuthArray {0, DID.AllowAUTHplain,
                                DID.AllowAUTHlogin, DID.AllowAUTHcrammd5};
    AuthMask = AuthArray {1, 2, 4, 8};
    DefaultAuthMethods = AuthMask[plain]+AuthMask[login]+AuthMask[crammd5];

VAR
    ChangeInProgress: BOOLEAN;
    OurPageHandle, notebookhandle: OS2.HWND;
    OldMAILFROMcheck, OldSPFenabled, OldUseFixedLocalName: BOOLEAN;
    OldBadPasswordLimit: CARDINAL;
    OldAuthTime: CARDINAL;
    OldAuthMethods: CARDINAL;
    OurPageID: CARDINAL;
    OldOurHostName: HostName;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "OptionP1.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "OptionP1.BadPasswordLimitBox", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BadPasswordLimitBox, stringval);
        StrToBuffer (lang, "Page1.Enabled", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BadPasswordLimitEnable, stringval);
        StrToBuffer (lang, "OptionP1.BadPasswordLimitLabel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BadPasswordLimitLabel, stringval);
        StrToBuffer (lang, "OptionP1.MAILFROMcheck", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.MAILFROMcheck, stringval);
        StrToBuffer (lang, "OptionP1.SPFenabled", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.SPFenabled, stringval);
        StrToBuffer (lang, "OptionP1.AuthenticationBox", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthenticationBox, stringval);
        StrToBuffer (lang, "OptionP1.EnablePOPbeforeSMTP", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.EnablePOPbeforeSMTP, stringval);
        StrToBuffer (lang, "OptionP1.minutes", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.minutes, stringval);
        StrToBuffer (lang, "OptionP1.AUTHbox", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AUTHbox, stringval);
        StrToBuffer (lang, "OptionP1.ReportOurName", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UseFixedLocalName, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file, *)
    (* or loads default values if they're not in the INI file.            *)

    (**********************************************************************)

    PROCEDURE LoadCheckbox (boxid: CARDINAL;  default: BOOLEAN;
                        VAR (*OUT*) oldval: BOOLEAN;  INIlabel: ARRAY OF CHAR);

        VAR val: BOOLEAN;

        BEGIN
            IF NOT INIFetch ('$SYS', INIlabel, val) THEN
                val := default;
            END (*IF*);
            oldval := val;
            OS2.WinSendDlgItemMsg (hwnd, boxid, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(val)), NIL);
        END LoadCheckbox;

    (**********************************************************************)

    VAR j: AuthKind;  val: CARDINAL;
        name: HostName;

    BEGIN
        OpenINIFile;

        (* Bad password limit. *)

        IF INIGetCard ('$SYS', 'BadPasswordLimit', OldBadPasswordLimit) THEN
            val := OldBadPasswordLimit;
        ELSE
            val := 4;
            OldBadPasswordLimit := MAX(CARDINAL);
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.BadPasswordLimit, val, FALSE);
        IF val = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.BadPasswordLimitEnable, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(FALSE)), NIL);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimit), FALSE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimitLabel), FALSE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.BadPasswordLimitEnable, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(TRUE)), NIL);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimit), TRUE);
            OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimitLabel), TRUE);
        END (*IF*);

        (* Apply address checks to MAIL FROM address. *)

        LoadCheckbox (DID.MAILFROMcheck, TRUE, OldMAILFROMcheck, 'MAILFROMcheck');

        (* Enable SPF check. *)

        LoadCheckbox (DID.SPFenabled, TRUE, OldSPFenabled, 'SPFenabled');

        (* POP-before-SMTP authentication. *)

        IF NOT INIGetCard ('$SYS', 'AuthTime', OldAuthTime) THEN
            OldAuthTime := 0;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.AuthentTime, OldAuthTime, FALSE);
        IF OldAuthTime = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.EnablePOPbeforeSMTP, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(FALSE)), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthentTime), FALSE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.EnablePOPbeforeSMTP, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(TRUE)), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthentTime), TRUE);
        END (*IF*);

        (* SMTP AUTH authentication. *)

        IF INIGetCard ('$SYS', 'AuthMethods', val) THEN
            OldAuthMethods := val;
        ELSE
            OldAuthMethods := 0;
            val := DefaultAuthMethods;
        END (*IF*);
        FOR j := MIN(AuthKind) TO MAX(AuthKind) DO
            IF (AuthDID[j] <> 0) AND (IAND (val, AuthMask[j]) <> 0) THEN
                OS2.WinSendDlgItemMsg (hwnd, AuthDID[j], OS2.BM_SETCHECK,
                                       OS2.MPFROMSHORT(ORD(TRUE)), NIL);
            END (*IF*);
        END (*FOR*);

        (* Use fixed local host name? *)

        LoadCheckbox (DID.UseFixedLocalName, FALSE, OldUseFixedLocalName, 'UseFixedLocalName');
        IF INIGetString ('$SYS', 'OurHostName', name) THEN
            OldOurHostName := name;
        ELSE
            AddressToHostName (ServerIPAddress(), name);
            OldOurHostName := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.OurHostName, name);
        IF OldUseFixedLocalName THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OurHostName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OurHostName), FALSE);
        END (*IF*);

        CloseINIFile;

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

    VAR j, AuthMethods: CARDINAL;
        temp: INT16;  k: AuthKind;
        name: HostName;

    BEGIN
        hwnd := OurPageHandle;
        OpenINIFile;

        (* Bad password limit. *)

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.BadPasswordLimitEnable,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
            OS2.WinQueryDlgItemShort (hwnd, DID.BadPasswordLimit, temp, FALSE);
            j := temp;
        ELSE
            j := 0;
        END (*IF*);
        IF j <> OldBadPasswordLimit THEN
            INIPut ('$SYS', 'BadPasswordLimit', j);
        END (*IF*);

        (* MAILFROM check. *)

        StoreCheckbox (DID.MAILFROMcheck, OldMAILFROMcheck, 'MAILFROMcheck');

        (* Enable SPF check. *)

        StoreCheckbox (DID.SPFenabled, OldSPFenabled, 'SPFenabled');

        (* POP-before-SMTP authentication. *)

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.EnablePOPbeforeSMTP,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
            OS2.WinQueryDlgItemShort (hwnd, DID.AuthentTime, temp, FALSE);
            j := temp;
        ELSE
            j := 0;
        END (*IF*);
        IF j <> OldAuthTime THEN
            INIPut ('$SYS', 'AuthTime', j);
        END (*IF*);

        (* SMTP AUTH authentication. *)

        AuthMethods := 0;
        FOR k := MIN(AuthKind) TO MAX(AuthKind) DO
            IF (AuthDID[k] <> 0) AND
                  (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg(hwnd, AuthDID[k],
                                OS2.BM_QUERYCHECK, NIL, NIL)) > 0) THEN
                INC (AuthMethods, AuthMask[k]);
            END (*IF*);
        END (*FOR*);
        IF AuthMethods <> OldAuthMethods THEN
            INIPut ('$SYS', 'AuthMethods', AuthMethods);
        END (*IF*);

        (* Use fixed local host name? *)

        StoreCheckbox (DID.UseFixedLocalName, OldUseFixedLocalName, 'UseFixedLocalName');
        OS2.WinQueryDlgItemText (hwnd, DID.OurHostName, MAX(HostNameIndex), name);
        IF NOT Strings.Equal (OldOurHostName, name) THEN
            INIPutString ('$SYS', 'OurHostName', name);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;

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
        ELSIF msg = OS2.WM_CONTROL THEN
            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                CASE ButtonID OF
                  | DID.BadPasswordLimitEnable:
                       IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.BadPasswordLimitEnable,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimit), TRUE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimitLabel), TRUE);
                       ELSE
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimit), FALSE);
                           OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.BadPasswordLimitLabel), FALSE);
                       END (*IF*);
                       RETURN NIL;
                  | DID.EnablePOPbeforeSMTP:
                       IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.EnablePOPbeforeSMTP,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthentTime), TRUE);
                       ELSE
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthentTime), FALSE);
                       END (*IF*);
                       RETURN NIL;
                  | DID.UseFixedLocalName:
                       IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.UseFixedLocalName,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OurHostName), TRUE);
                       ELSE
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OurHostName), FALSE);
                       END (*IF*);
                       RETURN NIL;
                  | ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*CASE*);
            END (*IF*);
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL);

    (* Creates option page 1 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.optionp1,        (* dialogue ID *)
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
END OptionP1.

