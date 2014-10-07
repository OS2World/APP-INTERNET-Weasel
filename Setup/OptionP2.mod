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

IMPLEMENTATION MODULE OptionP2;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                    Page 2 of the notebook                    *)
        (*                                                              *)
        (*        Started:        30 June 1999                          *)
        (*        Last edited:    19 July 2013                          *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT INT16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings;

FROM SUPage1 IMPORT
    (* proc *)  OutputThreadCountIs;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIPut, INIFetch, INIGetCard;

(**************************************************************************)

VAR
    OurPageHandle, notebookhandle: OS2.HWND;
    OnlineOption, OldOnlineOption: CARDINAL;
    OldMaxRecipients, OldRetryHours, OldOutputThreads: INT16;
    OldSingleMatch: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    OurPageID: CARDINAL;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "OptionP2.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "OptionP2.onlineGroup", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.onlineGroup, stringval);
        StrToBuffer (lang, "OptionP2.onlineONLINE", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.onlineONLINE, stringval);
        StrToBuffer (lang, "OptionP2.onlineDialup", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.onlineDialup, stringval);
        StrToBuffer (lang, "OptionP2.onlineAlways", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.onlineAlways, stringval);
        StrToBuffer (lang, "OptionP2.POP3Login", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.POP3Login, stringval);
        StrToBuffer (lang, "OptionP2.RetryHours", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RetryHoursLabel, stringval);
        StrToBuffer (lang, "OptionP2.MaxRecipients", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.MaxRecipientsLabel, stringval);
        StrToBuffer (lang, "OptionP2.SingleMatch", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.SingleMatch, stringval);
        StrToBuffer (lang, "OptionP2.OutboundThreadsBox", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.OutboundThreadsBox, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file, *)
    (* or loads default values if they're not in the INI file.            *)

    VAR SingleMatch: BOOLEAN;

    BEGIN
        OpenINIFile;

        (* Max recipients per mail item. *)

        IF NOT INIFetch ('$SYS', 'MaxRecipientsPerItem', OldMaxRecipients) THEN
            OldMaxRecipients := 100;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.MaxRecipientsPerItem, OldMaxRecipients, FALSE);

        (* Number of hours to try sending mail. *)

        IF NOT INIFetch ('$SYS', 'RetryHours', OldRetryHours) THEN
            OldRetryHours := 100;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.RetryHours, OldRetryHours, FALSE);

        (* Online option. *)

        IF NOT INIGetCard ('$SYS', 'OnlineOption', OnlineOption) THEN
            OnlineOption := 2;
            OldOnlineOption := 3;
        ELSE
            OldOnlineOption := OnlineOption;
        END (*IF*);
        IF OnlineOption = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.onlineONLINE, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSIF OnlineOption = 1 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.onlineDialup, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSE
            OnlineOption := 2;
            OS2.WinSendDlgItemMsg (hwnd, DID.onlineAlways, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        END (*IF*);

        (* POP3 SingleMatch option. *)

        IF NOT INIFetch ('$SYS', 'SingleMatch', OldSingleMatch) THEN
            OldSingleMatch := FALSE;
        END (*IF*);
        SingleMatch := OldSingleMatch;
        OS2.WinSendDlgItemMsg (hwnd, DID.SingleMatch, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(SingleMatch)), NIL);

        (* Number of outbound mail threads. *)

        IF NOT INIFetch ('$SYS', 'OutputThreads', OldOutputThreads) THEN
            OldOutputThreads := 16;
        END (*IF*);
        IF OldOutputThreads <= 0 THEN
            OldOutputThreads := 1;
        ELSIF OldOutputThreads > 64 THEN
            OldOutputThreads := 64;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.OutputThreads, OldOutputThreads, FALSE);
        OutputThreadCountIs (OldOutputThreads);

        CloseINIFile;

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData;

    (* Stores the values on this page back into the INI file. *)

    VAR SingleMatch: BOOLEAN;
        temp: INT16;

    BEGIN
        OpenINIFile;

        (* Max recipients per mail item. *)

        OS2.WinQueryDlgItemShort (OurPageHandle, DID.MaxRecipientsPerItem, temp, FALSE);
        IF temp <= 0 THEN temp := 1
        END (*IF*);
        IF temp <> OldMaxRecipients THEN
            INIPut ('$SYS', 'MaxRecipientsPerItem', temp);
        END (*IF*);

        (* Number of hours to retry outbound mail. *)

        OS2.WinQueryDlgItemShort (OurPageHandle, DID.RetryHours, temp, FALSE);
        IF temp <= 0 THEN temp := 1
        END (*IF*);
        IF temp <> OldRetryHours THEN
            INIPut ('$SYS', 'RetryHours', temp);
        END (*IF*);

        (* Online option. *)

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (OurPageHandle, DID.onlineONLINE,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            OnlineOption := 0;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (OurPageHandle, DID.onlineDialup,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            OnlineOption := 1;
        ELSIF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (OurPageHandle, DID.onlineAlways,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            OnlineOption := 2;
        END (*IF*);
        IF OnlineOption <> OldOnlineOption THEN
            INIPut ('$SYS', 'OnlineOption', OnlineOption);
        END (*IF*);

        (* POP3 SingleMatch option. *)

        SingleMatch := OS2.LONGFROMMR
                         (OS2.WinSendDlgItemMsg (OurPageHandle, DID.SingleMatch,
                                     OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF OldSingleMatch <> SingleMatch THEN
            INIPut ('$SYS', 'SingleMatch', SingleMatch);
        END (*IF*);

        (* Number of outbound mail threads. *)

        OS2.WinQueryDlgItemShort (OurPageHandle, DID.OutputThreads, temp, FALSE);
        IF temp <= 0 THEN temp := 1
        ELSIF temp > 64 THEN temp := 64
        END (*IF*);
        IF temp <> OldOutputThreads THEN
            INIPut ('$SYS', 'OutputThreads', temp);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE SetPOP3Visible (visible: BOOLEAN);

    (* Makes the POP3 SingleMatch option visible or invisible, depending  *)
    (* on the parameter.                                                  *)

    VAR hwnd1, hwnd2: OS2.HWND;

    BEGIN
        hwnd1 := OS2.WinWindowFromID (OurPageHandle, DID.POP3Login);
        hwnd2 := OS2.WinWindowFromID (OurPageHandle, DID.SingleMatch);
        IF visible THEN
            OS2.WinShowWindow (hwnd1, TRUE);
            OS2.WinShowWindow (hwnd2, TRUE);
        ELSE
            OS2.WinShowWindow (hwnd1, FALSE);
            OS2.WinShowWindow (hwnd2, FALSE);
        END (*IF*);
    END SetPOP3Visible;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode, ItemID: CARDINAL;
        count: INT16;

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
            ItemID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF (NotificationCode = OS2.EN_CHANGE) AND
                             (ItemID = DID.OutputThreads) THEN
                OS2.WinQueryDlgItemShort (hwnd, DID.OutputThreads, count, FALSE);
                OutputThreadCountIs (count);
            END (*IF*);
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL);

    (* Creates option page 2 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.optionp2,        (* dialogue ID *)
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
END OptionP2.

