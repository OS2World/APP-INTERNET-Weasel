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

IMPLEMENTATION MODULE SULogging;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                 Logging page of the notebook                 *)
        (*                                                              *)
        (*        Started:        2 November 2003                       *)
        (*        Last edited:    12 December 2016                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT TSIZE, ADDRESS, INT16, CAST, ADR;

IMPORT OS2, OS2RTL;

IMPORT DID, Strings, CommonSettings, UserPage, SUDomains;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  RemoteOperation, INIPut, INIPutString,
                INIFetch, INIGetCard, INIGetTwoShort, INIGetString,
                MakeDirectory;

FROM Remote IMPORT
    (* proc *)  OurDirectory;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

FROM Names IMPORT
    (* type *)  FilenameString;

(**************************************************************************)

CONST Nul = CHR(0);

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;

VAR
    ChangeInProgress: BOOLEAN;

    (* Handle to the window that belongs to this page.  We can save     *)
    (* this as a static value because there is never more than one      *)
    (* instance of this page.                                           *)

    PageID: CARDINAL;
    pagehandle, notebookhandle: OS2.HWND;

    (* Original values of the ini variables. *)

    OldServerPort, OldTimeout, OldMaxUsers: TwoCard;
    OldEnable, OldTransLevel: CARDINAL;
    OldLogPOPusers, OldLogSMTPItems, OldLogOutgoing, OldNoPOPinTL: BOOLEAN;
    OldSMTPLogName, OldPopLogName, OldOutgoingLogFile,
             OldTransLogName, OldSyslogHost: FilenameString;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Logging.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Logging.Transfer", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TransferLogBox, stringval);
        StrToBuffer (lang, "Logging.SMTP", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LogSMTPItems, stringval);
        StrToBuffer (lang, "Logging.POP", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LogPOPUsers, stringval);
        StrToBuffer (lang, "Logging.Outgoing", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LogOutgoing, stringval);
        StrToBuffer (lang, "Logging.Transaction", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.TransactionLogBox, stringval);
        StrToBuffer (lang, "Logging.DetailedLogTo", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DetailedLogTo, stringval);
        StrToBuffer (lang, "Logging.Disk", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.DiskLog, stringval);
        StrToBuffer (lang, "Logging.Screen", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ScreenLog, stringval);
        StrToBuffer (lang, "Logging.Pipe", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.PipeLog, stringval);
        StrToBuffer (lang, "Logging.Syslog", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SysLog, stringval);
        StrToBuffer (lang, "Logging.TransLogName", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LogFileName, stringval);
        StrToBuffer (lang, "Logging.SyslogHost", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SyslogHostLabel, stringval);
        StrToBuffer (lang, "Logging.NoPOPinTL", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.NoPOPinTL, stringval);
        StrToBuffer (lang, "Logging.ExtraLogging", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.ExtraLogging, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE EnableFields (hwnd: OS2.HWND);

    (* Enables or disables the filename windows, depending on the       *)
    (* checkbox values.                                                 *)

    BEGIN
        IF QueryButton (hwnd, DID.LogSMTPItems) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SMTPLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SMTPLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.LogPOPUsers) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PopLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PopLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.LogOutgoing) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OutgoingLogFile), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OutgoingLogFile), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.DiskLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TransLogName), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.TransLogName), FALSE);
        END (*IF*);
        IF QueryButton (hwnd, DID.SysLog) > 0 THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SyslogHost), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SyslogHost), FALSE);
        END (*IF*);
    END EnableFields;

(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    VAR val0: CARDINAL;  name: FilenameString;

    BEGIN
        OpenINIFile;

        (* SMTP Log. *)

        IF NOT INIFetch ('$SYS', 'LogSMTPItems', OldLogSMTPItems) THEN
            OldLogSMTPItems := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogSMTPItems, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(OldLogSMTPItems)), NIL);

        IF INIGetString ('$SYS', 'SMTPLogName', name) THEN
            OldSMTPLogName := name;
        ELSE
            OldSMTPLogName := '';
            name := 'SMTP.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.SMTPLogName, name);

        (* POP Log. *)

        IF NOT INIFetch ('$SYS', 'LogPOPusers', OldLogPOPusers) THEN
            OldLogPOPusers := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogPOPUsers, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(OldLogPOPusers)), NIL);

        IF INIGetString ('$SYS', 'PopLogName', name) THEN
            OldPopLogName := name;
        ELSE
            OldPopLogName := '';
            name := 'POP.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.PopLogName, name);

        (* Log outgoing mail. *)

        IF NOT INIFetch ('$SYS', 'LogOutgoing', OldLogOutgoing) THEN
            OldLogOutgoing := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.LogOutgoing, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(OldLogOutgoing)), NIL);

        IF INIGetString ('$SYS', 'OutgoingLogFile', name) THEN
            OldOutgoingLogFile := name;
        ELSE
            OldOutgoingLogFile := '';
            name := 'SMTPOUT.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.OutgoingLogFile, name);

        (* Transaction logging. *)

        IF INIGetCard ('$SYS', 'TransLevel', val0) THEN
            OldTransLevel := val0;
        ELSE
            val0 := 2;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.DiskLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.ScreenLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.PipeLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.SysLog, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);
        val0 := val0 DIV 2;
        OS2.WinSendDlgItemMsg (hwnd, DID.ExtraLogging, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(val0))), NIL);

        IF INIGetString ('$SYS', 'TransLogName', name) THEN
            OldTransLogName := name;
        ELSE
            OldTransLogName := '';
            name := 'WEASEL.LOG';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.TransLogName, name);

        IF INIGetString ('$SYS', 'SyslogHost', name) THEN
            OldSyslogHost := name;
        ELSE
            OldSyslogHost := '';
            name := '127.0.0.1';
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.SyslogHost, name);

        (* Suppress POP from transactin log. *)

        IF NOT INIFetch ('$SYS', 'NoPOPinTL', OldNoPOPinTL) THEN
            OldNoPOPinTL := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.NoPOPinTL, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(OldNoPOPinTL)), NIL);

        CloseINIFile;
        EnableFields (hwnd);

    END LoadValues;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE StoreData (hwnd: OS2.HWND);

    (* Stores the values on this page back into the INI file.  *)

    VAR val: TwoCard;  bool: BOOLEAN;
        filename: FilenameString;

    BEGIN
        filename := "";

        OpenINIFile;

        (* SMTP Log. *)

        bool := QueryButton (hwnd, DID.LogSMTPItems) > 0;
        IF bool <> OldLogSMTPItems THEN
            INIPut ('$SYS', 'LogSMTPItems', bool);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.SMTPLogName, TSIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldSMTPLogName) THEN
            INIPutString ('$SYS', 'SMTPLogName', filename);
        END (*IF*);

        (* POP Log. *)

        bool := QueryButton (hwnd, DID.LogPOPUsers) > 0;
        IF bool <> OldLogPOPusers THEN
            INIPut ('$SYS', 'LogPOPusers', bool);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.PopLogName, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldPopLogName) THEN
            INIPutString ('$SYS', 'PopLogName', filename);
        END (*IF*);

        (* Log outgoing mail. *)

        bool := QueryButton (hwnd, DID.LogOutgoing) > 0;
        IF bool <> OldLogOutgoing THEN
            INIPut ('$SYS', 'LogOutgoing', bool);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.OutgoingLogFile, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldOutgoingLogFile) THEN
            INIPutString ('$SYS', 'OutgoingLogFile', filename);
        END (*IF*);

        (* Transaction logging. *)

        val[0] := QueryButton (hwnd, DID.DiskLog)
                  + 2 * QueryButton (hwnd, DID.ScreenLog)
                   + 4 * QueryButton (hwnd, DID.PipeLog)
                    + 8 * QueryButton (hwnd, DID.SysLog)
                    + 16 * QueryButton (hwnd, DID.ExtraLogging);
        IF val[0] <> OldTransLevel THEN
            INIPut ('$SYS', 'TransLevel', val[0]);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.TransLogName, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldTransLogName) THEN
            INIPutString ('$SYS', 'TransLogName', filename);
        END (*IF*);

        OS2.WinQueryDlgItemText (hwnd, DID.SyslogHost, SIZE(FilenameString),
                                                                 filename);
        IF NOT Strings.Equal (filename, OldSyslogHost) THEN
            INIPutString ('$SYS', 'SyslogHost', filename);
        END (*IF*);

        (* No POP sessions in transaction Log. *)

        bool := QueryButton (hwnd, DID.NoPOPinTL) > 0;
        IF bool <> OldNoPOPinTL THEN
            INIPut ('$SYS', 'NoPOPinTL', bool);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode: CARDINAL;

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

            NotificationCode := OS2.ULONGFROMMP(mp1) DIV 65536;
            IF NotificationCode = OS2.BN_CLICKED THEN
                EnableFields (hwnd);
                RETURN NIL;
            ELSE
                RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSE    (* default *)
            RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) pgID: CARDINAL): OS2.HWND;

    (* Creates this page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.LoggingPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
        pgID := PageID;
        RETURN pagehandle;
    END CreatePage;

(**************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (pagehandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    ChangeInProgress := FALSE;
    pagehandle := OS2.NULLHANDLE;
    OldServerPort := TwoCard{0,0};
    OldTimeout := TwoCard{0,0};
    OldMaxUsers := TwoCard{0,0};
    OldEnable := 0;
    OldTransLevel := 0;
    OldLogPOPusers := FALSE;  OldLogSMTPItems := FALSE;
    OldLogOutgoing := FALSE;  OldNoPOPinTL := FALSE;
END SULogging.

