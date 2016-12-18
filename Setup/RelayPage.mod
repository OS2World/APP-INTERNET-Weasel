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

IMPLEMENTATION MODULE RelayPage;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                  Relay page of the notebook                  *)
        (*                                                              *)
        (*        Started:        30 June 1999                          *)
        (*        Last edited:    15 June 2010                          *)
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
    (* proc *)  INIPut, INIFetch,
                INIPutString, INIGetString, INIGetCard;

FROM Names IMPORT
    (* type *)  UserName, PassString;

(**************************************************************************)

TYPE
    NameString = ARRAY [0..511] OF CHAR;

    <* PUSH *>
    <* VOLATILE+ *>

VAR
    OurPageHandle, notebookhandle: OS2.HWND;
    OldRelayHost, OldAuthPOPhost: NameString;
    AuthOption, OldAuthOption: CARDINAL;
    RelayOption, OldRelayOption: CARDINAL;
    RelayEverything, OldRelayEverything: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    OldAuthUser: UserName;
    OldAuthPass: PassString;
    OurPageID: CARDINAL;

    <* POP *>

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Relay.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Relay.Title", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayPageTitle, stringval);
        StrToBuffer (lang, "Relay.host", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayHostBox, stringval);
        StrToBuffer (lang, "Relay.UseHost", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UseRelayHost, stringval);
        StrToBuffer (lang, "Relay.never", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayNever, stringval);
        StrToBuffer (lang, "Relay.asbackup", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayAsBackup, stringval);
        StrToBuffer (lang, "Relay.always", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayAlways, stringval);
        StrToBuffer (lang, "Relay.hostname", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayHostLabel, stringval);
        StrToBuffer (lang, "Relay.everything", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayEverything, stringval);
        StrToBuffer (lang, "Relay.AuthOutgoing", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RelayAuthenticationBox, stringval);
        StrToBuffer (lang, "Relay.none", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthNone, stringval);
        StrToBuffer (lang, "Relay.AUTH", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthAUTH, stringval);
        StrToBuffer (lang, "Relay.POPbefore", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthPOP, stringval);
        StrToBuffer (lang, "Relay.username", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthUsernameLabel, stringval);
        StrToBuffer (lang, "Relay.password", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthPasswordLabel, stringval);
        StrToBuffer (lang, "Relay.POPhost", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AuthPOPhostLabel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file, *)
    (* or loads default values if they're not in the INI file.            *)

    VAR stringval: NameString;

    BEGIN
        OpenINIFile;

        (* Relay option. *)

        IF NOT INIGetCard ('$SYS', 'RelayOption', RelayOption) THEN
            RelayOption := 0;
        END (*IF*);
        IF RelayOption = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.RelayNever, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSIF RelayOption = 1 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.RelayAsBackup, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSE
            RelayOption := 2;
            OS2.WinSendDlgItemMsg (hwnd, DID.RelayAlways, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        END (*IF*);
        OldRelayOption := RelayOption;

        (* Relay host. *)

        IF NOT INIGetString ('$SYS', 'ForwardRelay', stringval) THEN
            stringval := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.RelayHost, stringval);
        OldRelayHost := stringval;

        (* Relay everything. *)

        IF NOT INIFetch ('$SYS', 'RelayEverything', RelayEverything) THEN
            RelayEverything := FALSE;
        END (*IF*);
        IF RelayEverything THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.RelayEverything, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.RelayEverything, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
        END (*IF*);
        OldRelayEverything := RelayEverything;

        (* Authentication option. *)

        IF NOT INIGetCard ('$SYS', 'AuthOption', AuthOption) THEN
            AuthOption := 0;
        END (*IF*);
        IF AuthOption = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthNone, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSIF AuthOption = 1 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthAUTH, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        ELSE
            AuthOption := 2;
            OS2.WinSendDlgItemMsg (hwnd, DID.AuthPOP, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
        END (*IF*);
        OldAuthOption := AuthOption;

        (* Authentication user, password, and POP hostname. *)

        IF NOT INIGetString ('$SYS', 'AuthUser', stringval) THEN
            stringval := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AuthUser, stringval);
        Strings.Assign (stringval, OldAuthUser);

        IF NOT INIGetString ('$SYS', 'AuthPass', stringval) THEN
            stringval := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AuthPass, stringval);
        Strings.Assign (stringval, OldAuthPass);

        IF INIGetString ('$SYS', 'AuthPOPhost', stringval) THEN
            OldAuthPOPhost := stringval;
        ELSE
            OldAuthPOPhost := "";
            stringval := OldRelayHost;
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.AuthPOPhost, stringval);

        CloseINIFile;

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData;

    (* Stores the values on this page back into the INI file. *)

    CONST Nul = CHR(0);

    VAR hwnd: OS2.HWND;
        stringval: NameString;  j: CARDINAL;

    BEGIN
        hwnd := OurPageHandle;
        OpenINIFile;

        (* Relay option. *)

        IF RelayOption <> OldRelayOption THEN
            INIPut ('$SYS', 'RelayOption', RelayOption);
        END (*IF*);

        (* Relay host. *)

        OS2.WinQueryDlgItemText (hwnd, DID.RelayHost, 512, stringval);
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
        IF NOT Strings.Equal (stringval, OldRelayHost) THEN
            INIPutString ('$SYS', 'ForwardRelay', stringval);
        END (*IF*);

        (* Relay everything. *)

        RelayEverything := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.RelayEverything,
                                              OS2.BM_QUERYCHECK, NIL, NIL)) > 0;
        IF RelayEverything <> OldRelayEverything THEN
            INIPut ('$SYS', 'RelayEverything', RelayEverything);
        END (*IF*);

        (* Authentication option. *)

        IF AuthOption <> OldAuthOption THEN
            INIPut ('$SYS', 'AuthOption', AuthOption);
        END (*IF*);

        (* Authentication user. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AuthUser, 512, stringval);
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
        IF NOT Strings.Equal (stringval, OldAuthUser) THEN
            INIPutString ('$SYS', 'AuthUser', stringval);
        END (*IF*);

        (* Authentication password. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AuthPass, 512, stringval);
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
        IF NOT Strings.Equal (stringval, OldAuthPass) THEN
            INIPutString ('$SYS', 'AuthPass', stringval);
        END (*IF*);

        (* Authentication POP hostname. *)

        OS2.WinQueryDlgItemText (hwnd, DID.AuthPOPhost, 512, stringval);
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
        IF NOT Strings.Equal (stringval, OldAuthPOPhost) THEN
            INIPutString ('$SYS', 'AuthPOPhost', stringval);
        END (*IF*);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE EnableRelayFields (hwnd: OS2.HWND;  enable: BOOLEAN);

    (* Enables or disables those fields on this page that should only   *)
    (* be enabled if we will use relaying.                              *)

    BEGIN
        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayHostLabel),
                                                       CAST(OS2.BOOL32, VAL(CARDINAL,enable)));
        IF enable THEN
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayHostLabel), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayHost), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayEverything), TRUE);
            OS2.WinEnableWindow
                   (OS2.WinWindowFromID(hwnd, DID.RelayAuthenticationBox), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthNone), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthAUTH), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOP), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUsernameLabel), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPasswordLabel), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhostLabel), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUser), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPass), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhost), TRUE);
        ELSE
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayHostLabel), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayHost), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RelayEverything), FALSE);
            OS2.WinEnableWindow
                   (OS2.WinWindowFromID(hwnd, DID.RelayAuthenticationBox), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthNone), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthAUTH), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOP), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUsernameLabel), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPasswordLabel), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhostLabel), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUser), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPass), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhost), FALSE);
        END (*IF*);
    END EnableRelayFields;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            LoadValues (hwnd);
            EnableRelayFields (hwnd, RelayOption > 0);
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
            IF (NotificationCode = OS2.BN_CLICKED) AND
                     (OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, ButtonID,
                                         OS2.BM_QUERYCHECK, NIL, NIL)) > 0) THEN
                CASE ButtonID OF
                  | DID.RelayDummyButton:
                       (* Just ignore it. *)
                  | DID.RelayNever:
                       RelayOption := 0;
                  | DID.RelayAsBackup:
                       RelayOption := 1;
                  | DID.RelayAlways:
                       RelayOption := 2;
                  | DID.AuthNone:
                       AuthOption := 0;
                  | DID.AuthAUTH:
                       AuthOption := 1;
                  | DID.AuthPOP:
                       AuthOption := 2;
                  | ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*CASE*);

                EnableRelayFields (hwnd, RelayOption > 0);

                IF RelayOption > 0 THEN
                    IF AuthOption = 0 THEN
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUsernameLabel), FALSE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUser), FALSE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPasswordLabel), FALSE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPass), FALSE);
                    ELSE
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUsernameLabel), TRUE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthUser), TRUE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPasswordLabel), TRUE);
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.AuthPass), TRUE);
                    END (*IF*);

                    IF AuthOption = 2 THEN
                        OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhost), TRUE);
                        OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhostLabel), TRUE);
                    ELSE
                        OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhost), FALSE);
                        OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.AuthPOPhostLabel), FALSE);
                    END (*IF*);
                END (*IF*);

                RETURN NIL;

            END (*IF*);
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL);

    (* Creates this page and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.RelayPage,        (* dialogue ID *)
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
END RelayPage.

