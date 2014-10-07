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

IMPLEMENTATION MODULE EditUser;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for Weasel                   *)
        (*               Dialogue to edit user details              *)
        (*                                                          *)
        (*    Started:        8 July 1999                           *)
        (*    Last edited:    14 April 2012                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)

IMPORT SYSTEM, OS2, DID, Strings, PMInit, WSUINI, RINIData, INIData, Inet2Misc;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM Inet2Misc IMPORT
    (* proc *)  EVAL;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    INIFileName: ARRAY [0..9] OF CHAR;
    OldName, NewName: ARRAY [0..NameLength-1] OF CHAR;
    OurLang: LangHandle;
    ShowIMAPoption, AllowIMAP, SuppressAUTHField, IsInactive,
                    UseTNI: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (OurPageHandle: OS2.HWND;  lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "Edituser.Title", stringval);
        OS2.WinSetWindowText (OurPageHandle, stringval);
        StrToBuffer (lang, "Edituser.Inactive", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.Inactive, stringval);
        StrToBuffer (lang, "Edituser.AllowIMAP", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UIMAP, stringval);
        StrToBuffer (lang, "Edituser.AllowSMTPAuth", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.SMTPAuth, stringval);
        StrToBuffer (lang, "Edituser.Username", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UsernameLabel, stringval);
        StrToBuffer (lang, "Edituser.Password", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.PasswordLabel, stringval);
        StrToBuffer (lang, "Edituser.RealName", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RealnameLabel, stringval);
        StrToBuffer (lang, "Edituser.ForwardOption", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.ForwardBox, stringval);
        StrToBuffer (lang, "Edituser.KeepLocalCopy", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.KeepLocalCopy, stringval);
        StrToBuffer (lang, "Edituser.Comments", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.CommentsLabel, stringval);
        StrToBuffer (lang, "Edituser.Override", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.OverrideBox, stringval);
        StrToBuffer (lang, "Buttons.OK", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.NPOK, stringval);
        StrToBuffer (lang, "Buttons.Cancel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.NPCancel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadStringValue (hwnd: OS2.HWND;  itemID: CARDINAL;  label: ARRAY OF CHAR);

    (* Copies an item from the INI file to the dialogue. *)

    VAR textbuffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        IF (OldName[0] = Nul) OR NOT RINIData.INIGetString (OldName, label, textbuffer) THEN
            textbuffer := "";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, itemID, textbuffer);
    END LoadStringValue;

(**************************************************************************)

PROCEDURE LoadCheckbox (hwnd: OS2.HWND;  itemID: CARDINAL;
                          label: ARRAY OF CHAR;  default: BOOLEAN): BOOLEAN;

    (* Copies a checkbox state from the INI file to the dialogue. *)

    VAR value: BOOLEAN;

    BEGIN
        value := TRUE;
        IF (OldName[0] = Nul) OR NOT RINIData.INIFetch (OldName, label, value) THEN
            value := default;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, itemID, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(value)), NIL);
        RETURN value;
    END LoadCheckbox;

(**************************************************************************)

PROCEDURE StoreStringValue (hwnd: OS2.HWND;  itemID: CARDINAL;  label: ARRAY OF CHAR);

    (* Copies an item from the dialogue to the INI file. *)

    VAR textbuffer: ARRAY [0..2047] OF CHAR;

    BEGIN
        OS2.WinQueryDlgItemText (hwnd, itemID, 2048, textbuffer);
        RINIData.INIPutString (NewName, label, textbuffer);
    END StoreStringValue;

(************************************************************************)

PROCEDURE ComputeActive (hwnd: OS2.HWND): CARDINAL;

    (* Computes the value of "Active" from the checkbox states. *)
    (*   0   this user is temporarily deactivated, i.e. Weasel  *)
    (*       should act as if the user didn't exist.            *)
    (*   1   normal user.                                       *)
    (*   2   don't deliver to the user's account, instead       *)
    (*       forward the mail to the CopyTo address.            *)
    (*   3   deliver one copy to user, another to CopyTo.       *)

    VAR Active: CARDINAL;
        F, K: BOOLEAN;

    BEGIN
        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.Inactive,
                           OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            Active := 0;
        ELSE
            F := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.ForwardCopy,
                                        OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
            K := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.KeepLocalCopy,
                                        OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
            IF F THEN
                IF K THEN
                    Active := 3;
                ELSE
                    Active := 2;
                END (*IF*);
            ELSE
                Active := 1;
            END (*IF*);
        END (*IF*);
        RETURN Active;
    END ComputeActive;

(************************************************************************)

PROCEDURE StoreCheckbox (hwnd: OS2.HWND;  itemID: CARDINAL;  label: ARRAY OF CHAR);

    (* Copies a checkbox state from the dialogue to the INI file. *)

    VAR value: BOOLEAN;

    BEGIN
        value := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, itemID,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        RINIData.INIPut (NewName, label, value);
    END StoreCheckbox;

(************************************************************************)

PROCEDURE NameClash (name: ARRAY OF CHAR): CARDINAL;

    (* Returns 1 if this is the name of an existing user, and 2 if it   *)
    (* is the name of an existing alias, and 0 otherwise.  We assume    *)
    (* that the INI file is already open.                               *)

    BEGIN
        IF RINIData.AppExists(name) THEN RETURN 1
        ELSIF RINIData.KeyExists('$ALIAS', name) THEN RETURN 2
        ELSE RETURN 0
        END (*IF*);
    END NameClash;

(************************************************************************)

PROCEDURE EnableWindows (hwnd: OS2.HWND;  Active: CARDINAL);

    (* Enables and disables various windows depending on the value of   *)
    (* Active.  See procedure ComputeActive, above, for the meaning of  *)
    (* the Active codes.                                                *)

    BEGIN
        IF Active > 3 THEN
            Active := 3;
        END (*IF*);
        IF Active = 0 THEN

            (* Disable almost everything. *)

            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UIMAP), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.name), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.pass), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.realname), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ForwardingAddress), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ForwardCopy), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.KeepLocalCopy), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OverrideFilter2), FALSE);

        ELSE

            (* Enable all of the above, unconditionally except for the *)
            (* two fields that depend on whether we're forwarding.     *)

            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UIMAP), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.name), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.pass), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.realname), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ForwardCopy), TRUE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.OverrideFilter2), TRUE);
            IF Active > 1 THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ForwardingAddress), TRUE);
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.KeepLocalCopy), TRUE);
            ELSE
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ForwardingAddress), FALSE);
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.KeepLocalCopy), FALSE);
            END (*IF*);

        END (*IF*);

    END EnableWindows;

(************************************************************************)
(*                   THE USERNAME/PASSWORD DIALOGUE                     *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR NotificationCode, ButtonID: CARDINAL;
        clash: [0..2];  Active: SYSTEM.CARD8;
        flag: BOOLEAN;
        message: ARRAY [0..255] OF CHAR;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   message := "EditUser";
                   INIData.SetInitialWindowPosition (hwnd, INIFileName,
                                                     message, UseTNI);
                   SetLanguage (hwnd, OurLang);
                   OS2.WinSetDlgItemText (hwnd, DID.name, OldName);
                   WSUINI.OpenINIFile;
                   LoadStringValue (hwnd, DID.pass, "Password");
                   LoadStringValue (hwnd, DID.realname, "RealName");
                   LoadStringValue (hwnd, DID.ForwardingAddress, "CopyTo");
                   IF NOT RINIData.INIFetch (OldName, "Active", Active) THEN
                       Active := 1;
                   END (*IF*);
                   OS2.WinSendDlgItemMsg (hwnd, DID.Inactive, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(ORD(Active=0)), NIL);
                   OS2.WinSendDlgItemMsg (hwnd, DID.ForwardCopy, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(ORD(Active>1)), NIL);
                   OS2.WinSendDlgItemMsg (hwnd, DID.KeepLocalCopy, OS2.BM_SETCHECK,
                                        OS2.MPFROMSHORT(Active MOD 2), NIL);
                   EVAL(LoadCheckbox (hwnd, DID.UIMAP, "UseIMAP", AllowIMAP));
                   EVAL(LoadCheckbox (hwnd, DID.SMTPAuth, "SMTPAuth", NOT SuppressAUTHField));
                   LoadStringValue (hwnd, DID.comments, "Comments");
                   flag := LoadCheckbox (hwnd, DID.OverrideFilter2, "OverrideFilter2", FALSE);
                   LoadStringValue (hwnd, DID.UFilter2, "Filter2");
                   WSUINI.CloseINIFile;
                   EnableWindows (hwnd, ComputeActive(hwnd));
                   IF flag THEN
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UFilter2), TRUE);
                   ELSE
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UFilter2), FALSE);
                   END (*IF*);
                   IF ShowIMAPoption THEN
                       OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UIMAP), TRUE);
                   ELSE
                       OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.UIMAP), FALSE);
                   END (*IF*);
                   IF SuppressAUTHField THEN
                       OS2.WinSendDlgItemMsg (hwnd, DID.SMTPAuth, OS2.BM_SETCHECK,
                                                 OS2.MPFROMSHORT(ORD(TRUE)), NIL);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.SMTPAuth), FALSE);
                   END (*IF*);
                   RETURN NIL;

           | OS2.WM_CONTROL:
                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF NotificationCode = OS2.BN_CLICKED THEN
                       IF ButtonID = DID.OverrideFilter2 THEN
                           IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.OverrideFilter2,
                                             OS2.BM_QUERYCHECK, NIL, NIL)) > 0 THEN
                               OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UFilter2), TRUE);
                           ELSE
                               OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.UFilter2), FALSE);
                           END (*IF*);
                       ELSE
                           EnableWindows (hwnd, ComputeActive(hwnd));
                       END (*IF*);
                       RETURN NIL;
                   END (*IF*);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_COMMAND:
                   IF OS2.SHORT1FROMMP(mp1) = DID.NPOK THEN
                       OS2.WinQueryDlgItemText (hwnd, DID.name, NameLength, NewName);
                       (* Strip leading spaces and $ signs. *)
                       WHILE (NewName[0] = ' ') OR (NewName[0] = '$') DO
                           Strings.Delete (NewName, 0, 1);
                       END (*WHILE*);
                       (* Convert to lower case. *)
                       Inet2Misc.ToLower (NewName);
                       WSUINI.OpenINIFile;
                       IF NewName[0] <> Nul THEN
                           IF Strings.Equal (NewName, OldName) THEN
                               clash := 0;
                           ELSE
                               clash := NameClash(NewName);
                           END (*IF*);
                       ELSE
                           clash := 0;
                       END (*IF*);
                       IF clash = 0 THEN
                           StoreStringValue (hwnd, DID.pass, "Password");
                           StoreStringValue (hwnd, DID.realname, "RealName");
                           Active := ComputeActive(hwnd);
                           RINIData.INIPut (NewName, "Active", Active);
                           StoreStringValue (hwnd, DID.ForwardingAddress, "CopyTo");
                           StoreStringValue (hwnd, DID.comments, "Comments");
                           StoreCheckbox (hwnd, DID.UIMAP, "UseIMAP");
                           IF NOT SuppressAUTHField THEN
                               StoreCheckbox (hwnd, DID.SMTPAuth, "SMTPAuth");
                           END (*IF*);
                           StoreCheckbox (hwnd, DID.OverrideFilter2, "OverrideFilter2");
                           StoreStringValue (hwnd, DID.UFilter2, "Filter2");
                           IF Active > 1 THEN
                               OS2.WinQueryDlgItemText (hwnd, DID.ForwardingAddress, 256, message);
                               IF message[0] = Nul THEN
                                   StrToBuffer (OurLang, "Edituser.NoForwardingAddress", message);
                                   PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                               ELSE
                                   OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                               END (*IF*);
                           ELSE
                               OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                           END (*IF*);
                       ELSIF clash = 1 THEN
                           StrToBuffer (OurLang, "Aliases.UserExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       ELSE
                           StrToBuffer (OurLang, "Aliases.AliasExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       END (*IF*);
                       WSUINI.CloseINIFile;
                       RETURN NIL;
                   ELSIF (OS2.SHORT1FROMMP(mp1) = DID.NPCancel)
                         OR (OS2.SHORT1FROMMP(mp1) = OS2.DID_CANCEL) THEN
                       NewName := "";
                       OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                       RETURN NIL;
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   IsInactive := ComputeActive(hwnd) = 0;
                   message := "EditUser";
                   INIData.StoreWindowPosition (hwnd, INIFileName,
                                                 message, UseTNI);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  lang: LangHandle;
                     IMAPvisible, IMAPasDefault,
                      HideSMTPAuth, TNImode: BOOLEAN): BOOLEAN;

    (* Edit username and password, taking the username from the listbox whose   *)
    (* handle is "owner".  Returns TRUE iff the username was changed.           *)

    VAR index: CARDINAL;  NameChanged, WasInactive: BOOLEAN;

    BEGIN
        UseTNI := TNImode;
        OurLang := lang;
        ShowIMAPoption := IMAPvisible;
        AllowIMAP := IMAPasDefault;
        SuppressAUTHField := HideSMTPAuth;
        index := OS2.LONGFROMMR(
                     OS2.WinSendMsg (owner, OS2.LM_QUERYSELECTION,
                                     NIL, NIL));
        OS2.WinSendMsg (owner, OS2.LM_QUERYITEMTEXT,
                        OS2.MPFROM2USHORT(index, NameLength), SYSTEM.ADR(OldName));
        WasInactive := OldName[0] = '*';
        IsInactive := WasInactive;
        IF WasInactive THEN
            Strings.Delete (OldName, 0, 1);
        END (*IF*);
        OS2.WinDlgBox(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.NameAndPassword,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        NameChanged := NOT Strings.Equal (NewName, OldName);

        IF NewName[0] = Nul THEN

            (* An empty new name is treated as a "Cancel". *)

        ELSIF NameChanged OR (IsInactive <> WasInactive) THEN

            IF NameChanged THEN
                WSUINI.OpenINIFile;
                RINIData.INIDeleteApp (OldName);
                WSUINI.CloseINIFile;
            END (*IF*);

            IF IsInactive THEN
                Strings.Insert ('*', 0, NewName);
            END (*IF*);

            OS2.WinSendMsg (owner, OS2.LM_SETITEMTEXT,
                      OS2.MPFROMSHORT(index), SYSTEM.ADR(NewName));

        END (*IF*);
        RETURN NameChanged;
    END Edit;

(************************************************************************)

BEGIN
    INIFileName := "Setup.INI";
END EditUser.

