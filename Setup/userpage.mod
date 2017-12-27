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

IMPLEMENTATION MODULE UserPage;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                  User page of the notebook                   *)
        (*                                                              *)
        (*        Started:        8 July 1999                           *)
        (*        Last edited:    22 May 2017                           *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD8, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings, EditUser, PMInit, Strings;

FROM AliasPage IMPORT
    (* proc *)  AliasMatch;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, RemoveUser;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  INIFetch, INIGetString, ItemSize, MakeDirectory,
                AppExists, INIPut, INIPutString, INIDeleteKey,
                GetStringList, NextString, CloseStringList;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ConvertCard;

FROM Names IMPORT
    (* type *)  UserName;

(**************************************************************************)

CONST
    Nul = CHR(0);

VAR
    OurPageHandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    UserCount: CARDINAL;
    OurFontGroup: CommonSettings.FontGroup;
    IMAPasDefault: BOOLEAN;
    SMTPAuthForAll: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    UseTNI: BOOLEAN;
    OurLang: LangHandle;

(************************************************************************)
(*             ENSURING THAT EVERY USER HAS A MAIL DIRECTORY            *)
(************************************************************************)

PROCEDURE CheckUserDirectories (VAR (*IN*) MailRoot: ARRAY OF CHAR);

    (* Makes sure that MailRoot has a subdirectory for every user in    *)
    (* the user database.  (We don't have to check for the existence of *)
    (* the MailRoot directory itself, because this has been done in an  *)
    (* earlier step.)  Also creates the 'postmaster' username and       *)
    (* directory if they don't already exist.                           *)

    VAR name: UserName;
        DirName: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE MakeMailDirectory (username: ARRAY OF CHAR);

        (* Try to create a mailbox directory for the given user.  If    *)
        (* the directory already exists the operation will fail anyway. *)

        BEGIN
            Strings.Assign (MailRoot, DirName);
            Strings.Append (username, DirName);
            MakeDirectory (DirName);
        END MakeMailDirectory;

    (********************************************************************)

    VAR state: StringReadState;

    BEGIN
        OpenINIFile;

        (* Ensure that there's a user called "postmaster". *)

        IF NOT AppExists("postmaster")
                             AND NOT AliasMatch ("postmaster") THEN
            name := "Postmaster account";
            INIPutString ("postmaster", "Comments", name);
        END (*IF*);

        (* Get the list of users from the INI file. *)

        GetStringList ("", "", state);
        REPEAT
            NextString (state, name);
            IF (name[0] <> Nul) AND (name[0] <> "$") THEN

                (* User name found, try to create a directory.      *)
                (* (If the directory already exists the operation   *)
                (* will fail anyway.)                               *)

                MakeMailDirectory (name);

            END (*IF*);

        UNTIL name[0] = Nul;

        CloseStringList (state);
        CloseINIFile;

    END CheckUserDirectories;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Users.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Users.Label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UserPageLabel, stringval);
        StrToBufferN (lang, "Users.total", UserCount, stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.UserCount, stringval);
        StrToBuffer (lang, "Users.EnableIMAP", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.IMAPForNewUsers, stringval);
        StrToBuffer (lang, "Users.SMTPAuthAllUsers", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.SMTPAuthAllUsers, stringval);
        StrToBuffer (lang, "Buttons.Add", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AddUserButton, stringval);
        StrToBuffer (lang, "Buttons.Edit", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.EditUserButton, stringval);
        StrToBuffer (lang, "Buttons.Delete", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DeleteUserButton, stringval);
    END SetLanguage;

(************************************************************************)
(*                    LOADING AND STORING USER PAGE DATA                *)
(************************************************************************)

PROCEDURE DisplayUserCount (hwnd: OS2.HWND);

    (* Displays the value of global variable UserCount. *)

    VAR buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        StrToBufferN (OurLang, "Users.total", UserCount, buffer);
        OS2.WinSetDlgItemText (hwnd, DID.UserCount, buffer);
    END DisplayUserCount;

(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on the user page with data from the INI file,*)
    (* or loads default values if they're not in the INI file.                  *)

    VAR name: UserName;
        state: StringReadState;
        Active: CARD8;
        boolDummy: BOOLEAN;

    BEGIN
        UserCount := 0;
        OpenINIFile;

        (* Pick up the list of all applications. *)

        GetStringList ("", "", state);
        REPEAT
            NextString (state, name);
            IF (name[0] <> Nul) AND (name[0] <> '$') THEN

                (* Special flag for an inactive user. *)

                IF INIFetch (name, "Active", Active) AND (Active = 0) THEN
                    Strings.Insert ('*', 0, name);
                END (*IF*);

                (* Add name to the listbox. *)

                INC (UserCount);
                OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), ADR(name));

            END (*IF*);
        UNTIL name[0] = Nul;
        CloseStringList (state);

        (* IMAP as default. *)

        IF NOT INIFetch ("$SYS", "IMAPForNewUsers", IMAPasDefault) THEN
            IMAPasDefault := TRUE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.IMAPForNewUsers, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(IMAPasDefault)), NIL);

        (* SMTP AUTH valid for all users. *)

        IF NOT INIFetch ("$SYS", "SMTPAuthAllUsers", SMTPAuthForAll) THEN
            SMTPAuthForAll := TRUE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.SMTPAuthAllUsers, OS2.BM_SETCHECK,
                                 OS2.MPFROMSHORT(ORD(SMTPAuthForAll)), NIL);

        (* OBSOLETE - Support for the "leave messages on server" users. *)

        IF INIFetch ("$SYS", "POPFetchLatest", boolDummy) THEN
            INIDeleteKey ("$SYS", "POPFetchLatest");
        END (*IF*);

        CloseINIFile;

    END LoadValues;

(************************************************************************)

PROCEDURE SaveIMAPandAUTHFlags (hwnd: OS2.HWND);

    (* Saves the "IMAP as default" and "SMTP AUTH for all users" and    *)
    (* "POP Fetch Latest" options.                                      *)

    VAR bool, INIFileIsOpen: BOOLEAN;

    BEGIN
        INIFileIsOpen := FALSE;
        bool := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.IMAPForNewUsers,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF bool <> IMAPasDefault THEN
            OpenINIFile;  INIFileIsOpen := TRUE;
            INIPut ("$SYS", "IMAPForNewUsers", bool);
            IMAPasDefault := bool;
        END (*IF*);
        bool := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, DID.SMTPAuthAllUsers,
                                       OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
        IF bool <> SMTPAuthForAll THEN
            IF NOT INIFileIsOpen THEN
                OpenINIFile;  INIFileIsOpen := TRUE;
            END (*IF*);
            SMTPAuthForAll := bool;
            INIPut ("$SYS", "SMTPAuthAllUsers", bool);
        END (*IF*);

        IF INIFileIsOpen THEN
            CloseINIFile;
        END (*IF*);
    END SaveIMAPandAUTHFlags;

(************************************************************************)

PROCEDURE Close (notebook, hwnd: OS2.HWND);

    (* Shuts down this window and removes it from the notebook. *)

    BEGIN
        SaveIMAPandAUTHFlags(hwnd);
        OurFontGroup := CommonSettings.NilFontGroup;
        OS2.WinSendMsg (notebook, OS2.BKM_DELETEPAGE,
                        CAST(ADDRESS, PageID),
                        OS2.MPFROMLONG (OS2.BKA_SINGLE));
        OS2.WinSendMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
    END Close;

(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        listwindow: OS2.HWND;
        name: UserName;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), FALSE);
            LoadValues (hwnd);
            DisplayUserCount (hwnd);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, OurFontGroup);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSIF msg = OS2.WM_COMMAND THEN

            listwindow := OS2.WinWindowFromID(hwnd,DID.userlist);
            index := OS2.LONGFROMMR(
                      OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_QUERYSELECTION, NIL, NIL));
            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.AddUserButton:
                   name := "";
                   IF index = OS2.LIT_NONE THEN
                       index := 0;
                   ELSE
                       INC(index);
                   END (*IF*);
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   IF EditUser.Edit(listwindow, OurLang, TRUE,
                                        IMAPasDefault, SMTPAuthForAll, UseTNI) THEN
                       (* We have a new entry. *)
                       INC (UserCount);
                       DisplayUserCount (hwnd);
                   ELSE
                       (* Empty name, delete it. *)
                       OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                             OS2.MPFROMSHORT(DID.DeleteUserButton), NIL);
                   END (*IF*);

              | DID.EditUserButton:
                   EVAL (EditUser.Edit(listwindow, OurLang, TRUE,
                                                IMAPasDefault, SMTPAuthForAll, UseTNI));

              | DID.DeleteUserButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   OS2.WinSendDlgItemMsg (hwnd, DID.userlist, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), FALSE);
                   IF name[0] <> Nul THEN
                       RemoveUser (name);
                       DEC (UserCount);
                       DisplayUserCount (hwnd);
                   END (*IF*);

            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*CASE*);
            RETURN NIL;

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.userlist THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditUserButton), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteUserButton), TRUE);
                    RETURN NIL;
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditUserButton), NIL);
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSIF ((ButtonID = DID.IMAPForNewUsers)
                   OR (ButtonID = DID.SMTPAuthAllUsers))
                       AND (NotificationCode = OS2.BN_CLICKED) THEN
                SaveIMAPandAUTHFlags(hwnd);
                RETURN NIL;
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  AfterPage: CARDINAL;
                                 group: CommonSettings.FontGroup;
                                 TNImode: BOOLEAN;
                                 VAR (*OUT*) ID: CARDINAL): OS2.HWND;

    (* Creates the user page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        notebookhandle := notebook;
        OurFontGroup := group;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.UserPage,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        IF AfterPage = 0 THEN
            PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        ELSE
            PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         CAST (ADDRESS, AfterPage),
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_NEXT)));
        END (*IF*);
        StrToBuffer (OurLang, "Users.tab", Label);
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,OurPageHandle));
        ID := PageID;
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
    UseTNI := FALSE;
    ChangeInProgress := FALSE;
    IMAPasDefault := TRUE;
    OurPageHandle := OS2.NULLHANDLE;
END UserPage.

