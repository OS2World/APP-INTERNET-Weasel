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

IMPLEMENTATION MODULE DomainEditor;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for Weasel                   *)
        (*    The settings notebook for a domain, and its frame     *)
        (*                                                          *)
        (*    Started:        20 December 2001                      *)
        (*    Last edited:    6 July 2012                           *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM, OS2, OS2RTL, DID, PMInit, Strings, CommonSettings, UserPage,
       AliasPage, HostLists, INIData, RINIData, WSUINI;

FROM Names IMPORT
    (* type *)  DomainName, FilenameString;

FROM Remote IMPORT
    (* proc *)  SetInitialWindowPosition, StoreWindowPosition;

FROM Languages IMPORT
    (* type *)  LangHandle;

FROM Misc IMPORT
    (* type *)  HostCategory;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    LanguageString = ARRAY [0..31] OF CHAR;

VAR
    INIFileName: ARRAY [0..9] OF CHAR;
    UseTNI: BOOLEAN;
    SwitchData: OS2.SWCNTRL;     (* switch entry data *)
    pagehandle: ARRAY [1..2] OF OS2.HWND;
    DomName: DomainName;
    CreateWildAliases: BOOLEAN;
    CloseAfterInitialisation: BOOLEAN;
    ChangeInProgress: BOOLEAN;
    NewStyle: BOOLEAN;
    PageFont, TabFontName: CommonSettings.FontName;

(**************************************************************************)

PROCEDURE SetLanguage;

    (* Changes the language of the notebook pages to the current language *)
    (* as recorded by module CommonSettings.                              *)

    VAR NewLang: LangHandle;
        NewName: LanguageString;

    BEGIN
        CommonSettings.CurrentLanguage (NewLang, NewName);
        UserPage.SetLanguage (NewLang);
        AliasPage.SetLanguage (NewLang);
        HostLists.SetLanguage (NewLang);
    END SetLanguage;

(**************************************************************************)

PROCEDURE SetPageFonts;

    (* Changes the font of the notebook pages to the font recorded in the *)
    (* INI file as belonging to this notebook.                            *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (CommonSettings.DomainNotebook, NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;
            UserPage.SetFont(PageFont);
            AliasPage.SetFont(PageFont);
            HostLists.SetLocalFont(PageFont);
        END (*IF*);
    END SetPageFonts;

(**************************************************************************)

PROCEDURE MakeNotebookNewStyle (hwnd: OS2.HWND;  NewStyle: BOOLEAN);

    (* Change to Warp 3 or Warp 4 notebook style. *)

    CONST
        OldStyleFlags = OS2.BKS_BACKPAGESBR + OS2.BKS_MAJORTABBOTTOM
                + OS2.BKS_ROUNDEDTABS + OS2.BKS_TABTEXTCENTER
                + OS2.BKS_STATUSTEXTCENTER + OS2.BKS_SPIRALBIND;
        NewStyleFlags = OS2.BKS_TABBEDDIALOG + OS2.BKS_MAJORTABTOP + OS2.BKS_BACKPAGESTR;

    VAR style: CARDINAL;

    BEGIN
        style := OS2.WinQueryWindowULong (hwnd, OS2.QWL_STYLE);
        style := IAND (style, 0FFFF0000H);
        IF NewStyle THEN
            INC (style, NewStyleFlags);
        ELSE
            INC (style, OldStyleFlags);
        END (*IF*);
        OS2.WinSetWindowULong (hwnd, OS2.QWL_STYLE, style);
    END MakeNotebookNewStyle;

(**************************************************************************)

PROCEDURE InitialiseNotebook (hwnd: OS2.HWND);

    (* hwnd is the handle of the notebook control. *)

    VAR swp: OS2.SWP;  scale, dummy: CARDINAL;
        hini: INIData.HINI;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..18] OF CHAR;

    BEGIN
        CommonSettings.EnableFontChanges(FALSE);
        MakeNotebookNewStyle (hwnd, NewStyle);
        hini := INIData.OpenINIFile (INIFileName, UseTNI);
        app := "Font";
        key := "DomainNotebookTabs";
        IF NOT INIData.INIGetString (hini, app, key, TabFontName) THEN
            TabFontName := "8.Helv";
        END (*IF*);
        INIData.CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
        OS2.WinQueryWindowPos (hwnd, swp);
        scale := 2*swp.cx DIV 13;
        OS2.WinSendMsg (hwnd, OS2.BKM_SETDIMENSIONS,
             OS2.MPFROM2SHORT(scale,5*scale DIV 12), OS2.MPFROMSHORT(OS2.BKA_MAJORTAB));
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        OS2.MPFROMLONG(00FFFFAAH(*0055DBFFH*)), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDPAGECOLOR));
        OS2.WinSendMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                        OS2.MPFROMLONG(0080DBAAH), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDMAJORCOLOR));
        pagehandle[1] := UserPage.CreatePage(hwnd, 0, CommonSettings.DomainNotebook, UseTNI, dummy);
        pagehandle[2] := AliasPage.CreatePage(hwnd, 0, CreateWildAliases, UseTNI,
                                              CommonSettings.DomainNotebook, dummy);
        HostLists.CreatePage(hwnd, local, 0, CommonSettings.DomainNotebook,
                                              TRUE, UseTNI, dummy);
        SetPageFonts;
        SetLanguage;
        OS2.WinShowWindow (hwnd, TRUE);
        CommonSettings.EnableFontChanges(TRUE);
    END InitialiseNotebook;

(**************************************************************************)
(*                WINDOW PROCEDURE FOR SUBCLASSED CASE                    *)
(**************************************************************************)

PROCEDURE ["SysCall"] SubWindowProc (hwnd     : OS2.HWND;
                                     msg      : OS2.ULONG;
                                     mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Window procedure to intercept some of the things that happen in  *)
    (* the notebook subwindow.  We want this here mainly so that we can *)
    (* detect a new font dropped on the notebook tabs.  If the message  *)
    (* is something we don't want to deal with here, we pass it         *)
    (* to the parent window procedure.                                  *)

    VAR OldWndProc: OS2.PFNWP;
        owner: OS2.HWND;  hini: INIData.HINI;
        length, AttrFound: CARDINAL;
        NewFontName: CommonSettings.FontName;
        app: ARRAY [0..4] OF CHAR;
        key: ARRAY [0..18] OF CHAR;

    BEGIN
        OldWndProc := SYSTEM.CAST (OS2.PFNWP, OS2.WinQueryWindowPtr (hwnd, OS2.QWL_USER));
        owner := OS2.WinQueryWindow(hwnd,OS2.QW_OWNER);

        (* Because of the interaction between subclassing and DragText, *)
        (* some messages will go lost if we use the obvious strategy of *)
        (* sending them through to OldWndProc.  To get around this, we  *)
        (* have to send those messages directly to the target window.   *)

        IF (msg = OS2.WM_BUTTON2DOWN) OR (msg = OS2.DM_DRAGOVER)
                   OR (msg = OS2.DM_DRAGLEAVE) OR (msg = OS2.DM_DROP) THEN

            RETURN OS2.WinSendMsg (owner, msg, mp1, mp2);

        (* Check for font or colour change. *)

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress := TRUE;
                length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                             AttrFound, CommonSettings.FontNameSize, NewFontName,
                                              0(*OS2.QPF_NOINHERIT*));
                IF length < CommonSettings.FontNameSize THEN
                    NewFontName[length] := Nul;
                END (*IF*);

                IF NOT Strings.Equal (NewFontName, TabFontName) THEN
                    TabFontName := NewFontName;
                    hini := INIData.OpenINIFile (INIFileName, UseTNI);
                    app := "Font";
                    key := "DomainNotebookTabs";
                    INIData.INIPutString (hini, app, key, TabFontName);
                    INIData.CloseINIFile (hini);
                    OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);
                END (*IF*);
                ChangeInProgress := FALSE;
                RETURN NIL;
            END (*IF*);

        END (*IF*);

        RETURN OldWndProc (hwnd, msg, mp1, mp2);

    END SubWindowProc;

(**************************************************************************)
(*                   WINDOW PROCEDURE FOR MAIN DIALOGUE                   *)
(**************************************************************************)

PROCEDURE ["SysCall"] MainDialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR Title: ARRAY [0..255] OF CHAR;
        bookwin: OS2.HWND;

    BEGIN
        CASE msg OF
           |  OS2.WM_INITDLG:
                   Title := "DomainEditor";
                   INIData.SetInitialWindowPosition (hwnd, INIFileName,
                                                     Title, UseTNI);
                   Title := "Domain = ";
                   Strings.Append (DomName, Title);
                   OS2.WinSetWindowText (hwnd, Title);
                   bookwin := OS2.WinWindowFromID (hwnd, DID.DomainNotebook);
                   InitialiseNotebook (bookwin);
                   IF CloseAfterInitialisation THEN
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);
                   ELSE
                       OS2.WinSetWindowPtr (bookwin, OS2.QWL_USER,
                               SYSTEM.CAST(SYSTEM.ADDRESS,
                                    OS2.WinSubclassWindow (bookwin,
                                                           SubWindowProc)));
                   END (*IF*);

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   CommonSettings.EnableFontChanges(FALSE);
                   StoreWindowPosition (hwnd, "DomainEditor", FALSE);
                   UserPage.Close(OS2.WinWindowFromID (hwnd, DID.DomainNotebook),
                                  pagehandle[1]);
                   HostLists.StoreData (local);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
        RETURN NIL;
    END MainDialogueProc;

(**************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  DomainName, GlobalMailRoot: ARRAY OF CHAR;
                    AcceptUnknown, OpenAndClose, W4Style, TNImode: BOOLEAN);

    (* Opens and allows editing of the main dialogue box for this domain. *)
    (* If OpenAndClose = TRUE we open the dialogue and then close it      *)
    (* without waiting for user input; this is to cover the case where    *)
    (* we want to refresh INI file data as the result of things like      *)
    (* format changes resulting from an upgrade.                          *)

    VAR hwnd: OS2.HWND;
        pid: OS2.PID;  tid: OS2.TID;
        Directory: FilenameString;

    BEGIN
        UseTNI := TNImode;
        IF UseTNI THEN
           INIFileName := "Setup.TNI";
        ELSE
           INIFileName := "Setup.INI";
        END (*IF*);
        PageFont := "";
        TabFontName := "";
        NewStyle := W4Style;
        ChangeInProgress := FALSE;
        CreateWildAliases := AcceptUnknown;
        CloseAfterInitialisation := OpenAndClose;
        Strings.Assign (DomainName, DomName);
        Strings.Assign (GlobalMailRoot, Directory);
        Strings.Append (DomainName, Directory);
        RINIData.MakeDirectory (Directory);
        WSUINI.SetINIDirectory (GlobalMailRoot, DomainName);
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.BigFrame_Domain, (* dialogue ID *)
                       NIL);                (* creation parameters *)

        (* Put us on the visible task list. *)

        OS2.WinQueryWindowProcess (hwnd, pid, tid);
        SwitchData.hwnd := hwnd;
        WITH SwitchData DO
            hwndIcon      := 0;
            hprog         := 0;
            idProcess     := pid;
            idSession     := 0;
            uchVisibility := OS2.SWL_VISIBLE;
            fbJump        := OS2.SWL_JUMPABLE;
            szSwtitle     := "Weasel domain editor notebook";
            bProgType     := 0;
        END (*WITH*);
        OS2.WinCreateSwitchEntry (PMInit.OurHab(), SwitchData);

        OS2.WinProcessDlg(hwnd);
        OS2.WinDestroyWindow (hwnd);
        Strings.Append ('\', Directory);
        UserPage.CheckUserDirectories (Directory);
        WSUINI.SetINIDirectory (GlobalMailRoot, "");

    END Edit;

(**************************************************************************)

BEGIN
    INIFileName := "Setup.INI";
    CreateWildAliases := FALSE;
    CloseAfterInitialisation := FALSE;
    ChangeInProgress := FALSE;
    UseTNI := FALSE;
END DomainEditor.

