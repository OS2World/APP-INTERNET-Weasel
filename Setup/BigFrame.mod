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

IMPLEMENTATION MODULE BigFrame;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for Weasel                   *)
        (*             The settings notebook and its frame          *)
        (*                                                          *)
        (*    Started:        28 June 1999                          *)
        (*    Last edited:    10 August 2018                        *)
        (*    Status:         Working                               *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM;

IMPORT OS2, OS2RTL, Strings;

IMPORT DID, SUPage1, IMAPPage, SULogging, Filter, OptionP1, OptionP2, OptionP3,
       RelayPage, ChunkingPage, DomainPage, BlackLists, SUDomains, UserPage,
       AliasPage, HostLists, CommonSettings, INIData, RINIData, WSUINI;

FROM Names IMPORT
    (* type *)  FilenameString, UserName, UserNameIndex, DomainName;

FROM Remote IMPORT
    (* proc *)  SelectRemoteFile, StoreWindowPosition;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

FROM PMInit IMPORT
    (* proc *)  WarningBox;

FROM Misc IMPORT
    (* type *)  HostCategory;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(**************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    LanguageString = ARRAY [0..31] OF CHAR;
    Page = (pbase, pimap, pdomains, puser, palias, plocal, plog, pfilters,
            popt1, popt2, popt3, prelay, pchunking, pwhite, ptrusted, pgatefor,
            pbanned, pnochunk, pblack);

VAR
    (* INI file name for the Setup INI *)

    INIFileName: ARRAY [0..9] OF CHAR;

    pagehandle: ARRAY Page OF OS2.HWND;
    IDofPage: ARRAY Page OF CARDINAL;
    ChangeInProgress: BOOLEAN;
    MultiDomain: BOOLEAN;
    NewStyle: BOOLEAN;

    (* Use Weasel.TNI and Setup.TNI rather than Weasel.INI and Setup.INI. *)

    UseTNI: BOOLEAN;

    (* Page ID for the page after which pages should be inserted   *)
    (* when going to/from multidomain mode.                        *)

    InsertionPoint: CARDINAL;

    (* Page number for the initial page to display. *)

    StartingPage: Page;

    (* Fonts and language. *)

    PageFont, TabFontName: CommonSettings.FontName;
    OurLanguage: LanguageString;
    LangCode: LangHandle;

    (* AcceptUnknown is an option that became obsolete in version 1.62  *)
    (* (February 2003).  If we find it still set, we will create a      *)
    (* wildcard alias in each domain.                                   *)

    AcceptUnknown: BOOLEAN;

    (* A flag that procedure UpdateNotebook1 uses to tell that it is    *)
    (* switching into multidomain mode as a startup operation, rather   *)
    (* than as a change initiated by the user.                          *)

    InitialMultidomainSwitch: BOOLEAN;

(**************************************************************************)

PROCEDURE SetLanguage;

    (* Changes the language of the notebook pages to the current language *)
    (* as recorded by module CommonSettings.                              *)

    VAR NewName: LanguageString;

    BEGIN
        CommonSettings.CurrentLanguage (LangCode, NewName);
        IF NOT Strings.Equal(NewName, OurLanguage) THEN
            SUPage1.SetLanguage (LangCode);
            IMAPPage.SetLanguage(LangCode);
            SULogging.SetLanguage (LangCode);
            Filter.SetLanguage (LangCode);
            OptionP1.SetLanguage (LangCode);
            OptionP2.SetLanguage (LangCode);
            OptionP3.SetLanguage (LangCode);
            RelayPage.SetLanguage (LangCode);
            ChunkingPage.SetLanguage (LangCode);
            IF MultiDomain THEN
                DomainPage.SetLanguage(LangCode);
            ELSE
                UserPage.SetLanguage (LangCode);
                AliasPage.SetLanguage (LangCode);
            END (*IF*);
            HostLists.SetLanguage (LangCode);
            BlackLists.SetLanguage (LangCode);
        END (*IF*);
        OurLanguage := NewName;
    END SetLanguage;

(**************************************************************************)

PROCEDURE SetPageFonts (UpdateAll: BOOLEAN);

    (* Changes the font of the notebook pages to the font recorded in the *)
    (* INI file as belonging to this notebook.                            *)

    VAR NewFontName: CommonSettings.FontName;

    BEGIN
        CommonSettings.CurrentFont (CommonSettings.MainNotebook, NewFontName);
        IF NOT Strings.Equal (NewFontName, PageFont) THEN
            PageFont := NewFontName;

            IF UpdateAll THEN
                SUPage1.SetFont (PageFont);
                IMAPPage.SetFont(PageFont);
                SULogging.SetFont (PageFont);
                Filter.SetFont (PageFont);
                OptionP1.SetFont (PageFont);
                OptionP2.SetFont (PageFont);
                OptionP3.SetFont (PageFont);
                RelayPage.SetFont (PageFont);
                ChunkingPage.SetFont (PageFont);
                HostLists.SetFonts (PageFont);
                BlackLists.SetFont (PageFont);
            END (*IF*);
        END (*IF*);

        IF MultiDomain THEN
            DomainPage.SetFont(PageFont);
        ELSE
            UserPage.SetFont (PageFont);
            AliasPage.SetFont (PageFont);
            HostLists.SetLocalFont (PageFont);
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

    VAR swp: OS2.SWP;  scale, LastPageID: CARDINAL;
        owner: OS2.HWND;

    BEGIN
        (* Find OS version to decide what notebook style to use. *)

        scale := 30;
        OS2.DosQuerySysInfo(12, 12, SYSTEM.ADR(scale), SIZE(CARDINAL));
        NewStyle := scale >= 40;
        MakeNotebookNewStyle (hwnd, NewStyle);

        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE,CommonSettings.FontNameSize, TabFontName);

        (* If the new style is enabled, the following code will have no effect *)
        (* because the messages to set tab size and colours will be ignored.   *)

        OS2.WinQueryWindowPos (hwnd, swp);
        scale := 2*swp.cx DIV 13;
        IF NewStyle THEN
            OS2.WinPostMsg (hwnd, OS2.BKM_SETDIMENSIONS,
                 OS2.MPFROM2SHORT(scale,5*scale DIV 12), OS2.MPFROMSHORT(OS2.BKA_MAJORTAB));
            OS2.WinPostMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                            OS2.MPFROMLONG(00FFFFAAH(*0055DBFFH*)), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDPAGECOLOR));
            OS2.WinPostMsg (hwnd, OS2.BKM_SETNOTEBOOKCOLORS,
                            OS2.MPFROMLONG(0080DBAAH), OS2.MPFROMLONG(OS2.BKA_BACKGROUNDMAJORCOLOR));
        END (*IF*);

        pagehandle[pbase] := SUPage1.CreatePage(hwnd, IDofPage[pbase]);
        IDofPage[pimap] := 0;
        pagehandle[pimap] := IMAPPage.CreatePage(hwnd, IDofPage[pimap]);
        InsertionPoint := IDofPage[pimap];
        pagehandle[puser] := UserPage.CreatePage(hwnd, InsertionPoint,
                               CommonSettings.MainNotebook, UseTNI, IDofPage[puser]);
        pagehandle[palias] := AliasPage.CreatePage(hwnd, 0,
                                    AcceptUnknown AND NOT MultiDomain, UseTNI,
                                    CommonSettings.MainNotebook, IDofPage[palias]);
        HostLists.CreatePage(hwnd, local, 0, CommonSettings.MainNotebook,
                                   FALSE, UseTNI, IDofPage[plocal]);
        pagehandle[plog] := SULogging.CreatePage(hwnd, IDofPage[plog]);
        Filter.CreatePage(hwnd, LastPageID);
        IDofPage[pfilters] := LastPageID;
        OptionP1.CreatePage(hwnd, LastPageID);
        IDofPage[popt1] := LastPageID;
        OptionP2.CreatePage(hwnd, LastPageID);
        IDofPage[popt2] := LastPageID;
        OptionP3.CreatePage(hwnd, LastPageID);
        IDofPage[popt3] := LastPageID;
        RelayPage.CreatePage(hwnd, LastPageID);
        IDofPage[prelay] := LastPageID;
        ChunkingPage.CreatePage(hwnd, LastPageID);
        IDofPage[pchunking] := LastPageID;
        HostLists.CreatePage(hwnd, whitelisted, 0,
                     CommonSettings.MainNotebook, FALSE, UseTNI, IDofPage[pwhite]);
        HostLists.CreatePage(hwnd, mayrelay, 0,
                     CommonSettings.MainNotebook, FALSE, UseTNI, IDofPage[ptrusted]);
        HostLists.CreatePage(hwnd, relaydest, 0,
                     CommonSettings.MainNotebook, FALSE, UseTNI, IDofPage[pgatefor]);
        HostLists.CreatePage(hwnd, banned, 0,
                     CommonSettings.MainNotebook, FALSE, UseTNI, IDofPage[pbanned]);
        pagehandle[pblack] := BlackLists.CreatePage(hwnd, IDofPage[pblack]);
        OptionP2.SetPOP3Visible (FALSE);

        (* For simplicity, we start up in single-domain mode, and then  *)
        (* let UpdateNotebook1 (see below) switch us over if necessary. *)

        MultiDomain := FALSE;
        SetPageFonts (TRUE);
        SetLanguage;
        OS2.WinPostMsg (hwnd, OS2.BKM_TURNTOPAGE,
                           OS2.MPFROMULONG(IDofPage[StartingPage]), NIL);
        OS2.WinShowWindow (hwnd, TRUE);
        CommonSettings.EnableFontChanges(TRUE);

        (* The parent of this window is the frame.  The owner of that   *)
        (* frame is the window we want to hide.  For neatness, we also  *)
        (* blank out the previous status message in case the window     *)
        (* becomes unhidden again.                                      *)

        owner := OS2.WinQueryWindow (hwnd, OS2.QW_PARENT);
        owner := OS2.WinQueryWindow (owner, OS2.QW_OWNER);
        OS2.WinSetDlgItemText (owner, DID.Status, "");
        OS2.WinShowWindow (owner, FALSE);

    END InitialiseNotebook;

(**************************************************************************)

PROCEDURE UpdateNotebook1 (hwnd: OS2.HWND;  NewMultiDomain: BOOLEAN);

    (* Adds or removes notebook pages, depending on whether we're       *)
    (* dealing with multiple domain support.                            *)

    VAR notebook: OS2.HWND;  found: BOOLEAN;
        MailRootDir, message: FilenameString;
        OriginalDomainName: DomainName;

    BEGIN
        CommonSettings.EnableFontChanges(FALSE);
        notebook := OS2.WinWindowFromID(hwnd, DID.notebook);
        IF MultiDomain <> NewMultiDomain THEN

            SUPage1.CommitMailRoot (MailRootDir);
            SUDomains.SetMailRoot (MailRootDir);
            DomainPage.SetMailRoot (MailRootDir);
            WSUINI.SetINIDirectory (MailRootDir, "");
            SUDomains.GetOriginalDomainName (OriginalDomainName);

            IF NewMultiDomain THEN

                (* Switch to multidomain mode. *)

                UserPage.Close(notebook, pagehandle[puser]);
                IDofPage[puser] := 0;
                AliasPage.Close(notebook, pagehandle[palias]);
                IDofPage[palias] := 0;
                HostLists.Close(notebook, local);
                pagehandle[pdomains] := DomainPage.CreatePage(notebook, InsertionPoint,
                             AcceptUnknown, NewStyle, UseTNI, IDofPage[pdomains]);
                AcceptUnknown := FALSE;
                IF NOT SUDomains.DomainExists(OriginalDomainName) THEN
                    DomainPage.Add (OriginalDomainName);
                END (*IF*);
                Strings.Append (OriginalDomainName, MailRootDir);
                RINIData.MakeDirectory (MailRootDir);
                SUDomains.MoveAllUsers ("", OriginalDomainName);
                IF (NOT InitialMultidomainSwitch) OR (StartingPage = pdomains) THEN
                    OS2.WinPostMsg (notebook, OS2.BKM_TURNTOPAGE,
                         OS2.MPFROMULONG(IDofPage[pdomains]), NIL);
                END (*IF*);
                InitialMultidomainSwitch := FALSE;

            ELSE

                (* Switch to single domain mode. *)

                found := SUDomains.DomainExists(OriginalDomainName);
                IF NOT found THEN
                    SUDomains.ResetOriginal(OriginalDomainName);
                    found := SUDomains.DomainExists(OriginalDomainName);
                END (*IF*);

                (* Tell the user which domain is being converted, and allow     *)
                (* the option of cancelling the change.                         *)

                StrToBufferA (LangCode, "Rego.ToSingleDomain", OriginalDomainName, message);
                IF OS2.WinMessageBox (OS2.HWND_DESKTOP, notebook,
                             message, "", 0,
                             OS2.MB_OKCANCEL + OS2.MB_INFORMATION) <> OS2.MBID_OK THEN
                    OS2.WinSendDlgItemMsg (pagehandle[pdomains],
                             DID.RevertToSingleDomain, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(0), NIL);
                    RETURN;
                END (*IF*);

                IF found THEN
                    SUDomains.MoveAllUsers (OriginalDomainName, "");
                    DomainPage.Remove (OriginalDomainName);
                END (*IF*);

                (* DomainPage.Remove changes its record of the original domain   *)
                (* name; normally when we delete a domain we'd like the record   *)
                (* updated to choose the next available name.  In the present    *)
                (* case, however, we prefer to keep the old value as the one to  *)
                (* use if and when we switch back to single-domain mode.         *)

                SUDomains.SetOriginalDomainName (OriginalDomainName);

                DomainPage.Close(notebook);
                IDofPage[pdomains] := 0;
                HostLists.CreatePage (notebook, local, InsertionPoint,
                                      CommonSettings.MainNotebook, FALSE,
                                       UseTNI, IDofPage[plocal]);
                pagehandle[palias] := AliasPage.CreatePage(notebook, InsertionPoint, FALSE,
                                            UseTNI, CommonSettings.MainNotebook, IDofPage[palias]);
                pagehandle[puser] := UserPage.CreatePage(notebook, InsertionPoint,
                                            CommonSettings.MainNotebook,
                                             UseTNI, IDofPage[puser]);
                OS2.WinSendDlgItemMsg (pagehandle[pbase],
                                     DID.MultiDomainEnabled, OS2.BM_SETCHECK,
                                     NIL, NIL);
                OS2.WinPostMsg (notebook, OS2.BKM_TURNTOPAGE,
                         OS2.MPFROMULONG(IDofPage[pbase]), NIL);
            END (*IF*);
            MultiDomain := NewMultiDomain;
            SetPageFonts (FALSE);
            IF MultiDomain THEN
                DomainPage.SetLanguage(LangCode);
            ELSE
                UserPage.SetLanguage (LangCode);
                AliasPage.SetLanguage (LangCode);
                HostLists.SetLanguage (LangCode);
            END (*IF*);
            OptionP2.SetPOP3Visible (MultiDomain);

        END (*IF*);
        CommonSettings.EnableFontChanges(TRUE);

    END UpdateNotebook1;

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
        key: ARRAY [0..16] OF CHAR;

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
                    key := "MainNotebookTabs";
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

    VAR bookwin: OS2.HWND;  lang: LangHandle;
        hini: INIData.HINI;  pageID: CARDINAL;  pg: Page;
        stringval, filename: ARRAY [0..255] OF CHAR;
        app: ARRAY [0..12] OF CHAR;
        key: ARRAY [0..12] OF CHAR;

    BEGIN
        bookwin := OS2.WinWindowFromID (hwnd, DID.notebook);
        IF UseTNI THEN
            filename := "Weasel.TNI";
        ELSE
            filename := "Weasel.INI";
        END (*IF*);
        CASE msg OF
           |  OS2.WM_INITDLG:
                   stringval := "MainNotebook";
                   INIData.SetInitialWindowPosition (hwnd, INIFileName,
                                                     stringval, UseTNI);
                   CommonSettings.CurrentLanguage (lang, stringval);

                   IF RINIData.RemoteOperation() THEN
                       IF SelectRemoteFile(filename) THEN
                           StrToBuffer (lang, "Main.remote", stringval);
                       ELSE
                           StrToBuffer (lang, "Main.cantopen", stringval);
                       END (*IF*);
                   ELSE
                       StrToBuffer (lang, "Main.local", stringval);
                   END (*IF*);
                   Strings.Append ("      ", stringval);
                   Strings.Append (filename, stringval);
                   OS2.WinSetWindowText (hwnd, stringval);
                   OS2.WinSetWindowPtr (bookwin, OS2.QWL_USER,
                               SYSTEM.CAST(SYSTEM.ADDRESS,
                                    OS2.WinSubclassWindow (bookwin,
                                                           SubWindowProc)));
                   InitialiseNotebook (bookwin);
                   RETURN NIL;

           |  CommonSettings.FONTCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       SetPageFonts (TRUE);
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  CommonSettings.LANGCHANGED:

                   IF ChangeInProgress THEN
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   ELSE
                       ChangeInProgress := TRUE;
                       CommonSettings.CurrentLanguage (lang, stringval);
                       IF RINIData.RemoteOperation() THEN
                           StrToBuffer (lang, "Main.remote", stringval);
                       ELSE
                           StrToBuffer (lang, "Main.local", stringval);
                       END (*IF*);
                       Strings.Append ("      ", stringval);
                       Strings.Append (filename, stringval);
                       OS2.WinSetWindowText (hwnd, stringval);
                       SetLanguage;
                       ChangeInProgress := FALSE;
                       RETURN NIL;
                   END (*IF*);

           |  CommonSettings.WM_MULTIDOMAIN_CHANGE:
                   UpdateNotebook1 (hwnd, OS2.LONGFROMMR(mp1) > 0);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  CommonSettings.SETFOCUS:
                   OS2.WinSetFocus(OS2.HWND_DESKTOP,
                        OS2.WinWindowFromID(hwnd, IDofPage[StartingPage]));
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

           |  OS2.WM_CLOSE:
                   CommonSettings.EnableFontChanges(FALSE);
                   bookwin := OS2.WinWindowFromID(hwnd, DID.notebook);
                   pageID := OS2.ULONGFROMMR(OS2.WinSendMsg (bookwin, OS2.BKM_QUERYPAGEID,
                                 OS2.MPFROMULONG(0),
                                  OS2.MPFROMSHORT(OS2.BKA_TOP)));
                   pg := MAX(Page);
                   WHILE (IDofPage[pg] <> pageID) AND (pg > MIN(Page)) DO
                       DEC (pg);
                   END (*WHILE*);
                   hini := INIData.OpenINIFile (INIFileName, UseTNI);
                   app := "StartingPage";
                   key := "MainNotebook";
                   INIData.INIPut (hini, app, key, pg);
                   INIData.CloseINIFile (hini);
                   StoreWindowPosition (hwnd, "MainNotebook", FALSE);
                   SUPage1.StoreData (pagehandle[pbase], MultiDomain);
                   IMAPPage.StoreData (pagehandle[pimap]);
                   SULogging.StoreData (pagehandle[plog]);
                   Filter.StoreData;
                   OptionP1.StoreData;
                   OptionP2.StoreData;
                   OptionP3.StoreData;
                   ChunkingPage.StoreData;
                   RelayPage.StoreData;
                   IF MultiDomain THEN
                       DomainPage.StoreData;
                   ELSE
                       UserPage.Close(OS2.WinWindowFromID(hwnd, DID.notebook),
                                                            pagehandle[puser]);
                       HostLists.StoreData(local);
                   END (*IF*);
                   HostLists.StoreData (whitelisted);
                   HostLists.StoreData (mayrelay);
                   HostLists.StoreData (relaydest);
                   HostLists.StoreData (banned);
                   BlackLists.StoreData (pagehandle[pblack]);
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END MainDialogueProc;

(**************************************************************************)

PROCEDURE OpenBigFrame (owner: OS2.HWND;  TNImode: BOOLEAN);

    (* Creates the main dialogue box. *)

    VAR hini: INIData.HINI;
        app: ARRAY [0..12] OF CHAR;
        key: ARRAY [0..16] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        IF UseTNI THEN
           INIFileName := "Setup.TNI";
        ELSE
           INIFileName := "Setup.INI";
        END (*IF*);
        ChangeInProgress := FALSE;
        CommonSettings.EnableFontChanges(FALSE);

        (* Set the initial tab font and starting page. *)

        hini := INIData.OpenINIFile (INIFileName, UseTNI);
        app := "Font";
        key := "MainNotebookTabs";
        IF NOT INIData.INIGetString (hini, app, key, TabFontName) THEN
            TabFontName := "8.Helv";
        END (*IF*);
        app := "StartingPage";
        key := "MainNotebook";
        IF (NOT INIData.INIGet(hini, app, key, StartingPage))
                   OR (StartingPage > MAX(Page)) THEN
            StartingPage := MIN(Page);
        END (*IF*);
        INIData.CloseINIFile (hini);

        (* Get a few important options from Weasel INI file. *)

        EVAL (RINIData.OpenINIFile ('WEASEL.INI', UseTNI));
        SUDomains.SetMasterININame ('WEASEL', UseTNI);
        SUDomains.LoadOriginalDomainName;
        IF RINIData.INIFetch ('$SYS', 'AcceptUnknown', AcceptUnknown) THEN
            RINIData.INIDeleteKey ('$SYS', 'AcceptUnknown');
        ELSE
            AcceptUnknown := FALSE;
        END (*IF*);
        IF NOT RINIData.INIFetch ('$SYS', 'MultiDomainEnabled', MultiDomain) THEN
            MultiDomain := FALSE;
        END (*IF*);
        InitialMultidomainSwitch := MultiDomain;
        SUDomains.StoreOriginalDomainName;
        RINIData.CloseINIFile;

        (* Execute the dialogue. *)

        OS2.WinDlgBox(OS2.HWND_DESKTOP, owner,
                       MainDialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.BigFrame,        (* dialogue ID *)
                       NIL);                (* creation parameters *)
    END OpenBigFrame;

(**************************************************************************)

VAR pg: Page;

BEGIN
    UseTNI := FALSE;
    INIFileName := "Setup.INI";
    AcceptUnknown := FALSE;
    ChangeInProgress := FALSE;
    InitialMultidomainSwitch := FALSE;
    PageFont := "";
    TabFontName := "";
    OurLanguage := "";
    LangCode := NIL;
    StartingPage := MIN(Page);
    FOR pg := MIN(Page) TO MAX(Page) DO
        IDofPage[pg] := 0;
    END (*FOR*);
END BigFrame.

