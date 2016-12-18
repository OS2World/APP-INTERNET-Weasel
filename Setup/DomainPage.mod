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

IMPLEMENTATION MODULE DomainPage;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*             The 'our domains' page of the notebook           *)
        (*                                                              *)
        (*        Started:        19 December 2001                      *)
        (*        Last edited:    7 September 2014                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings, OneLine, DomainEditor, SUDomains;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  INIPut, INIPutBinary, GetStringList, NextString, CloseStringList;

FROM Names IMPORT
    (* type *)  FilenameString, DomainName;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ToLower;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = SIZE(DomainName);

TYPE
    LabelString = ARRAY [0..9] OF CHAR;

VAR
    OurPageHandle, notebookhandle, hwndParent: OS2.HWND;
    Changed, ChangeInProgress, UseTNI: BOOLEAN;
    PageID: CARDINAL;
    MailRoot: FilenameString;
    AcceptUnknown: BOOLEAN;
    W4style: BOOLEAN;
    OurLang: LangHandle;

(************************************************************************)
(*                     ADDING OR REMOVING A DOMAIN                      *)
(************************************************************************)

PROCEDURE Add (name: DomainName);

    (* Creates a new domain.  Should be called only after the page      *)
    (* has been made visible by CreatePage.                             *)

    VAR index: INTEGER;
        hwnd: OS2.HWND;

    BEGIN
        SUDomains.CreateDomain (name);
        hwnd := OurPageHandle;
        index := OS2.LONGFROMMR(
                   OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_QUERYSELECTION, NIL, NIL));
        IF index = OS2.LIT_NONE THEN
            index := 0;
        ELSE
            INC(index);
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_INSERTITEM,
               OS2.MPFROMSHORT(index), ADR(name));
        OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_SELECTITEM,
               OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
        Changed := TRUE;
    END Add;

(************************************************************************)

PROCEDURE Remove (name: DomainName);

    (* Deletes a domain. *)

    VAR index, count: CARDINAL;
        hwnd: OS2.HWND;  current: DomainName;

    BEGIN
        hwnd := OS2.WinWindowFromID (OurPageHandle, DID.DomainList);

        (* Find the index of the name in the listbox. *)

        index := 0;
        count := OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        LOOP
            IF index >= count THEN
                EXIT (*LOOP*);
            END (*IF*);
            OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2USHORT(index, NameLength), ADR(current));
            IF Strings.Equal (current, name) THEN
                EXIT (*LOOP*);
            END (*IF*);
            INC (index);
        END (*LOOP*);

        IF index < count THEN
            SUDomains.DeleteDomain (name, hwnd, OurLang);
            hwnd := OurPageHandle;
            OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_DELETEITEM,
                                   OS2.MPFROMSHORT(index), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteDomain), FALSE);
            Changed := TRUE;
        END (*IF*);

    END Remove;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Domains.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Domains.revert", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RevertToSingleDomain, stringval);
        StrToBuffer (lang, "Domains.Label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DomainPageLabel, stringval);
        StrToBuffer (lang, "Domains.explain", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DomainPageHint, stringval);
        StrToBuffer (lang, "Buttons.Add", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AddDomain, stringval);
        StrToBuffer (lang, "Buttons.Rename", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RenameDomain, stringval);
        StrToBuffer (lang, "Buttons.Edit", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.EditDomain, stringval);
        StrToBuffer (lang, "Buttons.Promote", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.PromoteDomain, stringval);
        StrToBuffer (lang, "Buttons.Delete", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DeleteDomain, stringval);
    END SetLanguage;

(************************************************************************)
(*                 MOVING DATA TO AND FROM THE INI FILE                 *)
(************************************************************************)

PROCEDURE StoreList (hwnd: OS2.HWND);

    (* Stores the list of domain names into the INI file.  The hwnd     *)
    (* parameter is the handle of the listbox.                          *)

    VAR bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        j, k, count, index: CARDINAL;
        name: DomainName;

    BEGIN
        (* Work out how much buffer space we need. *)

        BufferSize := 0;
        count := OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        IF count > 0 THEN
            FOR index := 0 TO count-1 DO
                INC (BufferSize,
                     OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXTLENGTH,
                                   OS2.MPFROMUSHORT(index), NIL)) + 1);
            END (*FOR*);
        END (*IF*);

        (* Create the string buffer. *)

        IF BufferSize = 0 THEN
            bufptr := NIL;
        ELSE
            INC (BufferSize);
            ALLOCATE (bufptr, BufferSize);
        END (*IF*);

        (* Store all the strings into the buffer. *)

        IF count > 0 THEN
            j := 0;
            FOR index := 0 TO count-1 DO
                OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2USHORT(index, NameLength), ADR(name));
                k := 0;
                REPEAT
                    bufptr^[j] := name[k];
                    INC (k);  INC (j);
                UNTIL (name[k] = Nul) OR (k = NameLength);
                bufptr^[j] := Nul;
                INC (j);
            END (*FOR*);

            bufptr^[j] := Nul;

        END (*IF*);

        (* Write the buffer to the INI file. *)

        OpenINIFile;
        IF BufferSize = 0 THEN
            INIPutBinary ("$SYS", "Domains", j, 0);
        ELSE
            INIPutBinary ("$SYS", "Domains", bufptr^, BufferSize);
        END (*IF*);
        CloseINIFile;

        (* Deallocate the buffer space. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

    END StoreList;

(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the listbox with data from the INI file. *)

    TYPE ListOfDomains = POINTER TO RECORD
                                        next: ListOfDomains;
                                        this: DomainName;
                                    END (*RECORD*);

    VAR name: DomainName;
        state: StringReadState;
        head, next: ListOfDomains;

    BEGIN
        (* Special case: the "revert to single domain" checkbox is      *)
        (* always unchecked when this page is visible.  As soon as the  *)
        (* user checks it, we navigate away from this page, so there is *)
        (* no need to save its state in the INI file.                   *)

        OS2.WinSendDlgItemMsg (hwnd, DID.RevertToSingleDomain, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(0), NIL);
        head := NIL;
        OpenINIFile;

        (* Load the list of domain names from the INI file. *)

        GetStringList ("$SYS", "Domains", state);
        REPEAT
            NextString (state, name);
            IF name[0] <> Nul THEN
                ToLower (name);
                OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_END), ADR(name));
                IF AcceptUnknown THEN
                    NEW (next);
                    next^.next := head;
                    next^.this := name;
                    head := next;
                END (*IF*);
            END (*IF*);
        UNTIL name[0] = Nul;
        CloseStringList (state);

        CloseINIFile;

        (* Special case: if we are migrating from a version that had    *)
        (* the 'accept mail for unknown users' option, open and close   *)
        (* all domains so that the alias lists can be updated.          *)

        WHILE head <> NIL DO
            next := head^.next;
            DomainEditor.Edit (hwnd, head^.this, MailRoot,
                                         TRUE, TRUE, W4style, UseTNI);
            DISPOSE (head);
            head := next;
        END (*WHILE*);

    END LoadValues;

(************************************************************************)

PROCEDURE StoreData;

    (* Stores back the domain list if it has changed. *)

    BEGIN
        IF Changed THEN
            StoreList(OS2.WinWindowFromID(OurPageHandle,DID.DomainList));
        END (*IF*);
    END StoreData;

(**************************************************************************)

PROCEDURE AreYouSure (count: CARDINAL): BOOLEAN;

    (* Asks "are you sure you want to delete this many users?" *)

    VAR confirmed: BOOLEAN;
        title, message: ARRAY [0..255] OF CHAR;

    BEGIN
        IF count < 2 THEN
            confirmed := TRUE;
        ELSE
            StrToBufferN (OurLang, "Domains.willdelete", count, message);
            StrToBuffer (OurLang, "Domains.areyousure", title);
            confirmed := OS2.WinMessageBox (OS2.HWND_DESKTOP, OurPageHandle,
                             message, title, 0,
                             OS2.MB_OKCANCEL + OS2.MB_WARNING) = OS2.MBID_OK;
        END (*IF*);
        RETURN confirmed;
    END AreYouSure;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd     : OS2.HWND
                                   ;msg      : OS2.ULONG
                                   ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        listwindow: OS2.HWND;
        oldname, name: DomainName;
        text: ARRAY [0..127] OF CHAR;

    BEGIN

        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteDomain), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteDomain), FALSE);
            LoadValues (hwnd);
            RETURN NIL;
        END (*IF*);

        listwindow := OS2.WinWindowFromID(hwnd,DID.DomainList);
        index := OS2.LONGFROMMR(
                   OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_QUERYSELECTION, NIL, NIL));
        IF index = OS2.LIT_NONE THEN
            name := "";
        ELSE
            OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                            OS2.MPFROM2USHORT(index, NameLength), ADR(name));
        END (*IF*);

        IF msg = OS2.WM_COMMAND THEN

            ButtonID := OS2.SHORT1FROMMP(mp1);

            IF ButtonID = DID.AddDomain THEN
                   name := "";
                   StrToBuffer (OurLang, "Domains.EnterName", text);
                   OneLine.Edit (hwnd, text, name, UseTNI);
                   IF name[0] <> Nul THEN
                       ToLower (name);
                       Add (name);
                       DomainEditor.Edit (hwnd, name, MailRoot,
                                             FALSE, FALSE, W4style, UseTNI);
                   END (*IF*);
                   Changed := TRUE;

            ELSIF ButtonID = DID.RenameDomain THEN

                   oldname := name;
                   StrToBuffer (OurLang, "Domains.NewName", text);
                   OneLine.Edit (hwnd, text, name, UseTNI);
                   ToLower (name);
                   SUDomains.RenameDomain (oldname, name);
                   OS2.WinSendDlgItemMsg (hwnd, DID.DomainList,
                           OS2.LM_SETITEMTEXT, OS2.MPFROMSHORT(index), ADR(name));
                   Changed := TRUE;

            ELSIF ButtonID = DID.EditDomain THEN

                   DomainEditor.Edit (hwnd, name, MailRoot,
                                         FALSE, FALSE, W4style, UseTNI);

            ELSIF ButtonID = DID.PromoteDomain THEN

                   OS2.WinSendMsg (listwindow, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   DEC (index);
                   OS2.WinSendMsg (listwindow, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendMsg (listwindow, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   Changed := TRUE;

            ELSIF ButtonID = DID.DeleteDomain THEN

                   IF AreYouSure (SUDomains.UserCount(name)) THEN
                       SUDomains.DeleteDomain (name, hwnd, OurLang);
                       OS2.WinSendDlgItemMsg (hwnd, DID.DomainList, OS2.LM_DELETEITEM,
                                              OS2.MPFROMSHORT(index), NIL);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameDomain), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditDomain), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteDomain), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteDomain), FALSE);
                       Changed := TRUE;
                   END (*IF*);

            END (*IF*);
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
            IF (ButtonID = DID.RevertToSingleDomain)
                             AND (NotificationCode = OS2.BN_CLICKED) THEN
                OS2.WinPostMsg (hwndParent, CommonSettings.WM_MULTIDOMAIN_CHANGE,
                                OS2.MPFROMLONG(0), NIL);
                RETURN NIL;

            ELSIF ButtonID = DID.DomainList THEN
                IF NotificationCode = OS2.LN_SELECT THEN

                    (* For some reason the more obvious code doesn't work below, so     *)
                    (* we have to use an if/then/else construct.                        *)

                    IF index > 0 THEN
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteDomain), TRUE);
                    ELSE
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteDomain), FALSE);
                    END (*IF*);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameDomain), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditDomain), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteDomain), TRUE);
                    RETURN NIL;
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditDomain), NIL);
                    RETURN NIL;
                ELSE
                    RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                END (*IF*);
            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*IF*);

        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE SetMailRoot (CurrentMailRoot: ARRAY OF CHAR);

    (* The caller tells us what the global mail root directory is.    *)

    BEGIN
        Strings.Assign (CurrentMailRoot, MailRoot);
    END SetMailRoot;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  AfterPage: CARDINAL;
                             CreateWildAliases, NewStyle, TNImode: BOOLEAN;
                                      VAR (*OUT*) ID: CARDINAL): OS2.HWND;

    (* Creates the domain list page, and adds it to the notebook.  *)

    VAR Label: LabelString;

    BEGIN
        UseTNI := TNImode;
        W4style := NewStyle;
        notebookhandle := notebook;
        Changed := FALSE;
        AcceptUnknown := CreateWildAliases;
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.DomainPage,  (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook,
                         OS2.BKM_INSERTPAGE, CAST (ADDRESS, AfterPage),
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_NEXT)));
        StrToBuffer (OurLang, "Domains.tab", Label);
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

PROCEDURE Close (notebook: OS2.HWND);

    (* Shuts down this window and removes it from the notebook. *)

    BEGIN
        OS2.WinSendMsg (notebook, OS2.BKM_DELETEPAGE,
                        CAST(ADDRESS, PageID),
                        OS2.MPFROMLONG (OS2.BKA_SINGLE));
        StoreData;
        OS2.WinSendMsg (OurPageHandle, OS2.WM_CLOSE, NIL, NIL);
    END Close;

(**************************************************************************)

BEGIN
    UseTNI := FALSE;
    MailRoot := "";
    AcceptUnknown := FALSE;
    ChangeInProgress := FALSE;
    OurPageHandle := OS2.NULLHANDLE;
    notebookhandle := OS2.NULLHANDLE;
END DomainPage.

