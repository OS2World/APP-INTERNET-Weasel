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

IMPLEMENTATION MODULE AliasPage;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                  Alias page of the notebook                  *)
        (*                                                              *)
        (*        Started:        8 July 1999                           *)
        (*        Last edited:    16 December 2009                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, PMInit, CommonSettings, EditAlias, OneLine;

FROM EditUser IMPORT
    (* proc *)  NameClash;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  INIDeleteKey, INIRenameKey,
                INIGetString, INIPutString, INIPutBinary,
                GetStringList, NextString, CloseStringList;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, ToLower;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    ChangeInProgress: BOOLEAN;
    OurPageHandle, notebookhandle: OS2.HWND;
    PageID: CARDINAL;
    AddWild, UseTNI: BOOLEAN;
    MaxLineNum: INTEGER;
    OurFontGroup: CommonSettings.FontGroup;
    OurLang: LangHandle;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Aliases.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(stringval));
        StrToBuffer (lang, "Aliases.Label", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AliasPageLabel, stringval);
        StrToBuffer (lang, "Buttons.Add", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AddAliasButton, stringval);
        StrToBuffer (lang, "Buttons.Rename", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.RenameAliasButton, stringval);
        StrToBuffer (lang, "Buttons.Edit", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.EditAliasButton, stringval);
        StrToBuffer (lang, "Buttons.Promote", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.PromoteAliasButton, stringval);
        StrToBuffer (lang, "Buttons.Demote", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DemoteAliasButton, stringval);
        StrToBuffer (lang, "Buttons.Delete", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DeleteAliasButton, stringval);
    END SetLanguage;

(************************************************************************)
(*                 MOVING DATA TO AND FROM THE INI FILE                 *)
(************************************************************************)

PROCEDURE UpdateIndices (hwnd: OS2.HWND;  from: CARDINAL);

    (* Updates the indices of aliases number from, from+1, etc. *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;

    BEGIN
        LOOP
            OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_QUERYITEMTEXT,
                          OS2.MPFROM2SHORT(from, NameLength), ADR(name));
            IF name[0] = Nul THEN
                EXIT (*LOOP*);
            END (*IF*);
            EditAlias.SetNumber (from, name);
            INC (from);
        END (*LOOP*);
    END UpdateIndices;

(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the listbox with all aliases in the INI file.  *)

    TYPE LineMap = POINTER TO
                       RECORD
                           prev, next: LineMap;
                           seqnum: CARDINAL;   (* sequence # *)
                           lineno: CARDINAL;   (* position in listbox *)
                       END (*RECORD*);

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        state: StringReadState;
        seq, itemcount: CARDINAL;
        MissingNumber: BOOLEAN;
        map, p, previous: LineMap;

    BEGIN
        MissingNumber := FALSE;
        map := NIL;
        OpenINIFile;

        (* Pick up the list of all aliases. *)

        GetStringList ("$ALIAS", "", state);
        itemcount := 0;
        REPEAT
            NextString (state, name);
            IF name[0] <> Nul THEN

                IF Strings.Equal (name, '*') THEN
                    AddWild := FALSE;
                END (*IF*);

                (* Add name to the listbox.  The order in which we      *)
                (* read the names is not the same as the order in which *)
                (* they will be displayed, so we maintain a linear list *)
                (* which maps between the two.                          *)

                seq := EditAlias.GetNumber (name);
                MissingNumber := MissingNumber OR (seq = EditAlias.MaxAliasNumber);
                NEW (p);
                p^.seqnum := seq;

                (* Adjust the pointers such that the correct     *)
                (* insertion point is between previous and map.  *)

                IF map = NIL THEN
                    previous := NIL;
                ELSE
                    previous := map^.prev;
                    WHILE (previous <> NIL) AND (seq < previous^.seqnum) DO
                        map := previous;  previous := map^.prev;
                    END (*WHILE*);
                    WHILE (map <> NIL) AND (seq >= map^.seqnum) DO
                        previous := map;  map := map^.next;
                    END (*WHILE*);
                END (*IF*);

                (* Do the insertion. *)

                p^.prev := previous;
                p^.next := map;
                IF previous <> NIL THEN
                    previous^.next := p;
                END (*IF*);
                IF map = NIL THEN
                    IF previous = NIL THEN
                        p^.lineno := 0;
                    ELSE
                        p^.lineno := previous^.lineno + 1;
                    END (*IF*);
                ELSE
                    map^.prev := p;
                    p^.lineno := map^.lineno;
                END (*IF*);

                (* Adjust the line numbers of all following entries. *)

                map := p;
                p := p^.next;
                seq := map^.lineno;
                WHILE p <> NIL DO
                    INC (seq);  p^.lineno := seq;
                    p := p^.next;
                END (*WHILE*);

                OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(map^.lineno), ADR(name));
                INC (itemcount);

            END (*IF*);
        UNTIL name[0] = Nul;
        CloseStringList (state);
        CloseINIFile;

        (* Dispose of the map we've constructed. *)

        IF map <> NIL THEN
            WHILE map^.next <> NIL DO
                map := map^.next;
            END (*WHILE*);
            WHILE map <> NIL DO
                previous := map^.prev;
                DISPOSE (map);
                map := previous;
            END (*WHILE*);
        END (*IF*);

        (* Correction to upgrade old INI file format that did not       *)
        (* have sequence numbers for aliases.                           *)

        IF MissingNumber THEN
            UpdateIndices (hwnd, 0);
        END (*IF*);

        (* Add a wildcard entry if necessary. *)

        IF AddWild THEN
            name := "    unknown";
            name[0] := CHR(1);
            name[1] := CHR(3);
            name[2] := CHR(itemcount MOD 256);
            name[3] := CHR(itemcount DIV 256);
            name[11] := CHR(0);
            name[12] := CHR(0);
            OpenINIFile;
            INIPutBinary ('$ALIAS', '*', name, 13);
            IF NOT INIGetString ('unknown', 'Password', name) THEN
                name[0] := Nul;
                INIPutString ('unknown', 'Password', name);
            END (*IF*);
            CloseINIFile;
            name := '*';
            INC (itemcount);
            OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_INSERTITEM,
                         OS2.MPFROMSHORT(OS2.LIT_END), ADR(name));
        END (*IF*);

        (* Set the global variable MaxLineNum.  *)

        MaxLineNum := itemcount;
        IF MaxLineNum = 0 THEN
            MaxLineNum := OS2.LIT_NONE;
        ELSE
            DEC(MaxLineNum);
        END (*IF*);

    END LoadValues;

(************************************************************************)

PROCEDURE AliasMatch (name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name matches (possibly as a wildcard match) an  *)
    (* existing alias.  Alphabetic case is not significant.  We assume  *)
    (* that the INI file is already open.                               *)

    VAR alias: ARRAY [0..NameLength-1] OF CHAR;
        state: StringReadState;

    BEGIN
        GetStringList ("$ALIAS", "", state);
        REPEAT
            NextString (state, alias);
        UNTIL (alias[0] = Nul) OR WildMatch (name, alias);
        CloseStringList (state);
        RETURN alias[0] <> Nul;
    END AliasMatch;

(************************************************************************)

PROCEDURE Close (notebook, hwnd: OS2.HWND);

    (* Shuts down this window and removes it from the notebook. *)

    BEGIN
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
        index: INTEGER;  clash: [0..2];
        listwindow: OS2.HWND;
        oldname, name: ARRAY [0..NameLength-1] OF CHAR;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        IF msg = OS2.WM_INITDLG THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditAliasButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameAliasButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteAliasButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DemoteAliasButton), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteAliasButton), FALSE);
            LoadValues (hwnd);
            RETURN NIL;
        END (*IF*);

        index := OS2.LONGFROMMR(
                  OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_QUERYSELECTION, NIL, NIL));

        IF msg = OS2.WM_COMMAND THEN

            listwindow := OS2.WinWindowFromID(hwnd,DID.aliaslist);
            OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_QUERYITEMTEXT,
                          OS2.MPFROM2SHORT(index, NameLength), ADR(name));
            CASE OS2.SHORT1FROMMP(mp1) OF

              | DID.AddAliasButton:
                   name := "";
                   StrToBuffer (OurLang, "Aliases.EnterName", message);
                   OneLine.Edit (hwnd, message, name, UseTNI);
                   ToLower (name);
                   IF name[0] <> Nul THEN
                       OpenINIFile;
                       clash := NameClash(name);
                       CloseINIFile;
                       IF clash = 0 THEN
                           IF index = OS2.LIT_NONE THEN
                               index := 0;
                           ELSE
                               INC(index);
                           END (*IF*);
                           IF MaxLineNum = OS2.LIT_NONE THEN
                               MaxLineNum := 0;
                           ELSE
                               INC(MaxLineNum);
                           END (*IF*);
                           OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_INSERTITEM,
                                  OS2.MPFROMSHORT(index), ADR(name));
                           OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_SELECTITEM,
                                  OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                           EditAlias.Edit(listwindow, name, OurLang, UseTNI);
                           UpdateIndices (hwnd, index);
                       ELSIF clash = 1 THEN
                           StrToBuffer (OurLang, "Aliases.UserExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       ELSE
                           StrToBuffer (OurLang, "Aliases.AliasExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       END (*IF*);
                   END (*IF*);

              | DID.EditAliasButton:
                   EditAlias.Edit(listwindow, name, OurLang, UseTNI);

              | DID.RenameAliasButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   Strings.Assign (name, oldname);
                   StrToBuffer (OurLang, "Aliases.NewName", message);
                   OneLine.Edit (hwnd, message, name, UseTNI);
                   ToLower (name);
                   IF NOT Strings.Equal (name, oldname) AND (name[0] <> Nul) THEN
                       OpenINIFile;
                       clash := NameClash(name);
                       IF clash = 0 THEN
                           INIRenameKey ("$ALIAS", oldname, name);
                           OS2.WinSendMsg (listwindow, OS2.LM_SETITEMTEXT,
                                       OS2.MPFROMUSHORT(index), ADR(name));
                       ELSIF clash = 1 THEN
                           StrToBuffer (OurLang, "Aliases.UserExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       ELSE
                           StrToBuffer (OurLang, "Aliases.AliasExists", message);
                           PMInit.MessageBox (hwnd, message, OS2.MB_OK, TRUE);
                       END (*IF*);
                       CloseINIFile;
                   END (*IF*);

              | DID.PromoteAliasButton:

                   OS2.WinSendMsg (listwindow, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   DEC (index);
                   OS2.WinSendMsg (listwindow, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendMsg (listwindow, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   UpdateIndices (hwnd, index);

              | DID.DemoteAliasButton:

                   OS2.WinSendMsg (listwindow, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   INC (index);
                   OS2.WinSendMsg (listwindow, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendMsg (listwindow, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   UpdateIndices (hwnd, index-1);

              | DID.DeleteAliasButton:
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, 32), ADR(name));
                   OpenINIFile;
                   INIDeleteKey ("$ALIAS", name);
                   CloseINIFile;
                   OS2.WinSendDlgItemMsg (hwnd, DID.aliaslist, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   IF MaxLineNum = 0 THEN
                       MaxLineNum := OS2.LIT_NONE;
                   ELSE
                       DEC(MaxLineNum);
                   END (*IF*);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditAliasButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameAliasButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteAliasButton), FALSE);
                   UpdateIndices (hwnd, index);

            ELSE
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            END (*CASE*);
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

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = DID.aliaslist THEN
                IF NotificationCode = OS2.LN_SELECT THEN
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.EditAliasButton), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.RenameAliasButton), TRUE);
                    IF index = 0 THEN
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteAliasButton), FALSE);
                    ELSE
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.PromoteAliasButton), TRUE);
                    END (*IF*);
                    IF index = MaxLineNum THEN
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DemoteAliasButton), FALSE);
                    ELSE
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DemoteAliasButton), TRUE);
                    END (*IF*);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteAliasButton), TRUE);
                    RETURN NIL;
                ELSIF NotificationCode = OS2.LN_ENTER THEN
                    (* Treat this one as if the edit button had been clicked. *)
                    OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                          OS2.MPFROMSHORT(DID.EditAliasButton), NIL);
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

(************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  AfterPage: CARDINAL;
                                AddWildcardAlias, TNImode: BOOLEAN;
                                group: CommonSettings.FontGroup;
                                VAR (*OUT*) ID: CARDINAL): OS2.HWND;

    (* Creates the alias page and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        notebookhandle := notebook;
        OurFontGroup := group;
        AddWild := AddWildcardAlias;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.aliases,                (* dialogue ID *)
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
        StrToBuffer (OurLang, "Aliases.tab", Label);
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,OurPageHandle));
        ID := PageID;
        RETURN OurPageHandle;
    END CreatePage;

(************************************************************************)

PROCEDURE SetFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on this page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (OurPageHandle, OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetFont;

(**************************************************************************)

BEGIN
    UseTNI := FALSE;
    OurFontGroup := CommonSettings.NilFontGroup;
    ChangeInProgress := FALSE;
    OurPageHandle := OS2.NULLHANDLE;
    notebookhandle := OS2.NULLHANDLE;
    AddWild := FALSE;
    MaxLineNum := OS2.LIT_NONE;
END AliasPage.

