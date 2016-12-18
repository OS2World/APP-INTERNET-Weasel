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

IMPLEMENTATION MODULE EditAlias;

        (************************************************************)
        (*                                                          *)
        (*                    PM Setup for Weasel                   *)
        (*            Dialogue to edit an alias definition          *)
        (*                                                          *)
        (*    Started:        11 July 1999                          *)
        (*    Last edited:    14 April 2012                         *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT SYSTEM, OS2, DID, Strings, WSUINI, INIData, RINIData, OneLine;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferA;

FROM Inet2Misc IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  ToLower;

FROM LowLevel IMPORT
    (* proc *)  CopyUp, AddOffset, EVAL;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

VAR
    INIFileName: ARRAY [0..9] OF CHAR;
    AliasName: ARRAY [0..NameLength-1] OF CHAR;
    AliasNumber: CARDINAL;
    OurLang: LangHandle;
    UseTNI: BOOLEAN;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (OurPageHandle: OS2.HWND;  lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Editalias.private", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.CheckPrivate, stringval);
        StrToBuffer (lang, "Buttons.Add", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AddButton, stringval);
        StrToBuffer (lang, "Buttons.Revise", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.ReviseButton, stringval);
        StrToBuffer (lang, "Buttons.Delete", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.DeleteItemButton, stringval);
        StrToBuffer (lang, "Buttons.OK", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.ADOK, stringval);
        StrToBuffer (lang, "Buttons.Cancel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.ADCancel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadExpansion (hwnd: OS2.HWND): BOOLEAN;

    (* Fills the namebox for the current alias with data from the INI   *)
    (* file, or loads default values if they're not in the INI file.    *)
    (* Returns TRUE iff this is a private alias.                        *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        j, k: CARDINAL;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        Private: BOOLEAN;

    BEGIN
        WSUINI.OpenINIFile;

        (* Load an alias list from the INI file. *)

        bufptr := NIL;
        EVAL (RINIData.ItemSize ("$ALIAS", AliasName, BufferSize));
        IF BufferSize > 0 THEN
            ALLOCATE (bufptr, BufferSize);
            IF NOT RINIData.INIFetchBinary ("$ALIAS", AliasName, bufptr^, BufferSize) THEN
                bufptr^[0] := Nul;
            END (*IF*);
        END (*IF*);
        Private := (bufptr <> NIL) AND (bufptr^[0] = CHR(0));
        j := 1;

        (* Extract the alias number, if present. *)

        IF BufferSize = 0 THEN
            AliasNumber := MaxAliasNumber;
        ELSIF bufptr^[1] = CHR(3) THEN
            AliasNumber := ORD(bufptr^[2]) + 256*ORD(bufptr^[3]);
            j := 4;
        ELSE
            AliasNumber := MaxAliasNumber;
        END (*IF*);

        (* Each time around this loop we extract one e-mail address. *)

        WHILE (j < BufferSize) AND (bufptr^[j] <> Nul) DO
            k := 0;
            REPEAT
                name[k] := bufptr^[j];
                INC (k);  INC (j);
            UNTIL (j >= BufferSize) OR (bufptr^[j-1] = Nul)
                                    OR (k >= NameLength);
            IF k < NameLength THEN
                name[k] := Nul;
            END (*IF*);

            (* Add name to the listbox. *)

            OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_SORTASCENDING), SYSTEM.ADR(name));

        END (*WHILE*);

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

        WSUINI.CloseINIFile;
        RETURN Private;

    END LoadExpansion;

(**************************************************************************)

PROCEDURE SaveExpansion (Public: BOOLEAN;  hwnd: OS2.HWND);

    (* Saves the alias definition back to the INI file. *)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        j, k, N, count: CARDINAL;
        bufptr: CharArrayPointer;
        BufferSize: CARDINAL;

    BEGIN
        WSUINI.OpenINIFile;

        (* Work out how much buffer space we need. *)

        BufferSize := 5;
        count := OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMCOUNT, NIL, NIL));
        IF count > 0 THEN
            FOR N := 0 TO count-1 DO
                INC (BufferSize,
                     OS2.ULONGFROMMR(OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXTLENGTH,
                                   OS2.MPFROMUSHORT(N), NIL)) + 1);
            END (*FOR*);
        END (*IF*);

        (* Create the string buffer. *)

        ALLOCATE (bufptr, BufferSize);

        (* Store the public/private flag. *)

        IF Public THEN bufptr^[0] := CHR(1)
        ELSE bufptr^[0] := CHR(0)
        END (*IF*);

        (* Store the index. *)

        bufptr^[1] := CHR(3);
        bufptr^[2] := CHR(AliasNumber MOD 256);
        bufptr^[3] := CHR(AliasNumber DIV 256);
        j := 4;

        (* Store the list of names. *)

        IF count > 0 THEN
            FOR N := 0 TO count-1 DO
                OS2.WinSendMsg (hwnd, OS2.LM_QUERYITEMTEXT,
                                    OS2.MPFROM2SHORT(N, NameLength), SYSTEM.ADR(name));
                k := 0;
                LOOP
                    IF j >= BufferSize THEN EXIT(*LOOP*); END(*IF*);
                    bufptr^[j] := name[k];
                    INC (j);
                    IF k >= NameLength THEN
                        IF j >= BufferSize THEN EXIT(*LOOP*); END(*IF*);
                        bufptr^[j] := Nul;
                        INC (j);
                        EXIT (*LOOP*);
                    ELSIF name[k] = Nul THEN
                        EXIT (*LOOP*);
                    END (*IF*);
                    INC (k);
                END (*LOOP*);

            END (*FOR*);
        END (*IF*);

        IF j < BufferSize THEN
            bufptr^[j] := Nul;  INC(j);
        END (*IF*);

        RINIData.INIPutBinary ("$ALIAS", AliasName, bufptr^, j);
        DEALLOCATE (bufptr, BufferSize);
        WSUINI.CloseINIFile;

    END SaveExpansion;

(**************************************************************************)

PROCEDURE GetNumber (name: ARRAY OF CHAR): CARDINAL;

    (* Returns the ordinal number of alias 'name'.  If it does not yet  *)
    (* have a defined number, returns MaxAliasNumber.                   *)
    (* We assume that the INI file is already open.                     *)

    VAR BufferSize, result: CARDINAL;
        bufptr: CharArrayPointer;

    BEGIN
        (* Load the alias list from the INI file. *)

        bufptr := NIL;
        EVAL (RINIData.ItemSize ("$ALIAS", name, BufferSize));
        IF BufferSize > 0 THEN
            ALLOCATE (bufptr, BufferSize);
            IF NOT RINIData.INIFetchBinary ("$ALIAS", name, bufptr^, BufferSize) THEN
                bufptr^[0] := Nul;
            END (*IF*);
        END (*IF*);

        (* Extract the alias number, if present. *)

        IF (BufferSize < 4) OR (bufptr^[1] <> CHR(3)) THEN
            result := MaxAliasNumber;
        ELSE
            result := ORD(bufptr^[2]) + 256*ORD(bufptr^[3]);
        END (*IF*);

        (* Deallocate the buffer. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

        RETURN result;

    END GetNumber;

(**************************************************************************)

PROCEDURE SetNumber (newnumber: CARDINAL;  name: ARRAY OF CHAR);

    (* Updates the ordinal number of alias 'name'. *)

    VAR BufferSize: CARDINAL;
        bufptr, bufptr2: CharArrayPointer;

    BEGIN
        WSUINI.OpenINIFile;

        (* Load the alias list from the INI file. *)

        bufptr := NIL;
        EVAL ( RINIData.ItemSize ("$ALIAS", name, BufferSize));
        IF BufferSize > 0 THEN
            ALLOCATE (bufptr, BufferSize);
            IF NOT RINIData.INIFetchBinary ("$ALIAS", name, bufptr^, BufferSize) THEN
                bufptr^[0] := Nul;
            END (*IF*);
        END (*IF*);

        (* Expand buffer if alias number not already present. *)

        IF BufferSize = 0 THEN
            ALLOCATE (bufptr, 4);
            bufptr^[0] := CHR(1);
            BufferSize := 4;
        ELSIF (BufferSize < 4) OR (bufptr^[1] <> CHR(3)) THEN
            ALLOCATE (bufptr2, BufferSize+3);
            bufptr2^[0] := bufptr^[0];
            CopyUp (AddOffset(bufptr,1), AddOffset(bufptr2,4), BufferSize-1);
            DEALLOCATE (bufptr, BufferSize);
            bufptr := bufptr2;
            INC (BufferSize, 3);
        END (*IF*);

        (* Insert the number. *)

        bufptr^[1] := CHR(3);
        bufptr^[2] := CHR(newnumber MOD 256);
        bufptr^[3] := CHR(newnumber DIV 256);

        (* Write the result back, then deallocate the buffer    *)
        (* and close the INI file.                              *)

        RINIData.INIPutBinary ("$ALIAS", name, bufptr^, BufferSize);
        DEALLOCATE (bufptr, BufferSize);
        WSUINI.CloseINIFile;

    END SetNumber;

(************************************************************************)
(*                    THE ALIAS DEFINITION DIALOGUE                     *)
(************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    (* Message handler for the setup dialogue. *)

    VAR NotificationCode, ButtonID: CARDINAL;
        Private: BOOLEAN;  listwindow: OS2.HWND;
        index: INTEGER;  button: CARDINAL;
        name: ARRAY [0..NameLength-1] OF CHAR;
        message: ARRAY [0..127] OF CHAR;

    BEGIN
        index := OS2.LONGFROMMR(
                   OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_QUERYSELECTION, NIL, NIL));
        CASE msg OF
           |  OS2.WM_INITDLG:
                   Private := LoadExpansion (hwnd);
                   OS2.WinSendDlgItemMsg (hwnd, DID.CheckPrivate, OS2.BM_SETCHECK,
                         OS2.MPFROMSHORT(ORD(Private)), NIL);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseButton), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteItemButton), FALSE);
                   RETURN NIL;

           |  OS2.WM_COMMAND:
                   listwindow := OS2.WinWindowFromID(hwnd,DID.namebox);
                   CASE OS2.SHORT1FROMMP(mp1) OF

                     | DID.AddButton:
                       name := "";
                       StrToBuffer (OurLang, "Editalias.EnterAddress", message);
                       OneLine.Edit (hwnd, message, name, UseTNI);
                       ToLower (name);
                       IF name[0] <> Nul THEN
                           IF index = OS2.LIT_NONE THEN
                               index := 0;
                           ELSE
                               INC(index);
                           END (*IF*);
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_INSERTITEM,
                                  OS2.MPFROMSHORT(index), SYSTEM.ADR(name));
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_SELECTITEM,
                                  OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                       END (*IF*);

                     | DID.ReviseButton:
                       OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2SHORT(index, NameLength), SYSTEM.ADR(name));
                       StrToBuffer (OurLang, "Editalias.EditAddress", message);
                       OneLine.Edit (hwnd, message, name, UseTNI);
                       ToLower (name);
                       IF name[0] <> Nul THEN
                           OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_SETITEMTEXT,
                                  OS2.MPFROMSHORT(index), SYSTEM.ADR(name));
                       END (*IF*);

                     | DID.DeleteItemButton:

                       OS2.WinSendDlgItemMsg (hwnd, DID.namebox, OS2.LM_DELETEITEM,
                                              OS2.MPFROMSHORT(index), NIL);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseButton), FALSE);
                       OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteItemButton), FALSE);

                     | DID.ADOK:

                       Private := OS2.LONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.CheckPrivate,
                                           OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                       SaveExpansion (NOT Private, listwindow);
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);

                     | DID.ADCancel:
                       OS2.WinPostMsg (hwnd, OS2.WM_CLOSE, NIL, NIL);

                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*CASE*);

                   RETURN NIL;

          |  OS2.WM_CONTROL:

                   NotificationCode := OS2.ULONGFROMMP(mp1);
                   ButtonID := NotificationCode MOD 65536;
                   NotificationCode := NotificationCode DIV 65536;
                   IF ButtonID = DID.namebox THEN
                       IF NotificationCode = OS2.LN_SELECT THEN
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.ReviseButton), TRUE);
                           OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.DeleteItemButton), TRUE);
                           RETURN NIL;
                       ELSIF NotificationCode = OS2.LN_ENTER THEN
                           (* Treat this one as if the revise or add button had *)
                           (* been clicked, depending on whether an item is     *)
                           (* selected already.                                 *)
                           IF index = OS2.LIT_NONE THEN
                               button := DID.AddButton;
                           ELSE
                               button := DID.ReviseButton;
                           END (*IF*);
                           OS2.WinSendMsg (hwnd, OS2.WM_COMMAND,
                                 OS2.MPFROMULONG(button), NIL);
                           RETURN NIL;
                       ELSE
                           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                       END (*IF*);
                   ELSE
                       RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
                   END (*IF*);

           |  OS2.WM_CLOSE:
                   RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);

        ELSE    (* default *)
           RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);

    END DialogueProc;

(************************************************************************)

PROCEDURE Edit (owner: OS2.HWND;  name: ARRAY OF CHAR;
                                  lang: LangHandle;  TNImode: BOOLEAN);

    (* Edit the properties of the alias called "name".  *)

    VAR hwnd: OS2.HWND;
        title: ARRAY [0..NameLength-1] OF CHAR;

    BEGIN
        UseTNI := TNImode;
        Strings.Assign (name, AliasName);
        hwnd := OS2.WinLoadDlg(OS2.HWND_DESKTOP, owner,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.AliasDialog,                (* dialogue ID *)
                       NIL);               (* creation parameters *)
        title := "EditAlias";
        INIData.SetInitialWindowPosition (hwnd, INIFileName, title, UseTNI);
        StrToBufferA (lang, "Editalias.Title", name, title);
        OS2.WinSetWindowText (hwnd, title);
        SetLanguage (hwnd, lang);
        OS2.WinProcessDlg(hwnd);
        title := "EditAlias";
        INIData.StoreWindowPosition (hwnd, INIFileName, title, UseTNI);
        OS2.WinDestroyWindow (hwnd);

    END Edit;

(************************************************************************)

BEGIN
    INIFileName := "Setup.INI";
END EditAlias.

