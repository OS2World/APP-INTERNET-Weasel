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

IMPLEMENTATION MODULE OptionP3;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*               Options, part 3, of the notebook               *)
        (*                                                              *)
        (*        Started:        16 December 2013                      *)
        (*        Last edited:    22 May 2017                           *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT LOC, CARD8, INT16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, CommonSettings;

FROM SUPage1 IMPORT
    (* proc *)  OutputThreadCountIs;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* proc *)  INIPut, INIFetch, INIGetCard, ServerIPAddress;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4;

FROM Misc IMPORT
    (* proc *)  WinQueryDlgItemCard, WinSetDlgItemCard;

(**************************************************************************)

CONST Nul = CHR(0);

VAR
    OurPageHandle, notebookhandle: OS2.HWND;
    ChangeInProgress: BOOLEAN;
    OurPageID: CARDINAL;
    OldBindAddr: CARDINAL;
    BounceBytes: CARDINAL;
    OldBounceBytes: CARDINAL;
    MaxMessageSize, OldMaxMessageSize: CARDINAL;

(************************************************************************)
(*          CONVERTING AN ADDRESS RECORD TO HUMAN-READABLE FORM         *)
(************************************************************************)

PROCEDURE CardToDotted (value: ARRAY OF LOC;  VAR (*OUT*) text: ARRAY OF CHAR;
                                              VAR (*INOUT*) place: CARDINAL);

    (* Converts the four-byte value to a string of the form a.b.c.d,    *)
    (* starting at text[place].  On return, place is the first location *)
    (* in the text array that hasn't been used.                         *)

    VAR j: [0..2];

    BEGIN
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,value[j]), text, place);
            text[place] := '.';  INC(place);
        END (*FOR*);
        ConvertCard (CAST(CARD8,value[3]), text, place);
        IF place <= HIGH(text) THEN
            text[place] := Nul;
        END (*IF*);
    END CardToDotted;

(************************************************************************)

PROCEDURE CardinalToDotted (value: CARDINAL;  VAR (*OUT*) text: ARRAY OF CHAR);

    (* Converts the value to a string of the form a.b.c.d    *)

    VAR place: CARDINAL;

    BEGIN
        place := 0;
        CardToDotted (value, text, place);
    END CardinalToDotted;

(************************************************************************)
(*             CONVERTING A STRING TO AN ADDRESS RECORD                 *)
(************************************************************************)

PROCEDURE SkipBlanks (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL);

    (* Increases j until text[j] is not a space character. *)

    BEGIN
        WHILE text[j] = ' ' DO INC(j) END(*WHILE*);
    END SkipBlanks;

(************************************************************************)

PROCEDURE DottedToCard (text: ARRAY OF CHAR;
                                 VAR (*INOUT*) pos: CARDINAL): CARDINAL;

    (* Converts a dotted-quad text string to a cardinal value. *)

    VAR result: CARDINAL;  j: [0..3];

    (********************************************************************)

    PROCEDURE ConvertOneComponent(): CARDINAL;

        TYPE CharSet = SET OF CHAR;
        CONST Digits = CharSet {'0'..'9'};

        VAR answer: CARDINAL;

        BEGIN
            answer := 0;
            WHILE text[pos] IN Digits DO
                answer := 10*answer + (ORD(text[pos]) - ORD('0'));
                INC (pos);
            END (*WHILE*);
            RETURN answer;
        END ConvertOneComponent;

    (********************************************************************)

    BEGIN
        result := 0;
        FOR j := 0 TO 3 DO
            result := 256*result + ConvertOneComponent();
            IF text[pos] = '.' THEN INC(pos) END(*IF*);
        END (*FOR*);
        RETURN Swap4(result);
    END DottedToCard;

(************************************************************************)

PROCEDURE StringToCard (text: ARRAY OF CHAR;  VAR (*INOUT*) j: CARDINAL): CARDINAL;

    (* Picks up an IP address, starting at text[j], updates j. *)

    VAR result: CARDINAL;

    BEGIN
        SkipBlanks (text, j);
        IF text[j] = '[' THEN INC(j) END(*IF*);
        result := DottedToCard (text, j);
        IF text[j] = ']' THEN INC(j) END(*IF*);
        RETURN result;
    END StringToCard;

(************************************************************************)

PROCEDURE StringToIPAddress (text: ARRAY OF CHAR): CARDINAL;

    (* Converts an IP address. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        RETURN StringToCard (text, j);
    END StringToIPAddress;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        StrToBuffer (lang, "OptionP3.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "OptionP3.BindGroupLabel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BindGroupLabel, stringval);
        StrToBuffer (lang, "OptionP3.BindAll", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BindAllButton, stringval);
        StrToBuffer (lang, "OptionP3.BindSpecificAddress", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BindSpecificButton, stringval);
        StrToBuffer (lang, "OptionP3.AttachOriginal", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.AttachOriginal, stringval);
        StrToBuffer (lang, "OptionP3.NonDeliveryGroup", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.NonDeliveryGroup, stringval);
        StrToBuffer (lang, "OptionP3.BounceBytesLabel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.BounceBytesLabel, stringval);
        StrToBuffer (lang, "OptionP3.MaxMessageSizeLabel", stringval);
        OS2.WinSetDlgItemText (OurPageHandle, DID.MaxMessageSizeLabel, stringval);
    END SetLanguage;

(**************************************************************************)
(*                 SHOW/HIDE SOME DIALOGUE ELEMENTS                       *)
(**************************************************************************)

PROCEDURE ShowHide (hwnd: OS2.HWND);

    (* Alters some dialogue elements, depending on whether BounceBytes  *)
    (* is zero or nonzero.                                              *)

    (* If it's nonzero we try to give the BounceBytes field the focus,  *)
    (* but for some reason this is failing.                             *)

    VAR hwnd1, hwnd2: OS2.HWND;

    BEGIN
        hwnd1 := OS2.WinWindowFromID (hwnd, DID.BounceBytes);
        hwnd2 := OS2.WinWindowFromID (hwnd, DID.BounceBytesLabel);
        IF BounceBytes = 0 THEN
            OS2.WinSendDlgItemMsg (hwnd, DID.AttachOriginal, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(1), NIL);
            OS2.WinShowWindow (hwnd1, FALSE);
            OS2.WinShowWindow (hwnd2, FALSE);
        ELSE
            OS2.WinSendDlgItemMsg (hwnd, DID.AttachOriginal, OS2.BM_SETCHECK,
                                         OS2.MPFROMSHORT(0), NIL);
            OS2.WinShowWindow (hwnd1, TRUE);
            OS2.WinShowWindow (hwnd2, TRUE);
            OS2.WinSetFocus (OS2.HWND_DESKTOP, hwnd1);
        END (*IF*);
    END ShowHide;

(**************************************************************************)
(*                   LOADING AND STORING INI DATA                         *)
(**************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file, *)
    (* or loads default values if they're not in the INI file.            *)

    VAR value, OurHostID: CARDINAL;
        AddressString: ARRAY [0..15] OF CHAR;

    BEGIN
        OpenINIFile;
        OurHostID := ServerIPAddress();

        (* Get Bind Address.  Zero means "bind to all interfaces". *)

        IF NOT INIGetCard ('$SYS', 'BindAddr', value) THEN
            value := 0;
        END (*IF*);

        IF value = 0 THEN

            OS2.WinSendDlgItemMsg (hwnd, DID.BindAllButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(1), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.BindSpecificButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(0), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), FALSE);
            value := OurHostID;

        ELSE

            OS2.WinSendDlgItemMsg (hwnd, DID.BindAllButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(0), NIL);
            OS2.WinSendDlgItemMsg (hwnd, DID.BindSpecificButton, OS2.BM_SETCHECK,
                             OS2.MPFROMSHORT(1), NIL);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), TRUE);

        END (*IF*);

        CardinalToDotted (value, AddressString);
        OS2.WinSetDlgItemText (hwnd, DID.IPAddress, AddressString);

        (* Number of bytes of original message to return. *)

        IF NOT INIFetch ('$SYS', 'BounceBytes', BounceBytes) THEN
            BounceBytes := 0;
        END (*IF*);
        OldBounceBytes := BounceBytes;
        OS2.WinSetDlgItemShort (hwnd, DID.BounceBytes, BounceBytes, FALSE);

        (* Maximum incoming message size. *)

        IF NOT INIFetch ('$SYS', 'MaxMessageSize', MaxMessageSize) OR (MaxMessageSize = 0) THEN
            MaxMessageSize := MAX(CARDINAL);
        END (*IF*);
        OldMaxMessageSize := MaxMessageSize;
        WinSetDlgItemCard (hwnd, DID.MaxMessageSize, MaxMessageSize);

        CloseINIFile;
        ShowHide(hwnd);

    END LoadValues;

(**************************************************************************)

PROCEDURE StoreData;

    (* Stores the values on this page back into the INI file. *)

    VAR temp: INT16;  value: CARDINAL;
        TextBuffer: ARRAY [0..15] OF CHAR;

    BEGIN
        OpenINIFile;

        (* IP address to bind to. *)

        IF OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (OurPageHandle, DID.BindAllButton,
                                    OS2.BM_QUERYCHECK, NIL, NIL)) <> 0 THEN
            value := 0;
        ELSE
            OS2.WinQueryDlgItemText (OurPageHandle, DID.IPAddress, 16, TextBuffer);
            value := StringToIPAddress (TextBuffer);
        END (*IF*);

        IF value <> OldBindAddr THEN
            INIPut ('$SYS', 'BindAddr', value);
        END (*IF*);

        (* Number of bytes of original message to return. *)

        OS2.WinQueryDlgItemShort (OurPageHandle, DID.BounceBytes, temp, FALSE);
        BounceBytes := temp;
        INIPut ('$SYS', 'BounceBytes', BounceBytes);

        (* Maximum incoming message size. *)

        WinQueryDlgItemCard (OurPageHandle, DID.MaxMessageSize, MaxMessageSize);
        INIPut ('$SYS', 'MaxMessageSize', MaxMessageSize);

        CloseINIFile;

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR NotificationCode, ItemID: CARDINAL;
        temp: INT16;  AttachOriginal: BOOLEAN;

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
            IF ItemID = DID.BindSpecificButton THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), TRUE);
                (*RETURN NIL;*)
            ELSIF ItemID = DID.BindAllButton THEN
                OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DID.IPAddress), FALSE);
                (*RETURN NIL;*)
            ELSIF (ItemID = DID.AttachOriginal)
                             AND (NotificationCode = OS2.BN_CLICKED) THEN
                AttachOriginal := OS2.LONGFROMMR
                     (OS2.WinSendDlgItemMsg (OurPageHandle, DID.AttachOriginal,
                                 OS2.BM_QUERYCHECK, NIL, NIL)) <> 0;
                IF AttachOriginal THEN
                    OS2.WinQueryDlgItemShort (hwnd, DID.BounceBytes, temp, FALSE);
                    OldBounceBytes := temp;
                    BounceBytes := 0;
                ELSE
                    BounceBytes := OldBounceBytes;
                    IF BounceBytes = 0 THEN
                        BounceBytes := 1024;
                    END (*IF*);
                END (*IF*);
                OS2.WinSetDlgItemShort (OurPageHandle, DID.BounceBytes,
                                                 BounceBytes, FALSE);
                ShowHide(hwnd);
            END (*IF*);
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        ELSE
            RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
        END (*CASE*);
    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL);

    (* Creates option page 3 and adds it to the notebook. *)

    BEGIN
        notebookhandle := notebook;
        OurPageHandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,        (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.optionp3,        (* dialogue ID *)
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
    OldBindAddr := MAX(CARDINAL);
END OptionP3.

