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

IMPLEMENTATION MODULE SUPage1;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*                    Page 1 of the notebook                    *)
        (*                                                              *)
        (*        Started:        30 June 1999                          *)
        (*        Last edited:    9 September 2014                      *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM Timer IMPORT Sleep;   (* while debugging *)

FROM SYSTEM IMPORT ADDRESS, INT16, CAST, ADR;

IMPORT OS2, OS2RTL;

IMPORT DID, Strings, CommonSettings, UserPage, SUDomains;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer, StrToBufferN;

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

(**************************************************************************)

CONST
    Nul = CHR(0);
    LangStringSize = 32;

TYPE
    TwoCard = ARRAY [0..1] OF CARDINAL;
    ThreeCard = ARRAY [0..2] OF CARDINAL;
    FourCard = ARRAY [0..3] OF CARDINAL;
    DirectoryString = ARRAY [0..511] OF CHAR;

VAR
    ChangeInProgress: BOOLEAN;
    OldMultiDomainEnabled: BOOLEAN;
    OurPageID: CARDINAL;
    OurLang: LangHandle;

    (* Handle to the window that belongs to this page.  We can save     *)
    (* this as a static value because there is never more than one      *)
    (* instance of this page.                                           *)

    pagehandle, hwndParent: OS2.HWND;
    notebookhandle: OS2.HWND;

    (* Values most recently stored in the INI file. *)

    ServicesEnabled: CARDINAL;
    MaxUsers: FourCard;

    (* Original values of some ini variables. *)

    OldServerPort, OldTimeout, OldMaxUsers: FourCard;
    OldEnable: CARDINAL;
    OldLanguage: ARRAY [0..LangStringSize-1] OF CHAR;
    MailRoot: DirectoryString;

    (* Information provided to us by other modules. *)

    OutputThreadCount: CARDINAL;
    IMAPdata: FourCard;
    IMAPenabled: BOOLEAN;

(************************************************************************)
(*             ENSURING THAT EVERY USER HAS A MAIL DIRECTORY            *)
(************************************************************************)

PROCEDURE CheckUserDirectories (VAR (*IN*) MailRoot: ARRAY OF CHAR;
                                                    Multidomain: BOOLEAN);

    (* If Multidomain is FALSE, makes sure that MailRoot has a          *)
    (* subdirectory for every user in the user database.  (We don't     *)
    (* have to check the existence of the MailRoot directory itself,    *)
    (* because this has been done in an earlier step; and we don't have *)
    (* to create user directories in the multidomain case, because the  *)
    (* Domains page looks after it in that case.)  Also creates the     *)
    (* forward directory if it doesn't already exist.                   *)

    (********************************************************************)

    PROCEDURE MakeMailDirectory (username: ARRAY OF CHAR);

        (* Try to create a mailbox directory for the given user.  If    *)
        (* the directory already exists the operation will fail anyway. *)

        VAR DirName: ARRAY [0..511] OF CHAR;

        BEGIN
            Strings.Assign (MailRoot, DirName);
            Strings.Append (username, DirName);
            MakeDirectory (DirName);
        END MakeMailDirectory;

    (********************************************************************)

    BEGIN
        IF NOT Multidomain THEN
            UserPage.CheckUserDirectories (MailRoot);
        END (*IF*);

        (* All users checked.  Now make sure the "forward"  *)
        (* directory exists.                                *)

        MakeMailDirectory ("forward");

    END CheckUserDirectories;

(************************************************************************)
(*                      COMMITTING MAILROOT CHANGES                     *)
(************************************************************************)

PROCEDURE CommitMailRoot (VAR (*OUT*) MailRootDir: ARRAY OF CHAR);

    (* Stores the current MailRoot value back to the INI file, if it    *)
    (* has changed; ensures that this directory exists; and returns the *)
    (* directory name (including a final '\') to the caller.  We assume *)
    (* that the caller has already opened the INI file.                 *)

    VAR j: CARDINAL;  stringval: DirectoryString;

    BEGIN

        (* Mail root directory.  Make sure that MailRoot has no leading *)
        (* or trailing spaces.                                          *)

        OS2.WinQueryDlgItemText (pagehandle, DID.RootDirectory,
                                                       512, stringval);
        WHILE stringval[0] = ' ' DO
            Strings.Delete (stringval, 0, 1);
        END (*WHILE*);
        j := Strings.Length (stringval);
        WHILE (j > 0) AND (stringval[j-1] = ' ') DO
            DEC (j);
            stringval[j] := Nul;
        END (*WHILE*);

        (* Create the mailroot directory if it doesn't already exist.   *)
        (* Note that we temporarily have to remove the trailing '\'     *)
        (* from the directory name.                                     *)

        j := Strings.Length (stringval);
        IF (j > 0) THEN
            DEC (j);
            IF (stringval[j] = '/') OR (stringval[j] = '\') THEN
                stringval[j] := Nul;
            END (*IF*);
            MakeDirectory (stringval);
            Strings.Append ('\', stringval);
        END (*IF*);

        (* Store the final directory name. *)

        IF NOT Strings.Equal (stringval, MailRoot) THEN
            INIPutString ('$SYS', 'MailRoot', stringval);
            Strings.Assign (stringval, MailRoot);
        END (*IF*);
        Strings.Assign (stringval, MailRootDir);

    END CommitMailRoot;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels this page in the new language. *)

    VAR stringval: ARRAY [0..511] OF CHAR;

    BEGIN
        OurLang := lang;
        StrToBuffer (lang, "Page1.tab", stringval);
        OS2.WinSendMsg (notebookhandle, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,OurPageID), ADR(stringval));
        StrToBuffer (lang, "Page1.Port", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPPortLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPPortLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MSAPortLabel, stringval);
        StrToBuffer (lang, "Page1.Timeout", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPTimeoutLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPTimeoutLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MSATimeoutLabel, stringval);
        StrToBuffer (lang, "Page1.MaxUsers", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPMaxLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPMaxLabel, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MSAMaxLabel, stringval);
        StrToBuffer (lang, "Page1.Enabled", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.SMTPenable, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.POPenable, stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MSAenable, stringval);
        StrToBuffer (lang, "Page1.MessageSubmission", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MSABoxLabel, stringval);
        StrToBuffer (lang, "Page1.Language", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.LanguageLabel, stringval);
        StrToBuffer (lang, "Page1.MailRoot", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MailRootLabel, stringval);
        StrToBuffer (lang, "Rego.Multidomain", stringval);
        OS2.WinSetDlgItemText (pagehandle, DID.MultiDomainEnabled, stringval);
    END SetLanguage;

(************************************************************************)
(*                     LOADING DATA FROM THE INI FILE                   *)
(************************************************************************)

PROCEDURE Mismatch (val1, val2: FourCard):  BOOLEAN;

    (* Returns TRUE iff val1 <> val2. *)

    BEGIN
        RETURN (val1[0] <> val2[0]) OR (val1[1] <> val2[1])
                                    OR (val1[2] <> val2[2])
                                    OR (val1[3] <> val2[3]);
    END Mismatch;

(************************************************************************)

PROCEDURE LoadTwoToFour (app, key: ARRAY OF CHAR;  default: FourCard;
                          VAR (*OUT*) result: FourCard): BOOLEAN;

    (* Loads a four-cardinal value from the INI file, allowing for the  *)
    (* possibility that an older version of the software might have     *)
    (* left only two or three cardinals there.  Returns TRUE iff at     *)
    (* least two values found.                                          *)

    VAR twoval: TwoCard;  threeval: ThreeCard;  atleasttwo: BOOLEAN;
        j: [0..3];

    BEGIN
        result := default;
        atleasttwo := INIFetch (app, key, result);
        IF NOT atleasttwo THEN
            IF INIFetch (app, key, threeval) THEN
                result[0] := threeval[0];
                result[1] := threeval[1];
                result[2] := threeval[2];
                result[3] := 0;
                atleasttwo := TRUE;
            ELSIF INIFetch (app, key, twoval) THEN
                result[0] := twoval[0];
                result[1] := twoval[1];
                result[2] := 0;
                result[3] := 0;
                atleasttwo := TRUE;
            END (*IF*);
        END (*IF*);
        FOR j := 0 TO 3 DO
            IF result[j] > MAX(INT16) THEN
                result[j] := MAX(INT16);
            END (*IF*);
        END (*FOR*);
        RETURN atleasttwo;
    END LoadTwoToFour;

(************************************************************************)

PROCEDURE LoadValues (hwnd: OS2.HWND);

    (* Fills the dialogue elements on page 1 with data from the INI file,       *)
    (* or loads default values if they're not in the INI file.                  *)

    CONST
        DefaultPort = FourCard{25, 110, 0, 587};
        DefaultTimeout = FourCard{120, 120, 0, 120};
        DefaultMaxUsers = FourCard{10, 10, 0, 10};

    VAR cardval: CARDINAL;  val0, val1, val3: INT16;
        stringval: DirectoryString;

    BEGIN
        OpenINIFile;

        (* Server ports. *)

        IF LoadTwoToFour ('$SYS', 'ServerPort', DefaultPort, OldServerPort) THEN
            val0 := OldServerPort[0];
            val1 := OldServerPort[1];
            IMAPdata[0] := OldServerPort[2];
            val3 := OldServerPort[3];
        ELSE
            val0 := 25;  val1 := 110;  IMAPdata[0] := 143;  val3 := 587;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.SMTPPortField, val0, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.POPPortField, val1, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.MSAPortField, val3, FALSE);

        (* Timeout values. *)

        IF LoadTwoToFour ('$SYS', 'TimeOut', DefaultTimeout, OldTimeout) THEN
            val0 := OldTimeout[0];
            val1 := OldTimeout[1];
            IMAPdata[1] := OldTimeout[2];
            val3 := OldTimeout[3];
        ELSE
            val0 := 120;  val1 := 120;  IMAPdata[1] := 1800;  val3 := 120;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.SMTPTimeOut, val0, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.POPTimeOut, val1, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.MSATimeOut, val3, FALSE);

        (* Maximum number of users. *)

        IF LoadTwoToFour ('$SYS', 'MaxUsers', DefaultMaxUsers, MaxUsers) THEN
            OldMaxUsers := MaxUsers;
            val0 := MaxUsers[0];
            val1 := MaxUsers[1];
            IMAPdata[2] := MaxUsers[2];
            val3 := MaxUsers[3];
        ELSE
            val0 := 10;  val1 := 10;  IMAPdata[2] := 10;  val3 := 10;
        END (*IF*);
        OS2.WinSetDlgItemShort (hwnd, DID.SMTPMaxUsers, val0, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.POPMaxUsers, val1, FALSE);
        OS2.WinSetDlgItemShort (hwnd, DID.MSAMaxUsers, val3, FALSE);

        (* "Service enabled" flags. *)

        IF INIGetCard ('$SYS', 'Enable', cardval) THEN
            ServicesEnabled := cardval;
            OldEnable := cardval;
        ELSE
            cardval := 1;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.SMTPenable, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(cardval))), NIL);
        OS2.WinSendDlgItemMsg (hwnd, DID.POPenable, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(cardval DIV 2))), NIL);
        IMAPenabled := ODD(cardval DIV 4);
        OS2.WinSendDlgItemMsg (hwnd, DID.MSAenable, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(ODD(cardval DIV 8))), NIL);

        (* Language. *)

        IF INIGetString ('$SYS', 'Language', stringval) THEN
            Strings.Assign (stringval, OldLanguage);
        ELSE
            stringval := "en";
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.Language, stringval);

        (* Mail root directory. *)

        IF INIGetString ('$SYS', 'MailRoot', stringval) THEN
            MailRoot := stringval;
        ELSE
            OurDirectory (RemoteOperation(), stringval);
            Strings.Append ("\MailRoot", stringval);
        END (*IF*);
        OS2.WinSetDlgItemText (hwnd, DID.RootDirectory, stringval);

        (* Multiple domain support enabled? *)

        IF NOT INIFetch ('$SYS', 'MultiDomainEnabled', OldMultiDomainEnabled) THEN
            OldMultiDomainEnabled := FALSE;
        END (*IF*);
        OS2.WinSendDlgItemMsg (hwnd, DID.MultiDomainEnabled, OS2.BM_SETCHECK,
                                    OS2.MPFROMSHORT(ORD(OldMultiDomainEnabled)), NIL);
        IF OldMultiDomainEnabled THEN
            OS2.WinPostMsg (hwndParent, CommonSettings.WM_MULTIDOMAIN_CHANGE,
                            OS2.MPFROMLONG(1), OS2.MPFROMLONG(0));
        END (*IF*);

        CloseINIFile;

    END LoadValues;

(************************************************************************)

PROCEDURE GetIMAPParameters (VAR (*OUT*) port, timeout, maxusers: CARDINAL;
                                           VAR (*OUT*) enable: BOOLEAN);

    (* Returns the IMAP settings that this module has loaded from the   *)
    (* INI file.                                                        *)

    BEGIN
        port := IMAPdata[0];
        timeout := IMAPdata[1];
        maxusers := IMAPdata[2];
        enable := IMAPenabled;
    END GetIMAPParameters;

(************************************************************************)
(*                      STORING DATA TO THE INI FILE                    *)
(************************************************************************)

PROCEDURE QueryButton (hwnd: OS2.HWND;  B: CARDINAL): CARDINAL;

    BEGIN
        RETURN OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (hwnd, B,
                                              OS2.BM_QUERYCHECK, NIL, NIL));
    END QueryButton;

(**************************************************************************)

PROCEDURE StoreUserNumbers (hwnd: OS2.HWND);

    VAR val: FourCard;  enable, ThreadCount: CARDINAL;  temp: INT16;
        INIopen: BOOLEAN;
        buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        INIopen := FALSE;

        (* Maximum number of users. *)

        OS2.WinQueryDlgItemShort (hwnd, DID.SMTPMaxUsers, temp, FALSE);
        val[0] := temp;
        OS2.WinQueryDlgItemShort (hwnd, DID.POPMaxUsers, temp, FALSE);
        val[1] := temp;
        val[2] := IMAPdata[2];
        OS2.WinQueryDlgItemShort (hwnd, DID.MSAMaxUsers, temp, FALSE);
        val[3] := temp;
        IF Mismatch (val, MaxUsers) THEN
            IF NOT INIopen THEN
                OpenINIFile;
                INIopen := TRUE;
            END (*IF*);
            INIPut ('$SYS', 'MaxUsers', val);
            MaxUsers := val;
        END (*IF*);

        (* "Service enabled" flags. *)

        enable := QueryButton (hwnd, DID.SMTPenable)
                   + 2 * QueryButton (hwnd, DID.POPenable)
                   + 8 * QueryButton (hwnd, DID.MSAenable);
        IF IMAPenabled THEN
            INC (enable, 4);
        END (*IF*);
        IF enable <> ServicesEnabled THEN
            IF NOT INIopen THEN
                OpenINIFile;
                INIopen := TRUE;
            END (*IF*);
            INIPut ('$SYS', 'Enable', enable);
            ServicesEnabled := enable;
        END (*IF*);

        IF INIopen THEN
            CloseINIFile;
        END (*IF*);

        (* Calculate and display the thread count. *)

        ThreadCount := 10;        (* initial overhead *)
        INC (ThreadCount, OutputThreadCount);
        FOR temp := 0 TO 3 DO
            IF ODD(enable) THEN
                INC (ThreadCount, 2*val[temp]);
            END (*IF*);
            enable := enable DIV 2;
        END (*FOR*);
        StrToBufferN (OurLang, "Page1.Threads", ThreadCount, buffer);
        OS2.WinSetDlgItemText (hwnd, DID.ThreadCount, buffer);

    END StoreUserNumbers;

(**************************************************************************)

PROCEDURE OutputThreadCountIs (N: CARDINAL);

    (* The caller tells us how many output threads there are. *)

    BEGIN
        IF OutputThreadCount <> N THEN
            OutputThreadCount := N;
            StoreUserNumbers (pagehandle);
        END (*IF*);
    END OutputThreadCountIs;

(**************************************************************************)

PROCEDURE SetIMAPParameters (port, timeout, maxusers: CARDINAL;
                                            enable: BOOLEAN);

    (* Inform this module of the IMAP settings, so that they can be     *)
    (* saved on exit with this module's settings.                       *)

    BEGIN
        IMAPdata[0] := port;
        IMAPdata[1] := timeout;
        IMAPdata[2] := maxusers;
        IMAPenabled := enable;
        IF pagehandle <> OS2.NULLHANDLE THEN
            StoreUserNumbers (pagehandle);
        END (*IF*);
    END SetIMAPParameters;

(************************************************************************)

PROCEDURE StoreData (hwnd1: OS2.HWND;  Multidomain: BOOLEAN);

    (* Stores the values on page 1 back into the INI file.  *)

    VAR val: FourCard;  temp: INT16;  enable: BOOLEAN;
        langval: ARRAY [0..LangStringSize] OF CHAR;
        (*debugmes: ARRAY [0..127] OF CHAR;*)

    BEGIN
        StoreUserNumbers (hwnd1);

        OpenINIFile;

        (* Server ports. *)

        OS2.WinQueryDlgItemShort (hwnd1, DID.SMTPPortField, temp, FALSE);
        val[0] := temp;
        OS2.WinQueryDlgItemShort (hwnd1, DID.POPPortField, temp, FALSE);
        val[1] := temp;
        val[2] := IMAPdata[0];
        OS2.WinQueryDlgItemShort (hwnd1, DID.MSAPortField, temp, FALSE);
        val[3] := temp;
        IF Mismatch (val, OldServerPort) THEN
            INIPut ('$SYS', 'ServerPort', val);
        END (*IF*);

        (* Timeout values. *)

        OS2.WinQueryDlgItemShort (hwnd1, DID.SMTPTimeOut, temp, FALSE);
        val[0] := temp;
        OS2.WinQueryDlgItemShort (hwnd1, DID.POPTimeOut, temp, FALSE);
        val[1] := temp;
        val[2] := IMAPdata[1];
        OS2.WinQueryDlgItemShort (hwnd1, DID.MSATimeOut, temp, FALSE);
        val[3] := temp;
        IF Mismatch (val, OldTimeout) THEN
            INIPut ('$SYS', 'TimeOut', val);
        END (*IF*);

        (* Language. *)

        OS2.WinQueryDlgItemText (hwnd1, DID.Language, 33, langval);
        IF NOT Strings.Equal (langval, OldLanguage) THEN
            INIPutString ('$SYS', 'Language', langval);
        END (*IF*);

        (* Debugging message. *)
        (*
        Strings.Assign ("About to commit mail root", debugmes);
        OS2.WinSetDlgItemText (hwnd1, DID.RootDirectory, debugmes);
        Sleep (2000);
        *)

        (* Mail root directory. *)

        CommitMailRoot (MailRoot);

        (* Debugging message. *)
        (*
        Strings.Assign ("Have set MailRoot to ", debugmes);
        Strings.Append (MailRoot, debugmes);
        OS2.WinSetDlgItemText (hwnd1, DID.RootDirectory, debugmes);
        Sleep (2000);
        *)

        (* Multidomain mode. *)

        enable := QueryButton (hwnd1, DID.MultiDomainEnabled) <> 0;
        IF enable <> OldMultiDomainEnabled THEN
            INIPut ('$SYS', 'MultiDomainEnabled', enable);
        END (*IF*);

        CloseINIFile;
        CheckUserDirectories (MailRoot, Multidomain);

    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc (hwnd: OS2.HWND;  msg: OS2.ULONG;
                                      mp1, mp2: OS2.MPARAM): OS2.MRESULT;

    VAR id, code: OS2.USHORT;
        value: CARDINAL;  NewLang: LangHandle;
        langval: ARRAY [0..LangStringSize] OF CHAR;

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

            id := OS2.USHORT1FROMMP (mp1);
            code := OS2.USHORT2FROMMP (mp1);
            value := OS2.ULONGFROMMP(mp2);
            IF code = OS2.EN_CHANGE THEN
                IF id = DID.Language THEN
                    OS2.WinQueryDlgItemText (hwnd, DID.Language,
                                             LangStringSize+1, langval);
                    IF CommonSettings.ChangeLanguageTo(langval) THEN
                        CommonSettings.CurrentLanguage (NewLang, langval);
                        SetLanguage (NewLang);
                        OS2.WinPostMsg (hwndParent, CommonSettings.LANGCHANGED,
                               OS2.MPFROMLONG(1), OS2.MPFROMLONG(0));
                    END (*IF*);
                    RETURN NIL;
                ELSIF (id = DID.SMTPMaxUsers) OR (id = DID.POPMaxUsers)
                        OR (id = DID.MSAMaxUsers) THEN
                    StoreUserNumbers (hwnd);
                END (*IF*);
            ELSIF code = OS2.BN_CLICKED THEN
                IF (id = DID.SMTPenable)
                             OR (id = DID.POPenable) OR (id = DID.MSAenable) THEN
                    StoreUserNumbers (hwnd);
                ELSIF id = DID.MultiDomainEnabled THEN
                    OS2.WinPostMsg (hwndParent, CommonSettings.WM_MULTIDOMAIN_CHANGE,
                                    OS2.WinSendDlgItemMsg (hwnd, id,
                                          OS2.BM_QUERYCHECK, NIL, NIL),
                                    OS2.MPFROMLONG(0));
                END (*IF*);
            END (*IF*);

        END (*IF*);

        RETURN OS2.WinDefDlgProc (hwnd, msg, mp1, mp2);

    END DialogueProc;

(**************************************************************************)

PROCEDURE CreatePage (notebook: OS2.HWND;  VAR (*OUT*) PageID: CARDINAL): OS2.HWND;

    (* Creates page 1 and adds it to the notebook. *)

    VAR Label: ARRAY [0..31] OF CHAR;

    BEGIN
        notebookhandle := notebook;
        hwndParent := OS2.WinQueryWindow (notebook, OS2.QW_PARENT);
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DID.page1,                (* dialogue ID *)
                       NIL);                 (* creation parameters *)
        PageID := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL, OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        OurPageID := PageID;
        Label := "Basic";
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID), CAST(ADDRESS,pagehandle));
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
    hwndParent := OS2.NULLHANDLE;
    OutputThreadCount := 0;
    ServicesEnabled := 0;
    MaxUsers := FourCard{0,0,0,0};
    OldServerPort := FourCard{0,0,0,0};
    OldTimeout := FourCard{0,0,0,0};
    OldMaxUsers := FourCard{0,0,0,0};
    OldEnable := 0;
    OldLanguage := "";
    MailRoot := "";
    OldMultiDomainEnabled := FALSE;
END SUPage1.

