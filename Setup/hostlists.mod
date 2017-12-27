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

IMPLEMENTATION MODULE HostLists;

        (****************************************************************)
        (*                                                              *)
        (*                      PM Setup for Weasel                     *)
        (*               Host list pages of the notebook                *)
        (*                                                              *)
        (*        Started:        8 July 1999                           *)
        (*        Last edited:    22 May 2017                           *)
        (*        Status:         OK                                    *)
        (*                                                              *)
        (****************************************************************)


FROM SYSTEM IMPORT CARD16, ADDRESS, CAST, ADR;

IMPORT OS2, OS2RTL, DID, Strings, CommonSettings, OneLine;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBuffer;

FROM WSUINI IMPORT
    (* proc *)  OpenINIFile, CloseINIFile;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  INIFetch, INIPut, INIPutBinary,
                GetStringList, NextString, CloseStringList;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString, NameIsNumeric, StringToIP;

FROM Misc IMPORT
    (* type *)  HostCategory;

FROM Sockets IMPORT
    (* const*)  AF_INET, SOCK_RAW, SOCK_DGRAM, AF_UNSPEC, SIOSTATAT,
                IFMIB_ENTRIES, NotASocket,
    (* type *)  Socket, iftype,
    (* proc *)  socket, ioctl, os2_ioctl, soclose;

FROM NetIF IMPORT
    (* const*)  IFNAMSIZ,
    (* type *)  ifconf, ifreq;

FROM ioctl IMPORT
    (* const*)  SIOCGIFCONF;

FROM LowLevel IMPORT
    (* proc *)  AddOffset;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);
    NameLength = 256;

TYPE
    LabelString = ARRAY [0..63] OF CHAR;
    Label = ARRAY HostCategory OF LabelString;
    IDarray = ARRAY HostCategory OF CARDINAL;
    CreationRecord = RECORD
                         size: CARD16;
                         cat : HostCategory;
                     END (*RECORD*);

CONST
    INILabel = Label {"Local", "Whitelisted", "MayRelay", "RelayDest", "Banned"};
    PageName = Label {"Local", "Whitelist", "Trusted", "GateFor", "Banned"};
    DialogueID = IDarray {DID.LocalHostNames, DID.WhitelistPage, DID.RelaySources, DID.RelayDest, DID.BannedHosts};
    HostList = IDarray {DID.localhostlist, DID.whitelist, DID.mayrelaylist, DID.relaydestlist, DID.bannedlist};
    HLLabel = IDarray {DID.locallistlabel, DID.whitelistlabel, DID.mayrelaylistlabel,
                                 DID.relaydestlistlabel, DID.bannedlistlabel};
    HLExplain = IDarray {DID.locallistexplain, DID.whitelistexplain, DID.mayrelaylistexplain,
                                 DID.relaydestlistexplain, DID.bannedlistexplain};
    AddButton = IDarray {DID.AddLocalHostName, DID.AddWhitelistName, DID.AddMayRelayName,
                            DID.AddRelayDestName, DID.AddBannedName};
    EditButton = IDarray {DID.EditLocalHostName, DID.EditWhitelistName, DID.EditMayRelayName,
                            DID.EditRelayDestName, DID.EditBannedName};
    DeleteButton = IDarray {DID.DeleteLocalHostName, DID.DeleteWhitelistName, DID.DeleteMayRelayName,
                            DID.DeleteRelayDestName, DID.DeleteBannedName};
    PromoteButton = IDarray {DID.PromoteLocalHostName, DID.PromoteWhitelistName, DID.PromoteMayRelayName,
                             DID.PromoteRelayDestName, DID.PromoteBannedName};

VAR
    OurLang: LangHandle;
    Handle, notebookhandle: ARRAY HostCategory OF OS2.HWND;
    PageActive, Changed: ARRAY HostCategory OF BOOLEAN;
    ChangeInProgress: ARRAY HostCategory OF BOOLEAN;
    Multidomain, UseTNI: BOOLEAN;
    PageID: ARRAY HostCategory OF CARDINAL;
    CreationData: CreationRecord;
    OurFontGroup: ARRAY HostCategory OF CommonSettings.FontGroup;

(************************************************************************)
(*                    OPERATIONS ON DIALOGUE LABELS                     *)
(************************************************************************)

PROCEDURE SetLanguage (lang: LangHandle);

    (* Relabels all hostlist pages in the new language. *)

    VAR code: ARRAY [0..127] OF CHAR;
        stringval: ARRAY [0..511] OF CHAR;
        c: HostCategory;

    BEGIN
        OurLang := lang;
        FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
            IF PageActive[c] THEN
                Strings.Assign (PageName[c], code);
                Strings.Append (".tab", code);
                StrToBuffer (lang, code, stringval);
                OS2.WinSendMsg (notebookhandle[c], OS2.BKM_SETTABTEXT,
                                CAST(ADDRESS,PageID[c]), ADR(stringval));
                Strings.Assign (PageName[c], code);
                IF (c = local) AND Multidomain THEN
                    Strings.Append (".LabelMD", code);
                ELSE
                    Strings.Append (".Label", code);
                END (*IF*);
                StrToBuffer (lang, code, stringval);
                OS2.WinSetDlgItemText (Handle[c], HLLabel[c], stringval);
                Strings.Assign (PageName[c], code);
                Strings.Append (".explain", code);
                StrToBuffer (lang, code, stringval);
                OS2.WinSetDlgItemText (Handle[c], HLExplain[c], stringval);
            END (*IF*);
        END (*FOR*);

        StrToBuffer (lang, "Local.AddAll", stringval);
        OS2.WinSetDlgItemText (Handle[local], DID.AddLocalAddresses, stringval);

        StrToBuffer (lang, "Local.StrictChecking", stringval);
        OS2.WinSetDlgItemText (Handle[local], DID.StrictChecking, stringval);

        StrToBuffer (lang, "Buttons.Add", stringval);
        FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
            IF PageActive[c] THEN
                OS2.WinSetDlgItemText (Handle[c], AddButton[c], stringval);
            END (*IF*);
        END (*FOR*);

        StrToBuffer (lang, "Buttons.Edit", stringval);
        FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
            IF PageActive[c] THEN
                OS2.WinSetDlgItemText (Handle[c], EditButton[c], stringval);
            END (*IF*);
        END (*FOR*);

        StrToBuffer (lang, "Buttons.Promote", stringval);
        FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
            IF PageActive[c] THEN
                OS2.WinSetDlgItemText (Handle[c], PromoteButton[c], stringval);
            END (*IF*);
        END (*FOR*);

        StrToBuffer (lang, "Buttons.Delete", stringval);
        FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
            IF PageActive[c] THEN
                OS2.WinSetDlgItemText (Handle[c], DeleteButton[c], stringval);
            END (*IF*);
        END (*FOR*);

    END SetLanguage;

(************************************************************************)
(*                 MOVING DATA TO AND FROM THE INI FILE                 *)
(************************************************************************)

PROCEDURE StoreList (category: HostCategory;  hwnd: OS2.HWND);

    (* Stores a HostList into the INI file.  The hwnd parameter is the  *)
    (* handle of the listbox.  We assume that the INI file is already   *)
    (* open.                                                            *)

    VAR bufptr: CharArrayPointer;
        BufferSize: CARDINAL;
        j, k, count, index: CARDINAL;
        name: ARRAY [0..NameLength-1] OF CHAR;

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

        IF BufferSize = 0 THEN
            INIPutBinary ("$SYS", INILabel[category], j, 0);
        ELSE
            INIPutBinary ("$SYS", INILabel[category], bufptr^, BufferSize);
        END (*IF*);

        (* Deallocate the buffer space. *)

        IF BufferSize > 0 THEN
            DEALLOCATE (bufptr, BufferSize);
        END (*IF*);

    END StoreList;

(************************************************************************)

PROCEDURE LoadValues (category: HostCategory;  hwnd: OS2.HWND);

    (* Fills the dialogue elements on the user page with data from the  *)
    (* INI file, or loads default values if they're not in the INI file.*)

    VAR name: ARRAY [0..NameLength-1] OF CHAR;
        state: StringReadState;  val: BOOLEAN;

    BEGIN
        OpenINIFile;

        (* Load a hostname list from the INI file. *)

        GetStringList ("$SYS", INILabel[category], state);
        REPEAT
            NextString (state, name);
            IF name[0] <> Nul THEN

                (* Convert old-format wildcards to new format. *)

                IF name[0] = '.' THEN
                    name[0] := '*';
                    Changed[category] := TRUE;
                END (*IF*);

                (* Add name to the listbox. *)

                OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_END), ADR(name));

            END (*IF*);
        UNTIL name[0] = Nul;
        CloseStringList (state);

        (* The local "strict checking" checkbox. *)

        IF category = local THEN
            IF NOT INIFetch ("$SYS", "StrictChecking", val) THEN
                val := FALSE;
            END (*IF*);
            OS2.WinSendDlgItemMsg (hwnd, DID.StrictChecking, OS2.BM_SETCHECK,
                                     OS2.MPFROMSHORT(ORD(val)), NIL);
        END (*IF*);

        CloseINIFile;

    END LoadValues;

(************************************************************************)

PROCEDURE AddLocalAddresses (hwnd: OS2.HWND);

    (* Adds all local IP addresses to the listbox. *)

    (********************************************************************)

    PROCEDURE AddressExists (addr: CARDINAL): BOOLEAN;

        (* Returns TRUE iff the listbox already contains the numeric    *)
        (* address addr.                                                *)

        VAR k, count: CARDINAL;
            name: ARRAY [0..NameLength-1] OF CHAR;

        BEGIN
            count := OS2.ULONGFROMMR(OS2.WinSendDlgItemMsg (hwnd, DID.localhostlist,
                                        OS2.LM_QUERYITEMCOUNT, NIL, NIL));
            k := 0;
            LOOP
                IF k >= count THEN
                    RETURN FALSE;
                END (*IF*);
                OS2.WinSendDlgItemMsg (hwnd, DID.localhostlist, OS2.LM_QUERYITEMTEXT,
                             OS2.MPFROM2USHORT(k, NameLength), ADR(name));
                IF NameIsNumeric(name) AND (StringToIP(name) = addr) THEN
                    RETURN TRUE;
                END (*IF*);
                INC (k);
            END (*LOOP*);
        END AddressExists;

    (********************************************************************)

    PROCEDURE AddAddress (addr: CARDINAL);

        (* Adds one address to the listbox, unless it's a duplicate. *)

        VAR text: ARRAY [0..19] OF CHAR;

        BEGIN
            IF NOT AddressExists(addr) THEN
                IPToString (addr, TRUE, text);

                (* Add text to the listbox. *)

                OS2.WinSendDlgItemMsg (hwnd, DID.localhostlist, OS2.LM_INSERTITEM,
                     OS2.MPFROMSHORT(OS2.LIT_END), ADR(text));
            END (*IF*);

        END AddAddress;

    (********************************************************************)

    TYPE ifreqPtr = POINTER TO ifreq;

    VAR s: Socket;  i, total: CARDINAL;
        ifc: ifconf;
        ifr: ifreqPtr;
        buf: ARRAY [0..IFMIB_ENTRIES*SIZE(ifreq)-1] OF CHAR;

    BEGIN
        (* Use an ioctl call to read all the ifreq records      *)
        (* into buffer "buf", and set "total" to the number of  *)
        (* such records.                                        *)

        s := socket(AF_INET, SOCK_DGRAM, AF_UNSPEC);
        IF s = NotASocket THEN
            total := 0;
        ELSE
            ifc.ifc_len := SIZE(buf);
            ifc.ifcu_buf := ADR(buf);
            IF ioctl (s, SIOCGIFCONF, ifc, SIZE(ifc)) < 0 THEN
                total := 0;
            ELSE
                total := ifc.ifc_len DIV SIZE(ifreq);
            END (*IF*);
            soclose(s);
        END (*IF*);

        (* Now work out which of the interface records are for  *)
        (* active interfaces, and put those addresses into the  *)
        (* listbox (unless they are duplicates).                *)

        IF total > 0 THEN

            ifr := ADR(buf);
            FOR i := 0 TO total-1 DO

                IF ifr^.ifru_addr.family = AF_INET THEN
                    AddAddress (ifr^.ifru_addr.in_addr.addr);
                END (*IF*);
                ifr := AddOffset (ifr, SIZE(ifreq));

            END (*FOR*);

        END (* IF total > 0 *);

    END AddLocalAddresses;

(************************************************************************)

PROCEDURE UpdateCIDRformat (j: HostCategory);

    (* Goes through this hostlist, changing all CIDR entries from old   *)
    (* to new format.                                                   *)

    TYPE CharSet = SET OF CHAR;
    CONST Digits = CharSet {'0'..'9'};

    (********************************************************************)

    PROCEDURE updated (VAR (*INOUT*) name: ARRAY OF CHAR): BOOLEAN;

        (* Returns TRUE iff name has been changed to new CIDR format. *)

        VAR k, m, countpos: CARDINAL;

        BEGIN
            k := 0;
            IF name[0] = '[' THEN k := 1 END(*IF*);

            (* Look for 4 numeric fields separated by '.'. *)

            FOR m := 0 TO 3 DO
                WHILE name[k] IN Digits DO INC(k) END(*WHILE*);
                IF m < 3 THEN
                    IF name[k] = '.' THEN
                        INC (k);
                    ELSE
                        RETURN FALSE;
                    END (*IF*);
                END (*IF*);
            END (*FOR*);

            (* If the address is followed by '/', we have a candidate. *)

            IF name[k] = '/' THEN
                INC (k);
                countpos := k;
                m := 0;
                WHILE name[k] IN Digits DO
                    m := 10*m + ORD(name[k]) - ORD('0');
                    INC (k);
                END (*WHILE*);
                IF m <= 32 THEN
                    m := 32 - m;
                ELSE
                    RETURN FALSE;
                END (*IF*);
            ELSE
                RETURN FALSE;
            END (*IF*);

            (* Final test: is the field properly terminated? *)

            IF name[0] = '[' THEN
                IF name[k] = ']' THEN
                    INC (k);
                ELSE
                    RETURN FALSE;
                END (*IF*);
            END (*IF*);
            IF name[k] <> Nul THEN
                RETURN FALSE;
            END (*IF*);

            (* If we get this far, then the alteration is desired. *)

            k := countpos;
            IF m > 9 THEN
                name[k] := CHR(ORD('0') + m DIV 10);
                m := m MOD 10;
                INC (k);
            END (*IF*);
            name[k] := CHR(ORD('0') + m);
            INC (k);
            name[k] := Nul;
            Strings.Insert ("CIDR ", 0, name);
            RETURN TRUE;

        END updated;

    (********************************************************************)

    VAR w: OS2.HWND;
        index, count: CARDINAL;
        name: ARRAY [0..NameLength-1] OF CHAR;

    BEGIN
        w := OS2.WinWindowFromID(Handle[j],HostList[j]);
        count := OS2.ULONGFROMMR(OS2.WinSendMsg (w, OS2.LM_QUERYITEMCOUNT, NIL, NIL));

        (* Check each entry to see whether it needs updating. *)

        IF count > 0 THEN
            FOR index := 0 TO count-1 DO
                OS2.WinSendMsg (w, OS2.LM_QUERYITEMTEXT,
                                OS2.MPFROM2USHORT(index, NameLength), ADR(name));
                IF updated(name) THEN
                    OS2.WinSendMsg (w, OS2.LM_SETITEMTEXT,
                                OS2.MPFROMSHORT(index), ADR(name));
                    Changed[j] := TRUE;
                END (*IF*);
            END (*FOR*);
        END (*IF*);

    END UpdateCIDRformat;

(************************************************************************)

PROCEDURE StoreData (j: HostCategory);

    (* Stores back data for this page. *)

    VAR bool: BOOLEAN;

    BEGIN
        UpdateCIDRformat(j);
        OpenINIFile;
        IF Changed[j] THEN
            StoreList(j, OS2.WinWindowFromID(Handle[j],HostList[j]));
        END (*IF*);
        Changed[j] := FALSE;

        IF j = local THEN

            (* Also save the local "strict checking" checkbox state. *)

            bool := OS2.LONGFROMMR (OS2.WinSendDlgItemMsg (Handle[j],
                                             DID.StrictChecking,
                                             OS2.BM_QUERYCHECK, NIL, NIL)) > 0;
            INIPut ("$SYS", "StrictChecking", bool);
        END (*IF*);

        CloseINIFile;
    END StoreData;

(**************************************************************************)

PROCEDURE ["SysCall"] DialogueProc(hwnd     : OS2.HWND
                     ;msg      : OS2.ULONG
                     ;mp1, mp2 : OS2.MPARAM): OS2.MRESULT;

    VAR ButtonID, NotificationCode: CARDINAL;
        index: INTEGER;
        listwindow: OS2.HWND;
        name: ARRAY [0..NameLength-1] OF CHAR;
        message: ARRAY [0..127] OF CHAR;
        category: HostCategory;
        p: POINTER TO CreationRecord;

    BEGIN

        IF msg = OS2.WM_INITDLG THEN
            p := mp2;
            category := p^.cat;
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, 0, 0, OS2.SWP_MOVE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, EditButton[category]), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, PromoteButton[category]), FALSE);
            OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DeleteButton[category]), FALSE);
            IF category = local THEN
                IF Multidomain THEN
                    OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.locallistexplain), FALSE);
                    OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.StrictChecking), TRUE);
                ELSE
                    OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.locallistexplain), TRUE);
                    OS2.WinShowWindow (OS2.WinWindowFromID(hwnd, DID.StrictChecking), FALSE);
                END (*IF*);
            END (*IF*);
            LoadValues (category, hwnd);
            PageActive[category] := TRUE;
            RETURN NIL;
        END (*IF*);

        (* Identify which page we're dealing with. *)

        category := MIN(HostCategory);
        LOOP
            IF Handle[category] = hwnd THEN
                EXIT (*LOOP*);
            ELSIF category = MAX(HostCategory) THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                INC (category);
            END (*IF*);
        END (*LOOP*);

        index := OS2.LONGFROMMR(
                   OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_QUERYSELECTION, NIL, NIL));
        IF msg = OS2.WM_COMMAND THEN

            listwindow := OS2.WinWindowFromID(hwnd,HostList[category]);
            ButtonID := OS2.SHORT1FROMMP(mp1);

            IF ButtonID = AddButton[category] THEN
                   name := "";
                   StrToBuffer (OurLang, "Local.EnterName", message);
                   OneLine.Edit (hwnd, message, name, UseTNI);
                   IF name[0] <> Nul THEN
                       IF index = OS2.LIT_NONE THEN
                           index := 0;
                       ELSE
                           INC(index);
                       END (*IF*);
                       OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_INSERTITEM,
                              OS2.MPFROMSHORT(index), ADR(name));
                       OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   END (*IF*);
                   Changed[category] := TRUE;

            ELSIF ButtonID = EditButton[category] THEN
                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                            OS2.MPFROM2USHORT(index, NameLength), ADR(name));
                   StrToBuffer (OurLang, "Local.EnterName", message);
                   OneLine.Edit (hwnd, message, name, UseTNI);
                   IF name[0] <> Nul THEN
                       OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_SETITEMTEXT,
                              OS2.MPFROMSHORT(index), ADR(name));
                       OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_SELECTITEM,
                              OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   END (*IF*);
                   Changed[category] := TRUE;

            ELSIF ButtonID = PromoteButton[category] THEN

                   OS2.WinSendMsg (listwindow, OS2.LM_QUERYITEMTEXT,
                                   OS2.MPFROM2USHORT(index, NameLength), ADR(name));
                   OS2.WinSendMsg (listwindow, OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   DEC (index);
                   OS2.WinSendMsg (listwindow, OS2.LM_INSERTITEM,
                          OS2.MPFROMSHORT(index), ADR(name));
                   OS2.WinSendMsg (listwindow, OS2.LM_SELECTITEM,
                          OS2.MPFROMSHORT(index), OS2.MPFROMSHORT(ORD(TRUE)));
                   Changed[category] := TRUE;

            ELSIF ButtonID = DeleteButton[category] THEN

                   OS2.WinSendDlgItemMsg (hwnd, HostList[category], OS2.LM_DELETEITEM,
                                          OS2.MPFROMSHORT(index), NIL);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, EditButton[category]), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, PromoteButton[category]), FALSE);
                   OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DeleteButton[category]), FALSE);
                   Changed[category] := TRUE;

            ELSIF ButtonID = DID.AddLocalAddresses THEN

                   AddLocalAddresses(hwnd);
                   Changed[category] := TRUE;

            END (*IF*);
            RETURN NIL;

        ELSIF msg = OS2.WM_PRESPARAMCHANGED THEN

            IF ChangeInProgress[category] THEN
                RETURN OS2.WinDefDlgProc(hwnd, msg, mp1, mp2);
            ELSE
                ChangeInProgress[category] := TRUE;
                CommonSettings.UpdateFontFrom (hwnd, OurFontGroup[category]);
                ChangeInProgress[category] := FALSE;
                RETURN NIL;
            END (*IF*);

        ELSIF msg = OS2.WM_CONTROL THEN

            NotificationCode := OS2.ULONGFROMMP(mp1);
            ButtonID := NotificationCode MOD 65536;
            NotificationCode := NotificationCode DIV 65536;
            IF ButtonID = HostList[category] THEN
                IF NotificationCode = OS2.LN_SELECT THEN

                    (* For some reason the more obvious code doesn't work below, so     *)
                    (* we have to use an if/then/else construct.                        *)

                    IF index > 0 THEN
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, PromoteButton[category]), TRUE);
                    ELSE
                        OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, PromoteButton[category]), FALSE);
                    END (*IF*);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, EditButton[category]), TRUE);
                    OS2.WinEnableWindow (OS2.WinWindowFromID(hwnd, DeleteButton[category]), TRUE);
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

PROCEDURE CreatePage (notebook: OS2.HWND;  category: HostCategory;
                      AfterPage: CARDINAL;  group: CommonSettings.FontGroup;
                      ModeIsMultidomain, TNImode: BOOLEAN;
                      VAR (*OUT*) pageID: CARDINAL);

    (* Creates a host list page and adds it to the notebook. *)

    VAR pagehandle: OS2.HWND;
        code: ARRAY [0..127] OF CHAR;
        Label: LabelString;

    BEGIN
        UseTNI := TNImode;
        Multidomain := ModeIsMultidomain;
        notebookhandle[category] := notebook;
        OurFontGroup[category] := group;
        WITH CreationData DO
            size := SIZE(CreationRecord);
            cat  := category;
        END (*WITH*);
        Changed[category] := FALSE;
        pagehandle := OS2.WinLoadDlg(notebook, notebook,
                       DialogueProc,    (* dialogue procedure *)
                       0,                   (* use resources in EXE *)
                       DialogueID[category], (* dialogue ID *)
                       ADR(CreationData));                 (* creation parameters *)
        Handle[category] := pagehandle;
        IF AfterPage = 0 THEN
            PageID[category] := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         NIL,
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_LAST)));
        ELSE
            PageID[category] := OS2.ULONGFROMMR (OS2.WinSendMsg (notebook, OS2.BKM_INSERTPAGE,
                         CAST (ADDRESS, AfterPage),
                         OS2.MPFROM2SHORT (OS2.BKA_MAJOR+OS2.BKA_AUTOPAGESIZE, OS2.BKA_NEXT)));
        END (*IF*);
        Strings.Assign (PageName[category], code);
        Strings.Append (".tab", code);
        StrToBuffer (OurLang, code, Label);
        OS2.WinSendMsg (notebook, OS2.BKM_SETTABTEXT,
                        CAST(ADDRESS,PageID[category]), ADR(Label));
        OS2.WinSendMsg (notebook, OS2.BKM_SETPAGEWINDOWHWND,
                        CAST(ADDRESS,PageID[category]), CAST(ADDRESS,pagehandle));
        pageID := PageID[category];
    END CreatePage;

(**************************************************************************)

PROCEDURE SetLocalFont (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on the "Local" page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    BEGIN
        OS2.WinSetPresParam (Handle[local], OS2.PP_FONTNAMESIZE, bufsize, name);
    END SetLocalFont;

(**************************************************************************)

PROCEDURE SetFonts (VAR (*IN*) name: CommonSettings.FontName);

    (* Sets the font of the text on all host pages except for the local page. *)

    CONST bufsize = CommonSettings.FontNameSize;

    VAR c: HostCategory;

    BEGIN
        FOR c := mayrelay TO MAX(HostCategory) DO
            OS2.WinSetPresParam (Handle[c], OS2.PP_FONTNAMESIZE, bufsize, name);
        END (*FOR*);
    END SetFonts;

(**************************************************************************)

PROCEDURE Close (notebook: OS2.HWND;  category: HostCategory);

    (* Shuts down this window and removes it from the notebook. *)

    BEGIN
        OurFontGroup[category] := CommonSettings.NilFontGroup;
        StoreData (category);
        PageActive[category] := FALSE;
        OS2.WinSendMsg (notebook, OS2.BKM_DELETEPAGE,
                        CAST(ADDRESS, PageID[category]),
                        OS2.MPFROMLONG (OS2.BKA_SINGLE));
        OS2.WinSendMsg (Handle[category], OS2.WM_CLOSE, NIL, NIL);
    END Close;

(**************************************************************************)

VAR c: HostCategory;

BEGIN
    UseTNI := FALSE;
    Multidomain := FALSE;
    FOR c := MIN(HostCategory) TO MAX(HostCategory) DO
        PageActive[c] := FALSE;
        OurFontGroup[c] := CommonSettings.NilFontGroup;
        Handle[c] := OS2.NULLHANDLE;
        ChangeInProgress[c] := FALSE;
    END (*FOR*);
END HostLists.

