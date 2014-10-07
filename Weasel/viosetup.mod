(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

MODULE VIOSetup;

        (********************************************************)
        (*                                                      *)
        (*              Weasel set-up utility                   *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            24 April 1998                   *)
        (*  Last edited:        1 January 2013                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT LOC;

IMPORT OS2, Strings, Blacklists, EditHosts;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM EditUsers IMPORT
    (* proc *)  CheckUserDirectories;

IMPORT SURelay, SU2Options, SU1Options, SUFilters;

FROM Windows IMPORT
    (* type *)  Window, Colour, FrameType, DividerType,
    (* proc *)  OpenWindow, CloseWindow, WriteChar, WriteString, WriteLn,
                SetCursor, GetKey, GetScreenSize;

FROM INIData IMPORT
    (* type *)  HINI,
    (* proc *)  INIValid, INIGet, INIGetString, INIPut;

FROM Menus IMPORT
    (* type *)  Menu, ItemText,
    (* proc *)  CreateMenu, MenuColours;

FROM ScreenEditor IMPORT
    (* type *)  Structure,
    (* proc *)  CardinalField, StringField, MenuField, Combine,
                ScreenEdit, DeleteStructure;

FROM MultiScreen IMPORT
    (* proc *)  EnableHotKeys;

FROM Names IMPORT
    (* type *)  ServiceType, CardArray;

FROM SetupINI IMPORT
    (* proc *)  OurINIHandle;

(************************************************************************)

VAR
    (* Number of display rows on screen.  *)

    ScreenRows: CARDINAL;

    (* Prompt line at bottom of screen.  *)

    bottombar: Window;

(************************************************************************)
(*                        INI FILE EDITING                              *)
(************************************************************************)

PROCEDURE EditDefaultParameters;

    VAR w: Window;
        R: Structure;
        port, MaxUsers, TimeoutLimit: CardArray;
        TransactionLogLevel, LogPOPLevel, LogSMTPLevel, LogOutgoingLevel,
                                                   ServerEnable: CARDINAL;
        LogPOPusers, LogSMTPItems, LogOutgoing: BOOLEAN;
        MailRoot: ARRAY [0..511] OF CHAR;
        abort: BOOLEAN;  ch: CHAR;
        hini: HINI;
        M1, M1a, M1b, M1c, M2: Menu;
        TrlOption: ARRAY [0..4] OF ItemText;
        POPLogOption: ARRAY [0..2] OF ItemText;
        EnableOption: ARRAY [0..3] OF ItemText;
        j: CARDINAL;
        SYSapp: ARRAY [0..4] OF CHAR;

    (********************************************************************)

    PROCEDURE LoadINIData;

        (* Loads setup parameters from "weasel.ini". *)

        PROCEDURE GetItem (name: ARRAY OF CHAR;
                             VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

            BEGIN
                RETURN INIGet (hini, SYSapp, name, variable);
            END GetItem;

        (****************************************************************)

        PROCEDURE GetStringItem (name: ARRAY OF CHAR;
                             VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

            BEGIN
                RETURN INIGetString (hini, SYSapp, name, variable);
            END GetStringItem;

        (****************************************************************)

        BEGIN
            hini := OurINIHandle();
            IF NOT INIValid(hini) THEN
                WriteString (w, "Could not open WEASEL.INI");
                WriteLn (w);
            ELSE
                TransactionLogLevel := 3;
                IF NOT GetItem ("Enable", ServerEnable) THEN
                    ServerEnable := 3;
                END (*IF*);
                IF NOT GetItem ("ServerPort", port) THEN
                    port := CardArray {25, 110, 143, 587};
                END (*IF*);
                IF NOT GetItem ("MaxUsers", MaxUsers) THEN
                    MaxUsers := CardArray {10, 10, 20, 10};
                END (*IF*);
                IF GetItem ("TransLevel", TransactionLogLevel) THEN
                    INC (TransactionLogLevel);
                END (*IF*);
                IF GetItem ("LogPOPusers", LogPOPusers) THEN
                    LogPOPLevel := ORD(LogPOPusers) + 1;
                ELSE
                    LogPOPLevel := 1;
                END (*IF*);
                IF GetItem ("LogSMTPItems", LogSMTPItems) THEN
                    LogSMTPLevel := ORD(LogSMTPItems) + 1;
                ELSE
                    LogSMTPLevel := 1;
                END (*IF*);
                IF GetItem ("LogOutgoing", LogOutgoing) THEN
                    LogOutgoingLevel := ORD(LogOutgoing) + 1;
                ELSE
                    LogOutgoingLevel := 1;
                END (*IF*);
                IF NOT GetItem ("TimeOut", TimeoutLimit) THEN
                    TimeoutLimit := CardArray {900, 900, 1800, 900};
                END (*IF*);
                IF NOT GetStringItem ("MailRoot", MailRoot) THEN
                    MailRoot := "C:\MPTN\ETC\MAIL\";
                END (*IF*);
            END (*IF*);
        END LoadINIData;

    (********************************************************************)

    PROCEDURE StoreINIData;

        (* Writes data back to the INI file. *)

        PROCEDURE PutItem (name: ARRAY OF CHAR;  VAR (*OUT*) variable: ARRAY OF LOC);

            BEGIN
                INIPut (hini, SYSapp, name, variable);
            END PutItem;

        (****************************************************************)

        BEGIN
            IF INIValid(hini) THEN
                PutItem ("Enable", ServerEnable);
                PutItem ("ServerPort", port);
                PutItem ("MaxUsers", MaxUsers);
                IF TransactionLogLevel > 0 THEN
                    DEC (TransactionLogLevel);
                END (*IF*);
                PutItem ("TransLevel", TransactionLogLevel);
                LogSMTPItems := LogSMTPLevel > 1;
                PutItem ("LogSMTPItems", LogSMTPItems);
                LogPOPusers := LogPOPLevel > 1;
                PutItem ("LogPOPusers", LogPOPusers);
                LogOutgoing := LogOutgoingLevel > 1;
                PutItem ("LogOutgoing", LogOutgoing);
                PutItem ("TimeOut", TimeoutLimit);
                PutItem ("MailRoot", MailRoot);
            END (*IF*);
        END StoreINIData;

    (********************************************************************)

    BEGIN
        SYSapp := "$SYS";
        OpenWindow (w, white, blue, ScreenRows DIV 2 - 6, ScreenRows DIV 2 + 11,
                                                  0, 79, noframe, nodivider);
        LoadINIData;

        SetCursor (w, 1, 6);  WriteString (w, "SMTP port");
        SetCursor (w, 2, 6);  WriteString (w, "Maximum number of SMTP users");
        SetCursor (w, 3, 6);  WriteString (w, "SMTP timeout (seconds)");

        SetCursor (w, 5, 6);  WriteString (w, "POP port");
        SetCursor (w, 6, 6);  WriteString (w, "Maximum number of POP users");
        SetCursor (w, 7, 6);  WriteString (w, "POP timeout (seconds)");

        SetCursor (w, 9, 6);  WriteString (w, "Root directory for mail");
        SetCursor (w, 6, 50);  WriteString (w, "Enable servers");
        SetCursor (w, 13, 12);  WriteString (w, "SMTP logging");
        SetCursor (w, 14, 12);  WriteString (w, "POP user logging");
        SetCursor (w, 15, 12);  WriteString (w, "Log outgoing mail");
        SetCursor (w, 16, 12);  WriteString (w, "Detailed transaction log");

        R := CardinalField (port[SMTP], 1, 36, 8);
        Combine (R, CardinalField (MaxUsers[SMTP], 2, 36, 8));
        Combine (R, CardinalField (TimeoutLimit[SMTP], 3, 36, 8));

        Combine (R, CardinalField (port[POP], 5, 36, 8));
        Combine (R, CardinalField (MaxUsers[POP], 6, 36, 8));
        Combine (R, CardinalField (TimeoutLimit[POP], 7, 36, 8));

        Combine (R, StringField (MailRoot, 10, 7, 65));

        (* Create the menu of "enable server" options. *)

        EnableOption[1] := "SMTP";
        EnableOption[2] := "POP";
        EnableOption[3] := "Both";
        CreateMenu (M2, 3, EnableOption, 3);
        MenuColours (M2, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (ServerEnable, 7, 55, 1, 21, M2));

        (* Create the menus of POP and SMTP log options. *)

        POPLogOption[1] := "Disable";
        POPLogOption[2] := "Enable";
        CreateMenu (M1a, 2, POPLogOption, 2);
        MenuColours (M1a, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (LogSMTPLevel, 13, 38, 1, 20, M1a));
        CreateMenu (M1b, 2, POPLogOption, 2);
        MenuColours (M1b, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (LogPOPLevel, 14, 38, 1, 20, M1b));
        CreateMenu (M1c, 2, POPLogOption, 2);
        MenuColours (M1c, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (LogOutgoingLevel, 15, 38, 1, 20, M1c));

        (* Create the menu of transaction log options. *)

        TrlOption[1] := "None";
        TrlOption[2] := "Disk";
        TrlOption[3] := "Screen";
        TrlOption[4] := "Both";
        CreateMenu (M1, 4, TrlOption, 4);
        MenuColours (M1, white, blue, blue, cyan, yellow, darkgrey);
        Combine (R, MenuField (TransactionLogLevel, 16, 38, 1, 28, M1));

        SetCursor (w, 1, 36);
        LOOP
            ScreenEdit (w, R, abort);

            (* Make sure that MailRoot ends with a trailing '\'. *)

            j := LENGTH (MailRoot);
            IF (j = 0) OR ((MailRoot[j-1] <> '/') AND (MailRoot[j-1] <> '\')) THEN
                Strings.Append ('\', MailRoot);
            END (*IF*);

            IF abort THEN EXIT(*LOOP*) END(*IF*);

            (* Consume the character that took us off the edge. *)

            ch := GetKey (w);
            IF ch = CHR(0) THEN
                EVAL (GetKey (w));
            END (*IF*);

        END (*LOOP*);

        DeleteStructure (R);
        StoreINIData;
        CheckUserDirectories (MailRoot);
        CloseWindow (w);

    END EditDefaultParameters;

(************************************************************************)

PROCEDURE PostUpdated (semName: ARRAY OF CHAR);

    (* Posts on a public event semaphore. *)

    VAR changehev: OS2.HEV;  count: CARDINAL;

    BEGIN
        changehev := 0;
        IF OS2.DosOpenEventSem (semName, changehev) = OS2.ERROR_SEM_NOT_FOUND THEN
            OS2.DosCreateEventSem (semName, changehev, OS2.DC_SEM_SHARED, FALSE);
        END (*IF*);
        OS2.DosPostEventSem (changehev);
        OS2.DosResetEventSem (changehev, count);
        OS2.DosCloseEventSem(changehev);
    END PostUpdated;

(************************************************************************)
(*                              MAIN PROGRAM                            *)
(************************************************************************)

PROCEDURE RunTheProgram;

    CONST Esc = CHR(1BH);
        UpdateSemName = "\SEM32\WEASEL\UPDATED";

    VAR w0, w1: Window;

    BEGIN
        OpenWindow (w0, yellow, red, 0, 5, 0, 43, noframe, nodivider);
        WriteLn (w0);
        WriteString (w0, "    SETUP FOR WEASEL POP3/SMTP SERVER");
        WriteLn (w0);  WriteLn (w0);
        WriteString (w0, "  Type F4 or F5 for other setup details");
        WriteLn (w0);
        WriteString (w0, "          Esc to exit program");

        OpenWindow (w1, black, green, 0, 5, 44, 79, noframe, nodivider);
        WriteLn (w1);
        WriteString (w1, "  NOTE: Most of the parameters set");  WriteLn (w1);
        WriteString (w1, "  on this page will not take effect");  WriteLn (w1);
        WriteString (w1, "  until the next time the server is");  WriteLn (w1);
        WriteString (w1, "        run on this machine");

        EditDefaultParameters;
        CloseWindow (w1);
        CloseWindow (w0);
        PostUpdated (UpdateSemName);

    END RunTheProgram;

(************************************************************************)

VAR dummy: CARDINAL;

BEGIN
    GetScreenSize (ScreenRows, dummy);
    EnableHotKeys (TRUE, CHR(62), TRUE, CHR(63), TRUE, CHR(64));
    OpenWindow (bottombar, yellow, red, ScreenRows-1, ScreenRows-1, 0, 79, noframe, nodivider);
    WriteString (bottombar, " Esc exit");
    SetCursor (bottombar, 0, 55);
    WriteString (bottombar, "F4,F5 previous/next page");
    RunTheProgram;
FINALLY
    CloseWindow (bottombar);
END VIOSetup.

