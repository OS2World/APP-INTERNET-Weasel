(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE INIData;

        (************************************************************)
        (*                                                          *)
        (*               Looking after our INI file data            *)
        (*                                                          *)
        (*    Started:        30 March 2000                         *)
        (*    Last edited:    17 July 2017                          *)
        (*    Status:         OK                                    *)
        (*                                                          *)
        (************************************************************)


IMPORT OS2, Strings, TNIData;

FROM SYSTEM IMPORT
    (* type *)  LOC,
    (* proc *)  ADR, CAST;

FROM FileOps IMPORT
    (* proc *)  Exists, CreateFile;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(**************************************************************************)

CONST
    Nul = CHR(0);  CR = CHR(13);  LF = CHR(10);

TYPE
    HINI = POINTER TO HINIRECORD;
    HINIRECORD = RECORD
                     CASE textini: BOOLEAN OF
                        |  FALSE: ihandle: OS2.HINI;
                        |  TRUE:  thandle: TNIData.THandle;
                     END (*CASE*);
                 END (*RECORD*);

    FilenameString = ARRAY [0..511] OF CHAR;

    WindowPosition = RECORD
                         x, y: INTEGER;
                     END (*RECORD*);

    CharArrayPointer = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;

    StringReadState = POINTER TO StateRecord;
    StateRecord = RECORD
                      pos, BufferSize: CARDINAL;
                      bufptr: CharArrayPointer;
                  END (*RECORD*);

VAR
    (* Anchor block handle for this application.  *)

    hab: OS2.HAB;

    (* ProgramDir is the directory from which this program is running.  *)

    ProgramDir: FilenameString;

(************************************************************************)
(*          DECIDING WHETHER TO SET INI OR TNI AS DEFAULT CHOICE        *)
(************************************************************************)

PROCEDURE ChooseDefaultINI (appname: ARRAY OF CHAR;
                                   VAR (*OUT*) useTNI: BOOLEAN): BOOLEAN;

    (* Returns useTNI=TRUE if we should default to using appname.TNI to *)
    (* hold this application's data, useTNI=FALSE if the default should *)
    (* be to use appname.INI.  The decision is based on factors like    *)
    (* which file exists.  Of course the caller might in some cases     *)
    (* override this decision; all we are supplying is an initial       *)
    (* default.  The function result is FALSE if we are unable to make  *)
    (* a decision, i.e. either choice is equally good, and in that case *)
    (* the returned useTNI value should be ignored.                     *)

    VAR hini: HINI;  useINI, useTNI0, foundI, foundT: BOOLEAN;
        app: ARRAY [0..5] OF CHAR;
        Iname, Tname: FilenameString;

    BEGIN
        Strings.Assign (appname, Iname);
        Strings.Assign (appname, Tname);
        Strings.Append (".INI", Iname);
        Strings.Append (".TNI", Tname);
        useINI := Exists(Iname);
        useTNI := Exists(Tname);

        (* If only one of the files exists, the decision is obvious.    *)
        (* If neither exists, default to using INI.                     *)

        IF NOT (useINI AND useTNI) THEN
            RETURN TRUE;
        END (*IF*);

        (* That leaves the case where both files exists.  In that case  *)
        (* we look up the entry ($SYS, useTNI) in each file.            *)

        useTNI := FALSE;  useTNI0 := FALSE;
        app := "$SYS";
        hini := OpenINIFile (Iname, FALSE);
        foundI :=INIGet (hini, app, "UseTNI", useTNI0);
        CloseINIFile (hini);

        hini := OpenINIFile (Tname, TRUE);
        foundT :=INIGet (hini, app, "UseTNI", useTNI);
        CloseINIFile (hini);

        (* If both entries missing, default to using INI.  *)

        IF NOT (foundI OR foundT) THEN
            RETURN TRUE;
        END (*IF*);

        (* If only one entry exists, use it. *)

        IF foundI <> foundT THEN
            IF foundI THEN
                useTNI := useTNI0;
            END (*IF*);
            RETURN TRUE;
        END (*IF*);

        (* That leaves the case where both entries exist. *)

        RETURN useTNI0 = useTNI;

    END ChooseDefaultINI;

(************************************************************************)
(*                   READING/WRITING A LOCAL INI FILE                   *)
(************************************************************************)

PROCEDURE OpenINIFile (VAR (*IN*) filename: ARRAY OF CHAR; UseTextFile: BOOLEAN): HINI;

    (* Opens an INI or TNI file, returns its handle. *)

    VAR result: HINI;  length: CARDINAL;  failure: BOOLEAN;

    BEGIN
        NEW (result);
        result^.textini := UseTextFile;

        IF UseTextFile THEN
            length := LENGTH(filename);
            IF length > 3 THEN
                filename[length-3] := 'T';
            END (*IF*);
            result^.thandle := TNIData.OpenTNIFile (filename);
            failure := result^.thandle = CAST(TNIData.THandle, NIL);
        ELSE
            failure := NOT Exists (filename);
            IF NOT failure THEN
                result^.ihandle := OS2.PrfOpenProfile (hab, filename);
                failure := result^.ihandle = 0;
            END (*IF*);
            IF failure THEN
                Strings.Capitalize (filename);
                failure := NOT Strings.Equal(filename, "OS2.INI");
            END (*IF*);
        END (*IF*);
        IF failure THEN
            DEALLOCATE (result, SIZE(HINIRECORD));
            result := NIL;
        END (*IF*);
        RETURN result;
    END OpenINIFile;

(************************************************************************)

PROCEDURE CreateINIFile (VAR (*IN*) filename: ARRAY OF CHAR; UseTextFile: BOOLEAN): HINI;

    (* Like OpenINIFile, but creates an initially empty new file. *)

    VAR result: HINI;  length: CARDINAL;  failure: BOOLEAN;

    BEGIN
        NEW (result);
        result^.textini := UseTextFile;

        IF UseTextFile THEN
            length := LENGTH(filename);
            IF length > 3 THEN
                filename[length-3] := 'T';
            END (*IF*);
            result^.thandle := TNIData.CreateTNIFile (filename);
            failure := result^.thandle = CAST(TNIData.THandle, NIL);
        ELSE
            CreateFile (filename);
            result^.ihandle := OS2.PrfOpenProfile (hab, filename);
            failure := result^.ihandle = 0;
            IF failure THEN
                Strings.Capitalize (filename);
                failure := NOT Strings.Equal(filename, "OS2.INI");
            END (*IF*);
        END (*IF*);
        IF failure THEN
            DEALLOCATE (result, SIZE(HINIRECORD));
            result := NIL;
        END (*IF*);
        RETURN result;
    END CreateINIFile;

(************************************************************************)

PROCEDURE INIValid (hini: HINI): BOOLEAN;

    (* Returns TRUE iff hini <> NIL. *)

    BEGIN
        RETURN hini <> NIL;
    END INIValid;

(************************************************************************)

PROCEDURE CloseINIFile (VAR (*INOUT*) hini: HINI);

    (* Closes our INI file. *)

    BEGIN
        IF hini <> NIL THEN
            IF hini^.textini THEN
                TNIData.CloseTNIFile (hini^.thandle);
            ELSE
                OS2.PrfCloseProfile (hini^.ihandle);
            END (*IF*);
            DEALLOCATE (hini, SIZE(HINIRECORD));
        END (*IF*);
    END CloseINIFile;

(************************************************************************)

PROCEDURE ItemSize (hini: HINI;  VAR (*IN*) application: ARRAY OF CHAR;
                          key: ARRAY OF CHAR;
                                VAR (*OUT*) size: CARDINAL): BOOLEAN;

    (* Sets size to the size in bytes of the given INI file entry,      *)
    (* or returns FALSE if there is no such entry.                      *)

    BEGIN
        size := 0;
        IF hini = NIL THEN
            RETURN FALSE;
        ELSIF hini^.textini THEN
            RETURN TNIData.ItemSize (hini^.thandle, application, key, size);
        ELSE
            IF application[0] = Nul THEN
                RETURN OS2.PrfQueryProfileSize (hini^.ihandle, NIL, NIL, size);
            ELSIF key[0] = Nul THEN
                RETURN OS2.PrfQueryProfileSize (hini^.ihandle, application, NIL, size);
            ELSE
                RETURN OS2.PrfQueryProfileSize (hini^.ihandle, application, key, size);
            END (*IF*);
        END (*IF*);
    END ItemSize;

(********************************************************************************)

PROCEDURE INIGetTrusted (hini: HINI;  VAR (*IN*) application: ARRAY OF CHAR;
                                       key: ARRAY OF CHAR;
                                       VAR (*OUT*) result: ARRAY OF LOC;
                                                         size: CARDINAL): BOOLEAN;

    (* Retrieves the value of a variable from the INI file.  Returns FALSE if   *)
    (* the variable was not found.  This the version in which we trust the      *)
    (* caller to have ensured that the size is correct.                         *)

    VAR success: BOOLEAN;

    BEGIN
        IF hini = NIL THEN
            RETURN FALSE;
        ELSIF hini^.textini THEN
            success := TNIData.INIGetTrusted (hini^.thandle, application, key, result, size);
        ELSE
            IF application[0] = Nul THEN
                success := OS2.PrfQueryProfileData (hini^.ihandle, NIL, NIL, ADR(result), size);
            ELSIF key[0] = Nul THEN
                success := OS2.PrfQueryProfileData (hini^.ihandle, application, NIL, ADR(result), size);
            ELSE
                success := OS2.PrfQueryProfileData (hini^.ihandle, application, key, ADR(result), size);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END INIGetTrusted;

(********************************************************************************)

PROCEDURE INIGet (hini: HINI;  VAR (*IN*) application: ARRAY OF CHAR;
                          key: ARRAY OF CHAR;
                                       VAR (*OUT*) result: ARRAY OF LOC): BOOLEAN;

    (* Retrieves the value of a variable from the INI file.  Returns FALSE if   *)
    (* the variable was not found or if there is a data size mismatch.          *)

    VAR size: CARDINAL;  success: BOOLEAN;

    BEGIN
        success := ItemSize (hini, application, key, size)
                                    AND (size = HIGH(result) + 1);
        IF success THEN
            IF hini^.textini THEN
                success := TNIData.INIGetTrusted (hini^.thandle,
                                                  application, key, result, size);
            ELSE
                IF application[0] = Nul THEN
                    success := OS2.PrfQueryProfileData (hini^.ihandle, NIL, NIL, ADR(result), size);
                ELSIF key[0] = Nul THEN
                    success := OS2.PrfQueryProfileData (hini^.ihandle, application, NIL, ADR(result), size);
                ELSE
                    success := OS2.PrfQueryProfileData (hini^.ihandle, application, key, ADR(result), size);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END INIGet;

(********************************************************************************)

PROCEDURE INIGetString (hini: HINI;  VAR (*IN*) name1: ARRAY OF CHAR;
                              name2: ARRAY OF CHAR;
                                    VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

    (* Like INIGet, but we accept any size data that will fit in the variable,  *)
    (* and we add a Nul terminator in the case of a size mismatch.              *)

    VAR size: CARDINAL;

    BEGIN
        IF hini = NIL THEN
            RETURN FALSE;
        ELSIF hini^.textini THEN
            RETURN TNIData.INIGetString (hini^.thandle,
                                              name1, name2, variable);
        ELSE
            IF OS2.PrfQueryProfileSize (hini^.ihandle, name1, name2, size)
                                       AND (size <= HIGH(variable)+1) THEN
                OS2.PrfQueryProfileData (hini^.ihandle, name1, name2,
                                                            ADR(variable), size);
                IF size <= HIGH(variable) THEN
                    variable[size] := Nul;
                END (*IF*);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        END (*IF*);
    END INIGetString;

(************************************************************************)

PROCEDURE INIPutBinary (hini: HINI;  VAR (*IN*) name1: ARRAY OF CHAR;
                              name2: ARRAY OF CHAR;
                                     VAR (*IN*) variable: ARRAY OF LOC;
                                     amount: CARDINAL);

    (* Writes variable-sized data to the INI file. *)

    BEGIN
        IF hini^.textini THEN
            TNIData.INIPutBinary (hini^.thandle, name1, name2, variable, amount);
        ELSE
            IF name1[0] = Nul THEN
                OS2.PrfWriteProfileData (hini^.ihandle, '', NIL, NIL, 0);
            ELSIF name2[0] = Nul THEN
                OS2.PrfWriteProfileData (hini^.ihandle, name1, NIL, NIL, 0);
            ELSE
                OS2.PrfWriteProfileData (hini^.ihandle, name1, name2,
                                                   ADR(variable), amount);
            END (*IF*);
        END (*IF*);
    END INIPutBinary;

(************************************************************************)

PROCEDURE INIPut (hini: HINI;  VAR (*IN*) name1: ARRAY OF CHAR;  name2: ARRAY OF CHAR;
                                                  variable: ARRAY OF LOC);

    (* Writes fixed-size data to the INI file. *)

    BEGIN
        INIPutBinary (hini, name1, name2, variable, HIGH(variable)+1);
    END INIPut;

(************************************************************************)

PROCEDURE INIPutString (hini: HINI;  VAR (*IN*) name1: ARRAY OF CHAR;
                              name2: ARRAY OF CHAR;
                                        VAR (*IN*) string: ARRAY OF CHAR);

    (* Writes a character string to the INI file. *)

    BEGIN
        INIPutBinary (hini, name1, name2, string, LENGTH(string));
    END INIPutString;

(************************************************************************)
(*              READING A STRING OF STRINGS FROM AN INI FILE            *)
(************************************************************************)

PROCEDURE GetStringList (hini: HINI;  VAR (*IN*) app, key: ARRAY OF CHAR;
                                   VAR (*OUT*) state: StringReadState);

    (* Initialisation in preparation for a "NextString" operation. *)

    BEGIN
        NEW (state);

        (* Store the entire string of strings in a buffer. *)

        state^.bufptr := NIL;
        IF NOT ItemSize (hini, app, key, state^.BufferSize) THEN
            state^.BufferSize := 0;
        END (*IF*);
        IF state^.BufferSize > 0 THEN
            ALLOCATE (state^.bufptr, state^.BufferSize);
            IF NOT INIGetTrusted (hini, app, key, state^.bufptr^,
                                                 state^.BufferSize) THEN
                state^.bufptr^[0] := Nul;
            END (*IF*);
        END (*IF*);
        state^.pos := 0;

    END GetStringList;

(************************************************************************)

PROCEDURE NextString (state: StringReadState;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Reads the next character string from a string-of-strings field.  *)
    (* An empty string is returned when we have run out of strings.     *)

    VAR k: CARDINAL;

    BEGIN
        WITH state^ DO
            IF pos >= BufferSize THEN
                result[0] := Nul;
            ELSE
                k := 0;
                REPEAT
                    result[k] := bufptr^[pos];
                    INC (k);  INC (pos);
                UNTIL (pos >= BufferSize) OR (bufptr^[pos-1] = Nul)
                                        OR (k > HIGH(result));
                IF k <= HIGH(result) THEN
                    result[k] := Nul;
                END (*IF*);
            END (*IF*);
        END (*WITH*);
    END NextString;

(**************************************************************************)

PROCEDURE CloseStringList (VAR (*INOUT*) state: StringReadState);

    (* Must be called to release the memory used in fetching a  *)
    (* string of strings.                                       *)

    BEGIN
        IF state <> NIL THEN
            IF state^.BufferSize > 0 THEN
                DEALLOCATE (state^.bufptr, state^.BufferSize);
            END (*IF*);
            DEALLOCATE (state, SIZE(StateRecord));
        END (*IF*);
    END CloseStringList;

(************************************************************************)
(*                      OTHER INI FILE OPERATIONS                       *)
(************************************************************************)

PROCEDURE INIDeleteApp (hini: HINI;  VAR (*IN*) app: ARRAY OF CHAR);

    (* Deletes an application from the INI file. *)

    BEGIN
        IF hini^.textini THEN
            TNIData.INIDeleteApp (hini^.thandle, app);
        ELSE
            OS2.PrfWriteProfileData (hini^.ihandle, app, NIL, NIL, 0);
        END (*IF*);
    END INIDeleteApp;

(************************************************************************)

PROCEDURE INIDeleteKey (hini: HINI;  VAR (*IN*) app: ARRAY OF CHAR;  key: ARRAY OF CHAR);

    (* Deletes a key, and its associated data, from the INI file. *)

    BEGIN
        IF hini^.textini THEN
            TNIData.INIDeleteKey (hini^.thandle, app, key);
        ELSE
            OS2.PrfWriteProfileData (hini^.ihandle, app, key, NIL, 0);
        END (*IF*);
    END INIDeleteKey;

(************************************************************************)

PROCEDURE INICopyKey (hini: HINI;  oldapp, oldkey,
                                       newapp, newkey: ARRAY OF CHAR);

    (* Creates a second copy of the data for (oldapp, oldkey).  *)

    VAR size: CARDINAL;  p: CharArrayPointer;

    BEGIN
        IF NOT ItemSize (hini, oldapp, oldkey, size) THEN
            size := 0;
        END (*IF*);
        IF size = 0 THEN
            INIPutBinary (hini, newapp, newkey, p, 0);
        ELSE
            ALLOCATE (p, size);
            IF INIGetTrusted (hini, oldapp, oldkey, p^, size) THEN
                INIPutBinary (hini, newapp, newkey, p^, size);
            END (*IF*);
            DEALLOCATE (p, size);
        END (*IF*);
    END INICopyKey;

(************************************************************************)

PROCEDURE INICopyApp (hini: HINI;  oldapp, newapp: ARRAY OF CHAR);

    (* Creates a second copy of all data for this application.  *)

    VAR state: StringReadState;
        key: ARRAY [0..127] OF CHAR;

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            key := "";
            GetStringList (hini, oldapp, key, state);
            NextString (state, key);
            WHILE key[0] <> Nul DO
                INICopyKey (hini, oldapp, key, newapp, key);
                INIDeleteKey (hini, oldapp, key);
                NextString (state, key);
            END (*WHILE*);
            CloseStringList (state);
        END (*IF*);
    END INICopyApp;

(************************************************************************)

PROCEDURE INIRenameApp (hini: HINI;  VAR (*IN*) oldapp, newapp: ARRAY OF CHAR);

    (* Changes the name of an application, retaining the data. *)

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            INICopyApp (hini, oldapp, newapp);
            INIDeleteApp (hini, oldapp);
        END (*IF*);
    END INIRenameApp;

(************************************************************************)
(*                      WHAT IS OUR DIRECTORY?                          *)
(************************************************************************)

PROCEDURE OurDirectory (VAR (*OUT*) dirname: ARRAY OF CHAR);

    (* Tells the caller the name of the program directory. *)

    BEGIN
        Strings.Assign (ProgramDir, dirname);
    END OurDirectory;

(************************************************************************)
(*                                                                      *)
(*                        FOR PM APPLICATIONS                           *)
(*                                                                      *)
(************************************************************************)

PROCEDURE SetInitialWindowPosition (hwnd: OS2.HWND;
                                    INIFileName, label: ARRAY OF CHAR;
                                    UseTNI: BOOLEAN);

    (* If this window has a previously stored position in our INI file, *)
    (* positions the window to that position.                           *)

    CONST bufsize = 256;

    VAR hini: HINI;  pos: WindowPosition;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        app: ARRAY [0..9] OF CHAR;

    BEGIN
        hini := OpenINIFile(INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "WindowPos";
        IF INIGet (hini, app, label, pos) THEN
            OS2.WinSetWindowPos (hwnd, 0, pos.x, pos.y, 0, 0, OS2.SWP_MOVE);
        END (*IF*);
        app := "Font";
        IF NOT INIGetString (hini, app, label, FontName)
                      OR (FontName[0] = Nul) THEN
            FontName := "8.Helv";
        END (*IF*);
        CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE, bufsize, FontName);
    END SetInitialWindowPosition;

(************************************************************************)

PROCEDURE SetInitialWindowSize (hwnd: OS2.HWND;
                                      INIFileName, label: ARRAY OF CHAR;
                                       UseTNI: BOOLEAN);

    (* If this window has a previously stored size in our INI file, *)
    (* adjusts the window to that size.                             *)

    CONST bufsize = 256;

    VAR hini: HINI;  pos: WindowPosition;
        app: ARRAY [0..10] OF CHAR;

    BEGIN
        hini := OpenINIFile (INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "WindowSize";
        IF INIGet (hini, app, label, pos) THEN
            OS2.WinSetWindowPos (hwnd, 0, 0, 0, pos.x, pos.y, OS2.SWP_SIZE);
        END (*IF*);
        CloseINIFile (hini);
    END SetInitialWindowSize;

(************************************************************************)

PROCEDURE SetFont (hwnd: OS2.HWND;  INIFileName, label: ARRAY OF CHAR;
                                         UseTNI: BOOLEAN);

    (* Sets the font for this window, provided a font is stored in      *)
    (* the INI file.                                                    *)

    CONST bufsize = 256;

    VAR hini: HINI;  FontName: ARRAY [0..bufsize-1] OF CHAR;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        hini := OpenINIFile(INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "Font";
        IF NOT INIGetString (hini, app, label, FontName)
                      OR (FontName[0] = Nul) THEN
            FontName := "10.System Proportional";
        END (*IF*);
        CloseINIFile (hini);
        OS2.WinSetPresParam (hwnd, OS2.PP_FONTNAMESIZE, bufsize, FontName);
    END SetFont;

(************************************************************************)

PROCEDURE StoreWindowPosition (hwnd: OS2.HWND;
                                INIFileName, label: ARRAY OF CHAR;
                                 UseTNI: BOOLEAN);

    (* Saves the location of this window in our INI file. *)

    CONST bufsize = 256;

    VAR hini: HINI;  swp: OS2.SWP;
        pos: WindowPosition;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        AttrFound, length: CARDINAL;
        app: ARRAY [0..9] OF CHAR;

    BEGIN
        OS2.WinQueryWindowPos (hwnd, swp);
        pos.x := swp.x;  pos.y := swp.y;
        length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                     AttrFound,
                                     bufsize, FontName, 0(*OS2.QPF_NOINHERIT*));
        IF length < bufsize THEN
            FontName[length] := Nul;
        END (*IF*);
        hini := OpenINIFile(INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "WindowPos";
        INIPut (hini, app, label, pos);
        app := "Font";
        INIPutString (hini, app, label, FontName);
        CloseINIFile (hini);
    END StoreWindowPosition;

(************************************************************************)

PROCEDURE StoreWindowSize (hwnd: OS2.HWND;
                             INIFileName, label: ARRAY OF CHAR;
                              UseTNI: BOOLEAN);

    (* Saves the size of this window in our INI file. *)

    CONST bufsize = 256;

    VAR hini: HINI;  swp: OS2.SWP;
        pos: WindowPosition;
        app: ARRAY [0..10] OF CHAR;

    BEGIN
        OS2.WinQueryWindowPos (hwnd, swp);
        pos.x := swp.cx;  pos.y := swp.cy;
        hini := OpenINIFile (INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "WindowSize";
        INIPut (hini, app, label, pos);
        CloseINIFile (hini);
    END StoreWindowSize;

(************************************************************************)

PROCEDURE StoreFont (hwnd: OS2.HWND;   INIFileName, label: ARRAY OF CHAR;
                                    UseTNI: BOOLEAN);

    (* Saves the font of this window in our INI file. *)

    CONST bufsize = 256;

    VAR hini: HINI;
        FontName: ARRAY [0..bufsize-1] OF CHAR;
        AttrFound, length: CARDINAL;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                     AttrFound,
                                     bufsize, FontName, 0(*OS2.QPF_NOINHERIT*));
        IF length < bufsize THEN
            FontName[length] := Nul;
        END (*IF*);
        hini := OpenINIFile(INIFileName, UseTNI);
        IF hini = NIL THEN
            hini := CreateINIFile(INIFileName, UseTNI);
        END (*IF*);
        app := "Font";
        INIPutString (hini, app, label, FontName);
        CloseINIFile (hini);
    END StoreFont;

(************************************************************************)
(*                         SET CURRENT DIRECTORY                        *)
(************************************************************************)

PROCEDURE SetProgramDir;

    (* Sets the ProgramDir variable to the name of the directory        *)
    (* where the executable resides.                                    *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                        ProgramDir) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (ProgramDir);
            WHILE (j > 0) AND (ProgramDir[j] <> '\') DO
                DEC (j);
            END (*WHILE*);
            ProgramDir[j] := CHR(0);
        ELSE
            ProgramDir := "";
        END (*IF*);

    END SetProgramDir;

(************************************************************************)

PROCEDURE GetProgramDirectory (VAR (*OUT*) dir: ARRAY OF CHAR);

    (* Returns the directory where the executable resides. *)

    BEGIN
        Strings.Assign (ProgramDir, dir);
    END GetProgramDirectory;

(************************************************************************)

PROCEDURE SetWorkingDirectory;

    (* Sets the working drive and directory to be the same as that  *)
    (* where the executable resides.                                *)

    VAR PathName: ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;

    BEGIN
        Strings.Assign (ProgramDir, PathName);

        (* Extract the drive name and set working drive. *)

        IF (PathName[0] <> Nul) AND (PathName[1] = ':') THEN
            OS2.DosSetDefaultDisk (ORD(CAP(PathName[0])) - ORD('A') + 1);
            Strings.Delete (PathName, 0, 2);
        END (*IF*);

        (* Set the working directory. *)

        OS2.DosSetCurrentDir (PathName);

    END SetWorkingDirectory;

(************************************************************************)

BEGIN
    hab := OS2.WinInitialize (0);
    SetProgramDir;
FINALLY
    IF hab <> OS2.NULLHANDLE THEN
        OS2.WinTerminate (hab);
    END (*IF*);
END INIData.

