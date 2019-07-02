(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE RINIData;

        (************************************************************)
        (*                                                          *)
        (*                    INI File Operations                   *)
        (*                                                          *)
        (*     This module reads/writes user INI data either        *)
        (*     remotely (using INIServe) or locally.  The present   *)
        (*     version is intended for applications where           *)
        (*     only one INI file is open at any one time, so        *)
        (*     we can afford to store the handle as private         *)
        (*     data rather than requiring the caller to supply      *)
        (*     a handle on every call.                              *)
        (*                                                          *)
        (*      Started:        13 January 2002                     *)
        (*      Last edited:    4 May 2019                          *)
        (*      Status:         OK                                  *)
        (*                                                          *)
        (************************************************************)

IMPORT OS2, Strings, FileSys, INIData, Remote;

FROM SYSTEM IMPORT
    (* type *)  LOC, INT16;

FROM Storage IMPORT ALLOCATE, DEALLOCATE;

FROM FileOps IMPORT
    (* type *)  FilenameString;

FROM Remote IMPORT
    (* proc *)  SelectRemoteFile, OurIPAddress,
                SendCommand, ExecCommand, ExecCommand2, ReceiveLine;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer,
    (* proc *)  EVAL, HexEncodeArray, DecodeHex, DecodeHexString;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    StringReadState = POINTER TO StateRecord;
    StateRecord = RECORD
                      pos, BufferSize: CARDINAL;
                      bufptr: CharArrayPointer;
                  END (*RECORD*);

VAR Ourhini: INIData.HINI;
    RemoteFlag: BOOLEAN;

(********************************************************************************)

PROCEDURE SetRemote (option: BOOLEAN);

    (* Establishes remote operation if option = TRUE, local otherwise. *)

    BEGIN
        RemoteFlag := option;
    END SetRemote;

(********************************************************************************)

PROCEDURE RemoteOperation(): BOOLEAN;

    (* Returns TRUE iff we are working with a remote INI file. *)

    BEGIN
        RETURN RemoteFlag;
    END RemoteOperation;

(********************************************************************************)

PROCEDURE ServerIPAddress (): CARDINAL;

    (* The IP address of the local host or the INIServe server, as      *)
    (* appropriate.  The result is in network byte order.               *)

    BEGIN
        RETURN OurIPAddress (RemoteFlag);
    END ServerIPAddress;

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
    (* the returned useTNI value is arbitrary.                          *)

    VAR useINI, useTNI0, foundI, foundT: BOOLEAN;
        app: ARRAY [0..5] OF CHAR;
        Iname, Tname: FilenameString;

    (* A CommitTNIDecision, see below, will effect the result the next  *)
    (* time we have to do the check.                                    *)

    BEGIN
        IF NOT RemoteFlag THEN
            RETURN INIData.ChooseDefaultINI (appname, useTNI);
        END (*IF*);

        (* The following code only needs to handle the remote case. *)

        Strings.Assign (appname, Iname);
        Strings.Assign (appname, Tname);
        Strings.Append (".INI", Iname);
        Strings.Append (".TNI", Tname);
        useTNI := SelectRemoteFile(Tname);
        useINI := SelectRemoteFile(Iname);

        (* If only one of the files exists, the decision is obvious.    *)
        (* If neither exists, default to using INI.                     *)

        IF NOT (useINI AND useTNI) THEN
            RETURN TRUE;
        END (*IF*);

        (* That leaves the case where both files exists.  In that case  *)
        (* we look up the entry ($SYS, useTNI) in each file.            *)

        useTNI := FALSE;  useTNI0 := FALSE;
        app := "$SYS";
        foundI :=INIFetch (app, "UseTNI", useTNI0);

        useTNI := SelectRemoteFile(Tname);
        foundT :=INIFetch (app, "UseTNI", useTNI);

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

PROCEDURE CommitTNIDecision (appname: ARRAY OF CHAR;  useTNI: BOOLEAN);

    (* Stores the specified useTNI value in such a way that it will     *)
    (* become the default for the next ChooseDefaultINI decision, all   *)
    (* other factors being equal.                                       *)

    VAR app: ARRAY [0..5] OF CHAR;
        name: FilenameString;

    BEGIN
        IF NOT RemoteFlag THEN
            INIData.CommitTNIDecision (appname, useTNI);
        END (*IF*);

        (* The following code only needs to handle the remote case.     *)
        (* Store the useTNI value in whichever of the two files exist,  *)
        (* or both if both exist.  If neither exists, create only one.  *)

        app := "$SYS";
        Strings.Assign (appname, name);
        Strings.Append (".TNI", name);
        IF SelectRemoteFile(name) OR useTNI THEN
            INIPut (app, "UseTNI", useTNI);
        END (*IF*);

        Strings.Assign (appname, name);
        Strings.Append (".INI", name);
        IF SelectRemoteFile(name) OR NOT useTNI THEN
            INIPut (app, "UseTNI", useTNI);
        END (*IF*);

    END CommitTNIDecision;

(************************************************************************)
(*                  OPENING AND CLOSING THE INI FILE                    *)
(************************************************************************)

PROCEDURE OpenINIFile (filename: ARRAY OF CHAR;  useTNI: BOOLEAN): BOOLEAN;

    (* Opens the INI or TNI file. *)

    BEGIN
        IF RemoteFlag THEN
            RETURN SelectRemoteFile (filename);
        ELSE
            Ourhini := INIData.OpenINIFile (filename, useTNI);
            IF NOT INIData.INIValid (Ourhini) THEN
                Ourhini := INIData.CreateINIFile (filename, useTNI);
            END (*IF*);
            RETURN INIData.INIValid (Ourhini);
        END (*IF*);
    END OpenINIFile;

(********************************************************************************)

PROCEDURE CloseINIFile;

    BEGIN
        IF NOT RemoteFlag THEN
            INIData.CloseINIFile (Ourhini);
        END (*IF*);
    END CloseINIFile;

(********************************************************************************)
(*                               WRITING DATA                                   *)
(********************************************************************************)

PROCEDURE PutBinaryString (app, key: ARRAY OF CHAR;  VAR (*IN*) variable: ARRAY OF LOC;
                                                     N: CARDINAL);

    (* Writes N bytes of data to the INI file. *)

    VAR bufptr: CharArrayPointer;

    BEGIN
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("A", app));
            EVAL (ExecCommand2 ("K", key));
            ALLOCATE (bufptr, 2*N+1);
            HexEncodeArray (variable, N, bufptr^);
            EVAL (ExecCommand2 ("W", bufptr^));
            DEALLOCATE (bufptr, 2*N+1);
        ELSIF INIData.INIValid(Ourhini) THEN
            INIData.INIPutBinary (Ourhini, app, key, variable, N);
        END (*IF*);
    END PutBinaryString;

(********************************************************************************)

PROCEDURE INIPut (app, key: ARRAY OF CHAR;  VAR (*IN*) value: ARRAY OF LOC);

    (* Writes data to the INI file. *)

    BEGIN
        PutBinaryString (app, key, value, HIGH(value)+1);
    END INIPut;

(********************************************************************************)

PROCEDURE INIPutBinary (app, key: ARRAY OF CHAR;  VAR (*IN*) variable: ARRAY OF LOC;
                                                  NumberOfBytes: CARDINAL);

    (* Writes a binary string of the specified length to the INI file. *)

    BEGIN
        PutBinaryString (app, key, variable, NumberOfBytes);
    END INIPutBinary;

(********************************************************************************)

PROCEDURE INIPutString (app, key: ARRAY OF CHAR;
                           VAR (*IN*) variable: ARRAY OF CHAR);

    (* Writes a character string to the INI file. *)

    BEGIN
        PutBinaryString (app, key, variable, LENGTH(variable));
    END INIPutString;

(********************************************************************************)
(*                               READING DATA                                   *)
(********************************************************************************)

PROCEDURE ItemSize (app, key: ARRAY OF CHAR;  VAR (*OUT*) size: CARDINAL): BOOLEAN;

    (* Returns the number of bytes in the specified INI item.  If the item does *)
    (* not exist, returns FALSE with size set to zero.                          *)

    VAR success: BOOLEAN;
        buffer: ARRAY [0..31] OF CHAR;

    BEGIN
        success := FALSE;
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("A", app));
            EVAL (ExecCommand2 ("K", key));
            EVAL (SendCommand ("S"));
            IF ReceiveLine (buffer) THEN
                IF buffer[0] = '+' THEN
                    Strings.Delete (buffer, 0, 1);
                    size := DecodeHex (buffer);
                    success := TRUE;
                END (*IF*);
            END (*IF*);
        ELSE
            success := INIData.INIValid(Ourhini)
                 AND INIData.ItemSize (Ourhini, app, key, size);
        END (*IF*);
        IF NOT success THEN
            size := 0;
        END (*IF*);
        RETURN success;
    END ItemSize;

(********************************************************************************)

PROCEDURE AppExists (app: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff this application exists. *)

    VAR size: CARDINAL;

    BEGIN
        RETURN ItemSize (app, '', size) AND (size > 0);
    END AppExists;

(********************************************************************************)

PROCEDURE KeyExists (app, key: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff this key exists. *)

    VAR size: CARDINAL;

    BEGIN
        RETURN ItemSize (app, key, size);
    END KeyExists;

(********************************************************************************)

PROCEDURE INIFetchBinary (app, key: ARRAY OF CHAR;
                          VAR (*OUT*) result: ARRAY OF LOC;
                                            size: CARDINAL): BOOLEAN;

    (* Retrieves the value of a variable from the INI file.  Returns FALSE if   *)
    (* the variable was not found.  This the version in which we trust the      *)
    (* caller to have ensured that the size is correct.                         *)

    VAR bufptr: CharArrayPointer;

    BEGIN
        IF RemoteFlag THEN
            ALLOCATE (bufptr, 2*size+3);
            IF ExecCommand2 ("A", app) AND ExecCommand2 ("K", key)
               AND SendCommand ("V") AND ReceiveLine (bufptr^)
                  AND (bufptr^[0] = '+') THEN
                Strings.Delete (bufptr^, 0, 1);
                DecodeHexString (bufptr^, result);
                DEALLOCATE (bufptr, 2*size+3);
                RETURN TRUE;
            ELSE
                DEALLOCATE (bufptr, 2*size+3);
                RETURN FALSE;
            END (*IF*);
        ELSIF NOT INIData.INIValid(Ourhini) THEN
            RETURN FALSE;
        ELSE
            RETURN INIData.INIGetTrusted (Ourhini, app, key, result, size);
        END (*IF*);
    END INIFetchBinary;

(********************************************************************************)

PROCEDURE INIFetch (app, key: ARRAY OF CHAR;
                    VAR (*OUT*) variable: ARRAY OF LOC): BOOLEAN;

    (* Reads data from the INI file.  A size mismatch will cause failure.  *)

    VAR N: CARDINAL;

    BEGIN
        IF NOT ItemSize (app, key, N) THEN
            RETURN FALSE;
        END (*IF*);
        IF N <> HIGH(variable) + 1 THEN
            RETURN FALSE;
        END (*IF*);
        RETURN INIFetchBinary (app, key, variable, N);
    END INIFetch;

(********************************************************************************)

PROCEDURE INIGetCard (app, key: ARRAY OF CHAR;
                          VAR (*OUT*) val: CARDINAL): BOOLEAN;

    (* Gets a single cardinal value from the INI file, returns true if OK. *)

    BEGIN
        RETURN INIFetch (app, key, val);
    END INIGetCard;

(************************************************************************)

PROCEDURE INIGetTwoShort (app, key: ARRAY OF CHAR;
                          VAR (*OUT*) val1, val2: INT16): BOOLEAN;

    (* Gets a two-number pair from the INI file, returns true if OK. *)
    (* Note: each number takes 4 bytes in INI file, but we have to   *)
    (* restrict ourselves to INT16 values because of limitations of  *)
    (* the PM dialogue interface.                                    *)

    VAR value: ARRAY [0..1] OF CARDINAL;  result: BOOLEAN;

    BEGIN
        result := INIFetch (app, key, value);
        IF value[0] > MAX(INT16) THEN
            val1 := 0;
        ELSE
            val1 := value[0];
        END (*IF*);
        IF value[1] > MAX(INT16) THEN
            val2 := 0;
        ELSE
            val2 := value[1];
        END (*IF*);
        RETURN result;
    END INIGetTwoShort;

(************************************************************************)

(*
PROCEDURE INIGetThreeShort (app, key: ARRAY OF CHAR;
                          VAR (*OUT*) val1, val2, val3: INT16): BOOLEAN;

    (* Gets a three-cardinal value from the INI file, returns true if   *)
    (* OK.  Note: each number takes 4 bytes in INI file, but we have to *)
    (* restrict ourselves to INT16 values because of limitations of     *)
    (* the PM dialogue interface.                                       *)

    VAR value: ARRAY [0..2] OF CARDINAL;  result: BOOLEAN;

    BEGIN
        result := INIFetch (app, key, value);
        IF value[0] > MAX(INT16) THEN
            val1 := 0;
        ELSE
            val1 := value[0];
        END (*IF*);
        IF value[1] > MAX(INT16) THEN
            val2 := 0;
        ELSE
            val2 := value[1];
        END (*IF*);
        IF value[2] > MAX(INT16) THEN
            val3 := 0;
        ELSE
            val3 := value[1];
        END (*IF*);
        RETURN result;
    END INIGetThreeShort;
*)

(************************************************************************)

PROCEDURE INIGetString (app, key: ARRAY OF CHAR;
                          VAR (*OUT*) val: ARRAY OF CHAR): BOOLEAN;

    (* Gets a string value from the INI file, returns true if OK. *)

    VAR size: CARDINAL;

    BEGIN
        IF NOT ItemSize (app, key, size) THEN
            RETURN FALSE;
        END (*IF*);
        IF size > HIGH(val)+1 THEN
            RETURN FALSE;
        END (*IF*);
        IF INIFetchBinary (app, key, val, size) THEN
            IF size <= HIGH(val) THEN
                val[size] := Nul;
            END (*IF*);
            RETURN TRUE;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END INIGetString;

(************************************************************************)
(*              READING A STRING OF STRINGS FROM AN INI FILE            *)
(************************************************************************)

PROCEDURE GetStringList (app, key: ARRAY OF CHAR;
                                   VAR (*OUT*) state: StringReadState);

    (* Initialisation in preparation for a "NextString" operation. *)

    BEGIN
        NEW (state);

        (* Pick up the list of all list names. *)

        state^.bufptr := NIL;
        IF NOT ItemSize (app, key, state^.BufferSize) THEN
            state^.BufferSize := 0;
        END (*IF*);
        IF state^.BufferSize > 0 THEN
            ALLOCATE (state^.bufptr, state^.BufferSize);
            IF NOT INIFetchBinary (app, key, state^.bufptr^,
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

(************************************************************************)

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
(*                      COPYING, DELETING, RENAMING                     *)
(************************************************************************)

PROCEDURE INIDeleteApp (app: ARRAY OF CHAR);

    (* Deletes an application from the INI file. *)

    BEGIN
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("A", app) AND ExecCommand ("K")
                    AND ExecCommand ("D"));
        ELSIF INIData.INIValid(Ourhini) THEN
            INIData.INIDeleteApp (Ourhini, app);
        END (*IF*);
    END INIDeleteApp;

(************************************************************************)

PROCEDURE INIDeleteKey (app, key: ARRAY OF CHAR);

    (* Deletes a key from the INI file. *)

    BEGIN
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("A", app) AND ExecCommand2 ("K", key)
                    AND ExecCommand ("D"));
        ELSIF INIData.INIValid(Ourhini) THEN
            INIData.INIDeleteKey (Ourhini, app, key);
        END (*IF*);
    END INIDeleteKey;

(************************************************************************)

PROCEDURE INICopyKey (oldapp, oldkey, newapp, newkey: ARRAY OF CHAR);

    (* Creates a second copy of the data for (oldapp, oldkey).  *)

    VAR size: CARDINAL;  p: CharArrayPointer;

    BEGIN
        EVAL (ItemSize (oldapp, oldkey, size));
        IF size = 0 THEN
            INIPutBinary (newapp, newkey, p, 0);
        ELSE
            ALLOCATE (p, size);
            IF INIFetchBinary (oldapp, oldkey, p^, size) THEN
                INIPutBinary (newapp, newkey, p^, size);
            END (*IF*);
            DEALLOCATE (p, size);
        END (*IF*);
    END INICopyKey;

(************************************************************************)

PROCEDURE INICopyApp (oldapp, newapp: ARRAY OF CHAR);

    (* Creates a second copy of all data for this application.  *)

    VAR state: StringReadState;
        key: ARRAY [0..127] OF CHAR;

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            GetStringList (oldapp, "", state);
            NextString (state, key);
            WHILE key[0] <> Nul DO
                INICopyKey (oldapp, key, newapp, key);
                INIDeleteKey (oldapp, key);
                NextString (state, key);
            END (*WHILE*);
            CloseStringList (state);
        END (*IF*);
    END INICopyApp;

(************************************************************************)

PROCEDURE INIRenameApp (oldapp, newapp: ARRAY OF CHAR);

    (* Changes the name of an application, retaining the data. *)

    BEGIN
        IF NOT Strings.Equal (oldapp, newapp) THEN
            INICopyApp (oldapp, newapp);
            INIDeleteApp (oldapp);
        END (*IF*);
    END INIRenameApp;

(************************************************************************)

PROCEDURE INIRenameKey (app, oldkey, newkey: ARRAY OF CHAR);

    (* Changes the name of a key, retaining the data. *)

    VAR size: CARDINAL;  p: CharArrayPointer;

    BEGIN
        IF NOT Strings.Equal (oldkey, newkey) THEN
            EVAL (ItemSize (app, oldkey, size));
            IF size = 0 THEN
                PutBinaryString (app, newkey, p, 0);
                INIDeleteKey (app, oldkey);
            ELSE
                ALLOCATE (p, size);
                IF INIFetchBinary (app, oldkey, p^, size) THEN
                    PutBinaryString (app, newkey, p^, size);
                    INIDeleteKey (app, oldkey);
                END (*IF*);
                DEALLOCATE (p, size);
            END (*IF*);
        END (*IF*);
    END INIRenameKey;

(************************************************************************)
(*                         DIRECTORY OPERATIONS                         *)
(************************************************************************)

PROCEDURE OurDirectory (VAR (*OUT*) DirectoryName: ARRAY OF CHAR);

    (* Returns either the remote current directory or the local current *)
    (* directory, as appropriate.                                       *)

    BEGIN
        Remote.OurDirectory (RemoteFlag, DirectoryName);
    END OurDirectory;

(************************************************************************)

PROCEDURE MakeDirectory (dirname: ARRAY OF CHAR);

    (* Creates a directory on either the local or remote machine, as    *)
    (* appropriate.  Has no effect if the directory already exists.     *)

    BEGIN
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("M", dirname));
        ELSE
            EVAL (FileSys.CreateDirectory(dirname));
        END (*IF*);
    END MakeDirectory;

(************************************************************************)

PROCEDURE MoveDirectory (srcname, dstname: ARRAY OF CHAR);

    (* Executes a "move srcname, dstname" on either the local or remote *)
    (* machine, as appropriate.                                         *)

    BEGIN
        IF RemoteFlag THEN
            EVAL (ExecCommand2 ("A", srcname));
            EVAL (ExecCommand2 ("R", dstname));
        ELSE
            OS2.DosMove (srcname, dstname);

            (* Possible eCS bug: DosMove doesn't seem to remove the     *)
            (* source directory, so we'd better delete it explicitly.   *)

            OS2.DosDeleteDir (srcname);

        END (*IF*);
    END MoveDirectory;

(************************************************************************)

PROCEDURE DeleteFile (filename: ARRAY OF CHAR): BOOLEAN;

    (* Deletes a file on either the local or remote machine, as         *)
    (* appropriate.  Returns FALSE if the deletion failed.              *)

    BEGIN
        IF RemoteFlag THEN
            RETURN ExecCommand2 ("X", filename);
        ELSE
            RETURN OS2.DosDelete (filename) = 0;
        END (*IF*);
    END DeleteFile;

(************************************************************************)

PROCEDURE DeleteDirectory (dirname: ARRAY OF CHAR): BOOLEAN;

    (* Deletes a directory on either the local or remote machine, as    *)
    (* appropriate.  Returns FALSE if the deletion failed.  Note that   *)
    (* only empty directories can be deleted.                           *)

    BEGIN
        IF RemoteFlag THEN
            RETURN ExecCommand2 ("X", dirname);
        ELSE
            RETURN OS2.DosDeleteDir (dirname) = 0;
        END (*IF*);
    END DeleteDirectory;

(************************************************************************)

BEGIN
    RemoteFlag := FALSE;
END RINIData.

