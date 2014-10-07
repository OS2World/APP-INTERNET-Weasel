(**************************************************************************)
(*                                                                        *)
(*  PMOS/2 software library                                               *)
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

IMPLEMENTATION MODULE FileOps;

        (********************************************************)
        (*                                                      *)
        (*                 File utilities                       *)
        (*                                                      *)
        (*       This is the 'long file pointer' version        *)
        (*       that uses the WSeB API extensions that         *)
        (*       permit file sizes bigger than 2 GiB.           *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            17 October 2001                 *)
        (*  Last edited:        22 January 2014                 *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (*    The original version of this module used          *)
        (*    the standard Modula-2 file I/O modules to do      *)
        (*    its job, but I suspect that those modules have    *)
        (*    an unresolved critical section problem, or        *)
        (*    something similar.  The present version bypasses  *)
        (*    those modules and goes directly to API calls.     *)
        (*                                                      *)
        (********************************************************)


IMPORT OS2, OS2A, FileSys, Strings;

FROM SYSTEM IMPORT
    (* type *)  LOC, INT32,
    (* proc *)  ADR, CAST;

FROM Types IMPORT
    (* type *)  CARD64;

FROM Conversions IMPORT
    (* proc *)  CardinalToString, Card64ToString;

FROM LowLevel IMPORT
    (* proc *)  IAND, IOR;

(************************************************************************)

CONST Nul = CHR(0);

VAR
    (* LongSupport is TRUE if the OS version is high enough to support  *)
    (* 64-bit file pointers.                                            *)

    LongSupport: BOOLEAN;

    (* Dummy variable to prevent the compiler from optimising out a     *)
    (* reference, so that we can see the status result in the debugger. *)

    global: CARDINAL;

(************************************************************************)
(* Entry points of dynamically loaded procedures.                       *)
(************************************************************************)

TYPE
    OpenLProc = PROCEDURE [OS2.APIENTRY] (ARRAY OF CHAR, VAR OS2.HFILE,
                                     VAR OS2.ULONG, OS2.ULONG, OS2.ULONG,
                                     OS2.ULONG, OS2.ULONG, OS2.ULONG,
                                     VAR [NIL] OS2.EAOP2): OS2.APIRET;

    SetFilePtrLProc = PROCEDURE [OS2.APIENTRY] (OS2.HFILE, OS2.ULONG,
                                 OS2.LONG, OS2.ULONG,
                                 OS2.PULONG): OS2.APIRET;

    SetFileSizeLProc = PROCEDURE [OS2.APIENTRY] (OS2.HFILE,
                                 OS2.ULONG, OS2.ULONG): OS2.APIRET;

VAR
    OpenL: OpenLProc;
    SetFilePtrL: SetFilePtrLProc;
    SetFileSizeL: SetFileSizeLProc;

(************************************************************************)
(*                         VOLUME INFORMATION                           *)
(************************************************************************)

PROCEDURE FreeSpace (drive: CHAR): CARDINAL;

    (* Returns the amount of free space on the given drive.  The        *)
    (* result is in kilobytes.  Note that I'll have to rewrite this     *)
    (* once disk sizes go over 4096 GB.                                 *)

    VAR Buffer: OS2.FSALLOCATE;  result: REAL;

    BEGIN
        drive := CAP(drive);
        IF drive < 'A' THEN
            RETURN 0;
        END (*IF*);
        IF OS2.DosQueryFSInfo (ORD(drive) - ORD('A') + 1, 1,
                                     ADR(Buffer), SIZE(Buffer)) <> 0 THEN
            RETURN 0;
        END (*IF*);

        (* Use floating point to calculate the free space in    *)
        (* kilobytes, to avoid overflow errors on large disks.  *)

        WITH Buffer DO
            result := FLOAT(cSectorUnit) * FLOAT(cbSector)
                                         * FLOAT(cUnitAvail) / 1024.0;
        END (*WITH*);
        RETURN TRUNC(result);

    END FreeSpace;

(************************************************************************)

PROCEDURE GetCurrentDirectory (VAR (*OUT*) CurrentDir: ARRAY OF CHAR);

    (* Returns the name of the current directory. *)

    VAR size: CARDINAL;
        result: FilenameString;

    BEGIN
        size := HIGH(CurrentDir);
        OS2.DosQueryCurrentDir (0, result, size);
        Strings.Assign ('\', CurrentDir);
        Strings.Append (result, CurrentDir);
    END GetCurrentDirectory;

(************************************************************************)
(*                      GENERAL FILE OPERATIONS                         *)
(************************************************************************)

PROCEDURE IncreaseFileHandles;

    (* Adds some more file handles to the process. *)

    VAR cbReqCount: OS2.LONG;  cbCurMaxFH: OS2.ULONG;

    BEGIN
        cbReqCount := 32;
        OS2.DosSetRelMaxFH (cbReqCount, cbCurMaxFH);
    END IncreaseFileHandles;

(************************************************************************)

PROCEDURE OpenOldFile (name: ARRAY OF CHAR;  WillWrite: BOOLEAN;
                                             binary: BOOLEAN): ChanId;

    (* Opens an existing file and returns its channel ID.  If the       *)
    (* second parameter is TRUE we are requesting write as well as read *)
    (* access; if it's FALSE, we want read-only access.                 *)
    (* The 'binary' parameter is ignored in the OS/2 version, but is    *)
    (* needed in the Windows version.                                   *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW
                    + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        Mode1 = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYNONE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READONLY;
        Mode2 = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Mode, Action: CARDINAL;

    BEGIN
        IF WillWrite THEN
            Mode := Mode2;
        ELSE
            Mode := Mode1;
        END (*IF*);
        IF LongSupport THEN
            rc := OpenL (name, cid, Action, 0, 0, 0, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, 0, OpenFlags, Mode, NIL);
        END (*IF*);
        IF rc = OS2.ERROR_TOO_MANY_OPEN_FILES THEN
            IncreaseFileHandles;
            RETURN OpenOldFile (name, WillWrite, binary);
        ELSIF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenOldFile;

(************************************************************************)

PROCEDURE OpenNewFile (name: ARRAY OF CHAR;  binary: BOOLEAN): ChanId;

    (* Opens a new file and returns its channel ID.                     *)
    (* The 'binary' parameter is ignored in the OS/2 version, but is    *)
    (* needed in the Windows version.                                   *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_CREATE_IF_NEW
                    + OS2.OPEN_ACTION_FAIL_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Action: CARDINAL;

    BEGIN
        IF LongSupport THEN
            rc := OpenL (name, cid, Action, 0, 0, 0, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, 0, OpenFlags, Mode, NIL);
        END (*IF*);
        IF rc = OS2.ERROR_TOO_MANY_OPEN_FILES THEN
            IncreaseFileHandles;
            RETURN OpenNewFile (name, binary);
        ELSIF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenNewFile;

(************************************************************************)

PROCEDURE OpenNewFile0 (name: ARRAY OF CHAR;  Attributes: CARDINAL;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile, but returns an indication of whether the       *)
    (* file couldn't be created because of a name duplication.  Also    *)
    (* allows attributes to be specified.                               *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_CREATE_IF_NEW
                    + OS2.OPEN_ACTION_FAIL_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  rc: OS2.APIRET;  Action: CARDINAL;

    BEGIN
        IF LongSupport THEN
            rc := OpenL (name, cid, Action, 0, 0,
                                       Attributes, OpenFlags, Mode, NIL);
        ELSE
            rc := OS2.DosOpen (name, cid, Action, 0, Attributes, OpenFlags,
                                       Mode, NIL);
        END (*IF*);
        duplicate := (rc = OS2.ERROR_FILE_EXISTS)
                       OR (rc =OS2.ERROR_OPEN_FAILED);
        IF rc <> 0 THEN
            cid := NoSuchChannel;
        END (*IF*);
        RETURN cid;
    END OpenNewFile0;

(************************************************************************)

PROCEDURE OpenNewFile1 (name: ARRAY OF CHAR;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile, but returns an indication of whether the       *)
    (* file couldn't be created because of a name duplication.          *)

    BEGIN
        RETURN OpenNewFile0 (name, 0, duplicate);
    END OpenNewFile1;

(************************************************************************)

PROCEDURE OpenNewHiddenFile (name: ARRAY OF CHAR;
                         VAR (*OUT*) duplicate: BOOLEAN): ChanId;

    (* Like OpenNewFile1, but creates the file hidden.  *)

    BEGIN
        RETURN OpenNewFile0 (name, OS2.FILE_HIDDEN, duplicate);
    END OpenNewHiddenFile;

(************************************************************************)

PROCEDURE OpenAtEnd (name: ARRAY OF CHAR): ChanId;

    (* If the file already exists, opens it and positions the file      *)
    (* pointer at the end of the file.  If the file doesn't already     *)
    (* exist, opens a new file.                                         *)

    CONST binary = FALSE;    (* don't need binary support so far *)

    VAR cid: ChanId;  Actual: FilePos;

    BEGIN
        IF FileSys.Exists (name) THEN
            cid := OpenOldFile (name, TRUE, binary);
            IF cid <> NoSuchChannel THEN
                IF LongSupport THEN
                    SetFilePtrL (cid, 0, 0, OS2.FILE_END, ADR(Actual));
                ELSE
                    OS2.DosSetFilePtr (cid, 0, OS2.FILE_END, ADR(Actual.low));
                END (*IF*);
            END (*IF*);
        ELSE
            cid := OpenNewFile (name, binary);
        END (*IF*);
        RETURN cid;
    END OpenAtEnd;

(************************************************************************)

PROCEDURE CloseFile (cid: ChanId);

    (* Closes a file. *)

    BEGIN
        OS2.DosClose (cid);
    END CloseFile;

(************************************************************************)

PROCEDURE Flush (cid: ChanId);

    (* Sends all pending output to the file. *)

    BEGIN
        OS2.DosResetBuffer (cid);
    END Flush;

(************************************************************************)

PROCEDURE HideFileS (name: ARRAY OF CHAR;  HideIt: BOOLEAN);

    (* Hides or unhides a file, depending on the second parameter. *)
    (* This is the version for LongSupport = FALSE.                *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW
                    + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYREADWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  Action: CARDINAL;  InfoBuf: OS2.FILESTATUS3;

    BEGIN
        IF OS2.DosOpen (name, cid, Action, 0,
                        0, OpenFlags, Mode, NIL) = 0 THEN

            OS2.DosQueryFileInfo(cid, OS2.FIL_STANDARD,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            IF HideIt THEN
                InfoBuf.attrFile := IOR (InfoBuf.attrFile, 2);
            ELSE
                (* For some mysterious reason, if the 'archive' and 'hidden'    *)
                (* bits are both set, clearing the 'hidden' bit does not unhide *)
                (* the file.  To get around this, I can see no choice but to    *)
                (* clear both bits.                                             *)

                InfoBuf.attrFile := IAND (InfoBuf.attrFile, MAX(CARDINAL)-2-32);
            END (*IF*);
            OS2.DosSetFileInfo(cid, OS2.FIL_STANDARD,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            OS2.DosClose (cid);

        END (*IF*);

    END HideFileS;

(************************************************************************)

PROCEDURE HideFileL (name: ARRAY OF CHAR;  HideIt: BOOLEAN);

    (* Hides or unhides a file, depending on the second parameter. *)
    (* This is the version for LongSupport = TRUE.                 *)

    CONST
        OpenFlags = OS2.OPEN_ACTION_FAIL_IF_NEW
                    + OS2.OPEN_ACTION_OPEN_IF_EXISTS;
        Mode = OS2.OPEN_FLAGS_FAIL_ON_ERROR + OS2.OPEN_SHARE_DENYREADWRITE
                + OS2.OPEN_FLAGS_NOINHERIT
                + OS2.OPEN_ACCESS_READWRITE;

    VAR cid: ChanId;  Action: CARDINAL;  InfoBuf: OS2A.FILESTATUS3L;

    BEGIN
        IF OpenL (name, cid, Action, 0, 0,
                         0, OpenFlags, Mode, NIL) = 0 THEN

            OS2.DosQueryFileInfo(cid, OS2.FIL_STANDARDL,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            IF HideIt THEN
                InfoBuf.attrFile := IOR (InfoBuf.attrFile, 2);
            ELSE
                (* For some mysterious reason, if the 'archive' and 'hidden'    *)
                (* bits are both set, clearing the 'hidden' bit does not unhide *)
                (* the file.  To get around this, I can see no choice but to    *)
                (* clear both bits.                                             *)

                InfoBuf.attrFile := IAND (InfoBuf.attrFile, MAX(CARDINAL)-32-2);
            END (*IF*);
            OS2.DosSetFileInfo(cid, OS2.FIL_STANDARDL,
                                    ADR(InfoBuf), SIZE(InfoBuf));
            OS2.DosClose (cid);

        END (*IF*);

    END HideFileL;

(************************************************************************)

(*
PROCEDURE NameOf (f: ChanId): FilenameString;

    (* Returns the name of the file. *)
    (* Question: is this procedure used anywhere? *)

    BEGIN
        RETURN f.name;
    END NameOf;
*)

(************************************************************************)

PROCEDURE Exists (name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff 'name' already exists. *)

    VAR L: CARDINAL;

    BEGIN
        L := LENGTH(name);
        IF L > 0 THEN
            DEC (L);
            IF (name[L] = '\') OR (name[L] = '/') THEN
                name[L] := Nul;
            END (*IF*);
        END (*IF*);
        RETURN FileSys.Exists (name);
    END Exists;

(************************************************************************)

PROCEDURE HideFile (name: ARRAY OF CHAR;  HideIt: BOOLEAN);

    (* Hides or unhides a file, depending on the second parameter. *)

    BEGIN
        IF LongSupport THEN
            HideFileL (name, HideIt);
        ELSE
            HideFileS (name, HideIt);
        END (*IF*);
    END HideFile;

(************************************************************************)

PROCEDURE DeleteFile (name: ARRAY OF CHAR);

    (* Deletes a named file. *)

    VAR dummy: BOOLEAN;

    BEGIN
        FileSys.Remove (name, dummy);
    END DeleteFile;

(************************************************************************)

PROCEDURE MoveFile (oldname, newname: ARRAY OF CHAR): BOOLEAN;

    (* Renames a file, returns TRUE iff successful.  The source and     *)
    (* destination files must be on the same drive.  This procedure is  *)
    (* also a mechanism for renaming a file.                            *)

    VAR code: CARDINAL;

    BEGIN
        code := OS2.DosMove (oldname, newname);
        RETURN code = 0;
    END MoveFile;

(************************************************************************)

PROCEDURE AppendFile (src, dst: ARRAY OF CHAR): BOOLEAN;

    (* Appends src to dst, returns TRUE iff successful. *)

    VAR code: CARDINAL;

    BEGIN
        code := OS2.DosCopy (src, dst, OS2.DCPY_APPEND);
        RETURN code = 0;
    END AppendFile;

(************************************************************************)

PROCEDURE CopyFile (src, dst: ARRAY OF CHAR): BOOLEAN;

    (* Copies src to dst, returns TRUE iff successful. *)

    VAR code: CARDINAL;

    BEGIN
        code := OS2.DosCopy (src, dst, 0);
        RETURN code = 0;
    END CopyFile;

(************************************************************************)
(*                       FILE POSITION AND SIZE                         *)
(************************************************************************)

PROCEDURE CurrentPosition (cid: ChanId): FilePos;

    (* Returns the current position within the file. *)

    VAR Actual: FilePos;

    BEGIN
        Actual.high := 0;
        IF LongSupport THEN
            SetFilePtrL (cid, 0, 0, OS2.FILE_CURRENT, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_CURRENT, ADR(Actual.low));
        END (*IF*);
        RETURN Actual;
    END CurrentPosition;

(************************************************************************)

PROCEDURE StartPosition (cid: ChanId):  FilePos;

    (* Returns the start-of-file position. *)

    BEGIN
        RETURN CARD64{0,0};
    END StartPosition;

(************************************************************************)

PROCEDURE SetPositionS (cid: ChanId;  position: CARDINAL);

    (* Sets the current position within the file.  This is the version  *)
    (* where there is no long support.  The special cases that must be  *)
    (* considered here arise because the second argument to the         *)
    (* function DosSetFilePtr is a signed offset.  That means that in   *)
    (* some cases we must do the jump in two steps.                     *)

    CONST MaxJump = MAX(CARDINAL) DIV 2;

    VAR Actual: CARDINAL;  jump: INTEGER;

    BEGIN
        Actual := 0;              (* to prevent a compiler warning *)
        IF position <= MaxJump THEN
            OS2.DosSetFilePtr (cid, position, OS2.FILE_BEGIN, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_CURRENT, ADR(Actual));
            WHILE Actual <> position DO
                IF Actual > position THEN
                    IF Actual - position > MaxJump THEN
                        jump := -MaxJump;
                    ELSE
                        jump := position - Actual;
                    END (*IF*);
                ELSE
                    IF position - Actual > MaxJump THEN
                        jump := MaxJump;
                    ELSE
                        jump := position - Actual;
                    END (*IF*);
                END (*IF*);
                OS2.DosSetFilePtr (cid, jump, OS2.FILE_CURRENT, ADR(Actual));
            END (*WHILE*);
        END (*IF*);
    END SetPositionS;

(************************************************************************)

PROCEDURE EndPosition (cid: ChanId): FilePos;

    (* Returns the end-of-file position. *)

    VAR Actual, EndPos: FilePos;

    BEGIN
        Actual.high := 0;  EndPos.high := 0;
        IF LongSupport THEN
            SetFilePtrL (cid, 0, 0, OS2.FILE_CURRENT, ADR(Actual));
            SetFilePtrL (cid, 0, 0, OS2.FILE_END, ADR(EndPos));
            SetFilePtrL (cid, Actual.low, Actual.high, OS2.FILE_BEGIN, ADR(Actual));
        ELSE
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_CURRENT, ADR(Actual.low));
            OS2.DosSetFilePtr (cid, 0, OS2.FILE_END, ADR(EndPos.low));
            SetPositionS (cid, Actual.low);
        END (*IF*);
        RETURN EndPos;
    END EndPosition;

(************************************************************************)

PROCEDURE SetPosition (cid: ChanId;  position: FilePos);

    (* Sets the current position within the file. *)

    VAR Actual: FilePos;

    BEGIN
        IF LongSupport THEN
            SetFilePtrL (cid, position.low, CAST(INT32,position.high),
                                OS2.FILE_BEGIN, ADR(Actual));
        ELSE
            SetPositionS (cid, position.low);
        END (*IF*);
    END SetPosition;

(************************************************************************)

PROCEDURE GetFileSize (name: ARRAY OF CHAR): CARD64;

    (* Returns the file size.  The file should not already be open. *)

    VAR result: CARD64;
        D: DirectoryEntry;

    BEGIN
        IF FirstDirEntry (name, FALSE, TRUE, D) THEN
            result := D.size;
        ELSE
            result.high := 0;
            result.low := 0;
        END (*IF*);
        DirSearchDone (D);
        RETURN result;
    END GetFileSize;

(************************************************************************)

PROCEDURE SetFileSize (cid: ChanId;  newsize: CARD64);

    (* Changes the size of a file.  The file must already be open in    *)
    (* a mode that permits writing.  If the operation fails the file    *)
    (* keeps its old size.                                              *)

    VAR rc: OS2.APIRET;

    BEGIN
        IF LongSupport THEN
            rc := SetFileSizeL (cid, newsize.low, newsize.high);
        ELSE
            rc := OS2.DosSetFileSize (cid, newsize.low);
        END (*IF*);
        global := rc;
    END SetFileSize;

(************************************************************************)
(*                              READ                                    *)
(************************************************************************)

PROCEDURE ReadRaw (cid: ChanId;  VAR (*OUT*) data: ARRAY OF LOC;
                   limit: CARDINAL;  VAR (*OUT*) NumberRead: CARDINAL);

    (* Reads a buffer-full of information from a file. *)

    BEGIN
        OS2.DosRead (cid, ADR(data), limit, NumberRead);
    END ReadRaw;

(************************************************************************)

PROCEDURE ReadLine (cid: ChanId;  VAR (*OUT*) data: ARRAY OF CHAR);

    (* Reads a line of text from a file.  Assumption: a line ends with  *)
    (* CRLF.  To avoid having to keep a lookahead character, I take     *)
    (* the LF as end of line and skip the CR.                           *)

    CONST CR = CHR(13);  LF = CHR(10);  CtrlZ = CHR(26);

    VAR j, NumberRead: CARDINAL;
        ch: CHAR;

    BEGIN
        j := 0;  ch := Nul;
        LOOP
            OS2.DosRead (cid, ADR(ch), 1, NumberRead);
            IF NumberRead = 0 THEN
                IF j = 0 THEN
                    data[0] := CtrlZ;  j := 1;
                END (*IF*);
                EXIT (*LOOP*);
            ELSIF ch = CR THEN
                (* ignore carriage return. *)
            ELSIF ch = LF THEN
                EXIT (*LOOP*);
            ELSIF j <= HIGH(data) THEN
                data[j] := ch;  INC(j);
            END (*IF*);
        END (*LOOP*);

        IF j <= HIGH(data) THEN
            data[j] := Nul;
        END (*IF*);

    END ReadLine;

(************************************************************************)
(*                               WRITE                                  *)
(************************************************************************)

PROCEDURE WriteRaw (cid: ChanId;  VAR (*IN*) data: ARRAY OF LOC;
                                            amount: CARDINAL);

    (* Writes a binary string to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(data), amount, actual);
    END WriteRaw;

(************************************************************************)

PROCEDURE WriteRawV (cid: ChanId;  data: ARRAY OF LOC;  amount: CARDINAL);

    (* Like WriteRaw, but data passed by value. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(data), amount, actual);
    END WriteRawV;

(************************************************************************)

PROCEDURE FWriteChar (cid: ChanId;  character: CHAR);

    (* Writes a single character to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(character), 1, actual);
    END FWriteChar;

(************************************************************************)

PROCEDURE FWriteHexDigit (cid: ChanId;  value: CARDINAL);

    (* Writes a one digit hexadecimal number to a file. *)

    BEGIN
        IF value < 10 THEN
            FWriteChar (cid, CHR(ORD('0') + value));
        ELSE
            FWriteChar (cid, CHR(ORD('A') - 10 + value));
        END (*IF*);
    END FWriteHexDigit;

(************************************************************************)

PROCEDURE FWriteHexByte (cid: ChanId;  value: CARDINAL);

    (* Writes a two digit hexadecimal number to a file. *)

    BEGIN
        FWriteHexDigit (cid, value DIV 16);
        FWriteHexDigit (cid, value MOD 16);
    END FWriteHexByte;

(************************************************************************)

PROCEDURE FWriteString (cid: ChanId;  string: ARRAY OF CHAR);

    (* Writes a string to a file. *)

    VAR actual: CARDINAL;

    BEGIN
        OS2.DosWrite (cid, ADR(string), LENGTH(string), actual);
    END FWriteString;

(************************************************************************)

PROCEDURE FWriteLn (cid: ChanId);

    (* Writes end-of-line to the file. *)

    TYPE TwoChar = ARRAY [0..1] OF CHAR;
    CONST CRLF = TwoChar {CHR(13), CHR(10)};

    BEGIN
        WriteRawV (cid, CRLF, 2);
    END FWriteLn;

(************************************************************************)

PROCEDURE FWriteCard (cid: ChanId;  value, fieldwidth: CARDINAL);

    (* Converts value to decimal and writes it to the file. *)

    VAR Buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        WriteRaw (cid, Buffer, fieldwidth);
    END FWriteCard;

(************************************************************************)

PROCEDURE FWriteCard64 (cid: ChanId;  value: CARD64;  fieldwidth: CARDINAL);

    (* Converts value to decimal and writes it to the file. *)

    VAR Buffer: ARRAY [0..63] OF CHAR;

    BEGIN
        Card64ToString (value, Buffer, fieldwidth);
        WriteRaw (cid, Buffer, fieldwidth);
    END FWriteCard64;

(************************************************************************)

PROCEDURE FWriteZCard (cid: ChanId;  value, fieldwidth: CARDINAL);

    (* Like FWriteCard, but with 0 fill at the left. *)

    VAR Buffer: ARRAY [0..63] OF CHAR;  j: CARDINAL;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        j := 0;
        WHILE Buffer[j] = ' ' DO
            Buffer[j] := '0';  INC(j);
        END (*WHILE*);
        WriteRaw (cid, Buffer, fieldwidth);
    END FWriteZCard;

(************************************************************************)

PROCEDURE FWriteLJString (cid: ChanId;  Buffer: ARRAY OF CHAR);

    (* Writes the Buffer contents after deleting leading spaces. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        WHILE Buffer[j] = ' ' DO INC(j) END (*WHILE*);
        IF j > 0 THEN
            Strings.Delete (Buffer, 0, j);
        END (*IF*);
        j := 0;
        WHILE (j < HIGH(Buffer)) AND (Buffer[j] <> Nul) DO
            INC (j);
        END (* WHILE *);
        WriteRaw (cid, Buffer, j);
    END FWriteLJString;

(************************************************************************)

PROCEDURE FWriteLJCard (cid: ChanId;  value: CARDINAL);

    (* Converts value to decimal and writes it left justified. *)

    CONST fieldwidth = 64;

    VAR Buffer: ARRAY [0..fieldwidth-1] OF CHAR;

    BEGIN
        CardinalToString (value, Buffer, fieldwidth);
        FWriteLJString (cid, Buffer);
    END FWriteLJCard;

(************************************************************************)

PROCEDURE FWriteLJCard64 (cid: ChanId;  value: CARD64);

    (* Converts value to decimal and writes it left justified. *)

    CONST fieldwidth = 64;

    VAR Buffer: ARRAY [0..fieldwidth-1] OF CHAR;

    BEGIN
        Card64ToString (value, Buffer, fieldwidth);
        FWriteLJString (cid, Buffer);
    END FWriteLJCard64;

(************************************************************************)
(*                        DIRECTORY SEARCHES                            *)
(************************************************************************)

PROCEDURE ConvertFindResultS (VAR (*IN*) FindBuffer: OS2.FILEFINDBUF3;
                              VAR (*OUT*) D: DirectoryEntry);

    (* Copies the result of a directory lookup to the format we're using. *)

    BEGIN
        D.attr      := CAST (FileAttr, FindBuffer.attrFile);
        D.timePkd   := FindBuffer.ftimeLastWrite;
        D.datePkd   := FindBuffer.fdateLastWrite;
        D.timeCre   := FindBuffer.ftimeCreation;
        D.dateCre   := FindBuffer.fdateCreation;
        D.size.low  := FindBuffer.cbFile;
        D.size.high := 0;
        Strings.Assign (FindBuffer.achName, D.name);
    END ConvertFindResultS;

(************************************************************************)

PROCEDURE ConvertFindResultL (VAR (*IN*) FindBuffer: OS2A.FILEFINDBUF3L;
                              VAR (*OUT*) D: DirectoryEntry);

    (* Copies the result of a directory lookup to the format we're using. *)

    BEGIN
        D.attr    := CAST (FileAttr, FindBuffer.attrFile);
        D.timePkd := FindBuffer.ftimeLastWrite;
        D.datePkd := FindBuffer.fdateLastWrite;
        D.timeCre := FindBuffer.ftimeCreation;
        D.dateCre := FindBuffer.fdateCreation;
        D.size    := FindBuffer.cbFile;
        Strings.Assign (FindBuffer.achName, D.name);
    END ConvertFindResultL;

(************************************************************************)

PROCEDURE FirstDirEntryS (mask: ARRAY OF CHAR;  attrib: CARDINAL;
                                  VAR (*OUT*) D: DirectoryEntry): BOOLEAN;

    (* Gets the first directory entry matching 'attrib'.  Parameter     *)
    (* "mask" is a filename specification that may include wildcards.   *)
    (* This version is for the case LongSupport = FALSE.                *)

    CONST ResultBufLen = SIZE(OS2.FILEFINDBUF3);

    VAR FindBuffer: OS2.FILEFINDBUF3;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        FindCount := 1;
        rc := OS2.DosFindFirst (mask, D.dirHandle, attrib,
                                ADR(FindBuffer), ResultBufLen,
                                FindCount, OS2.FIL_STANDARD);
        ConvertFindResultS (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END FirstDirEntryS;

(************************************************************************)

PROCEDURE FirstDirEntryL (mask: ARRAY OF CHAR;  attrib: CARDINAL;
                                  VAR (*OUT*) D: DirectoryEntry): BOOLEAN;

    (* Gets the first directory entry matching 'attrib'.  Parameter     *)
    (* "mask" is a filename specification that may include wildcards.   *)
    (* This version is for the case LongSupport = TRUE.                 *)

    CONST ResultBufLen = SIZE(OS2A.FILEFINDBUF3L);

    VAR FindBuffer: OS2A.FILEFINDBUF3L;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        FindCount := 1;
        rc := OS2.DosFindFirst (mask, D.dirHandle, attrib,
                                ADR(FindBuffer), ResultBufLen,
                                FindCount, OS2.FIL_STANDARDL);
        ConvertFindResultL (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END FirstDirEntryL;

(************************************************************************)

PROCEDURE FirstDirEntry (mask: ARRAY OF CHAR;
                             Subdirectory, AllowHidden: BOOLEAN;
                                  VAR (*OUT*) D: DirectoryEntry): BOOLEAN;

    (* Gets the first directory entry satisfying the conditions:        *)
    (*  (a) if Subdirectory is FALSE, we want the first entry that      *)
    (*      matches "mask".                                             *)
    (*  (b) if Subdirectory is TRUE, we want the first directory that   *)
    (*      matches "mask".                                             *)
    (* In either case "mask" is a filename specification that may       *)
    (* include wildcards.  Hidden files are included in the search iff  *)
    (* AllowHidden is TRUE.                                             *)

    CONST ResultBufLen = SIZE(OS2.FILEFINDBUF3);

    VAR attrib: CARDINAL;

    BEGIN
        OS2.DosError (OS2.FERR_DISABLEHARDERR);
        D.dirHandle := OS2.HDIR_CREATE;
        IF Subdirectory THEN
            attrib := 1035H;
        ELSE
            attrib := 035H;
        END (*IF*);
        IF AllowHidden THEN
            INC (attrib, 2);
        END (*IF*);
        IF LongSupport THEN
            RETURN FirstDirEntryL (mask, attrib, D);
        ELSE
            RETURN FirstDirEntryS (mask, attrib, D);
        END (*IF*);
    END FirstDirEntry;

(************************************************************************)

PROCEDURE NextDirEntryS (VAR (*INOUT*) D: DirectoryEntry): BOOLEAN;

    (* Read the next directory entry satisfying the search criteria     *)
    (* specified by the FirstDirEntry call.                             *)

    CONST ResultBufLen = SIZE(OS2.FILEFINDBUF3);

    VAR FindBuffer: OS2.FILEFINDBUF3;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        FindCount := 1;
        rc := OS2.DosFindNext(D.dirHandle, ADR(FindBuffer),
                              ResultBufLen, FindCount);
        ConvertFindResultS (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END NextDirEntryS;

(************************************************************************)

PROCEDURE NextDirEntryL (VAR (*INOUT*) D: DirectoryEntry): BOOLEAN;

    (* Read the next directory entry satisfying the search criteria     *)
    (* specified by the FirstDirEntry call.                             *)

    CONST ResultBufLen = SIZE(OS2A.FILEFINDBUF3L);

    VAR FindBuffer: OS2A.FILEFINDBUF3L;
        FindCount: CARDINAL;
        rc: OS2.APIRET;

    BEGIN
        FindCount := 1;
        rc := OS2.DosFindNext(D.dirHandle, ADR(FindBuffer),
                              ResultBufLen, FindCount);
        ConvertFindResultL (FindBuffer, D);
        RETURN rc = OS2.NO_ERROR;
    END NextDirEntryL;

(************************************************************************)

PROCEDURE NextDirEntry (VAR (*INOUT*) D: DirectoryEntry): BOOLEAN;

    (* Read the next directory entry satisfying the search criteria     *)
    (* specified by the FirstDirEntry call.                             *)

    BEGIN
        IF LongSupport THEN
            RETURN NextDirEntryL(D);
        ELSE
            RETURN NextDirEntryS(D);
        END (*IF*);
    END NextDirEntry;

(************************************************************************)

PROCEDURE DirSearchDone (VAR (*INOUT*) D: DirectoryEntry);

    (* Close the directory that D represents. *)

    BEGIN
        OS2.DosFindClose (D.dirHandle);
    END DirSearchDone;

(************************************************************************)
(*                      QUERYING THE SYSTEM VERSION                     *)
(************************************************************************)

PROCEDURE CheckSystemVersion;

    (* Checks whether the system version is high enough to support      *)
    (* files of size > 2GB.                                             *)

    VAR pfn: OS2.PFN;
        hmod: OS2.HMODULE;
        rc: OS2.APIRET;
        FailureObject: ARRAY [0..511] OF CHAR;

    BEGIN
        rc := OS2.DosLoadModule (FailureObject, SIZE(FailureObject),
                             'DOSCALLS', hmod);
        global := rc;
        LongSupport := rc = 0;

        IF LongSupport THEN

            (* 981 is the ordinal number of DosOpenL. *)

            rc := OS2.DosQueryProcAddr(hmod, 981, NIL, pfn);
            OpenL := CAST (OpenLProc, pfn);
            global := rc;
            LongSupport := rc = 0;
        END (*IF*);

        IF LongSupport THEN

            (* 988 is the ordinal number of DosSetFilePtrL. *)

            rc := OS2.DosQueryProcAddr(hmod, 988, NIL, pfn);
            SetFilePtrL := CAST (SetFilePtrLProc, pfn);
            global := rc;
            LongSupport := rc = 0;
        END (*IF*);

        IF LongSupport THEN

            (* 989 is the ordinal number of DosSetFileSizeL. *)

            rc := OS2.DosQueryProcAddr(hmod, 989, NIL, pfn);
            SetFileSizeL := CAST (SetFileSizeLProc, pfn);
            global := rc;
            LongSupport := rc = 0;
        END (*IF*);

    END CheckSystemVersion;

(************************************************************************)

PROCEDURE GetEXEDirectory (VAR (*OUT*) Dir: ARRAY OF CHAR);

    (* Returns the name of the directory holding this executable. *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                              Dir) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (Dir);
            WHILE (j > 0) AND (Dir[j] <> '\') DO
                DEC (j);
            END (*WHILE*);

        ELSE
            j := 0;
        END (*IF*);

        Dir[j] := Nul;

    END GetEXEDirectory;

(************************************************************************)

PROCEDURE GetProgName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the name of this executable, with path and extension     *)
    (* removed.                                                         *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        j, L: CARDINAL;  found: BOOLEAN;
        longname: FilenameString;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                              longname) <> OS2.NO_ERROR THEN
            longname[0] := Nul;
        END (*IF*);

        L := LENGTH (longname);
        IF L > 0 THEN

            (* Find the last '\' or '/'. *)

            j := L-1;
            WHILE (j > 0) AND (longname[j] <> '\') AND (longname[j] <> '/') DO
                DEC (j);
            END (*WHILE*);

            (* Strip the drive and path. *)

            IF (longname[j] = '\') OR (longname[j] = '/') THEN
                Strings.Delete (longname, 0, j+1);
                DEC (L, j+1);
            END (*IF*);
        END (*IF*);

        IF L > 0 THEN
            (* Remove the extension, if any. *)

            Strings.FindPrev ('.', longname, L-1, found, j);
            IF found THEN
                longname[j] := Nul;
            END (*IF*);
        END (*IF*);

        Strings.Assign (longname, name);

    END GetProgName;

(************************************************************************)

(*
PROCEDURE SetWorkingDirectory;

    (* Sets the working drive and directory to be the same as that  *)
    (* where the executable resides.                                *)

    VAR pPib: OS2.PPIB;  pTib: OS2.PTIB;
        PathName: ARRAY [0..OS2.CCHMAXPATH-1] OF CHAR;
        j: CARDINAL;

    BEGIN
        OS2.DosGetInfoBlocks (pTib, pPib);
        IF OS2.DosQueryModuleName (pPib^.pib_hmte, OS2.CCHMAXPATH,
                                        PathName) = OS2.NO_ERROR THEN

            (* Strip the string back to just before the last '\'. *)

            j := LENGTH (PathName);
            WHILE (j > 0) AND (PathName[j] <> '\') DO
                DEC (j);
            END (*WHILE*);
            PathName[j] := Nul;

            (* Extract the drive name and set working drive. *)

            IF (j > 1) AND (PathName[1] = ':') THEN
                OS2.DosSetDefaultDisk (ORD(CAP(PathName[0])) - ORD('A') + 1);
                Strings.Delete (PathName, 0, 2);
            END (*IF*);

            (* Set the working directory. *)

            OS2.DosSetCurrentDir (PathName);

        END (*IF*);

    END SetWorkingDirectory;
*)

(************************************************************************)

BEGIN
    OS2.DosError (OS2.FERR_DISABLEHARDERR); (* disable hard error popups *)
    CheckSystemVersion;
    (*OS2.DosSetMaxFH (500);*)      (* Call removed -- obsolete *)
END FileOps.

