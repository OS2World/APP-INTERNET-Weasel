(**************************************************************************)
(*                                                                        *)
(*  Support modules for network applications                              *)
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

IMPLEMENTATION MODULE TNIData;

        (************************************************************)
        (*                                                          *)
        (*            Looking after text-based INI data             *)
        (*                                                          *)
        (*    Started:        26 June 2005                          *)
        (*    Last edited:    22 May 2017                           *)
        (*    Status:         Now working, I believe                *)
        (*                                                          *)
        (************************************************************)


IMPORT Strings, FileSys;

FROM SYSTEM IMPORT
    (* type *)  LOC, ADDRESS, CARD8,
    (* proc *)  ADR, CAST;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId, FilenameString,
    (* proc *)  OpenOldFile, OpenNewFile, OpenNewFile1,
                CloseFile, ReadLine, WriteRaw, FWriteChar, FWriteString,
                FWriteLn, DeleteFile, MoveFile;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  Copy, AddOffset, EVAL;

FROM Heap IMPORT
    (* type *)  Track,
    (* proc *)  ALLOCATE, DEALLOCATE
                <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                , StartTracking, TrackUpdate
                <* END *>
                ;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM TaskControl IMPORT
    (* proc *)  CreateTask;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCard;

(************************************************************************)

CONST
    Nul = CHR(0);   CtrlZ = CHR(26);

TYPE
    (* Category that tells us how to write an item to the TNI file.     *)

    ItemKind = (ObsoleteItem, HexItem, NumericItem,
                              StringItem, String0Item, StringOfStrings);

    CharPtr = POINTER TO ARRAY [0..65535] OF CHAR;
    ByteArrayPtr = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CARD8;
    BytePtr = POINTER TO CARD8;

    (* Our method of storing a variable-length byte sequence. *)

    StrPtr = RECORD
                 length: CARDINAL;
                 start: ADDRESS;
             END (*RECORD*);

    (* Structures by which we store INI data in memory. *)

    KeyPtr = POINTER TO KeyRecord;
    KeyRecord =   RECORD
                      next: KeyPtr;
                      name: StrPtr;
                      val: StrPtr;
                      kind: ItemKind;
                      ComponentSize: CARDINAL;
                  END (*RECORD*);

    AppPtr = POINTER TO AppRecord;
    AppRecord =   RECORD
                      next: AppPtr;
                      FirstKey: KeyPtr;
                      name: StrPtr;
                  END (*RECORD*);

    THandle = POINTER TO TNIStructure;

    TNIStructure = RECORD
                       access: Lock;
                       opencount: CARDINAL;
                       changed: BOOLEAN;
                       next: THandle;
                       FirstApp: AppPtr;
                       filename: FilenameString;
                   END (*RECORD*);

    CharSet = SET OF CHAR;

CONST
    DecimalDigits = CharSet {'0'..'9'};
    ExtraHexDigits = CharSet {'A'..'Z', 'a'..'z'};
    HexDigits = DecimalDigits + ExtraHexDigits;

(************************************************************************)

VAR
    (* Linked list of all TNIStructure records. *)

    TNIChain: THandle;

    (* Critical section protection for TNIChain. *)

    MasterLock: Lock;

    (* String used in creating a unique file name. *)

    NextName: ARRAY [0..7] OF CHAR;
    NextNameLock: Lock;

    (* Counter used for debugging. *)

    appcount: CARDINAL;

    <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
        (* Variables needed for logging memory usage. *)

        TNITrack: Track;
    <* END *>

(************************************************************************)
(*                        MISCELLANEOUS UTILITIES                       *)
(************************************************************************)

(*
PROCEDURE WriteCard (N: CARDINAL);

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        STextIO.WriteChar (CHR(N + ORD('0')));
    END WriteCard;

(************************************************************************)

PROCEDURE DumpCountOfApps (hini: THandle);

    (* We re-count the applications on each call. *)

    VAR count: CARDINAL;  p: AppPtr;

    BEGIN
        count := 0;
        p := hini^.FirstApp;
        WHILE p <> NIL DO
            INC (count);
            p := p^.next;
        END (*WHILE*);
        STextIO.WriteString ("We now have ");
        WriteCard (count);
        STextIO.WriteString (" applications stored.");
        STextIO.WriteLn;
    END DumpCountOfApps;
*)

(************************************************************************)

PROCEDURE StringMatch (str1, str2: ARRAY OF CHAR): BOOLEAN;

    (* Checks if str1 and str2 are equal, modulo character case. *)

    BEGIN
        Strings.Capitalize (str1);
        Strings.Capitalize (str2);
        RETURN Strings.Equal (str1, str2);
    END StringMatch;

(************************************************************************)

PROCEDURE IdentifyByFilename (VAR (*IN*) filename: ARRAY OF CHAR): THandle;

    (* Translates a hini into a THandle.  We assume that the caller     *)
    (* has obtained MasterLock.                                         *)

    VAR result: THandle;

    BEGIN
        result := TNIChain;
        WHILE (result <> NIL)
                     AND NOT StringMatch (result^.filename, filename) DO
            result := result^.next;
        END (*WHILE*);
        RETURN result;
    END IdentifyByFilename;

(************************************************************************)

PROCEDURE TextToStrPtr (VAR (*IN*) text: ARRAY OF CHAR;
                                                 VAR (*OUT*) V: StrPtr);

    (* Converts a text string to StrPtr representation. *)

    VAR size: CARDINAL;

    BEGIN
        size := LENGTH(text);
        ALLOCATE (V.start, size);
        <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
            TrackUpdate (TNITrack, size);
        <* END *>
        Copy (ADR(text), V.start, size);
        V.length := size;
    END TextToStrPtr;

(************************************************************************)

PROCEDURE BuildAppList (TS: THandle;  VAR (*INOUT*) list: StrPtr);

    (* Fills 'list' with a list of null-terminated application names.   *)
    (* On entry list.length has already been given the right value,     *)
    (* and the space has been allocated.  We assume that the caller has *)
    (* exclusive access to TS.                                          *)

    VAR p: AppPtr;
        q: POINTER TO CHAR;
        length: CARDINAL;

    BEGIN
        IF (TS <> NIL) AND (list.length > 0) THEN
            q := list.start;
            p := TS^.FirstApp;
            WHILE p <> NIL DO
                length := p^.name.length;
                Copy (p^.name.start, q, length);
                q := AddOffset (q, length);
                q^ := Nul;
                q := AddOffset (q, 1);
                p := p^.next;
            END (*WHILE*);
            q^ := Nul;
        END (*IF*);
    END BuildAppList;

(************************************************************************)

PROCEDURE BuildKeyList (app: AppPtr;  VAR (*INOUT*) list: StrPtr);

    (* Like BuildAppList, but builds a list of key names.       *)
    (* We assume that the caller has exclusive access to app.   *)

    VAR p: KeyPtr;
        q: POINTER TO CHAR;
        length: CARDINAL;

    BEGIN
        IF (app <> NIL) AND (list.length > 0) THEN
            q := list.start;
            p := app^.FirstKey;
            WHILE p <> NIL DO
                length := p^.name.length;
                Copy (p^.name.start, q, length);
                q := AddOffset (q, length);
                q^ := Nul;
                q := AddOffset (q, 1);
                p := p^.next;
            END (*WHILE*);
            q^ := Nul;
        END (*IF*);
    END BuildKeyList;

(************************************************************************)
(*                    FINDING AN APPLICATION OR KEY                     *)
(************************************************************************)

PROCEDURE MatchString (p: CharPtr;  VAR (*IN*) str: ARRAY OF CHAR;
                                            length: CARDINAL): BOOLEAN;

    (* Returns TRUE IFF the first length characters of p^ are equal to  *)
    (* the corresponding characters of str.                             *)

    VAR j: CARDINAL;  result: BOOLEAN;

    BEGIN
        IF length = 0 THEN
            RETURN TRUE;
        ELSE
            result := TRUE;
            j := 0;
            LOOP
                IF j >= length THEN
                    EXIT (*LOOP*);
                END (*IF*);
                result := p^[j] = str[j];
                IF result THEN
                    INC (j);
                ELSE
                    EXIT (*LOOP*);
                END (*IF*);
            END (*LOOP*);
        END (*IF*);
        RETURN result;
    END MatchString;

(************************************************************************)

PROCEDURE NameMatch (name: StrPtr;  VAR (*IN*) desired: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name matches desired (case-sensitive). *)

    VAR L: CARDINAL;

    BEGIN
        L := LENGTH(desired);
        IF L <> name.length THEN
            RETURN FALSE;
        ELSIF L = 0 THEN
            RETURN TRUE;
        ELSE
            RETURN MatchString (name.start, desired, L);
        END (*IF*);
    END NameMatch;

(************************************************************************)

PROCEDURE NameCompare (name: StrPtr;  VAR (*IN*) str: ARRAY OF CHAR): INTEGER;

    (* Returns -1 if name<str, 0 if name=str, +1 otherwise.  *)
    (* The comparison is case-sensitive.                     *)

    VAR j, L: CARDINAL;
        p: CharPtr;

    BEGIN
        p := name.start;
        L := Strings.Length(str);
        j := 0;
        LOOP
            IF j = name.length THEN
                IF j = L THEN
                    RETURN 0;
                ELSE
                    RETURN -1;
                END (*IF*);
            END (*IF*);
            IF p^[j] < str[j] THEN
                RETURN -1;
            ELSIF p^[j] > str[j] THEN
                RETURN +1;
            END (*IF*);
            INC (j);
        END (*LOOP*);
    END NameCompare;

(************************************************************************)

PROCEDURE FindApp (TS: THandle;  VAR (*IN*) application: ARRAY OF CHAR;
                                     InsertIfMissing: BOOLEAN): AppPtr;

    (* We assume that the caller has exclusive access to TS.   *)

    VAR previous, result: AppPtr;  code: INTEGER;

    BEGIN
        previous := NIL;
        IF TS = NIL THEN
            result := NIL;
        ELSE
            result := TS^.FirstApp;
            LOOP
                IF result = NIL THEN EXIT(*LOOP*) END(*IF*);
                code := NameCompare (result^.name, application);
                IF code > 0 THEN
                    result := NIL;
                END (*IF*);
                IF code >= 0 THEN EXIT(*LOOP*) END(*IF*);
                previous := result;
                result := result^.next;
            END (*LOOP*);
        END (*IF*);

        (* Insert a new application record if necessary. *)

        IF InsertIfMissing AND (result = NIL) THEN
            NEW (result);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, SIZE(AppRecord));
            <* END *>
            result^.FirstKey := NIL;
            TextToStrPtr (application, result^.name);
            IF previous = NIL THEN
                result^.next := TS^.FirstApp;
                TS^.FirstApp := result;
            ELSE
                result^.next := previous^.next;
                previous^.next := result;
            END (*IF*);

            (* Debug messages. *)

            (*
            INC (appcount);
            STextIO.WriteString ("Adding application number ");
            WriteCard (appcount);
            IF previous = NIL THEN
                STextIO.WriteString (" at beginning of list");
            ELSIF result^.next = NIL THEN
                STextIO.WriteString (" at end of list");
            END (*IF*);
            STextIO.WriteLn;
            DumpCountOfApps (TNIChain);
            STextIO.SkipLine;
            *)

        END (*IF*);

        RETURN result;

    END FindApp;

(************************************************************************)

PROCEDURE FindKey (AP: AppPtr;  VAR (*IN*) key: ARRAY OF CHAR;
                                InsertIfMissing: BOOLEAN): KeyPtr;

    (* We assume that the caller has exclusive access to AP.   *)

    VAR previous, result: KeyPtr;  code: INTEGER;

    BEGIN
        previous := NIL;
        IF AP = NIL THEN
            result := NIL;
        ELSE
            result := AP^.FirstKey;
            LOOP
                IF result = NIL THEN EXIT(*LOOP*) END(*IF*);
                code := NameCompare (result^.name, key);
                IF code > 0 THEN
                    result := NIL;
                END (*IF*);
                IF code >= 0 THEN EXIT(*LOOP*) END(*IF*);
                previous := result;
                result := result^.next;
            END (*LOOP*);
        END (*IF*);

        (* Insert a new key record, with Null value, if necessary. *)

        IF InsertIfMissing AND (result = NIL) AND (AP <> NIL) THEN
            NEW (result);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, SIZE(KeyRecord));
            <* END *>
            TextToStrPtr (key, result^.name);
            result^.val.start := NIL;
            result^.val.length := 0;
            result^.kind := HexItem;
            result^.ComponentSize := 1;
            IF previous = NIL THEN
                result^.next := AP^.FirstKey;
                AP^.FirstKey := result;
            ELSE
                result^.next := previous^.next;
                previous^.next := result;
            END (*IF*);

        END (*IF*);

        RETURN result;

    END FindKey;

(************************************************************************)
(*                          SETTING A VALUE                             *)
(************************************************************************)

PROCEDURE SetValue (AP: AppPtr;
                    VAR (*IN*) key: ARRAY OF CHAR;  pval: StrPtr);

    (* Uses pval to set the new value.  A previous value, if any, is    *)
    (* discarded.  Assumption: AP <> NIL.                               *)
    (* We assume that the caller has exclusive access to AP.            *)
    (* The returned value is the net increase in allocated storage; it  *)
    (* can be negative because of deallocation of an old value.         *)

    VAR place: KeyPtr;  amount: CARDINAL;

    BEGIN
        place := FindKey (AP, key, TRUE);
        amount := place^.val.length;
        IF amount > 0 THEN
            DEALLOCATE (place^.val.start, amount);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -VAL(INTEGER,amount));
            <* END *>
            place^.val.length := 0;
        END (*IF*);
        place^.val := pval;
    END SetValue;

(************************************************************************)
(************************************************************************)
(***                                                                  ***)
(***         LOADING THE DATA FROM A DISK FILE TO MAIN MEMORY         ***)
(***                                                                  ***)
(************************************************************************)
(************************************************************************)

PROCEDURE GetNextLine (cid: ChanId;  VAR (*OUT*) line: ARRAY OF CHAR);

    (* Reads the next nonblank line, and removes leading spaces and tabs. *)

    CONST Tab = CHR(8);

    VAR k: CARDINAL;

    BEGIN
        REPEAT
            ReadLine (cid, line);
            k := 0;
            WHILE (line[k] = ' ') OR (line[k] = Tab) DO
                INC (k);
            END (*WHILE*);
            IF k > 0 THEN
                Strings.Delete (line, 0, k);
            END (*IF*);
        UNTIL line[0] <> Nul;
    END GetNextLine;

(************************************************************************)

PROCEDURE SkipSpaces (VAR (*IN*) line: ARRAY OF CHAR;
                      VAR (*INOUT*) bufpos: CARDINAL);

    (* Moves bufpos past space characters. *)

    BEGIN
        WHILE line[bufpos] = ' ' DO
            INC (bufpos);
        END (*WHILE*);
    END SkipSpaces;

(************************************************************************)

PROCEDURE AppendString (VAR (*INOUT*) V: StrPtr;
                              VAR (*IN*) newstr: ARRAY OF CHAR;
                                                  AddNul: BOOLEAN);

    (* Appends a new string to an existing one. *)

    VAR size, newlength: CARDINAL;
        p, q: POINTER TO CHAR;

    BEGIN
        size := LENGTH(newstr);
        newlength := V.length + size;
        IF AddNul THEN
            INC (newlength);
        END (*IF*);
        ALLOCATE (p, newlength);
        <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
            TrackUpdate (TNITrack, newlength);
        <* END *>
        Copy (V.start, p, V.length);
        q := AddOffset (p, V.length);
        IF size > 0 THEN
            Copy (ADR(newstr), q, size);
        END (*IF*);
        IF V.length > 0 THEN
            DEALLOCATE (V.start, V.length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -VAL(INTEGER,V.length));
            <* END *>
        END (*IF*);
        V.start := p;
        V.length := newlength;
        IF AddNul THEN
            q := AddOffset (p, V.length-1);
            q^ := Nul;
        END (*IF*);
    END AppendString;

(************************************************************************)

PROCEDURE LoadStringOfStrings (cid: ChanId;  app: AppPtr;
                               VAR (*INOUT*) line: ARRAY OF CHAR);

    (* This is a special case: we have a block surrounded by [key] and  *)
    (* [/key] delimiters, where each line in the block is to be         *)
    (* interpreted as a text string, and the whole must be loaded as a  *)
    (* sequence of Nul-terminated strings.                              *)
    (* The returned value is the net increase in allocated storage; it  *)
    (* can be negative because of deallocation of an old value.         *)

    VAR pos: CARDINAL;  found: BOOLEAN;
        V: StrPtr;
        NulStr: ARRAY [0..0] OF CHAR;
        KeyName: ARRAY [0..255] OF CHAR;

    BEGIN
        pos := Strings.Length(line) - 1;
        Strings.FindPrev (']', line, pos, found, pos);
        IF found AND (pos > 1) THEN
            Strings.Extract (line, 1, pos-1, KeyName);
        ELSE
            KeyName := "?";  pos := 2;
        END (*IF*);
        V.length := 0;  V.start := NIL;
        GetNextLine (cid, line);
        WHILE (line[0] <> CtrlZ) AND ((line[0] <> '[') OR (line[1] <> '/')) DO
            AppendString (V, line, TRUE);
            GetNextLine (cid, line);
        END (*WHILE*);
        NulStr[0] := Nul;
        AppendString (V, NulStr, TRUE);
        SetValue (app, KeyName, V);
    END LoadStringOfStrings;

(************************************************************************)

PROCEDURE ScanString (VAR (*OUT*) result: ARRAY OF CHAR;
                      VAR (*IN*) InputLine: ARRAY OF CHAR;
                      VAR (*INOUT*) pos: CARDINAL;  Stoppers: CharSet);

    (* Copies a string from InputLine, starting at InputLine[pos], and  *)
    (* stopping when we reach end-of-line or a character in Stoppers.   *)

    VAR j: CARDINAL;

    BEGIN
        Stoppers := Stoppers + CharSet{Nul, CtrlZ};
        j := 0;
        WHILE (pos <= HIGH(InputLine)) AND NOT (InputLine[pos] IN Stoppers) DO
            IF pos <= HIGH(result) THEN
                result[j] := InputLine[pos];
                INC(j);
            END (*IF*);
            INC(pos);
        END (*WHILE*);
        result[j] := Nul;
    END ScanString;

(************************************************************************)

PROCEDURE LoadStringItem (cid: ChanId;  app: AppPtr;
              VAR (*IN*) KeyName: ARRAY OF CHAR;
                VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Loads a string-valued item, with quote-mark delimiters.          *)
    (* Multiple substrings can be concatenated with the '+' operator,   *)
    (* to allow for (a) strings containing both kinds of string         *)
    (* delimiter and (b) strings that don't fit on one line.  If a      *)
    (* string is to be continued on a new line then the '+' must be the *)
    (* last thing on the line.                                          *)
    (* The returned value is the net increase in allocated storage; it  *)
    (* can be negative because of deallocation of an old value.         *)

    VAR partial: ARRAY [0..511] OF CHAR;
        bufpos: CARDINAL;
        V: StrPtr;
        Stoppers: CharSet;
        NulStr: ARRAY [0..0] OF CHAR;

    BEGIN
        NulStr[0] := Nul;
        bufpos := 0;
        V.start := NIL;  V.length := 0;
        LOOP
            SkipSpaces (line, bufpos);
            Stoppers := CharSet{};
            IF line[bufpos] = "'" THEN
                INCL (Stoppers, "'");  INC(bufpos);
            ELSIF line[bufpos] = '"' THEN
                INCL (Stoppers, '"');  INC(bufpos);
            ELSE
                INCL (Stoppers, ' ');
            END (*IF*);
            ScanString (partial, line, bufpos, Stoppers);
            AppendString (V, partial, FALSE);
            IF line[bufpos] IN Stoppers THEN
                INC (bufpos);
            END (*IF*);
            SkipSpaces (line, bufpos);
            IF line[bufpos] = '+' THEN
                INC (bufpos);
                SkipSpaces (line, bufpos);
                IF line[bufpos] = Nul THEN
                    GetNextLine (cid, line);
                    bufpos := 0;
                END (*IF*);
            ELSE
                EXIT (*LOOP*);
            END (*IF*);
        END (*LOOP*);

        (* Allow for a trailing Nul. *)

        IF line[bufpos] = '0' THEN
            AppendString (V, NulStr, TRUE);
        END (*IF*);

        SetValue (app, KeyName, V);

    END LoadStringItem;

(************************************************************************)

PROCEDURE LoadOneNumber (p: ByteArrayPtr;
                         VAR (*INOUT*) line: ARRAY OF CHAR;
                         VAR (*INOUT*) bufpos: CARDINAL;
                         base, start, length: CARDINAL;
                                 Digits: CharSet);

    (* Loads a decimal or hex number from line[bufpos], updating        *)
    (* bufpos.  The result is stored in 'length' bytes starting at      *)
    (* p^[start].                                                       *)

    VAR k, N, carry: CARDINAL;

    BEGIN
        IF length > 0 THEN
            FOR k := start TO start+length-1 DO
                p^[k] := 0;
            END (*FOR*);
        END (*IF*);
        WHILE (bufpos <= HIGH(line)) AND (line[bufpos] IN Digits) DO

            (* Multiply the existing value by base, and add in the new  *)
            (* digit.  Note that we are using little-endian storage.    *)

            IF line[bufpos] IN DecimalDigits THEN
                carry := ORD(line[bufpos]) - ORD('0');
            ELSE
                carry := ORD(CAP(line[bufpos])) - ORD('A') + 10;
            END (*IF*);
            INC (bufpos);
            FOR k := start TO start+length-1 DO
                N := base*p^[k] + carry;
                p^[k] := N MOD 256;
                carry := N DIV 256;
            END (*FOR*);

        END (*WHILE*);

    END LoadOneNumber;

(************************************************************************)

PROCEDURE Resize (VAR (*INOUT*) V: StrPtr;  newsize: CARDINAL;
                                     VAR (*OUT*) spare: CARDINAL);

    (* Changes the length of V to newsize.  Returns spare=0 if this     *)
    (* is an increased allocation, or no change, otherwise returns      *)
    (* spare as the increase in allocated space.                        *)

    VAR tocopy: CARDINAL;
        p: ADDRESS;

    BEGIN
        tocopy := V.length;
        IF tocopy > newsize THEN
            tocopy := newsize;
            spare := 0;
        ELSE
            spare := newsize - tocopy;
        END (*IF*);
        IF tocopy = 0 THEN
            p := NIL;
        ELSE
            ALLOCATE (p, newsize);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, newsize);
            <* END *>
            Copy (V.start, p, tocopy);
        END (*IF*);
        DEALLOCATE (V.start, V.length);
        <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
            TrackUpdate (TNITrack, -VAL(INTEGER,V.length));
        <* END *>
        V.start := p;
        V.length := newsize;
    END Resize;

(************************************************************************)

PROCEDURE LoadNumeric (cid: ChanId;  VAR (*INOUT*) line: ARRAY OF CHAR;
                                  length: CARDINAL; hex: BOOLEAN): StrPtr;

    (* On entry, length is the item length. *)

    CONST allocsize = 128;

    VAR base, len0, start, bufpos, spare: CARDINAL;
        Digits: CharSet;
        pval: StrPtr;

    BEGIN
        IF hex THEN
            base := 16;
            Digits := HexDigits;
        ELSE
            base := 10;
            Digits := DecimalDigits;
        END (*IF*);
        len0 := length;
        length := 0;
        start := 0;
        bufpos := 0;
        SkipSpaces (line, bufpos);
        IF line[bufpos] = '(' THEN
            REPEAT
                INC (bufpos);
            UNTIL line[bufpos] = ')';
            INC (bufpos);
        END (*IF*);
        SkipSpaces (line, bufpos);

        (* Begin by allocating enough space for one item.  As we run    *)
        (* out of space we'll expand the available space.               *)

        ALLOCATE (pval.start, len0);
        <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
            TrackUpdate (TNITrack, len0);
        <* END *>
        pval.length := len0;
        spare := len0;

        REPEAT
            IF spare < len0 THEN
                Resize (pval, pval.length + allocsize, spare);
            END (*IF*);
            LoadOneNumber (pval.start, line, bufpos, base, start, len0, Digits);
            INC (length, len0);
            INC (start, len0);
            DEC (spare, len0);
            SkipSpaces (line, bufpos);
            IF line[bufpos] = '+' THEN
                GetNextLine (cid, line);
                bufpos := 0;
                SkipSpaces (line, bufpos);
            END (*IF*);
        UNTIL (bufpos > HIGH(line)) OR NOT (line[bufpos] IN Digits);

        (* If we've over-allocated, reduce the allocated space. *)

        IF spare > 0 THEN
            Resize (pval, pval.length-spare, spare);
        END (*IF*);

        RETURN pval;

    END LoadNumeric;

(************************************************************************)

PROCEDURE IsNumericItem (VAR (*IN*) line: ARRAY OF CHAR;
                         VAR (*OUT*) length: CARDINAL;
                         VAR (*OUT*) hex: BOOLEAN): BOOLEAN;

    (* Scans the line to see whether it can represent a number or   *)
    (* sequence of numbers.  The line may start with '('length')'   *)
    (* or with "(X)", and it may end with '+' (all optional), but   *)
    (* apart from that it may have only digits and spaces.          *)

    VAR bufpos, k, L: CARDINAL;
        Digits: CharSet;

    BEGIN
        hex := FALSE;  length := 4;
        bufpos := 0;
        Digits := DecimalDigits;
        IF line[bufpos] = "(" THEN
            length := 0;  INC(bufpos);
            IF CAP(line[bufpos]) = 'X' THEN
                INC (bufpos);
                hex := TRUE;  length := 1;
                Digits := HexDigits;
            ELSE
                WHILE line[bufpos] IN DecimalDigits DO
                    length := 10*length + (ORD(line[bufpos]) - ORD('0'));
                    INC (bufpos);
                END (*WHILE*);
            END (*IF*);
            IF line[bufpos] <> ")" THEN
                RETURN FALSE;
            END (*IF*);
            INC (bufpos);
            SkipSpaces (line, bufpos);
        END (*IF*);

        (* Decide whether the value is numeric or a string. *)

        L := LENGTH (line);
        IF bufpos = L THEN

            (* Value is an empty string *)
            RETURN FALSE;

        ELSIF NOT (line[bufpos] IN Digits) THEN

            RETURN FALSE;

        ELSE
            (* Value is numeric iff all non-space characters are  *)
            (* digits, although we do allow one final '+'.        *)

            k := bufpos;
            LOOP
                IF k = L THEN RETURN TRUE
                ELSIF (line[k] = ' ') OR (line[k] IN Digits) THEN
                    INC (k);
                ELSIF line[k] = '+' THEN
                    RETURN (k+1 = L);
                ELSE
                    RETURN FALSE;
                END (*IF*);
            END (*LOOP*);
        END (*IF*);

    END IsNumericItem;

(************************************************************************)

PROCEDURE LoadKeyAndValue (cid: ChanId;  app: AppPtr;
                             VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Loads INI data from one line of the TNI file.           *)
    (* We assume that the caller has exclusive access to app.  *)
    (* Returned value is number of bytes allocated.            *)

    VAR keyname: ARRAY [0..127] OF CHAR;
        pos, length: CARDINAL;  found, hex: BOOLEAN;

    BEGIN
        (* We are expecting a 'key=value' line. *)

        Strings.FindNext ('=', line, 0, found, pos);
        IF found THEN
            Strings.Extract (line, 0, pos, keyname);
            Strings.Delete (line, 0, pos+1);
            IF IsNumericItem (line, length, hex) THEN

                SetValue (app, keyname,
                                   LoadNumeric (cid, line, length, hex));

            ELSE

                LoadStringItem (cid, app, keyname, line);

            END (*IF*);

        END (*IF*);

    END LoadKeyAndValue;

(************************************************************************)

PROCEDURE LoadApp (cid: ChanId; hini: THandle;
                        VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Loads INI data from its disk file into memory.  On entry we      *)
    (* have line[0] = '['.                                              *)
    (* We assume that the caller has exclusive access to hini.          *)

    VAR appptr: AppPtr;  pos: CARDINAL;  found: BOOLEAN;
        AppName: ARRAY [0..255] OF CHAR;

    BEGIN
        pos := Strings.Length(line) - 1;
        Strings.FindPrev (']', line, pos, found, pos);
        IF found AND (pos > 1) THEN
            Strings.Extract (line, 1, pos-1, AppName);
        ELSE
            AppName := "?";  pos := 2;
        END (*IF*);
        appptr := FindApp (hini, AppName, TRUE);
        GetNextLine (cid, line);
        WHILE (line[0] <> CtrlZ) AND ((line[0] <> '[') OR (line[1] <> '/')) DO
            IF line[0] = '[' THEN
                LoadStringOfStrings (cid, appptr, line);
            ELSE
                LoadKeyAndValue (cid, appptr, line);
            END (*IF*);
            GetNextLine (cid, line);
        END (*WHILE*);
    END LoadApp;

(************************************************************************)

PROCEDURE LoadData (cid: ChanId;  hini: THandle);

    (* Loads INI data from its disk file into memory.          *)
    (* We assume that the caller has exclusive access to hini. *)

    VAR line: ARRAY [0..511] OF CHAR;

    BEGIN
        GetNextLine (cid, line);
        WHILE line[0] = '[' DO
            LoadApp (cid, hini, line);
            IF line[0] <> CtrlZ THEN
                GetNextLine (cid, line);
            END (*WHILE*);
        END (*WHILE*);
    END LoadData;

(************************************************************************)
(*                          DEBUGGING STUFF                             *)
(************************************************************************)

(*
PROCEDURE WriteValue (cid: ChanId;  key: KeyPtr);  FORWARD;

(************************************************************************)

PROCEDURE DebugDump (app, key: ARRAY OF CHAR);

    (* Writes a TNI file entry to the debug file. *)

    VAR appptr: AppPtr;
        keyptr: KeyPtr;

    BEGIN
        IF app[0] = Nul THEN
            DebugLine ("Looking up list of all applications");
        ELSIF key[0] = Nul THEN
            DebugText ("Looking up all keys for ");
            DebugText (app);
            DebugLine ("");
        END (*IF*);

        appptr := FindApp (TNIChain, app, FALSE);
        IF appptr = NIL THEN
            DebugText ("Application ");
            DebugText (app);
            DebugLine (" not found");
        ELSE
            keyptr := FindKey (appptr, key, FALSE);
            IF keyptr = NIL THEN
                DebugLine ("Key not found");
            ELSE
                WriteValue (debugcid, keyptr);
            END (*IF*);
        END (*IF*);

    END DebugDump;
*)

(************************************************************************)
(*                            OPENING A FILE                            *)
(************************************************************************)

PROCEDURE OpenTNIFile (VAR (*IN*) fname: ARRAY OF CHAR): THandle;

    (* Opens an existing TNI file, returns its handle.  The result is   *)
    (* NIL if the file doesn't exist or can't be opened.                *)

    VAR TS: THandle;  newcid: ChanId;

    BEGIN
        Obtain (MasterLock);
        TS := IdentifyByFilename (fname);
        IF TS = NIL THEN
            newcid := OpenOldFile (fname, TRUE, FALSE);
            IF newcid = NoSuchChannel THEN
                Release (MasterLock);
            ELSE
                NEW (TS);
                <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                    TrackUpdate (TNITrack, SIZE(TNIStructure));
                <* END *>
                WITH TS^ DO
                    opencount := 1;
                    changed := FALSE;
                    next := TNIChain;
                    TNIChain := TS;
                    FirstApp := NIL;
                    Strings.Assign (fname, filename);
                    CreateLock (access);
                END (*WITH*);
                Obtain (TS^.access);
                Release (MasterLock);
                LoadData (newcid, TS);
                CloseFile (newcid);
                (*
                DebugText ("Loaded ");
                DebugText (fname);
                IF StringMatch(fname, "weasel.tni") THEN
                    DebugText (",");
                    DebugDump ("$SYS", "FilterProg4");
                ELSE
                    DebugLine ("");
                END (*IF*);
                *)
            END (*IF*);
        ELSE
            Obtain (TS^.access);
            INC (TS^.opencount);
            Release (MasterLock);
        END (*IF*);
        IF TS <> NIL THEN
            Release (TS^.access);
        END (*IF*);
        RETURN TS;
    END OpenTNIFile;

(************************************************************************)

PROCEDURE CreateTNIFile (VAR (*IN*) fname: ARRAY OF CHAR): THandle;

    (* Like OpenTNIFile, but creates an initially empty new file.  We   *)
    (* assume that the caller has already verified that the file does   *)
    (* not already exist.                                               *)

    VAR TS: THandle;  newcid: ChanId;

    BEGIN
        TS := NIL;
        newcid := OpenNewFile (fname, FALSE);
        IF newcid <> NoSuchChannel THEN
            NEW (TS);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, SIZE(TNIStructure));
            <* END *>
            WITH TS^ DO
                opencount := 1;
                changed := FALSE;
                Obtain (MasterLock);
                next := TNIChain;
                TNIChain := TS;
                Release (MasterLock);
                FirstApp := NIL;
                Strings.Assign (fname, filename);
                CreateLock (access);
            END (*WITH*);
            CloseFile (newcid);
        END (*IF*);
        RETURN TS;
    END CreateTNIFile;

(************************************************************************)
(************************************************************************)
(***                                                                  ***)
(***           READING THE INFORMATION IN THE FORMAT FILE             ***)
(***                                                                  ***)
(************************************************************************)
(************************************************************************)

PROCEDURE InterpretFormatCode (app: AppPtr;  VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Processes the format code in one line of the FMT file.           *)
    (* We assume that the caller has exclusive access to app.           *)

    (********************************************************************)

    PROCEDURE ProcessCode (key: KeyPtr);

        (* On entry, line should contain one of the codes:              *)
        (*       *          obsolete item                               *)
        (*       '          string                                      *)
        (*       '0         nul-terminated string                       *)
        (*       "          string of strings                           *)
        (*       (N)        numeric                                     *)
        (* In the numeric case, N can be a decimal length or the char X.*)
        (* We also accept (), which implies the default N=4.            *)

        VAR length, bufpos: CARDINAL;

        BEGIN
            IF line[0] = "*" THEN
                key^.kind := ObsoleteItem;
                key^.ComponentSize := 0;
            ELSIF line[0] = "'" THEN
                IF line[1] = '0' THEN
                    key^.kind := String0Item;
                ELSE
                    key^.kind := StringItem;
                END (*IF*);
                key^.ComponentSize := 1;
            ELSIF line[0] = '"' THEN
                key^.kind := StringOfStrings;
                key^.ComponentSize := 0;
            ELSIF line[0] = "(" THEN
                IF line[1] = ')' THEN
                    key^.kind := NumericItem;
                    key^.ComponentSize := 4;
                ELSIF CAP(line[1]) = 'X' THEN
                    key^.kind := HexItem;
                    key^.ComponentSize := 1;
                ELSE
                    length := 0;  bufpos := 1;
                    WHILE line[bufpos] IN DecimalDigits DO
                        length := 10*length + (ORD(line[bufpos]) - ORD('0'));
                        INC (bufpos);
                    END (*WHILE*);
                    key^.kind := NumericItem;
                    key^.ComponentSize := length;
                END (*IF*);
            END (*IF*);

        END ProcessCode;

    (****************************************************************)

    VAR keyname: ARRAY [0..127] OF CHAR;
        key: KeyPtr;
        pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        (* We are expecting a key=code line, using one of the codes:   *)
        (*       '          string                                     *)
        (*       "          string of strings                          *)
        (*       (N)        numeric                                    *)
        (* Note: if app = NIL then we are going to ignore the line.    *)
        (* Similarly, missing keys will be ignored.  But we still have *)
        (* to parse the lines, if only to skip past them.              *)

        Strings.FindNext ('=', line, 0, found, pos);
        IF found THEN
            Strings.Extract (line, 0, pos, keyname);
            Strings.Delete (line, 0, pos+1);
            IF app <> NIL THEN
                key := FindKey (app, keyname, FALSE);
                IF key <> NIL THEN
                    ProcessCode (key);
                END (*IF*);
            END (*IF*);
        END (*IF*);

    END InterpretFormatCode;

(************************************************************************)

PROCEDURE LoadAppGroupFormats (cid: ChanId;  hini: THandle;
                                    VAR (*INOUT*) line: ARRAY OF CHAR);

    (* Loads format data from a disk file into memory.  On entry we     *)
    (* have line[0] = '['.                                              *)
    (* We assume that the caller has exclusive access to hini.          *)

    VAR appptr: AppPtr;  pos: CARDINAL;  found: BOOLEAN;
        AppName: ARRAY [0..255] OF CHAR;

    BEGIN
        pos := Strings.Length(line) - 1;
        Strings.FindPrev (']', line, pos, found, pos);
        IF found AND (pos > 1) THEN
            Strings.Extract (line, 1, pos-1, AppName);
        ELSE
            AppName := "?";  pos := 2;
        END (*IF*);
        appptr := FindApp (hini, AppName, FALSE);
        GetNextLine (cid, line);
        WHILE (line[0] <> CtrlZ) AND (line[0] <> '[') DO
            InterpretFormatCode (appptr, line);
            GetNextLine (cid, line);
        END (*WHILE*);
    END LoadAppGroupFormats;

(************************************************************************)

PROCEDURE ReadFormatFile (hini: THandle);

    (* Reads the applicable *.FMT file, to work out how items should    *)
    (* be written to the TNI file.                                      *)

    VAR cid: ChanId;
        pos: CARDINAL;  found: BOOLEAN;
        fmtname: FilenameString;
        line: ARRAY [0..511] OF CHAR;

    BEGIN
        fmtname := hini^.filename;
        Strings.FindPrev (".", fmtname, LENGTH(fmtname), found, pos);
        IF found THEN
            fmtname[pos] := Nul;
        END (*IF*);
        Strings.Append (".FMT", fmtname);
        cid := OpenOldFile (fmtname, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            GetNextLine (cid, line);
            WHILE line[0] = '[' DO
                LoadAppGroupFormats (cid, hini, line);
                IF line[0] <> CtrlZ THEN
                    GetNextLine (cid, line);
                END (*WHILE*);
            END (*WHILE*);
            CloseFile (cid);
        END (*IF*);
    END ReadFormatFile;

(************************************************************************)
(************************************************************************)
(***                                                                  ***)
(***         STORING THE DATA FROM MAIN MEMORY TO A DISK FILE         ***)
(***                                                                  ***)
(************************************************************************)
(************************************************************************)

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                NextName[N] := '0';
                IF N > 0 THEN
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (NextNameLock);
        Strings.Assign (NextName, name);
        Increment (7);
        Release (NextNameLock);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (VAR (*IN*) BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR UName: FilenameString;

    BEGIN
        REPEAT
            MakeUniqueName (UName);
            Strings.Assign (BaseName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(************************************************************************)

PROCEDURE OpenNewOutputFile (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString): ChanId;

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist, and *)
    (* opens that file.                                                 *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR cid: ChanId;  duplication: BOOLEAN;

    BEGIN
        REPEAT
            MakeNewFilename (BaseName, tail, NewName);
            cid := OpenNewFile1 (NewName, duplication);
        UNTIL NOT duplication;
        RETURN cid;
    END OpenNewOutputFile;

(************************************************************************)
(*       CLASSIFYING AN ITEM IN ORDER TO KNOW HOW TO WRITE IT OUT       *)
(************************************************************************)

PROCEDURE IsAString (V: StrPtr; VAR (*OUT*) NulTerminated: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff V can plausibly be a printable string.          *)
    (* Special cases:                                                   *)
    (*    Empty string is OK.                                           *)
    (*    Null-terminated string OK                                     *)
    (*    String starting with decimal digit is OK, but will need to be *)
    (*       quoted when written to avoid confusion with number.        *)

    VAR q: BytePtr;
        N: CARDINAL;
        OK: BOOLEAN;

    BEGIN
        q := V.start;
        N := V.length;
        OK := TRUE;
        NulTerminated := FALSE;
        (* N = number of characters still not examined. *)
        LOOP
            IF N = 0 THEN
                EXIT (*LOOP*);
            ELSIF q^ = 0 THEN
                NulTerminated := N=1;
                OK := NulTerminated;
                EXIT (*LOOP*);
            ELSIF (q^ < 32) OR (q^ > 126) THEN
                OK := FALSE;
                EXIT (*LOOP*);
            ELSE
                q := AddOffset (q, 1);
                DEC (N);
            END (*IF*)
        END (*LOOP*);
        RETURN OK;
    END IsAString;

(************************************************************************)

PROCEDURE IsAStringOfStrings (V: StrPtr): BOOLEAN;

    (* Returns TRUE iff V has the format of a sequence of printable     *)
    (* non-empty strings, each terminated with a Nul byte, with exactly *)
    (* one extra Nul byte at the end.                                   *)

    VAR q: BytePtr;
        L, N: CARDINAL;

    BEGIN
        q := V.start;
        N := V.length;

        (* The outer loop is executed once per string. *)

        LOOP
            L := 0;
            LOOP
                IF N = 0 THEN
                    (* No terminating Nul. *)
                    RETURN FALSE;
                END (*IF*);
                IF (q^ < 32) OR (q^ > 126) THEN
                    EXIT (*LOOP*);
                ELSE
                    q := AddOffset (q, 1);
                    DEC (N);
                    INC (L);
                END (*IF*)
            END (*LOOP*);

            (* End of a string; it should have been a non-empty *)
            (* string, and we should now see a Nul.             *)

            IF (L = 0) OR (q^ <> 0) THEN
                RETURN FALSE;
            END (*IF*);
            q := AddOffset (q, 1);
            DEC (N);

            (* Check for final Nul at end of all strings. *)

            IF N = 1 THEN
                RETURN q^ = 0;
            END (*IF*);

        END (*LOOP*);

    END IsAStringOfStrings;

(************************************************************************)

PROCEDURE Classify (key: KeyPtr;  VAR (*OUT*) itemlength: CARDINAL): ItemKind;

    (* Works out what format we should use when writing key^.val to the *)
    (* TNI file.  Also returns the size in bytes of one item.  (But     *)
    (* note that we might end up writing out an array of such items.)   *)
    (* The item length is meaningful only for numeric items.            *)

    VAR V: StrPtr;  result: ItemKind;  NulTerminated: BOOLEAN;

    BEGIN
        result := key^.kind;
        itemlength := key^.ComponentSize;

        IF result = HexItem THEN

            (* HexItem is a catch-all case when we can't find a better  *)
            (* classification; so if this is what is recorded, we might *)
            (* be able to do better by examining the value.             *)

            V := key^.val;
            IF V.length = 0 THEN
                result := StringItem;
            ELSIF V.length <= 4 THEN
                result := NumericItem;
                itemlength := V.length;
            ELSIF IsAString(V, NulTerminated) THEN
                IF NulTerminated THEN
                    result := String0Item;
                ELSE
                    result := StringItem;
                END (*IF*);
            ELSIF IsAStringOfStrings(V) THEN
                result := StringOfStrings;
            END (*IF*);
        END (*IF*);

        RETURN result;

    END Classify;

(************************************************************************)
(*             WRITING A CHARACTER STRING TO THE TNI FILE               *)
(************************************************************************)

PROCEDURE ExtractLeadingPart (VAR (*INOUT*) str: ARRAY OF CHAR;
                              VAR (*INOUT*) length: CARDINAL;
                              amount: CARDINAL;
                              VAR (*OUT*) leading: ARRAY OF CHAR);

    (* Moves the first "amount" characters of str to leading, deleting  *)
    (* those characters from str.  Special note: str might not be       *)
    (* Nul-terminated, so we can't afford to pass it as a value         *)
    (* parameter.  Also for that reason, we can't do the extraction     *)
    (* with calls to the Strings module.                                *)

    VAR j: CARDINAL;

    BEGIN
        IF amount = 0 THEN
            leading[0] := Nul;
        ELSE
            Copy (ADR(str), ADR(leading), amount);
            IF amount <= HIGH(leading) THEN
                leading[amount] := Nul;
            END (*IF*);
            DEC (length, amount);
            FOR j := 0 TO length-1 DO
                str[j] := str[j+amount];
            END (*FOR*);
            str[length] := Nul;
        END (*IF*);
    END ExtractLeadingPart;

(************************************************************************)

PROCEDURE PutString (cid: ChanId;  VAR (*IN*) str: ARRAY OF CHAR;
                          length: CARDINAL;
                               ForceQuote, AddPlus, FirstPart: BOOLEAN);

    (* Writes a string to the output file.  Special handling is needed  *)
    (* if the string overflows one line, or if it starts with the '('   *)
    (* character or with a decimal digit, or if it contains one or both *)
    (* quotation characters, or if it contains a space character.  If   *)
    (* AddPlus is TRUE then we append a '+' character, which implies    *)
    (* that we must also add quotation marks.  Quotation marks are also *)
    (* added if needed for the "special handling" mentioned above.      *)

    (* The ForceQuote parameter forces a quoted string even if it       *)
    (* would not otherwise need to be quoted.  For a string that has to *)
    (* be broken up into substrings, including the case of breaking     *)
    (* it up over more than one line, ForceQuote affects only the last  *)
    (* substring; but lines other than the last will be quoted anyway   *)
    (* to separate the string from the following + continuation mark.   *)

    (* I believe that length>0 always on entry to this procedure, but   *)
    (* for safety's sake I've allowed for empty strings.                *)

    CONST SizePerLine = 60;

    VAR SingleQuotePos, DoubleQuotePos, SpacePos, amount: CARDINAL;
        quotechar: CHAR;
        HasSingleQuote, HasDoubleQuote, HasSpace, finished: BOOLEAN;
        partial: ARRAY [0..SizePerLine] OF CHAR;

    BEGIN
        finished := FALSE;
        quotechar := "'";

        (* Repeatedly process leading substrings until we   *)
        (* are certain that str will fit on one line.       *)

        WHILE length > SizePerLine DO
            ExtractLeadingPart (str, length, SizePerLine, partial);
            PutString (cid, partial, LENGTH(partial), FALSE, TRUE, FirstPart);
            FirstPart := FALSE;
            FWriteLn (cid);
            FWriteString (cid, "          ");
        END (*WHILE*);

        (* On the first entry we had length > 0, and in that case we    *)
        (* still have length > 0 after the above loop.  However, I have *)
        (* not yet confirmed that length > 0 for recursive entries.     *)

        (* Work out whether the string contains either or both  *)
        (* kinds of quotation mark.                             *)

        Strings.FindNext ("'", str, 0, HasSingleQuote, SingleQuotePos);
        Strings.FindNext ('"', str, 0, HasDoubleQuote, DoubleQuotePos);

        (* Important check: since the string is not Nul-terminated, it  *)
        (* is possible for the quotation mark to be a spurious one that *)
        (* is not inside the string.                                    *)

        HasSingleQuote := HasSingleQuote AND (SingleQuotePos < length);
        HasDoubleQuote := HasDoubleQuote AND (DoubleQuotePos < length);

        IF HasSingleQuote THEN

            IF HasDoubleQuote THEN

                (* The tricky case: a string that contains both kinds   *)
                (* of quote character.  That means that we have to      *)
                (* break it up.  We define the leading part to be the   *)
                (* longest leading substring that contains one kind of  *)
                (* quotation but stops just short of including the      *)
                (* other.  Note that this implies that neither the      *)
                (* leading part nor what is left can be the empty       *)
                (* string.                                              *)

                amount := SingleQuotePos;
                IF DoubleQuotePos > amount THEN
                    amount := DoubleQuotePos;
                END (*IF*);
                ExtractLeadingPart (str, length, amount, partial);
                PutString (cid, partial, LENGTH(partial), FALSE, TRUE, FirstPart);
                FWriteChar (cid, ' ');
                PutString (cid, str, length, ForceQuote, FALSE, FALSE);
                finished := TRUE;

            ELSE
                quotechar := '"';

                (* We haven't yet processed this case, but we'll get    *)
                (* back to it because finished = FALSE.                 *)

            END (*IF*);

        ELSIF HasDoubleQuote OR AddPlus
               OR (length = 0)
                OR (FirstPart AND ((str[0] = '(')
                                   OR ((str[0] >= '0')
                                         AND (str[0] <= '9')))) THEN

            (* Having to quote when the string contains one or more     *)
            (* double quote characters is an obvious case.  (We have    *)
            (* already handled the case, above, where it contains one   *)
            (* or more single quote characters.)  The remaining tests   *)
            (* in the above "IF" are for some special cases: an empty   *)
            (* string, or one that starts with a left parenthesis or    *)
            (* a digit.                                                 *)

            quotechar := "'";

        ELSE
            (* One final special case: does the string contain any      *)
            (* space characters?                                        *)

            Strings.FindNext (' ', str, 0, HasSpace, SpacePos);
            HasSpace := HasSpace AND (SpacePos < length);
            IF HasSpace THEN
                quotechar := "'";
            ELSE
                quotechar := Nul;
            END (*IF*);
        END (*IF*);

        IF (quotechar = Nul) AND ForceQuote THEN
            quotechar := "'";
        END (*IF*);

        (* Finally, write the string, surrounded by quotes and/or  *)
        (* followed by '+' as decided above.                       *)

        IF NOT finished THEN
            IF quotechar <> Nul THEN
                FWriteChar (cid, quotechar);
            END (*IF*);
            IF length > 0 THEN
                WriteRaw (cid, str, length);
            END (*IF*);
            IF quotechar <> Nul THEN
                FWriteChar (cid, quotechar);
            END (*IF*);
            IF AddPlus THEN
                FWriteString (cid, " +");
            END (*IF*);
        END (*IF*);

    END PutString;

(************************************************************************)

PROCEDURE WriteStringItem (cid: ChanId;  V: StrPtr;  AddNulMarker: BOOLEAN);

    (* Writes a string to the output file.  Special handling is needed  *)
    (* if the string overflows one line, or if it starts with the '('   *)
    (* character, or if it contains one or both quotation characters,   *)
    (* or if it contains a space character.                             *)

    VAR p: CharPtr;  length: CARDINAL;

    BEGIN
        p := V.start;
        length := V.length;

        IF (p = NIL) OR (length = 0) THEN
            IF AddNulMarker THEN
                FWriteString (cid, "''0");
            END (*IF*);
        ELSE
            PutString (cid, p^, length, AddNulMarker, FALSE, TRUE);
            IF AddNulMarker THEN
                FWriteChar (cid, '0');
            END (*IF*);
        END (*IF*);
    END WriteStringItem;

(************************************************************************)

PROCEDURE WriteStr (cid: ChanId;  str: StrPtr);

    (* Writes a StrPtr string to the file.     *)

    BEGIN
        WriteRaw (cid, str.start^, str.length);
    END WriteStr;

(************************************************************************)

PROCEDURE SectionLabel (cid: ChanId;  V: StrPtr;  atend: BOOLEAN);

    (* Writes a bracketed section label. *)

    BEGIN
        FWriteChar (cid, '[');
        IF atend THEN
            FWriteChar (cid, '/');
        END (*IF*);
        WriteStr (cid, V);
        FWriteChar (cid, ']');
        FWriteLn (cid);
    END SectionLabel;

(************************************************************************)

PROCEDURE WriteStringOfStrings (cid: ChanId;  key: KeyPtr);

    (* Writes a string list to the output file.  *)

    VAR p: POINTER TO CHAR;
        N: CARDINAL;

    BEGIN
        FWriteString (cid, "   ");  SectionLabel (cid, key^.name, FALSE);
        p := key^.val.start;  N := key^.val.length;
        WHILE (N > 0) AND (p^ <> Nul) DO
            FWriteString (cid, "      ");
            WHILE (N > 0) AND (p^ <> Nul) DO
                FWriteChar (cid, p^);
                p := AddOffset (p, 1);
                DEC (N);
            END (*WHILE*);
            FWriteLn (cid);
            IF N > 0 THEN
                p := AddOffset (p, 1);
                DEC (N);
            END (*IF*);
        END (*WHILE*);
        FWriteString (cid, "   ");  SectionLabel (cid, key^.name, TRUE);
    END WriteStringOfStrings;

(************************************************************************)
(*                        OUTPUT OF NUMERIC ITEMS                       *)
(************************************************************************)

PROCEDURE WriteHexNybble (cid: ChanId;  val: CARD8);

    (* Writes val as a hexadecimal one-character string. *)

    BEGIN
        IF val < 10 THEN
            FWriteChar (cid, CHR(val + ORD('0')));
        ELSE
            FWriteChar (cid, CHR(val - 10 + ORD('A')));
        END (*IF*);
    END WriteHexNybble;

(************************************************************************)

PROCEDURE WriteHexByte (cid: ChanId;  val: CARD8);

    (* Writes val as a hexadecimal two-character string. *)

    BEGIN
        WriteHexNybble (cid, val DIV 16);
        WriteHexNybble (cid, val MOD 16);
    END WriteHexByte;

(************************************************************************)

PROCEDURE WriteAsHex (cid: ChanId;  V: StrPtr;  charcount: CARDINAL);

    (* Writes V as a sequence of hexadecimal byte values. *)

    CONST linesize = 80;

    VAR q: BytePtr;
        j: CARDINAL;

    BEGIN
        FWriteString (cid, "(X)");
        INC (charcount, 3);
        q := V.start;
        FOR j := 1 TO V.length DO
            IF charcount >= linesize THEN
                FWriteString (cid, " +");
                FWriteLn (cid);
                FWriteString (cid, "    ");
                charcount := 4;
            END (*IF*);
            FWriteChar (cid, " ");
            WriteHexByte (cid, q^);
            INC (charcount, 3);
            q := AddOffset (q, 1);
        END (*FOR*);
    END WriteAsHex;

(************************************************************************)

PROCEDURE WriteSmallDecimal (cid: ChanId;  val: CARDINAL): CARDINAL;

    (* Writes a decimal value to disk.  The returned value is the     *)
    (* number of characters written.                                  *)

    VAR charcount: CARDINAL;

    BEGIN
        charcount := 0;
        IF val > 9 THEN
            charcount := WriteSmallDecimal (cid, val DIV 10);
            val := val MOD 10;
        END (*IF*);
        FWriteChar (cid, CHR(ORD('0') + val));
        INC (charcount);
        RETURN charcount;
    END WriteSmallDecimal;

(************************************************************************)

PROCEDURE FWriteNumber (cid: ChanId;  VAR (*IN*) number: ARRAY OF CARD8;
                                                size: CARDINAL): CARDINAL;

    (* Writes a number of "size" bytes to a file.  This is a      *)
    (* destructive operation; it changes the "number" parameter.  *)
    (* The returned value is the number of characters written.    *)

    VAR k, carry, N, charcount: CARDINAL;
        allzero: BOOLEAN;

    BEGIN
        charcount := 0;

        (* Divide number by 10.  We are using little-endian storage,    *)
        (* so we have to work backwards through the array.              *)

        allzero := TRUE;  carry := 0;
        FOR k := size-1 TO 0 BY -1 DO
            (* carry can be 0..9 *)
            (* number[k] can be 0..255 *)
            N := 256*carry + number[k];
            (* N can be 0..2559 *)
            carry := N MOD 10;
            N := N DIV 10;
            (* N can be 0..255 *)
            allzero := allzero AND (N = 0);
            number[k] := N;
        END (*FOR*);

        (* Recursive call if the quotient is nonzero. *)

        IF NOT allzero THEN
            charcount := FWriteNumber (cid, number, size);
        END (*IF*);

        (* Now write the final digit. *)

        FWriteChar (cid, CHR(ORD('0') + carry));
        INC (charcount);
        RETURN charcount;

    END FWriteNumber;

(************************************************************************)

PROCEDURE WriteDecimalItems (cid: ChanId;  V: StrPtr;
                                  itemlength, charcount: CARDINAL);

    (* Writes V as a sequence of one or more decimal values.     *)
    (* The value string might be changed as a side-effect.       *)

    CONST linesize = 80;

    VAR N: CARDINAL;
        p: ByteArrayPtr;

    BEGIN
        IF itemlength <> 4 THEN
            FWriteChar (cid, '(');
            charcount := WriteSmallDecimal(cid, itemlength) + 2;
            FWriteChar (cid, ')');
        END (*IF*);
        p := V.start;
        N := V.length;
        WHILE N > 0 DO
            IF charcount >= linesize THEN
                FWriteString (cid, " +");
                FWriteLn (cid);
                FWriteString (cid, "    ");
                charcount := 4;
            END (*IF*);
            IF N < itemlength THEN
                N := itemlength;
            END (*IF*);
            INC (charcount, FWriteNumber (cid, p^, itemlength));
            IF N > itemlength THEN
                p := AddOffset (p, itemlength);
                DEC (N, itemlength);
                FWriteChar (cid, ' ');
            ELSE
                N := 0;
            END (*IF*);
        END (*WHILE*);
    END WriteDecimalItems;

(************************************************************************)
(*                    STORING THE DATA TO A DISK FILE                   *)
(************************************************************************)

PROCEDURE CopyOf (V: StrPtr): StrPtr;

    (* Returns a copy of V.  *)

    VAR result: StrPtr;
        N: CARDINAL;

    BEGIN
        N := V.length;
        result.length := N;
        IF N > 0 THEN
            ALLOCATE (result.start, N);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, N);
            <* END *>
            Copy (V.start, result.start, N);
        ELSE
            result.start := NIL;
        END (*IF*);
        RETURN result;
    END CopyOf;

(************************************************************************)

PROCEDURE DiscardStr (VAR (*INOUT*) name: StrPtr);

    (* Discards a variable-length string.                               *)
    (* We assume that the caller has exclusive access to the string.    *)

    BEGIN
        IF name.length > 0 THEN
            DEALLOCATE (name.start, name.length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -VAL(INTEGER,name.length));
            <* END *>
            name.length := 0;
            name.start := NIL;
        END (*IF*);
    END DiscardStr;

(************************************************************************)

PROCEDURE PruneString (VAR (*INOUT*) V: StrPtr);

    (* Removes the terminating Nul, and anything that follows, from a   *)
    (* character string.                                                *)

    VAR p, q: CharPtr;  length, newlength: CARDINAL;

    BEGIN
        p := V.start;
        length := V.length;
        newlength := 0;

        (* Scan the string to find the true length, not counting the    *)
        (* terminating Nul or what follows.  Note that we can't use     *)
        (* Strings.Length for this, because there might not be a        *)
        (* terminating Nul.                                             *)

        WHILE (newlength < length) AND (p^[newlength] <> Nul) DO
            INC (newlength);
        END (*WHILE*);

        IF newlength < length THEN
            IF newlength = 0 THEN
                q := NIL;
            ELSE
                ALLOCATE (q, newlength);
                <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                    TrackUpdate (TNITrack, newlength);
                <* END *>
                Copy (p, q, newlength);
            END (*IF*);
            DEALLOCATE (p, length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, length);
            <* END *>
            V.start := q;
            V.length := newlength;

        END (*IF*);

    END PruneString;

(************************************************************************)

PROCEDURE WriteValue (cid: ChanId;  key: KeyPtr);

    (* Writes one key value to disk.                          *)
    (* We assume that the caller has exclusive access to key. *)

    VAR V: StrPtr;  itemlength, leadingchars: CARDINAL;
        kind: ItemKind;

    BEGIN
        kind := Classify (key, itemlength);
        IF kind = ObsoleteItem THEN
            (* Don't write it to the output file *)
        ELSIF kind = StringOfStrings THEN
            WriteStringOfStrings (cid, key);
        ELSE
            FWriteString (cid, "   ");
            WriteStr (cid, key^.name);
            FWriteChar (cid, '=');
            leadingchars := key^.name.length + 4;
            V := CopyOf (key^.val);
            CASE kind OF
                |  HexItem:
                        WriteAsHex (cid, V, leadingchars);
                |  NumericItem:
                        WriteDecimalItems (cid, V, itemlength,
                                                 leadingchars);
                |  StringItem:
                        PruneString (V);
                        WriteStringItem (cid, V, FALSE);
                |  String0Item:
                        PruneString (V);
                        WriteStringItem (cid, V, TRUE);
                |  ELSE
                        (* Nothing else to cover *)
            END (*CASE*);
            FWriteLn (cid);
            DiscardStr (V);
        END (*IF*);
    END WriteValue;

(************************************************************************)

PROCEDURE WriteApp (cid: ChanId;  p: AppPtr);

    (* Writes one application to disk.                      *)
    (* We assume that the caller has exclusive access to p. *)

    VAR k: KeyPtr;

    BEGIN
        SectionLabel (cid, p^.name, FALSE);
        k := p^.FirstKey;
        WHILE k <> NIL DO
            WriteValue (cid, k);
            k := k^.next;
        END (*WHILE*);
        SectionLabel (cid, p^.name, TRUE);
    END WriteApp;

(************************************************************************)

PROCEDURE StoreData (hini: THandle);

    (* Stores all of the INI data to disk, without deleting it from     *)
    (* memory.  We assume that the caller has exclusive access to hini. *)

    VAR cid: ChanId;  p: AppPtr;
        NewName: FilenameString;
        strTMP: ARRAY [0..4] OF CHAR;

    BEGIN
        (*
        STextIO.WriteString ("Entering StoreData");
        STextIO.WriteLn;
        DumpCountOfApps (hini);
        *)
        (*
        DebugText ("Storing ");
        DebugText (hini^.filename);
        IF StringMatch(hini^.filename, "weasel.tni") THEN
            DebugText (",");
            DebugDump ("$SYS", "FilterProg4");
        ELSE
            DebugLine ("");
        END (*IF*);
        *)

        NewName := hini^.filename;
        NewName[LENGTH(NewName)-4] := Nul;
        strTMP := ".TMP";
        cid := OpenNewOutputFile (NewName, strTMP, NewName);
        p := hini^.FirstApp;
        WHILE p <> NIL DO
            WriteApp (cid, p);
            p := p^.next;
        END (*WHILE*);
        CloseFile (cid);
        DeleteFile (hini^.filename);
        EVAL(MoveFile (NewName, hini^.filename));
    END StoreData;

(************************************************************************)
(*                         DISCARDING TNI DATA                          *)
(************************************************************************)

PROCEDURE DisposeOfKey (VAR (*INOUT*) key: KeyPtr);

    (* Deletes a key, which we assume has already been       *)
    (* detached from the INI structure.                      *)
    (* We assume that the caller has exclusive access to it. *)

    BEGIN
        IF key <> NIL THEN
            DiscardStr (key^.val);
            DiscardStr (key^.name);
            DEALLOCATE (key, SIZE(KeyRecord));
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -SIZE(KeyRecord));
            <* END *>
        END (*IF*);
    END DisposeOfKey;

(************************************************************************)

PROCEDURE DisposeOfApp (VAR (*INOUT*) app: AppPtr);

    (* Deletes an application, which we assume has already been *)
    (* detached from the INI structure.                         *)
    (* We assume that the caller has exclusive access to it.    *)

    VAR this: KeyPtr;

    BEGIN
        IF app <> NIL THEN
            WHILE app^.FirstKey <> NIL DO
                this := app^.FirstKey;
                app^.FirstKey := this^.next;
                DisposeOfKey (this);
            END (*IF*);
            DiscardStr (app^.name);
            DEALLOCATE (app, SIZE(AppRecord));
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -SIZE(AppRecord));
            <* END *>
        END (*IF*);
    END DisposeOfApp;

(************************************************************************)

PROCEDURE DiscardTNIStructure (VAR (*INOUT*) hini: THandle);

    (* Discards the entire structure.                           *)
    (* We assume that the caller has exclusive access to hini.  *)

    VAR p: AppPtr;

    BEGIN
        p := hini^.FirstApp;
        WHILE p <> NIL DO
            hini^.FirstApp := p^.next;
            DisposeOfApp(p);
            p := hini^.FirstApp;
        END (*WHILE*);
        Release (hini^.access);
        DestroyLock (hini^.access);
        DEALLOCATE (hini, SIZE(TNIStructure));
        <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
            TrackUpdate (TNITrack, -SIZE(TNIStructure));
        <* END *>
    END DiscardTNIStructure;

(************************************************************************)
(*       CLOSING A TNI STRUCTURE AND WRITING CHANGES BACK TO DISK       *)
(************************************************************************)

PROCEDURE CloseTNIFile (VAR (*INOUT*) hini: THandle);

    (* Closes a TNI file. *)

    VAR prev, TS: THandle;

    BEGIN
        Obtain (MasterLock);
        IF hini <> NIL THEN
            Obtain (hini^.access);
            DEC (hini^.opencount);
            IF hini^.opencount > 0 THEN
                Release (hini^.access);
            ELSE
                IF hini^.changed THEN
                    ReadFormatFile (hini);
                    StoreData (hini);
                END (*IF*);

                (* Remove this structure from the master list. *)

                prev := NIL;  TS := TNIChain;
                WHILE (TS <> NIL) AND (TS <> hini) DO
                    prev := TS;  TS := TS^.next;
                END (*WHILE*);
                IF TS <> NIL THEN
                    TS := TS^.next;
                    IF prev = NIL THEN
                        TNIChain := TS;
                    ELSE
                        prev^.next := TS;
                    END (*IF*);
                END (*IF*);

                (* Reclaim the memory. *)

                DiscardTNIStructure (hini);

            END (*IF*);

        END (*IF*);
        Release (MasterLock);

    END CloseTNIFile;

(************************************************************************)
(************************************************************************)
(***                                                                  ***)
(***          MANIPULATING THE DATA ONCE IT IS IN MAIN MEMORY         ***)
(***                                                                  ***)
(************************************************************************)
(************************************************************************)

(************************************************************************)
(*                  SIZE OF AN ITEM OR A LIST OF NAMES                  *)
(************************************************************************)

PROCEDURE ItemLength0 (TS: THandle;  VAR (*OUT*) length: CARDINAL): BOOLEAN;

    (* Sets length equal to the sum of the lengths of all application   *)
    (* names, where the result includes a trailing Nul for each name    *)
    (* and one further final Nul.  Returns FALSE if TS = NIL.           *)
    (* We assume that the caller has exclusive access to TS.            *)

    VAR p: AppPtr;
        success: BOOLEAN;

    BEGIN
        length := 0;
        success := TS <> NIL;
        IF success THEN
            p := TS^.FirstApp;
            WHILE p <> NIL DO
                INC (length, p^.name.length+1);
                p := p^.next;
            END (*WHILE*);
            INC (length);
        END (*IF*);
        RETURN success;
    END ItemLength0;

(************************************************************************)

PROCEDURE ItemLength1 (app: AppPtr;  VAR (*OUT*) length: CARDINAL): BOOLEAN;

    (* Sets length to the sum of the lengths of all key names for this  *)
    (* application, where the result includes a trailing Nul for each   *)
    (* name and one further final Nul.  Returns FALSE iff app = NIL.    *)
    (* We assume that the caller has exclusive access to app.           *)

    VAR p: KeyPtr;
        success: BOOLEAN;

    BEGIN
        length := 0;
        success := app <> NIL;
        IF success THEN
            p := app^.FirstKey;
            WHILE p <> NIL DO
                INC (length, p^.name.length+1);
                p := p^.next;
            END (*WHILE*);
            INC (length);
        END (*IF*);
        RETURN success;
    END ItemLength1;

(************************************************************************)

PROCEDURE ItemLength (TS: THandle;
                           VAR (*IN*) application, key: ARRAY OF CHAR;
                           VAR (*OUT*) length: CARDINAL): BOOLEAN;

    (* Sets length to the size of the specified entry, or returns       *)
    (* FALSE if there is no such entry.                                 *)
    (* We assume that the caller has exclusive access to TS.            *)

    VAR appres: AppPtr;
        keyres: KeyPtr;
        success: BOOLEAN;

    BEGIN
        length := 0;
        success := FALSE;
        IF application[0] = Nul THEN
            success := ItemLength0 (TS, length);
        ELSE
            appres := FindApp (TS, application, FALSE);
            IF appres <> NIL THEN
                IF key[0] = Nul THEN
                    success := ItemLength1 (appres, length);
                ELSE
                    keyres := FindKey (appres, key, FALSE);
                    IF keyres <> NIL THEN
                        length := keyres^.val.length;
                        success := TRUE;
                    END (*IF*);
                END (*IF*);
            END (*IF*);
        END (*IF*);
        RETURN success;
    END ItemLength;

(************************************************************************)
(*                    FINDING AN ITEM OR NAME CHAIN                     *)
(************************************************************************)

PROCEDURE FindItem (TS: THandle;
                    VAR (*IN*) application, key: ARRAY OF CHAR;
                    VAR (*OUT*) result: StrPtr): BOOLEAN;

    (* Returns a copy of the specified item.  (Therefore the caller     *)
    (* must deallocate it after use.)  If application or key is an      *)
    (* empty string, returns the usual list of names.                   *)
    (* We assume that the caller has exclusive access to TS.            *)

    VAR appres: AppPtr;
        keyres: KeyPtr;
        exists: BOOLEAN;

    BEGIN
        result.length := 0;
        exists := ItemLength (TS, application, key, result.length);
        result.start := NIL;
        IF exists AND (result.length > 0) THEN
            ALLOCATE (result.start, result.length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, result.length);
            <* END *>
            IF application[0] = Nul THEN
                BuildAppList (TS, result);
            ELSE
                appres := FindApp (TS, application, FALSE);
                IF key[0] = Nul THEN
                    BuildKeyList (appres, result);
                ELSE
                    keyres := FindKey (appres, key, FALSE);

                    (* Make a copy of this result, so as to retain      *)
                    (* the original intact.  We have to return a copy   *)
                    (* to be consistent with the other cases handled    *)
                    (* above.  Note that we have already allocated the  *)
                    (* space.                                           *)

                    IF keyres^.val.length <> result.length THEN
                        result.length := keyres^.val.length;
                    END (*IF*);
                    Copy (keyres^.val.start, result.start, result.length);

                END (*IF*);
            END (*IF*);
        END (*IF*);
        RETURN exists;
    END FindItem;

(************************************************************************)
(*                         THE "GET" OPERATIONS                         *)
(************************************************************************)

PROCEDURE ItemSize (hini: THandle;  VAR (*IN*) application, key: ARRAY OF CHAR;
                                VAR (*OUT*) size: CARDINAL): BOOLEAN;

    (* Sets size to the size in bytes of the given INI file entry,      *)
    (* or returns FALSE if there is no such entry.                      *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (hini^.access);
        result := ItemLength (hini, application, key, size);
        Release (hini^.access);
        RETURN result;
    END ItemSize;

(************************************************************************)

PROCEDURE INIGetTrusted (hini: THandle;
                               VAR (*IN*) application, key: ARRAY OF CHAR;
                                       VAR (*OUT*) result: ARRAY OF LOC;
                                                 size: CARDINAL): BOOLEAN;

    (* Retrieves the value of a variable from the INI file.  Returns    *)
    (* FALSE if the variable was not found.  We trust the caller to     *)
    (* have ensured that the size is correct, so this procedure should  *)
    (* be called only when the size is known.                           *)

    VAR p: StrPtr;
        success: BOOLEAN;

    BEGIN
        p.start := NIL;
        success := hini <> NIL;
        IF success THEN
            Obtain (hini^.access);
            IF size = 0 THEN
                success := TRUE;
            ELSE
                success := FindItem (hini, application, key, p)
                                           AND (p.length = size);
                IF success THEN
                    IF p.start = NIL THEN
                        size := 0;
                    END (*IF*);
                    IF size > 0 THEN
                        Copy (p.start, ADR(result), size);
                    END (*IF*);
                END (*IF*);
                IF p.start <> NIL THEN
                    DEALLOCATE (p.start, p.length);
                    <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                        TrackUpdate (TNITrack, -VAL(INTEGER,p.length));
                    <* END *>
                END (*IF*);
            END (*IF*);
            Release (hini^.access);
        END (*IF*);
        RETURN success;
    END INIGetTrusted;

(************************************************************************)

PROCEDURE INIGetString (hini: THandle; VAR (*IN*)   name1, name2: ARRAY OF CHAR;
                                    VAR (*OUT*) variable: ARRAY OF CHAR): BOOLEAN;

    (* Like INIGet, but we accept any size data that will fit in the variable,  *)
    (* and we add a Nul terminator in the case of a size mismatch.              *)

    VAR p: StrPtr;
        success: BOOLEAN;

    BEGIN
        p.start := NIL;
        Obtain (hini^.access);
        success := FindItem (hini, name1, name2, p)
                                   AND (p.length <= HIGH(variable)+1);
        IF success THEN
            IF p.length > 0 THEN
                Copy (p.start, ADR(variable), p.length);
            END (*IF*);
            IF p.length <= HIGH(variable) THEN
                variable[p.length] := Nul;
            END (*IF*);
        END (*IF*);
        IF p.start <> NIL THEN
            DEALLOCATE (p.start, p.length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -VAL(INTEGER,p.length));
            <* END *>
        END (*IF*);
        Release (hini^.access);
        RETURN success;
    END INIGetString;

(************************************************************************)
(*                          THE "PUT" OPERATION                         *)
(*                                                                      *)
(*   There is only one "put" operation.  A higher-level module reduces  *)
(*   all cases down to this one single case.                            *)
(*                                                                      *)
(************************************************************************)

PROCEDURE INIPutBinary (hini: THandle;  VAR (*IN*)  name1, name2: ARRAY OF CHAR;
                        VAR (*IN*) variable: ARRAY OF LOC;  amount: CARDINAL);

    (* Writes data to the INI data structure. *)

    VAR ap: AppPtr;
        kp: KeyPtr;

    BEGIN
        Obtain (hini^.access);
        ap := FindApp (hini, name1, TRUE);
        kp := FindKey (ap, name2, TRUE);

        (* Get rid of the old value, if any. *)

        IF kp^.val.start <> NIL THEN
            DEALLOCATE (kp^.val.start, kp^.val.length);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, -VAL(INTEGER,kp^.val.length));
            <* END *>
            kp^.val.length := 0;
        END (*IF*);

        (* Now insert the new value. *)

        IF amount <> 0 THEN
            ALLOCATE (kp^.val.start, amount);
            <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
                TrackUpdate (TNITrack, amount);
            <* END *>
            Copy (ADR(variable), kp^.val.start, amount);
            kp^.val.length := amount;
        END (*IF*);

        hini^.changed := TRUE;
        Release (hini^.access);

    END INIPutBinary;

(************************************************************************)
(*                    DELETING APPLICATIONS AND KEYS                    *)
(************************************************************************)

PROCEDURE INIDeleteApp (hini: THandle;  VAR (*IN*) app: ARRAY OF CHAR);

    (* Deletes an application from the INI file. *)

    VAR prev, current: AppPtr;

    BEGIN
        IF hini <> NIL THEN
            Obtain (hini^.access);
            prev := NIL;  current := hini^.FirstApp;
            WHILE (current <> NIL)
                      AND NOT NameMatch (current^.name, app) DO
                prev := current;
                current := current^.next;
            END (*IF*);
            IF current <> NIL THEN
                IF prev = NIL THEN
                    hini^.FirstApp := current^.next;
                ELSE
                    prev^.next := current^.next;
                END (*IF*);
                DisposeOfApp (current);
            END (*IF*);
            hini^.changed := TRUE;
            Release (hini^.access);
        END (*IF*);
    END INIDeleteApp;

(************************************************************************)

PROCEDURE INIDeleteKey (hini: THandle;  VAR (*IN*) app, key: ARRAY OF CHAR);

    (* Deletes a key from the INI structure.                   *)

    VAR ap: AppPtr;
        prev, current: KeyPtr;

    BEGIN
        Obtain (hini^.access);
        ap := FindApp (hini, app, FALSE);
        IF ap <> NIL THEN
            prev := NIL;  current := ap^.FirstKey;
            WHILE (current <> NIL)
                      AND NOT NameMatch (current^.name, key) DO
                prev := current;
                current := current^.next;
            END (*IF*);
            IF current <> NIL THEN
                IF prev = NIL THEN
                    ap^.FirstKey := current^.next;
                ELSE
                    prev^.next := current^.next;
                END (*IF*);
                DisposeOfKey (current);
            END (*IF*);
            hini^.changed := TRUE;
        END (*IF*);
        Release (hini^.access);
    END INIDeleteKey;

(************************************************************************)
(*                             INITIALISATION                           *)
(************************************************************************)

BEGIN
    appcount := 0;
    TNIChain := NIL;
    NextName := "00000000";
    CreateLock (MasterLock);
    CreateLock (NextNameLock);
    <* IF DEFINED(TRACKTNIUSAGE) & TRACKTNIUSAGE THEN *>
        TNITrack := StartTracking ("TNIData");
    <* END *>
FINALLY
    DestroyLock (NextNameLock);
    DestroyLock (MasterLock);
END TNIData.

