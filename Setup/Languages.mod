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

IMPLEMENTATION MODULE Languages;

        (********************************************************)
        (*                                                      *)
        (*                Mailing list manager                  *)
        (*    Module to generate language-dependent messages    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            9 November 2003                 *)
        (*  Last edited:        4 May 2019                      *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, FileSys;

FROM SYSTEM IMPORT
    (* type *)  ADDRESS,
    (* proc *)  ADR;

FROM Names IMPORT
    (* type *)  FilenameString;

FROM FileOps IMPORT
    (* const*)  NoSuchChannel,
    (* type *)  ChanId,
    (* proc *)  GetEXEDirectory, OpenOldFile, CloseFile, ReadLine,
                WriteRaw, FWriteString, FWriteLJCard;

FROM Conversions IMPORT
    (* proc *)  CardinalToStringLJ;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  Copy, AddOffset;

(************************************************************************)
(*                         GLOBAL DECLARATIONS                          *)
(************************************************************************)

CONST
    Nul = CHR(0);
    CtrlZ = CHR(26);

TYPE
    TagType = ARRAY [0..63] OF CHAR;
    StringPtr = POINTER TO ARRAY [0..MAX(CARDINAL) DIV 4] OF CHAR;
    ValueString = RECORD
                      length: CARDINAL;
                      pval: StringPtr;
                  END (*RECORD*);

    (* For a single language, the data are arranged as a linear list of *)
    (* values.  For each element of the list, we have a tag and a       *)
    (* value; but we also have a subtree, to deal with the case that    *)
    (* the tag is itself structured.  The result is a linear list of    *)
    (* trees.                                                           *)

    TreePtr = POINTER TO LangDataRecord;
    LangHandle = TreePtr;
    LangDataRecord = RECORD
                         tag: TagType;
                         next: TreePtr;
                         subtree: TreePtr;
                         value: ValueString;
                     END (*RECORD*);

    (* To support multiple languages, we have a linear list where each  *)
    (* list element has a language name and a data structure as         *)
    (* described above.  The count shows how many clients are currently *)
    (* using this language structure.                                   *)

    LangListPtr = POINTER TO LangListRecord;
    LangListRecord = RECORD
                         count: CARDINAL;
                         name: LangName;
                         next: LangListPtr;
                         this: LangHandle;
                     END (*RECORD*);

(************************************************************************)

VAR
    (* The executable's directory, which is where the language files    *)
    (* are supposed to be located.                                      *)

    OurDir: FilenameString;

    (* The set of all language lists currently loaded. *)

    MasterList: RECORD
                    access: Lock;
                    head: LangListPtr;
                END (*RECORD*);

    (* The language to use if all else fails. *)

    DefaultLanguage: LangName;

    (* Some useful string 'constants'. *)

    Nothing: ARRAY [0..0] OF CHAR;
    CRLF: ARRAY [0..1] OF CHAR;

(************************************************************************)
(*                   OPERATIONS ON TYPE ValueString                     *)
(************************************************************************)

PROCEDURE DiscardValueString (VAR (*INOUT*) VS: ValueString);

    (* Disposes of the space allocated to the ValueString. *)

    BEGIN
        IF VS.length > 0 THEN
            DEALLOCATE (VS.pval, VS.length);
            VS.length := 0;
        END (*IF*);
    END DiscardValueString;

(************************************************************************)

PROCEDURE CreateValueString (VAR (*OUT*) VS: ValueString);

    (* Creates a new ValueString, with the empty string as its value. *)

    BEGIN
        VS.length := 1;
        ALLOCATE (VS.pval, 1);
        VS.pval^[0] := Nul;
    END CreateValueString;

(************************************************************************)

PROCEDURE AppendToValueString (VAR (*INOUT*) VS: ValueString;
                        VAR (*IN*) str: ARRAY OF CHAR;  amount: CARDINAL);

    (* Appends 'amount' more characters to the value. *)

    VAR oldlength, newlength: CARDINAL;
        pstring: StringPtr;

    BEGIN
        IF amount > 0 THEN
            oldlength := VS.length;
            newlength := oldlength + amount;
            ALLOCATE (pstring, newlength);
            IF oldlength > 0 THEN
                Copy (VS.pval, pstring, oldlength);
                Copy (ADR(str), AddOffset (pstring, oldlength-1), amount);
                DEALLOCATE (VS.pval, oldlength);
            ELSE
                Copy (ADR(str), pstring, amount);
            END (*IF*);
            pstring^[newlength-1] := Nul;
            VS.pval := pstring;
            VS.length := newlength;
        END (*IF*);
    END AppendToValueString;

(************************************************************************)

(*
PROCEDURE WriteValue (VS: ValueString);

    (* Writes a value to the screen. *)

    CONST buffersize = 4096;
    VAR buffer: ARRAY [0..buffersize-1] OF CHAR;  amount: CARDINAL;

    BEGIN
        amount := VS.length;
        IF amount > 0 THEN
            IF amount > buffersize THEN
                amount := buffersize;
            END (*IF*);
            Copy (VS.pval, ADR(buffer), amount);
            WriteString (buffer);
        END (*IF*);
    END WriteValue;
*)

(************************************************************************)
(*              MAPPING BETWEEN LANGUAGE CODE AND HANDLE                *)
(************************************************************************)

PROCEDURE SetDefaultLanguage (name: ARRAY OF CHAR);

    (* This is the language that will be used if no valid language      *)
    (* is specified.                                                    *)

    BEGIN
        Strings.Assign (name, DefaultLanguage);
    END SetDefaultLanguage;

(************************************************************************)

PROCEDURE LanguageCode (lang: LangHandle;  VAR (*OUT*) name: ARRAY OF CHAR);

    (* Sets 'name' to the code used to load the language.  *)

    VAR current: LangListPtr;

    BEGIN
        Obtain (MasterList.access);
        current := MasterList.head;
        WHILE (current <> NIL) AND (current^.this <> lang) DO
            current := current^.next;
        END (*LOOP*);
        IF current = NIL THEN
            Strings.Assign (DefaultLanguage, name);
        ELSE
            Strings.Assign (current^.name, name);
        END (*IF*);
        Release (MasterList.access);
    END LanguageCode;

(************************************************************************)
(*                  SEARCHING THROUGH A LANGUAGE TREE                   *)
(************************************************************************)

PROCEDURE Split (VAR (*OUT*) head: TagType;  VAR (*INOUT*) str: ARRAY OF CHAR);

    (* Sets head to the left part of str before the first '.', and      *)
    (* sets the new value of str to the remainder (not including the    *)
    (* dot).  If there was no dot, returns with str empty and with      *)
    (* head equal to the original string.  Special case: if str starts  *)
    (* with a dot, we return head = '?' since an empty head would       *)
    (* cause trouble elsewhere in the program.                          *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.FindNext ('.', str, 0, found, pos);
        IF found THEN
            IF pos = 0 THEN
                head[0] := '?';
            ELSE
                Strings.Assign (str, head);
                head[pos] := Nul;
            END (*IF*);
            Strings.Delete (str, 0, pos+1);
        ELSE
            Strings.Assign (str, head);
            str[0] := Nul;
        END (*IF*);
    END Split;

(************************************************************************)

PROCEDURE LocateNode (root: TreePtr;
                        VAR (*INOUT*) label: ARRAY OF CHAR;
                        VAR (*OUT*) head: TagType;
                        VAR (*OUT*) ResultIsParent: BOOLEAN): TreePtr;

    (* Finds 'label' in the tree whose root is specified as the first   *)
    (* parameter, or if not present finds where it should be inserted.  *)
    (* The label was completely resolved (i.e. no insertion is          *)
    (* necessary) iff head is returned as the empty string.             *)
    (*                                                                  *)
    (* Input:                                                           *)
    (*   root is the starting node for our search.                      *)
    (*                                                                  *)
    (* On success:                                                      *)
    (*   head is the empty string, the function result is the found     *)
    (*   node, other parameters are irrelevant.                         *)
    (*                                                                  *)
    (* On failure:                                                      *)
    (*   head holds the first component of the label that could not     *)
    (*      be found, label holds the following components if any.      *)
    (*      The function result is the predecessor of where the new     *)
    (*      node should be inserted, with ResultIsParent TRUE if the    *)
    (*      predecessor is a parent rather than a left sibling.         *)

    VAR comp: Strings.CompareResults;
        predecessor, p: TreePtr;

    BEGIN
        predecessor := NIL;  ResultIsParent := FALSE;
        p := root;
        LOOP
            Split (head, label);

            (* Find head, or an insertion point for head, in the linear     *)
            (* list that starts with p^.                                    *)

            comp := Strings.less;
            LOOP
                IF p = NIL THEN EXIT(*LOOP*) END(*IF*);
                comp := Strings.Compare (p^.tag, head);
                IF comp <> Strings.less THEN EXIT(*LOOP*) END(*IF*);
                ResultIsParent := FALSE;
                predecessor := p;  p := p^.next;
            END (*LOOP*);

            IF (p = NIL) OR (comp <> Strings.equal) THEN
                p := predecessor;
                EXIT (*LOOP*);
            END (*IF*);

            head[0] := Nul;

            (* We have a match so far.  Do we need to go deeper *)
            (* into the tree?                                   *)

            IF label[0] = Nul THEN
                ResultIsParent := FALSE;
                EXIT (*LOOP*);
            END (*IF*);

            ResultIsParent := TRUE;
            predecessor := p;  p := p^.subtree;

        END (*LOOP*);

        RETURN p;

    END LocateNode;

(************************************************************************)
(*                           OUTPUT TO A FILE                           *)
(************************************************************************)

PROCEDURE OutString (cid: ChanId;  strlength: CARDINAL;
                            VAR (*IN*) str, Param1, Param2: ARRAY OF CHAR;
                            number: CARDINAL);

    (* Writes str with parameter expansion.  The parameter position     *)
    (* within str is indicated by the code:                             *)
    (*      %a    =>   value of Param1                                  *)
    (*      %b    =>   value of Param2                                  *)
    (*      %1    =>   value of number                                  *)
    (* Any other occurence of the % character is taken literally.       *)

    (* Remark: We break the string up into small buffers mainly because *)
    (* Strings.FindNext insists on having value parameters.             *)

    CONST Buffersize = 256;

    VAR amount, foundpos: CARDINAL;  pos: ADDRESS;  found: BOOLEAN;
        chptr: POINTER TO CHAR;
        buffer: ARRAY [0..Buffersize-1] OF CHAR;

    BEGIN
        pos := ADR(str);
        WHILE strlength > 0 DO

            IF strlength >= Buffersize THEN
                amount := Buffersize-1;
            ELSE
                amount := strlength;
            END (*IF*);
            Copy (pos, ADR(buffer), amount);
            pos := AddOffset (pos, amount);
            DEC (strlength, amount);

            (* Make sure the string in the buffer is Nul-terminated,    *)
            (* but also make sure that the Nul is not counted in amount.*)

            IF buffer[amount-1] = Nul THEN
                DEC (amount);
            ELSE
                buffer[amount] := Nul;
            END (*IF*);

            (* We have now moved a substring into the buffer.  Check    *)
            (* for the presence of one or more % codes.                 *)

            found := amount > 0;
            WHILE found DO

                Strings.FindNext ('%', buffer, 0, found, foundpos);
                IF found THEN
                    IF foundpos > 0 THEN
                        WriteRaw (cid, buffer, foundpos);
                        Strings.Delete (buffer, 0, foundpos);
                        DEC (amount, foundpos);
                    END (*IF*);

                    (* Now the % is in buffer[0].  Check for the special   *)
                    (* case where the % was right at the end of buffer.    *)

                    IF (amount = 1) AND (strlength > 0) THEN
                        chptr := pos;
                        buffer[1] := chptr^;
                        buffer[2] := Nul;
                        pos := AddOffset (pos, 1);
                        DEC (strlength);
                    END (*IF*);

                    (* Is the % really a parameter marker? *)

                    CASE buffer[1] OF
                      | '1': FWriteLJCard (cid, number);
                             Strings.Delete (buffer, 0, 2);
                             DEC (amount, 2);

                      | 'a': FWriteString (cid, Param1);
                             Strings.Delete (buffer, 0, 2);
                             DEC (amount, 2);

                      | 'b': FWriteString (cid, Param2);
                             Strings.Delete (buffer, 0, 2);
                             DEC (amount, 2);

                      | ELSE
                             WriteRaw (cid, buffer, 1);
                             Strings.Delete (buffer, 0, 1);
                             DEC (amount, 1);
                    END (*CASE*);

                    found := amount > 0;

                END (*IF found*);

            END (*WHILE found*);

            IF amount > 0 THEN
                WriteRaw (cid, buffer, amount);
            END (*IF*);

        END (*WHILE strlength > 0*);

    END OutString;

(************************************************************************)

PROCEDURE LWriteStringABN (LH: LangHandle;  cid: ChanId;
                   StringId, Param1, Param2: ARRAY OF CHAR;  N: CARDINAL);

    (* Writes a string to a file, with two string parameters and one *)
    (* numeric parameter.                                            *)

    VAR node: TreePtr;  head: TagType;  ResultIsParent: BOOLEAN;

    BEGIN
        node := LocateNode (LH, StringId, head, ResultIsParent);
        IF head[0] = Nul THEN
            WITH node^.value DO
                OutString (cid, length, pval^, Param1, Param2, N);
            END (*WITH*);
        END (*IF*);
    END LWriteStringABN;

(************************************************************************)

PROCEDURE LWriteStringAN (LH: LangHandle;  cid: ChanId;
                            StringId, Param: ARRAY OF CHAR;  N: CARDINAL);

    (* Writes a string to a file, with one string parameter and one  *)
    (* numeric parameter.                                            *)

    BEGIN
        LWriteStringABN (LH, cid, StringId, Param, Nothing, N);
    END LWriteStringAN;

(************************************************************************)

PROCEDURE LWriteStringA (LH: LangHandle;  cid: ChanId;
                                         StringId, Param: ARRAY OF CHAR);

    (* Writes a string to a file, with one string parameter. *)

    BEGIN
        LWriteStringABN (LH, cid, StringId, Param, Nothing, 0);
    END LWriteStringA;

(************************************************************************)

PROCEDURE LWriteStringAB (LH: LangHandle;  cid: ChanId;
                                 StringId, Param1, Param2: ARRAY OF CHAR);

    (* Writes a string to a file, with two string parameters. *)

    BEGIN
        LWriteStringABN (LH, cid, StringId, Param1, Param2, 0);
    END LWriteStringAB;

(************************************************************************)

PROCEDURE LWriteStringN (LH: LangHandle;  cid: ChanId;
                               StringId: ARRAY OF CHAR;  Param: CARDINAL);

    (* Writes a string to a file, with one numeric parameter. *)

    BEGIN
        LWriteStringAN (LH, cid, StringId, Nothing, Param);
    END LWriteStringN;

(************************************************************************)

PROCEDURE LWriteString (LH: LangHandle;  cid: ChanId;
                                         StringId: ARRAY OF CHAR);

    (* Writes a string to a file. *)

    BEGIN
        LWriteStringA (LH, cid, StringId, Nothing);
    END LWriteString;

(************************************************************************)
(*              CONVERSION FROM LANGUAGE STRING TO BUFFER               *)
(************************************************************************)

PROCEDURE StrToBufferABN (LH: LangHandle;  StringId, Param1, Param2: ARRAY OF CHAR;
                              N: CARDINAL;  VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Translates StringId to buffer, with two string parameters and    *)
    (* one numeric parameter.  We assume that the expanded message      *)
    (* will still fit into a single buffer and that each parameter will *)
    (* appear only once.  This allows us to have a simpler solution     *)
    (* than for the above procedures for writing to a file.             *)

    VAR node: TreePtr;  head: TagType;  ResultIsParent, found: BOOLEAN;
        foundpos: CARDINAL;
        NumBuffer: ARRAY [0..31] OF CHAR;

    BEGIN
        node := LocateNode (LH, StringId, head, ResultIsParent);
        IF head[0] = Nul THEN
            WITH node^.value DO
                IF length > 1 THEN
                    Copy (pval, ADR(buffer), length);
                END (*IF*);
                buffer[length] := Nul;
            END (*WITH*);

            (* Second string parameter, if any. *)

            Strings.FindNext ('%b', buffer, 0, found, foundpos);
            IF found THEN
                Strings.Delete (buffer, foundpos, 2);
                Strings.Insert (Param2, foundpos, buffer);
            END (*IF*);

            (* First string parameter, if any. *)

            Strings.FindNext ('%a', buffer, 0, found, foundpos);
            IF found THEN
                Strings.Delete (buffer, foundpos, 2);
                Strings.Insert (Param1, foundpos, buffer);
            END (*IF*);

            (* Numeric parameter, if any. *)

            Strings.FindNext ('%1', buffer, 0, found, foundpos);
            IF found THEN
                Strings.Delete (buffer, foundpos, 2);
                CardinalToStringLJ (N, NumBuffer);
                Strings.Insert (NumBuffer, foundpos, buffer);
            END (*IF*);
        ELSE
            Strings.Assign (Param1, buffer);
            Strings.Append (Param2, buffer);
        END (*IF*);
    END StrToBufferABN;

(************************************************************************)

PROCEDURE StrToBuffer (LH: LangHandle;  StringId: ARRAY OF CHAR;
                                       VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Translates StringId to buffer.  We assume that the expanded      *)
    (* message will fit into the buffer.                                *)

    BEGIN
        StrToBufferABN (LH, StringId, Nothing, Nothing, 0, buffer);
    END StrToBuffer;

(************************************************************************)

PROCEDURE StrToBufferA (LH: LangHandle;  StringId, param: ARRAY OF CHAR;
                                       VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Like StrToBuffer, but with a text parameter to substitute for %a.*)

    BEGIN
        StrToBufferABN (LH, StringId, param, Nothing, 0, buffer);
    END StrToBufferA;

(************************************************************************)

PROCEDURE StrToBufferAB (LH: LangHandle;
                                 StringId, param1, param2: ARRAY OF CHAR;
                                       VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Like StrToBuffer, but with two text parameters.*)

    BEGIN
        StrToBufferABN (LH, StringId, param1, param2, 0, buffer);
    END StrToBufferAB;

(************************************************************************)

PROCEDURE StrToBufferN (LH: LangHandle;  StringId: ARRAY OF CHAR;
                                       N: CARDINAL;
                                       VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Like StrToBuffer, but with a numeric parameter %1.*)

    BEGIN
        StrToBufferABN (LH, StringId, Nothing, Nothing, N, buffer);
    END StrToBufferN;

(************************************************************************)

PROCEDURE StrToBufferAN (LH: LangHandle;  StringId: ARRAY OF CHAR;
                                    txtparam: ARRAY OF CHAR;  N: CARDINAL;
                                    VAR (*OUT*) buffer: ARRAY OF CHAR);

    (* Like StrToBuffer, but with a text parameter %a and a numeric     *)
    (* parameter %1.                                                    *)

    BEGIN
        StrToBufferABN (LH, StringId, txtparam, Nothing, N, buffer);
    END StrToBufferAN;

(************************************************************************)
(*                         TRANSACTION LOGGING                          *)
(************************************************************************)
(*
PROCEDURE LLogTransactionABN (LH: LangHandle;  LogId: TransactionLogID;
                              StringId, Param1, Param2: ARRAY OF CHAR;
                                N: CARDINAL);

    (* Writes a line to the log file, with two string parameters and    *)
    (* one numeric parameter.  We assume that the expanded log message  *)
    (* will still fit into a single buffer and that each parameter will *)
    (* appear only once.  This allows us to have a simpler solution     *)
    (* than for the above procedures for writing to a file.             *)

    CONST BufferSize = 256;

    VAR buffer: ARRAY [0..BufferSize-1] OF CHAR;

    BEGIN
        StrToBufferABN (LH, StringId, Param1, Param2, N, buffer);
        LogTransaction (LogId, buffer);
    END LLogTransactionABN;

(************************************************************************)

PROCEDURE LLogTransactionAB (LH: LangHandle;  LogId: TransactionLogID;
                                 StringId, Param1, Param2: ARRAY OF CHAR);

    (* Writes a line to the log file, with two string parameters.  *)

    BEGIN
        LLogTransactionABN (LH, LogId, StringId, Param1, Param2, 0);
    END LLogTransactionAB;

(************************************************************************)

PROCEDURE LLogTransactionA (LH: LangHandle;  LogId: TransactionLogID;
                                         StringId, Param: ARRAY OF CHAR);

    (* Writes a line to the log file, with one string parameter.  *)

    BEGIN
        LLogTransactionABN (LH, LogId, StringId, Param, Nothing, 0);
    END LLogTransactionA;

(************************************************************************)

PROCEDURE LLogTransactionAN (LH: LangHandle;  LogId: TransactionLogID;
                             StringId, Param: ARRAY OF CHAR;  N: CARDINAL);

    (* Writes a line to the log file, with one string parameter and one *)
    (* numeric parameter.                                               *)

    BEGIN
        LLogTransactionABN (LH, LogId, StringId, Nothing, Nothing, N);
    END LLogTransactionAN;

(************************************************************************)

PROCEDURE LLogTransaction (LH: LangHandle;  LogId: TransactionLogID;
                                            StringId: ARRAY OF CHAR);

    (* Writes a line to the log file.  *)

    BEGIN
        LLogTransactionAB (LH, LogId, StringId, Nothing, Nothing);
    END LLogTransaction;
*)
(************************************************************************)
(*              LOADING THE DATA FOR ONE SPECIFIC LANGUAGE              *)
(************************************************************************)

PROCEDURE GoToNode (VAR (*INOUT*) root: LangHandle;
                             VAR (*IN*) label: ARRAY OF CHAR): TreePtr;

    (* Finds the tree node corresponding to 'label'.  Unlike LocateNode *)
    (* this procedure always succeeds, because it creates new empty     *)
    (* nodes as necessary.  Parameter 'root' is normally not altered,   *)
    (* but a change might be necessary if we are inserting a new node   *)
    (* ahead of the original list head.                                 *)

    VAR this, predecessor: TreePtr;
        head: TagType;  ResultIsParent: BOOLEAN;

    BEGIN
        predecessor := LocateNode (root, label, head, ResultIsParent);
        this := predecessor;
        WHILE head[0] <> Nul DO

            (* Insert a new node into the tree. *)

            NEW (this);
            WITH this^ DO
                tag := head;
                next := NIL;
                subtree := NIL;
            END (*WITH*);
            CreateValueString (this^.value);

            IF predecessor = NIL THEN
                this^.next := root;
                root := this;
            ELSIF ResultIsParent THEN
                this^.next := predecessor^.subtree;
                predecessor^.subtree := this;
            ELSE
                this^.next := predecessor^.next;
                predecessor^.next := this;
            END (*IF*);

            (* Do we need to create a subtree? *)

            IF label[0] = Nul THEN
                head[0] := Nul;
            ELSE
                Split (head, label);
                predecessor := this;
                ResultIsParent := TRUE;
            END (*IF*);

        END (*WHILE*);

        RETURN this;

    END GoToNode;

(************************************************************************)

PROCEDURE SetValue (VAR (*INOUT*) root: LangHandle;
                     VAR (*IN*) label, line: ARRAY OF CHAR;  cid: ChanId);

    (* Stores a value for 'label' in tree headed by 'root'.  The first  *)
    (* line of the value is in parameter 'line', but if this is a       *)
    (* quoted string with no closing quote then we continue reading     *)
    (* further lines from channel 'cid'.  Parameter root is normally    *)
    (* not changed, but a change might be necessary if we are inserting *)
    (* new data ahead of the original list head.                        *)

    VAR node: TreePtr;  quotechar: CHAR;  quoted: BOOLEAN;
        length: CARDINAL;

    BEGIN
        node := GoToNode (root, label);
        length := Strings.Length(line);
        quotechar := line[0];
        quoted := (quotechar = '"') OR (quotechar = "'");
        IF quoted THEN
            Strings.Delete (line, 0, 1);
            DEC (length);
            IF (length > 0) AND (line[length-1] = quotechar) THEN
                line[length-1] := Nul;
                DEC (length);
                quoted := FALSE;
            END (*IF*);
        END (*IF*);
        IF length > 0 THEN
            AppendToValueString (node^.value, line, length);
        END (*IF*);

        (* At this stage we have stored the result in the first line.   *)
        (* If quoted is still TRUE, it means we expect a multiline      *)
        (* result.                                                      *)

        WHILE quoted DO
            AppendToValueString (node^.value, CRLF, 2);
            ReadLine (cid, line);
            IF line[0] = CtrlZ THEN
                length := 0;  quoted := FALSE;
            ELSE
                length := Strings.Length(line);
            END (*IF*);
            IF (length > 0) AND (line[length-1] = quotechar) THEN
                line[length-1] := Nul;
                DEC (length);
                quoted := FALSE;
            END (*IF*);
            IF length > 0 THEN
                AppendToValueString (node^.value, line, length);
            END (*IF*);
        END (*WHILE*);

    END SetValue;

(************************************************************************)

PROCEDURE LoadLanguageData (VAR (*IN*) name: FilenameString): LangHandle;

    (* Loads the data from file 'file'. *)

    TYPE LineString = ARRAY [0..1023] OF CHAR;

    VAR line: LineString;

    (********************************************************************)

    PROCEDURE SkipSpaces;

        (* Removes leading spaces from 'line'. *)

        VAR j: CARDINAL;

        BEGIN
            j := 0;
            WHILE line[j] = ' ' DO INC(j) END(*WHILE*);
            IF j > 0 THEN
                Strings.Delete (line, 0, j);
            END (*IF*);
        END SkipSpaces;

    (********************************************************************)

    VAR result: LangHandle;
        cid: ChanId;
        label: LineString;
        found: BOOLEAN;
        pos: CARDINAL;

    BEGIN
        result := NIL;
        cid := OpenOldFile (name, FALSE, FALSE);
        IF cid <> NoSuchChannel THEN
            LOOP
                ReadLine (cid, line);
                SkipSpaces;
                IF line[0] = CtrlZ THEN
                    EXIT (*LOOP*);
                END (*IF*);
                IF (line[0] <> '#') AND (line[0] <> Nul) THEN

                    (* Parse the line.  It should be of the form        *)
                    (*     label' 'value                                *)

                    label := line;
                    Strings.FindNext (' ', line, 0, found, pos);
                    IF found THEN
                        label[pos] := Nul;
                        Strings.Delete (line, 0, pos+1);
                        SkipSpaces;
                    ELSE
                        line := "";
                    END (*IF*);
                    SetValue (result, label, line, cid);

                END (*IF*);
            END (*LOOP*);
            CloseFile (cid);
        END (*IF*);

        RETURN result;

    END LoadLanguageData;

(************************************************************************)

PROCEDURE UnloadLanguage (VAR (*INOUT*) L: LangHandle);

    (* Removes this language from our in-memory database.  The caller  *)
    (* has the responsibility of removing it from the master list.     *)

    VAR next: LangHandle;

    BEGIN
        WHILE L <> NIL DO
            DiscardValueString (L^.value);
            UnloadLanguage (L^.subtree);
            next := L^.next;
            DEALLOCATE (L, SIZE(LangDataRecord));
            L := next;
        END (*WHILE*);
    END UnloadLanguage;

(************************************************************************)
(*            OPERATIONS ON THE MASTER LIST OF ALL LANGUAGES            *)
(************************************************************************)

PROCEDURE LoadLanguage (prefix, name: ARRAY OF CHAR): LangHandle;

    (* Checks whether 'name' is a language we already have loaded.  If  *)
    (* so, returns its handle.  If not, loads data for the new language *)
    (* and creates and returns a new handle.                            *)

    VAR prev, current, next: LangListPtr;  result: LangHandle;
        comp: Strings.CompareResults;  file: FilenameString;

    BEGIN
        Strings.Capitalize (name);
        Obtain (MasterList.access);
        prev := NIL;  current := MasterList.head;
        comp := Strings.less;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            comp := Strings.Compare (current^.name, name);
            IF comp <> Strings.less THEN EXIT(*LOOP*) END(*IF*);
            prev := current;  current := current^.next;
        END (*LOOP*);
        IF comp = Strings.equal THEN
            INC (current^.count);
            result := current^.this;
        ELSE
            Strings.Assign (prefix, file);
            Strings.Append (".", file);
            Strings.Append (name, file);
            Strings.Append (".lng", file);
            IF NOT FileSys.Exists (file) THEN
                Strings.Assign (prefix, file);
                Strings.Append (".", file);
                Strings.Append (DefaultLanguage, file);
                Strings.Append (".lng", file);
            END (*IF*);
            result := LoadLanguageData (file);
            IF result <> NIL THEN
                next := current;
                NEW (current);
                current^.next := next;
                IF prev = NIL THEN
                    MasterList.head := current;
                ELSE
                    prev^.next := current;
                END (*IF*);
                current^.count := 1;
                Strings.Assign (name, current^.name);
                current^.this := result;
            END (*IF*);
        END (*IF*);
        Release (MasterList.access);
        RETURN result;
    END LoadLanguage;

(************************************************************************)
(*                    INTRODUCTION OF A NEW CLIENT                      *)
(************************************************************************)

PROCEDURE LanguageSupported (prefix, name: ARRAY OF CHAR): BOOLEAN;

    (* Checks for the existence of a file called 'prefix.name.lng'. *)

    VAR file: FilenameString;

    BEGIN
        Strings.Assign (prefix, file);
        Strings.Append (".", file);
        Strings.Append (name, file);
        Strings.Append (".lng", file);
        RETURN FileSys.Exists (file);
    END LanguageSupported;

(************************************************************************)

PROCEDURE UseLanguage (prefix, name: ARRAY OF CHAR): LangHandle;

    (* The arguments are text strings that refer to a filename          *)
    (* 'prefix.name.lng'.  That file is supposed to hold                *)
    (* language-dependent messages.  If the file does not exist, we use *)
    (* the default language.  If that too does not exist, the results   *)
    (* are unpredictable but will probably lead to blank messages.      *)
    (* Returns a handle that can be used in future calls to this module.*)

    VAR fullprefix: FilenameString;

    BEGIN
        fullprefix := OurDir;
        IF fullprefix[0] <> Nul THEN
            Strings.Append ('\', fullprefix);
        END (*IF*);
            Strings.Append (prefix, fullprefix);
        RETURN LoadLanguage (fullprefix, name);
    END UseLanguage;

(************************************************************************)

PROCEDURE DropLanguage (VAR (*INOUT*) LH: LangHandle);

    (* Decrements the count of users of this language.  If the count    *)
    (* becomes zero, discards the language data.  In any case, sets     *)
    (* LH to NIL.                                                       *)

    VAR prev, current: LangListPtr;

    BEGIN
        Obtain (MasterList.access);
        prev := NIL;  current := MasterList.head;
        WHILE (current <> NIL) AND (current^.this <> LH) DO
            prev := current;  current := current^.next;
        END (*WHILE*);
        IF current <> NIL THEN
            DEC (current^.count);
            IF current^.count = 0 THEN
                UnloadLanguage (LH);
                IF prev = NIL THEN
                    MasterList.head := current^.next;
                ELSE
                    prev^.next := current^.next;
                END (*IF*);
                DEALLOCATE (current, SIZE(LangListRecord));
            END (*IF*);
        END (*IF*);
        Release (MasterList.access);
    END DropLanguage;

(************************************************************************)
(*                               TESTING                                *)
(************************************************************************)

(*
PROCEDURE DumpTree (root: TreePtr;  indent: CARDINAL);

    (* For testing (will become obsolete once this module is working).  *)
    (* Writes data structure contents to the screen.                    *)

    VAR j: CARDINAL;

    BEGIN
        FOR j := 0 TO indent DO
            WriteChar ('');
        END (*FOR*);
        WriteString (root^.tag);  WriteString (': ');
        WriteValue (root^.value);  WriteLn;
        root := root^.subtree;
        INC (indent, 4);
        WHILE root <> NIL DO
            DumpTree (root, indent);
            root := root^.next;
        END (*WHILE*);
    END DumpTree;

(************************************************************************)

PROCEDURE DumpLanguage (p: LangHandle);

    (* For testing (will become obsolete once this module is working).  *)
    (* Writes data structure contents to the screen.                    *)

    BEGIN
        IF p = NIL THEN
            WriteString ("Empty language record.");  WriteLn;
        ELSE
            WHILE p <> NIL DO
                DumpTree (p, 0);
                p := p^.next;
            END (*WHILE*);
        END (*IF*);
    END DumpLanguage;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Writes a decimal number to the screen. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);  N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(N + ORD('0')));
    END WriteCard;

(************************************************************************)

PROCEDURE DumpData;

    (* For testing (will become obsolete once this module is working).  *)
    (* Writes data structure contents to the screen.                    *)

    VAR p: LangListPtr;

    BEGIN
        Obtain (MasterList.access);
        p := MasterList.head;
        IF p = NIL THEN
            WriteString ("No languages are loaded.");
        ELSE
            WHILE p <> NIL DO
               WriteString ("Dumping data for language ");
               WriteString (p^.name);  WriteString (", count = ");
               WriteCard (p^.count);  WriteLn;
               DumpLanguage (p^.this);
               p := p^.next;
               IF p <> NIL THEN
                   WriteString ("==============================================");
                   WriteLn;
               END (*IF*);
            END (*WHILE*);
        END (*IF*);
        Release (MasterList.access);
        WriteLn;
    END DumpData;
*)

(************************************************************************)
(*                MODULE INITIALISATION AND FINALISATION                *)
(************************************************************************)

PROCEDURE UnloadAllLanguages;

    (* Discards the list of all language data. *)

    VAR this, next: LangListPtr;

    BEGIN
        Obtain (MasterList.access);
        this := MasterList.head;
        MasterList.head := NIL;
        WHILE this <> NIL DO
            next := this^.next;
            UnloadLanguage (this^.this);
            DEALLOCATE (this, SIZE(LangListRecord));
            this := next;
        END (*WHILE*);
        Release (MasterList.access);
    END UnloadAllLanguages;

(************************************************************************)

BEGIN
    Nothing[0] := Nul;
    CRLF[0] := CHR(13);  CRLF[1] := CHR(10);
    GetEXEDirectory (OurDir);
    SetDefaultLanguage ("EN");
    WITH MasterList DO
        CreateLock (access);
        head := NIL;
    END (*WITH*);
FINALLY
    UnloadAllLanguages;
    DestroyLock (MasterList.access);
END Languages.

