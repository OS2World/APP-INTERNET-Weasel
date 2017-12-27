IMPLEMENTATION MODULE GetDNStxt;

        (********************************************************)
        (*                                                      *)
        (*       Extracting TXT records from a nameserver       *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            22 December 2016                *)
        (*  Last edited:        22 May 2017                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SYSTEM IMPORT
    (* type *)  CARD8, CARD16;

FROM NameServer IMPORT
    (* const*)  C_IN, T_TXT,
    (* proc *)  res_query;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM MiscFuncs IMPORT
    (* proc *)  HeadMatch;

(************************************************************************)

CONST Nul = CHR(0);

(************************************************************************)
(*                          WORKING WITH StringLists                    *)
(************************************************************************)

PROCEDURE DiscardResults (VAR (*INOUT*) head: StringList);

    (* Deletes a StringList. Must be called after extracting the        *)
    (* results, unless the caller has taken responsibility for deleting *)
    (* the list.                                                        *)

    VAR next: StringList;

    BEGIN
        WHILE head <> NIL DO
            next := head^.next;
            DISPOSE (head);
            head := next;
        END (*WHILE*);
    END DiscardResults;

(************************************************************************)

PROCEDURE AddResult (VAR (*INOUT*) head: StringList;
                        VAR (*IN*) val: ARRAY OF CHAR);

    (* Adds a new string to our list of results. *)

    VAR this, tail: StringList;

    BEGIN
        tail := head;
        WHILE (tail <> NIL) AND (tail^.next <> NIL) DO
            tail := tail^.next;
        END (*WHILE*);
        NEW (this);
        this^.next := NIL;
        Strings.Assign (val, this^.val);
        IF tail = NIL THEN
            head := this;
        ELSE
            tail^.next := this;
        END (*IF*);
    END AddResult;

(************************************************************************)
(*         EXTRACTING THE STRINGS FROM THE NAMESERVER RESPONSE          *)
(************************************************************************)

PROCEDURE Get16 (VAR (*IN*) buffer: ARRAY OF CARD8;
                            VAR (*INOUT*) j: CARDINAL): CARDINAL;

    (* Picks up a two-byte value, most significant byte first. *)

    VAR b1, b2: CARD8;

    BEGIN
        b1 := buffer[j];  INC(j);
        b2 := buffer[j];  INC(j);
        RETURN 256*b1 + b2;
    END Get16;

(************************************************************************)

PROCEDURE SkipName (VAR (*IN*) buffer: ARRAY OF CARD8;
                          VAR (*INOUT*) j: CARDINAL);

    (* Skips over a name in the buffer. *)

    VAR count: CARD8;

    BEGIN
        REPEAT
            count := buffer[j];  INC(j);
            IF count >= 192 THEN
                INC (j);  count := 0;
            ELSIF count > 0 THEN
                INC (j, count);
            END (*IF*);
        UNTIL count = 0;
    END SkipName;

(************************************************************************)

PROCEDURE SkipQuestion (VAR (*IN*) buffer: ARRAY OF CARD8;
                          VAR (*INOUT*) j: CARDINAL;  count: CARDINAL);

    (* Skips over the "question" records.  *)

    VAR k: CARDINAL;

    BEGIN
        FOR k := 1 TO count DO
            SkipName (buffer, j);
            INC (j, 4);
        END (*FOR*);
    END SkipQuestion;

(************************************************************************)

PROCEDURE InterpretResourceRecords (VAR (*IN*) buffer: ARRAY OF CARD8;
                                        VAR (*INOUT*) j: CARDINAL;
                                        count: CARDINAL;
                                        filter: ARRAY OF CHAR): StringList;

    (* Interpretation starts at buffer[j], and stops when we've         *)
    (* extracted all information for this section or when j >= count.   *)
    (* On exit buffer[j] is the first byte we haven't used.  We return  *)
    (* only those records that start with the filter string.            *)

    VAR k, m, n, QTYPE, size, subsize: CARDINAL;
        txtbuf: ARRAY [0..511] OF CHAR;
        list: StringList;

    BEGIN
        list := NIL;
        k := 0;
        WHILE k < count DO

            (* We can ignore the name in the resource record. *)

            SkipName (buffer, j);
            QTYPE := Get16 (buffer, j);
            INC (j, 6);    (* skip class and TTL *)

            IF QTYPE = T_TXT THEN

                (* TXT records are the only records that interest   *)
                (* us.  The record may contain a sequence of        *)
                (* substrings.                                      *)

                size := Get16 (buffer, j);
                n := 0;
                WHILE size > 0 DO
                    subsize := buffer[j];  INC (j);  DEC(size);
                    IF subsize > 0 THEN
                        FOR m := 0 TO subsize-1 DO
                            txtbuf[n] := CHR(buffer[j]);
                            INC (n);  INC (j);  DEC(size);
                        END (*FOR*);
                    END (*FOR*);
                END (*WHILE*);
                txtbuf[n] := Nul;

                (* We have found a new string.  *)

                IF HeadMatch (txtbuf, filter) THEN
                    AddResult (list, txtbuf);
                END (*IF*);

            ELSE
                size := Get16 (buffer, j);
                INC (j, size);
            END (*IF*);

            INC (k);
        END (*WHILE*);

        RETURN list;

    END InterpretResourceRecords;

(************************************************************************)

PROCEDURE ExtractTXTrecords (VAR (*IN*) buffer: ARRAY OF CARD8;
                             filter: ARRAY OF CHAR;
                             VAR (*OUT*) list: StringList): CARDINAL;

    (* Extracts the TXT records that begin with the specified filter    *)
    (* from a nameserver response.  Returns the RCODE success code of   *)
    (* the query.                                                       *)

    VAR j, QuestionCount, AnswerCount: CARDINAL;
        flags: CARD16;

    BEGIN
        j := 2;
        list := NIL;

        (* Interpret the flags, ignoring the bits that don't interest us. *)

        flags := IAND (Get16 (buffer, j), 000FH);
        IF flags = 0 THEN

            QuestionCount := Get16 (buffer, j);
            AnswerCount := Get16 (buffer, j);

            (* Skip the authority and additional counts. *)

            INC (j, 4);

            (* Skip the question, extract the answer. *)

            SkipQuestion (buffer, j, QuestionCount);
            list := InterpretResourceRecords (buffer, j, AnswerCount, filter);

        END (*IF*);

        RETURN flags;

    END ExtractTXTrecords;

(************************************************************************)

PROCEDURE GetTXTrecords (VAR (*IN*) host: ARRAY OF CHAR;
                  filter: ARRAY OF CHAR;
                  VAR (*OUT*) list: StringList): CARDINAL;

    (* Does a TXT query for the given host, returns a list of TXT       *)
    (* records that begin with the filter string.  (If the filter is an *)
    (* empty string, returns all TXT records.)                          *)
    (* Function result is 0 iff the nameserver gave a useable result.   *)

    CONST BufferSize = 512;  MaxResultSize = 512;

    TYPE BufferSubscript = [0..BufferSize-1];

    VAR length, rcode: CARDINAL;
        buffer: ARRAY BufferSubscript OF CARD8;

    BEGIN
        list := NIL;
        length := res_query (host, C_IN, T_TXT, buffer, BufferSize);
        IF length = MAX(CARDINAL) THEN

            (* There are several possible causes for failure, but for   *)
            (* our purposes they can all be treated as "no useful       *)
            (* result from nameserver".                                 *)

            rcode := 1;

        ELSIF length >= MaxResultSize THEN
            rcode := 1;
            list := NIL;
        ELSE
            rcode := ExtractTXTrecords (buffer, filter, list);
        END (*IF*);

        RETURN rcode;

    END GetTXTrecords;

(************************************************************************)

PROCEDURE GetFirstTXTrecord (VAR (*IN*) host: ARRAY OF CHAR;
                  filter: ARRAY OF CHAR;
                  VAR (*OUT*) result: ARRAY OF CHAR): CARDINAL;

    (* Like GetTXTrecords, but for the case where we know we will only  *)
    (* accept the first result that matches the filter.                 *)

    VAR code: CARDINAL;
        list: StringList;

    BEGIN
        code := GetTXTrecords (host, filter, list);
        IF list = NIL THEN
            result[0] := Nul;
        ELSE
            Strings.Assign (list^.val, result);
            DiscardResults (list);
        END (*IF*);
        RETURN code;
    END GetFirstTXTrecord;

(************************************************************************)

END GetDNStxt.

