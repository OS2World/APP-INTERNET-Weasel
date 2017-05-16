IMPLEMENTATION MODULE SPF;

        (********************************************************)
        (*                                                      *)
        (* Extracting and using SPF information from nameserver *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            03 October 2016                 *)
        (*  Last edited:        23 December 2016                *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (*       The 'exp' modifier is not yet handled.         *)
        (*                                                      *)
        (********************************************************)

(************************************************************************)
(*                                                                      *)
(*  RFC 7208 describes SPF records, which are realised as TXT records   *)
(*  in the nameserver data.                                             *)
(*                                                                      *)
(*  RFC 1035 specifies the format of a nameserver response.             *)
(*                                                                      *)
(************************************************************************)

(*
FROM FileOps IMPORT             (* only if debugging *)
                ChanId, OpenNewFile, CloseFile, FWriteChar, FWriteString,
                FWriteHexByte, FWriteZCard, FWriteLn, Exists, DeleteFile;
*)

IMPORT Strings;

FROM SYSTEM IMPORT
    (* type *)  CARD32;

FROM Names IMPORT
    (* type *)  DomainName, DomainNameIndex;

FROM Sockets IMPORT
    (* const*)  AF_INET;

FROM NetDB IMPORT
    (* type *)  HostEntPtr, AddressPointerArrayPointer,
    (* proc *)  tcp_h_errno, gethostbyaddr, gethostbyname;

FROM Inet2Misc IMPORT
    (* proc *)  NameIsNumeric, StringToIP, IPToString, HeadMatch;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM MXCheck IMPORT
    (* proc *)  MakeMask, MXMatch;

FROM STextIO IMPORT
    (* proc *)  WriteString, WriteLn;

FROM GetDNStxt IMPORT
    (* proc *)  GetFirstTXTrecord;

(************************************************************************)

TYPE
    CharSet = SET OF CHAR;

    (* An "ExtraData" record is just a way of passing around a few      *)
    (* parameters that are rarely changed and/or rarely used.           *)

    (* The fields are:                                                  *)
    (*      QueriesLeft the remaining number of DNS lookups we are      *)
    (*                      allowed to do before giving up.             *)
    (*      IPaddr      the client address we are trying to validate    *)
    (*                      (network byte order, i.e. bigendian)        *)
    (*      IPstring    the textual form of IPaddr                      *)
    (*      sender      sender address in MAIL FROM command             *)
    (*      senderuser,                                                 *)
    (*       senderdomain   the two components of sender                *)
    (*      HELOdomain  argument of the HELO or EHLO command            *)
    (*      validated   domain that was validated in the deprecated     *)
    (*                      ptr mechanism.                              *)

    ParamString = DomainName;

    ExtraData = RECORD
                    QueriesLeft: CARDINAL;
                    IPaddr: CARD32;
                    IPstring: ARRAY [0..15] OF CHAR;
                    sender, senderuser, senderdomain: ParamString;
                    HELOdomain, validated: DomainName;
                END (*RECORD*);

CONST
    Nul = CHR(0);
    Digits = CharSet {'0'..'9'};

VAR
    (* Set this to TRUE to get trace messages to screen. *)

    tracing: BOOLEAN;

(************************************************************************)
(*                           DEBUGGING STUFF                            *)
(************************************************************************)

(*
PROCEDURE DumpBuffer (VAR (*IN*) buffer: ARRAY OF CARD8;  count: CARDINAL);

    (* Writes out the first count bytes of the buffer. *)

    CONST fname = "dump.txt";

    VAR cid: ChanId;  j: CARDINAL;
        val: CARD8;

    BEGIN
        IF Exists (fname) THEN
            DeleteFile (fname);
        END (*IF*);
        cid := OpenNewFile (fname, FALSE);
        FOR j := 0 TO count-1 DO
            val := buffer[j];
            FWriteChar(cid, '[');
            FWriteZCard (cid, j, 4);
            FWriteString(cid, ']  ');
            FWriteHexByte (cid, val);
            FWriteString (cid, '  ');
            FWriteZCard (cid, val, 3);
            FWriteString (cid, '  ');
            IF val = 0 THEN
                FWriteString (cid, "Nul");
            ELSE
                FWriteChar (cid, CHR(val));
            END (*IF*);
            FWriteLn (cid);
        END (*FOR*);
        CloseFile (cid);
    END DumpBuffer;

(************************************************************************)

PROCEDURE WriteCard (N: CARDINAL);

    (* Write N in decimal, left justified. *)

    BEGIN
        IF N > 9 THEN
            WriteCard (N DIV 10);
            N := N MOD 10;
        END (*IF*);
        WriteChar (CHR(ORD('0') + N));
    END WriteCard;
*)

(************************************************************************)
(*                       MISCELLANEOUS UTILITIES                        *)
(************************************************************************)

PROCEDURE TailMatch (VAR (*IN*) str1, str2: ARRAY OF CHAR): BOOLEAN;

    (* We return TRUE if str2 is a trailing substring of str1, with the *)
    (* further condition that if the two strings are not equal then the *)
    (* first non-matching character must be '.'.  That is, we are       *)
    (* checking that str1 is a subdomain of str2.                       *)

    VAR j1, j2: CARDINAL;

    BEGIN
        j1 := LENGTH (str1);
        j2 := LENGTH (str2);
        IF j1 < j2 THEN RETURN FALSE END (*IF*);
        IF j1 > j2 THEN
            IF (str1[j1-j2-1] <> '.') THEN RETURN FALSE END (*IF*);
        END (*IF*);

        (* If we pass those preliminary tests, then do a    *)
        (* character-by-character comparison.               *)

        LOOP
            IF j2 = 0 THEN RETURN TRUE END(*IF*);
            DEC (j1);  DEC (j2);
            IF CAP(str1[j1]) <> CAP(str2[j2]) THEN RETURN FALSE END (*IF*);
        END (*LOOP*);

    END TailMatch;

(************************************************************************)

PROCEDURE GetNum (VAR (*INOUT*) str: ARRAY OF CHAR): CARDINAL;

    (* Decodes a decimal number at the head of str, then deletes the    *)
    (* characters we have used.                                         *)

    VAR N, result: CARDINAL;

    BEGIN
        N := 0;  result := 0;
        WHILE str[N] IN Digits DO
            result := 10*result + ORD(str[N]) - ORD('0');
            INC (N);
        END (*WHILE*);
        IF N > 0 THEN
            Strings.Delete (str, 0, N);
        END (*IF*);
        RETURN result;
    END GetNum;

(************************************************************************)
(*                       MORE NAMESERVER LOOKUPS                        *)
(************************************************************************)

PROCEDURE DomainExists (VAR (*IN*) domain: DomainName): BOOLEAN;

    (* Returns TRUE iff this domain has an A record. *)

    VAR hostlist: HostEntPtr;
        p: AddressPointerArrayPointer;

    BEGIN
        hostlist := gethostbyname (domain);
        p := hostlist^.h_addr_list;
        RETURN (p <> NIL) AND (p^[0]^ <> 0);
    END DomainExists;

(************************************************************************)

PROCEDURE PtrCheck (addr: CARD32;  domain: DomainName;
                              VAR (*OUT*) validated: DomainName): BOOLEAN;

    (* This does the complicated three-step check for the deprecated    *)
    (* "ptr" mechanism.                                                 *)

    VAR hostlist: HostEntPtr;
        p: AddressPointerArrayPointer;
        j, k, namecount: CARDINAL;
        name: ARRAY [0..9] OF DomainName;
        match: BOOLEAN;

    BEGIN
        match := FALSE;

        (* Step 1: the reverse DNS lookup. *)

        hostlist := gethostbyaddr (addr, SIZE(addr), AF_INET);
        IF hostlist <> NIL THEN
            k := 0;
            IF hostlist^.h_name <> NIL THEN
                Strings.Assign (hostlist^.h_name^, name[0]);
                k := 1;
            END (*IF*);

            (* Should I also pick up aliases? The standard doesn't say, *)
            (* but it does imply that more than one name might have to  *)
            (* be evaluated, so we'd better include the aliases.        *)

            IF hostlist^.h_aliases <> NIL THEN
                j := 0;
                REPEAT
                    IF hostlist^.h_aliases^[j] <> NIL THEN
                        Strings.Assign (hostlist^.h_aliases^[j]^, name[k]);
                        INC (j);
                    ELSE
                        name[k] := "";
                    END (*IF*);
                    INC (k);
                UNTIL (k > 9) OR (name[k-1][0] = Nul);
            END (*IF*);
            WHILE (k > 0) AND (name[k-1][0] = Nul) DO
                DEC (k);
            END (*IF*);
            namecount := k;

            (* Step 2: for each name, and see whether it has a  *)
            (* numeric address that matches addr.               *)

            IF namecount > 0 THEN
                FOR k := 0 TO namecount-1 DO
                    IF NOT match THEN
                        hostlist := gethostbyname (name[k]);
                        IF hostlist <> NIL THEN
                            p := hostlist^.h_addr_list;
                            IF p <> NIL THEN
                                j := 0;
                                WHILE (p^[j] <> NIL) AND (p^[j]^ <> 0) AND (p^[j]^ <> addr) DO
                                    INC (j);
                                END (*WHILE*);
                                IF (p^[j] <> NIL) AND (p^[j]^ <> 0) THEN

                                    validated := name[k];

                                    (* Step 3: for this validated name, check whether   *)
                                    (* name[k] is equal to the target name or is a      *)
                                    (* subdomain of it.  (RFC 7208 contradicts itself   *)
                                    (* as to which should be a subdomain of which, but  *)
                                    (* my tests on yahoo.com suggest that the less      *)
                                    (* plausible interpretation is the correct one.)    *)

                                    match := TailMatch (name[k], domain);

                                END (*IF*);
                            END (*IF*);
                        END (*IF*);
                    END (*IF*);
                END (*FOR*);
            END (*IF*);
        END (*IF*);

        RETURN match;

    END PtrCheck;

(************************************************************************)
(*                      EVALUATING THE SPF STRING                       *)
(************************************************************************)

PROCEDURE ReverseParts (VAR (*INOUT*) str: DomainName;  delims: CharSet);

    (* Treats 'str' as a sequence of substrings separated by            *)
    (* characters in 'delims'.  Reverses the order of those substrings, *)
    (* using '.' as the new delimiter.                                  *)

    VAR result, substring: DomainName;
        j, k: CARDINAL;
        first: BOOLEAN;

    BEGIN
        result := "";  first := TRUE;
        k := LENGTH(str);

        WHILE k > 0 DO

            j := k;
            REPEAT
                DEC (j);
            UNTIL (j = 0) OR (str[j] IN delims);

            IF NOT (str[j] IN delims) THEN

                (* Special case: we've reached the beginning of str. *)

                Strings.Extract (str, 0, k, substring);
            ELSE

                (* The substring we want is str[j+1..k-1] (which could  *)
                (* in the worst case be an empty string).  Note that    *)
                (* j < k, therefore k-j-1 >= 0.                         *)

                Strings.Extract (str, j+1, k-j-1, substring);
            END (*IF*);

            IF first THEN
                result := substring;
                first := FALSE;
            ELSE
                Strings.Append ('.', result);
                Strings.Append (substring, result);
            END (*IF*);

            k := j;

        END (*WHILE*);

        (* On loop exit, j = k = 0. Check for the special case where    *)
        (* str[0] is a delimiter.                                       *)

        IF str[0] IN delims THEN
            Strings.Append ('.', result);
        END (*IF*);

        str := result;

    END ReverseParts;

(************************************************************************)

PROCEDURE Trim (VAR (*INOUT*) str: DomainName;  N: CARDINAL;
                                                        delims: CharSet);

    (* Same delimiter rules as for ReverseParts.  We remove substrings  *)
    (* from the left, if necessary, so as to ensure that the result     *)
    (* contains no more than N substrings.  Assumption: N > 0.          *)

    VAR k: CARDINAL;

    BEGIN
        k := LENGTH (str);
        IF k = 0 THEN
            RETURN;     (* leave str unchanged *)
        END (*IF*);

        (* Back up through exactly N delimiters, unless we hit the  *)
        (* head of the string first.                                *)

        REPEAT
            DEC (k);
            IF str[k] IN delims THEN
                DEC (N);
            END (*IF*);
        UNTIL (N = 0) OR (k = 0);

        (* At this point either k = 0 or str[k] is a delimiter (or      *)
        (* both).  Correct the overshoot if any.                        *)

        IF str[k] IN delims THEN
            INC (k);
        END (*IF*);

        (* Finished counting.  Now we want to retain the string from    *)
        (* position k onwards.                                          *)

        IF k > 0 THEN
            Strings.Delete (str, 0, k);
        END (*IF*);

    END Trim;

(************************************************************************)

CONST Delimiters = CharSet {'.', '-', '+', ',', '/', '_', '='};

(************************************************************************)

PROCEDURE GetDomainString (VAR (*INOUT*) str: ARRAY OF CHAR;
                                    VAR (*IN*) domain: DomainName;
                                    VAR (*IN*) params: ExtraData;
                                    VAR (*OUT*) result: DomainName;
                                    VAR (*OUT*) error: BOOLEAN);

    (* Extracts a domain name from the leading characters of str, and   *)
    (* deletes those characters from str.  This includes macro          *)
    (* evaluation, as specified in Section 7 of RFC 7208.  The matched  *)
    (* part of str is deleted.                                          *)

    VAR j, k, val: CARDINAL;  ch: CHAR;  reverse: BOOLEAN;
        part: DomainName;
        delims: CharSet;

    BEGIN
        error := FALSE;
        ch := str[0];  j := 1;  k := 0;
        WHILE (ch <> Nul) AND (ch <> ' ') AND (k <= MAX(DomainNameIndex)) DO
            IF ch = '%' THEN
                ch := str[j];  INC(j);
                IF ch = '%' THEN
                    result[k] := ch;  INC(k);
                ELSIF ch = '_' THEN
                    result[k] := ' ';  INC(k);
                ELSIF ch = '-' THEN
                    result[k] := '%';  INC(k);
                    result[k] := '2';  INC(k);
                    result[k] := '0';  INC(k);
                ELSIF ch = '(' THEN
                    ch := str[j];  INC(j);
                    result[k] := Nul;
                    part := "";
                    CASE ch OF
                         's':   (* sender *)

                                Strings.Assign (params.sender, part);

                       | 'l':   (* local-part of sender *)

                                Strings.Assign (params.senderuser, part);

                       | 'o':   (* domain of sender *)

                                Strings.Assign (params.senderdomain, part);

                       | 'd':   (* domain being checked *)

                                Strings.Assign (domain, part);

                       | 'i':   (* IP address being checked *)

                                Strings.Assign (params.IPstring, part);

                       | 'p':   (* domain validated by 'ptr' option *)

                                Strings.Assign (params.validated, part);

                       | 'h':   (* the HELO/EHLO domain *)

                                Strings.Assign (params.HELOdomain, part);

                       | 'c':   (* used only in 'exp' text *)

                                part[0] := Nul;

                       | 'r':   (* used only in 'exp' text *)

                                part[0] := Nul;

                       | 't':   (* used only in 'exp' text *)

                                part[0] := Nul;

                       | 'v':   (* in-addr constant string *)

                                Strings.Assign ("in-addr", part);

                    ELSE
                        error := TRUE;
                    END (*CASE*);

                    IF NOT error THEN
                        ch := str[j];  INC(j);
                    END (*IF*);

                    (* The macro can be followed by a transformer, which is *)
                    (* an optional number followed by an optional 'r', and  *)
                    (* then apparently an optional number of delimiters.    *)
                    (* It looks as if multiple delimiters are allowed.      *)

                    val := 0;
                    WHILE ch IN Digits DO
                        val := 10*val + ORD(ch) - ORD('0');
                        ch := str[j];  INC(j);
                    END (*WHILE*);
                    reverse := ch = 'r';
                    IF reverse THEN
                        ch := str[j];  INC(j);
                    END (*IF*);
                    delims := CharSet{};
                    WHILE ch IN Delimiters DO
                        INCL (delims, ch);
                        ch := str[j];  INC(j);
                    END (*WHILE*);
                    IF delims = CharSet{} THEN
                        delims := CharSet {'.'};
                    END (*IF*);

                    (* One more syntax check: the closing ')'.  *)

                    error := ch <> ')';
                    IF NOT error THEN
                        ch := str[j];  INC(j);
                    END (*IF*);

                    (* Do the reversal and/or trimming, if required. *)

                    IF reverse THEN
                        ReverseParts (part, delims);
                    END (*IF*);
                    IF val > 0 THEN
                        Trim (part, val, delims);
                    END (*IF*);

                    (* At last, we have finished expanding the macro. *)

                    Strings.Append (part, result);
                    k := LENGTH (result);

                ELSE
                    error := TRUE;
                END (*IF*);
            ELSE
                (* No macro, just copy the character over. *)

                result[k] := ch;  INC(k);
                ch := str[j];  INC(j);
            END (*IF*);

        END (*WHILE*);

        IF k <= MAX(DomainNameIndex) THEN
            result[k] := Nul;
        END (*IF*);
        Strings.Delete (str, 0, j);

    END GetDomainString;

(************************************************************************)

PROCEDURE CheckHost (domain: DomainName;
                        VAR (*IN*) params: ExtraData;
                        VAR (*OUT*) SPFstring: ARRAY OF CHAR): SPFresult;
                                                            FORWARD;

(************************************************************************)

TYPE KwdType = ARRAY [0..9] OF ARRAY [0..8] OF CHAR;

CONST Keyword = KwdType {"all", "include", "a", "mx", "ptr", "ip4", "ip6", "exists",
                         "redirect", "exp"};

(************************************************************************)

PROCEDURE Evaluate (VAR (*IN*) domain: DomainName;
                          VAR (*INOUT*) params: ExtraData;
                                    SPFstring: ARRAY OF CHAR): SPFresult;

    (* Parses and evaluates the SPF string.  This string is             *)
    (* deliberately not a VAR parameter; we will modify it during       *)
    (* processing, but don't want to affect the caller's copy.          *)

    TYPE CharSet = SET OF CHAR;

    VAR qualifier: CHAR;
        match, error: BOOLEAN;
        k, val, pos, nbits, mask: CARDINAL;
        result: SPFresult;
        newdomain: DomainName;
        NewSPFstring: ARRAY [0..511] OF CHAR;

    BEGIN
        (* Delete the "v=spf1 " at the beginning. *)

        Strings.Delete (SPFstring, 0, 7);

        REPEAT
            (* Skip blanks. *)

            WHILE SPFstring[0] = ' ' DO
                Strings.Delete (SPFstring, 0, 1);
            END (*IF*);

            (* If we have exhausted the SPF string, return "neutral". *)

            qualifier := SPFstring[0];
            IF qualifier = Nul THEN
                RETURN SPF_neutral;

            (* Look for a qualifier.  If found, a mechanism must follow. *)

            ELSIF qualifier IN CharSet{"+", "-", "?", "~"} THEN
                Strings.Delete (SPFstring, 0, 1);
            ELSE
                qualifier := ' ';
            END (*IF*);

            (* Find the mechanism or modifier. *)

            k := 0;
            WHILE (k <= 9) AND NOT HeadMatch (SPFstring, Keyword[k]) DO
                INC (k);
            END (*WHILE*);
            IF (k <= 9) THEN
                Strings.Delete (SPFstring, 0, LENGTH(Keyword[k]));
            END (*IF*);

            IF (k < 8) AND (qualifier = ' ') THEN
                qualifier := '+';
            END (*IF*);

            match := FALSE;

            (* Common code for several of the mechanisms. *)

            CASE k OF
                   1,2,3,4,7:
                        IF params.QueriesLeft = 0 THEN
                            RETURN SPF_permerror;
                        ELSE
                            DEC (params.QueriesLeft);
                        END (*IF*);

                        IF SPFstring[0] = ':' THEN
                            Strings.Delete (SPFstring, 0, 1);
                            GetDomainString (SPFstring, domain, params, newdomain, error);
                            IF error THEN RETURN SPF_permerror END(*IF*);
                        ELSIF (k = 1) OR (k = 7) THEN
                            RETURN SPF_permerror;
                        ELSE
                            (* If there is no domain specification, we  *)
                            (* have to match using the currently        *)
                            (* specified domain.                        *)

                            Strings.Assign (domain, newdomain);
                        END (*IF*);
                |
                   8,9:
                        IF (k = 8) AND (params.QueriesLeft = 0) THEN
                            RETURN SPF_permerror;
                        ELSE
                            DEC (params.QueriesLeft);
                        END (*IF*);
                        IF SPFstring[0] = '=' THEN
                            Strings.Delete (SPFstring, 0, 1);
                            GetDomainString (SPFstring, domain, params, newdomain, error);
                            IF error THEN RETURN SPF_permerror END(*IF*);
                        ELSE
                            RETURN SPF_permerror;
                        END (*IF*);
                |
                  ELSE  (* do nothing *);

            END (*CASE*);

            (* Complete processing the mechanism or modifier. *)

            CASE k OF
                   0:                               (* all *)
                        match := TRUE;
                |
                   1:                               (* include *)
                        result := CheckHost (newdomain, params, NewSPFstring);
                        IF result = SPF_temperror THEN RETURN SPF_temperror;
                        ELSIF (result = SPF_none) OR (result = SPF_permerror) THEN
                            RETURN SPF_permerror;
                        ELSE match := (result = SPF_pass);
                        END (*IF*);
                |
                   2,3:                             (* a, mx *)

                        (* Get the number of bits to match. *)

                        IF SPFstring[0] = '/' THEN
                            Strings.Delete (SPFstring, 0, 1);
                            val := GetNum (SPFstring);
                        ELSE
                            val := 32;
                        END (*IF*);
                        match := MXMatch (newdomain, params.IPaddr, val, k=2, error);
                        IF error THEN
                            RETURN SPF_permerror;
                        END (*IF*);
                |
                   4:                               (* ptr *)

                            (* deprecated but still used by *)
                            (* AOL, by Yahoo, ..., maybe    *)
                            (* most of the spammer-friendly *)
                            (* domains.                     *)

                        match := PtrCheck (params.IPaddr, newdomain, params.validated);
                |
                   5:                               (* ip4 *)
                        IF SPFstring[0] = ':' THEN
                            Strings.Delete (SPFstring, 0, 1);
                        ELSE
                            RETURN SPF_permerror;
                        END (*IF*);
                        Strings.Assign (SPFstring, NewSPFstring);

                        pos := 0;  match := TRUE;

                        (* Rough syntax check for valid ip4 address: *)

                        FOR val := 1 TO 4 DO
                            IF match THEN
                                WHILE NewSPFstring[pos] IN Digits DO
                                    INC (pos);
                                END (*WHILE*);
                                IF val < 4 THEN
                                    match := NewSPFstring[pos] = '.';
                                    IF match THEN INC(pos) END(*IF*);
                                END (*IF*);
                            END (*IF*);
                        END (*FOR*);
                        IF match THEN
                            Strings.Delete (SPFstring, 0, pos);
                            NewSPFstring[pos] := Nul;
                            val := StringToIP (NewSPFstring);
                        ELSE
                            RETURN SPF_permerror;
                        END (*IF*);

                        (* So far, match only indicates syntactic correctness. *)

                        IF SPFstring[0] = '/' THEN
                            Strings.Delete (SPFstring, 0, 1);
                            nbits := GetNum (SPFstring);
                        ELSE
                            nbits := 32;
                        END (*IF*);
                        mask := MakeMask (nbits);
                        match := IAND(params.IPaddr,mask) = IAND(val,mask);

                |
                   6:                               (* ip6 *)
                        (* We can't handle ip6 addresses, so just skip this. *)
                        Strings.FindNext (' ', SPFstring, 0, match, pos);
                        IF match THEN
                            Strings.Delete (SPFstring, 0, pos);
                        ELSE
                            SPFstring[0] := Nul;
                        END (*IF*);
                        match := FALSE;
                |
                   7:           (* exists *)
                        match := DomainExists (newdomain);
                |
                   8:                               (* redirect *)
                        result := CheckHost (newdomain, params, NewSPFstring);

                        (* The basic difference between include and *)
                        (* redirect is that the result of redirect  *)
                        (* is the final result.                     *)

                        IF result = SPF_none THEN RETURN SPF_permerror;
                        ELSE RETURN result;
                        END (*IF*);
                |
                   9:                               (* exp *)
                        (* For now I parse this but don't implement it.*)
                        (* The parsing is in the 'common code' above.  *)
                |
                  ELSE  RETURN SPF_permerror;

            END (*CASE*);

        UNTIL match;

        CASE qualifier OF
            |  '+':   result := SPF_pass;
            |  '-':   result := SPF_fail;
            |  '~':   result := SPF_softfail;
            |  '?':   result := SPF_neutral;
            |  ELSE   result := SPF_none;
        END (*CASE*);

        RETURN result;

    END Evaluate;

(************************************************************************)

PROCEDURE CheckHost (domain: DomainName;
                        VAR (*IN*) params: ExtraData;
                        VAR (*OUT*) SPFstring: ARRAY OF CHAR): SPFresult;

    (* Does an SPF check to see whether params.IPaddr is a valid sender *)
    (* for the given domain.  The actual SPF string is returned in case *)
    (* the caller wants to record it.                                   *)

    VAR code: CARDINAL;
        result: SPFresult;

    BEGIN
        params.validated := "";
        SPFstring[0] := Nul;
        code := GetFirstTXTrecord (domain, "v=spf1 ", SPFstring);
        IF code = 0 THEN
            IF SPFstring[0] = Nul THEN result := SPF_none;
            ELSE
                IF tracing THEN
                    WriteString ("Working on SPF string: ");
                    WriteString (SPFstring);  WriteLn;
                END (*IF*);
                result := Evaluate (domain, params, SPFstring);
            END (*IF*);
        ELSIF code = 2 THEN
            result := SPF_temperror;
        ELSE
            (* No useful nameserver response. *)
            result := SPF_none;
        END (*IF*);
        RETURN result;
    END CheckHost;

(************************************************************************)
(*                       THE END-USER PROCEDURES                        *)
(************************************************************************)

PROCEDURE DoSPFLookup (ipaddr: CARDINAL;  VAR (*IN*) domain: DomainName;
                        VAR (*IN*) HELOname: DomainName;
                        user: ARRAY OF CHAR;
                        VAR (*IN*) userdomain: ARRAY OF CHAR;
                        VAR (*OUT*) SPFstring: ARRAY OF CHAR): SPFresult;

    (* Does an SPF check to see whether ipaddr is a valid sender for    *)
    (* the given domain.  The actual SPF string, which may be up to     *)
    (* 450 characters in length, is returned in case the caller wants   *)
    (* to record it.                                                    *)

    (* The IP address must be in BigEndian format.  The "sender" input  *)
    (* parameter should be an empty string when doing a HELO or EHLO    *)
    (* check, or otherwise the argument of the MAIL FROM command.       *)

    VAR result: SPFresult;
        params: ExtraData;

    BEGIN
        IF NameIsNumeric(domain) THEN

            (* No SPF check is allowed?  I can no longer find the       *)
            (* section in RFC 7208 that covers this case, although I    *)
            (* think I've seen it mentioned.                            *)

            result := SPF_none;

        ELSE
            params.QueriesLeft := 10;
            params.IPaddr := ipaddr;
            IPToString (ipaddr, FALSE, params.IPstring);
            params.HELOdomain := HELOname;

            (* Store the 'sender' details, putting in defaults if   *)
            (* some parts are missing.                              *)

            Strings.Assign (user, params.senderuser);
            Strings.Assign (userdomain, params.senderdomain);
            IF params.senderdomain[0] = Nul THEN
                Strings.Assign (domain, params.senderdomain);
            END (*IF*);
            IF params.senderuser[0] = Nul THEN
                Strings.Assign ("postmaster", params.senderuser);
            END (*IF*);
            Strings.Assign (params.senderuser, params.sender);
            Strings.Append ('@', params.sender);
            Strings.Append (params.senderdomain, params.sender);

            (* Finally, the actual check. *)

            result := CheckHost (domain, params, SPFstring);

        END (*IF*);
        RETURN result;

    END DoSPFLookup;

(************************************************************************)

PROCEDURE SPFresultToString (val: SPFresult;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Produces a human-readable version of val.  The result string     *)
    (* must be big enough to hold at least nine characters.             *)

    TYPE SPFstr = ARRAY SPFresult OF ARRAY [0..8] OF CHAR;
    CONST resultStr = SPFstr {"none", "neutral", "pass", "fail",
                          "softfail", "temperror", "permerror"};

    BEGIN
        Strings.Assign (resultStr[val], result);
    END SPFresultToString;

(************************************************************************)
(*          INTERNAL TESTS, USED ONLY DURING MODULE DEVELOPMENT         *)
(************************************************************************)

(*
PROCEDURE TestTrim (str: DomainName;  N: CARDINAL;  stoppers: CharSet);

    BEGIN
        WriteString ("Trim(");  WriteCard(N);  WriteString (")(");
        WriteString (str);
        WriteString (") is ");
        Trim (str, N, stoppers);
        WriteString (str);  WriteLn;
    END TestTrim;
*)

(************************************************************************)

PROCEDURE InternalTests;

    (* Internal testing of some procedures. *)

    CONST S = CharSet{'.'};

        (* Treatment of different delimiters here is not an issue, so   *)
        (* we might as well stick with the simplest delimiter set.      *)

    BEGIN
        (*
        TestTrim ("a..b", 2, S);
        TestTrim ("abc.def.ghi", 120, S);
        TestTrim ("abc.def.ghi", 4, S);
        TestTrim ("abc.def.ghi", 3, S);
        TestTrim (".abc.def.ghi", 3, S);
        TestTrim ("abc.def.ghi", 2, S);
        TestTrim ("def..ghi", 2, S);
        TestTrim ("def.ghi.", 2, S);
        *)
    END InternalTests;

(************************************************************************)

BEGIN
    tracing := FALSE;
    InternalTests;
END SPF.

