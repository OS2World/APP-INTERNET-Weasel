IMPLEMENTATION MODULE MXCheck;

        (********************************************************)
        (*                                                      *)
        (*      Extracting MX information from nameserver.      *)
        (*           This version discards everything           *)
        (*     except the MX responses and the IP addresses.    *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            03 July 1998                    *)
        (*  Last edited:        20 October 2016                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings;

FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, CARD16,
    (* proc *)  ADR, MOVE;

FROM Names IMPORT
    (* type *)  HostName, HostNameIndex;

FROM NameServer IMPORT
    (* const*)  C_IN, T_MX,
    (* proc *)  res_query;

FROM NetDB IMPORT
    (* type *)  HostEntPtr, AddressPointerArrayPointer,
    (* proc *)  gethostbyname, tcp_h_errno;

FROM Internet IMPORT
    (* proc *)  inet_addr;

FROM InetUtilities IMPORT
    (* proc *)  NameIsNumeric;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4;

FROM LowLevel IMPORT
    (* proc *)  IAND;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

TYPE
    (* A HostList is a list of host names, each with an associated      *)
    (* preference (smaller is better).  Each record also contains an    *)
    (* IP address, if we know it, or 0 if we don't know it.             *)

    HostList = POINTER TO RECORD
                              next: HostList;
                              pref: CARDINAL;
                              name: HostName;
                              address: CARDINAL;  (* network byte order *)
                          END (*RECORD*);

(************************************************************************)
(*                  INTERPRETING NAMESERVER RESPONSES                   *)
(************************************************************************)

PROCEDURE GetRaw (VAR (*IN*) buffer: ARRAY OF LOC;
                  VAR (*INOUT*) j: CARDINAL;
                  VAR (*OUT*) result: ARRAY OF LOC);

    (* Picks up a string of bytes from the buffer, without changing     *)
    (* the byte order.                                                  *)

    VAR bytecount: CARDINAL;

    BEGIN
        bytecount := HIGH(result) + 1;
        MOVE (ADR(buffer[j]), ADR(result), bytecount);
        INC (j, bytecount);
    END GetRaw;

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

PROCEDURE GetName (VAR (*IN*) buffer: ARRAY OF CARD8;
                          VAR (*INOUT*) j: CARDINAL;
                          VAR (*OUT*) Name: HostName): BOOLEAN;

    (* Picks up a domain name from buffer, using the compressed format  *)
    (* used in nameserver responses.  Returns FALSE if there's          *)
    (* a fault in the data in the buffer.                               *)

    VAR k, count, ptr: CARDINAL;  Partial: HostName;  valid: BOOLEAN;

    BEGIN
        Name := "";  valid := TRUE;
        REPEAT
            IF j > HIGH(buffer) THEN
                valid := FALSE;  count := 0;
            ELSE
                count := buffer[j];  INC(j);
                IF count >= 192 THEN

                    (* Sneaky space-saver *)

                    ptr := 256*(count-192);
                    valid := j <= HIGH(buffer);
                    IF valid THEN
                        INC (ptr, buffer[j]);  INC(j);
                        valid := GetName (buffer, ptr, Partial);
                        IF valid THEN
                            IF Name[0] <> CHR(0) THEN
                                Strings.Append ('.', Name);
                            END (*IF*);
                            Strings.Append (Partial, Name);
                        END (*IF*);
                    END (*IF*);
                    count := 0;

                ELSIF j+count-1 > HIGH(buffer) THEN
                    valid := FALSE;  count := 0;

                ELSIF count > 0 THEN

                    FOR k := 0 TO count-1 DO
                        Partial[k] := CHR(buffer[j]);  INC(j);
                    END (*FOR*);
                    Partial[count] := CHR(0);
                    IF Name[0] <> CHR(0) THEN
                        Strings.Append ('.', Name);
                    END (*IF*);
                    Strings.Append (Partial, Name);

                END (*IF*);
            END (*IF*);

        UNTIL count = 0;

        RETURN valid;

    END GetName;

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

PROCEDURE NameMatch (Name1, Name2: HostName): BOOLEAN;

    (* String equality, with case differences ignored. *)

    VAR j: HostNameIndex;

    BEGIN
        j := 0;
        LOOP
            IF Name1[j] = CHR(0) THEN RETURN Name2[j] = CHR(0)
            ELSIF Name2[j] = CHR(0) THEN RETURN FALSE
            ELSIF CAP(Name1[j]) <> CAP(Name2[j]) THEN RETURN FALSE
            ELSIF j = MAX(HostNameIndex) THEN RETURN TRUE
            ELSE INC(j)
            END (*IF*);
        END (*LOOP*);
    END NameMatch;

(************************************************************************)

PROCEDURE StoreAddress (Name: HostName;  IPAddr: CARDINAL;  list: HostList);

    (* If Name is already on the list, stores the IPAddr value in the   *)
    (* corresponding record.  Otherwise does nothing.                   *)

    VAR current: HostList;

    BEGIN
        current := list;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            IF NameMatch (Name, current^.name) THEN
                current^.address := IPAddr;
                EXIT (*LOOP*);
            ELSE
                current := current^.next;
            END (*IF*);
        END (*LOOP*);
    END StoreAddress;

(************************************************************************)

PROCEDURE InterpretResourceRecords (VAR (*IN*) buffer: ARRAY OF CARD8;
                                        VAR (*INOUT*) j: CARDINAL;
                                        count: CARDINAL;
                                        VAR (*INOUT*) result: HostList);

    (* Interpretation starts at buffer[j], and stops when we've         *)
    (* extracted all information for this section or when j >= length.  *)
    (* On exit buffer[j] is the first byte we haven't used.             *)

    VAR previous, current, this: HostList;
        k, QTYPE, length, IPAddr: CARDINAL;
        Name: HostName;  valid: BOOLEAN;

    BEGIN
        valid := TRUE;
        FOR k := 1 TO count DO
            valid := valid AND GetName (buffer, j, Name);
            IF valid THEN
                QTYPE := Get16 (buffer, j);
                INC (j, 6);

                IF QTYPE = 1 THEN

                    INC (j, 2);

                    (* Type 1 records map a name to an IP address. *)

                    GetRaw (buffer, j, IPAddr);
                    StoreAddress (Name, IPAddr, result);

                ELSIF QTYPE = 15 THEN

                    INC (j, 2);

                    (* Type 15 records (MX) are the only other  *)
                    (* records that interest us.                *)

                    NEW (this);
                    WITH this^ DO
                        pref := Get16 (buffer, j);
                        valid := GetName (buffer, j, name);
                        address := 0;
                    END (*WITH*);

                    IF valid THEN

                        (* Insert the new result into the list. *)

                        previous := NIL;  current := result;
                        LOOP
                            IF current = NIL THEN EXIT(*LOOP*) END (*IF*);
                            IF current^.pref > this^.pref THEN EXIT(*LOOP*) END(*IF*);
                            previous := current;  current := current^.next;
                        END (*LOOP*);
                        IF previous = NIL THEN
                            this^.next := current;
                            result := this;
                        ELSE
                            this^.next := previous^.next;
                            previous^.next := this;
                        END (*IF*);
                    ELSE
                        DISPOSE (this);
                    END (*IF*);

                ELSE
                    length := Get16 (buffer, j);
                    INC (j, length);
                END (*IF*);

            END (*IF*);

        END (*FOR*);

    END InterpretResourceRecords;

(************************************************************************)

PROCEDURE InterpretResponse (VAR (*IN*) buffer: ARRAY OF CARD8): HostList;

    (* Extracts the resource records out of a nameserver response. *)

    VAR j, QuestionCount, AnswerCount, AuthorityCount, AddCount: CARDINAL;
        flags: CARD16;  result: HostList;

    BEGIN
        j := 2;  result := NIL;

        (* Interpret the flags, ignoring the bits that don't interest us. *)

        flags := IAND (Get16 (buffer, j), 020FH);
        IF (flags MOD 16) = 0 THEN

            QuestionCount := Get16 (buffer, j);
            AnswerCount := Get16 (buffer, j);
            AuthorityCount := Get16 (buffer, j);
            AddCount := Get16 (buffer, j);

            (* Skip some more stuff we don't need. *)

            SkipQuestion (buffer, j, QuestionCount);
            InterpretResourceRecords (buffer, j, AnswerCount, result);
            InterpretResourceRecords (buffer, j, AuthorityCount, result);
            InterpretResourceRecords (buffer, j, AddCount, result);

        END (*IF*);

        RETURN result;

    END InterpretResponse;

(************************************************************************)

PROCEDURE Lookup (VAR (*IN*) host: ARRAY OF CHAR;
                  VAR (*OUT*) NoNameserver: BOOLEAN): HostList;

    (* Does an MX query for the given host, returns a list of results.  *)
    (* The list is empty if the resolver couldn't give us an answer.    *)
    (* In case of no answer, NoNameserver is TRUE if the nameserver     *)
    (* couldn't be reached, and FALSE if it could be reached but it     *)
    (* couldn't supply an answer.                                       *)

    CONST BufferSize = 4096;

    TYPE BufferSubscript = [0..BufferSize-1];

    VAR length: CARDINAL;
        buffer: ARRAY BufferSubscript OF CARD8;

    BEGIN
        length := res_query (host, C_IN, T_MX, buffer, BufferSize);
        IF length = MAX(CARDINAL) THEN
            NoNameserver := tcp_h_errno() = 2;
            RETURN NIL;
        ELSIF length >= BufferSize THEN
            NoNameserver := FALSE;
            RETURN NIL;
        ELSE
            NoNameserver := FALSE;
            RETURN InterpretResponse (buffer);
        END (*IF*);
    END Lookup;

(************************************************************************)

PROCEDURE DoMXLookup (VAR (*IN*) domain: HostName;
                      VAR (*OUT*) address: ARRAY OF CARDINAL): CARDINAL;

    (* Checks the MX records on the nameserver for the given domain,    *)
    (* and returns an array (ordered by preference) of IP addresses for *)
    (* hosts that will handle mail for that domain.  If the address     *)
    (* array is not filled, the end of the list is marked by a zero     *)
    (* address.  An empty list means that the nameserver lookup failed  *)
    (* to find an answer.  The function result is an error code:        *)
    (*      0    all OK, no error                                       *)
    (*      1    hard error, name not found                             *)
    (*      2    soft error, try again later                            *)
    (*      3    nameserver unreachable, try again later                *)
    (* The addresses are in network byte order.                         *)

    VAR list, next: HostList;  j, k, result: CARDINAL;
        HostInfo: HostEntPtr;  NoNameserver: BOOLEAN;
        p: AddressPointerArrayPointer;

    BEGIN

        (* Check first for the special case of a numeric address. *)

        IF NameIsNumeric(domain) THEN
            address[0] := inet_addr(domain);
            address[1] := 0;
            RETURN 0;
        END (*IF*);

        (* Normal case, do a nameserver lookup. *)

        result := 1;
        list := Lookup (domain, NoNameserver);
        IF list = NIL THEN
            NEW (list);
            WITH list^ DO
                name := domain;  address := 0;  next := NIL;
            END (*WITH*);
            IF NoNameserver THEN result := 3 END(*IF*);
        END (*IF*);

        (* Note how we treat the error status:                          *)
        (*   result = 0   we have at least one good result, so we'll    *)
        (*                report success even if not all the results    *)
        (*                are good, and we'll discard later results     *)
        (*                that aren't as good.                          *)
        (*   result = 1   we don't have any good result so far, so      *)
        (*                we'll settle for whatever we can get.         *)
        (*   result = 2   all results so far have resulted in a "soft   *)
        (*                error" status.                                *)
        (*   result = 3   we have no results, nameserver unreachable    *)

        j := 0;
        WHILE list <> NIL DO
            next := list^.next;
            IF j <= HIGH(address) THEN
                address[j] := list^.address;
                IF address[j] <> 0 THEN
                    result := 0;  INC (j);
                ELSE
                    HostInfo := gethostbyname (list^.name);
                    IF HostInfo = NIL THEN p := NIL
                    ELSE p := HostInfo^.h_addr_list
                    END (*IF*);
                    IF p = NIL THEN
                        IF (result = 1) AND (tcp_h_errno() = 2) THEN
                            result := 2;
                        END (*IF*);
                    ELSE
                        result := 0;  k := 0;
                        REPEAT
                            address[j] := p^[k]^;
                            INC(j);  INC(k);
                        UNTIL (j > HIGH(address)) OR (p^[k] = NIL);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            DISPOSE (list);
            list := next;
        END (*WHILE*);

        (* Terminate the list. *)

        IF j <= HIGH(address) THEN
            address[j] := 0;
        END (*IF*);

        RETURN result;

    END DoMXLookup;

(************************************************************************)

PROCEDURE MakeMask (nbits: CARDINAL): CARDINAL;

    (* Creates a 32-bit result where the top nbits are 1, and all       *)
    (* others are zero.  We return a byte-reversed version of this      *)
    (* mask, so that it can be used for comparing big-endian addresses. *)

    VAR k, mask, mask0: CARDINAL;

    BEGIN
        mask0 := 80000000H;  mask := 0;
        FOR k := 1 TO nbits DO
            INC (mask, mask0);
            mask0 := mask0 DIV 2;
        END (*FOR*);
        RETURN Swap4(mask);
    END MakeMask;

(************************************************************************)

PROCEDURE MXMatch (VAR (*IN*) domain: HostName;  addr, Nbits: CARDINAL;
                   skipMX: BOOLEAN;  VAR (*OUT*) error: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the top Nbits bits of the 32-bit IP address     *)
    (* addr matches one of the addresses of the MX hosts for the        *)
    (* specified domain.  To avoid DNS overload, we return with a FALSE *)
    (* result and with the error parameter set if we do not have a      *)
    (* match within the first 10 results.                               *)

    (* If skipMX is TRUE we skip the MX lookup and go directly for      *)
    (* the A records for domain.                                        *)

    VAR list, next: HostList;  k, mask, ChecksLeft: CARDINAL;
        HostInfo: HostEntPtr;  NoNameserver, StillSearching, match: BOOLEAN;
        p: AddressPointerArrayPointer;

    BEGIN
        ChecksLeft := 10;
        error := FALSE;  match := FALSE;
        IF Nbits = 0 THEN RETURN TRUE END(*IF*);
        mask := MakeMask (Nbits);
        addr := IAND(addr, mask);

        IF skipMX THEN

            (* Pretend that we've already done the MX lookup.*)

            NEW (list);
            WITH list^ DO
                next := NIL;
                pref := 1;
                name := domain;
                address := 0;
            END (*WITH*);

        ELSE

            (* Do the MX nameserver lookup. *)

            list := Lookup (domain, NoNameserver);

        END (*IF*);

        (* Extract the addresses and check each for a match. *)

        StillSearching := TRUE;
        WHILE list <> NIL DO
            next := list^.next;
            IF StillSearching THEN
                IF list^.address <> 0 THEN
                    match := IAND (list^.address, mask) = addr;
                    StillSearching := NOT match;
                ELSIF ChecksLeft = 0 THEN
                    error := TRUE;
                    StillSearching := FALSE;
                ELSE
                    HostInfo := gethostbyname (list^.name);
                    DEC (ChecksLeft);
                    IF HostInfo = NIL THEN p := NIL
                    ELSE p := HostInfo^.h_addr_list
                    END (*IF*);
                    IF p <> NIL THEN
                        k := 0;
                        REPEAT
                            match := IAND (p^[k]^, mask) = addr;
                            StillSearching := NOT match;
                            INC(k);
                        UNTIL match OR (p^[k] = NIL);
                    END (*IF*);
                END (*IF*);
            END (*IF*);
            DISPOSE (list);
            list := next;
        END (*WHILE*);

        RETURN match;

    END MXMatch;

(************************************************************************)

END MXCheck.

