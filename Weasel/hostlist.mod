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

IMPLEMENTATION MODULE HostLists;

        (********************************************************)
        (*                                                      *)
        (*           Lists of host names and addresses          *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            1 September 2002                *)
        (*  Last edited:        23 July 2012                    *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT OS2, Strings, INIData;

FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8,
    (* proc *)  CAST;

FROM INIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  OpenINIFile, CloseINIFile, GetStringList, NextString, CloseStringList;

FROM Names IMPORT
    (* type *)  FilenameString, HostName, HostNameIndex;

FROM WildCard IMPORT
    (* proc *)  WildMatch;

FROM Sockets IMPORT
    (* const*)  AF_INET,
    (* proc *)  sock_init, gethostid;

FROM Internet IMPORT
    (* proc *)  inet_addr;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostname, gethostbyname, gethostbyaddr;

FROM LogCtx IMPORT
    (* var  *)  WCtx;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  CreateLogID, DiscardLogID, LogTransaction;

FROM Inet2Misc IMPORT
    (* proc *)  Swap4, NameIsNumeric, IPToString;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM LowLevel IMPORT
    (* proc *)  INOTB, IORB, IANDB;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* List identifier for log messages. *)

    LabelString = ARRAY [0..127] OF CHAR;    (* was [0..9] *)

    (* Linked list of textual host names. *)

    StringRecordPtr = POINTER TO StringRecord;
    StringRecord = RECORD
                       next: StringRecordPtr;
                       expand: BOOLEAN;
                       textaddr: HostName;
                   END (*RECORD*);

    StringList = StringRecordPtr;

    (* Linked list of numeric IP addresses.  If range is TRUE then an   *)
    (* entry represents the range [numaddr..numaddr2], inclusive.  If   *)
    (* range is FALSE then numaddr2 is ignored, and we have a single    *)
    (* numeric address numaddr.                                         *)

    NumericRecordPtr = POINTER TO NumericRecord;
    NumericRecord = RECORD
                        next: NumericRecordPtr;
                        range: BOOLEAN;
                        expand: BOOLEAN;
                        numaddr: CARDINAL;         (* network byte order *)
                        numaddr2: CARDINAL;        (* network byte order *)
                    END (*RECORD*);

    NumericList = NumericRecordPtr;

    (* A hostlist has both textual and numeric sublists. *)

    HostList = POINTER TO HLrecord;
    HLrecord = RECORD
                   access: Lock;
                   IsLocal: BOOLEAN;
                   LogLabel: LabelString;
                   textnames: StringList;
                   numnames: NumericList;
               END (*RECORD*);

(************************************************************************)

VAR
    LogID: TransactionLogID;

(************************************************************************)
(*                           SCREEN OUTPUT                              *)
(************************************************************************)

(*
PROCEDURE WriteIPAddr (addr: CARDINAL);

    (* Writes an IP address to the screen. *)

    VAR IPBuffer: ARRAY [0..16] OF CHAR;

    BEGIN
        IPToString (addr, TRUE, IPBuffer);
        WriteString (IPBuffer);
    END WriteIPAddr;

(************************************************************************)

PROCEDURE DumpList (j: HostCategory);

    (* Writes the list to the screen. *)

    VAR current: HostRecordPtr;

    BEGIN
        WriteString ("Dumping ");  WriteString (INILabel[j]);
        WriteLn;
        current := MasterList[j];
        WHILE current <> NIL DO
            IF current^.numeric THEN
                WriteIPAddr (current^.numaddr);
            END (*IF*);
            IF current^.text THEN
                WriteString (current^.textaddr);
            END (*IF*);
            WriteLn;
            current := current^.next;
        END (*WHILE*);
    END DumpList;
*)

(************************************************************************)
(*                RETURNING ALL THE NUMERIC ADDRESSES                   *)
(************************************************************************)

PROCEDURE FindAllAddresses (list: HostList;
                             VAR (*OUT*) address: ARRAY OF CARDINAL);

    (* Sets address to contain all the numeric addresses in list.  The  *)
    (* terminator is a zero address.                                    *)

    VAR j, val, limit: CARDINAL;  p: NumericRecordPtr;

    BEGIN
        Obtain (list^.access);
        j := 0;  p := list^.numnames;
        WHILE (p <> NIL) AND (j <= HIGH(address)) DO
            val := p^.numaddr;
            address[j] := val;
            IF p^.range THEN
                limit := Swap4 (p^.numaddr2);
                val := Swap4 (val);
                WHILE (j < HIGH(address)) AND (val <= limit) DO
                    INC (j);
                    val := Swap4 (val+1);
                    address[j] := val;
                    val := Swap4 (val);
                END (*WHILE*);
            END (*IF*);
            p := p^.next;
            INC (j);
        END (*WHILE*);
        Release (list^.access);
        IF j <= HIGH(address) THEN
            address[j] := 0;
        END (*IF*);
    END FindAllAddresses;

(************************************************************************)
(*                       SEARCHING A HOSTLIST                           *)
(************************************************************************)

PROCEDURE MatchByName (L: StringList;  VAR (*IN*) host: HostName;
                  ExtraLogging: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Returns TRUE iff host is in list L.      *)
    (* We assume we have exclusive access to L. *)

    VAR found: BOOLEAN;
        message: FilenameString;

    BEGIN
        found := FALSE;
        WHILE (L <> NIL) AND NOT found DO

            IF ExtraLogging THEN
                message := "Comparing ";
                Strings.Append (host, message);
                Strings.Append (" with ", message);
                Strings.Append (L^.textaddr, message);
            END (*IF*);

            found := WildMatch (host, L^.textaddr);
            IF found THEN

                IF ExtraLogging THEN
                    Strings.Append (" (match)", message);
                    LogTransaction (LogID, message);
                END (*IF*);

            ELSE

                IF ExtraLogging THEN
                    Strings.Append (" (no match)", message);
                    LogTransaction (LogID, message);
                END (*IF*);

                L := L^.next;
            END (*IF*);
        END (*WHILE*);
        RETURN found;
    END MatchByName;

(************************************************************************)

PROCEDURE MatchNumeric (L: NumericList;  IPAddress: CARDINAL): BOOLEAN;

    (* Returns TRUE if the address IPAddress is in list L. *)
    (* IPAddress is in network byte order.                 *)
    (* We assume we have exclusive access to L.            *)

    VAR found: BOOLEAN;  temp: CARDINAL;

    BEGIN
        found := FALSE;
        WHILE (L <> NIL) AND NOT found DO
            IF L^.range THEN
                temp := Swap4 (IPAddress);
                found := (temp >= Swap4(L^.numaddr))
                         AND (temp <= Swap4(L^.numaddr2))
            ELSE
                found := L^.numaddr = IPAddress;
            END (*IF*);
            IF NOT found THEN
                L := L^.next;
            END (*IF*);
        END (*WHILE*);
        RETURN found;
    END MatchNumeric;

(************************************************************************)

(*
PROCEDURE MatchEither (list: HostList;  IPAddress: CARDINAL;
                                      VAR (*IN*) host: HostName): BOOLEAN;

    (* Returns TRUE if either the address IPAddress or the name host    *)
    (* is in the list.  IPAddress is in network byte order.             *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (list^.access);
        result := MatchNumeric (list^.numnames, IPAddress)
                      OR MatchByName (list^.textnames, host);
        Release (list^.access);
        RETURN result;
    END MatchEither;
*)

(************************************************************************)

PROCEDURE MatchAnAddress (list: HostList;  IPAddress: CARDINAL): BOOLEAN;

    (* Returns TRUE if either the address IPAddress, or the hostname    *)
    (* obtained by rDNS lookup is in the list.  In the second case we   *)
    (* look up only the primary name, i.e. we don't bother with things  *)
    (* like aliases.  IPAddress is in network byte order.               *)

    VAR result: BOOLEAN;
        NameDetails: HostEntPtr;
        name: HostName;

    BEGIN
        Obtain (list^.access);
        result := MatchNumeric (list^.numnames, IPAddress);
        IF (NOT result) AND (list^.textnames <> NIL) THEN
            NameDetails := gethostbyaddr (IPAddress, SIZE(CARDINAL), AF_INET);
            result := (NameDetails <> NIL) AND (NameDetails^.h_name <> NIL);
            IF result THEN
                Strings.Assign (NameDetails^.h_name^, name);
                result := MatchByName (list^.textnames, name, FALSE, LogID);
            END (*IF*);
        END (*IF*);
        Release (list^.access);
        RETURN result;
    END MatchAnAddress;

(************************************************************************)

PROCEDURE MatchHostName (list: HostList;  VAR (*IN*) name: HostName;
                  ExtraLogging: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Returns TRUE if name matches a name in the list, either as a     *)
    (* text string or as a numeric x.x.x.x name.                        *)

    VAR result: BOOLEAN;

    BEGIN
        Obtain (list^.access);
        IF NameIsNumeric (name) THEN
            result := MatchNumeric (list^.numnames, inet_addr(name))
        ELSE result := MatchByName (list^.textnames, name, ExtraLogging, LogID);
        END (*IF*);
        Release (list^.access);
        RETURN result;
    END MatchHostName;

(************************************************************************)
(*                 PUTTING THE DATA INTO A HOSTLIST                     *)
(************************************************************************)

PROCEDURE CreateHostList (LogName: ARRAY OF CHAR;  local: BOOLEAN): HostList;

    (* Creates an empty list. *)

    VAR result: HostList;

    BEGIN
        NEW (result);
        WITH result^ DO
            CreateLock (access);
            IsLocal := local;
            Strings.Assign (LogName, LogLabel);
            textnames := NIL;
            numnames := NIL;
        END (*WITH*);
        RETURN result;
    END CreateHostList;

(************************************************************************)

PROCEDURE FlushHostList (VAR (*INOUT*) list: HostList);

    (* Discards the existing contents of the list.   We assume we       *)
    (* already have exclusive access to the list.                       *)

    VAR p, p1: StringRecordPtr;  q, q1: NumericRecordPtr;

    BEGIN
        p := list^.textnames;
        WHILE p <> NIL DO
            p1 := p^.next;
            DEALLOCATE (p, SIZE(StringRecord));
            p := p1;
        END (*WHILE*);
        list^.textnames := NIL;

        q := list^.numnames;
        WHILE p <> NIL DO
            q1 := q^.next;
            DEALLOCATE (q, SIZE(NumericRecord));
            q := q1;
        END (*WHILE*);
        list^.numnames := NIL;

    END FlushHostList;

(************************************************************************)

PROCEDURE DestroyHostList (VAR (*INOUT*) list: HostList);

    (* Empties the list, then discards it. *)

    BEGIN
        Obtain (list^.access);
        FlushHostList (list);
        Release (list^.access);
        DestroyLock (list^.access);
        DEALLOCATE (list, SIZE(HLrecord));
    END DestroyHostList;

(************************************************************************)

PROCEDURE Wild (name: HostName): BOOLEAN;

    (* Returns TRUE iff name contains wildcard characters. *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;
        LOOP
            IF name[j] = Nul THEN RETURN FALSE
            ELSIF name[j] = '*' THEN RETURN TRUE
            ELSIF name[j] = '?' THEN RETURN TRUE
            ELSIF j = MAX(HostNameIndex) THEN RETURN FALSE
            ELSE INC (j);
            END (*IF*);
        END (*LOOP*);
    END Wild;

(************************************************************************)

PROCEDURE AddNumericRecord (list: HostList;  low, high: CARDINAL;
                                  AllowExpansions, ConfirmToLog: BOOLEAN);

    (* Adds a new IP address record to the list, unless it duplicates   *)
    (* an entry that is already present.  In the present version I      *)
    (* don't eliminate all possible redundancies, only the more likely  *)
    (* ones.  A missed redundancy is not fatal.                         *)
    (* Assumption: we already have exclusive access to the list.        *)

    VAR IPAddrBuff: ARRAY [0..20] OF CHAR;
        LogMessage: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE ContinueLogMessage;

        (* Logs a message about low..high, assuming that the caller has *)
        (* already put the "Adding " or "Skipping " message into the    *)
        (* LogMessage buffer and that the low address has already been  *)
        (* converted to text in IPAddrBuff.                             *)

        BEGIN
            Strings.Append (list^.LogLabel, LogMessage);
            Strings.Append (" address ", LogMessage);
            IF high <> low THEN
                Strings.Append ("range ", LogMessage);
                Strings.Append (IPAddrBuff, LogMessage);
                Strings.Append (" to ", LogMessage);
                IPToString (high, TRUE, IPAddrBuff);
            END (*IF*);
            Strings.Append (IPAddrBuff, LogMessage);
            LogTransaction (LogID, LogMessage);
        END ContinueLogMessage;

    (********************************************************************)

    PROCEDURE WriteAddMessage;

        (* Logs an "adding address (range)" message.  We assume that    *)
        (* the low address has already been converted in IPAddrBuff.    *)

        BEGIN
            Strings.Assign ("Adding ", LogMessage);
            ContinueLogMessage;
        END WriteAddMessage;

    (********************************************************************)

    PROCEDURE WriteSkipMessage;

        (* Like WriteAddMessage, but for skipping a duplicate (range).  *)

        BEGIN
            Strings.Assign ("Skipping duplicate ", LogMessage);
            ContinueLogMessage;
        END WriteSkipMessage;

    (********************************************************************)

    VAR previous, current, p: NumericRecordPtr;
        temp1, temp2: CARDINAL;

    BEGIN
        previous := NIL;  current := list^.numnames;
        IPToString (low, TRUE, IPAddrBuff);
        LOOP
            IF current = NIL THEN

                (* We have reached the end of the list without finding  *)
                (* a match, so add the new entry here.                  *)

                NEW (p);
                p^.next := NIL;
                p^.range := low <> high;
                p^.expand := AllowExpansions AND NOT p^.range;
                p^.numaddr := low;
                p^.numaddr2 := high;
                IF previous = NIL THEN
                    list^.numnames := p;
                ELSE
                    previous^.next := p
                END (*IF*);
                IF ConfirmToLog THEN
                    WriteAddMessage;
                END (*IF*);
                EXIT (*LOOP*);

            ELSIF current^.range THEN

                temp1 := Swap4 (low);
                temp2 := Swap4 (high);
                IF (temp1 >= Swap4(current^.numaddr))
                             AND (temp2 <= Swap4(current^.numaddr2)) THEN
                    IF ConfirmToLog THEN
                        WriteSkipMessage;
                    END (*IF*);
                    EXIT (*LOOP*);
                ELSE
                    previous := current;  current := current^.next;
                END (*IF*);

            ELSIF (low = high) AND (current^.numaddr = low) THEN

                IF ConfirmToLog THEN
                    WriteSkipMessage;
                END (*IF*);
                EXIT (*LOOP*);

            ELSE

                previous := current;  current := current^.next;

            END (*IF*);

        END (*LOOP*);

    END AddNumericRecord;

(************************************************************************)

PROCEDURE AddTextRecord (list: HostList;  newname: HostName;
                                  AllowExpansions, ConfirmToLog: BOOLEAN);

    (* Adds a new text record to the list, unless it duplicates an      *)
    (* entry that is already present.                                   *)
    (* Assumption: we already have exclusive access to the list.        *)

    VAR LogMessage: ARRAY [0..511] OF CHAR;

    (********************************************************************)

    PROCEDURE WriteLogMessage (skip: BOOLEAN);

        (* Logs an "adding name" or "skipping name" message, depending  *)
        (* on the 'skip' parameter.                                     *)

        BEGIN
            IF skip THEN
                Strings.Assign ("Skipping duplicate ", LogMessage);
            ELSE
                Strings.Assign ("Adding ", LogMessage);
            END (*IF*);
            Strings.Append (list^.LogLabel, LogMessage);
            Strings.Append (" name ", LogMessage);
            Strings.Append (newname, LogMessage);
            LogTransaction (LogID, LogMessage);
        END WriteLogMessage;

    (********************************************************************)

    VAR previous, current, p: StringRecordPtr;

    BEGIN
        previous := NIL;  current := list^.textnames;
        LOOP
            IF current = NIL THEN

                (* We have reached the end of the list without finding  *)
                (* a match, so add the new entry here.                  *)

                NEW (p);
                p^.next := NIL;
                p^.expand := AllowExpansions AND NOT Wild(newname);
                p^.textaddr := newname;
                IF previous = NIL THEN
                    list^.textnames := p;
                ELSE
                    previous^.next := p;
                END (*IF*);
                IF ConfirmToLog THEN
                    WriteLogMessage(FALSE);
                END (*IF*);
                EXIT (*LOOP*);

            ELSIF WildMatch (newname, current^.textaddr) THEN

                IF ConfirmToLog THEN
                    WriteLogMessage(TRUE);
                END (*IF*);
                EXIT (*LOOP*);

            ELSE

                previous := current;  current := current^.next;

            END (*IF*);

        END (*LOOP*);

    END AddTextRecord;

(************************************************************************)

PROCEDURE ExtractHostInfo (list: HostList;  HostInfo: HostEntPtr;
                                            ConfirmToLog: BOOLEAN);

    (* Extracts all the names and addresses from HostInfo, and adds     *)
    (* to the list unless they duplicate something already on the list. *)
    (* Assumption: we already have exclusive access to the list.        *)
    (* Assumption: HostInfo <> NIL.                                     *)

    VAR newname: HostName;  j, k: CARDINAL;
        found: BOOLEAN;

    BEGIN
        (* Official name *)

        IF HostInfo^.h_name <> NIL THEN
            Strings.Assign (HostInfo^.h_name^, newname);
            IF newname[0] <> Nul THEN

                AddTextRecord (list, newname, FALSE, ConfirmToLog);

                IF list^.IsLocal THEN

                    (* Allow abbreviated form for a local name. *)

                    Strings.FindNext ('.', newname, 0, found, k);
                    IF found THEN
                        newname[k] := Nul;
                        AddTextRecord (list, newname, FALSE, ConfirmToLog);
                    END (*IF*);

                END (*IF*);
            END (*IF*);
        END (*IF*);

        (* Aliases *)

        IF HostInfo^.h_aliases <> NIL THEN
            j := 0;
            WHILE HostInfo^.h_aliases^[j] <> NIL DO
                Strings.Assign (HostInfo^.h_aliases^[j]^, newname);
                IF newname[0] <> Nul THEN
                    AddTextRecord (list, newname, FALSE, ConfirmToLog);
                    IF list^.IsLocal THEN

                        (* Allow abbreviated form for a local name. *)

                        Strings.FindNext ('.', newname, 0, found, k);
                        IF found THEN
                            newname[k] := Nul;
                            AddTextRecord (list, newname, FALSE, ConfirmToLog);
                        END (*IF*);

                    END (*IF*);
                END (*IF*);


                INC (j);

            END (*IF*);
        END (*IF*);

        (* IP addresses *)

        IF HostInfo^.h_addr_list <> NIL THEN
            j := 0;
            WHILE HostInfo^.h_addr_list^[j] <> NIL DO
                k := HostInfo^.h_addr_list^[j]^;
                AddNumericRecord (list, k, k, FALSE, ConfirmToLog);
                INC (j);
            END (*WHILE*);
        END (*IF*);

    END ExtractHostInfo;

(************************************************************************)

PROCEDURE ExpandAllAliases (list: HostList;  ExpandNumeric, ConfirmToLog: BOOLEAN);

    (* Looks for aliases, other addresses, etc. for all expandable      *)
    (* entries in the list, and adds the new information to the tail of *)
    (* the list.                                                        *)
    (* Assumption: we already have exclusive access to the list.        *)

    VAR p1: StringRecordPtr;  hostname: HostName;  IPaddr: CARDINAL;
        p2: NumericRecordPtr;
        HostInfo: HostEntPtr;
        logmessage: ARRAY [0..255] OF CHAR;
        IPBuffer: ARRAY [0..16] OF CHAR;

    BEGIN
        (* Start by working through the text list. *)

        p1 := list^.textnames;
        WHILE p1 <> NIL DO
            hostname := p1^.textaddr;
            IF p1^.expand THEN
                IF ConfirmToLog THEN
                    Strings.Assign ("Expanding ", logmessage);
                    Strings.Append (list^.LogLabel, logmessage);
                    Strings.Append (" name ", logmessage);
                    Strings.Append (hostname, logmessage);
                    LogTransaction (LogID, logmessage);
                END (*IF*);
                HostInfo := gethostbyname (hostname);
                IF HostInfo <> NIL THEN
                    ExtractHostInfo (list, HostInfo, ConfirmToLog);
                END (*IF*);
                p1^.expand := FALSE;
            END (*IF*);
            p1 := p1^.next;
        END (*WHILE*);

        IF ExpandNumeric THEN

            (* Now the same for the numeric list. *)

            p2 := list^.numnames;
            WHILE p2 <> NIL DO
                IF p2^.expand THEN
                    IPaddr := p2^.numaddr;
                    IF ConfirmToLog THEN
                        Strings.Assign ("Expanding ", logmessage);
                        Strings.Append (list^.LogLabel, logmessage);
                        Strings.Append (" IP address ", logmessage);
                        IPToString (IPaddr, TRUE, IPBuffer);
                        Strings.Append (IPBuffer, logmessage);
                        LogTransaction (LogID, logmessage);
                    END (*IF*);
                    HostInfo := gethostbyaddr (IPaddr, SIZE(CARDINAL), AF_INET);
                    IF HostInfo <> NIL THEN
                        ExtractHostInfo (list, HostInfo, ConfirmToLog);
                    END (*IF*);
                    p2^.expand := FALSE;
                END (*IF*);
                p2 := p2^.next;
            END (*WHILE*);

        END (*IF*);

    END ExpandAllAliases;

(************************************************************************)

PROCEDURE DecodeRange (VAR (*IN*) Name: HostName;
                        VAR (*OUT*) First, Second: ARRAY OF LOC): BOOLEAN;

    (* Returns TRUE iff Name is a numeric IP address or a range of      *)
    (* the form [a.b.c.d-e], in which case First and Second are set to  *)
    (* the lower and upper limits of the range, in host byte order.     *)
    (* (For a single address, First and Second get the same value.)     *)

    VAR j: CARDINAL;

    (********************************************************************)

    PROCEDURE ConvertByte (VAR (*OUT*) val: LOC): BOOLEAN;

        (* Converts decimal string starting at Name[j] to numeric in    *)
        (* val, updates j.  Returns FALSE if number not found.          *)

        TYPE CharSet = SET OF CHAR;
        CONST Digits = CharSet {'0'..'9'};
        VAR result: CARD8;

        BEGIN
            IF NOT (Name[j] IN Digits) THEN
                RETURN FALSE;
            END (*IF*);
            result := 0;
            REPEAT
                result := 10*result + (ORD(Name[j]) - ORD('0'));
                INC (j);
            UNTIL (j > MAX(HostNameIndex)) OR NOT (Name[j] IN Digits);
            val := CAST(LOC,result);
            RETURN TRUE;
        EXCEPT
            RETURN FALSE;
        END ConvertByte;

    (********************************************************************)

    VAR k: CARDINAL;  count, mask: CARD8;  success, HaveStar: BOOLEAN;

    BEGIN
        j := 0;  HaveStar := FALSE;
        IF Name[0] = '[' THEN j := 1 END (*IF*);

        (* Convert a sequence of decimal numbers separated by '.' *)

        success := ConvertByte(First[0]);
        Second[0] := First[0];
        k := 1;
        WHILE success AND (k <= 3) AND (Name[j] = '.') DO
            INC (j);
            IF Name[j] = '*' THEN
                First[k] := CAST(LOC,VAL(CARD8,0));
                Second[k] := CAST(LOC,VAL(CARD8,MAX(CARD8)));
                HaveStar := TRUE;
                INC (j);
            ELSIF HaveStar THEN
                success := FALSE;
            ELSE
                success := ConvertByte(First[k]);
                Second[k] := First[k];
            END (*IF*);
            INC (k);
        END (*WHILE*);

        (* If we have a '-', get one more number. *)

        IF success AND (k <= 4) AND (Name[j] = '-') THEN
            INC (j);
            success := ConvertByte(Second[k-1]);
        END (*IF*);

        (* Pad out remaining bytes of the result. *)

        IF success THEN
            WHILE k <= 3 DO
                First[k] := CAST(LOC,VAL(CARD8,0));
                Second[k] := CAST(LOC,VAL(CARD8,MAX(CARD8)));
                INC(k);
            END (*WHILE*);
        END (*IF*);

        (* If we have a '/', get one more number (range 1-32), *)
        (* giving the number of "don't care" bits at the end   *)
        (* of the address.                                     *)

        count := 0;
        IF success AND (Name[j] = '/') THEN
            INC (j);
            success := (First[3] = Second[3]) AND ConvertByte(count);
            IF success AND (count > 0) THEN
                IF count >= 32 THEN
                    First[0] := CAST(LOC,VAL(CARD8,0));
                    Second[0] := CAST(LOC,VAL(CARD8,MAX(CARD8)));
                    count := 24;
                END (*IF*);
                k := 3;
                WHILE count >= 8 DO
                    First[k] := CAST(LOC,VAL(CARD8,0));
                    Second[k] := CAST(LOC,VAL(CARD8,MAX(CARD8)));
                    DEC(k);
                    DEC (count, 8);
                END (*WHILE*);

                (* Finally we have count < 8, so we're down to  *)
                (* masking the bits in the current byte.        *)

                mask := 0;
                WHILE count > 0 DO
                    mask := 2*mask + 1;
                    DEC (count);
                END (*WHILE*);
                IF mask <> 0 THEN
                    count := CAST (CARD8, First[k]);
                    First[k] := CAST(LOC, IANDB(count, INOTB(mask)));
                    Second[k] := CAST(LOC, IORB (count, mask));
                END (*IF*);

            END (*IF*);
        END (*IF*);

        (* Check that the string is correctly terminated. *)

        IF Name[0] = '[' THEN
            success := success AND (Name[j] = ']');
            INC (j);
        END (*IF*);
        success := success AND ((j >= MAX(HostNameIndex)) OR (Name[j] = CHR(0)));

        RETURN success;

    END DecodeRange;

(************************************************************************)

PROCEDURE UpdateList (VAR (*IN*) INIFile: FilenameString;
                      VAR (*IN*) app, key: ARRAY OF CHAR;
                      UseTNI: BOOLEAN;
                      VAR (*OUT*) list: HostList;
                      ExpandNumeric, ConfirmToLog: BOOLEAN);

    (* Adds data from the INI file to the list.  *)

    (********************************************************************)

    PROCEDURE ProcessOneName (ThisName: HostName);

        VAR low, high: CARDINAL;

        BEGIN
            (* If the new name is not empty, add it to the master list. *)

            IF ThisName[0] <> Nul THEN

                IF DecodeRange (ThisName, low, high) THEN

                    (* Number or numeric range. *)

                    AddNumericRecord (list, low, high, ExpandNumeric,
                                                          ConfirmToLog);

                ELSE

                    (* Textual name, possibly with wildcards. *)

                    IF ThisName[0] = '.' THEN
                        (* Old-style wildcard. *)
                        ThisName[0] := '*';
                    END (*IF*);
                    AddTextRecord (list, ThisName, TRUE, ConfirmToLog);

                END (*IF*);

            END (*IF*);

        END ProcessOneName;

    (********************************************************************)

    VAR hini: INIData.HINI;
        state: StringReadState;  ThisName: HostName;

    BEGIN          (* Body of procedure UpdateList *)

        hini := OpenINIFile (INIFile, UseTNI);
        IF INIData.INIValid (hini) THEN
            GetStringList (hini, app, key, state);
            LOOP
                NextString (state, ThisName);
                IF ThisName[0] = Nul THEN
                    EXIT (*LOOP*);
                END (*IF*);
                ProcessOneName (ThisName);
            END (*LOOP*);
            CloseStringList (state);
            CloseINIFile (hini);
        END (*IF*);

    END UpdateList;

(************************************************************************)

PROCEDURE RefreshHostList (VAR (*IN*) INIFile: FilenameString;
                           VAR (*IN*) app, key: ARRAY OF CHAR;
                           UseTNI: BOOLEAN;
                           VAR (*INOUT*) list: HostList;
                           ExpandNumeric, Log: BOOLEAN);

    (* Discards the existing contents of the list, then reloads it      *)
    (* from INI file data.                                              *)

    BEGIN
        (*LogTransaction (LogID, "Refreshing host list");*)
        Obtain (list^.access);
        FlushHostList (list);
        UpdateList (INIFile, app, key, UseTNI, list, ExpandNumeric, Log);
        ExpandAllAliases (list, ExpandNumeric, Log);
        Release (list^.access);
    END RefreshHostList;

(************************************************************************)

PROCEDURE RefreshHostList2 (VAR (*IN*) INIFile: FilenameString;
                           VAR (*IN*) app, key: ARRAY OF CHAR;
                           UseTNI: BOOLEAN;
                           VAR (*INOUT*) list: HostList;
                           InitialAddresses: ARRAY OF CARDINAL;
                           ExpandNumeric, Log: BOOLEAN);

    (* Like RefreshHostList, but uses the IP addresses from a given     *)
    (* array to preload the list before adding the INI file data.       *)
    (* A zero address terminates the InitialAddresses array.            *)

    VAR j, val: CARDINAL;

    BEGIN
        Obtain (list^.access);
        FlushHostList (list);
        j := 0;
        WHILE (j <= HIGH(InitialAddresses)) AND (InitialAddresses[j] <> 0) DO
            val := InitialAddresses[j];
            AddNumericRecord (list, val, val, ExpandNumeric, Log);
            INC (j);
        END (*WHILE*);
        ExpandAllAliases (list, ExpandNumeric, Log);
        UpdateList (INIFile, app, key, UseTNI, list, ExpandNumeric, Log);
        ExpandAllAliases (list, ExpandNumeric, Log);
        Release (list^.access);
    END RefreshHostList2;

(************************************************************************)
(*                        MODULE INITIALISATION                         *)
(************************************************************************)

BEGIN
    sock_init();
    LogID := CreateLogID (WCtx, "Setup  ");
FINALLY
    DiscardLogID (LogID);
END HostLists.

