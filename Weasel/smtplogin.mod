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

IMPLEMENTATION MODULE SMTPLogin;

        (********************************************************)
        (*                                                      *)
        (*         Logging in to another SMTP server in         *)
        (*             order to send outbound mail              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 February 2003                 *)
        (*  Last edited:        24 April 2017                   *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)


(************************************************************************)
(*  REFERENCES:                                                         *)
(*                                                                      *)
(*     RFC1869    ESMTP and EHLO                                        *)
(*     RFC2595    PLAIN                                                 *)
(*      ??        LOGIN                                                 *)
(*     RFC2195    CRAM-MD5                                              *)
(*                                                                      *)
(*  For more detail see module Authentication.                          *)
(*                                                                      *)
(************************************************************************)

FROM SplitScreen IMPORT  WriteString, WriteLn;    (* ONLY WHILE DEBUGGING *)

IMPORT Strings, Base64;

FROM SYSTEM IMPORT ADDRESS, CAST;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  CreateSBuffer, CloseSBuffer, SendString, SendLine,
                FlushOutput, PositiveResponse, GetLine,
                ResponseCode, GetLastLine;

FROM Sockets IMPORT
    (* const*)  NotASocket, AF_INET, SOCK_STREAM, AF_UNSPEC,
    (* type *)  Socket, SockAddr,
    (* proc *)  socket, connect, soclose;

FROM Internet IMPORT
    (* const*)  Zero8;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM MXCheck IMPORT
    (* proc *)  DoMXLookup;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType,
    (* proc *)  MD5DigestToString;

FROM HMAC IMPORT
    (* proc *)  HMAC_MD5;

FROM Names IMPORT
    (* type *)  UserName, PassString, HostName, DomainName, FilenameString;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2, IPToString;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    debugging = FALSE;
    Nul = CHR(0);
    ParamStringLength = 512;

TYPE
    ParamString = ARRAY [0..ParamStringLength-1] OF CHAR;

    AuthMethod = (cheat, plain, login, crammd5, dummy);

    MethodName = ARRAY [0..20] OF CHAR;
    MethodNameArray = ARRAY AuthMethod OF MethodName;

CONST
    MNames = MethodNameArray {"CHEAT", "PLAIN", "LOGIN", "CRAM-MD5", ""};

    (* Option used only in testing: a flag that says to use AUTH only   *)
    (* with the most secure available method.                           *)

    UseOnlyMostSecure = TRUE;

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

PROCEDURE SendCommand (SB: SBuffer;  command: ARRAY OF CHAR;
                         VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* Sends a command, returns TRUE if the command was sent OK and     *)
    (* a positive response was returned.                                *)

    VAR sent: CARDINAL;

    BEGIN
        ConnectionLost := NOT SendLine (SB, command, sent);
        EVAL (FlushOutput (SB));
        RETURN (NOT ConnectionLost) AND PositiveResponse(SB, ConnectionLost);
    END SendCommand;

(************************************************************************)

PROCEDURE SendCommandPC (SB: SBuffer;  command: ARRAY OF CHAR;
                         Logit: BOOLEAN;  LogID: TransactionLogID;
                         VAR (*OUT*) ConnectionLost: BOOLEAN;
                         VAR (*OUT*) TempFailure: BOOLEAN): BOOLEAN;

    (* A variant of SendCommand that allows logging of the response,    *)
    (* with a prepended [pc] in the log.                                *)

    VAR LogLine: ParamString;
        sent: CARDINAL;
        result: BOOLEAN;

    BEGIN
        TempFailure := FALSE;
        ConnectionLost := NOT SendLine (SB, command, sent);
        IF Logit THEN
            Strings.Insert ("[pc]>", 0, command);
            LogTransaction (LogID, command);
        END (*IF*);
        EVAL (FlushOutput (SB));
        IF ConnectionLost THEN
            result := FALSE;
        ELSE
            result := PositiveResponse(SB, ConnectionLost);
            GetLastLine (SB, LogLine);
            TempFailure := LogLine[0] = '4';
            IF Logit THEN
                Strings.Insert ("[pc]<", 0, LogLine);
                LogTransaction (LogID, LogLine);
            END (*IF*);
        END (*IF*);
        RETURN result;
    END SendCommandPC;

(************************************************************************)

PROCEDURE GetToken (VAR (*INOUT*) str: ARRAY OF CHAR;
                    VAR (*OUT*) token: ARRAY OF CHAR);

    (* Sets token to the leading substring of str that is delimited by  *)
    (* either a space or a Nul.  We delete that token from str, as well *)
    (* as any spaces that follow it.                                    *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.Assign (str, token);
        Strings.FindNext (' ', str, 0, found, pos);
        IF found THEN
            token[pos] := Nul;
            REPEAT
                INC (pos);
            UNTIL str[pos] <> ' ';
            Strings.Delete (str, 0, pos);
        ELSE
            str[0] := Nul;
        END (*IF*);
    END GetToken;

(************************************************************************)
(*              THE 'SMTP AUTH' AUTHENTICATION OPERATIONS               *)
(************************************************************************)

PROCEDURE AuthenticateWith (method: AuthMethod;  SB: SBuffer;
                            user: UserName;  pass: PassString;
                            LogID: TransactionLogID): BOOLEAN;

    (* Attempts to authenticate with the specified method.  Returns     *)
    (* FALSE if the attempt fails.                                      *)

    VAR success, ConnectionLost: BOOLEAN;
        j, k, sent: CARDINAL;
        Buffer, Buffer2, B64Buffer: ParamString;
        TimeStamp: FilenameString;
        digest: MD5_DigestType;
        digeststr: ARRAY [0..31] OF CHAR;

    BEGIN
        ConnectionLost := FALSE;
        Buffer := "AUTH ";
        Strings.Append (MNames[method], Buffer);
        IF method = plain THEN

            (* The PLAIN method data can be sent entirely as    *)
            (* part of the AUTH command.                        *)

            Strings.Assign (' ', Buffer2);
            Strings.Append (user, Buffer2);
            j := Strings.Length (Buffer2);
            Strings.Append (' ', Buffer2);
            Strings.Append (pass, Buffer2);
            k := Strings.Length (Buffer2);
            Buffer[0] := Nul;
            Buffer[j] := Nul;
            Base64.Encode (Buffer2, k, B64Buffer);
            Strings.Append (' ', Buffer);
            Strings.Append (B64Buffer, Buffer);
        END (*IF*);
        success := SendLine (SB, Buffer, sent);
        Strings.Insert ("> ", 0, Buffer);
        LogTransaction (LogID, Buffer);

        IF success THEN
            CASE method OF
              | cheat:
                    (* Nothing more to do. *)

              | plain:
                    (* Nothing more to do. *)

              | login:
                    success := GetLine (SB, Buffer) AND (Buffer[0] = '3');
                    Strings.Insert ("< ", 0, Buffer);
                    LogTransaction (LogID, Buffer);
                    IF success THEN
                        Base64.Encode (user, LENGTH(user), B64Buffer);
                        success := SendLine (SB, B64Buffer, sent);
                        Strings.Insert ("> ", 0, B64Buffer);
                        LogTransaction (LogID, B64Buffer);
                    END (*IF*);
                    success := success AND PositiveResponse (SB, ConnectionLost);
                    IF success THEN
                        GetLastLine (SB, Buffer);
                        Strings.Insert ("< ", 0, Buffer);
                        LogTransaction (LogID, Buffer);
                        Base64.Encode (pass, LENGTH(pass), B64Buffer);
                        success := SendLine (SB, B64Buffer, sent);
                        Strings.Insert ("> ", 0, B64Buffer);
                        LogTransaction (LogID, B64Buffer);
                    END (*IF*);

              | crammd5:
                    success := GetLine (SB, Buffer) AND (Buffer[0] = '3');
                    Strings.Insert ("< ", 0, Buffer);
                    LogTransaction (LogID, Buffer);
                    IF success THEN
                        Strings.Delete (Buffer, 0, 5);
                        k := Base64.Decode (Buffer, TimeStamp);

                        (* For some unexplained reason (compiler bug?), we  *)
                        (* cannot trust the value of k here.                *)

                        HMAC_MD5 (TimeStamp, LENGTH(TimeStamp),
                                  pass, LENGTH(pass), digest);
                        MD5DigestToString (digest, digeststr);

                        (* Temporary code while debugging. *)

                        IF debugging THEN
                            WriteString ("Challenge ");  WriteString (TimeStamp);  WriteLn;
                            WriteString ("User='");  WriteString (user);
                            WriteString ("', password='");  WriteString (pass);  WriteString ("'");
                            WriteLn;
                            WriteString ("Sending ");  WriteString (digeststr);  WriteLn;
                        END (*IF*);

                        Strings.Assign (user, Buffer);
                        Strings.Append (' ', Buffer);
                        Strings.Append (digeststr, Buffer);
                        Base64.Encode (Buffer, LENGTH(Buffer), B64Buffer);
                        success := SendLine (SB, B64Buffer, sent);
                        Strings.Insert ("> ", 0, B64Buffer);
                        LogTransaction (LogID, B64Buffer);
                    END (*IF*);

              | ELSE
                    (* Nothing more to do. *)

            END (*CASE*);
        END (*IF*);

        (* Take note of the response code, so that we can report any    *)
        (* authentication failure.                                      *)

        IF success THEN
            k := ResponseCode(SB);
            success := (k > 0) AND (k < 4);
        END (*IF*);
        GetLastLine (SB, Buffer);
        Strings.Insert ("< ", 0, Buffer);
        LogTransaction (LogID, Buffer);

        RETURN success;

    END AuthenticateWith;

(************************************************************************)
(*                     THE SMTP LOGIN OPERATION                         *)
(************************************************************************)

PROCEDURE DoLogin (SB: SBuffer;  LocalHostName: HostName;
                   UseAuth: BOOLEAN;  user: UserName;  pass: PassString;
                   VAR (*OUT*) ConnectionLost: BOOLEAN;
                   VAR (*OUT*) ChunkingAvailable: BOOLEAN;
                   LogID: TransactionLogID): BOOLEAN;

    (* Assumption: we already have a connection to the server via SB.   *)
    (* We now do an EHLO login if possible, or a HELO if the EHLO is    *)
    (* not recognised.  Returns FALSE if we can't log in.               *)
    (* If UseAuth is TRUE, we authenticate with the AUTH command        *)
    (* provided that the server will accept one of the AUTH methods     *)
    (* that we support.                                                 *)

    (* ChunkingAvailable is TRUE iff the remote server supports the     *)
    (* CHUNKING option.                                                 *)

    VAR success, moretocome, UseHELO: BOOLEAN;
        sent: CARDINAL;
        method: AuthMethod;
        Auth: ARRAY AuthMethod OF BOOLEAN;
        Buffer, keyword, message: ParamString;

    BEGIN
        ChunkingAvailable := FALSE;
        UseHELO := FALSE;
        Buffer := "EHLO ";
        Strings.Append (LocalHostName, Buffer);
        ConnectionLost := NOT SendLine (SB, Buffer, sent);
        Strings.Insert ("> ", 0, Buffer);
        LogTransaction (LogID, Buffer);
        success := NOT ConnectionLost;
        moretocome := success;
        FOR method := MIN(AuthMethod) TO MAX(AuthMethod) DO
            Auth[method] := FALSE;
        END (*FOR*);

        (* We expect a series of response lines, each starting  *)
        (* with a three-digit code, and all but the last        *)
        (* following that with a '-'.                           *)

        WHILE moretocome DO
            ConnectionLost := NOT GetLine (SB, Buffer);
            IF ConnectionLost THEN
                success := FALSE;  moretocome := FALSE;
            ELSE
                Strings.Assign ("< ", message);
                Strings.Append (Buffer, message);
                LogTransaction (LogID, message);
                IF Buffer[0] = '2' THEN
                    moretocome := Buffer[3] = '-';
                    Strings.Delete (Buffer, 0, 4);
                    Strings.Capitalize (Buffer);
                    GetToken (Buffer, keyword);
                    IF Strings.Equal (keyword, 'CHUNKING') THEN
                        ChunkingAvailable := TRUE;
                    ELSIF UseAuth THEN
                        IF Strings.Equal (keyword, 'AUTH') THEN
                            WHILE Buffer[0] <> Nul DO
                                GetToken (Buffer, keyword);
                                FOR method := MIN(AuthMethod) TO MAX(AuthMethod) DO
                                    IF Strings.Equal (keyword, MNames[method]) THEN
                                        Auth[method] := TRUE;
                                    END (*IF*);
                                END (*FOR*);
                            END (*WHILE*);
                        END (*IF*);
                    END (*IF*);
                ELSE
                    UseHELO := TRUE;
                    success := FALSE;  moretocome := FALSE;
                END (*IF*);
            END (*IF*);
        END (*WHILE*);

        IF success AND UseAuth THEN

            (* Now we know what authentication methods the server will  *)
            (* accept.  Use the most secure one that we support.        *)

            method := MAX(AuthMethod);
            LOOP
                IF Auth[method] THEN
                    IF AuthenticateWith(method, SB, user, pass, LogID) THEN
                        EXIT (*LOOP*);
                    ELSIF UseOnlyMostSecure THEN
                        success := FALSE;
                        EXIT (*LOOP*);
                    END (*IF*);
                END (*IF*);
                IF method = MIN(AuthMethod) THEN
                    EXIT (*LOOP*);
                ELSE
                    DEC (method);
                END (*IF*);
            END (*LOOP*);

        END (*IF*);

        (* The following code is invoked only if the EHLO was rejected. *)

        IF UseHELO THEN
            Buffer := "HELO ";
            Strings.Append (LocalHostName, Buffer);
            success := SendCommand (SB, Buffer, ConnectionLost);
            Strings.Insert ("> ", 0, Buffer);
            LogTransaction (LogID, Buffer);
        END (*IF*);

        RETURN success;

    END DoLogin;

(************************************************************************)
(*                           OPEN A CONNECTION                          *)
(************************************************************************)

PROCEDURE OpenConnection (IPAddr: CARDINAL): Socket;

    CONST SMTPport = 25;

    VAR s: Socket;
        peer: SockAddr;

    BEGIN
        s := NotASocket;
        IF IPAddr <> 0 THEN
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
        END (*IF*);

        IF s <> NotASocket THEN
            WITH peer DO
                family := AF_INET;
                WITH in_addr DO
                    port := Swap2(SMTPport);
                    addr := IPAddr;
                    zero := Zero8;
                END (*WITH*);
            END (*WITH*);
            IF connect (s, peer, SIZE(peer)) THEN
                soclose(s);
                s := NotASocket;
            END (*IF*);
        END (*IF*);

        RETURN s;

    END OpenConnection;

(************************************************************************)

PROCEDURE ConnectToDomain (domain: HostName;  VAR (*OUT*) s: Socket;
                         LogId: TransactionLogID;
                         watchdog: Semaphore;
                         VAR (*OUT*) LookupFailure: BOOLEAN): BOOLEAN;

    (* Try to connect to the given domain.  If necessary and        *)
    (* we get multiple possible addresses for that domain, we try   *)
    (* the multiple addresses in order of preference.               *)

    CONST Max = 31;

    VAR success: BOOLEAN;  j: CARDINAL;
        address: ARRAY [0..Max] OF CARDINAL;
        message: ARRAY [0..63] OF CHAR;
        IPstr: ARRAY [0..21] OF CHAR;

    BEGIN
        success := FALSE;  LookupFailure := FALSE;  j := 0;
        s := NotASocket;
        Strings.Assign ("Looking up domain name ", message);
        Strings.Append (domain, message);
        LogTransaction (LogId, message);
        CASE DoMXLookup (domain, address) OF
          |  0: REPEAT
                    IPToString (address[j], TRUE, IPstr);
                    Strings.Assign ("MX lookup result ", message);
                    Strings.Append (IPstr, message);
                    LogTransaction (LogId, message);
                    s := OpenConnection (address[j]);
                    Signal (watchdog);
                    success := s <> NotASocket;
                    IF success THEN
                        LogTransactionL (LogId, "connected");
                    ELSE
                        LogTransactionL (LogId, "can't connect");
                    END (*IF*);
                    INC (j);
                UNTIL success OR (j > Max) OR (address[j] = 0);
          |  2,3:
                (* Fault could be at our end, e.g. nameserver failure. *)
                LookupFailure := TRUE;
          |  ELSE
                success := FALSE;
        END (*CASE*);

        RETURN success;

    END ConnectToDomain;

(************************************************************************)
(*        CHECK TO SEE WHETHER A DOMAIN HAS A POSTMASTER ACCOUNT        *)
(************************************************************************)

PROCEDURE PostmasterCheck (domain: DomainName;  OurHostName: HostName;
                                OurDomain: DomainName;
                                LogID: TransactionLogID;
                                watchdog: Semaphore;
                                VAR (*OUT*) TempFailure: BOOLEAN): BOOLEAN;

    (* SMTP connection for a dummy "send to postmaster" whose purpose   *)
    (* is to check that user "postmaster" is recognised.                *)

    CONST LogIt = TRUE;

    VAR SB: SBuffer;  s: Socket;
        Buffer: ParamString;
        success, ConnectionLost: BOOLEAN;

    BEGIN
        TempFailure := FALSE;
        SB := NIL;
        success := ConnectToDomain (domain, s, LogID, watchdog, ConnectionLost);
        IF ConnectionLost THEN

            (* We should give the domain the benefit of the doubt if    *)
            (* the failure could be at our end.                         *)

            RETURN TRUE;
        END (*IF*);
        IF success THEN
            SB := CreateSBuffer (s, TRUE);
            success := (CAST(ADDRESS,SB) <> NIL) AND PositiveResponse (SB, ConnectionLost);
            IF LogIt THEN
                GetLastLine (SB, Buffer);
                Strings.Insert ("[pc]<", 0, Buffer);
                LogTransaction (LogID, Buffer);
            END (*IF*);
        END (*IF*);

        IF success THEN
            Buffer := "HELO ";
            Strings.Append (OurHostName, Buffer);
            success := SendCommandPC (SB, Buffer, LogIt, LogID,
                                       ConnectionLost, TempFailure);
        END (*IF*);
        IF success THEN
            Buffer := "MAIL FROM:<postmaster@";
            Strings.Append (OurDomain, Buffer);
            Strings.Append ('>', Buffer);
            success := SendCommandPC (SB, Buffer, LogIt, LogID,
                                       ConnectionLost, TempFailure);
        END (*IF*);
        IF success THEN
            Buffer := "RCPT TO:<postmaster@";
            Strings.Append (domain, Buffer);
            Strings.Append ('>', Buffer);
            success := SendCommandPC (SB, Buffer, LogIt, LogID,
                                       ConnectionLost, TempFailure);
            IF NOT (success OR TempFailure) THEN
                Buffer := "RCPT TO:<postmaster>";
                success := SendCommandPC (SB, Buffer, LogIt, LogID,
                                       ConnectionLost, TempFailure);
            END (*IF*);
        END (*IF*);
        IF success THEN
            Buffer := "QUIT";
            EVAL (SendCommandPC (SB, Buffer, LogIt, LogID,
                                       ConnectionLost, TempFailure));
        END (*IF*);
        CloseSBuffer (SB);
        RETURN success;
    END PostmasterCheck;

(************************************************************************)
(*          POP3 LOGIN, USE FOR POP-BEFORE-SMTP AUTHENTICATION          *)
(************************************************************************)

PROCEDURE DoPOPLogin (IPAddr, port: CARDINAL;
                           user: UserName;  pass: PassString;
                           LogIt: BOOLEAN;  LogID: TransactionLogID);

    (* Attempts a POP3 login to the given address.  We do not need to   *)
    (* record whether the attempt succeeded, because if it failed       *)
    (* this would not change our actions.                               *)

    CONST PopPort = 110;

    VAR s: Socket;  SB: SBuffer;
        sent: CARDINAL;
        success, ConnectionLost: BOOLEAN;

    BEGIN
        ConnectionLost := FALSE;
        s := OpenConnection (IPAddr);
        success := s <> NotASocket;

        IF success THEN

            (* We have our connection, now send username and password. *)

            SB := CreateSBuffer (s, TRUE);
            success := (CAST(ADDRESS,SB) <> NIL) AND PositiveResponse (SB, ConnectionLost)
                       AND SendString (SB, 'USER ', sent)
                       AND SendLine (SB, user, sent);
            IF success THEN
                success := PositiveResponse (SB, ConnectionLost)
                           AND SendString (SB, 'PASS ', sent)
                           AND SendLine (SB, pass, sent)
                           AND PositiveResponse (SB, ConnectionLost);
                EVAL (SendCommand (SB, "QUIT", ConnectionLost));
            END (*IF*);

            CloseSBuffer (SB);

        END (*IF*);

        IF LogIt AND NOT success THEN
            IF ConnectionLost THEN
                LogTransactionL (LogID, "Lost connection during POP3 login");
            ELSE
                LogTransactionL (LogID, "POP3 login failed");
            END (*IF*);
        END (*IF*);

    END DoPOPLogin;

(************************************************************************)

END SMTPLogin.

