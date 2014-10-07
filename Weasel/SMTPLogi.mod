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

IMPLEMENTATION MODULE SMTPLogin;

        (********************************************************)
        (*                                                      *)
        (*         Logging in to another SMTP server in         *)
        (*             order to send outbound mail              *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            5 February 2003                 *)
        (*  Last edited:        17 October 2013                 *)
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

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType,
    (* proc *)  MD5DigestToString;

FROM HMAC IMPORT
    (* proc *)  HMAC_MD5;

FROM Names IMPORT
    (* type *)  UserName, PassString, HostName, FilenameString;

FROM Inet2Misc IMPORT
    (* proc *)  Swap2;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

(************************************************************************)

CONST
    Nul = CHR(0);
    ParamStringLength = 512;

TYPE
    ParamString = ARRAY [0..ParamStringLength-1] OF CHAR;

    AuthMethod = (cheat, plain, login, crammd5, dummy);

    MethodName = ARRAY [0..20] OF CHAR;
    MethodNameArray = ARRAY AuthMethod OF MethodName;

CONST
    MNames = MethodNameArray {"CHEAT", "PLAIN", "LOGIN", "CRAM-MD5", ""};

(************************************************************************)
(*                      MISCELLANEOUS UTILITIES                         *)
(************************************************************************)

PROCEDURE SendCommand (SB: SBuffer;  command: ARRAY OF CHAR;
                         VAR (*OUT*) ConnectionLost: BOOLEAN): BOOLEAN;

    (* Sends a command, returns TRUE if the command was sent OK and     *)
    (* a positive response was returned.                                *)

    BEGIN
        ConnectionLost := NOT SendLine (SB, command);
        FlushOutput (SB);
        RETURN (NOT ConnectionLost) AND PositiveResponse(SB, ConnectionLost);
    END SendCommand;

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
        j, k: CARDINAL;
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
        LogTransaction (LogID, Buffer);
        success := SendLine (SB, Buffer);

        IF success THEN
            CASE method OF
              | cheat:
                    (* Nothing more to do. *)

              | plain:
                    (* Nothing more to do. *)

              | login:
                    success := GetLine (SB, Buffer) AND (Buffer[0] = '3');
                    LogTransaction (LogID, Buffer);
                    IF success THEN
                        Base64.Encode (user, LENGTH(user), B64Buffer);
                        success := SendLine (SB, B64Buffer);
                        LogTransaction (LogID, B64Buffer);
                    END (*IF*);
                    success := success AND PositiveResponse (SB, ConnectionLost);
                    IF success THEN
                        GetLastLine (SB, Buffer);
                        LogTransaction (LogID, Buffer);
                        Base64.Encode (pass, LENGTH(pass), B64Buffer);
                        success := SendLine (SB, B64Buffer);
                        LogTransaction (LogID, B64Buffer);
                    END (*IF*);

              | crammd5:
                    success := GetLine (SB, Buffer) AND (Buffer[0] = '3');
                    LogTransaction (LogID, Buffer);
                    IF success THEN
                        Strings.Delete (Buffer, 0, 4);
                        k := Base64.Decode (Buffer, TimeStamp);
                        HMAC_MD5 (TimeStamp, k,
                                  pass, LENGTH(pass), digest);
                        MD5DigestToString (digest, digeststr);
                        Strings.Assign (user, Buffer);
                        Strings.Append (' ', Buffer);
                        Strings.Append (digeststr, Buffer);
                        Base64.Encode (Buffer, LENGTH(Buffer), B64Buffer);
                        success := SendLine (SB, B64Buffer);
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
        LogTransaction (LogID, Buffer);

        RETURN success;

    END AuthenticateWith;

(************************************************************************)
(*                     THE SMTP LOGIN OPERATION                         *)
(************************************************************************)

PROCEDURE DoLogin (SB: SBuffer;  LocalHostName: HostName;
                   UseAuth: BOOLEAN;  user: UserName;  pass: PassString;
                   VAR (*OUT*) ConnectionLost: BOOLEAN;
                   LogIt: BOOLEAN;  LogID: TransactionLogID): BOOLEAN;

    (* Assumption: we already have a connection to the server via SB.   *)
    (* We now do an EHLO login if possible, or a HELO if the EHLO is    *)
    (* not recognised.  Returns FALSE if we can't log in.               *)
    (* If UseAuth is TRUE, we authenticate with the AUTH command        *)
    (* provided that the server will accept one of the AUTH methods     *)
    (* that we support.                                                 *)

    VAR success, UseHELO, moretocome: BOOLEAN;
        method: AuthMethod;
        Auth: ARRAY AuthMethod OF BOOLEAN;
        Buffer, keyword: ParamString;

    BEGIN
        UseHELO := FALSE;

        Buffer := "EHLO ";
        Strings.Append (LocalHostName, Buffer);
        IF LogIt THEN
            LogTransaction (LogID, Buffer);
        END (*IF*);
        ConnectionLost := NOT SendLine (SB, Buffer);
        IF LogIt AND ConnectionLost THEN
            LogTransactionL (LogID, "Connection lost");
        END (*IF*);
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
                IF LogIt THEN
                    LogTransaction (LogID, Buffer);
                END (*IF*);
                IF Buffer[0] = '2' THEN
                    moretocome := Buffer[3] = '-';
                    IF UseAuth THEN
                        Strings.Delete (Buffer, 0, 4);
                        Strings.Capitalize (Buffer);
                        GetToken (Buffer, keyword);
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

        IF UseAuth THEN

            (* Now we know what authentication methods the server will  *)
            (* accept.  Use the most secure one that we support.        *)

            method := MAX(AuthMethod);
            LOOP
                IF Auth[method] THEN
                    IF AuthenticateWith(method, SB, user, pass, LogID) THEN
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
            IF LogIt THEN
                LogTransaction (LogID, Buffer);
            END (*IF*);
        END (*IF*);

        RETURN success;

    END DoLogin;

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

    VAR s: Socket;  peer: SockAddr;
        SB: SBuffer;
        success, ConnectionLost: BOOLEAN;

    BEGIN
        ConnectionLost := FALSE;
        s := NotASocket;
        IF IPAddr = 0 THEN
            success := FALSE;
        ELSE
            s := socket (AF_INET, SOCK_STREAM, AF_UNSPEC);
            success := s <> NotASocket;
        END (*IF*);

        IF success THEN
            WITH peer DO
                family := AF_INET;
                WITH in_addr DO
                    port := Swap2(port);
                    addr := IPAddr;
                    zero := Zero8;
                END (*WITH*);
            END (*WITH*);
            IF connect (s, peer, SIZE(peer)) THEN
                soclose(s);
                success := FALSE;
            END (*IF*);
        END (*IF*);

        IF success THEN

            (* We have our connection, now send username and password. *)

            SB := CreateSBuffer (s, TRUE);
            success := (CAST(ADDRESS,SB) <> NIL) AND PositiveResponse (SB, ConnectionLost)
                       AND SendString (SB, 'USER ')
                       AND SendLine (SB, user);
            IF success THEN
                success := PositiveResponse (SB, ConnectionLost)
                           AND SendString (SB, 'PASS ')
                           AND SendLine (SB, pass)
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

