(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
(*  Copyright (C) 2016   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE SMTPCommands;

        (********************************************************)
        (*                                                      *)
        (*       Command interpreter for SMTP server            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            27 April 1998                   *)
        (*  Last edited:        14 December 2016                *)
        (*  Status:             Basically OK                    *)
        (*                                                      *)
        (*  Improvements needed:                                *)
        (*    HELO: should check validity of sending host name? *)
        (*                                                      *)
        (********************************************************)

(********************************************************************************)
(*                        COMPLIANCE WITH THE STANDARD                          *)
(********************************************************************************)
(*                                                                              *)
(* I'm working from the SMTP standard RFC821, as modified by RFC1123 and        *)
(* RFC2821.  We also support ESMTP as specified in RFC1869.                     *)
(*                                                                              *)
(* The required commmands are all implemented and tested:                       *)
(*                                                                              *)
(*      DATA, HELO, MAIL, NOOP, QUIT, RCPT, RSET, VRFY                          *)
(*                                                                              *)
(* The optional commands that are implemented are:                              *)
(*                                                                              *)
(*      AUTH (RFC2554), EHLO (RFC1869), EXPN (RFC821)                           *)
(*                                                                              *)
(* We also support the SIZE parameter in MAIL (RFC1870).                        *)
(*                                                                              *)
(********************************************************************************)

IMPORT Strings, Delivery;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE(*, SayHeapCount*);

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  GetLine, SendLine, FlushOutput, SocketOf;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Semaphores IMPORT
    (* type *)  Semaphore,
    (* proc *)  Signal;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM InetUtilities IMPORT
    (* proc *)  ConvertCard, AddEOL, CurrentTimeToString;

FROM Inet2Misc IMPORT
    (* proc *)  SplitArg, ToLower, StringMatch;

FROM Names IMPORT
    (* type *)  UserName, HostName, DomainName;

FROM Domains IMPORT
    (* type *)  Domain,
    (* proc *)  NameOfDomain, DomainIsLocal, IsValidUsername, SMTPAuthAllowed;

FROM Hosts IMPORT
    (* proc *)  AcceptableRelayDestination;

FROM Authentication IMPORT
    (* type *)  AuthenticationState,
    (* proc *)  StartAuthentication, AuthenticationIncomplete,
                CreateNextChallenge, CheckResponse, GetAuthNames,
                AuthenticationDone, SetAuthMethods;

FROM SMTPData IMPORT
    (* type *)  ItemDescriptor,
    (* proc *)  LimitOnMessageSize, CreateItemDescriptor, DiscardItemDescriptor,
                ResetItemDescriptor, ResetReturnPath, SetClaimedSendingHost,
                AddLocalRecipient, AddRelayRecipient, AcceptMessage,
                RunFilter03, (*RunFinalFilter, DistributeMessage,*) FilterAndDistribute,
                SenderNotSpecified, NoRecipients,
                ProcessRCPTAddress, FromAddressAcceptable;

FROM Extra IMPORT
    (* proc *)  UserAndDomain;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

(********************************************************************************)

CONST
    Nul = CHR(0);
    BadCommandLimit = 3;
    BadAUTHLimit = 3;

TYPE
    FourChar = ARRAY [0..3] OF CHAR;
    ClientState = (Idle, LoggedIn, MustAbort, MustExit);

    (* The session record.  The fields are:                             *)
    (*     ID          session ID for transaction logging               *)
    (*     sbuffer     The socket buffer                                *)
    (*     state       To track whether the user is currently logged in *)
    (*     HostAddr    Our IP address                                   *)
    (*     desc        Information about the next item to be delivered  *)
    (*     BadCommandCount  number of unknown commands we've received   *)
    (*                      since last good command                     *)
    (*     authenticated    TRUE iff we have processed a valid AUTH     *)
    (*     whitelisted      TRUE iff the client is on our whitelist     *)
    (*     AcceptRelayMail  TRUE iff we're willing to accept mail to    *)
    (*                      be relayed                                  *)
    (*     StrictAUTH  TRUE iff we are applying stronger checks on      *)
    (*                  whether relay mail should be accepted.          *)
    (*     watchdog    Semaphore that times out if we don't kick it     *)
    (*                  now and then                                    *)

    Session = POINTER TO SessionRecord;
    SessionRecord = RECORD
                        ID: TransactionLogID;
                        sbuffer: SBuffer;
                        state: ClientState;
                        HostAddr: CARDINAL;
                        desc: ItemDescriptor;
                        BadCommandCount: CARDINAL;
                        BadAUTHCount: CARDINAL;
                        authenticated: BOOLEAN;
                        whitelisted: BOOLEAN;
                        AcceptRelayMail: BOOLEAN;
                        StrictAUTH: BOOLEAN;
                        watchdog: Semaphore;
                    END (*RECORD*);

VAR
    (* Mask to say what AUTH mechanisms are accepted. *)

    AuthentMask: CARDINAL;

    (* Maximum acceptable message size. *)

    MaxMessageSize: CARDINAL;

(********************************************************************************)
(*                            SETTING THE AUTH MASK                             *)
(********************************************************************************)

PROCEDURE SetAuthMask (SMTPAuthMask: CARDINAL);

    (* The parameter is a bit mask that says which SMTP AUTH methods we are     *)
    (* willing to accept on incoming mail.                                      *)

    BEGIN
        AuthentMask := SMTPAuthMask;
        SetAuthMethods (SMTPAuthMask);
    END SetAuthMask;

(********************************************************************************)
(*                       SENDING REPLY BACK TO CLIENT                           *)
(********************************************************************************)

PROCEDURE Reply3 (session: Session;  message1, message2, message3: ARRAY OF CHAR);

    (* Sends all of message1, followed by message2, followed by message3,       *)
    (* followed by end-of-line.                                                 *)
    (* If the operation fails, session^.state is set to MustExit.               *)

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (message1, buffer);
        Strings.Append (message2, buffer);
        Strings.Append (message3, buffer);
        LogTransaction (session^.ID, buffer);
        IF NOT SendLine (session^.sbuffer, buffer) THEN
            session^.state := MustExit;
        END (*IF*);
        FlushOutput (session^.sbuffer);
    END Reply3;

(********************************************************************************)

PROCEDURE Reply2 (session: Session;  message1, message2: ARRAY OF CHAR);

    (* Sends all of message1, followed by message2, followed by end-of-line.    *)
    (* If the operation fails, session^.state is set to MustExit.               *)

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (message1, buffer);
        Strings.Append (message2, buffer);
        LogTransaction (session^.ID, buffer);
        IF NOT SendLine (session^.sbuffer, buffer) THEN
            session^.state := MustExit;
        END (*IF*);
        FlushOutput (session^.sbuffer);
    END Reply2;

(********************************************************************************)

PROCEDURE Reply (session: Session;  message: ARRAY OF CHAR);

    (* Like Reply2, except that there is no message2. *)

    VAR buffer: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (message, buffer);
        LogTransaction (session^.ID, buffer);
        IF NOT SendLine (session^.sbuffer, buffer) THEN
            session^.state := MustExit;
        END (*IF*);
        FlushOutput (session^.sbuffer);
    END Reply;

(********************************************************************************)
(*                           STARTING A NEW SESSION                             *)
(********************************************************************************)

PROCEDURE OpenSession (SB: SBuffer;  HostIPAddress, ClientIPAddress: CARDINAL;
                       KeepAlive: Semaphore; LogID: TransactionLogID;
                       OnWhitelist, MayRelay, MSAsession: BOOLEAN;
                       VAR (*OUT*) success: BOOLEAN): Session;

    (* Creates a new session state record. *)

    VAR result: Session;
        FailureReason: ARRAY [0..127] OF CHAR;

    BEGIN
        NEW (result);
        WITH result^ DO
            ID := LogID;
            sbuffer := SB;
            HostAddr := HostIPAddress;
            state := Idle;
            whitelisted := OnWhitelist;
            AcceptRelayMail := MayRelay;
            authenticated := FALSE;
            StrictAUTH := MSAsession;
            watchdog := KeepAlive;
            desc := CreateItemDescriptor (SB, ClientIPAddress, LogID, MayRelay, OnWhitelist);
            BadCommandCount := 0;
            BadAUTHCount := 0;
            success := RunFilter03 (0, desc, FailureReason) < 3;
        END (*WITH*);
        IF NOT success THEN
            Reply (result, FailureReason);
        END (*IF*);
        RETURN result;
    END OpenSession;

(********************************************************************************)

PROCEDURE CloseSession (S: Session);

    (* Destroys the session state record. *)

    BEGIN
        IF S <> NIL THEN
            FlushOutput (S^.sbuffer);
            DiscardItemDescriptor (S^.desc);
            DEALLOCATE (S, SIZE(SessionRecord));
        END (*IF*);
    END CloseSession;

(********************************************************************************)
(*                     HANDLERS FOR SOME ERROR CONDITIONS                       *)
(********************************************************************************)

PROCEDURE NoSuchCommand (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Command is not a recognised command. *)

    BEGIN
        Sleep (3000);
        IF Command[0] = Nul THEN
            Reply (session, "500 No command supplied");
        ELSE
            Reply2 (session, "500 Unknown command ", Command);
        END (*IF*);
    END NoSuchCommand;

(********************************************************************************)

PROCEDURE GarbledCommandSequence (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* Too many unknown commands have been received. *)

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "421 Too many bad commands, closing connection");
        session^.state := MustExit;
    END GarbledCommandSequence;

(********************************************************************************)

PROCEDURE NotLoggedIn (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* Command is illegal because user is not yet logged in. *)

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "503 Not logged in");
    END NotLoggedIn;

(********************************************************************************)
(*                     HANDLERS FOR THE INDIVIDUAL COMMANDS                     *)
(********************************************************************************)

PROCEDURE AUTH (session: Session;  VAR (*IN*) args: ARRAY OF CHAR);

    VAR mechanism: ARRAY [0..20] OF CHAR;
        message:  ARRAY [0..511] OF CHAR;
        domainname: DomainName;
        state: AuthenticationState;
        working, ReplySent: BOOLEAN;
        username: UserName;  domain: Domain;

    BEGIN
        ReplySent := FALSE;
        IF session^.authenticated THEN
            Reply (session, "503 Already authenticated");
        ELSE
            SplitArg (mechanism, args);
            working := TRUE;
            domain := NIL;
            IF session^.StrictAUTH AND NOT StringMatch(mechanism, "CRAM-MD5") THEN
                Reply (session, "504 CRAM-MD5 is required");
                ReplySent := TRUE;
                working := FALSE;
            END (*IF*);
            IF working THEN
                IF StartAuthentication (state, session^.HostAddr,
                                        AuthentMask, mechanism, args) THEN
                    WHILE working AND AuthenticationIncomplete (state,
                                                      session^.authenticated) DO
                        CreateNextChallenge (state, message);
                        Reply2 (session, "334 ", message);
                        IF GetLine (session^.sbuffer, message) THEN
                            LogTransaction (session^.ID, message);
                            IF (message[0] = '*') AND (message[1] = Nul) THEN
                                Reply (session, "501 Authentication cancelled");
                                ReplySent := TRUE;
                                working := FALSE;
                            ELSE
                                CheckResponse (state, message);
                            END (*IF*);
                        ELSE
                            session^.state := MustExit;
                            working := FALSE;
                        END (*IF*);
                    END (*WHILE*);
                    session^.authenticated := session^.authenticated AND working;
                    IF NOT session^.authenticated THEN
                        Sleep (3000);
                        INC (session^.BadAUTHCount);
                        IF session^.BadAUTHCount >= BadAUTHLimit THEN
                            Reply (session, "535 too many retries, disconnecting");
                            ReplySent := TRUE;
                            Sleep (10000);
                            session^.state := MustExit;
                        END (*IF*);
                    END (*IF*);
                ELSE
                    Reply2 (session, "504 unsupported authentication mechanism ", mechanism);
                    ReplySent := TRUE;
                END (*IF*);
                AuthenticationDone (state, username, domain);
            END (*IF*);
            IF session^.authenticated AND SMTPAuthAllowed (username, domain) THEN
                Reply (session, "235 Authentication successful");
            ELSE
                session^.authenticated := FALSE;
                IF NOT ReplySent THEN
                    Reply (session, "535 Authentication failed");
                END (*IF*);
            END (*IF*);
            IF NOT ReplySent THEN
                message := "Authentication username was ";
                Strings.Append (username, message);
                NameOfDomain (domain, domainname);
                IF domainname[0] <> Nul THEN
                    Strings.Append ('@', message);
                    Strings.Append (domainname, message);
                END (*IF*);
                LogTransaction (session^.ID, message);
            END (*IF*);
        END (*IF*);
    END AUTH;

(********************************************************************************)

PROCEDURE DATA (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    VAR result: CARDINAL;
        FailureReason: ARRAY [0..127] OF CHAR;

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        IF SenderNotSpecified (session^.desc) THEN
            Reply (session, "503 Sender has not been specified");
        ELSIF NoRecipients (session^.desc) THEN
            Reply (session, "503 No valid recipients");
        ELSE

            (* Pre-reception filtering. *)

            result := RunFilter03(3, session^.desc, FailureReason);
            IF result = 3 THEN
                ResetItemDescriptor (session^.desc, "");
                Reply (session, FailureReason);
                session^.state := MustAbort;
            ELSE
                Reply (session, "354 Socket to me");
                WITH session^ DO
                    IF AcceptMessage (sbuffer, desc, watchdog, FailureReason) THEN

                        (* Post-reception filtering and delivery. *)

                        IF NOT FilterAndDistribute (desc, sbuffer) THEN
                             session^.state := MustAbort;
                        END (*IF*);
                        ResetItemDescriptor (desc, "");

                    ELSE
                        Reply (session, FailureReason);
                    END (*IF*);

                END (*WITH*);
            END (*IF*);
        END (*IF*);

    END DATA;

(********************************************************************************)

PROCEDURE EHLO (session: Session;  VAR (*IN*) name: ARRAY OF CHAR);

    VAR host: HostName;  pos: CARDINAL;
        FailureReason, buffer: ARRAY [0..127] OF CHAR;

    BEGIN
        ResetItemDescriptor (session^.desc, "");
        ResetReturnPath (session^.desc);
        Strings.Assign (name, host);
        IF NOT SetClaimedSendingHost (session^.desc, host) THEN
            Reply (session, "550 Access denied");
            session^.state := MustAbort;
        ELSIF RunFilter03(1, session^.desc, FailureReason) = 0 THEN
            session^.state := LoggedIn;
            host := "[127.0.0.0]";            (* fallback default *)
            Delivery.GetOurHostName (SocketOf(session^.sbuffer), host);
            Reply3 (session, "250-", host, " says hello");
            IF session^.StrictAUTH THEN
                Reply (session, "250-AUTH CRAM-MD5");
            ELSE
                GetAuthNames (host, FALSE);
                IF host[0] <> Nul THEN
                    Reply2 (session, "250-AUTH", host);
                END (*IF*);
            END (*IF*);
            buffer := "250-SIZE ";  pos := Strings.Length(buffer);
            ConvertCard (MaxMessageSize, buffer, pos);
            buffer[pos] := Nul;
            Reply (session, buffer);
            Reply (session, "250 EXPN");

            (* N.B. No continuation marker on last line. *)

        ELSE
            Reply (session, FailureReason);
            session^.state := MustAbort;
        END (*IF*);
    END EHLO;

(********************************************************************************)

PROCEDURE EXPN (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "550 That information is confidential");
    END EXPN;

(********************************************************************************)

PROCEDURE HELO (session: Session;  VAR (*IN*) name: ARRAY OF CHAR);

    VAR FailureReason: ARRAY [0..127] OF CHAR;
        host: HostName;

    BEGIN
        ResetItemDescriptor (session^.desc, "");
        ResetReturnPath (session^.desc);
        Strings.Assign (name, host);
        IF NOT SetClaimedSendingHost (session^.desc, host) THEN
            Reply (session, "550 Access denied");
            session^.state := MustAbort;
        ELSIF RunFilter03(1, session^.desc, FailureReason) = 0 THEN
            session^.state := LoggedIn;
            Reply (session, "250 OK");
        ELSE
            Reply (session, FailureReason);
            session^.state := MustAbort;
        END (*IF*);
    END HELO;

(********************************************************************************)

PROCEDURE MAIL (session: Session;  VAR (*IN*) from: ARRAY OF CHAR);

    CONST SizeEq = "SIZE=";

    VAR j, pos, spacepos, size: CARDINAL;  found, TempFailure: BOOLEAN;
        ch: CHAR;
        FailureReason: ARRAY [0..127] OF CHAR;

    BEGIN
        (* Delete the "FROM:" part of the command.  (There's no harm in         *)
        (* assuming it's there without checking.  If it's not there then the    *)
        (* command is faulty anyway, and it's unlikely that this fault would be *)
        (* followed by a valid sender address.)                                 *)

        j := 5;
        WHILE from[j] = ' ' DO
            INC (j);
        END (*WHILE*);
        Strings.Delete (from, 0, j);

        (* Check for a possible SIZE value after the address. *)

        size := 0;
        Strings.FindNext (' ', from, 0, found, spacepos);
        IF found THEN
            pos := spacepos + 1;
            j := 0;
            WHILE (j < 5) AND (CAP(from[pos]) = SizeEq[j]) DO
                INC (j);  INC(pos);
            END (*WHILE*);
            IF j = 5 THEN
                (* Have found "SIZE=", now decode following number. *)
                ch := from[pos];
                WHILE (ch >= '0') AND (ch <= '9') DO
                    size := 10*size + ORD(ch) - ORD('0');
                    INC (pos);  ch := from[pos];
                END (*WHILE*);
            END (*IF*);
            from[spacepos] := Nul;
        END (*IF*);

        ResetItemDescriptor (session^.desc, from);
        IF size > MaxMessageSize THEN
            Reply (session, "552 Message too large");
            ResetItemDescriptor (session^.desc, "");
        ELSIF RunFilter03(2, session^.desc, FailureReason) = 0 THEN
            IF session^.whitelisted
                   OR FromAddressAcceptable (session^.desc,
                                SocketOf(session^.sbuffer),
                                session^.watchdog, TempFailure) THEN
                Reply (session, "250 Sender accepted");
            ELSIF TempFailure THEN
                Reply (session, "451 Postmaster check failed, try again later");
            ELSE
                Reply (session, "553 FROM address not accepted");
            END (*IF*);
        ELSE
            ResetItemDescriptor (session^.desc, "");
            Reply (session, FailureReason);
            session^.state := MustAbort;
        END (*IF*);

    END MAIL;

(********************************************************************************)

PROCEDURE NOOP (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "250 OK");
    END NOOP;

(********************************************************************************)

PROCEDURE QUIT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "221 Closing connection");
        session^.state := MustExit;
    END QUIT;

(********************************************************************************)

PROCEDURE RCPT (session: Session;  VAR (*IN*) target: ARRAY OF CHAR);

    CONST delay = 10*1000;    (* 10 seconds, to slow down spammers *)

    VAR user: UserName;
        host: HostName;
        D: Domain;
        response: ARRAY [0..255] OF CHAR;

    BEGIN

        (*SayHeapCount (session^.ID, "entering RCPT");*)

        (* Delete the "TO:" part of the command.  (There's no harm in assuming  *)
        (* it's there without checking.  If it's not there then the command is  *)
        (* faulty anyway, and it's unlikely that this fault would be followed   *)
        (* by a valid recipient address.)                                       *)

        Strings.Delete (target, 0, 3);
        WHILE target[0] = ' ' DO
            Strings.Delete (target, 0, 1);
        END (*WHILE*);
        ToLower (target);
        ProcessRCPTAddress (session^.desc, target, user, host);

        (* Has a sender been specified? *)

        IF SenderNotSpecified (session^.desc) THEN

            Reply (session, "503 Bad sequence of commands");

        (* Is this a local user? *)

        ELSIF DomainIsLocal(host, D) THEN

            (*LogDomainInfo (session^.ID, D);*)

            (*SayHeapCount (session^.ID, "before AddLocalRecipient");*)

            IF AddLocalRecipient (session^.desc, D, user, host) THEN
                Reply (session, "250 mailbox OK");
            ELSE
                Sleep (delay);
                response := "550 unknown user ";
                Strings.Append (user, response);
                Reply (session, response);
            END (*IF*);

            (*SayHeapCount (session^.ID, "after AddLocalRecipient");*)

        (* If not, do we agree to relay it? *)

        ELSIF session^.AcceptRelayMail OR session^.authenticated
                 OR (NOT session^.StrictAUTH AND AcceptableRelayDestination(host)) THEN

            AddRelayRecipient (session^.desc, target);
            Reply2 (session, "250 User not local; will forward to ", target);

        ELSE
            Reply2 (session, "550 Unauthorized user or relaying not authorized for ", target);
        END (*IF*);

        (*SayHeapCount (session^.ID, "leaving RCPT");*)

    END RCPT;

(********************************************************************************)

PROCEDURE RSET (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    CONST delay = 10*1000;    (* 10 seconds, to slow down spammers *)

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        ResetItemDescriptor (session^.desc, "");
        ResetReturnPath (session^.desc);
        Sleep (delay);
        Reply (session, "250 OK");
    END RSET;

(********************************************************************************)

PROCEDURE VRFY (session: Session;  VAR (*IN*) username: ARRAY OF CHAR);

    VAR response: ARRAY [0..255] OF CHAR;
        user: UserName;  domainname: DomainName;
        D: Domain;

    BEGIN
        ToLower (username);
        UserAndDomain (username, user, domainname);

        (* Is this a local user? *)

        IF DomainIsLocal(domainname, D) AND IsValidUsername(user, D, NIL) THEN
            response := "250 ";
            Strings.Append (username, response);
        ELSE
            response := "553 unknown user";
        END (*IF*);
        Reply (session, response);

    END VRFY;

(********************************************************************************)
(*                        THE MAIN COMMAND DISPATCHER                           *)
(********************************************************************************)

TYPE
    KeywordNumber = [0..10];
    HandlerProc = PROCEDURE (Session, VAR (*IN*) ARRAY OF CHAR);
    HandlerArray = ARRAY KeywordNumber OF HandlerProc;
    KeywordArray = ARRAY KeywordNumber OF FourChar;

CONST
    KeywordList = KeywordArray {'AUTH', 'DATA', 'EHLO', 'EXPN', 'HELO', 'MAIL',
                                'NOOP', 'QUIT', 'RCPT', 'RSET', 'VRFY'};

CONST
    HandlerList = HandlerArray {AUTH, DATA, EHLO, EXPN, HELO, MAIL, NOOP, QUIT,
                                RCPT, RSET, VRFY};

(********************************************************************************)

PROCEDURE HandleCommand (S: Session;  VAR (*IN*) Command: ARRAY OF CHAR;
                                          VAR (*OUT*) Quit, ServerAbort: BOOLEAN);

    (* Executes one user command.  Returns with ServerAbort=TRUE if we have     *)
    (* unilaterally decided to kill the session.  Otherwise, returns with       *)
    (* Quit=TRUE if the command is one that closes the session, or if the       *)
    (* connection is lost.                                                      *)

    VAR k: [0..3];

    (****************************************************************************)

    PROCEDURE Compare4 (n: KeywordNumber): INTEGER;

        (* Compares the first four characters of Command with KeywordList[n].   *)
        (* Returns >0 if Command[0..3] > KeywordList[n], and so on.             *)

        VAR ch1, ch2: CHAR;

        BEGIN
            k := 0;
            LOOP
                ch1 := Command[k];  ch2 := KeywordList[n][k];
                IF ch1 > ch2 THEN RETURN +1
                ELSIF ch1 < ch2 THEN RETURN -1
                ELSIF k = 3 THEN RETURN 0
                END (*IF*);
                INC (k);
            END (*LOOP*);
        END Compare4;

    (****************************************************************************)

    VAR m: CARDINAL;  Match, QuitReceived: BOOLEAN;
        first, middle, last: CARDINAL;  test: INTEGER;
        Handler: HandlerProc;

    BEGIN
        (* Watch out for lower case. *)

        FOR k := 0 TO 3 DO
            Command[k] := CAP(Command[k]);
        END (*FOR*);

        (* Go through the keyword list to find a match with the command.  *)
        (* In this version I'm using a binary search.                     *)

        first := 0;  last := MAX(KeywordNumber);  Match := FALSE;
        LOOP
            middle := (first + last) DIV 2;
            test := Compare4 (middle);
            IF test < 0 THEN
                IF middle = 0 THEN
                    EXIT (*LOOP*);
                ELSE
                    last := middle - 1;
                END (*IF*);
            ELSIF test = 0 THEN
                Match := TRUE;  EXIT (*LOOP*);
            ELSIF test > 0 THEN
                first := middle + 1;
            END (*IF*);
            IF first > last THEN EXIT (*LOOP*) END (*IF*);
        END (*LOOP*);

        IF Match THEN
            Handler := HandlerList[middle];  S^.BadCommandCount := 0;
        ELSIF S^.BadCommandCount >= BadCommandLimit THEN
            Handler := GarbledCommandSequence;
        ELSE
            Handler := NoSuchCommand;  INC(S^.BadCommandCount);
        END (*IF*);
        QuitReceived := Handler = QUIT;

        (* Echo command to transaction log. *)

        LogTransaction (S^.ID, Command);

        IF (Handler <> NoSuchCommand) AND (Handler <> GarbledCommandSequence) THEN

            (* If the user is not yet logged in, only EHLO and HELO and QUIT are legal. *)

            IF NOT QuitReceived AND (S^.state <> LoggedIn) AND (Handler <> EHLO)
                                 AND (Handler <> HELO) THEN
                Handler := NotLoggedIn;
            END (*IF*);

            (* Strip out the command characters, leaving only the parameters. *)

            m := 4;
            WHILE (m < HIGH(Command)) AND (Command[m] = " ") DO INC(m) END (*WHILE*);
            Strings.Delete (Command, 0, m);

        END (*IF*);

        (* Call the handler. *);

        Handler (S, Command);
        ServerAbort := S^.state = MustAbort;
        IF ServerAbort THEN
            Reply (S, "421 Closing session");
            LogTransactionL (S^.ID, "Dropping connection");
        ELSIF (S^.state = MustExit) AND NOT QuitReceived THEN
            LogTransactionL (S^.ID, "Connection lost");
        END (*IF*);
        Quit := S^.state = MustExit;

    END HandleCommand;

(********************************************************************************)
(*                              INITIALISATION                                  *)
(********************************************************************************)

PROCEDURE SetMaxMessageSize;

    (* Gets maximum message size as determined from INI file. *)

    BEGIN
        MaxMessageSize := LimitOnMessageSize();
    END SetMaxMessageSize;

(********************************************************************************)

BEGIN
    AuthentMask := 0;
END SMTPCommands.

