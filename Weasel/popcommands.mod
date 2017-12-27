(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

IMPLEMENTATION MODULE POPCommands;

        (********************************************************)
        (*                                                      *)
        (*       Command interpreter for POP3 server            *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            21 April 1998                   *)
        (*  Last edited:        22 May 2017                     *)
        (*  Status:             Working                         *)
        (*                                                      *)
        (********************************************************)

(********************************************************************************)
(*                        COMPLIANCE WITH THE STANDARD                          *)
(********************************************************************************)
(*                                                                              *)
(* I'm working from the POP3 standard RFC1939                                   *)
(* All commmands from that standard are now implemented:                        *)
(*                                                                              *)
(*    APOP, DELE, LIST, NOOP, PASS, QUIT, RETR, RSET, STAT, TOP, UIDL, USER     *)
(*                                                                              *)
(* Additional commands:                                                         *)
(*                                                                              *)
(*    XTND XMIT   (not doing anything with it yet)                              *)
(*    LAST        (obsolete, but apparently Yahoo requires it)                  *)
(*                    (support for LAST withdrawn 26 March 2009)                *)
(*    AUTH        (RFC 1734)                                                    *)
(*    CAPA        (RFC 2449)                                                    *)
(*                                                                              *)
(********************************************************************************)


FROM SYSTEM IMPORT CAST, LOC, CARD8;

IMPORT Strings, Delivery;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  EVAL;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, Obtain, Release;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Semaphores IMPORT
    (* type *)  Semaphore;

FROM Conversions IMPORT
    (* proc *)  StringToCardinal;

FROM SBuffers IMPORT
    (* type *)  SBuffer,
    (* proc *)  SocketOf, GetLine, SendLine, FlushOutput;

FROM Names IMPORT
    (* type *)  FilenameString, UserName, UserNameIndex, HostName, DomainName;

FROM TimeConv IMPORT
    (* proc *)  time;

FROM MyClock IMPORT
    (* proc *)  CurrentTimeToString;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType;

FROM MiscFuncs IMPORT
    (* proc *)  SplitArg, AddEOL, ConvertCard;

FROM Inet2Misc IMPORT
    (* proc *)  IPToString;

FROM Authentication IMPORT
    (* type *)  AuthenticationState,
    (* proc *)  GetAuthNames, StartAuthentication, AuthenticationIncomplete,
                CreateNextChallenge, CheckResponse,
                AuthenticationDone;

FROM POPData IMPORT
    (* type *)  Mailbox,
    (* proc *)  OpenMailbox, DiscardMailbox, PasswordOK, NumberAndSize,
                SizeOfMessage, SendMessage, MarkForDeletion, UndeleteAll,
                CommitChanges, MaxMessageNumber, GetUID, APOPCheck,
                ClaimMailbox, MarkForThrottling;

FROM Domains IMPORT
    (* type *)  Domain, NameOfDomain;

FROM MailAccounts IMPORT
    (* proc *)  CreateTimeStamp;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenAtEnd, CloseFile, FWriteChar, FWriteString, FWriteLJCard, FWriteLn;

FROM TransLog IMPORT
    (* type *)  TransactionLogID,
    (* proc *)  LogTransaction, LogTransactionL;

(********************************************************************************)

CONST
    Nul = CHR(0);
    BadCommandLimit = 3;

TYPE
    FourChar = ARRAY [0..3] OF CHAR;

    ClientState = (Idle, LoggedIn, MustExit);

    (* The session record.  The fields are:                             *)
    (*     ID          a session identifier for transaction logging     *)
    (*     sbuffer     The socket buffer                                *)
    (*     HostAddr    Our own IP address                               *)
    (*     ClientAddr  The IP address of the client                     *)
    (*     state       To track whether the user is currently logged in.*)
    (*     mailbox     Information about the user's mailbox.            *)
    (*     watchdog    A semaphore on which we have to signal on slow   *)
    (*                  transfers so as to avoid timeout.               *)
    (*     badpasscount  The number of times the client has given an    *)
    (*                   incorrect password in this session             *)
    (*     BadCommandCount  Number of invalid commands in sequence      *)
    (*     retrcount   Number of messages the client has retrieved      *)
    (*     retrchars   Number of characters retrieved                   *)
    (*     delecount   Number of messages the client has deleted        *)
    (*     delechars   Number of characters deleted                     *)
    (*     tdelcount   Tentative values for delecount and delechars,    *)
    (*     tdelchars     - deletions not yet committed                  *)
    (*     username    the name with which this user logged in          *)
    (*                      (needed only for logging)                   *)
    (*     domain      the user's domain name (needed only for logging) *)

    Session = POINTER TO SessionRecord;
    SessionRecord = RECORD
                        ID: TransactionLogID;
                        TimeStamp: FilenameString;
                        sbuffer: SBuffer;
                        HostAddr, ClientAddr: CARDINAL;
                        state: ClientState;
                        mailbox: Mailbox;
                        watchdog: Semaphore;
                        badpasscount, BadCommandCount: CARDINAL;
                        retrcount, retrchars,
                          delecount, delechars,
                          tdelcount, tdelchars: CARDINAL;
                        username: UserName;
                        domain: Domain;
                    END (*RECORD*);

    (* Records for POP-before-SMTP authorisation.  Each POP login       *)
    (* creates one of these records, and they are checked for some      *)
    (* (not all) arriving SMTP connections.  To keep the list clean,    *)
    (* we remove expired entries as a side-effect of each check.        *)

    AuthListPointer = POINTER TO AuthEntry;
    AuthEntry = RECORD
                    next: AuthListPointer;
                    time: CARDINAL;
                    address: CARDINAL;
                END (*RECORD*);

(********************************************************************************)

VAR
    (* LogPOPusers is TRUE iff we are keeping a user log. *)

    LogPOPusers: BOOLEAN;

    (* Exclusive access lock for the user log. *)

    LogFileLock: Lock;

    (* Name of the POP user log file. *)

    PopLogName: FilenameString;

    (* List of POP-before-SMTP authorisations, ordered by expiry time.    *)

    AuthList: RECORD
                  lock: Lock;
                  head: AuthListPointer;
              END (*RECORD*);

    (* Time, in minutes, that a POP-before-SMTP authorisation remains valid.    *)
    (* This form of authorisation is disabled if the time is zero.  We use      *)
    (* AuthList.lock as the critical section protection for AuthTime.           *)

    AuthTime: CARDINAL;

    (* Limit on the number of password failures before we forcibly terminate    *)
    (* the session.  A value of 0 disables this check.                          *)

    MaxBadPassCount: CARDINAL;

(********************************************************************************)
(*                     LIMIT ON NUMBER OF BAD PASSWORDS                         *)
(********************************************************************************)

PROCEDURE SetBadPasswordLimit (limit: CARDINAL);

    (* Sets the limit on the number of password failures before we forcibly     *)
    (* terminate a POP3 session.  A value of 0 disables this check.             *)

    BEGIN
        MaxBadPassCount := limit;
    END SetBadPasswordLimit;

(********************************************************************************)
(*                               USER LOGGING                                   *)
(********************************************************************************)

PROCEDURE SetPopLogName (enable: BOOLEAN;  VAR (*IN*) name: FilenameString);

    (* Sets the filename of the POP user log. *)

    BEGIN
        LogPOPusers := enable;
        PopLogName := name;
    END SetPopLogName;

(********************************************************************************)

PROCEDURE WriteLogData (S: Session);

    (* Writes the summary for this session to the user log. *)

    VAR cid: ChanId;  datetime: ARRAY [0..31] OF CHAR;
        dname: DomainName;

    (****************************************************************************)

    PROCEDURE TimeStampandUser;

        (* Writes timestamp, followed by user@domain. *)

        BEGIN
            FWriteString (cid, datetime);  FWriteString (cid, " ");
            FWriteString (cid, S^.username);
            FWriteChar (cid, '@');
            IF dname[0] <> Nul THEN
                FWriteString (cid, dname);
            END (*IF*);
        END TimeStampandUser;

    (****************************************************************************)

    VAR IPBuffer: ARRAY [0..16] OF CHAR;

    BEGIN
        Obtain (LogFileLock);
        cid := OpenAtEnd (PopLogName);
        CurrentTimeToString (datetime);
        NameOfDomain (S^.domain, dname);

        IF S^.retrcount > 0 THEN
            FWriteString (cid, datetime);  FWriteChar (cid, " ");

            (* Write timestamp user@domain [IPaddr] *)

            TimeStampandUser;
            FWriteChar (cid, " ");
            IPToString (S^.ClientAddr, TRUE, IPBuffer);
            FWriteString (cid, IPBuffer);

            FWriteString (cid, " retrieved ");
            FWriteLJCard (cid, S^.retrcount);  FWriteString (cid, " files (");
            FWriteLJCard (cid, S^.retrchars);  FWriteString (cid, " bytes)");
            FWriteLn (cid);
        END (*IF*);

        IF S^.delecount > 0 THEN
            TimeStampandUser;
            FWriteString (cid, " deleted ");
            FWriteLJCard (cid, S^.delecount);  FWriteString (cid, " files (");
            FWriteLJCard (cid, S^.delechars);  FWriteString (cid, " bytes)");
            FWriteLn (cid);
        END (*IF*);

        CloseFile (cid);
        Release (LogFileLock);
    END WriteLogData;

(********************************************************************************)
(*                         STARTING A NEW SESSION                               *)
(********************************************************************************)

PROCEDURE AppendTimeStamp (S: Session;  VAR (*INOUT*) buffer: ARRAY OF CHAR);

    (* Appends the TimeStamp string to a string buffer. *)

    BEGIN
        Strings.Append (S^.TimeStamp, buffer);
    END AppendTimeStamp;

(********************************************************************************)

PROCEDURE OpenSession (SB: SBuffer;  HostIPAddress, ClientIPAddress: CARDINAL;
                                    KeepAlive: Semaphore;
                                    LogID: TransactionLogID): Session;

    (* Creates a new session state record.  During lengthy operations           *)
    (* we have to do a Signal(KeepAlive) every so often in order to stop the    *)
    (* session from timing out.                                                 *)

    VAR result: Session;
        LocalHostName: HostName;

    BEGIN
        LocalHostName := "[127.0.0.0]";            (* fallback default *)
        Delivery.GetOurHostName (SocketOf(SB), LocalHostName);
        NEW (result);
        WITH result^ DO
            ID := LogID;
            mailbox := NIL;
            sbuffer := SB;
            HostAddr := HostIPAddress;
            ClientAddr := ClientIPAddress;
            state := Idle;
            CreateTimeStamp (ID, LocalHostName, TimeStamp);
            watchdog := KeepAlive;
            badpasscount := 0;
            BadCommandCount := 0;
            retrcount := 0;  retrchars := 0;
            delecount := 0;  delechars := 0;
            tdelcount := 0;  tdelchars := 0;
            username := "";
            domain := NIL;
        END (*WITH*);
        RETURN result;
    END OpenSession;

(********************************************************************************)

PROCEDURE CloseSession (S: Session);

    (* Destroys the session state record. *)

    BEGIN
        EVAL (FlushOutput (S^.sbuffer));
        IF LogPOPusers AND ((S^.retrcount > 0) OR (S^.delecount > 0)) THEN
            WriteLogData (S);
        END (*IF*);
        DiscardMailbox (S^.mailbox);
        DEALLOCATE (S, SIZE(SessionRecord));
    END CloseSession;

(********************************************************************************)
(*                     POP-BEFORE-SMTP RELAY AUTHORISATION                      *)
(********************************************************************************)

PROCEDURE SetPOPParameters (AuthTimeLimit: CARDINAL);

    (* Sets the time (in minutes) that a POP-before-SMTP authorisation remains  *)
    (* valid.  A zero value disables this form of authorisation.                *)

    BEGIN
        Obtain (AuthList.lock);
        AuthTime := AuthTimeLimit;
        Release (AuthList.lock);
    END SetPOPParameters;

(********************************************************************************)

PROCEDURE ClearObsoleteAuthorisations(): CARDINAL;

    (* Removes expired entries on the POP-before-SMTP authorisation list.       *)
    (* We assume that the caller has exclusive access to this list.  Also       *)
    (* returns the current time, measured in minutes from an arbitrary origin.  *)

    VAR current: AuthListPointer;
        now: CARDINAL;

    BEGIN
        (* Current time, to nearest minute. *)

        now := (time() + 30) DIV 60;

        current := AuthList.head;
        WHILE (current <> NIL) AND (current^.time < now) DO
            AuthList.head := current^.next;
            DEALLOCATE (current, SIZE(AuthEntry));
            current := AuthList.head;
        END (*WHILE*);

        RETURN now;

    END ClearObsoleteAuthorisations;

(********************************************************************************)

PROCEDURE AddToAuthList (endtime, IPAddress: CARDINAL);

    (* Inserts a new entry into the POP-before-SMTP authorisation list. *)
    (* We assume that the caller has exclusive access to this list.     *)

    VAR previous, current, next: AuthListPointer;

    BEGIN
        previous := NIL;
        current := AuthList.head;
        LOOP
            IF current = NIL THEN EXIT(*LOOP*) END(*IF*);
            next := current^.next;
            IF current^.address = IPAddress THEN

                (* We have an earlier entry for the same address. *)
                (* Get rid of it, it's now obsolete.              *)

                IF previous = NIL THEN
                    AuthList.head := next;
                ELSE
                    previous^.next := next;
                END (*IF*);
                DEALLOCATE (current, SIZE(AuthEntry));

            ELSIF current^.time > endtime THEN
                EXIT (*LOOP*);
            ELSE
                previous := current;
            END (*IF*);
            current := next;

        END (*LOOP*);

        (* We've found the right insertion point.  Now add a new list   *)
        (* entry between previous and current.                          *)

        NEW (current);
        current^.address := IPAddress;
        current^.time := endtime;
        IF previous = NIL THEN
            current^.next := AuthList.head;
            AuthList.head := current;
        ELSE
            current^.next := previous^.next;
            previous^.next := current;
        END (*IF*);

    END AddToAuthList;

(********************************************************************************)

PROCEDURE RefreshSMTPAuthorisation (IPAddress: CARDINAL);

    (* Notes that there has been a successful POP login from this address.      *)

    VAR now: CARDINAL;

    BEGIN
        Obtain (AuthList.lock);
        IF AuthTime > 0 THEN
            now := ClearObsoleteAuthorisations();
            AddToAuthList (now + AuthTime, IPAddress);
        END (*IF*);
        Release (AuthList.lock);
    END RefreshSMTPAuthorisation;

(********************************************************************************)

PROCEDURE POPbeforeSMTP (IPAddress: CARDINAL): BOOLEAN;

    (* Returns TRUE iff a client at that address has done a successful POP      *)
    (* login and the POP-before-SMTP timeout has not yet expired.               *)

    VAR current: AuthListPointer;
        success: BOOLEAN;

    BEGIN
        success := FALSE;
        Obtain (AuthList.lock);
        IF AuthTime > 0 THEN
            EVAL (ClearObsoleteAuthorisations());
            current := AuthList.head;
            WHILE (current <> NIL) AND NOT success DO
                success := current^.address = IPAddress;
                current := current^.next;
            END (*WHILE*);
        END (*IF*);
        Release (AuthList.lock);

        RETURN success;

    END POPbeforeSMTP;

(********************************************************************************)
(*                       SENDING REPLY BACK TO CLIENT                           *)
(********************************************************************************)

PROCEDURE Reply2 (session: Session;  message1, message2: ARRAY OF CHAR);

    (* Sends all of message1, followed by message2, followed by end-of-line.    *)
    (* If the operation fails, session^.state is set to MustExit.               *)

    VAR buffer: ARRAY [0..511] OF CHAR;
        sent: CARDINAL;

    BEGIN
        Strings.Assign ("> ", buffer);
        Strings.Append (message1, buffer);
        Strings.Append (message2, buffer);
        LogTransaction (session^.ID, buffer);
        Strings.Delete (buffer, 0, 2);
        IF NOT SendLine (session^.sbuffer, buffer, sent) THEN
            session^.state := MustExit;
        END (*IF*);
        EVAL (FlushOutput (session^.sbuffer));
    END Reply2;

(********************************************************************************)

PROCEDURE Reply (session: Session;  message: ARRAY OF CHAR);

    (* Like Reply2, except that there is no message2. *)

    VAR buffer: ARRAY [0..511] OF CHAR;
        sent: CARDINAL;

    BEGIN
        Strings.Assign ("> ", buffer);
        Strings.Append (message, buffer);
        LogTransaction (session^.ID, buffer);
        Strings.Delete (buffer, 0, 2);
        IF NOT SendLine (session^.sbuffer, buffer, sent) THEN
            session^.state := MustExit;
        END (*IF*);
        EVAL (FlushOutput (session^.sbuffer));
    END Reply;

(********************************************************************************)
(*                     HANDLERS FOR SOME ERROR CONDITIONS                       *)
(********************************************************************************)

PROCEDURE NoSuchCommand (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Command is not a recognised command. *)

    BEGIN
        Sleep (3000);
        Reply2 (session, "-ERR Unknown command ", Command);
    END NoSuchCommand;

(********************************************************************************)

PROCEDURE NotLoggedIn (session: Session;  VAR (*IN*) Command: ARRAY OF CHAR);

    (* Command is illegal because user is not yet logged in. *)

    BEGIN
        Reply2 (session, "-ERR Not logged in ", Command);
    END NotLoggedIn;

(********************************************************************************)

PROCEDURE GarbledCommandSequence (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    (* Too many unknown commands have been received. *)

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "-ERR Too many bad commands, closing connection");
        session^.state := MustExit;
    END GarbledCommandSequence;

(********************************************************************************)
(*                     HANDLERS FOR THE INDIVIDUAL COMMANDS                     *)
(********************************************************************************)

PROCEDURE APOP (session: Session;  VAR (*IN*) args: ARRAY OF CHAR);

    VAR name: UserName;  PasswordIsBad: BOOLEAN;

    BEGIN
        (* Extract the username. *)

        SplitArg (name, args);

        (* Open the mailbox, check that username is valid. *)

        session^.state := Idle;
        PasswordIsBad := FALSE;
        session^.domain := NIL;
        IF OpenMailbox (session^.mailbox, name, session^.HostAddr) THEN

            (* Check the digest string in args. *)

            session^.username := name;
            CASE APOPCheck (session^.mailbox, session^.ID, args,
                                      session^.TimeStamp, session^.domain) OF
               |  0:  session^.state := LoggedIn;
                      RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "+OK logged in");
               |  1:  PasswordIsBad := TRUE;
               |  2:  RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "-ERR mailbox is already locked");
               |  3:  Reply (session, "-ERR configuration error");
            END (*CASE*);
        ELSE
            PasswordIsBad := TRUE;
        END (*IF*);

        IF PasswordIsBad THEN
            INC (session^.badpasscount);
            IF (MaxBadPassCount > 0)
                         AND (session^.badpasscount >= MaxBadPassCount) THEN
                Reply (session, "-ERR too many retries, disconnecting");
                MarkForThrottling (session^.ClientAddr);
                session^.state := MustExit;
            ELSE
                Reply (session, "-ERR authorisation failure");
            END (*IF*);
            Sleep (3000);
        END (*IF*);

    END APOP;

(********************************************************************************)

PROCEDURE AUTH (session: Session;  VAR (*IN*) args: ARRAY OF CHAR);

    VAR mechanism: ARRAY [0..20] OF CHAR;
        message, logline:  ARRAY [0..511] OF CHAR;
        state: AuthenticationState;
        working, authenticated: BOOLEAN;
        username: UserName;

    BEGIN
        authenticated := FALSE;
        session^.domain := NIL;
        IF session^.state = LoggedIn THEN
            Reply (session, "-ERR Already logged in");
        ELSE
            SplitArg (mechanism, args);
            IF mechanism[0] = Nul THEN
                Reply (session, "-ERR No authentication method specified");
            ELSE
                working := TRUE;
                IF StartAuthentication (state, session^.HostAddr, MAX(CARDINAL),
                                                                  mechanism, args) THEN
                    WHILE working AND AuthenticationIncomplete (state, authenticated) DO
                        CreateNextChallenge (state, message);
                        Reply2 (session, "+ ", message);
                        IF GetLine (session^.sbuffer, message) THEN
                            Strings.Assign ("< ", logline);
                            Strings.Append (message, logline);
                            LogTransaction (session^.ID, logline);
                            IF (message[0] = '*') AND (message[1] = Nul) THEN
                                Reply (session, "-ERR Authentication cancelled");
                                working := FALSE;
                            ELSE
                                CheckResponse (state, message);
                            END (*IF*);
                        ELSE
                            Reply (session, "-ERR Communication lost");
                            session^.state := MustExit;
                            working := FALSE;
                        END (*IF*);
                    END (*WHILE*);
                    IF working AND NOT authenticated THEN
                        INC (session^.badpasscount);
                        IF (MaxBadPassCount > 0)
                                AND (session^.badpasscount >= MaxBadPassCount) THEN
                            Reply (session, "-ERR too many retries, disconnecting");
                            MarkForThrottling (session^.ClientAddr);
                            session^.state := MustExit;
                        ELSE
                            Reply (session, "-ERR Authentication failed");
                        END (*IF*);
                        Sleep (3000);
                    END (*IF*);

                ELSE
                    Reply2 (session, "-ERR unsupported authentication mechanism ", mechanism);
                END (*IF*);
                AuthenticationDone (state, username, session^.domain);
            END (*IF*);
        END (*IF*);
        IF authenticated THEN
            CASE ClaimMailbox (session^.mailbox, session^.ID, username,
                                                        session^.domain) OF
               |  0:  session^.state := LoggedIn;
                      Strings.Assign (username, session^.username);
                      RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "+OK logged in");
               |  1:  Reply (session, "-ERR internal server error");
               |  2:  RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "-ERR mailbox is already locked");
               |  3:  Reply (session, "-ERR mailbox file does not exist");
            END (*CASE*);
        END (*IF*);
    END AUTH;

(********************************************************************************)

PROCEDURE CAPA (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    VAR list: ARRAY [0..255] OF CHAR;

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "+OK Capability list follows");
        Reply (session, "TOP");
        Reply (session, "USER");
        GetAuthNames (list, FALSE);
        IF list[0] <> Nul THEN
            Reply2 (session, "SASL", list);
        END (*IF*);
        Reply (session, "UIDL");
        Reply (session, ".");
    END CAPA;

(********************************************************************************)

PROCEDURE DELE (session: Session;  VAR (*IN*) number: ARRAY OF CHAR);

    VAR N, ItemSize: CARDINAL;

    BEGIN
        N := StringToCardinal (number);
        IF MarkForDeletion (session^.mailbox, N, ItemSize) THEN
            INC (session^.tdelcount);  INC (session^.tdelchars, ItemSize);
            Reply (session, "+OK message deleted");
        ELSE
            Reply (session, "-ERR no such message");
        END (*IF*);
    END DELE;

(********************************************************************************)

PROCEDURE LAST (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        Reply (session, "-ERR Deprecated POP2 command no longer supported");
    END LAST;

(********************************************************************************)

PROCEDURE LIST (session: Session;  VAR (*IN*) number: ARRAY OF CHAR);

    CONST buffersize = 128;

    VAR N, size, pos: CARDINAL;
        buffer: ARRAY [0..buffersize-1] OF CHAR;

    (****************************************************************************)

    PROCEDURE SendNumberAndSize;

        (* Appends the values of N and size to buffer, then sends buffer. *)

        BEGIN
            pos := LENGTH (buffer);
            ConvertCard (N, buffer, pos);
            buffer[pos] := ' ';  INC(pos);
            ConvertCard (size, buffer, pos);
            IF pos < buffersize THEN
                buffer[pos] := Nul;
            END (*IF*);
            Reply (session, buffer);
        END SendNumberAndSize;

    (****************************************************************************)

    BEGIN
        IF number[0] = Nul THEN

            (* List all. *)

            Reply (session, "+OK");
            FOR N := 1 TO MaxMessageNumber(session^.mailbox) DO
                buffer := "";
                IF SizeOfMessage (session^.mailbox, N, size) THEN
                    SendNumberAndSize;
                END (*IF*);
            END (*FOR*);
            Reply (session, ".");

        ELSE

            (* List for message "number". *)

            N := StringToCardinal (number);
            IF SizeOfMessage (session^.mailbox, N, size) THEN
                Strings.Assign ("+OK ", buffer);
                SendNumberAndSize;
            ELSE
                Reply (session, "-ERR No such message");
            END (*IF*);

        END (*IF*);

    END LIST;

(********************************************************************************)

PROCEDURE NOOP (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];     (* to avoid a compiler warning *)
        Reply (session, "+OK");
    END NOOP;

(********************************************************************************)

PROCEDURE PASS (session: Session;  VAR (*IN*) password: ARRAY OF CHAR);

    VAR PasswordIsBad: BOOLEAN;

    BEGIN
        IF session^.state = LoggedIn THEN
            Reply (session, "-ERR already logged in");
        ELSE
            session^.state := Idle;
            PasswordIsBad := FALSE;
            CASE PasswordOK (session^.mailbox, password, session^.domain, session^.ID) OF
               |  0:  session^.state := LoggedIn;
                      RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "+OK logged in");
               |  1:  PasswordIsBad := TRUE;
               |  2:  RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "-ERR mailbox is already locked");
               |  3:  RefreshSMTPAuthorisation (session^.ClientAddr);
                      Reply (session, "-ERR configuration error");
            END (*CASE*);

            IF PasswordIsBad THEN
                INC (session^.badpasscount);
                IF (MaxBadPassCount > 0)
                           AND (session^.badpasscount >= MaxBadPassCount) THEN
                    Reply (session, "-ERR too many retries, disconnecting");
                    MarkForThrottling (session^.ClientAddr);
                    session^.state := MustExit;
                ELSE
                    Reply (session, "-ERR authorisation failure");
                END (*IF*);
                Sleep (3000);
            END (*IF*);

        END (*IF*);

    END PASS;

(********************************************************************************)

PROCEDURE QUIT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        WITH session^ DO
            IF state = LoggedIn THEN
                CommitChanges (mailbox);
                delecount := tdelcount;  delechars := tdelchars;
            END (*IF*);
        END (*WITH*);
        Reply (session, "+OK");
        session^.state := MustExit;
    END QUIT;

(********************************************************************************)

PROCEDURE RETR (session: Session;  VAR (*IN*) number: ARRAY OF CHAR);

    VAR N, pos, size, bytessent: CARDINAL;
        message: ARRAY [0..31] OF CHAR;

    BEGIN
        N := StringToCardinal (number);
        IF SizeOfMessage (session^.mailbox, N, size) THEN
            message := "+OK ";  pos := 4;
            ConvertCard (size, message, pos);  message[pos] := Nul;
            Strings.Append (" bytes", message);
            Reply (session, message);
            IF SendMessage (session^.sbuffer, session^.watchdog,
                            session^.mailbox, N, MAX(CARDINAL),
                             bytessent, session^.ID) THEN
                INC (session^.retrcount);  INC (session^.retrchars, bytessent);
                pos := 0;  ConvertCard (bytessent, message, pos);  message[pos] := Nul;
                Strings.Append (" bytes sent", message);
                LogTransaction (session^.ID, message);
            Strings.Append (" bytes", message);

            ELSE
                session^.state := MustExit;
            END (*IF*);
        ELSE
            Reply (session, "-ERR no such message");
        END (*IF*);
    END RETR;

(********************************************************************************)

PROCEDURE RSET (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        UndeleteAll (session^.mailbox);
        session^.tdelcount := 0;  session^.tdelchars := 0;
        Reply (session, "+OK");
    END RSET;

(********************************************************************************)

PROCEDURE STAT (session: Session;  VAR (*IN*) dummy: ARRAY OF CHAR);

    CONST buffersize = 128;

    VAR N, size, pos: CARDINAL;
        buffer: ARRAY [0..buffersize-1] OF CHAR;

    BEGIN
        dummy[0] := dummy[0];                   (* to avoid a compiler warning *)
        NumberAndSize (session^.mailbox, N, size);
        Strings.Assign ("+OK ", buffer);
        pos := 4;
        ConvertCard (N, buffer, pos);
        buffer[pos] := ' ';  INC(pos);
        ConvertCard (size, buffer, pos);
        IF pos < buffersize THEN
            buffer[pos] := Nul;
        END (*IF*);
        Reply (session, buffer);
    END STAT;

(********************************************************************************)

PROCEDURE TOP (session: Session;  VAR (*IN*) Params: ARRAY OF CHAR);

    VAR N, size, lines, dummy: CARDINAL;

    BEGIN
        N := StringToCardinal (Params);
        size := 1;
        WHILE Params[size] <> ' ' DO
            INC (size);
        END (*WHILE*);
        Strings.Delete (Params, 0, size+1);
        lines := StringToCardinal (Params);
        IF SizeOfMessage (session^.mailbox, N, size) THEN
            Reply (session, "+OK");
            IF NOT SendMessage (session^.sbuffer, session^.watchdog,
                            session^.mailbox, N, lines, dummy, session^.ID) THEN
                session^.state := MustExit;
            END (*IF*);
        ELSE
            Reply (session, "-ERR no such message");
        END (*IF*);
    END TOP;

(********************************************************************************)

PROCEDURE UIDL (session: Session;  VAR (*IN*) number: ARRAY OF CHAR);

    CONST buffersize = 64;

    VAR N, pos: CARDINAL;
        buffer: ARRAY [0..buffersize-1] OF CHAR;
        UID: MD5_DigestType;

    (****************************************************************************)

    PROCEDURE PutHexDigit (N: CARD8);

        (* Puts a single hexadecimal digit at buffer[pos], updates pos. *)

        BEGIN
            IF N < 10 THEN
                buffer[pos] := CHR(N + ORD('0'));
            ELSE
                buffer[pos] := CHR(N - 10 + ORD('A'));
            END (*IF*);
            INC (pos);
        END PutHexDigit;

    (****************************************************************************)

    PROCEDURE ConvertUID (array: ARRAY OF LOC);

        (* Converts a 16-byte code to a 32-char hexadecimal string, stores      *)
        (* the result at buffer[pos], updates pos.                              *)

        VAR j: [0..15];  val: CARD8;

        BEGIN
            FOR j := 0 TO 15 DO
                val := CAST(CARD8, array[j]);
                PutHexDigit (val DIV 16);
                PutHexDigit (val MOD 16);
            END (*FOR*);
        END ConvertUID;

    (****************************************************************************)

    PROCEDURE SendNumberAndUID;

        (* Appends the values of N and size to buffer, then sends buffer. *)

        BEGIN
            pos := LENGTH (buffer);
            ConvertCard (N, buffer, pos);
            buffer[pos] := ' ';  INC(pos);
            ConvertUID (UID);
            IF pos < buffersize THEN
                buffer[pos] := Nul;
            END (*IF*);
            Reply (session, buffer);
        END SendNumberAndUID;

    (****************************************************************************)

    BEGIN
        IF number[0] = Nul THEN

            (* UIDL for everything in the mailbox, excluding deleted messages. *)

            Reply (session, "+OK");
            FOR N := 1 TO MaxMessageNumber(session^.mailbox) DO
                buffer := "";
                IF GetUID (session^.mailbox, N, UID, session^.ID) THEN
                    SendNumberAndUID;
                END (*IF*);
            END (*FOR*);
            Reply (session, ".");

        ELSE

            (* List for message "number". *)

            N := StringToCardinal (number);
            IF GetUID (session^.mailbox, N, UID, session^.ID) THEN
                Strings.Assign ("+OK ", buffer);
                SendNumberAndUID;
            ELSE
                Reply (session, "-ERR No such message");
            END (*IF*);

        END (*IF*);

    END UIDL;

(********************************************************************************)

PROCEDURE USER (session: Session;  VAR (*IN*) username: ARRAY OF CHAR);

    BEGIN
        session^.state := Idle;
        IF OpenMailbox (session^.mailbox, username, session^.HostAddr) THEN
            Strings.Assign (username, session^.username);
        END (*IF*);

        (* For security reasons, we don't tell the client whether the   *)
        (* username was invalid.                                        *)

        Reply (session, "+OK send password");
    END USER;

(********************************************************************************)

PROCEDURE HeadMatch (kwd: ARRAY OF CHAR;
                         VAR (*IN*) Line: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff Line starts with kwd.   *)
    (* Character case is ignored.               *)

    VAR j: CARDINAL;  match: BOOLEAN;

    BEGIN
        j := 0;  match := TRUE;
        LOOP
            IF (j > HIGH(kwd)) OR (kwd[j] = Nul) THEN
                EXIT (*LOOP*);
            ELSIF (j > HIGH(Line)) OR (CAP(kwd[j]) <> CAP(Line[j])) THEN
                match := FALSE;
                EXIT (*LOOP*);
            ELSE
                INC (j);
            END (*IF*);
        END (*LOOP*);
        RETURN match;
    END HeadMatch;

(************************************************************************)

PROCEDURE XTND (session: Session;  VAR (*IN*) param: ARRAY OF CHAR);

    (* Experimental. *)

    VAR answer: ARRAY [0..255] OF CHAR;

    BEGIN
        IF HeadMatch ("XMIT", param) THEN
            Reply (session, "-ERR Sorry, XTND XMIT not implemented here");
        ELSE
            Strings.Assign ("-ERR don't understand subcommand ", answer);
            Strings.Append (param, answer);
            Reply (session, answer);
        END (*IF*);
    END XTND;

(********************************************************************************)
(*                      THE MAIN COMMAND DISPATCHER                             *)
(********************************************************************************)

TYPE
    KeywordNumber = [0..15];
    HandlerProc = PROCEDURE (Session, VAR (*IN*) ARRAY OF CHAR);
    HandlerArray = ARRAY KeywordNumber OF HandlerProc;
    KeywordArray = ARRAY KeywordNumber OF FourChar;

CONST
    KeywordList = KeywordArray {'APOP', 'AUTH', 'CAPA', 'DELE', 'LAST', 'LIST',
                                'NOOP', 'PASS', 'QUIT', 'RETR', 'RSET', 'STAT',
                                'TOP ', 'UIDL', 'USER', 'XTND'};

CONST
    HandlerList = HandlerArray {APOP, AUTH, CAPA, DELE, LAST, LIST, NOOP, PASS,
                                QUIT, RETR, RSET, STAT, TOP, UIDL, USER, XTND};

(********************************************************************************)

PROCEDURE HandleCommand (S: Session;  VAR (*IN*) Command: ARRAY OF CHAR;
                                                     VAR (*OUT*) Quit: BOOLEAN);

    (* Executes one user command.  Returns with Quit=TRUE if the command is one *)
    (* that closes the session, or if the connection is lost.                   *)

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
        (* Special case: add a space to a three-character command, to make the  *)
        (* search algorithm simpler.                                            *)

        IF Command[3] = CHR(0) THEN
            Command[3] := ' ';
            Command[4] := CHR(0);
        END (*IF*);

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

        IF Handler = PASS THEN
            LogTransactionL (S^.ID, "< PASS ******");
        ELSE
            Strings.Insert ("< ", 0, Command);
            LogTransaction (S^.ID, Command);
            Strings.Delete (Command, 0, 2);
        END (*IF*);

        IF (Handler <> NoSuchCommand) AND (Handler <> GarbledCommandSequence) THEN

            (* If the user is not yet logged in, only AUTH, CAPA, APOP,  *)
            (* USER, PASS, and QUIT are legal.                           *)

            IF NOT QuitReceived AND (S^.state <> LoggedIn) AND (Handler <> APOP)
                                AND (Handler <> AUTH) AND (Handler <> CAPA)
                                AND (Handler <> USER) AND (Handler <> PASS) THEN
                Handler := NotLoggedIn;
            END (*IF*);

            (* Strip out the command characters, leaving only the parameters. *)

            m := 4;
            WHILE (m < HIGH(Command)) AND (Command[m] = " ") DO INC(m) END (*WHILE*);
            Strings.Delete (Command, 0, m);

        END (*IF*);

        (* Call the handler. *);

        Handler (S, Command);
        IF (S^.state = MustExit) AND NOT QuitReceived THEN
            LogTransactionL (S^.ID, "Connection lost");
        END (*IF*);
        Quit := S^.state = MustExit;

    END HandleCommand;

(********************************************************************************)

BEGIN
    MaxBadPassCount := 4;
    CreateLock (LogFileLock);
    AuthTime := 0;
    WITH AuthList DO
        head := NIL;
        CreateLock (lock);
    END (*WITH*);
    LogPOPusers := FALSE;
END POPCommands.

