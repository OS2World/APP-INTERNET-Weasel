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

IMPLEMENTATION MODULE Authentication;

        (********************************************************)
        (*                                                      *)
        (*           SASL authentication mechanisms             *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            31 January 2003                 *)
        (*  Last edited:        23 July 2012                    *)
        (*  Status:             CHEAT,PLAIN,LOGIN,CRAM-MD5:     *)
        (*                             working                  *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings, Base64, OS2;

FROM MailAccounts IMPORT
    (* type *)  PasswordSearchState,
    (* proc *)  ConfirmPassword, CreateTimeStamp, StartPasswordSearch,
                NextPassword, EndPasswordSearch;

FROM Domains IMPORT
    (* type *)  Domain;

FROM MD5 IMPORT
    (* type *)  MD5_DigestType,
    (* proc *)  MD5DigestToString;

FROM HMAC IMPORT
    (* proc *)  HMAC_MD5;

FROM Inet2Misc IMPORT
    (* proc *)  AddressToHostName;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  OpenLogContext, CloseLogContext, CreateLogID, DiscardLogID;

FROM Names IMPORT
    (* type *)  UserName, HostName, FilenameString;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)
(*                                                                      *)
(*                SUPPORTED SASL AUTHENTICATION MECHANISMS              *)
(*                                                                      *)
(*   CHEAT      For testing, always gives success with no negotiation.  *)
(*   PLAIN      See RFC2595                                             *)
(*   LOGIN      (not covered by a standard, it seems, but still the     *)
(*                most popular method)                                  *)
(*   CRAM-MD5   See RFC2195                                             *)
(*                                                                      *)
(* SASL is described in RFC 2222, with applications to IMAP and with    *)
(* definitions of some mechanisms.  Its application to SMTP is defined  *)
(* in RFC2554.  Because we are using this module only for SMTP and      *)
(* IMAP4, we assume that challenge/response strings are Base64 encoded. *)
(*                                                                      *)
(************************************************************************)

CONST
    Nul = CHR(0);
    ParamStringLength = 512;
    CheatAccepted = FALSE;          (* TRUE for testing *)

TYPE
    AuthMethod = (cheat, plain, login, crammd5, dummy);

    MethodName = ARRAY [0..20] OF CHAR;
    MethodNameArray = ARRAY AuthMethod OF MethodName;
    MethodEnabledArray = ARRAY AuthMethod OF BOOLEAN;

    ParamString = ARRAY [0..ParamStringLength-1] OF CHAR;

    (* The fields in an authentication state record are:                *)
    (*   mechanism     The authentication method                        *)
    (*   OurIPAddress  IP address of the server                         *)
    (*   user          the username for which we are doing the          *)
    (*                   authentication, for those methods that involve *)
    (*                   a username                                     *)
    (*   domain        the domain of this user; set after we have       *)
    (*                   managed to authenticate this user              *)
    (*   stage         the number of challenge/response steps we have   *)
    (*                   so far negotiated, for the more complex checks *)
    (*   cache         data needed to be saved from one stage to next   *)
    (*   done          TRUE iff we have completed all stages            *)
    (*   success       TRUE iff we have passed the authentication test  *)

    AuthenticationState = POINTER TO AuthRecord;
    AuthRecord = RECORD
                     mechanism: AuthMethod;
                     OurIPAddress: CARDINAL;
                     user: UserName;
                     domain: Domain;
                     stage: CARDINAL;
                     cache: ParamString;
                     done, success: BOOLEAN;
                 END (*RECORD*);

(************************************************************************)

CONST
    MNames = MethodNameArray {"CHEAT", "PLAIN", "LOGIN", "CRAM-MD5", ""};

VAR
    MethodEnabled: MethodEnabledArray;

(************************************************************************)
(*                      THE 'CHEAT' MECHANISM                           *)
(*              For testing: always returns success.                    *)
(************************************************************************)

PROCEDURE FCCheat (state: AuthenticationState;
                          VAR (*IN*) initial: ARRAY OF CHAR);

    BEGIN
        initial[0] := initial[0];     (* to avoid a compiler warning *)
        state^.done := TRUE;
        state^.success := TRUE;
    END FCCheat;

(************************************************************************)
(*                      THE 'PLAIN' MECHANISM                           *)
(************************************************************************)

PROCEDURE RcPlain (state: AuthenticationState;
                          VAR (*IN*) response: ARRAY OF CHAR);

    (* The response string is supposed to be a Base64 encoding of       *)
    (*       loginid<NUL>username<NUL>password                          *)
    (* where loginid is always ignored in our case.                     *)

    CONST MaxParamLength = 3*512;

    VAR pos: CARDINAL;

    (********************************************************************)

    PROCEDURE GetString (VAR (*OUT*) result: ARRAY OF CHAR);

        (* Loads a string from response[pos], updates pos. *)

        VAR j: CARDINAL;

        BEGIN
            j := 0;
            WHILE (pos < MaxParamLength) AND (response[pos] <> Nul) DO
                IF j <= HIGH(result) THEN
                    result[j] := response[pos];  INC(j);
                END (*IF*);
                INC (pos);
            END (*WHILE*);
            IF pos < HIGH(response) THEN
                INC (pos);
            END (*IF*);
            IF j <= HIGH(result) THEN
                result[j] := Nul;
            END (*IF*);
        END GetString;

    (********************************************************************)

    VAR loginid, password: ParamString;

    BEGIN
        pos := 0;
        GetString (loginid);
        GetString (state^.user);
        GetString (password);
        state^.success := (state^.user[0] <> Nul)
                          AND ConfirmPassword (state^.OurIPAddress,
                                    state^.user, password, state^.domain);
        state^.done := TRUE;
    END RcPlain;

(************************************************************************)

PROCEDURE FCPlain (state: AuthenticationState;
                          VAR (*IN*) initial: ARRAY OF CHAR);

    (* The initial string is supposed to be a Base64 encoding of        *)
    (*       loginid<NUL>username<NUL>password                          *)
    (* where loginid is always ignored in our case.  In the IMAP case   *)
    (* the initial string might not be supplied until after the first   *)
    (* challenge, so we have to allow for an empty initial string.      *)

    BEGIN
        IF (initial[0] = Nul) AND (initial[1] = Nul) THEN
            state^.done := FALSE;
        ELSE
            RcPlain (state, initial);
        END (*IF*);
    END FCPlain;

(************************************************************************)
(*                      THE 'LOGIN' MECHANISM                           *)
(************************************************************************)

PROCEDURE FCLogin (state: AuthenticationState;
                          VAR (*IN*) initial: ARRAY OF CHAR);

    (* The initial string is normally empty, but I'll allow for the     *)
    (* possibility of a username being supplied.                        *)

    BEGIN
        IF initial[0] <> Nul THEN
            Strings.Assign (initial, state^.user);
            state^.stage := 1;
        END (*IF*);
        state^.success := FALSE;
        state^.done := FALSE;
    END FCLogin;

(************************************************************************)

PROCEDURE ChLogin (state: AuthenticationState;
                          VAR (*OUT*) challenge: ARRAY OF CHAR);

    BEGIN
        IF state^.stage = 0 THEN
            Strings.Assign ("Username:", challenge);
        ELSE
            Strings.Assign ("Password:", challenge);
        END (*IF*);
    END ChLogin;

(************************************************************************)

PROCEDURE RcLogin (state: AuthenticationState;
                                VAR (*IN*) response: ARRAY OF CHAR);

    BEGIN
        IF state^.stage = 0 THEN
            (* Save the username. *)
            Strings.Assign (response, state^.user);
            state^.stage := 1;
        ELSE
            (* The response is the password, so we can now do the check. *)
            state^.success := (state^.user[0] <> Nul)
                       AND ConfirmPassword (state^.OurIPAddress,
                                     state^.user, response, state^.domain);
            state^.done := TRUE;
        END (*IF*);
    END RcLogin;

(************************************************************************)
(*                     THE 'CRAM-MD5' MECHANISM                         *)
(************************************************************************)

PROCEDURE ChCramMD5 (state: AuthenticationState;
                          VAR (*OUT*) challenge: ARRAY OF CHAR);

    VAR LocalHostName: HostName;
        ctx: LogContext;
        ID: TransactionLogID;
        TimeStamp: FilenameString;

    BEGIN
        ctx := OpenLogContext();
        ID := CreateLogID (ctx, "CRAM-MD5");
        AddressToHostName (state^.OurIPAddress, LocalHostName);
        CreateTimeStamp (ID, LocalHostName, TimeStamp);
        Strings.Assign (TimeStamp, challenge);
        Strings.Assign (TimeStamp, state^.cache);
        DiscardLogID (ID);
        CloseLogContext (ctx);
    END ChCramMD5;

(************************************************************************)

PROCEDURE RcCramMD5 (state: AuthenticationState;
                     VAR (*IN*) response: ARRAY OF CHAR);

    (* The response is username<SP>digest, where digest is the result   *)
    (* of a keyed-MD5 encoding of the timestamp, using the user's       *)
    (* password as the key.                                             *)

    VAR digest: MD5_DigestType;
        txtdigest: ParamString;
        digeststr, pass: ARRAY [0..31] OF CHAR;
        pos: CARDINAL;  found: BOOLEAN;
        s: PasswordSearchState;

    BEGIN
        state^.success := FALSE;
        Strings.FindNext (' ', response, 0, found, pos);
        IF found THEN
            Strings.Assign (response, txtdigest);
            response[pos] := Nul;
            Strings.Assign (response, state^.user);
            Strings.Delete (txtdigest, 0, pos+1);
            s := StartPasswordSearch (state^.user, state^.OurIPAddress);
            WHILE NOT state^.success
                          AND NextPassword (s, pass, state^.domain) DO

                (* Compute the digest with this password, and see    *)
                (* whether the result matches.                       *)

                HMAC_MD5 (state^.cache, LENGTH(state^.cache),
                          pass, LENGTH(pass), digest);
                MD5DigestToString (digest, digeststr);
                state^.success := Strings.Equal (digeststr, txtdigest);

            END (*WHILE*);
            EndPasswordSearch (s);
        END (*IF*);
        state^.done := TRUE;
    END RcCramMD5;

(************************************************************************)
(*                        THE DUMMY MECHANISM                           *)
(*    Used as a sentinel; can also be used as the 'do nothing' cases.   *)
(************************************************************************)

PROCEDURE FCDummy (state: AuthenticationState;  VAR initial: ARRAY OF CHAR);

    BEGIN
        initial[0] := initial[0];     (* to avoid a compiler warning *)
        state^.done := FALSE;
    END FCDummy;

(************************************************************************)

PROCEDURE ChDummy (state: AuthenticationState;
                          VAR (*OUT*) challenge: ARRAY OF CHAR);

    BEGIN
        (* Dummy rubbish to avoid a compiler warning. *)

        IF state <> NIL THEN
            challenge[0] := Nul;
        END (*IF*);

    END ChDummy;

(************************************************************************)
(*                       THE DISPATCHER ARRAYS                          *)
(************************************************************************)

TYPE
    ChallengeType = ARRAY AuthMethod OF
                         PROCEDURE (AuthenticationState, VAR ARRAY OF CHAR);

CONST
    FirstCheck = ChallengeType {FCCheat, FCPlain, FCLogin, FCDummy, FCDummy};
    Challenge = ChallengeType {ChDummy, ChDummy, ChLogin, ChCramMD5, ChDummy};
    ResponseCheck = ChallengeType {FCDummy, RcPlain, RcLogin, RcCramMD5, FCDummy};

(************************************************************************)
(*                STARTING A NEW AUTHENTICATION CHECK                   *)
(************************************************************************)

PROCEDURE GetAuthNames (VAR (*OUT*) list: ARRAY OF CHAR; labelled: BOOLEAN);

    (* Sets list to a space-separated list of mechanism names.  If the  *)
    (* list is nonempty, it always starts with a space character.  If   *)
    (* labelled is specified, each name is preceded by 'AUTH='.         *)

    VAR k: AuthMethod;

    BEGIN
        list[0] := Nul;
        FOR k := MAX(AuthMethod) TO MIN(AuthMethod) BY -1 DO
            IF MethodEnabled[k] THEN
                Strings.Append (' ', list);
                IF labelled THEN
                    Strings.Append ('AUTH=', list);
                END (*IF*);
                Strings.Append (MNames[k], list);
            END (*IF*);
        END (*FOR*);

    END GetAuthNames;

(************************************************************************)

PROCEDURE StartAuthentication (VAR (*OUT*) state: AuthenticationState;
                               ServerIPaddress, mask: CARDINAL;
                               VAR (*IN*) mechanism,
                                   initialstring: ARRAY OF CHAR): BOOLEAN;

    (* Starts an authentication exchange.  The initialstring is the     *)
    (* initial response from the client, if this is a mechanism that    *)
    (* requires the client to start the exchange, but it will be the    *)
    (* null string for most mechanisms.  Returns FALSE if we don't      *)
    (* support the mechanism or if the corresponding bit of 'mask'      *)
    (* is zero.                                                         *)

    VAR method: AuthMethod;  pos: CARDINAL;
        param: ParamString;

    BEGIN
        Strings.Capitalize (mechanism);
        method := MIN(AuthMethod);
        WHILE (method < MAX(AuthMethod))
                        AND NOT Strings.Equal (mechanism, MNames[method]) DO
            INC (method);
            mask := mask DIV 2;
        END (*WHILE*);
        IF NOT (MethodEnabled[method] AND ODD(mask)) THEN
            state := NIL;
            RETURN FALSE;
        ELSE
            NEW (state);
            WITH state^ DO
                mechanism := method;
                OurIPAddress := ServerIPaddress;
                user := "";
                domain := NIL;
                stage := 0;
                cache := "";
                done := FALSE;
                success := FALSE;
            END (*WITH*);

            (* Complication: although initialstring is always an ASCII  *)
            (* string if it's non-empty, the decoded version can        *)
            (* contain Nul characters, which makes it hard to tell the  *)
            (* difference between an empty string and one whose first   *)
            (* character is the Nul character.  To get around this, we  *)
            (* adopt the special convention, for this initial check     *)
            (* only, that an empty string is represented by TWO initial *)
            (* Nul characters.                                          *)

            IF initialstring[0] = Nul THEN
                param[0] := Nul;  param[1] := Nul;
            ELSE
                pos := Base64.Decode (initialstring, param);
                IF pos < ParamStringLength THEN
                    param[pos] := Nul;
                END (*IF*);
            END (*IF*);

            FirstCheck[method] (state, param);
            RETURN TRUE;

        END (*IF*);
    END StartAuthentication;

(************************************************************************)
(*              THE GENERIC CHALLENGE/RESPONSE PROCEDURES               *)
(************************************************************************)

PROCEDURE AuthenticationIncomplete (state: AuthenticationState;
                                    VAR (*OUT*) success: BOOLEAN): BOOLEAN;

    (* Returns TRUE iff the authentication operation still has more     *)
    (* steps.  When the result is finally FALSE, the success parameter  *)
    (* says whether the authentication is successful.                   *)

    BEGIN
        IF state = NIL THEN
            success := FALSE;
            RETURN FALSE;
        ELSE
            success := state^.success;
            IF state^.done THEN
                RETURN FALSE;
            ELSE
                RETURN TRUE;
            END (*IF*);
        END (*IF*);
    END AuthenticationIncomplete;

(************************************************************************)

PROCEDURE CreateNextChallenge (state: AuthenticationState;
                               VAR (*OUT*) challenge: ARRAY OF CHAR);

    (* Generates the next challenge that the server must send. *)

    VAR param: ParamString;

    BEGIN
        Challenge[state^.mechanism] (state, param);
        Base64.Encode (param, LENGTH(param), challenge);
    END CreateNextChallenge;

(************************************************************************)

PROCEDURE CheckResponse (state: AuthenticationState;
                                VAR (*IN*) response: ARRAY OF CHAR);

    (* Checks whether the response is valid, and updates the internal   *)
    (* state such that AuthenticationIncomplete will show the outcome.  *)

    VAR param: ParamString;  pos: CARDINAL;

    BEGIN
        pos := Base64.Decode (response, param);
        IF pos < ParamStringLength THEN
            param[pos] := Nul;
        END (*IF*);
        ResponseCheck[state^.mechanism] (state, param);
    END CheckResponse;

(************************************************************************)

PROCEDURE AuthenticationDone (VAR (*INOUT*) state: AuthenticationState;
                              VAR (*OUT*)   username: ARRAY OF CHAR;
                              VAR (*OUT*)   domain: Domain);

    (* Disposes of the state record, and tells the caller the name and  *)
    (* domain of the user who has been authenticated.                   *)

    BEGIN
        IF state = NIL THEN
            Strings.Assign ("", username);  domain := NIL;
        ELSE
            Strings.Assign (state^.user, username);
            domain := state^.domain;
            DEALLOCATE (state, SIZE(AuthRecord));
        END (*IF*);
    END AuthenticationDone;

(************************************************************************)
(*                           INITIALISATION                             *)
(************************************************************************)

PROCEDURE SetAuthMethods (mask: CARDINAL);

    (* Initialisation procedure: the mask is a Boolean bit-mask that    *)
    (* specifies which of the AUTH mechanisms are enabled.              *)

    VAR k: AuthMethod;

    BEGIN
        FOR k := MIN(AuthMethod) TO MAX(AuthMethod) DO
            MethodEnabled[k] := ODD(mask);
            mask := mask DIV 2;
        END (*FOR*);
        MethodEnabled[cheat] := CheatAccepted;

    END SetAuthMethods;

(************************************************************************)

BEGIN
    (* By default, all methods are enabled except for CHEAT and the     *)
    (* dummy method.                                                    *)

    MethodEnabled := MethodEnabledArray{FALSE, TRUE, TRUE, TRUE, FALSE};

END Authentication.

