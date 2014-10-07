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

IMPLEMENTATION MODULE MailAccounts;

        (********************************************************)
        (*                                                      *)
        (*      Looking up Weasel mail account information      *)
        (*      We keep this separate from the rest of the      *)
        (*      Weasel code because these procedures are        *)
        (*      needed for SMTP and IMAP authentication.        *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            8 March 2003                    *)
        (*  Last edited:        2 February 2013                 *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

IMPORT Strings, OS2, FileSys, INIData, Domains;

FROM SYSTEM IMPORT
    (* type *)  CARD8,
    (* proc *)  CAST;

FROM Domains IMPORT
    (* type *)  Domain, DomainSearchState,
    (* proc *)  StartDomainSearch, NextDomain, EndDomainSearch,
                OpenDomainINI, MailDirectoryFor, NameOfDomain;

FROM MyClock IMPORT
    (* proc *)  AppendTimeString, PackedCurrentDateTime;

FROM Names IMPORT
    (* type *)  UserName, PassString, HostName, DomainName, FilenameString;

FROM TransLog IMPORT
    (* type *)  LogContext, TransactionLogID,
    (* proc *)  CreateLogID, LogTransaction, LogTransactionL;

FROM Inet2Misc IMPORT
    (* proc *)  ToLower, ConvertCard;

FROM INIData IMPORT
    (* proc *)  OpenINIFile, CloseINIFile, INIValid, INIGet, INIGetString, INIPut;

FROM TaskControl IMPORT
    (* type *)  Lock,
    (* proc *)  CreateLock, DestroyLock, Obtain, Release;

FROM Heap IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    (* State while searching for all passwords that match a given user. *)

    PasswordSearchState = POINTER TO SSrecord;
    SSrecord = RECORD
                   head, current: DomainList;
               END (*RECORD*);

    (* Data for a local user.  Possible values for 'active' are:        *)
    (*   0   this user is absent or temporarily deactivated.            *)
    (*   1   normal user.                                               *)
    (*   2   don't deliver to the user's account, instead               *)
    (*       forward the mail to the CopyTo address.                    *)
    (*   3   deliver one copy to user, another to CopyTo.               *)
    (*                                                                  *)
    (* If active < 2, we always set CopyTo = ''.                        *)
    (*                                                                  *)
    (* The 'next' field allows us to maintain a list of currently       *)
    (* active LocalUser records, and the 'refcount' tells us how many   *)
    (* threads are currently using this record.  We discard the record  *)
    (* when the refcount gets down to zero.                             *)

    LocalUser = POINTER TO LUrecord;
    LUrecord = RECORD
                   next: LocalUser;
                   access: Lock;
                   refcount: CARDINAL;
                   username, CopyTo: UserName;
                   domain: Domain;
                   active: CARD8;
                   (*NextNameChanged: BOOLEAN;*)
                   OverrideFilter: BOOLEAN;
                   FinalFilterName: FilenameString;
                   DirName: FilenameString;
                   (*NextName: ARRAY [0..7] OF CHAR;*)
               END (*RECORD*);

    (* Circular list of domains.  For each domain we have a password.   *)
    (* This is the set of domains to which this user might belong.      *)
    (* We make it circular as an easy way to promote different elements *)
    (* to the list head without losing the others.                      *)

    DomainList = POINTER TO DomainRecord;
    DomainRecord = RECORD
                       next: DomainList;
                       domain: Domain;
                       pass: PassString;
                   END (*RECORD*);

(************************************************************************)

VAR
    (* Transaction log ID and flag for debugging. *)

    LogID: TransactionLogID;
    Debugging: BOOLEAN;

    (* Pointer to a linear list of all current LocalUser records, and   *)
    (* a critical section protection lock for this list.                *)

    LUChain: LocalUser;
    LUChainLock: Lock;

(************************************************************************)
(* BUILDING THE LIST OF DOMAINS TO WHICH A GIVEN USERNAME COULD BELONG  *)
(************************************************************************)

VAR debugmes: ARRAY [0..127] OF CHAR;

PROCEDURE BuildDomainList (VAR (*INOUT*) username: ARRAY OF CHAR;
                                      OurIPAddress: CARDINAL): DomainList;

    (* Strips the '@domain' part from the username, and builds a   *)
    (* circular list of domains to which this user could belong.   *)

    VAR found: BOOLEAN;  pos: CARDINAL;
        pass: PassString;  domainstring: DomainName;
        state: DomainSearchState;  D: Domain;
        head, this, last: DomainList;

    BEGIN
        IF Debugging THEN
            Strings.Assign ("Domain list for username ", debugmes);
            Strings.Append (username, debugmes);
            Strings.Append (":", debugmes);
            LogTransaction (LogID, debugmes);
        END (*IF*);

        (* Separate out the username into user@domain form.  We also    *)
        (* allow the forms user'domain and user%domain, and they take   *)
        (* precedence; i.e. if we find the ' then we don't look for %,  *)
        (* and if we find ' or % then we don't look for @.              *)

        Strings.FindNext ("'", username, 0, found, pos);
        IF NOT found THEN
            Strings.FindNext ("%", username, 0, found, pos);
            IF NOT found THEN
                Strings.FindNext ('@', username, 0, found, pos);
            END (*IF*);
        END (*IF*);
        IF found THEN
            Strings.Assign (username, domainstring);
            username[pos] := Nul;
            Strings.Delete (domainstring, 0, pos+1);
        ELSE
            domainstring[0] := Nul;
        END (*IF*);

        (* Build the list of domains to which this username might belong. *)

        head := NIL;
        last := NIL;
        (*LogTransactionL (LogID, "About to call StartDomainSearch");*)
        state := StartDomainSearch (username, domainstring, OurIPAddress);
        (*LogTransactionL (LogID, "Returned from StartDomainSearch");*)
        WHILE NextDomain (state, D, pass) DO
            NEW (this);
            this^.domain := D;
            this^.pass := pass;
            IF last = NIL THEN
                head := this;
            ELSE
                last^.next := this;
            END (*IF*);
            this^.next := head;
            last := this;
            IF Debugging THEN
                Strings.Assign ("domain=", debugmes);
                NameOfDomain (this^.domain, domainstring);
                Strings.Append (domainstring, debugmes);
                Strings.Append (", password=", debugmes);
                Strings.Append (this^.pass, debugmes);
                LogTransaction (LogID, debugmes);
            END (*IF*);
        END (*WHILE*);
        EndDomainSearch (state);

        IF Debugging THEN
            LogTransactionL (LogID, "End of domain list");
        END (*IF*);

        RETURN head;

    END BuildDomainList;

(************************************************************************)

PROCEDURE MakeSingletonList (D: Domain): DomainList;

    (* Creates a DomainList with a single domain in it, and Nul password. *)

    VAR result: DomainList;

    BEGIN
        NEW (result);
        result^.next := result;
        result^.domain := D;
        result^.pass[0] := Nul;
        RETURN result;
    END MakeSingletonList;

(************************************************************************)

PROCEDURE DiscardDomainList (VAR (*INOUT*) DL: DomainList);

    (* Disposes of the list of domain data. *)

    VAR current: DomainList;

    BEGIN
        IF DL <> NIL THEN

            (* First convert the circular list to a linear list; this   *)
            (* makes it easier to see that our code is correct.         *)

            current := DL;
            DL := DL^.next;
            current^.next := NIL;
            REPEAT
                current := DL;
                DL := DL^.next;
                DEALLOCATE (current, SIZE(DomainRecord));
            UNTIL DL = NIL;

        END (*IF*);

    END DiscardDomainList;

(************************************************************************)

PROCEDURE NonEmptyList (DL: DomainList): BOOLEAN;

    (* Returns TRUE iff DL contains at least one domain. *)

    BEGIN
        RETURN DL <> NIL;
    END NonEmptyList;

(************************************************************************)

PROCEDURE CurrentDomain (DL: DomainList): Domain;

    (* Returns the current head of the list. *)

    BEGIN
        RETURN DL^.domain;
    END CurrentDomain;

(************************************************************************)

PROCEDURE CurrentPassword (DL: DomainList;  VAR (*OUT*) password: PassString);

    (* Returns the password for the current head of the list. *)

    BEGIN
        Strings.Assign (DL^.pass, password);
    END CurrentPassword;

(************************************************************************)

PROCEDURE StepToNextDomain (VAR (*INOUT*) DL: DomainList);

    (* Moves forward one step in the circular list of domains. *)

    BEGIN
        DL := DL^.next;
    END StepToNextDomain;

(************************************************************************)
(*             PASSWORD VALIDITY CHECK, NO STATE RETAINED               *)
(************************************************************************)

PROCEDURE ConfirmPassword (OurIPAddress: CARDINAL;
                           VAR (*INOUT*) username: ARRAY OF CHAR;
                           VAR (*IN*) password: ARRAY OF CHAR;
                           VAR (*OUT*) domain: Domain): BOOLEAN;

    (* Returns TRUE iff this is a valid username/password pair.  Unlike *)
    (* normal POP3 operations, we do not retain the mailbox after the   *)
    (* check is done; this is a simple validity check.  If the search   *)
    (* is successful, the username has had its "@domain" part (if any)  *)
    (* removed, with the domain information instead being encoded in    *)
    (* the final parameter.                                             *)

    VAR current, head: DomainList;
        success: BOOLEAN;

    BEGIN
        success := FALSE;  domain := NIL;

        (* Build a circular list of domains to which this user  *)
        (* might belong.                                        *)

        ToLower (username);
        head := BuildDomainList (username, OurIPAddress);

        (* Search through the list for a password match.   *)

        IF head <> NIL THEN

            current := head;
            REPEAT
                IF Strings.Equal (password, current^.pass) THEN
                    domain := current^.domain;
                    success := TRUE;
                ELSE
                    current := current^.next;
                END (*IF*);
            UNTIL success OR (current = head);

        END (*IF*);

        (* Throw away the domain list before returning. *)

        DiscardDomainList (head);

        RETURN success;

    END ConfirmPassword;

(************************************************************************)
(*        PASSWORD CHECK WHEN WE ALSO WANT TO IDENTIFY THE DOMAIN       *)
(************************************************************************)

PROCEDURE StartPasswordSearch (VAR (*INOUT*) username: ARRAY OF CHAR;
                             OurIPAddress: CARDINAL): PasswordSearchState;

    (* Starts a search for a username/password pair, where the username *)
    (* is known but the password is not.  As a desired side-effect,     *)
    (* strips off the '@domain' part of the username if present.        *)

    VAR s: PasswordSearchState;

    BEGIN
        ToLower (username);
        NEW (s);
        s^.head := BuildDomainList (username, OurIPAddress);
        s^.current := s^.head;
        RETURN s;
    END StartPasswordSearch;

(************************************************************************)

PROCEDURE NextPassword (s: PasswordSearchState;
                                VAR (*OUT*) pass: ARRAY OF CHAR;
                                VAR (*OUT*) domain: Domain): BOOLEAN;

    (* Sets 'pass' to the next password that matches in this search.    *)
    (* Returns FALSE if there is no next password.                      *)

    BEGIN
        IF s^.current = NIL THEN
            pass[0] := Nul;
            domain := NIL;
            RETURN FALSE;
        END (*IF*);

        domain := s^.current^.domain;
        Strings.Assign (s^.current^.pass, pass);
        s^.current := s^.current^.next;
        IF s^.current = s^.head THEN
            s^.current := NIL;
        END (*IF*);
        RETURN TRUE;
    END NextPassword;

(************************************************************************)

PROCEDURE EndPasswordSearch (VAR (*INOUT*) s: PasswordSearchState);

    (* Discards the data structure used for a password search.  *)

    BEGIN
        DiscardDomainList (s^.head);
        DEALLOCATE(s, SIZE(SSrecord));
    END EndPasswordSearch;

(************************************************************************)
(*                         CREATING A TIMESTAMP                         *)
(************************************************************************)

PROCEDURE CreateTimeStamp (ID: TransactionLogID;
                                  VAR (*IN*) LocalHostName: HostName;
                                              VAR (*OUT*) result: FilenameString);

    (* Sets result to a string of the form <nnn.clock@hostname>, where nnn is   *)
    (* derived from the ID, clock is derived from the current date and time,    *)
    (* and hostname is our local host name.                                     *)

    VAR pos: CARDINAL;

    BEGIN
        result[0] := '<';  pos := 1;
        ConvertCard (CAST(CARDINAL, ID), result, pos);
        result[pos] := '.';  INC(pos);  result[pos] := Nul;
        AppendTimeString (result);
        Strings.Append ('@', result);
        Strings.Append (LocalHostName, result);
        Strings.Append ('>', result);
    END CreateTimeStamp;

(************************************************************************)
(*             THE DATA FOR A USER ONCE WE'VE IDENTIFIED HIM            *)
(************************************************************************)

PROCEDURE IsIMAPUser (VAR (*IN*) username: ARRAY OF CHAR;  domain: Domain): BOOLEAN;

    (* Returns TRUE iff this is an active user who may use IMAP. *)

    VAR hini: INIData.HINI;  size, code: CARDINAL;
        IMAPEnabled, result: BOOLEAN;
        key: ARRAY [0..7] OF CHAR;

    BEGIN
        result := FALSE;
        hini := OpenDomainINI (domain);

        IF INIData.INIValid (hini) THEN
            key[0] := Nul;
            IF INIData.ItemSize (hini, username, key, size)
                                   AND (size <> 0) THEN
                key := "UseIMAP";
                result := INIGet (hini, username, key, IMAPEnabled)
                          AND IMAPEnabled;
                IF result THEN
                    key := "Active";
                    result := NOT (INIGet (hini, username, key, code)
                                                           AND (code = 0));
                END (*IF*);
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);

        RETURN result;

    END IsIMAPUser;

(************************************************************************)

PROCEDURE CreateLUrecord (VAR (*IN*) username: ARRAY OF CHAR;
                                        domain: Domain): LocalUser;

    (* Creates a LocalUser record, initially with reference count = 1.  *)
    (* The caller must ensure that the operation is indivisible.        *)

    TYPE CharSet = SET OF CHAR;

    CONST Alphanumeric = CharSet {'0'..'9', 'A'..'Z'};

    VAR U: LocalUser;
        hini: INIData.HINI;  size: CARDINAL;
        key: ARRAY [0..15] OF CHAR;

    BEGIN
        U := NIL;
        hini := OpenDomainINI (domain);

        IF INIData.INIValid (hini) THEN
            key[0] := Nul;
            IF INIData.ItemSize (hini, username, key, size)
                                   AND (size <> 0) THEN
                NEW (U);
                U^.next := NIL;
                CreateLock (U^.access);
                U^.refcount := 1;
                key := "Active";
                IF NOT (INIGet (hini, username, key, U^.active)) THEN
                   U^.active := 1;
                END (*IF*);
                U^.CopyTo[0] := Nul;
                IF U^.active > 1 THEN
                    key := "CopyTo";
                    IF NOT INIGetString (hini, username, key, U^.CopyTo) THEN
                        U^.active := 1;
                    END (*IF*);
                END (*IF*);
                key := "OverrideFilter2";
                IF NOT INIGet (hini, username, key, U^.OverrideFilter) THEN
                    U^.OverrideFilter := FALSE;
                END (*IF*);
                key := "Filter2";
                IF NOT INIGetString (hini, username, key, U^.FinalFilterName) THEN
                    U^.FinalFilterName[0] := Nul;
                END (*IF*);
                (*
                U^.NextNameChanged := FALSE;
                key := "NextName";
                IF NOT INIGet (hini, username, key, U^.NextName) THEN
                    U^.NextName := "00000000";
                    U^.NextNameChanged := TRUE;
                END (*IF*);
                *)
            END (*IF*);
            CloseINIFile (hini);
        END (*IF*);

        IF U <> NIL THEN
            Strings.Assign (username, U^.username);
            U^.domain := domain;
            MailDirectoryFor (domain, U^.DirName);
            Strings.Append (username, U^.DirName);
            Strings.Append ('\', U^.DirName);
            IF U^.active < 2 THEN
                U^.CopyTo[0] := Nul;
            END (*IF*);

            (* Ensure that U^.NextName has the right format. *)
            (*
            FOR j := 0 TO 7 DO
                IF NOT (U^.NextName[j] IN Alphanumeric) THEN
                    U^.NextName[j] := '0';
                    U^.NextNameChanged := TRUE;
                END (*IF*);
            END (*FOR*);
            *)
        END (*IF*);

        RETURN U;

    END CreateLUrecord;

(************************************************************************)

PROCEDURE DiscardLUrecord (VAR (*INOUT*) U: LocalUser);

    (* Discards the in-memory copy of the information we have are  *)
    (* holding about this user.                                    *)
    (* The caller must ensure that the operation is indivisible.   *)

    (*
    VAR hini: INIData.HINI;
        key: ARRAY [0..8] OF CHAR;
    *)

    BEGIN
        IF U <> NIL THEN
            (*
            Obtain (U^.access);
            IF U^.NextNameChanged THEN
                hini := OpenDomainINI (U^.domain);
                IF INIValid(hini) THEN
                    key := "NextName";
                    INIPut (hini, U^.username, key, U^.NextName);
                    CloseINIFile (hini);
                END (*IF*);
            END (*IF*);
            Release (U^.access);
            *)
            DestroyLock (U^.access);
            DEALLOCATE (U, SIZE(LUrecord));
        END (*IF*);
    END DiscardLUrecord;

(************************************************************************)

PROCEDURE LoadLocalUser (VAR (*IN*) username: ARRAY OF CHAR;
                                    domain: Domain): LocalUser;

    (* Loads the user data for one user into memory. *)

    VAR U: LocalUser;

    BEGIN
        (* Look for the desired record in our master list.  Remark: it  *)
        (* can happen that another caller to this procedure can arrive  *)
        (* while we're still in the middle of constructing a user       *)
        (* record.  The LUChainLock guards not only the chain; it also  *)
        (* ensures that we don't interrupt a half-completed operation.  *)

        Obtain (LUChainLock);
        U := LUChain;
        WHILE (U <> NIL) AND NOT ((U^.domain = domain)
                             AND Strings.Equal(U^.username, username)) DO
            U := U^.next;
        END (*WHILE*);

        IF U = NIL THEN
            (* Create a new record, and link it into the chain.  Note   *)
            (* that the creation can fail on a "no such user" condition.*)

            U := CreateLUrecord (username, domain);
            IF U <> NIL THEN
                U^.next := LUChain;
                LUChain := U;
            END (*IF*);

        ELSE
            (* Return a pointer to a record that already existed. *)

            Obtain (U^.access);
            INC (U^.refcount);
            Release (U^.access);

        END (*IF*);

        Release (LUChainLock);
        RETURN U;

    END LoadLocalUser;

(************************************************************************)

PROCEDURE UnloadLocalUser (VAR (*INOUT*) U: LocalUser);

    (* Discards the in-memory copy of the information we have are  *)
    (* holding about this user.                                    *)

    VAR previous, p: LocalUser;

    BEGIN
        IF U <> NIL THEN

            (* Note: we must obtain LUChainLock BEFORE obtaining    *)
            (* U^.access, to avoid a potential deadlock.            *)

            Obtain (LUChainLock);
            Obtain (U^.access);
            DEC (U^.refcount);
            IF U^.refcount = 0 THEN

                (* Find the record in our master list. *)

                previous := NIL;  p := LUChain;
                WHILE (p <> NIL) AND (p <> U) DO
                    previous := p;  p := p^.next;
                END (*WHILE*);

                (* Unlink the record from the master list. *)

                IF p <> NIL THEN
                    IF previous = NIL THEN
                        LUChain := p^.next;
                    ELSE
                        previous^.next := p^.next;
                    END (*IF*);
                END (*IF*);

                (* Discard the record, regardless of whether it *)
                (* was on the chain.                            *)

                Release (U^.access);
                DiscardLUrecord (U);

            ELSE
                Release (U^.access);
            END (*IF*);

            Release (LUChainLock);

        END (*IF*);
    END UnloadLocalUser;

(************************************************************************)

PROCEDURE LockLocalUser (U: LocalUser);

    (* Critical section protection for operations on this user. *)

    BEGIN
        Obtain (U^.access);
    END LockLocalUser;

(************************************************************************)

PROCEDURE UnlockLocalUser (U: LocalUser);

    (* End of critical section. *)

    BEGIN
        Release (U^.access);
    END UnlockLocalUser;

(************************************************************************)

PROCEDURE IsActiveUser (U: LocalUser;
                        VAR (*OUT*) Active: CARDINAL;
                        VAR (*OUT*) CopyTo: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff U is a valid and active local user.             *)

    (* Possible values for 'Active' are:                                *)
    (*   0   this user is absent or temporarily deactivated; in this    *)
    (*       case we also return a FALSE result.                        *)
    (*   1   normal user.                                               *)
    (*   2   don't deliver to the user's account, instead               *)
    (*       forward the mail to the CopyTo address.                    *)
    (*   3   deliver one copy to user, another to CopyTo.               *)
    (*                                                                  *)
    (* If Active < 2, we always return with CopyTo = ''.                *)

    BEGIN
        IF U = NIL THEN
            Active := 0;
        ELSE
            Obtain (U^.access);
            Active := U^.active;
            Strings.Assign (U^.CopyTo, CopyTo);
            Release (U^.access);
        END (*IF*);
        RETURN Active > 0;
    END IsActiveUser;

(************************************************************************)

PROCEDURE SetFilterOverride (U: LocalUser;
                       VAR (*INOUT*) FilterName: FilenameString): BOOLEAN;

    (* Returns TRUE and alters FilterName iff this user has a filter    *)
    (* override.                                                        *)

    VAR override: BOOLEAN;

    BEGIN
        override := (U <> NIL) AND (U^.active > 0) AND U^.OverrideFilter;
        IF override THEN
            FilterName := U^.FinalFilterName;
        END (*IF*);
        RETURN override;
    END SetFilterOverride;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

(*
PROCEDURE MakeUniqueName (U: LocalUser;  VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments U^.NextName[N], with carry as appropriate. *)

        BEGIN
            IF U^.NextName[N] = '9' THEN
                U^.NextName[N] := 'A';
            ELSIF U^.NextName[N] = 'Z' THEN
                U^.NextName[N] := '0';
                IF N > 0 THEN
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (U^.NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Obtain (U^.access);
        Strings.Assign (U^.NextName, name);
        Increment (7);
        U^.NextNameChanged := TRUE;
        Release (U^.access);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE OldNewMessageFilename (U: LocalUser;
                              VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form U^.DirName||xxx||.MSG, where xxx *)
    (* is chosen such that a file of that name does not already exist.  *)
    (* Assumption: the caller has made this operation indivisible.      *)

    VAR UName: FilenameString;

    BEGIN
        REPEAT
            MakeUniqueName (U, UName);
            Strings.Assign (U^.DirName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (".MSG", NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END OldNewMessageFilename;
*)

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(*                  (NEW VERSION NOW BEING TESTED)                      *)
(************************************************************************)

PROCEDURE Base36 (N: CARDINAL): CHAR;

    (* Returns the base-36 encoding of N, assumed to be < 36. *)

    BEGIN
        IF N < 10 THEN
            RETURN CHR(ORD('0') +  N);
        ELSE
            RETURN CHR(ORD('A') +  N - 10);
        END (*IF*);
    END Base36;

(************************************************************************)

PROCEDURE TimeStampToString (code, suffixcount: CARDINAL;
                                 VAR (*OUT*) Name: ARRAY OF CHAR);

    (* Makes a 6-character timestamp string by base-36 encoding of      *)
    (* 'code', which is a 32-bit representation of the current          *)
    (* date/time.  If suffixcount > 0, appends up to two extra          *)
    (* characters to allow unique names to be generated even when more  *)
    (* than one request arrives before the binary timestamp ticks over. *)
    (* The caller must guarantee that suffixcount is small enough       *)
    (* that two characters will be sufficient.                          *)

    VAR j: INTEGER;

    BEGIN
        (* Generate the initial six characters. *)

        Name[6] := Nul;
        FOR j := 5 TO 0 BY -1 DO
            Name[j] := Base36 (code MOD 36);
            code := code DIV 36;
        END (*FOR*);

        (* Now the provision for collisions.  We can generate 36 unique *)
        (* one-character codes, and 36*36 two-character codes, so we    *)
        (* can handle a suffixcount in the range [1..36*37].  Bigger    *)
        (* suffixes don't occur -- the caller guarantees that.          *)

        IF suffixcount > 0 THEN

            DEC (suffixcount);

            IF suffixcount > 36 THEN
                (* We need a two-character suffix. *)
                DEC (suffixcount, 36);
                Name[6] := Base36 (suffixcount DIV 36);
                Name[7] := Base36 (suffixcount MOD 36);
                Name[8] := Nul;

            ELSE
                (* One base-36 digit is sufficient. *)
                Name[6] := Base36 (suffixcount);
                Name[7] := Nul;

            END (*IF*);

        END (*IF*);

    END TimeStampToString;

(************************************************************************)

PROCEDURE NewMessageFilename (U: LocalUser;
                                    VAR (*OUT*) NewName: ARRAY OF CHAR);

    (* Creates a file name of the form U^.DirName||xxx||.MSG, where xxx *)
    (* is chosen such that a file of that name does not already exist.  *)
    (* Assumption: the caller has made this operation indivisible.      *)

    CONST SuffixBound = 36*37;

    VAR BaseName: FilenameString;
        code, suffixcount: CARDINAL;
        conflict: BOOLEAN;

    BEGIN
        suffixcount := 0;

        (* Start with a 32-bit encoding of the current date and time.   *)
        (* It doesn't matter if the time ticks over in the inner loop   *)
        (* below; we don't need to get a new time encoding because      *)
        (* we're going to append a suffix to the basic code anyway.     *)

        code := PackedCurrentDateTime();

        REPEAT
            TimeStampToString (code, suffixcount, BaseName);
            Strings.Assign (U^.DirName, NewName);
            Strings.Append (BaseName, NewName);
            Strings.Append (".MSG", NewName);
            conflict := FileSys.Exists(NewName);

            (* Note: indivisibility of this procedure (which the caller *)
            (* must guarantee) implies that no other thread will be     *)
            (* trying to create a file in this user's directory with a  *)
            (* "*.MSG" file name.  Other threads might create temporary *)
            (* files, but they have different extensions.  Other        *)
            (* threads (e.g. a POP thread) might delete such files, but *)
            (* that is harmless to us.  Once we have found a filename   *)
            (* that doesn't yet exist, we can be certain that nobody    *)
            (* else will try to claim that filename until the caller    *)
            (* calls procedure UnlockLocalUser.                         *)

            IF conflict THEN
                INC (suffixcount);
                IF suffixcount >= SuffixBound THEN
                    INC (code);
                    DEC (suffixcount, SuffixBound);

                    (* Remark: in this case it is possible, even        *)
                    (* likely, that this incremented code is a timestamp*)
                    (* that a future caller will want to use.  If so we *)
                    (* could get a snowball effect where timestamps     *)
                    (* outstrip real time.  For that to be a problem,   *)
                    (* however, we would need a sustained arrival rate  *)
                    (* of at least 666 messages/sec for this one user,  *)
                    (* a figure that I find implausible given present   *)
                    (* network and mail-handling speeds.  Once the      *)
                    (* hardware gets faster we might need to look at    *)
                    (* going beyond the 8.3 filename format.            *)

                END (*WHILE*);
            END (*IF*);
        UNTIL NOT conflict;

    END NewMessageFilename;

(************************************************************************)
(*                         LOGGING FOR DEBUGGING                        *)
(************************************************************************)

PROCEDURE StartDebugLogging (ctx: LogContext);

    (* Temporary code for debugging the MailAccounts module. Starts     *)
    (* transaction logging for this module.                             *)

    BEGIN
        LogID := CreateLogID (ctx, "Account");
        Debugging := TRUE;
        (*Domains.StartDebugLogging (ctx);*)
    END StartDebugLogging;

(************************************************************************)

BEGIN
    Debugging := FALSE;
    LUChain := NIL;
    CreateLock (LUChainLock);
FINALLY
    DestroyLock (LUChainLock);
END MailAccounts.

