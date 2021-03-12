(**************************************************************************)
(*                                                                        *)
(*  Setup for Weasel mail server                                          *)
(*  Copyright (C) 2020   Peter Moylan                                     *)
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

IMPLEMENTATION MODULE SUDomains;

    (****************************************************************)
    (*                                                              *)
    (*                      PM Setup for Weasel                     *)
    (*                     Operations on domains                    *)
    (*                                                              *)
    (*        Started:        11 January 2002                       *)
    (*        Last edited:    2 May 2020                            *)
    (*        Status:         OK                                    *)
    (*                                                              *)
    (****************************************************************)


IMPORT Strings, OS2, FileSys;

FROM SYSTEM IMPORT
    (* proc *)  ADR;

FROM SUPage1 IMPORT
    (* proc *)  CurrentMailRoot;

FROM Languages IMPORT
    (* type *)  LangHandle,
    (* proc *)  StrToBufferA;

FROM Names IMPORT
    (* type *)  FilenameString, DomainName, UserName;

FROM RINIData IMPORT
    (* type *)  StringReadState,
    (* proc *)  OpenINIFile, CloseINIFile, MakeDirectory, MoveDirectory,
                DeleteFile, DeleteDirectory,
                GetStringList, NextString, CloseStringList,
                ItemSize, INIFetchBinary, INIPutBinary, INIDeleteApp,
                INIDeleteKey, INIGetString, INIPutString,
                INIPut, INIFetch;

FROM PMInit IMPORT
    (* proc *)  WarningBox;

FROM MiscFuncs IMPORT
    (* type *)  CharArrayPointer, LocArrayPointer,
    (* proc *)  EVAL;

FROM FileOps IMPORT
    (* type *)  ChanId,
    (* proc *)  OpenNewFile, CloseFile, Exists;

FROM Timer IMPORT
    (* proc *)  Sleep;

FROM Storage IMPORT
    (* proc *)  ALLOCATE, DEALLOCATE;

FROM LowLevel IMPORT
    (* proc *)  IAND;

(************************************************************************)

CONST
    Nul = CHR(0);
    DefaultOriginalName = "Please rename this to your real domain name";

TYPE
    DomainList = POINTER TO RECORD
                    next: DomainList;
                    name: DomainName;
                END (*RECORD*);

VAR
    MailRoot: FilenameString;
    MasterINIFile: FilenameString;
    OriginalDomainName: DomainName;
    UseTNI: BOOLEAN;

(************************************************************************)
(*       MANIPULATING THE "Domains" ENTRY IN THE MASTER INI FILE        *)
(************************************************************************)

PROCEDURE FirstDomainName (VAR (*OUT*) first: DomainName);

    (* Returns the first name in our ist of domain names.   *)

    VAR state: StringReadState;

    BEGIN
        IF OpenINIFile (MasterINIFile) THEN
            GetStringList ("$SYS", "Domains", state);
            NextString (state, first);
            CloseStringList (state);
            CloseINIFile;
        END (*IF*);
    END FirstDomainName;

(************************************************************************)

PROCEDURE NoDomainsDefined(): BOOLEAN;

    (* Returns TRUE if our INI file records no domains. *)

    VAR first: DomainName;

    BEGIN
        FirstDomainName (first);
        RETURN first[0] = Nul;
    END NoDomainsDefined;

(************************************************************************)

PROCEDURE DomainExists (domainname: DomainName): BOOLEAN;

    (* Returns TRUE iff this domain exists. *)

    VAR state: StringReadState;
        current: DomainName;

    BEGIN
        IF (domainname[0] = Nul) OR NOT OpenINIFile (MasterINIFile) THEN
            RETURN FALSE;
        END (*IF*);
        GetStringList ("$SYS", "Domains", state);
        REPEAT
            NextString (state, current);
        UNTIL (current[0] = Nul) OR (Strings.Equal(current, domainname));
        CloseStringList (state);
        CloseINIFile;
        RETURN current[0] <> Nul;
    END DomainExists;

(************************************************************************)

PROCEDURE AddDomain (domainname: DomainName);

    (* Adds this domain to the "Domains" INI file entry. *)

    VAR size1, size2, k: CARDINAL;
        bufptr: CharArrayPointer;

    BEGIN
        IF OpenINIFile (MasterINIFile) THEN
            EVAL (ItemSize ("$SYS", "Domains", size1));
            IF size1 > 0 THEN
                DEC (size1);
            END (*IF*);
            size2 := size1 + LENGTH(domainname) + 2;
            ALLOCATE (bufptr, size2);
            EVAL (INIFetchBinary ("$SYS", "Domains", bufptr^, size2));
            FOR k := 0 TO LENGTH(domainname)-1 DO
                bufptr^[k+size1] := domainname[k];
            END (*FOR*);
            bufptr^[size2-2] := Nul;
            bufptr^[size2-1] := Nul;
            INIPutBinary ("$SYS", "Domains", bufptr^, size2);
            DEALLOCATE (bufptr, size2);
            CloseINIFile;
        END (*IF*);
    END AddDomain;

(************************************************************************)

PROCEDURE LoadDomainList(): DomainList;

    (* Loads the list of domain names from the INI file. *)

    VAR size: CARDINAL;
        state: StringReadState;
        current: DomainName;
        dlist, tail, p: DomainList;
        originalfound: BOOLEAN;

    BEGIN
        dlist := NIL;  tail := NIL;
        size := 1;
        originalfound := FALSE;
        IF OpenINIFile (MasterINIFile) THEN
            GetStringList ("$SYS", "Domains", state);
            REPEAT
                NextString (state, current);
                IF (current[0] <> Nul) THEN
                    IF Strings.Equal (current, OriginalDomainName) THEN
                        originalfound := TRUE;
                    END (*IF*);
                    INC (size, LENGTH(current)+1);
                    NEW (p);
                    p^.next := NIL;
                    p^.name := current;
                    IF tail = NIL THEN
                        dlist := p;
                    ELSE
                        tail^.next := p;
                    END (*IF*);
                    tail := p;
                END (*IF*);
            UNTIL current[0] = Nul;
            CloseStringList (state);
            CloseINIFile;
        END (*IF*);

        IF NOT originalfound THEN
            (* Set OriginalDomainName to the first domain on the list.  *)
            (* If there is no first domain, leave OriginalDomainName    *)
            (* unchanged.                                               *)

            IF (dlist <> NIL) AND (dlist^.name[0] <> Nul) THEN
                Strings.Assign (dlist^.name, OriginalDomainName);
            END (*IF*);

        END (*IF*);

        RETURN dlist;
    END LoadDomainList;

(************************************************************************)

PROCEDURE StrOfStrSize (dlist: DomainList): CARDINAL;

    (* Calculates the number of characters needed to store dlist in     *)
    (* the INI file.                                                    *)

    VAR size: CARDINAL;

    BEGIN
        size := 1;
        WHILE dlist <> NIL DO
            INC (size, LENGTH(dlist^.name)+1);
            dlist := dlist^.next;
        END (*WHILE*);
        RETURN size;
    END StrOfStrSize;

(************************************************************************)

PROCEDURE StoreDomainList (VAR (*INOUT*) dlist: DomainList);

    (* Writes back the domain list to the INI file, also discards the   *)
    (* list.                                                            *)

    VAR size, j, k: CARDINAL;
        p: DomainList;
        current: DomainName;
        bufptr: CharArrayPointer;

    BEGIN

        size := StrOfStrSize(dlist);
        ALLOCATE (bufptr, size);
        k := 0;
        WHILE dlist <> NIL DO
            p := dlist;  dlist := p^.next;
            current := p^.name;
            DISPOSE (p);
            FOR j := 0 TO LENGTH(current)-1 DO
                bufptr^[k] := current[j];  INC(k);
            END (*FOR*);
            bufptr^[k] := Nul;  INC(k);
        END (*WHILE*);
        bufptr^[k] := Nul;

        IF OpenINIFile (MasterINIFile) THEN
            INIPutBinary ("$SYS", "Domains", bufptr^, size);
            CloseINIFile;
        END (*IF*);

        DEALLOCATE (bufptr, size);

    END StoreDomainList;

(************************************************************************)

PROCEDURE ChangeDomainName (oldname, newname: DomainName);

    (* Alters one name in the "Domains" INI file entry. *)

    VAR dlist: DomainList;
        done: BOOLEAN;

    BEGIN
        dlist := LoadDomainList();

        done := dlist <> NIL;
        WHILE NOT done DO
            IF Strings.Equal (dlist^.name, oldname) THEN
                dlist^.name := newname;
                done := TRUE;
            ELSE
                done := dlist^.next = NIL;
            END (*IF*);
            dlist := dlist^.next;
        END (*WHILE*);

        StoreDomainList(dlist);

        IF Strings.Equal (oldname, OriginalDomainName) THEN
            OriginalDomainName := newname;
        END (*IF*);

    END ChangeDomainName;

(************************************************************************)

PROCEDURE SubtractDomain (domainname: DomainName);

    (* Removes this domain from the "Domains" INI file entry. *)

    VAR prev, dlist, p, next: DomainList;
        done: BOOLEAN;

    BEGIN
        dlist := LoadDomainList();

        IF domainname = OriginalDomainName THEN
            IF (dlist <> NIL) AND (dlist^.name[0] <> Nul) THEN
                Strings.Assign (dlist^.name, OriginalDomainName);
            END (*IF*);
        END (*IF*);

        done := dlist <> NIL;
        prev := NIL;
        p := dlist;
        WHILE NOT done DO
            next := p^.next;
            IF p^.name = domainname THEN
                IF prev = NIL THEN
                    dlist := next;
                ELSE
                    prev^.next := next;
                END (*IF*);
                done := TRUE;
            ELSE
                prev := p;
                done := next = NIL;
            END (*IF*);
            p := next;
        END (*WHILE*);

        StoreDomainList(dlist);

    END SubtractDomain;

(************************************************************************)
(*                   SHUFFLING USERS BETWEEN DOMAINS                    *)
(************************************************************************)

PROCEDURE SetMailRoot (CurrentMailRoot: FilenameString);

    (* The caller gives us an updated value of the mail root. *)

    BEGIN
        Strings.Assign (CurrentMailRoot, MailRoot);
    END SetMailRoot;

(************************************************************************)

PROCEDURE MoveOneUser (name: UserName;  INIFile1, INIFile2: FilenameString);

    (* Removes 'name' from INIFile1 and puts it into INIFile2.  We      *)
    (* don't move any files, that's the caller's job.                   *)

    TYPE ListOfKeys = POINTER TO RECORD
                                     next: ListOfKeys;
                                     key: UserName;
                                     size: CARDINAL;
                                     val: LocArrayPointer;
                                 END (*RECORD*);

    VAR keylist, listelt: ListOfKeys;
        state: StringReadState;
        key: UserName;

    BEGIN
        (* Module RINIData allows us to have only one INI file open at  *)
        (* a time.  Because of this, we start by reading all the user   *)
        (* data into memory - after which we'll add it to the second    *)
        (* INI file.                                                    *)

        keylist := NIL;
        IF OpenINIFile (INIFile1) THEN
            GetStringList (name, "", state);
            REPEAT
                NextString (state, key);
                IF key[0] <> Nul THEN
                    NEW (listelt);
                    listelt^.key := key;
                    EVAL (ItemSize (name, key, listelt^.size));
                    IF listelt^.size = 0 THEN
                        listelt^.val := NIL;
                    ELSE
                        ALLOCATE (listelt^.val, listelt^.size);
                        EVAL (INIFetchBinary(name, key, listelt^.val^,
                                                        listelt^.size));
                    END (*IF*);
                    listelt^.next := keylist;
                    keylist := listelt;
                END (*IF*);
            UNTIL key[0] = Nul;
            CloseStringList (state);
            CloseINIFile;
        END (*IF*);

        (* Now open the second INI file and write the data to it. *)

        IF OpenINIFile (INIFile2) THEN
            WHILE keylist <> NIL DO
                listelt := keylist;
                keylist := listelt^.next;
                IF listelt^.size = 0 THEN
                    INIPutBinary (name, listelt^.key, key, 0);
                ELSE
                    INIPutBinary (name, listelt^.key, listelt^.val^, listelt^.size);
                    DEALLOCATE (listelt^.val, listelt^.size);
                END (*IF*);
                DISPOSE (listelt);
            END (*WHILE*);
            CloseINIFile;
        END (*IF*);

        (* Now that the copy is complete, remove the user from  *)
        (* the first INI file.                                  *)

        IF OpenINIFile (INIFile1) THEN
            INIDeleteApp (name);
            CloseINIFile;
        END (*IF*);

    END MoveOneUser;

(************************************************************************)

PROCEDURE MoveAllUsers (domain1, domain2: DomainName);

    (* Removes all users from domain1 and puts them into domain2.  This *)
    (* procedure also moves the local host names and the IMAP flag.     *)

    TYPE ListOfHosts = POINTER TO RECORD
                                      next: ListOfHosts;
                                      host: DomainName;
                                  END (*RECORD*);

    VAR INIFile1, INIFile2: FilenameString;
        hostlist: ListOfHosts;

    (********************************************************************)

    PROCEDURE CollectLocalHostNames;

        (* Reads the local host names from the INI file (assumed to be  *)
        (* already open) and adds them to the hostlist list.            *)

        VAR hostlistelt: ListOfHosts;
            hostname: DomainName;
            state: StringReadState;

        BEGIN
            GetStringList ("$SYS", "Local", state);
            REPEAT
                NextString (state, hostname);
                IF hostname[0] <> Nul THEN
                    NEW (hostlistelt);
                    hostlistelt^.host := hostname;
                    hostlistelt^.next := hostlist;
                    hostlist := hostlistelt;
                END (*IF*);
            UNTIL hostname[0] = Nul;
            CloseStringList (state);
        END CollectLocalHostNames;

    (********************************************************************)

    PROCEDURE MoveLocalList;

        (* Takes the 'hostlist' list and merges those entries into the  *)
        (* local list of the target INI file.                           *)

        VAR j, k, BufferSize: CARDINAL;
            p: ListOfHosts;
            bufptr: CharArrayPointer;

        BEGIN
            (* We already have a list from the source INI file.  Now    *)
            (* add names, if any, from the target INI file.             *)

            IF OpenINIFile (INIFile2) THEN
                CollectLocalHostNames;
                CloseINIFile;
            END (*IF*);

            (* Now we want to write all those names back to the target  *)
            (* INI file, as a string of strings.  Start by counting how *)
            (* much space we need.                                      *)

            BufferSize := 0;
            p := hostlist;
            WHILE p <> NIL DO
                INC (BufferSize, LENGTH(p^.host)+1);
                p := p^.next;
            END (*WHILE*);

            (* Create the string buffer. *)

            IF BufferSize = 0 THEN
                bufptr := NIL;
            ELSE
                INC (BufferSize);
                ALLOCATE (bufptr, BufferSize);
            END (*IF*);

            (* Store all the names into the buffer. *)

            j := 0;
            WHILE hostlist <> NIL DO
                p := hostlist;
                hostlist := p^.next;
                k := 0;
                REPEAT
                    bufptr^[j] := p^.host[k];
                    INC (k);  INC (j);
                UNTIL (p^.host[k] = Nul) OR (k = SIZE(DomainName));
                bufptr^[j] := Nul;  INC (j);
                DISPOSE (p);
            END (*WHILE*);

            (* Write the buffer to the target INI file. *)

            IF OpenINIFile (INIFile2) THEN
                IF BufferSize = 0 THEN
                    INIPutBinary ("$SYS", "Local", j, 0);
                ELSE
                    bufptr^[j] := Nul;
                    INIPutBinary ("$SYS", "Local", bufptr^, BufferSize);
                    DEALLOCATE (bufptr, BufferSize);
                END (*IF*);
                CloseINIFile;
            END (*IF*);

            (* Remove the original list from the source INI file. *)

            IF OpenINIFile (INIFile1) THEN
                INIDeleteKey ("$SYS", "Local");
                CloseINIFile;
            END (*IF*);

        END MoveLocalList;

    (********************************************************************)

    TYPE
        ListOfUsers = POINTER TO RECORD
                                     next: ListOfUsers;
                                     user: UserName;
                                 END (*RECORD*);

    VAR MailRoot, SrcRoot, DstRoot, SrcDir, DstDir: FilenameString;
        userlist, listelt: ListOfUsers;
        state: StringReadState;
        current: UserName;
        cid: ChanId;
        IMAPFlag, IMFlagExists: BOOLEAN;

    BEGIN
        CurrentMailRoot(MailRoot);

        (* Work out the source directory and source INI file. *)

        SrcRoot := MailRoot;
        IF domain1[0] = Nul THEN
            INIFile1 := MasterINIFile;
        ELSE
            Strings.Append (domain1, SrcRoot);
            Strings.Append ('\', SrcRoot);
            INIFile1 := SrcRoot;
            IF UseTNI THEN
                Strings.Append ("DOMAIN.TNI", INIFile1);
            ELSE
                Strings.Append ("DOMAIN.INI", INIFile1);
            END (*IF*);
        END (*IF*);

        (* Work out the destination directory and INI file. *)

        DstRoot := MailRoot;
        IF domain2[0] = Nul THEN
            INIFile2 := MasterINIFile;
        ELSE
            Strings.Append (domain2, DstRoot);
            Strings.Append ('\', DstRoot);
            INIFile2 := DstRoot;
            IF UseTNI THEN
                Strings.Append ("DOMAIN.TNI", INIFile2);
            ELSE
                Strings.Append ("DOMAIN.INI", INIFile2);
            END (*IF*);
        END (*IF*);

        (* Make sure that the destination INI file exists.      *)

        IF NOT FileSys.Exists (INIFile2) THEN
            cid := OpenNewFile (INIFile2, FALSE);
            CloseFile (cid);
        END (*IF*);

        (* Complication: RINIData assumes only one INI file open at     *)
        (* a time.  To avoid rewriting that module, I can make a linear *)
        (* list of all users, and only then go back and move user data. *)

        userlist := NIL;
        hostlist := NIL;
        IMFlagExists := FALSE;
        IF OpenINIFile (INIFile1) THEN

            (* Take the IMAPForNewUsers flag from the source file. *)

            IMFlagExists := INIFetch ("$SYS", "IMAPForNewUsers", IMAPFlag);
            IF IMFlagExists THEN
                INIDeleteKey ("$SYS", "IMAPForNewUsers");
            END (*IF*);

            GetStringList ("", "", state);
            REPEAT
                NextString (state, current);
                IF (current[0] <> Nul) AND NOT Strings.Equal(current, '$SYS') THEN
                    NEW (listelt);
                    listelt^.user := current;
                    listelt^.next := userlist;
                    userlist := listelt;
                END (*IF*);
            UNTIL current[0] = Nul;
            CloseStringList (state);

            (* While we're at it, load the local host list as well. *)

            CollectLocalHostNames;
            CloseINIFile;
        END (*IF*);

        (* Now that we have a list of all users, go back and move       *)
        (* them one at a time.  Remark: the user '$ALIAS' also gets     *)
        (* moved, thereby dealing with the aliases.                     *)

        WHILE userlist <> NIL DO
            listelt := userlist;
            current := listelt^.user;
            userlist := listelt^.next;
            DISPOSE (listelt);
            MoveOneUser (current, INIFile1, INIFile2);
            IF current[0] <> '$' THEN
                SrcDir := SrcRoot;
                Strings.Append (current, SrcDir);
                DstDir := DstRoot;
                Strings.Append (current, DstDir);
                EVAL(MoveDirectory (SrcDir, DstDir));
            END (*IF*);
        END (*WHILE*);

        (* Put the IMAPForNewUsers flag into the destination file, *)
        (* unless it already has such a flag.                      *)

        IF OpenINIFile (INIFile2) THEN
            IF NOT INIFetch ("$SYS", "IMAPForNewUsers", IMAPFlag) THEN
                IF IMFlagExists THEN
                    INIPut ("$SYS", "IMAPForNewUsers", IMAPFlag);
                ELSE
                    INIDeleteKey ("$SYS", "IMAPForNewUsers");
                END (*IF*);
            END (*IF*);
            CloseINIFile;
        END (*IF*);

        (* Finally, move the local host names over. *)

        MoveLocalList;

    END MoveAllUsers;

(************************************************************************)

PROCEDURE UserCount (domain: DomainName): CARDINAL;

    (* Returns the number of users in domain. *)

    VAR MailRoot, INIFile: FilenameString;
        count: CARDINAL;
        state: StringReadState;
        current: UserName;

    BEGIN
        CurrentMailRoot (MailRoot);
        count := 0;

        (* Work out the source directory and source INI file. *)

        IF domain[0] = Nul THEN
            INIFile := MasterINIFile;
        ELSE
            INIFile := MailRoot;
            Strings.Append (domain, INIFile);
            Strings.Append ('\', INIFile);
            Strings.Append ("DOMAIN", INIFile);
            IF UseTNI THEN
                Strings.Append (".TNI", INIFile);
            ELSE
                Strings.Append (".INI", INIFile);
            END (*IF*);
        END (*IF*);

        IF OpenINIFile (INIFile) THEN
            GetStringList ("", "", state);
            REPEAT
                NextString (state, current);
                IF (current[0] <> Nul) AND NOT Strings.Equal(current, '$SYS') THEN
                    INC (count);
                END (*IF*);
            UNTIL current[0] = Nul;
            CloseStringList (state);
            CloseINIFile;
        END (*IF*);

        RETURN count;

    END UserCount;

(************************************************************************)
(*                            DELETING USERS                            *)
(************************************************************************)

PROCEDURE DeleteUser (user: UserName;  MailRoot: FilenameString);

    (* Deletes one user in this domain from the INI file,               *)
    (* and also deletes the user's mail directory provided that it is   *)
    (* empty.  (If it is not empty, we want the directory deletion      *)
    (* to fail, so that the system manager can decide what to do about  *)
    (* the problem.)  The domain is implicitly defined by the MailRoot  *)
    (* value.  Note: MailRoot includes a trailing '\'.                  *)
    (* Assumption: the INI file is already open.                        *)

    BEGIN
        Strings.Append (user, MailRoot);
        EVAL (DeleteDirectory (MailRoot));
        INIDeleteApp (user);
    END DeleteUser;

(************************************************************************)

PROCEDURE DeleteAllUsers (MailDir, INIFile: FilenameString);

    (* Deletes all users and aliases in this domain from the INI file,  *)
    (* and also deletes the user directories provided that they are     *)
    (* empty.                                                           *)

    VAR state: StringReadState;
        current: UserName;

    BEGIN
        IF OpenINIFile (INIFile) THEN
            GetStringList ("", "", state);
            REPEAT
                NextString (state, current);
                IF (current[0] <> Nul) AND NOT Strings.Equal(current, '$SYS') THEN
                    DeleteUser (current, MailDir);
                END (*IF*);
            UNTIL current[0] = Nul;
            CloseStringList (state);
            CloseINIFile;
        END (*IF*);
    END DeleteAllUsers;

(************************************************************************)
(*              CREATING, RENAMING, AND DESTROYING DOMAINS              *)
(************************************************************************)

PROCEDURE CreateDomain (domainname: DomainName);

    (* Creates a directory for this domain, creates the DOMAIN.TNI if   *)
    (* needed, and adds this name to the list of domain names in the    *)
    (* master INI file.  Does nothing if the domain already exists.     *)

    VAR cid: ChanId;  Filename: FilenameString;

    BEGIN
        IF NOT DomainExists (domainname) THEN
            Strings.Assign (MailRoot, Filename);
            Strings.Append (domainname, Filename);
            MakeDirectory (Filename);

            (* INI files are created as necessary, but TNI files have   *)
            (* to be explicitly created.                                *)

            IF UseTNI THEN
                Strings.Append ("\DOMAIN.TNI", Filename);
                IF NOT Exists(Filename) THEN
                    cid := OpenNewFile (Filename, FALSE);
                    CloseFile (cid);
                END (*IF*);
            END (*IF*);

            AddDomain (domainname);
        END (*IF*);
    END CreateDomain;

(************************************************************************)

PROCEDURE RenameDomain (oldname, newname: DomainName);

    (* Changes the name of a domain, including the renaming     *)
    (* of its mail directory.                                   *)

    VAR srcdir, dstdir: FilenameString;

    BEGIN
        Strings.Assign (MailRoot, srcdir);
        Strings.Append (oldname, srcdir);
        Strings.Assign (MailRoot, dstdir);
        Strings.Append (newname, dstdir);
        EVAL(MoveDirectory (srcdir, dstdir));
        ChangeDomainName (oldname, newname);
    END RenameDomain;

(************************************************************************)

PROCEDURE DeleteDomain (domainname: DomainName;
                               hwnd: OS2.HWND;  lang: LangHandle);

    (* Removes this domain from the INI file, also deletes its          *)
    (* directory if it's empty.  The hwnd parameter supplies an owner   *)
    (* of a message box, and lang is the language to use in that        *)
    (* message box, if we need one.                                     *)

    VAR Directory, INIFileName, INIFileName2, subdir: FilenameString;
        message: ARRAY [0..511] OF CHAR;

    BEGIN
        Strings.Assign (MailRoot, Directory);
        Strings.Append (domainname, Directory);
        Strings.Assign (Directory, INIFileName);
        Strings.Append ("\DOMAIN.", INIFileName);
        Strings.Assign (INIFileName, INIFileName2);
        IF UseTNI THEN
            Strings.Append ("TNI", INIFileName);
            Strings.Append ("INI", INIFileName2);
        ELSE
            Strings.Append ("INI", INIFileName);
            Strings.Append ("TNI", INIFileName2);
        END (*IF*);

        Strings.Assign (Directory, subdir);
        Strings.Append ("\", subdir);
        DeleteAllUsers (subdir, INIFileName);
        SubtractDomain (domainname);

        Strings.Assign (Directory, subdir);
        Strings.Append ("\Unknown", subdir);
        EVAL (DeleteDirectory (subdir));
        EVAL (DeleteFile (INIFileName2));
        EVAL (DeleteFile (INIFileName));

        IF NOT DeleteDirectory (Directory) THEN
            StrToBufferA (lang, "Domains.CantDelete", domainname, message);
            WarningBox (hwnd, message);
        END (*IF*);

        IF Strings.Equal (domainname, OriginalDomainName) THEN
            ResetOriginal (OriginalDomainName);
        END (*IF*);

    END DeleteDomain;

(************************************************************************)
(*                        ORIGINAL DOMAIN NAME                          *)
(************************************************************************)

PROCEDURE GetOriginalDomainName (VAR (*OUT*) OriginalName: DomainName);

    (* Returns the current original domain name, i.e. the       *)
    (* domain that will be migrated back if we switch back to   *)
    (* single-domain mode, and the domain that will be used     *)
    (* when migrating in the other direction.                   *)

    BEGIN
        OriginalName := OriginalDomainName;
        IF OriginalName[0] = Nul THEN
            OriginalName := DefaultOriginalName;
        END (*IF*);
    END GetOriginalDomainName;

(************************************************************************)

PROCEDURE SetOriginalDomainName (VAR (*IN*) name: DomainName);

    (* Called by another module that wishes to set the name that will   *)
    (* be recorded as the original domain name.                         *)

    BEGIN
        OriginalDomainName := name;
    END SetOriginalDomainName;

(************************************************************************)

PROCEDURE ResetOriginal (VAR (*OUT*) NewOriginalName: DomainName);

    (* Recovery procedure to use if the original domain has been        *)
    (* deleted.  Finds the first domain that does exist and changes     *)
    (* our record of the original domain name to match that first       *)
    (* domain.  Returns that name as NewOriginalName.  If, however, no  *)
    (* domains exist, NewOriginalName is returned as the empty string   *)
    (* but our internal record of the original name is left unchanged.  *)

    VAR first: DomainName;

    BEGIN
        FirstDomainName (first);
        IF first[0] = Nul THEN
            NewOriginalName[0] := Nul;
        ELSE
            OriginalDomainName := first;
            NewOriginalName := first;
        END (*IF*);
    END ResetOriginal;

(************************************************************************)

PROCEDURE OriginalNotRenamed (VAR (*OUT*) name: DomainName): BOOLEAN;

    (* Returns TRUE iff OriginalDomainName is still equal to the        *)
    (* default.  The returned value of name is also set to the current  *)
    (* value of OriginalDomainName.                                     *)

    BEGIN
        name := OriginalDomainName;
        RETURN Strings.Equal (name, DefaultOriginalName);
    END OriginalNotRenamed;

(************************************************************************)

PROCEDURE SetMasterININame (name: FilenameString;  TNImode: BOOLEAN);

    (* Initialisation: sets the name of the INI or TNI file to use. *)

    BEGIN
        MasterINIFile := name;
        UseTNI := TNImode;
        IF TNImode THEN
            Strings.Append (".TNI", MasterINIFile);
        ELSE
            Strings.Append (".INI", MasterINIFile);
        END (*IF*);
    END SetMasterININame;

(************************************************************************)

PROCEDURE LoadOriginalDomainName;

    (* Gets the default domain name from the INI file. *)

    BEGIN
        OriginalDomainName[0] := Nul;
        IF NOT INIGetString ('$SYS', 'OriginalDomainName', OriginalDomainName) THEN
            IF INIGetString ('$SYS', 'DefaultDomainName', OriginalDomainName) THEN
                INIDeleteKey ('$SYS', 'DefaultDomainName');
            END (*IF*);
        END (*IF*);
        IF OriginalDomainName[0] = Nul THEN
            OriginalDomainName := DefaultOriginalName;
        END (*IF*);
    END LoadOriginalDomainName;

(************************************************************************)

PROCEDURE StoreOriginalDomainName;

    (* Writes the default domain name back to the INI file. *)

    BEGIN
        INIPutString ('$SYS', 'OriginalDomainName', OriginalDomainName);
    END StoreOriginalDomainName;

(************************************************************************)

BEGIN
    SetMasterININame ("Weasel", FALSE);
    MailRoot := "";
    OriginalDomainName := DefaultOriginalName;
FINALLY
    IF OpenINIFile (MasterINIFile) THEN
        StoreOriginalDomainName;
        CloseINIFile;
    END (*IF*);
END SUDomains.

