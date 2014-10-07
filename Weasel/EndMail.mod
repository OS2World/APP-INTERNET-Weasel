(**************************************************************************)
(*                                                                        *)
(*  The Weasel mail server                                                *)
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

MODULE EndMail;

        (********************************************************)
        (*                                                      *)
        (*         Accepts incoming mail from sendmail          *)
        (*                                                      *)
        (*  This requires a line in sendmail.cf similar to the  *)
        (*  following (all in one line):                        *)
        (*    Mlocal, P=d:\apps\weasel\endmail.exe, F=lnsDFP,   *)
        (*      S=10, R=0, A=d:\apps\weasel\endmail.exe         *)
        (*      d:\apps\weasel\MailRoot\ $u                     *)
        (*  Remark: the first argument is ignored by this       *)
        (*  program; it's there to achieve consistency over     *)
        (*  different versions of sendmail.                     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            25 April 1998                   *)
        (*  Last edited:        21 February 2001                *)
        (*  Status:             OK, I believe                   *)
        (*                                                      *)
        (********************************************************)

FROM SYSTEM IMPORT ADR;

IMPORT STextIO, TextIO, Strings, OS2, IOChan, IOConsts,
       ChanConsts, SeqFile, SIOResult, FileSys;

FROM ProgramArgs IMPORT
    (* proc *)  ArgChan, IsArgPresent;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    FilenameString = ARRAY [0..511] OF CHAR;

VAR
    MailRoot, userID: FilenameString;
    NextName: ARRAY [0..7] OF CHAR;

(************************************************************************)
(*                    CREATING A UNIQUE FILENAME                        *)
(************************************************************************)

PROCEDURE MakeUniqueName (VAR (*OUT*) name: ARRAY OF CHAR);

    (* Generates a unique 8-character string.  The argument must of     *)
    (* course be big enough to take at least 8 characters.              *)

    (********************************************************************)

    PROCEDURE Increment (N: CARDINAL);

        (* Increments NextName[N], with carry as appropriate. *)

        BEGIN
            IF NextName[N] = '9' THEN
                NextName[N] := 'A';
            ELSIF NextName[N] = 'Z' THEN
                IF N = 0 THEN
                    NextName := "00000000";
                ELSE
                    NextName[N] := '0';
                    Increment (N-1);
                END (*IF*);
            ELSE
                INC (NextName[N]);
            END (*IF*);
        END Increment;

    (********************************************************************)

    BEGIN
        Strings.Assign (NextName, name);
        Increment (7);
    END MakeUniqueName;

(************************************************************************)

PROCEDURE MakeNewFilename (BaseName, tail: ARRAY OF CHAR;
                       VAR (*OUT*) NewName: FilenameString);

    (* Creates a file name of the form BaseNamexxxtail, where xxx is    *)
    (* chosen such that a file of that name does not already exist.     *)
    (* Note that BaseName and tail can include punctuation.             *)

    VAR UName: FilenameString;

    BEGIN
        REPEAT
            MakeUniqueName (UName);
            Strings.Assign (BaseName, NewName);
            Strings.Append (UName, NewName);
            Strings.Append (tail, NewName);
        UNTIL NOT FileSys.Exists(NewName);
    END MakeNewFilename;

(********************************************************************************)
(*                          GET PROGRAM PARAMETERS                              *)
(********************************************************************************)

PROCEDURE GetParameters (VAR (*OUT*) userID: FilenameString);

    (* Picks up program arguments from the command line. *)

    TYPE CharNumber = [0..511];

    VAR j: CARDINAL;
        ArgString: ARRAY CharNumber OF CHAR;

    (****************************************************************************)

    PROCEDURE GetArg (VAR (*OUT*) result: ARRAY OF CHAR);

        (* Loads a text string from the argument channel. *)

        VAR k: CARDINAL;

        BEGIN
            (* Skip any leading blanks. *)

            LOOP
                IF ArgString[j] <> ' ' THEN EXIT(*LOOP*) END(*IF*);
                IF j = MAX(CharNumber) THEN
                    ArgString[j] := Nul;  EXIT (*LOOP*);
                ELSE
                    INC (j);
                END (*IF*);
            END (*LOOP*);

            (* Read string, terminator is space or end-of-input. *)

            k := 0;
            WHILE (ArgString[j] <> ' ') AND (ArgString[j] <> Nul) DO
                result[k] := ArgString[j];
                INC (j);  INC(k);
            END (*WHILE*);
            result[k] := Nul;
        END GetArg;

    (****************************************************************************)

    VAR args: IOChan.ChanId;

    BEGIN
        args := ArgChan();
        IF IsArgPresent() THEN
            j := 0;
            TextIO.ReadString (args, ArgString);
            GetArg (MailRoot);    (* skip over first arg *)
            GetArg (MailRoot);
            GetArg (userID);
        END (*IF*);
    END GetParameters;

(********************************************************************************)
(*                       COPY THE MESSAGE FROM STANDARD INPUT                   *)
(********************************************************************************)

PROCEDURE CopyFromStdIn (outfile: IOChan.ChanId);

    VAR MoreToGo, AtEOL: BOOLEAN;
        buffer: ARRAY [0..2047] OF CHAR;
        result: IOConsts.ReadResults;

    BEGIN
        AtEOL := TRUE;
        MoreToGo := TRUE;
        WHILE MoreToGo DO
            STextIO.ReadString (buffer);

            (* Result is set to the value allRight, endOfLine, or endOfInput. *)

            result := SIOResult.ReadResult ();
            IF result = IOConsts.endOfInput THEN

                MoreToGo := FALSE;

            ELSIF result = IOConsts.endOfLine THEN

                TextIO.WriteLn (outfile);
                STextIO.SkipLine;
                AtEOL := TRUE;

            ELSE

                TextIO.WriteString (outfile, buffer);
                AtEOL := FALSE;

            END (*IF*);

        END (*WHILE*);

        IF NOT AtEOL THEN
            TextIO.WriteLn (outfile);
        END (*IF*);

    END CopyFromStdIn;

(********************************************************************************)

PROCEDURE CopyFile (VAR (*IN*) filename: FilenameString): BOOLEAN;

    VAR outfile: IOChan.ChanId;
        res: ChanConsts.OpenResults;
        success: BOOLEAN;

    BEGIN
        SeqFile.OpenWrite (outfile, filename,
                        SeqFile.write + SeqFile.text, res);
        success := res = ChanConsts.opened;
        IF success THEN
            CopyFromStdIn (outfile);
            SeqFile.Close (outfile);
        END (*IF*);
        RETURN success;
    END CopyFile;

(********************************************************************************)

PROCEDURE DeliverTheMessage(): BOOLEAN;

    VAR BaseName, targetname, tempname: FilenameString;
        success, dummy: BOOLEAN;

    BEGIN
        GetParameters (userID);
        Strings.Assign (MailRoot, BaseName);
        Strings.Append (userID, BaseName);
        Strings.Append ('\', BaseName);
        MakeNewFilename (BaseName, ".###", tempname);
        MakeNewFilename (BaseName, ".MSG", targetname);
        success := CopyFile (tempname);
        IF success THEN
            FileSys.Rename (tempname, targetname, success);
        END (*IF*);
        IF NOT success THEN
            FileSys.Remove (tempname, dummy);
        END (*IF*);
        RETURN success;
    END DeliverTheMessage;

(********************************************************************************)
(*                                 MAIN PROGRAM                                 *)
(********************************************************************************)

VAR result: CARDINAL;

BEGIN
    NextName := "00000000";
    IF DeliverTheMessage() THEN
        result := 0;
    ELSE
        result := 3;
        OS2.DosExit (OS2.EXIT_THREAD, 67);
    END (*IF*);
END EndMail.

