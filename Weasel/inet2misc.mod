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

IMPLEMENTATION MODULE Inet2Misc;

        (********************************************************)
        (*                                                      *)
        (* Miscellaneous procedures for networking applications *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            17 January 2002                 *)
        (*  Last edited:        22 May 2017                     *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


FROM SYSTEM IMPORT
    (* type *)  LOC, CARD8, CARD16, CARD32,
    (* proc *)  CAST;

IMPORT Strings;

FROM Conversions IMPORT
    (* proc *)  CardinalToString;

FROM MiscFuncs IMPORT
    (* proc *)  ConvertCard, GetNum;

FROM Names IMPORT
    (* type *)  HostName;

FROM NetDB IMPORT
    (* type *)  HostEntPtr,
    (* proc *)  gethostbyaddr;

FROM Sockets IMPORT
    (* const*)  AF_INET,
    (* const*)  NotASocket,
    (* type *)  Socket,
    (* proc *)  select, setsockopt, send;

FROM LowLevel IMPORT
    (* proc *)  SwapIt;

(********************************************************************************)

TYPE CharSet = SET OF CHAR;

CONST
    Nul = CHR(0);
    DecimalDigits = CharSet{'0'..'9'};

(********************************************************************************)

PROCEDURE Swap2 (val: CARD16): CARD16;

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD16;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap2;

(********************************************************************************)

PROCEDURE Swap4 (val: CARD32): CARD32;

    (* Returns the argument value in byte-reversed order.  This is needed       *)
    (* because network byte order is most significant byte first, whereas our   *)
    (* local host order is least significant byte first.                        *)

    VAR temp: CARD32;

    BEGIN
        temp := val;
        SwapIt (temp);
        RETURN temp;
    END Swap4;

(********************************************************************************)

PROCEDURE IPToString (IP: ARRAY OF LOC;  EncloseInBrackets: BOOLEAN;
                                VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts a four-byte IP address (in network byte order) to a             *)
    (* human-readable form.  There must be at least 15 character positions      *)
    (* available in the result array, or 17 if EncloseInBrackets is TRUE.       *)

    VAR j, position: CARDINAL;

    BEGIN
        IF EncloseInBrackets THEN
            result[0] := '[';  position := 1;
        ELSE
            position := 0;
        END (*IF*);
        FOR j := 0 TO 2 DO
            ConvertCard (CAST(CARD8,IP[j]), result, position);
            result[position] := '.';  INC(position);
        END (*FOR*);
        ConvertCard (CAST(CARD8,IP[3]), result, position);
        IF EncloseInBrackets THEN
            result[position] := ']';  INC(position);
        END (*IF*);
        IF position <= HIGH(result) THEN
            result[position] := Nul;
        END (*IF*);
    END IPToString;

(************************************************************************)

PROCEDURE AddressToHostName (address: CARDINAL;
                             VAR (*OUT*) Name: HostName);

    (* Converts a numeric IP address to a name.  *)

    VAR HostInfo: HostEntPtr;

    BEGIN
        Name[0] := Nul;
        HostInfo := gethostbyaddr (address, SIZE(CARDINAL), AF_INET);
        IF HostInfo <> NIL THEN
            IF HostInfo^.h_name <> NIL THEN
                Strings.Assign (HostInfo^.h_name^, Name);
            ELSIF (HostInfo^.h_addr_list <> NIL)
                          AND (HostInfo^.h_addr_list^[0] <> NIL) THEN
                address := HostInfo^.h_addr_list^[0]^;
            END (*IF*);
        END (*IF*);

        (* If we have failed to find the name via nameserver lookup,    *)
        (* convert the address to a string and use that.                *)

        IF Name[0] = Nul THEN
            IPToString (address, TRUE, Name);
        END (*IF*);

    END AddressToHostName;

(********************************************************************************)

PROCEDURE WaitForSocket (S: Socket;  timeout: CARDINAL): INTEGER;

    (* Waits until something is available on socket S.  The possible return     *)
    (* codes are +1 for OK, 0 for timeout, -1 for error (or cancel).            *)
    (* Specify timeout=MAX(CARDINAL) if you don't want a timeout.               *)

    VAR SocketArray: ARRAY [0..0] OF Socket;

    BEGIN
        SocketArray[0] := S;
        RETURN select (SocketArray, 1, 0, 0, timeout);
    END WaitForSocket;

(************************************************************************)

PROCEDURE WaitForSocketOut (S: Socket;  timeout: CARDINAL): INTEGER;

    (* Waits until socket S is ready for more output.  The possible     *)
    (* return codes are +1 for OK, 0 for timeout, -1 for error (or      *)
    (* cancel).  Set timeout=MAX(CARDINAL) if you don't want a timeout. *)

    VAR SocketArray: ARRAY [0..0] OF Socket;

    BEGIN
        SocketArray[0] := S;
        RETURN select (SocketArray, 0, 1, 0, timeout);
    END WaitForSocketOut;

(********************************************************************************)

PROCEDURE WaitForDataSocket (output: BOOLEAN;
                             DataSocket, CommandSocket: Socket): BOOLEAN;

    (* Waits until DataSocket is ready or out-of-band data arrives on           *)
    (* CommandSocket.  The first parameter should be TRUE if DataSocket is      *)
    (* being used as an output socket, and FALSE if it is being used as an      *)
    (* input socket.  The function result is TRUE iff DataSocket is ready AND   *)
    (* no out-of-band data has arrived on CommandSocket.                        *)

    VAR SocketArray: ARRAY [0..1] OF Socket;  count: INTEGER;

    BEGIN
        SocketArray[0] := DataSocket;
        SocketArray[1] := CommandSocket;
        IF output THEN
            count := select (SocketArray, 0, 1, 1, MAX(CARDINAL));
        ELSE
            count := select (SocketArray, 1, 0, 1, MAX(CARDINAL));
        END (*IF*);
        IF count > 0 THEN
            RETURN SocketArray[1] = NotASocket;
        ELSE
            RETURN FALSE;
        END (*IF*);
    END WaitForDataSocket;

(************************************************************************)

PROCEDURE Synch (S: Socket);

    (* Ensures that outgoing data is sent right now rather than being   *)
    (* buffered.  This reduces performance a little, but is needed to   *)
    (* work around a bug in Netscape and MS FTP Exploder.               *)

    VAR OptionValue: CARDINAL;  dummy: CHAR;

    BEGIN
        OptionValue := 1;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
        send (S, dummy, 0, 0);
        OptionValue := 0;
        setsockopt (S, 6, 1, OptionValue, SIZE(CARDINAL));
    END Synch;

(************************************************************************)

PROCEDURE NameIsNumeric (VAR (*INOUT*) name: ARRAY OF CHAR): BOOLEAN;

    (* Returns TRUE iff name has the form N.N.N.N or [N.N.N.N] where each N is  *)
    (* a decimal number.  (The present version actually accepts an arbitrary    *)
    (* number of numbers separated by dots.)  As a side-effect, we also strip   *)
    (* the square brackets if they are present.                                 *)

    VAR j: CARDINAL;  result: BOOLEAN;

    (****************************************************************************)

    PROCEDURE ScanNumber(): BOOLEAN;

        (* At least one digit. *)

        BEGIN
            IF name[j] IN DecimalDigits THEN
                REPEAT
                    INC(j);
                UNTIL NOT(name[j] IN DecimalDigits);
                RETURN TRUE;
            ELSE
                RETURN FALSE;
            END (*IF*);
        END ScanNumber;

    (****************************************************************************)

    PROCEDURE ScanNumberString(): BOOLEAN;

        (* Numbers separated by '.' *)

        BEGIN
            LOOP
                IF NOT ScanNumber() THEN RETURN FALSE END(*IF*);
                IF name[j] = '.' THEN INC(j)
                ELSE RETURN TRUE
                END (*IF*);
            END (*LOOP*);
        END ScanNumberString;

    (****************************************************************************)

    BEGIN
        j := 0;
        IF name[0] = '[' THEN j := 1 END(*IF*);
        result := ScanNumberString();
        IF result THEN
            IF name[0] = '[' THEN
                result := name[j] = ']';
                IF result THEN INC(j) END(*IF*);
            END (*IF*);
            result := result AND (name[j] = Nul);
        END (*IF*);
        IF result AND (name[0] = '[') THEN
            name[j-1] := Nul;
            Strings.Delete (name, 0, 1);
        END (*IF*);
        RETURN result;
    END NameIsNumeric;

(********************************************************************************)

PROCEDURE StringToIP (name: ARRAY OF CHAR): CARDINAL;

    (* Converts an N.N.N.N string to an address in network byte order.  We      *)
    (* assume that the caller has already checked that the string is in this    *)
    (* format.                                                                  *)

    TYPE Arr4 = ARRAY [0..3] OF CARD8;

    VAR k: [0..3];  val: Arr4;
        pos: CARDINAL;

    BEGIN
        pos := 0;  k := 0;
        val := CAST(Arr4, VAL(CARDINAL,0));
        LOOP
            val[k] := GetNum(name, pos);
            IF (k = 3) OR (name[pos] <> '.') THEN
                EXIT (*LOOP*);
            END (*IF*);
            INC (pos);
            INC (k);
        END (*LOOP*);
        RETURN CAST(CARDINAL, val);
    END StringToIP;

(************************************************************************)

END Inet2Misc.

