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

IMPLEMENTATION MODULE MyClock;

        (********************************************************)
        (*                                                      *)
        (*   Conversion of date and time to various formats     *)
        (*                                                      *)
        (*  Programmer:         P. Moylan                       *)
        (*  Started:            26 June 1998                    *)
        (*  Last edited:        9 January 2013                  *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)


IMPORT Strings;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM TimeConv IMPORT
    (* proc *)  millisecs;

(********************************************************************************)

CONST Nul = CHR(0);

TYPE
    MonthNameType = ARRAY [0..15] OF ARRAY [0..2] OF CHAR;
    CharSet = SET OF CHAR;

CONST
    MonthName = MonthNameType {'M00', 'Jan', 'Feb', 'Mar', 'Apr', 'May',
                               'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov',
                               'Dec', 'M13', 'M14', 'M15'};
    Digits = CharSet {'0'..'9'};

(********************************************************************************)

PROCEDURE AdjustTime (VAR (*INOUT*) date: DateTime;  addminutes: INTEGER);

    (* Adds the given number of minutes to date. *)

    TYPE MonthArray = ARRAY [1..12] OF CARDINAL;
    CONST DaysInMonth = MonthArray {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

    (****************************************************************************)

    PROCEDURE IsLeapYear (year: CARDINAL): BOOLEAN;

        (* Returns TRUE iff this year is a leap year.  The result is good *)
        (* up to 2099.                                                    *)

        BEGIN
            RETURN (year MOD 4) = 0;
        END IsLeapYear;

    (****************************************************************************)

    PROCEDURE IncDay;

        (* Adds one day to date. *)

        BEGIN
            IF (date.day < DaysInMonth[date.month])
                       OR ((date.month = 2)
                            AND IsLeapYear(date.year) AND (date.day = 28)) THEN
                INC (date.day);
            ELSE
                date.day := 1;
                IF date.month = 12 THEN
                    date.month := 1;  INC(date.year);
                ELSE
                    INC (date.month);
                END (*IF*);
            END (*IF*);
        END IncDay;

    (****************************************************************************)

    PROCEDURE DecDay;

        (* Subtracts one day from date. *)

        BEGIN
            IF date.day > 1 THEN
                DEC (date.day);
            ELSE
                IF date.month > 1 THEN
                    DEC (date.month);
                ELSE
                    date.month := 12;  DEC (date.year);
                END (*IF*);
                date.day := DaysInMonth[date.month];
                IF (date.month = 2) AND IsLeapYear (date.year) THEN
                    INC (date.day);
                END (*IF*);
            END (*IF*);
        END DecDay;

    (****************************************************************************)

    PROCEDURE AddHr (amount: CARDINAL);

        (* Increments date.hour by amount. *)

        BEGIN
            INC (amount, date.hour);
            WHILE amount >= 24 DO
                IncDay;
                DEC (amount, 24);
            END (*WHILE*);
            date.hour := amount;
        END AddHr;

    (****************************************************************************)

    PROCEDURE SubHr (amount: CARDINAL);

        (* Decrements date.hour by amount. *)

        BEGIN
            WHILE amount >= 24 DO
                DecDay;
                DEC (amount, 24);
            END (*WHILE*);
            IF amount > 0 THEN
                IF amount <= date.hour THEN
                    DEC (date.hour, amount);
                ELSE
                    INC (date.hour, 24 - amount);
                    DecDay;
                END (*IF*);
            END (*IF*);
        END SubHr;

    (****************************************************************************)

    VAR adjust: CARDINAL;

    BEGIN
        adjust := ABS (addminutes);                  (* correction in minutes *)
        IF addminutes < 0 THEN
            SubHr (adjust DIV 60);
            adjust := adjust MOD 60;
            IF adjust <> 0 THEN
                IF adjust <= date.minute THEN
                    DEC (date.minute, adjust);
                ELSE
                    INC (date.minute, 60 - adjust);
                    SubHr (1);
                END (*IF*);
            END (*IF*);
        ELSIF addminutes > 0 THEN
            AddHr (adjust DIV 60);
            adjust := adjust MOD 60 + date.minute;
            IF adjust < 60 THEN
                date.minute := adjust;
            ELSE
                date.minute := adjust - 60;
                AddHr (1);
            END (*IF*);
        END (*IF*);
    END AdjustTime;

(********************************************************************************)

PROCEDURE CorrectToGMT (VAR (*INOUT*) date: DateTime);

    (* Adjusts a local time to the equivalent UTC. *)

    VAR Now: DateTime;

    BEGIN
        GetClock (Now);
        AdjustTime (date, Now.zone);
    END CorrectToGMT;

(********************************************************************************)

PROCEDURE CorrectFromGMT (VAR (*INOUT*) date: DateTime);

    (* Adjusts a UTC time to the equivalent local time. *)

    VAR Now: DateTime;

    BEGIN
        GetClock (Now);
        AdjustTime (date, -Now.zone);
    END CorrectFromGMT;

(********************************************************************************)

PROCEDURE Convert2 (value: CARDINAL;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                      VAR (*INOUT*) j: CARDINAL);

    (* Puts a 2-digit number at result[j], updates j. *)

    (****************************************************************************)

    PROCEDURE Convert1 (value: CARDINAL);

        (* Puts a 1-digit number at result[j], updates j. *)

        BEGIN
            result[j] := CHR(value + ORD('0'));  INC(j);
        END Convert1;

    (****************************************************************************)

    BEGIN
        Convert1 (value DIV 10);  Convert1 (value MOD 10);
    END Convert2;

(********************************************************************************)

PROCEDURE DateTimeToString (Time: DateTime;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string in the format                *)
    (*         01 Jan 2000 00:00:00 +1000                               *)
    (* The result array must have room for at least 26 characters.      *)

    VAR k: [0..2];  j, tzhour, tzmin: CARDINAL;

    BEGIN
        j := 0;
        Convert2 (Time.day, result, j);  result[j] := ' ';  INC(j);
        FOR k := 0 TO 2 DO
            result[j] := MonthName[Time.month][k];  INC(j);
        END (*FOR*);
        result[j] := ' ';  INC(j);
        Convert2 (Time.year DIV 100, result, j);
        Convert2 (Time.year MOD 100, result, j);
        result[j] := ' ';  INC(j);

        Convert2 (Time.hour, result, j);  result[j] := ':';  INC(j);
        Convert2 (Time.minute, result, j);  result[j] := ':';  INC(j);
        Convert2 (Time.second, result, j);  result[j] := ' ';  INC(j);

        IF Time.zone <> -1 THEN
            IF Time.zone < 0 THEN result[j] := '+'
            ELSE result[j] := '-'
            END (*IF*);
            INC(j);
            tzmin := ABS (Time.zone);
            tzhour := tzmin DIV 60;
            tzmin := tzmin MOD 60;
            Convert2 (tzhour, result, j);
            Convert2 (tzmin, result, j);
        END (*IF*);

        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
    END DateTimeToString;

(********************************************************************************)

PROCEDURE UnpackDate (date: CARDINAL;  VAR (*OUT*) day, month, year: CARDINAL);

    (* Converts a packed date to its three components.  *)

    BEGIN
        day := date MOD 32;
        month := date DIV 32;
        year := month DIV 16 + 1980;
        month := month MOD 16;
    END UnpackDate;

(********************************************************************************)

PROCEDURE UnpackTime (time: CARDINAL;  VAR (*OUT*) hour, min, sec: CARDINAL);

    (* Converts a packed time to its three components.  *)

    BEGIN
        sec := 2*(time MOD 32);
        min := time DIV 32;
        hour := min DIV 64;
        min := min MOD 64;
    END UnpackTime;

(********************************************************************************)

PROCEDURE PackedDateTimeToString (date, time: CARDINAL;
                                  VAR (*OUT*) result: ARRAY OF CHAR);

    (* The date and time parameters are in the packed format used by the *)
    (* file system.  We convert them to a date/time string in the format *)
    (*         01-Jan-2000 00:00:00 +1000                                *)
    (* The result array must have room for at least 26 characters.       *)

    VAR k: [0..2];  one, two, three, j, tzhour, tzmin: CARDINAL;
        now: DateTime;

    BEGIN
        j := 0;
        UnpackDate (date, one, two, three);
        Convert2 (one, result, j);  result[j] := '-';  INC(j);
        FOR k := 0 TO 2 DO
            result[j] := MonthName[two][k];  INC(j);
        END (*FOR*);
        result[j] := '-';  INC(j);
        Convert2 (three DIV 100, result, j);
        Convert2 (three MOD 100, result, j);
        result[j] := ' ';  INC(j);

        UnpackTime (date, one, two, three);
        Convert2 (one, result, j);  result[j] := ':';  INC(j);
        Convert2 (two, result, j);  result[j] := ':';  INC(j);
        Convert2 (three, result, j);  result[j] := ' ';  INC(j);

        GetClock (now);
        IF now.zone <> -1 THEN
            IF now.zone < 0 THEN result[j] := '+'
            ELSE result[j] := '-'
            END (*IF*);
            INC(j);
            tzmin := ABS (now.zone);
            tzhour := tzmin DIV 60;
            tzmin := tzmin MOD 60;
            Convert2 (tzhour, result, j);
            Convert2 (tzmin, result, j);
        END (*IF*);

        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
    END PackedDateTimeToString;

(********************************************************************************)

PROCEDURE StringToPackedDateTime (str: ARRAY OF CHAR;  pos: CARDINAL;  UTC: BOOLEAN;
                                  VAR (*OUT*) date, time: CARDINAL): BOOLEAN;

    (* Takes a 14-character string YYYYMMDDHHMMSS at str[pos] and       *)
    (* converts it to a date and time in the packed format used by the  *)
    (* file system.  The date/time are assumed to be UTC if UTC, and    *)
    (* local time otherwise.  Returns FALSE on a format error.          *)

    (****************************************************************************)

    PROCEDURE Load2 (VAR (*OUT*) N: CARDINAL): BOOLEAN;

        (* Evaluates a two-digit number at str[pos], updates pos. *)

        VAR numeric: BOOLEAN;

        BEGIN
            numeric := str[pos] IN Digits;
            IF numeric THEN
                N := ORD(str[pos]) - ORD('0');
                INC (pos);
                numeric := str[pos] IN Digits;
                IF numeric THEN
                    N := 10*N + ORD(str[pos]) - ORD('0');
                    INC (pos);
                END (*IF*);
            END (*IF*);
            RETURN numeric;
        END Load2;

    (****************************************************************************)

    VAR century, mm, dd, HH, MM, SS: CARDINAL;
        daterecord: DateTime;
        found: BOOLEAN;

    BEGIN
        WITH daterecord DO
            found := Load2(century) AND Load2(year) AND Load2(mm) AND Load2(dd)
                        AND Load2(HH) AND Load2(MM) AND Load2(SS);
            IF found THEN
                month := mm;
                day := dd;
                hour := HH;
                minute := MM;
                second := SS;
                fractions := 0;
                zone := 0;
                SummerTimeFlag := FALSE;
                INC (year, 100*century);
            END (*IF*);
        END (*WITH*);
        IF found THEN
            IF UTC THEN
                CorrectFromGMT (daterecord);
            END (*IF*);
            WITH daterecord DO
                IF year < 1980 THEN
                    year := 0;
                ELSE
                    DEC (year, 1980);
                END (*IF*);
                date := 32*(16*year + month) + day;
                time := 32*(64*hour + minute) + second DIV 2;
            END (*WITH*);
        END (*IF*);
        RETURN found;
    END StringToPackedDateTime;

(********************************************************************************)

PROCEDURE AppendShortDate (Time: DateTime;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date string, appends it to result. *)
    (* Format is yyyymmdd.                                   *)

    VAR j: CARDINAL;

    BEGIN
        j := LENGTH(result);
        Convert2 (Time.year DIV 100, result, j);
        Convert2 (Time.year MOD 100, result, j);
        Convert2 (Time.month, result, j);
        Convert2 (Time.day, result, j);
        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
    END AppendShortDate;

(********************************************************************************)

PROCEDURE AppendShortDateTime (Time: DateTime;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string, appends it to result. *)
    (* Format is yyyymmddhhmm.                                    *)

    VAR j: CARDINAL;

    BEGIN
        AppendShortDate (Time, result);
        j := LENGTH(result);
        Convert2 (Time.hour, result, j);
        Convert2 (Time.minute, result, j);
        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
    END AppendShortDateTime;

(********************************************************************************)

PROCEDURE AppendTime (Time: DateTime;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts Time to a time string in the format           *)
    (*         17:32:10                                       *)
    (* and appends it to result.                              *)

    VAR j: CARDINAL;

    BEGIN
        j := LENGTH (result);
        Convert2 (Time.hour, result, j);  result[j] := ':';  INC(j);
        Convert2 (Time.minute, result, j);  result[j] := ':';  INC(j);
        Convert2 (Time.second, result, j);
        IF j <= HIGH(result) THEN
            result[j] := Nul;
        END (*IF*);
    END AppendTime;

(********************************************************************************)

PROCEDURE AppendSyslogDateTime (Time: DateTime;  VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string, appends it to result. *)
    (* Format is Mmm dd hh:mm:ss.                                 *)

    VAR j: CARDINAL;

    BEGIN
        Strings.Append (MonthName[Time.month], result);
        j := Strings.Length (result);
        result[j] := ' ';  INC(j);
        Convert2 (Time.day, result, j);
        IF result[j-2] = '0' THEN
            result[j-2] := ' ';
        END (*IF*);
        result[j] := ' ';  INC(j);
        IF j <= HIGH(result) THEN
            result[j] := Nul;
        END (*IF*);
        AppendTime (Time, result);
    END AppendSyslogDateTime;

(********************************************************************************)

PROCEDURE TimeToString19 (Time: DateTime;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string in the format                *)
    (*        2000-01-26 17:32:10                                       *)
    (* The result array must have room for at least 19 characters.      *)

    VAR j: CARDINAL;

    BEGIN
        j := 0;  Convert2 (Time.year DIV 100, result, j);
        Convert2 (Time.year MOD 100, result, j);  result[j] := '-';  INC(j);
        Convert2 (Time.month, result, j);  result[j] := '-';  INC(j);
        Convert2 (Time.day, result, j);  result[j] := ' ';  INC(j);
        IF j <= HIGH(result) THEN
            result[j] := Nul;
        END (*IF*);
        AppendTime (Time, result);
    END TimeToString19;

(********************************************************************************)

PROCEDURE CurrentTimeToString (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time in the format                  *)
    (*        2000-01-26 17:32:10                                       *)
    (* The result array must have room for at least 19 characters.      *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        TimeToString19 (now, result);
    END CurrentTimeToString;

(********************************************************************************)

PROCEDURE CurrentDateAndTime (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time in the format                  *)
    (*         01 Jan 2000 00:00:00 +1000                               *)
    (* The result array must have room for at least 26 characters.      *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        DateTimeToString (now, result);
    END CurrentDateAndTime;

(********************************************************************************)

PROCEDURE PackedCurrentDateTime(): CARDINAL;

    (* Encodes the current date and time as a 32-bit integer, using 16 bits     *)
    (* for date and 16 bits for time.                                           *)

    CONST shift = 65536;

    VAR now: DateTime;  date, time: CARDINAL;

    BEGIN
        GetClock (now);
        WITH now DO
            IF year < 1980 THEN year := 0;
            ELSE DEC(year,1980);
            END (*IF*);
            IF year > 127 THEN year := 127 END (*IF*);
            date := 32*(16*year + month) + day;
            time := 32*(64*hour + minute) + second DIV 2;
        END (*WITH*);
        RETURN shift*date + time;
    END PackedCurrentDateTime;

(********************************************************************************)

PROCEDURE AppendDateString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends yyyymmdd to result, where yyyymmdd is a digit string     *)
    (* (without any punctuation) representing the current date.         *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendShortDate (now, result);
    END AppendDateString;

(********************************************************************************)

PROCEDURE AppendDateTimeString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends yyyymmddhhmm to result, where yyyymmddhhmm is a digit string     *)
    (* (without any punctuation) representing the current date and time.        *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendShortDateTime (now, result);
    END AppendDateTimeString;

(********************************************************************************)

PROCEDURE AppendTimeString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends a 10-digit numeric string to result.  The number is time-based,  *)
    (* but we are less concerned with its exact value than with having a        *)
    (* high probability that the value will be different on each call.          *)

    VAR j, time, count, H: CARDINAL;

    BEGIN
        time := millisecs();
        j := LENGTH(result) + 10;  count := 10;
        H := HIGH(result);
        IF j <= H THEN
            result[j] := CHR(0);
        END (*IF*);
        DEC (j);
        IF j > H THEN
            DEC (count, j-H);
            j := H;
        END (*IF*);
        WHILE count > 0 DO
            result[j] := CHR(ORD('0') + (time MOD 10));
            DEC (j);
            time := time DIV 10;
            DEC (count);
        END (*WHILE*);
    END AppendTimeString;

(********************************************************************************)

PROCEDURE AppendSyslogDateTimeString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends Mmm dd hh:mm:ss to result (where Mmm is a month abbreviation and *)
    (* the other fields are numeric) representing the current date and time.    *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendSyslogDateTime (now, result);
    END AppendSyslogDateTimeString;

(********************************************************************************)

END MyClock.

