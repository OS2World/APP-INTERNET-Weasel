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
        (*  Last edited:        27 April 2015                   *)
        (*  Status:             OK                              *)
        (*                                                      *)
        (********************************************************)

(****************************************************************************)
(*  Remark: now that I've written FormatDateTime, the size of this module   *)
(*  can probably be reduced substantially.  For now, though, I haven't done *)
(*  enough testing to check simplified versions of many procedures.         *)
(****************************************************************************)

IMPORT Strings;

FROM SysClock IMPORT
    (* type *)  DateTime,
    (* proc *)  GetClock;

FROM TimeConv IMPORT
    (* proc *)  millisecs;

(****************************************************************************)

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
(*                                DAY OF WEEK                                   *)
(********************************************************************************)

PROCEDURE PutDayName (date: DateTime;  VAR (*INOUT*) result: ARRAY OF CHAR;
                                          VAR (*INOUT*) j: CARDINAL);

    (* Puts the three letter name for the day of week in result, updates j. *)

    TYPE
        MonthData = ARRAY [1..13] OF CARDINAL;
        DayOfWeek = [0..6];       (* 0 = Sunday *)

    CONST
        BaseDay = 6;

        (* Days since beginning of year, for the 1st of each month.  In a   *)
        (* leap year you need an extra correction.                          *)

        FirstDayInMonth = MonthData {  0,  31,  59,  90, 120, 151,
                                     181, 212, 243, 273, 304, 334, 365};

    VAR FirstDayOfYear, weekday: DayOfWeek;
        dayofyear: CARDINAL;
        name: ARRAY [0..2] OF CHAR;
        IsLeapYear: BOOLEAN;

    BEGIN
        (* Every group of four years has 4*365+1 = 1461 days, and       *)
        (* 1461 MOD 7 = 5.  This means that the DayOfWeek changes by    *)
        (* 5 days per 4 years.                                          *)

        FirstDayOfYear := (BaseDay + 5*(date.year DIV 4)) MOD 7;

        (* Thereafter, it changes by 2 days in the first year, and one  *)
        (* day per year after that.                                     *)

        IsLeapYear := (date.year MOD 4) = 0;
        IF date.year MOD 4 <> 0 THEN
            FirstDayOfYear := (FirstDayOfYear + (date.year MOD 4) + 1) MOD 7;
        END (*IF*);

        (* Now we want to know the current day of the year. *)

        dayofyear := FirstDayInMonth[ORD(date.month)] + ORD(date.day) - 1;
        IF IsLeapYear AND (ORD(date.month) > 2) THEN
            INC (dayofyear);
        END (*IF*);

        weekday := (FirstDayOfYear + dayofyear - 1) MOD 7;
        result[j] := Nul;
        CASE weekday OF
          |  0:  name := "Sun";
          |  1:  name := "Mon";
          |  2:  name := "Tue";
          |  3:  name := "Wed";
          |  4:  name := "Thu";
          |  5:  name := "Fri";
          |  6:  name := "Sat";
        END (*CASE*);
        Strings.Append (name, result);
        INC (j, 3);
    END PutDayName;

(********************************************************************************)
(*                            ARITHMETIC ON DATES                               *)
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
        date.zone := 0;
    END CorrectToGMT;

(********************************************************************************)

PROCEDURE CorrectFromGMT (VAR (*INOUT*) date: DateTime);

    (* Adjusts a UTC time to the equivalent local time. *)

    VAR Now: DateTime;

    BEGIN
        GetClock (Now);
        AdjustTime (date, -Now.zone);
        date.zone := Now.zone;
    END CorrectFromGMT;

(********************************************************************************)
(*                       FORMATTING DATE/TIME AS STRING                         *)
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

PROCEDURE FormatDateTime (Time: DateTime;  format: ARRAY OF CHAR;
                            GMT: BOOLEAN;  VAR (*OUT*) result: ARRAY OF CHAR);

    (* Puts the current date and time into result according to the      *)
    (* given format specification.  The format codes are:               *)
    (*      dd          day, numeric                                    *)
    (*      ddd         day, three-letter name                          *)
    (*      mm          month, numeric                                  *)
    (*      mmm         month, three-letter name                        *)
    (*      yy          year, including Y2K bug                         *)
    (*      yyyy        year, all four digits                           *)
    (*      HH          hours                                           *)
    (*      MM          minutes                                         *)
    (*      SS          seconds                                         *)
    (*      zz          first time: sign and hours of time zone         *)
    (*      zz          second time: minutes part of time zone          *)
    (* Time is in GMT if GMT=TRUE; local time otherwise.                *)

    VAR j, k, kf: CARDINAL;  literal, zonefound: BOOLEAN;

    BEGIN
        IF GMT THEN
            CorrectToGMT (Time);
        END (*IF*);
        k := 0;  kf := 0;  zonefound := FALSE;
        WHILE (kf <= HIGH(format)) AND (format[kf] <> Nul) DO
            literal := TRUE;
            CASE format[kf] OF
              | 'd':
                    IF format[kf+1] = 'd' THEN
                        INC (kf, 2);
                        IF format[kf] = 'd' THEN
                            INC (kf);
                            PutDayName (Time, result, k);
                        ELSE
                            Convert2 (Time.day, result, k);
                        END (*IF*);
                        literal := FALSE;
                    END (*IF*);
              | 'm':
                    IF format[kf+1] = 'm' THEN
                        INC (kf, 2);
                        IF format[kf] = 'm' THEN
                            INC (kf);
                            FOR j := 0 TO 2 DO
                                result[k] := MonthName[Time.month][j];  INC(k);
                            END (*FOR*);
                        ELSE
                            Convert2 (Time.month, result, k);
                        END (*IF*);
                        literal := FALSE;
                    END (*IF*);
              | 'y':
                    IF format[kf+1] = 'y' THEN
                        INC (kf, 2);
                        IF (format[kf] = 'y') AND (format[kf+1] = 'y') THEN
                            INC (kf, 2);
                            Convert2 (Time.year DIV 100, result, k);
                        END (*IF*);
                        Convert2 (Time.year MOD 100, result, k);
                        literal := FALSE;
                    END (*IF*);
              | 'H':
                    IF format[kf+1] = 'H' THEN
                        INC (kf, 2);
                        Convert2 (Time.hour, result, k);
                        literal := FALSE;
                    END (*IF*);
              | 'M':
                    IF format[kf+1] = 'M' THEN
                        INC (kf, 2);
                        Convert2 (Time.minute, result, k);
                        literal := FALSE;
                    END (*IF*);
              | 'S':
                    IF format[kf+1] = 'S' THEN
                        INC (kf, 2);
                        Convert2 (Time.second, result, k);
                        literal := FALSE;
                    END (*IF*);
              | 'z':
                    IF format[kf+1] = 'z' THEN
                        INC (kf, 2);
                        IF Time.zone <> -1 THEN
                            IF zonefound THEN
                                Convert2 (-Time.zone MOD 60, result, k);
                            ELSE
                                IF Time.zone > 0 THEN
                                    result[k] := '-';
                                ELSE
                                    result[k] := '+';
                                END (*IF*);
                                INC (k);
                                Convert2 (-Time.zone DIV 60, result, k);
                            END (*IF*);
                        END (*IF*);
                        zonefound := TRUE;
                        literal := FALSE;
                    END (*IF*);
              | ELSE
                    literal := TRUE;
            END (*CASE*);
            IF literal THEN
                result[k] := format[kf];
                INC (k);  INC(kf);
            END (*IF*);
        END (*WHILE*);
        IF k <= HIGH(result) THEN
            result[k] := Nul;
        END (*IF*);
    END FormatDateTime;

(********************************************************************************)

PROCEDURE DateTimeToString (Time: DateTime;  IncludeDayName: BOOLEAN;
                                VAR (*OUT*) result: ARRAY OF CHAR);

    (* Converts Time to a date/time string in the format                *)
    (*         01 Jan 2000 00:00:00 +1000                               *)
    (* The result array must have room for at least 26 characters, or   *)
    (* 5 more if IncludeDayName is specified.                           *)

    VAR k: [0..2];  j, tzhour, tzmin: CARDINAL;

    BEGIN
        j := 0;
        IF IncludeDayName THEN
            PutDayName (Time, result, j);
            result[j] := ',';  INC(j);
            result[j] := ' ';  INC(j);
        END (*IF*);
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
            IF Time.zone = 0 THEN
                IF j <= HIGH(result) THEN
                    result[j] := CHR(0);
                END (*IF*);
                Strings.Append ("GMT", result);
                INC (j, 3);
            ELSE
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
        END (*IF*);

        IF j <= HIGH(result) THEN
            result[j] := CHR(0);
        END (*IF*);
    END DateTimeToString;

(********************************************************************************)
(*                         PACK OR UNPACK DATE/TIME                             *)
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

PROCEDURE UnpackDateTime (date, time: CARDINAL): DateTime;

    (* Converts packed date/time to a DateTime record. *)

    VAR dt: DateTime;  minute, month: CARDINAL;

    BEGIN
        GetClock (dt);               (* to fill in time zone *)
        dt.day := date MOD 32;
        month := date DIV 32;
        dt.year := month DIV 16 + 1980;
        dt.month := month MOD 16;
        dt.second := 2*(time MOD 32);
        minute := time DIV 32;
        dt.hour := minute DIV 64;
        dt.minute := minute MOD 64;
        RETURN dt;
    END UnpackDateTime;

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
        mm := 0;  dd := 0;  HH := 0;  MM := 0;  SS := 0;
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

PROCEDURE RFC1123StringToDateTime (VAR (*IN*) str: ARRAY OF CHAR): DateTime;

    (* Input is a string of the form                                            *)
    (*        Sat, 04 Apr 2015 16:00:19 TZ"                                     *)
    (* where the day name is optional, and the optional TZ field is either GMT  *)
    (* or something like -02:30.  (If the field is missing, we assume GMT.)     *)
    (* The result is in GMT.                                                    *)

    VAR j: CARDINAL;

    (****************************************************************************)

    PROCEDURE SkipBlanks;

        BEGIN
            WHILE str[j] = ' ' DO
                INC (j);
            END (*WHILE*);
        END SkipBlanks;

    (****************************************************************************)

    PROCEDURE GetNumber(): CARDINAL;

        VAR val: CARDINAL;

        BEGIN
            val := 0;
            WHILE str[j] IN Digits DO
                val := 10*val + ORD(str[j]) - ORD('0');
                INC(j);
            END (*WHILE*);
            RETURN val;
        END GetNumber;

    (****************************************************************************)

    VAR k, month: CARDINAL;  result: DateTime;
        monthname: ARRAY [0..2] OF CHAR;
        negative: BOOLEAN;

    BEGIN
        j := 0;

        (* Skip past the day name if present. *)

        WHILE NOT (str[j] IN Digits) DO
            INC (j);
        END (*WHILE*);

        (* Date. *)

        result.day := GetNumber();

        SkipBlanks;
        FOR k := 0 TO 2 DO
            monthname[k] := str[j];  INC(j);
        END (*FOR*);
        k := 0;  month := 0;
        WHILE (k <= 12) AND (month = 0) DO
            IF Strings.Equal (monthname, MonthName[k]) THEN
                month := k;
            END (*IF*);
            INC (k);
        END (*WHILE*);
        IF (month < 1) OR (month > 12) THEN
            result.month := 1;           (* arbitrary decision *)
        ELSE
            result.month := month;
        END (*IF*);

        SkipBlanks;
        result.year := GetNumber();

        (* Time. *)

        SkipBlanks;
        result.hour := GetNumber();
        IF str[j] = ':' THEN INC(j) END(*IF*);
        result.minute := GetNumber();
        IF str[j] = ':' THEN INC(j) END(*IF*);
        result.second := GetNumber();
        result.fractions := 0;

        (* Time zone. *)

        result.zone := 0;  negative := FALSE;
        SkipBlanks;
        IF (str[j] = '+') OR (str[j] = '-') THEN
            negative := str[j] = '+';     (* really! *)
            INC (j);
        END (*IF*);
        result.zone := GetNumber();
        IF str[j] = ':' THEN INC(j) END(*IF*);
        result.zone := 60*result.zone + VAL(INTEGER,GetNumber());
        IF negative THEN result.zone := -result.zone; END(*IF*);
        IF result.zone <> 0 THEN
            CorrectToGMT (result);
        END (*IF*);

        RETURN result;

    END RFC1123StringToDateTime;

(********************************************************************************)
(*                            DATE/TIME COMPARISONS                             *)
(********************************************************************************)

PROCEDURE CompareDates (VAR (*IN*) A, B: DateTime): INTEGER;

    (* The result is -1 if A is earlier than B, 0 if they are the same, and     *)
    (* +1 if A is later than B.                                                 *)

    BEGIN
        IF A.zone <> B.zone THEN
            CorrectToGMT (A);
            CorrectToGMT (B);
        END (*IF*);

        IF A.year < B.year THEN RETURN -1;
        ELSIF A.year > B.year THEN RETURN +1;

        ELSIF A.month < B.month THEN RETURN -1;
        ELSIF A.month > B.month THEN RETURN +1;

        ELSIF A.day < B.day THEN RETURN -1;
        ELSIF A.day > B.day THEN RETURN +1;

        ELSIF A.hour < B.hour THEN RETURN -1;
        ELSIF A.hour > B.hour THEN RETURN +1;

        ELSIF A.minute < B.minute THEN RETURN -1;
        ELSIF A.minute > B.minute THEN RETURN +1;

        ELSIF A.second < B.second THEN RETURN -1;
        ELSIF A.second > B.second THEN RETURN +1;

        ELSIF A.fractions < B.fractions THEN RETURN -1;
        ELSIF A.fractions > B.fractions THEN RETURN +1;

        ELSE RETURN 0;

        END (*IF*);
    END CompareDates;

(********************************************************************************)

PROCEDURE CompareDateStrings (VAR (*IN*) A, B: ARRAY OF CHAR): INTEGER;

    (* Both inputs are strings of the form                                      *)
    (*        Sat, 04 Apr 2015 16:00:19 TZ"                                     *)
    (* where the day name is optional, and the optional TZ field is either GMT  *)
    (* or something like -02:30.  (If the field is missing, we assume GMT.)     *)
    (* The result is -1 if A is earlier than B, 0 if they are the same, and     *)
    (* +1 if A is later than B.                                                 *)

    VAR ADT, BDT: DateTime;

    BEGIN
        ADT := RFC1123StringToDateTime (A);
        BDT := RFC1123StringToDateTime (B);
        RETURN CompareDates (ADT, BDT);
    END CompareDateStrings;

(****************************************************************************)
(*                           SHORT STRING FORMATS                           *)
(****************************************************************************)

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

(****************************************************************************)

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

(****************************************************************************)

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

(****************************************************************************)

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

(****************************************************************************)

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

(****************************************************************************)

PROCEDURE PackedDateTimeToGMT (date, time: CARDINAL;
                                  VAR (*OUT*) result: ARRAY OF CHAR);

    (* The date and time parameters are in the packed format used by the *)
    (* file system.  We convert them to a date/time string in the format *)
    (*        Sat, 04 Apr 2015 16:00:19 GMT"                            *)
    (* This is the format preferred for internet applications (RFC1123).*)
    (* The result array must have room for at least 29 characters.      *)

    VAR dt: DateTime;

    BEGIN
        dt := UnpackDateTime (date, time);
        FormatDateTime (dt, "ddd, dd mmm yyyy HH:MM:SS GMT", TRUE, result);
    END PackedDateTimeToGMT;

(****************************************************************************)
(*                            CURRENT DATE/TIME                             *)
(****************************************************************************)

PROCEDURE OurTimezone (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes our time zone in the format +10:00                   *)
    (* The result array must have room for at least 6 characters.   *)
    (* Returns empty string if time zone is undefined.              *)

    VAR now: DateTime;
        j: CARDINAL;
        val: INTEGER;

    BEGIN
        GetClock (now);
        val := now.zone;
        IF val = -1 THEN
            result[0] := Nul;
        ELSE
            IF val < 0 THEN
                result[0] := '+';
                val := -val;
            ELSE
                result[0] := '-';
            END (*IF*);
            j := 1;
            Convert2 (val DIV 60, result, j);
            result[j] := ':';  INC(j);
            Convert2 (val MOD 60, result, j);
            IF j <= HIGH(result) THEN
                result[j] := Nul;
            END (*IF*);
        END (*IF*);
    END OurTimezone;

(****************************************************************************)

PROCEDURE FormatCurrentDateTime (format: ARRAY OF CHAR;  GMT: BOOLEAN;
                                    VAR (*OUT*) result: ARRAY OF CHAR);

    (* Puts the current date and time into result according to the      *)
    (* given format specification.  The format codes are:               *)
    (*      dd          day, numeric                                    *)
    (*      ddd         day, three-letter name                          *)
    (*      mm          month, numeric                                  *)
    (*      mmm         month, three-letter name                        *)
    (*      yy          year, including Y2K bug                         *)
    (*      yyyy        year, all four digits                           *)
    (*      HH          hours                                           *)
    (*      MM          minutes                                         *)
    (*      SS          seconds                                         *)
    (*      zz          first time: sign and hours of time zone         *)
    (*      zz          second time: minutes part of time zone          *)
    (* Time is in GMT if GMT=TRUE; local time otherwise.                *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        FormatDateTime (now, format, GMT, result);
    END FormatCurrentDateTime;

(****************************************************************************)

PROCEDURE CurrentTimeToString (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time in the format                  *)
    (*        2000-01-26 17:32:10                                       *)
    (* The result array must have room for at least 19 characters.      *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        TimeToString19 (now, result);
    END CurrentTimeToString;

(****************************************************************************)

PROCEDURE CurrentDateAndTime (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time in the format                  *)
    (*         01 Jan 2000 00:00:00 +1000                               *)
    (* The result array must have room for at least 26 characters.      *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        DateTimeToString (now, FALSE, result);
    END CurrentDateAndTime;

(****************************************************************************)

PROCEDURE CurrentDateAndTimeGMT (VAR (*OUT*) result: ARRAY OF CHAR);

    (* Encodes the current date and time in the format                  *)
    (*        Sat, 04 Apr 2015 16:00:19 GMT"                            *)
    (* This is the format preferred for internet applications (RFC1123).*)
    (* The result array must have room for at least 29 characters.      *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        FormatDateTime (now, "ddd, dd mmm yyyy HH:MM:SS GMT", TRUE, result);
    END CurrentDateAndTimeGMT;

(****************************************************************************)

PROCEDURE PackedCurrentDateTime(): CARDINAL;

    (* Encodes the current date and time as a 32-bit integer, using 16 bits *)
    (* for date and 16 bits for time.                                       *)

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

(****************************************************************************)

PROCEDURE AppendDateString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends yyyymmdd to result, where yyyymmdd is a digit string     *)
    (* (without any punctuation) representing the current date.         *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendShortDate (now, result);
    END AppendDateString;

(****************************************************************************)

PROCEDURE AppendDateTimeString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends yyyymmddhhmm to result, where yyyymmddhhmm is a digit string *)
    (* (without any punctuation) representing the current date and time.    *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendShortDateTime (now, result);
    END AppendDateTimeString;

(****************************************************************************)

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

(****************************************************************************)

PROCEDURE AppendSyslogDateTimeString (VAR (*INOUT*) result: ARRAY OF CHAR);

    (* Appends Mmm dd hh:mm:ss to result (where Mmm is a month abbreviation and *)
    (* the other fields are numeric) representing the current date and time.    *)

    VAR now: DateTime;

    BEGIN
        GetClock (now);
        AppendSyslogDateTime (now, result);
    END AppendSyslogDateTimeString;

(****************************************************************************)

END MyClock.

