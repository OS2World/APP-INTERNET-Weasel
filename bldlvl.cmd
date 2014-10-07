/*----------------------------------------------------------
   Appends a build level to WEASEL.EXE.

           Author:       Peter Moylan
           Last revised: 4 January 2013

   Usage:
           bldlvl ver

           where ver is the version string

------------------------------------------------------------*/

parse arg ver
projHost = "PJM2"
timestamp = LEFT(DATE() TIME(),25)LEFT(projHost,10)
signature = "@#Peter Moylan:"ver"#@##1## "timestamp"::EN:AU:::@@Weasel SMTP and POP server for OS/2 and eCS"
outfile = "level.txt"
"@DEL "outfile" 2> NUL"
CALL LINEOUT outfile, signature
CALL STREAM outfile,'C','CLOSE'
"@copy weasel.exe /B + level.txt weasel.exe /B > NUL"
"@DEL "outfile

exit

