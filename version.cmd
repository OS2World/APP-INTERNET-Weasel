/*----------------------------------------------------------
   Returns the version number of Weasel.

           Author:       Peter Moylan
           Last revised: 19 September 2014

   Usage:
           ver = version()

           (Run this from the Weasel top-level directory)

------------------------------------------------------------*/

DEFFile = "DEF\WV.def"

DO FOREVER
    IF lines(DEFFile) != 1 THEN LEAVE
    parse value linein(DEFFile) with kwd'='val
    kwd = STRIP(kwd)
    IF RIGHT(kwd,7) = "version" THEN LEAVE
END

/* Extract the part of val inside double quotes. */

PARSE VALUE val WITH v1 '"' version '"' v2
RETURN version

exit

