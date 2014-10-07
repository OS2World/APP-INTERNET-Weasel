/* Batch file to create the Weasel distribution. */
/* The result is one distribution zip file and one source zip file. */

'del weasel*.zip 2>nul'
'copy D:\Dev1\general\doc\gpl.txt'
'cd doc'

/* Create the INF files. */

'ipfc -i weasel.ipf'
'ipfc -i weaselpro.ipf'
'ipfc -i techdata.ipf'

/* Build the executables.  */

'cd ..'
'cd ..\WSU'
'xc =p setup.prj'
'cd ..\Weasel'
'copy ..\WSU\Setup.exe'
'copy ..\WSU\Setup.*.lng'
'xc =p weasel.prj'
'xc =p viosetup.prj'
'xc =p endmail.prj'
'\apps\lxlite\lxlite *.exe'

/* Generate symbol files.  */
/* If you don't have Perl, the next three lines can be skipped. */

'call PerlEnv.cmd'
perl 'D:\Apps\scripts\makexqs.pl' weasel.map
say "weasel.sym and weasel.xqs should now exist"

/* Set icons and build level. */

call seticon
ver = version()
CALL bldlvl ver

/* Copy all the files we want to package up into a temp directory. */

mkdir temp
cd temp
mkdir tools
'copy ..\endmail.exe tools'
'copy ..\tools\readme tools'
'copy ..\tools\mloadini.cmd tools'
'copy ..\tools\mdumpini.cmd tools'
mkdir doc
'copy ..\doc\changes.doc doc'
'copy ..\doc\weasel.ipf doc'
'copy ..\doc\weaselpro.ipf doc'
'copy ..\doc\techdata.ipf doc'
'copy ..\doc\ONLINE'
'copy ..\README'
'copy ..\README.FILTERS'
'copy ..\file_id.diz'
'copy ..\doc\weasel.inf'
'copy ..\doc\weaselpro.inf'
'copy ..\doc\techdata.inf'
'copy ..\weasel.exe'
'copy ..\weasel.fmt'
'copy ..\weasel.map'
'copy ..\weasel.sym'
'copy ..\weasel.xqs'
'copy ..\VIOsetup.exe'
'copy ..\Setup.exe'
'copy ..\setup.*.lng'
'copy ..\makefolder.cmd'
'copy ..\gpl.txt'

/* Zip up the main package. */

'zip -q -r ..\weasel'ver'.zip .'
'del doc\* /n'
rmdir doc
'del tools\* /n'
rmdir tools
'del * /n'

/* SOURCE FILES */

'mkdir Weasel'
'mkdir Setup'
'cd ..'

/* Sources for the server and some utilities. */

'del src*.zip /N 2>nul'
'zip src.zip Decode64.prj endmail.prj VIOSetup.prj Weasel.prj'
'Imports Decode64 | zip -q -j -u src.zip -@'
'Imports Weasel | zip -q -j -u src.zip -@'
'Imports VIOSetup | zip -q -j -u src.zip -@'
'Imports EndMail | zip -q -j -u src.zip -@'
'move src.zip temp\Weasel'

/* Sources for Setup. */

'cd ..\WSU'
'del src.zip 2>nul'
'Imports Setup | zip -q -j -u src.zip -@'
'zip src.zip Setup.prj RES\DID.RES'
'move src.zip ..\Weasel\temp\Setup'
'cd ..\Weasel'

/* Unzip the two source zip files, and re-zip into a single package.  */

'cd temp'
'move ..\gpl.txt .'
'copy ..\BUILDING'
'copy ..\makezip.cmd'
'copy ..\version.cmd'
'copy ..\bldlvl.cmd'

'cd Weasel'
'unzip -q -o src.zip'
'del src.zip /N'

'cd ..\Setup'
'unzip -q -o src.zip'
'del src.zip /N'
'cd ..'

'zip -q -r ..\WeaselSrc'ver'.zip .'

/* Remove temporary files. */

'del Weasel\* /N'
'rmdir Weasel'
'del Setup\RES\* /N'
'rmdir Setup\RES'
'del Setup\* /N'
'rmdir Setup'
'del * /N'
'cd ..'
rmdir temp

