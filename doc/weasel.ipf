:userdoc.
:title.Weasel documentation
:docprof toc=1234.

.***********************************
.*   INTRODUCTION
.***********************************

:h1 res=1001 id=1001 global.Introduction

:artwork name='weasel.bmp' align=center.
:p.
Weasel is a combined POP3 and SMTP daemon (Post Office server) for OS/2.
The POP3 and SMTP sections can be enabled or disabled separately.
IMAP can be handled by running a separate IMAP add-on in parallel
with Weasel. Both Weasel and the IMAP add-on are distributed as freeware,
subject to the GNU GLP licence.

:p.This documentation is for version 2.3.

:p.Weasel can be configured either to handle a single mail domain,
or to host multiple domains. This choice, together with a variety of other
configuration details, is managed using a Setup utility that is
described later in this manual.

:p.
:hp2.Disclaimer of Warranty:ehp2.

:sl compact.
:li.
:hp4.
This Product is provided "as-is", without warranty of any
kind, either expressed or implied, including, but not limited to,
the implied warranties of merchantability and fitness for a
particular purpose. The entire risk as to the quality and
performance of the Product is with you. Should the Product prove
defective, the full cost of repair, servicing, or correction lies
with you.
:ehp4.
:esl.

:p.
The author of Weasel is Peter Moylan, peter@pmoylan.org.

:p.
The latest version of Weasel is normally kept at http&colon.&slash.&slash.pmoylan.org/ftp/Weasel.
Information about other software on this site may be found at
http&colon.&slash.&slash.pmoylan.org/pages/os2.

:p.
:hp2.Getting information about new versions:ehp2.

:p.
You can, if you wish, join a mailing list for announcements about
new releases of this software. To join the list, send an
e-mail to majormajor@os2voice.org. The subject line is not
important and may be anything. In the body of the message, put the
lines
:xmp.       subscribe weasel-list
       end
:exmp.

:p.To have yourself removed from the list, send a similar e-mail but
using the command "unsubscribe" instead of "subscribe".

:p.
:hp2.Obtaining the source code:ehp2.

:p.The source code for Weasel can be found at the place where
you obtained this package. It is governed by the GNU GPL licence,
which permits copying with some restrictions.

.***********************************
.*   REGISTRATION
.***********************************

:h1 id=register.Registration

:hp2.Registration:ehp2.

:p.Weasel, which was formerly shareware, is now distributed as
freeware. Registration is no longer required.

.***********************************
.*   EXPLANATION OF HOW E-MAIL WORKS
.***********************************

:h1.How e-mail is transported
:hp2.How e-mail is transported:ehp2.
:p.
Although it is possible to send e-mail directly from one
computer to another, a more common arrangement is to go via a
third machine, which we can call the Post Office machine. The
advantage of such an arrangement is that the Post Office machine
is the only one that needs to be on-line all the time. The
sender and receiver only need to go on-line when sending or
collecting their mail.

:p.Initially, the sender composes the message by using a program
known as a User Agent. (This is optional - it is possible
to send mail without having a User Agent - but most people like
to have the high-level features, such as automatic construction
of mail headers, that are supplied as part of the User Agent.)
The default OS/2 User Agent is the program called Ultimail Lite,
but of course you are free to replace this by one of the alternatives
sold by various suppliers. The most popular OS/2 mail user agents,
as far as I know, are PMMail and Thunderbird.

:p.When the mail is ready to be sent, a piece of software called
an SMTP client is used to send it. (SMTP stands for Simple Mail
Transport Protocol.) This must talk to an SMTP server on another
machine. In the scenario we are considering here, the other machine
is the Post Office machine.

:p.The default OS/2 SMTP client is called "sendmail", and the
default SMTP server is also called sendmail. That is, the same
program does either job, depending on what parameters you give
it when sendmail is started. Typically one copy of sendmail is
running all the time, acting as a server for incoming mail.
Another copy is
started each time you want to send a piece of outgoing mail.
Unfortunately, sendmail is hard to configure. One of the main
reasons Weasel was developed was so that you wouldn't have
to run sendmail.

:p.The message received by the SMTP server remains stored on the
disk of the Post Office machine, until the recipient checks in
to see whether any mail has arrived. This is done with a POP client.
(POP stands for Post Office Protocol.) The Post Office machine must
therefore be running both an SMTP server and a POP server.

:p.This appears to add up to five different programs, but often
their functions are combined in various ways. For example, the
POP and SMTP clients are very often included as built-in parts
of the User Agent program.
In the case of Weasel, I've put both servers (POP and SMTP) into
the same program. This makes it easier for the two servers to
cooperate with each other. It also means that total memory usage is
lower than if you ran separate POP and SMTP servers.

:p.A separate IMAP daemon may be run to support the IMAP protocol
instead of, or in addition to, POP. This IMAP daemon is not
included with Weasel, but can be obtained from the site where you
obtained Weasel. You are warned, however, that the IMAP daemon
is not as well developed as Weasel itself, and currently has some
compatibility problems with some mail clients.

:p.IMAP serves the same purpose as POP, in
that it gives users access to mail that has arrived for them; but it
is designed for the case where the mail is left on the server, and
because of this it has many more options such as letting the user
create multiple mail folders and manipulate the mail in various
ways without necessarily keeping a local copy.

:p.This is attractive to users, but as a mail server manager you
need to be aware of the drawbacks. First, because the IMAP protocol
is far more complicated than the simple POP3 protocol, it puts a
bigger load on your processor. Second, and probably more importantly,
if users keep all of their mail on your server then they will use
an enormous amount of disk space, and you will need to have
enough free space to allow for this.

.***********************************
.*   SERVER FEATURES
.***********************************

:h1 res=002.Server features
:hp2.Server features:ehp2.

:p.Weasel is a post office mail server based on the POP3 standard, known as
RFC 1939 or STD 53. Weasel implements all features of the POP3 standard,
plus some extensions.

:p.Weasel also includes an SMTP server to receive the mail, and
SMTP client software to handle outgoing mail. The SMTP software is based
on the standard known as RFC821, as modified by the later standards RFC1123
and RFC2821.
It also supports ESMTP as specified in RFC1869.
If you wish, you can disable the
built-in SMTP server and use a separate SMTP server from another source.

:p.:hp2.The POP3 subsystem:ehp2.

:p.The Weasel POP3 server, which is what clients contact to fetch
their mail, is a complete implementation of the POP3 standard,
therefore it should be compatible with any standard POP3 client.
The extended commands CAPA (RFC 2449)  and AUTH (RFC 1734) are
also supported.
The USER/PASS and APOP and AUTH methods of logging in are all supported.

:p.All clients are required to supply a username and password.
(The username is what comes before the '@' in that user's e-mail
address.) Passwords are set by a separate program called Setup,
which is described later in this document.

:p.:hp2.The SMTP subsystem:ehp2.

:p.The SMTP server is the part of the system that accepts incoming
mail. The Weasel SMTP implements all of the required commands in
the standard RFC2821, but not the commands identified as obsolete
in that standard. In addition it supports the ESMTP commands
AUTH (RFC2554), EHLO (RFC1869), and EXPN (RFC821).

:p.The AUTH command is separately configurable for incoming and
outgoing mail. That is, you can decide to enable it for incoming
mail, or for outgoing mail, or neither, or both. For incoming mail
you can also control which user accounts are allowed to use AUTH.

:p.:hp2.Restrictions when mail is left on the server:ehp2.

:p.Although some mail clients have a "Leave mail on server" option,
the POP protocol was never intended to allow anything other than
short-term message storage. The POP3 rules require a stable message
base, in that the number of visible messages must not change during
a single POP login. To make this work, the server must do a directory
scan when the POP client logs in, and build a table of messages for
that user. Any messages that arrive after that table has been built
will not be seen until the next login, at which time a new table is
built. This is rarely a problem, because typically the mail client
will log in about once every three minutes to see whether new mail
has arrived. Any message missed on the current scan will be seen on
the next login.

:p.In the most common case, where only a small number of new messages
are waiting, that message table is held in a cache in main memory.
When there are many messages, however, the table has to be offloaded
to a disk file, and of course that file has to be rebuilt on
every login. In that case the client is going to see a delay in
fetching mail. The delay is made worse by delays at the client end,
if the client has to fetch a list of, say, 100 messages, and then
discover that 99 of them are old messages.

:p.An earlier version of Weasel attempted to solve this problem
by making only the newest 512 messages visible to the client.
(But earlier messages would become visible once the later ones
had been fetched and deleted from the server.) Unfortunately that
strategy was foiled by users who failed to delete fetched
messages. The present approach is a compromise that should cause
no problems at all for normal users, but will slow down fetching
for those who leave mail on the server. In practice much of the
slowdown will be at the client end, because of the overhead of
checking message identifiers for all mail on the server in order to
work out what has already been fetched; but there will also be
some penalty at the server end.

:p.:hp2.Mail relaying:ehp2.

:p.An e-mail address has the form user@domain, where 'user' is a
username, and 'domain' is either a machine name or something that
will be mapped to a machine name by a nameserver. If 'domain' is
a domain for which the current machine is supposed to be handling
the mail, then we have mail for a local destination.
Obviously, every SMTP server must be able to handle this case.
If 'user' is a valid username on the current machine, the mail
will be delivered; otherwise, it will be rejected.

:p.Most SMTP servers will also accept mail for addresses on
other machines. If the server accepts such a non-local address,
it accepts the mail and then forwards it on towards its final
destination. This is to allow for the case - which used to be
very common - where there is no direct mail path from the sender
to the final destination. In such a case the SMTP server is said
to be acting as a "relay host".

:p.Junk mailers love relay hosts. Sending out a million pieces of
junk mail can be expensive, because it can tie up your machine
for hours. To avoid that overhead, the people who send junk mail
often choose to use another machine, selected more or less
randomly, as a relay host. (They can't use the same machine every
time, or they would be caught.) The relay host is usually an innocent
victim in this case, but the owner of the relay host is often
blamed for the junk mail. Using a relay host also helps in
disguising the original sender of the mail.

:p.Because of this abuse, Weasel has strong restrictions on
relaying. It will accept relay requests only in four cases:
:ol compact.
:li.when the destination domain is on a list of domains for which
we agree to pass on mail; or
:li.when the host sending us the mail is on the list of "trusted hosts"; or
:li.when the sender has provided a valid AUTH command; or
:li.when the sending host has temporarily become a trusted host as
a result of a POP login, and the system manager has enabled
POP-before-SMTP authentication.
:eol.

:p.All other requests to pass mail on to another machine are rejected.

:p.Case 1 above allows for the situation where your machine
is a mail gateway for your organisation's local network, or is a
backup server for another mail server. Case 2 is for the situation
where your machine must act as an outgoing relay for a group of
local machines. The purpose of cases 3 and 4 is to let you give
your POP users an outgoing mail service,
in addition to the incoming mail service that they automatically have
by virtue of being registered POP users, independently of where
they are logging in from.

:note.There isn't any corresponding IMAP-before-SMTP option, because
IMAP clients invariably have good support for the SMTP AUTH command.

:p.:hp2.How mail is stored:ehp2.

:p.After an e-mail message is accepted by the SMTP server, but
before it is picked up by a POP or IMAP client, the message is stored as
a file on the local disk. When setting up Weasel, you specify one
directory to be used as the "mail root". This directory has one
subdirectory for each user. (In multiple domain mode, there is one
extra level of directories.) Each message is stored as a file
xxxxxxxx.MSG, where xxxxxxx is an internally-generated identifier.

:p.Apart from the *.MSG files, a user's mail directory might contain
a file called LOCK.!!!. This is created when a POP client starts to
access the directory, and it is deleted when the POP session ends.
While this file exists, no other POP client is allowed to get at the
directory. The SMTP server can still store new mail during this
period, but the new mail will not be seen by the POP client until
the next time the client logs in. The time delay is not particularly
important, because most POP clients check for new mail every few
minutes.

:p.The Setup program creates an extra directory called "forward",
as a subdirectory of the mail root directory, to hold outgoing mail
that is still waiting to be sent. In most cases mail files will
remain in this directory for only a short time, but sometimes the
transmission is not successful. (The destination host might be
unreachable, for example.) Weasel attempts to re-send the unsuccessful
mail a number of times over a period of about four days, if it
judges that reattempts are worth trying. (This depends on the reason
for failure. If, for example, the destination host sends back a
"no such user" reply then there is no point in repeating the attempt.)
After several unsuccessful attempts, the sender gets back a
message saying that Weasel is still trying to deliver the mail. If
the final attempt is still not successful, the mail is returned to
the original sender with a note saying that it could not be delivered.

:p.You might find that the "forward" directory contains many hidden
files. Weasel makes them hidden while it is working on them. It
makes them visible again when they are deleted from the internal
queues but must be left in the directory to be found on a later scan.
You should not change the hidden&slash.visible status of these files,
because it is used by Weasel as a "work in progress" indicator.

.***********************************
.*   INSTALLATION
.***********************************

:h1.Installation
:hp2.Installation:ehp2.
:p.
See also :link reftype=hd refid=deinstall.De-installation:elink.
and :link reftype=hd refid=exceptq.Enabling exceptq support:elink.

:p.To operate your machine as a post office, you need to run both
a POP server and an SMTP server. For the SMTP part, you can choose
to run either Weasel's built-in SMTP server, or to run a separate
SMTP server. For now, let's assume that you will use the built-in
server. The other option is explained on a
:link reftype=hd refid=otherSMTP.later page:elink..

:ol.

:li.You should have received this package in the form of a zip file.
The first step is to unzip the file into a directory of your choice.
(Presumably you have already done this.) If you are upgrading from
an earlier version, it is preferable to unzip into the same
directory as the earlier version, so that your existing settings
are retained. Alternatively, you can unzip into a new directory
but keep your settings by copying your old WEASEL.INI and/or WEASEL.TNI
into that new directory.

:li.Optionally, run the script "makefolder.cmd" which you will find in
this directory. (You can do this either now or later.)
This creates a Weasel folder, on the desktop, containing
some useful objects. This is purely for your convenience. If you don't
run makefolder, Weasel will still work.

:li.Create a new directory to hold the users' mail. For example, you
could create a subdirectory called "MailRoot" in your Weasel directory;
or you might choose to use the directory C&colon.\MPTN\ETC\MAIL.

:li.Run the program SETUP.EXE, select the "Local" option, and
click on the "GO" button. Go to the field called "Root directory
for mail". Fill in the name of the directory you created in the
previous step. This can be either a relative name or a full path
name. You may, of course, choose simply to accept the
default entry that is already entered for you.

:li.While still in SETUP.EXE, click on the page tab labelled "Users".
This will give you a list of users; of course, the list will
initially be empty. Click on the "Add" button to
add a new user.

:li.Type in the username and password for one user. The remaining
fields are optional, and you might as well leave them blank for this
initial test. The password is case-sensitive, but the username is
not. Click on the "OK" button to confirm your entry.

:li.Close the Setup window, which will terminate the program.

:eol.

:p.
The server is the program called weasel.exe. You can run it
either by double-clicking on the desktop icon, or by entering the
command "weasel" in a command-line session. Most people will want to
put a program object or shadow for weasel into the startup folder,
or to start it from TCPSTART.CMD; but that's up to you.

:p.After you have done some testing, you'll probably want to run the
Setup program again to add more users. You don't have to restart
Weasel to get the new users recognised.

:p.
For some other options, see
:ul compact.
:li.:link reftype=hd refid=inetd.Running from inetd:elink.
:li.:link reftype=hd refid=detached.Running Weasel detached:elink.
:li.:link reftype=hd refid=otherSMTP.Using a different SMTP server:elink.
:eul.

:note.If you are upgrading from an earlier version, it is a good
idea to run Setup before restarting the server. This will upgrade
your WEASEL.INI to the latest version, in the case where some
options have changed.

:p.If you wish to use IMAP, then you must run imapd.exe as well as
weasel.exe. This program is not included in the Weasel distribution,
but can be obtained from the place where you got Weasel. It should
be installed into the same directory as Weasel, so that the two
programs can share configuration data.

.***********************************
.*   EXCEPTQ
.***********************************

:h2 id=exceptq.Enabling support for exceptq
:hp2.Enabling support for exceptq:ehp2.
:p.
Exceptq is a tool that generates debugging information if a program crashes.
It is implemented in the form of two DLLs which should be placed in a
directory in your LIBPATH. (Or in the Weasel directory, but that is a less
desirable option.) You can find the latest distribution of exceptq at
http&colon.&slash.&slash.home.earthlink.net&slash.~steve53&slash.betas&slash..
Slightly older versions are available from the usual archive sites.

:p.It is not compulsory to install exceptq to run Weasel, but Weasel will
use it if it finds exceptq.dll. Installing it is a very minor job, and
it could help if you need to report a bug.

.***********************************
.*   DEINSTALLATION
.***********************************

:h1 id=deinstall.De-installation
:hp2.De-installation:ehp2.
:p.
Weasel does not tamper with CONFIG.SYS or with other system files.
If you decide that you don't want to keep Weasel, simply delete
the directory into which you installed it. Before doing this, you
should probably check the user mail directories, to see whether
there is any undelivered mail remaining.

:p.If you have been using Weasel as a POP server only, working in
collaboration with some other SMTP server, you might have modified
files like INETD.LST or SENDMAIL.CF. If so, don't forget to reverse
these changes.

.***********************************
.*   CONFIGURATION
.***********************************

:h1 id=configuration.Configuration

:p.The Weasel configuration is controlled by a program called
:link reftype=hd refid=pmconfiguser.Setup:elink.. This controls
all configuration details&colon. adding and deleting users,
changing options, and so on.

:p.
The parameter settings are usually stored in a file WEASEL.INI. Weasel reads its
INI file as it starts up, so some of the changes you make might not take effect until
the next time you start the server. See below, however; many of the
changes take effect immediately, with no need to restart the server.

:p.Exception: if you start the Setup program with the "t" parameter, i.e.
:xmp.
      setup -t
:exmp.
then Setup will edit the file WEASEL.TNI (a human-readable file) instead
of WEASEL.INI. If you use this option, then you should also start
weasel.exe with the command
:xmp.
      weasel -t
:exmp.
so that it will read its configuration data from the TNI file rather
than from the INI file.

:p.In the present version, we are in the process of developing a precise
specification of which Setup parameters take effect immediately, and
which ones have no effect until you next restart Weasel.
The rules for this version are described below.

:p.:hp2.Parameters read only on startup:ehp2.

:p.The following options are set at the time Weasel is started. If you
change them, the changes will not take effect until you stop and restart
the server. Until that time, the previous options will remain in effect.

:ul compact.
:li.The server ports, the flags that say whether SMTP and/or POP
are enabled, and the directory used to hold the mail that is waiting
to be forwarded.
:eul.

:p.:hp2.Parameter changes that take effect immediately:ehp2.

:p.Changes to the following options take effect as soon as you close the
Setup notebook. In such cases, Setup sends a message to Weasel to inform
it that it must re-read its INI file.

:ul compact.
:li.The timeout limits, and the maximum number of users per service.
:li.All logging options. If you change the name (or location) of a log
file, any log entries currently being written will go to the old log
file, and then the old log file will be closed and Weasel will start
to use the new one.
:li.The names of the filters, and the option to serialise filter operations.
:li.The option to apply host tests to the MAIL FROM address.
:li.The maximum message size we will accept.
:li.The number of threads to handle the outgoing mail. (There will be a
delay while the existing threads finish what they are doing, but the
number will gradually increase or decrease to the specified level.)
:li.The specification of which SMTP AUTH methods are accepted for validation of
incoming mail, the enabling/disabling of POP-before-SMTP authentication, and
(if relevant) the number of minutes that POP-before-SMTP authentication
remains valid.
:li.The decision about when to use a relay host for outgoing mail, and
all parameters associated with that decision (hostname, authentication
method, etc.).
:li.All user information, including in particular usernames and passwords.
:li.Alias expansions.
:li.The lists of whitelisted, trusted, "GateFor", and banned hosts.
:li.The domains used for blacklist checking.
:li.The domain information: whether multidomain mode is enabled, and the
hostnames and addresses in the "Local" page of the Setup notebook.
:li.The option (which appears only in the multidomain mode) that controls
whether we use the password to decide which domain a mail user belongs to,
or whether we check only the first domain that has such a username.
:li.The specification of when to go online.
:eul.

:p.Remark: As well as changing the names of the filters, you have the
option of replacing the filter code without changing the name of the
filter. Weasel knows which filters it should run, but of course it does
not know what the filters do, and in particular it does not know whether
you have upgraded a filter. If you
edit the filter code, the change will take effect for the next mail item
to be filtered after you do a "save" operation in your editor. If a filter
is being executed at precisely the time you update it, the effect can be
unpredictable, although in practice the risk of a problem appears to be low.

:p.Similarly, it can happen that a client session is in progress at exactly the
same time as you are making other changes. In such cases, the old or new options
might apply to that client session, depending on the precise timing of
events. Weasel will do its best not to take a mixture of old and
new values. The worst that can happen is that the client will get a
message to say that the operation failed.

:p.In some cases, the word "immediately" must be interpreted liberally. If,
for example, you reduce the maximum number of users, and the existing number
of users is greater than the new limit, the existing users will be allowed
to proceed to completion, but new users are rejected until the number of
users falls below the new limit. The best we can do in such cases is to try
to ensure that the rules applied are the "intuitively obvious" rules.

:p.:hp2.The root directory for mail:ehp2.

:p.The mail root directory, as specified on the first page of the Setup
notebook, can be altered while the server is running, but this has
side-effects that you need to consider before making such a change. After
considering these side-effects, you might decide that it is more
prudent to shut down the server before changing this directory.

:p.First, if mail is still being received and/or fetched while the
directory is being changed, you can end up with a situation where mail
is being received in the old directory and a POP user is trying to
fetch it from the new directory, or vice versa. That means that mail
can become temporarily "lost" because we are looking for it in the
wrong directory. This is not a fatal error, since you can fix it by
manually moving the affected files, but you need to be aware that
manual intervention might be needed. The old mail directories must not
be deleted until you have checked whether they still contain any files.

:p.Second, the server has a special "forward" directory that holds
mail that is in transit while waiting to be sent out. This directory is
a subdirectory of the mail root directory. Some of the files in this
directory are hidden, and some have their pathnames recorded in queues
inside the server. For these reasons, it is essential that we
:hp1.not:ehp1.  change the location of this "forward" directory while
the server is still running. If the mail root directory is changed,
the server will continue to use the old "forward" directory, and will
not switch to the new one until the server is shut down and restarted.
At that stage, you will need to check whether the old "forward"
directory still contains any files, including hidden files, and move
them all to the new "forward" directory.

:p.Most of these complications disappear if the change is made while
the server is shut down. To avoid confusion, it is probably better
to shut down the server, then change the root directory for mail, and
then restart the server.

:p.For similar reasons, it is probably not a good idea to switch from
single-domain mode to multidomain mode, or vice versa, while the
server is running, because this causes a change in the directory structure.
It is safer to shut down the server while making such a major change.
Some users have reporting losing their user data when making such a switch,
possibly caused by access violations when Setup attempted to move files
that were still in use.

.*******************************************************************************
.*   VIOSETUP
.***********************************

:h2 id=VIOconfiguser.The VIOSetup utility

:p.The recommended way to configure Weasel is with the
:link reftype=hd refid=pmconfiguser.Setup:elink. program.
If, however you prefer to use a text-mode configuration programs, you can instead run
:link reftype=hd database='viosetup.inf' refid=1003.VIOSetup:elink.. This does
essentially the same job as Setup, but it runs as a VIO application
rather than as a PM "notebook" application.

:p.Note, however, that
(depending on the version) VIOSetup will not necessarily support
all of the options that Setup supports. In particular, the current
version of VIOSetup does not support the multidomain
mode of operation. Because VIOSetup is being phased out, it does not support
many of the options that have been added to later versions of Setup.

.*******************************************************************************
.*   THE SETUP UTILITY
.***********************************

:h2 id=PMconfiguser.The Setup utility

:p.The program SETUP.EXE has two functions:
:ul compact.
:li.To set the parameters that Weasel.exe will use when it starts up.
:li.To define which users have accounts on the server, and to define the
privileges of the users.
:eul.

:p.When you run Setup, you will get a small screen window with Local/Remote
radio buttons and three pushbuttons. The Remote option is for remote
configuration, which is described in a
:link reftype=hd refid=remoteconfig.separate section:elink.. Normally you
should choose the Local option, which will edit the Weasel.INI file that
resides in the same directory as SETUP.EXE. Click on the "GO" button to
start the editing.

:p.If you start Setup with the command
:xmp.            setup -T

:exmp.
then Setup will edit the file WEASEL.TNI rather than WEASEL.INI.

:p.If you start Setup with the command
:xmp.            setup -i

:exmp.
then Setup will edit the file WEASEL.INI rather than WEASEL.TNI. This
option is redundant, because editing WEASEL.INI is the default
behaviour, but the option is available if some future version changes
the default.

:p.The 't' or 'i' option can, if desired, be combined with the 'L', 'R', and 'G' options
documented below. Of course you should not specify 't' and 'i' together; if
you do, the result is undefined.

:p.If you start Setup with the command
:xmp.            setup -L

:exmp.
then you will bypass the small opening screen and go directly into local editing.

:p.If you start Setup with the command
:xmp.            setup -R

:exmp.
then you will bypass the small opening screen and go directly into remote editing.

:p.If you start Setup with the command
:xmp.            setup -G

:exmp.
then you don't get the small opening screen, and the Local/Remote option
has whatever value it had the last time you ran Setup.

:p.More than one option can be specified, but of course if you try to
specify more than one of 'L', 'R', and 'G' the result is undefined.
These option characters are case-independent, e.g. 'r' means the same as
'R'. In the present version of Setup the '-' character is ignored, so it
does not matter if you omit it. This could change if a future version of Setup
has command-line parameters more than one character long.

:p.The configuration details are split up into a number of different
notebook pages. Click on the notebook tabs to select
the page you need. If you have Warp 4 or better, you can also get a
menu of pages by clicking on the tab area with mouse button 2.

:p.
The page tabs are

:ul compact.
:li.:link reftype=hd refid=pmconfiguser1.Basic:elink.
:li.:link reftype=hd refid=pmconfigimap.IMAP:elink.
:li.:link reftype=hd refid=pmconfigdomains.Domains:elink.   (in multidomain mode only)
:li.:link reftype=hd res=1002.Users:elink.       (absent in multidomain mode)
:li.:link reftype=hd res=1004.Aliases:elink.   (absent in multidomain mode)
:li.:link reftype=hd refid=pmconfiguser3.Local:elink.        (absent in multidomain mode)
:li.:link reftype=hd refid=pmlogging.Logging:elink.
:li.:link reftype=hd refid=pmconfigfilters.Filters:elink.
:li.:link reftype=hd refid=pmconfigoptions1.Options 1:elink.
:li.:link reftype=hd refid=pmconfigoptions2.Options 2:elink.
:li.:link reftype=hd refid=pmconfigoptions3.Options 3:elink.
:li.:link reftype=hd refid=pmrelay.Relay:elink.
:li.:link reftype=hd refid=pmconfigwhitelist.Whitelist:elink.
:li.:link reftype=hd refid=pmconfiguser4.Trusted:elink.
:li.:link reftype=hd refid=pmconfiguser4a.GateFor:elink.
:li.:link reftype=hd refid=pmconfiguser5.Banned:elink.
:li.:link reftype=hd refid=pmconfiguser6.Blacklists:elink.
:li.:link reftype=hd refid=pmregister.Register:elink.    (obsolete)
:eul.

:p.The instructions on the following pages talk of clicking on various
screen controls - that is, pointing the mouse cursor to the control and
clicking mouse button 1. As with all OS/2 dialogue controls, you can
of course also move around a dialogue using the tab key and the cursor
movement keys on the keyboard.

:p.If you find that the text in the Setup program does not fit into
the available space - this will depend on your screen resolution and
on the language used - you can change the fonts. To do this, drag a
font from the Font Palette (which is usually found in your System Setup
folder) and drop it either onto a notebook or onto a notebook tab,
depending on what you want to change. Fonts dropped onto individual
fields of the notebook dialogue will not be remembered, except in the
special case of the page tabs, but fonts dropped on the background of
any notebook page will be remembered and will affect the entire
notebook.

.***********************************
.*   THE BASIC PAGE
.***********************************

:h3 id=pmconfiguser1.Setting the basic server parameters
:hp2.Setting the basic server parameters:ehp2.
:p.
The first page in the Setup notebook starts with an SMTP section, a POP
section, and a message submission section. The SMTP server is for receiving incoming mail (and for forwarding it
to other SMTP servers, if appropriate). The POP server is the software that
lets your users pick up the mail that has arrived for them. If Weasel is the
only e-mail software you are running, then you would normally enable both
of these.

:p.The message submission port is, in effect, an extra SMTP port to which
your local users may submit mail. The original SMTP standard made no distinction
between mail that was entering the mail transport system and mail that was already
in the system and was being relayed from one mail server to another. A revised standard
called RFC2476 makes this distinction: it proposes that mail should initially be
submitted on port 587, to mark it as "newly entering" mail, and that the standard
SMTP port 25 should be reserved for server-to-server transfers of mail that has already
entered the system. It should be noted, however, that some existing e-mail software
does not understand this distinction, so it is possible that the message submission port will
never be used. You should enable this third service if your users have software
that knows how to use it.

:p.Mail received on the message submission port is treated in the same way as
mail received on the SMTP port, except for the way in which "trusted" status
is established. "Trusted" status is needed to send relay mail, i.e. mail to
be passed on to other servers. (Local mail must always be accepted if the
recipient address is valid; this does not require "trusted" status.) For SMTP
mail there are several alternative ways of establishing "trusted" status,
including several authentication methods and use of the "Trusted" list of
trusted hosts and the "GateFor" list of non-checked destinations. For mail
on the message submission port all of these mechanisms are ignored. The
:hp2.only:ehp2. way of becoming an acceptable sender on this port is by
using CRAM-MD5 authentication. This is a stricter criterion than Weasel uses
on the SMTP port; but the whole intent of the message submission port is
that stricter criteria should be used.

:p.All three servers have the options&colon.

:dl break=all.
:dt.     Port
:dd.The TCP/IP port on which the server listens for connections. Unless you are
doing something nonstandard (for example, running two mail servers
on the same machine) this should always be 25 for the SMTP server, 110 for
the POP server, and 587 for the message submission port.

:p.Hint: One use for nonstandard ports is to allow you to test Weasel without
disabling your existing mail software. You don't have to switch to the standard
port numbers until you have decided that you trust Weasel not to lose your mail.
Another use is where, because of special needs such as special filtering
scripts, you need to run two mail servers (for example, two copies of Weasel)
in tandem.
:dt.     Timeout (seconds)
:dd.The time that a session may remain idle before it is forcibly closed.
Most SMTP and POP clients will log out cleanly, but occasionally the server has to
terminate a "dangling" session where, for example, the client machine crashed.
A typical choice for the timeout would be 120 seconds (i.e. 2 minutes).
:dt.     Max users
:dd.This specifies how many clients will be allowed to use the server simultaneously.
I usually set this to 10. The number is not particularly critical; you would have to
have a very busy mail node before getting simultaneous access from many clients.
Note that making this number too large can make Weasel use many threads, which
could lead to performance problems.
:dt.     Enabled
:dd.This box must be checked in order to allow the corresponding server to run.
You can disable one of the servers if you are running Weasel in conjunction with
a different POP or SMTP server, or if you have any other reason for running Weasel
as a POP-only or SMTP-only system. It would not make sense to disable all
sections, because then Weasel would have nothing to do.
:edl.

:p.Below the POP settings group there is an estimate of how many threads
Weasel will use, based on your current options. An OS/2 VIO program cannot
safely use more than 256 threads. If the estimate is approaching that limit,
you might have to reduce some of the "max users" numbers.

:p.Next, you may specify a language code, which is usually a two-letter code
but which is allowed to be up to 32 characters long. This controls the
language used by the Setup program. A code 'xy' will be accepted if the
file setup.xy.lng exists in the Weasel directory. To add support for a
new language, you need only copy one of the existing setup.*.lng files
(they are plain text files)
and translate the messages in the obvious way.

:p.(For the translations into non-English languages, I am indebted to Allan Holm
(Danish), Ronald van Ovost (Dutch) Fritz Schori
(German, Spanish), and Marion Gevers (French). If you have a new
translation, I would be happy to add it to the distribution.)

:p.Next, there is a checkbox to switch to and from multidomain mode.
In multidomain mode, the Users, Aliases, and Local page disappear from
the Setup notebook, and a "Domains" page is added. For further information,
see the
:link reftype=hd refid=multidomainmode.Multidomain Mode:elink.
section in this manual.

:p.:hp2.Note.:ehp2. It is risky to change from single-domain mode to
multidomain mode, or vice versa, while the server is running. The mode
change alters the subdirectory structure inside the "mail root" directory,
and unexpected effects can occur if you change the directory structure
while the server is accessing files in those subdirectories. For more
detail, see the
:link reftype=hd refid=configuration.Configuration:elink. page.
For safety, you should shut down Weasel before making a mode change, and
then restart it after you have exited from the Setup utility.


:p.The final item on this page is the root directory for mail. This should be the
full path name of the directory that will be used for storing users'
mail files. This directory should exist, and preferably should not be used
for anything else. Note that Setup will create subdirectories in this directory,
if they are not already there, to hold the mail for each user.

.***********************************
.*        IMAP
.***********************************

:h3 id=pmconfigimap.IMAP
:hp2.IMAP:ehp2.

:p.This page is irrelevant unless you are running the program imapd.exe
in addition to weasel.exe. When you are running imapd, it takes its
settings from the Weasel INI or TNI file, so that it has the same knowledge
about domains, users, etc. To obtain imapd.exe, go to the place
where you obtained Weasel, and look for a zip file
called IMAPDN.N.zip, where N.N is the version number; and then unzip
it into the Weasel directory.

:p.The Port, Timeout, Max users, and Enabled fields have the same meanings
as on the :link reftype=hd refid=pmconfiguser1.Basic:elink. page.
The standard port for IMAP is 143, so you should not change that unless
you are doing something unusual. The Timeout value probably needs to be a lot
longer than for a POP or SMTP session, because IMAP clients expect to stay
connected for a long time.

:p.Below that, you have the option of logging the IMAP operations to
disk, or screen, or both. Note that IMAP can generate very large log
files, so you might not want to log to disk unless you have a lot of
free space or you are trying to track down a problem.

:p.:hp2.Enabling IMAP for your users:ehp2.

:p.Because IMAP is "expensive", we don't necessarily make it available
to all users. Weasel actually has a separate "IMAP enable" for each
user account, to allow the finest possible control.

:note.If you are upgrading from a version of Weasel prior to version
1.73, you will probably have to check your user accounts to ensure
that IMAP support is enabled/disabled according to your preferences,
because in those earlier versions there were no IMAP settings.

:p.In the Setup page for each user, there is an "Allow IMAP" checkbox.
This is how you control IMAP access on a per-user basis.

:p.Naturally, you need a default so that you don't have to step through
all the users one by one. The default is the "Enable IMAP for new users"
checkbox on the "Users" page of Setup. In multidomain mode, there is
a separate default for each domain. This checkbox does not actually
enable or disable anything. Instead, it provides the initial state for
the "Allow IMAP" option each time you create a new user in this domain.

.***********************************
.*   THE DOMAINS PAGE
.***********************************

:h3 id=pmconfigdomains.Domains
:hp2.Domains:ehp2.

:p.This page exists only in
:link reftype=hd refid=multidomainmode.Multidomain Mode:elink.

:p.Once you have activated multidomain mode, which is done on the
:link reftype=hd refid=pmconfiguser1.Basic:elink.
page, your Setup notebook will
contain a page called "Domains". If you wish to
host multiple domains, you should probably first read the
:link reftype=hd refid=multidomainmode.Multidomain Mode:elink.
section in this manual.

:p.The first item on this page is a checkbox to switch you back to
single-domain mode. This is useful if you are just exploring the
Setup notebook, and want to be able to reverse your decision. If you
do switch back to single-domain mode, the list of users and similar detail
is migrated from just one of the domains in the list. The directories
for the other domains, if any, are not destroyed, but are no longer
accessible from Weasel.

:p.When you make this switch you might get a message that some files
or directories could not be deleted. This is normal, and probably
indicates something like a directory that could not be deleted because
it was not empty. In that case you should manually check the directories to
look for things like messages that have not yet been fetched.

:p.Next, you will see a list of domains. When you first switch
to multidomain mode, there will be just one domain with an implausible
name. You should immediately rename that domain to give it the name
of a domain you own. The listbox should contain
precisely the names of your mail domains. That is, if you are hosting
a mail domain called coyote.acme.net, then one of the entries in that
listbox should be the name coyote.acme.net.

:p.The order of the domains in the listbox is significant, because it
defines the search order when a user logs in. For more detail on this,
see the section on
:link reftype=hd refid=identifyusers.matching users with domains:elink..

:p.There are five operations available to you on this page.

:p.:hp2.Adding a new domain:ehp2.

:p.If you select the "Add" button, you get a box inviting you
to type the domain name for the new domain. Finish this with
the <Enter> key, and then proceed as for
:link reftype=hd refid=editdomain.editing the properties of a domain:elink..

:p.:hp2.Renaming an existing domain:ehp2.

:p.The "Rename" button allows you to change the name of the selected domain.
Finish this operation with the <Enter> key.

:p.:hp2.Editing the properties of a domain:ehp2.

:p.The "Edit" button opens a new dialogue where you can
:link reftype=hd refid=editdomain.edit the properties:elink.
of the currently selected domain. You can also get the same
effect with a double-click of mouse button 1.

:p.:hp2.Changing the order of domains in the list:ehp2.

:p.The "Promote" button moves the currently selected domain one
place higher in the list of domains.

:p.:hp2.Deleting a domain:ehp2.

:p.The "Delete" button completely removes the currently selected domain.
This is a major change, so make sure that you really mean it.  If
some directories cannot be deleted - because, for example, some users
still have undelivered mail - you will get a message saying that
the remainder of the deletion must be done manually.

.***********************************
.*   EDITING THE PROPERTIES OF A DOMAIN
.***********************************

:h4 id=editdomain.Editing the properties of a domain

:hp2.Editing the properties of a domain:ehp2.

:p.When you choose to edit a domain, you get a new notebook with
three pages&colon.

:dl tsize=15 break=none.
:dt.    :link reftype=hd res=1002.Users:elink.
:dd.The list of users for this domain.
:dt.    :link reftype=hd res=1004.Aliases:elink.
:dd.The username aliases for this domain.
:dt.    :link reftype=hd refid=pmconfiguser3.Local:elink.
:dd.The hostnames and IP addresses that belong to this domain.
:edl.

:p.The 'Users' and 'Aliases' information are exactly the same as for
the single-domain case.
The 'Local' list is mostly the same as in the single-domain case,
but with one extra rule: the Local page must contain at least one
numeric IP address, or a textual name that will translate to a
numeric IP address. For many installations, you can satisfy this rule
by using the "add all local addresses" button on the Local page, but
some system managers will want to allocate different IP addresses to
different domains.

.***********************************
.*        USERS
.***********************************

:h3 res=1002 id=1002 global toc=1234.Users

:p.:hp2.Users:ehp2.

:p.In single-domain mode, this page will be found in the main Setup notebook.
In multidomain mode, it is instead found in the notebook belong to that domain,
a notebook which is accessed from the
:link reftype=hd refid=pmconfigdomains.Domains:elink. page.

:p.
Adding a username on this page has three results&colon.

:ul compact.
:li.It tells the Weasel SMTP server to accept mail for that user on
this machine.
:li.It allows the user to log in to the Weasel POP server and collect any
mail that has arrived.
:li.It permits the user to relay outgoing mail through Weasel via
the SMTP AUTH command and/or via POP-before-SMTP authentication,
depending on what is enabled on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink. page.
:eul.

:p.What the users are able to do is also affected by the checkboxes at
the bottom of this page. These are described in the
:link reftype=hd refid=pmconfigupoptions.User Page Options:elink. section.

:p.Users who have had their status set to 'inactive' are indicated by an
asterisk (*) in the list of users.

:p.
:hp2.Adding a new user:ehp2.
:p.
Click on the "Add" button, and then proceed as for :link reftype=hd res=1003.Editing a user record:elink..
:p.
:hp2.Editing the details for an existing user:ehp2.
:p.
Click on the "Edit" button, and then follow the instructions in the section :link reftype=hd res=1003.Editing a user record:elink..
:p.
:hp2.Deleting a user:ehp2.
:p.
Click on the username to be deleted, and then click on the "Delete" button.

.***********************************
.*        USER PAGE CHECKBOXES
.***********************************

:h4 res=1005 id=pmconfigupoptions.Options on the Users page
:hp2.Options on the Users page:ehp2.

:p.At the bottom of the Users page there is one or more checkboxes, which control the
options described below. Note that these are domain-specific options, since
there is a separate Users page for each domain.

:p.
:hp2.All users may use SMTP AUTH:ehp2.
:p.
As described in the early part of this manual, a mail server must have
controls on who is allowed to relay mail through it, because without
such controls the server will become a target for spammers. Almost
invariably these controls implement, in one way or another, a rule that
allows the server's own users to relay through the server but prevents
anyone else from relaying.

:p.One of the most popular ways of implementing such a restriction is
with the SMTP AUTH command. The AUTH command is an extension to the
SMTP standard that lets a sender supply a username and password (usually
encrypted, but this depends on which version of the AUTH command is used)
as part of the operation of supplying mail to be relayed out. The
presumption is that anyone who can supply a password is a local user,
while spammers will not know the password. The weakness of this system
is that it only requires authentication for one account on the system.
If just one username/password combination is stolen (or guessed), then
the spammer is able to use the server as a relay.

:p.If the checkbox being described here is checked, it means that any
username/password pair belonging to a legitimate user can be used in
the AUTH command. This is the conventional way of working, and is the
initial default for all domains.

:p.If the checkbox is not checked, then the AUTH permission has to be
enabled or disabled individually for each user in this domain. This makes a little
extra work for the system manager, but it makes it possible to ensure
that the AUTH permission
is only given to those users who genuinely need it.

:p.
:hp2.Enable IMAP for new users:ehp2.
:p.
The IMAP component of Weasel is not at present working satisfactorily,
so it should be avoided until the problems can be fixed.

Once IMAP is working, it will be possible to enable or disable it
individually for each user.

The present control is simply an initial default for when a new user
is being added to the system. The initial value can be changed when
the user record is being filled in.

:p.
:hp2.POP fetch starts with most recent (obsolete):ehp2.
:p.
This option is obsolete, and should not be present. You might see it
if you are using an older version of the Setup program.

:p.An older version of Weasel had the restriction that only the first
512 messages were visible to a POP3 client. This was causing trouble
for people who left their mail on the server for long periods of time.
Such people could be supported by doing the fetch backwards, so that
they only saw the :hp3.last:ehp3. messages. This was bound to cause
trouble sooner or later, so Weasel has now been modified such that it
makes visible all of the messages on the server, no matter how many
there are. This option does slow down POP operations, but the
slowdown affects only those people with many messages to process.

:p.It is still important that you, as a system manager, warn people
that they cannot safely leave large numbers of messages on the
server. Although the problem has now been fixed at the server end,
there is still a potential problem at the client end. How long will
it be until the client software loses track of which messages have already
been fetched? The limits for popular mail clients are, unfortunately,
not documented.

.***********************************
.*        USER EDITOR
.***********************************

:h4 res=1003 id=pmedituser global.Editing a user record
:hp2.Editing a user record:ehp2.
:p.
When you add a new user, or edit an existing user, you get a new dialogue
box with a number of options and entry fields.

:p.The first checkbox is labelled "Inactive account". Selecting this option
in effect removes the user account. Mail for this user will not be accepted,
and the user cannot fetch mail. This option is for the case where you want
to deactivate an account temporarily, with the possibility of restoring
it at some future time. (If you want a permanent deactivation, it is better
to delete this user record rather than to edit it. There is no point in
retaining information you will never use.)

:p.The "Allow IMAP" box lets you enable or disable IMAP for this user.

:p.The "Allow SMTP AUTH" box lets you specify whether this user will be
able to use the extended SMTP command AUTH to authenticate outgoing mail.
If this box is disabled, it will be because you have already given
AUTH permission to everyone in this domain, in which case an individual
permission would be redundant. Note: if you want disable AUTH for everyone,
you can do that on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink. page.

:p.Next, we have the main account details.
:dl break=all.
:dt.    Username
:dd.The name that the user will use when logging in. The username is not case
sensitive, and Setup will store it in lower case even if you type it in in
upper case or mixed case.
:dt.    Password
:dd.This user's password. The password is case sensitive; that is, the user
must use exactly the same mix of upper and lower case that you used when
typing it in.
:dt.    Real name
:dd.The user's real name. This is for your own information, and is optional.
:edl.

:warning.If you change the username, the data for the previous
username will be deleted. You should also avoid using a username
that is the same as for some other user. Most importantly, if
the same username occurs in more than one domain then the passwords
should be different.:ewarning.

:p.The next section is for forwarding incoming mail to a different e-mail
address. This can be used for situations like having a user temporarily
or permanently relocated, or for making archival copies. The forwarding
address can either be local or remote. If you activate this option, you
must also specify an e-mail address. Choose the "also keep local copy"
option if you want two copies of all messages, one for the local account
and one to the forwarding address.

:p.The "Comments" field below this is for your own use. You can put
anything you like in here, up to 2048 characters in length.
The "Real name" and "Comments" fields are not actually used by
Weasel. They are there purely for the convenience of the system
manager.

:p.The "Override filter" option is for use when you want the final
:link reftype=hd refid=filters.filter:elink. for mail to this user
to be different from the global default filter. There are three
possibilities:
:ul.
:li.If you want mail for this user to be handled just like mail for
any other user, you should disable the "Override filter" option.
:li.If you have a special filter for mail to this user, enable the
"Override filter" checkbox and put the filter name next to the
checkbox. This will cause that filter to be used instead of the
global default setting for the final filter.
:li.If you want no filtering for this user, enable the "Override
filter" checkbox but leave the filter name blank.
:eul.

:p.This option applies only to the final filter. The filters for
earlier stages cannot be changed on a per-user basis.

:p.
Alter these as desired, and then either type the Enter key or click on
the "OK" button to confirm the change. If you change your mind, click on
the "Cancel" button, or type the Esc key, and your changes will be ignored.

.***********************************
.*           ALIASES
.***********************************

:h3 res=1004 id=pmaliases toc=1234 global.Aliases
:hp2.Aliases:ehp2.

:p.In single-domain mode, this page will be found in the main Setup notebook.
In multidomain mode, it is instead found in the notebook belong to that domain,
a notebook which is accessed from the
:link reftype=hd refid=pmconfigdomains.Domains:elink. page.

:p.
An alias looks like a username from the viewpoint of incoming mail,
but the name does not correspond to the name of any POP or IMAP user. Instead,
the alias refers to a list of e-mail addresses. Whenever an e-mail
arrives addressed to that alias, a copy is sent to everyone on the
list.

:p.In multidomain mode, each domain has its own set of aliases. (There are no 'global'
aliases that extend across domains; there would be no point in
having such a feature.) Apart from this detail, aliases in
multidomain mode work exactly like aliases in single-domain mode.

:p.:hp2.Example 1.:ehp2. Suppose you have a user called Bill Smith who
wants to receive all mail in duplicate, with one copy to each of his two
computers. You can do this by giving him two user accounts called bill1
and bill2, and setting up an alias "bill.smith" defined as
:xmp.
      bill1
      bill2
:exmp.
:p.With this arrangement, people send mail to Bill by addressing it to
Bill.Smith@yourdomain, but Bill collects his mail by logging in as
bill1 from one of his computers, and as bill2 from the other.

:p.:hp2.Example 2.:ehp2. You can use an alias to set up a simple
mailing list. Suppose, for example, you have a group of friends who
are interested in chess and want to discuss it as a group. If you
create an alias called "chessgroup", with entries like
:xmp.
      "Alan Jones" <alanj@xyz.org>
      Bill.Smith
      susan@alpha.beta.uk (Susan G.)
      kw123
      "Me" <myself>
:exmp.
:p.then anyone can send mail to chessgroup@yourdomain, and copies will
be distributed to everyone on the list.

:p.Alias lists may themselves contain aliases. To guard against circular
definitions, duplicate names are stripped out during the expansion.

:p.As Example 2 shows, an alias list may contain both local and non-local
e-mail addresses, and this can generate relay mail even if the sender is
not otherwise permitted to send relay mail. As a guard against abuses,
an alias can be set up to be either "public" or "private". Anyone can
send mail to a public alias. A private alias can be accessed only by
those senders with "relay mail" privilege.

:p.The following pages explain how to create and modify aliases.

.***********************************
.*        ALIAS EDITOR
.***********************************

:h4.Adding and changing aliases
:hp2.Adding and changing aliases:ehp2.
:p.
In the "Aliases" page of the Setup notebook, you have the following options&colon.
:p.
:hp2.Adding a new alias:ehp2.
:p.
Click on the "Add" button, and type the alias name into the entry field that
appears. (The entry field will close when you type the Return key.) Then
proceed as for :link reftype=hd refid=pmeditalias.Editing an alias expansion:elink..
:p.
:hp2.Renaming an existing alias:ehp2.
:p.
Click on the listbox entry that you want to change, then click on the "Rename"
button. Edit the name in the entry field that appears. The name will be changed
when you type the <Enter> key, or when you close the entry window.
:p.
:hp2.Editing the expansion of an existing alias:ehp2.
:p.
Click on the listbox entry that you want to change, then click on the "Edit"
button and follow the instructions in the section :link reftype=hd refid=pmeditalias.Editing an alias expansion:elink..
:p.
:hp2.Changing the order of the entries:ehp2.
:p.
Click on the desired entry, then click on the Promote or Demote button.
This will swap the selected entry with the one above or below it. By doing
this enough times, you can sort the entries in any way you wish.
The order is important only when some of the entries contain
:link reftype=hd refid=pmwildalias.wildcards:elink..
:p.
:hp2.Deleting an alias:ehp2.
:p.
Click on the entry you want to delete, then click on the Delete button.

.***********************************
.*        EDITING AN ALIAS EXPANSION
.***********************************

:h4 id=pmeditalias.Editing an alias expansion
:hp2.Editing an alias expansion:ehp2.
:p.
An alias expands out to a list of e-mail addresses; these can be either local
or remote addresses. When you add or edit an alias, you can manipulate this list.
:p.
First, you should decide whether this is to be a private or public alias, and
select the "Private" checkbox if you don't want this alias to be public. Anyone can send
mail to a public alias, but only people sending from one of the "Trusted" hosts
can send mail to a private alias.

:p.
:hp2.Adding a new e-mail address:ehp2.
:p.
Click on the "Add" button, and then type an e-mail address. Use the <Enter> key
when finished, to close the entry-field window.
:p.
:hp2.Editing an existing entry:ehp2.
:p.
Click on the listbox entry to be changed; click on the "Revise" button; and then
edit the e-mail address in the entry field that appears.
:p.
:hp2.Deleting an existing entry:ehp2.
:p.
Click on the listbox entry to be removed, and then click on the "Delete" button.

:p.
When you have finished making modifications on this page, click on the "OK" button
to confirm your changes, or click on the "Cancel" button to throw away the
changes.

.***********************************
.*        WILDCARD ALIASES
.***********************************

:h4 id=pmwildalias.Wildcard aliases
:hp2.Wildcard aliases:ehp2.

:p.One special case is where the name of an
:link reftype=hd res=1004.alias:elink. contains the wildcard
characters '?' and '*'. When checking a local e-mail address to see
whether it is an alias, the '?' matches any single character, and the
'*' matches any substring of zero or more characters. For example,
if you have an alias called 'a*b?c*d', this will match an incoming
address 'abdabecfd@yourdomain', where 'yourdomain' is the local domain
for which you are doing the check.

:note.Although wildcard characters can be used in the :hp2.name:ehp2. of
an alias, they should not be used in the addresses in the
:hp2.expansion:ehp2. of that alias. The alias should expand out to
ordinary e-mail addresses that do not contain wildcard characters.

:p.The most obvious application of this feature is where you have an
alias whose name is the single character '*', and whose expansion is
the address of some local user, for example the username 'unknown'.
Since this will match any address at all, it can be used to accept
mail for all usernames for which you have :hp1.not:ehp1.  created an
account. This can be useful for things like junk mail checking.

:p.When you use wildcards, it is obviously possible for an incoming
address to match more than one alias. In that case, the one that
occurs first in the list of alias names is the one that is used.
For that reason, you should normally order the names such that the
wildcard entries come at the end of the list.

.***********************************
.*   NAMES FOR THE LOCAL HOST
.***********************************

:h3 id=pmconfiguser3.Local
:hp2.Local:ehp2.

:p.In single-domain mode, this page will be found in the main Setup notebook.
In multidomain mode, it is instead found in the notebook belong to that domain,
a notebook which is accessed from the
:link reftype=hd refid=pmconfigdomains.Domains:elink. page.

:p.
This notebook page lists the names that Weasel should accept as
names for the machine on which it is running.

:p.An e-mail address has the form "user@domain". If your local
nameserver has an MX record for your machine that matches the "domain"
part, then this is mail to be delivered locally.

:p.In the case where the "domain" part is actually the hostname of
your machine, or an alias known to your local nameserver, and the server
is in single-domain mode, Weasel can work
this out for itself. Many users, therefore, can afford to leave this
list empty. Unfortunately Weasel can't (yet) handle the case where the
domain is not a hostname, but is instead something mapped to a hostname
by a nameserver MX record. (The catch here is that we can't tell
whether the MX record identifies the final destination, or simply a
relay host. If there's a way to make this distinction, I haven't
yet worked it out.) In such cases, then, you must explicitly list
the domain name(s) on this page.

:p.In multidomain mode, the Local page must contain at least one
numeric IP address, or a textual name that will translate to a
numeric IP address.

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=pmhostlist.Editing a list of host names:elink..

:p.Note, by the way, that you can't define hostname aliases simply by adding
extra entries to this list. The aliases are no good unless they are
also recognised by the nameserver. If the nameserver does not
define a name, then there is no way for people at other sites to use
that name as part of an e-mail address; their own SMTP client will
report an "unknown host" error.

:p.Note, too, that some mail clients will refuse to send mail to
your machine if they can't find an MX record for it. You should ask
your local network manager - the person who looks after the
nameserver - to include MX records for your computer in the
nameserver tables.

:p.:hp2.Add all local IP addresses:ehp2.

:p.This is equivalent to a set of 'Add' operations, where the
IP addresses for interfaces on this host are added to the listbox.
Addresses that are already listed are not added again, so a
second press on this button will have no effect.

:p.The 'Add all local IP addresses' option is simply a convenience
to save you some typing. Often the easiest way to define the
IP addresses for this domain is to add the addresses for all
interfaces on this machine, and then delete the ones that you
don't want to associate with this domain.

:p.:hp2.Strict domain name checking:ehp2.

:p.In the multidomain case only, the "Local" page has a checkbox for
strict domain name checking. This works as follows.

:p.When mail arrives for username@dname, we have to decide which
domain this mail is for. If dname matches one of our domain names,
then the decision is obvious. When strict domain name checking
is enabled for a domain, the :hp2.only:ehp2. way that "dname" can be
accepted for that domain is if it precisely matches the domain name.

:p.Most typically you will not want to enable this option,
because it is common for domain names to have aliases recorded
in the nameserver records. For example, if you own the domain
names xyz.com and xyz.net you might want these two names to be
treated as equivalent. To do this, you could set up your
nameserver records such that xyz.net is an alias for xyz.com, and
then set up a domain called xyz.com in the Weasel setup. Since
Weasel checks the nameserver for aliases, it will accept mail
for both addresses but store that mail in a single domain.

:p.Thus, the strict domain checking is really an instruction to
Weasel to ignore the extra information from the nameserver.
If your nameserver records are set up correctly then you are
unlikely to need this option. You might, however, need it if an
overlap in the aliases is causing mail to go to the wrong
domain, and you have no way of getting that overlap corrected.

.***********************************
.*   THE LOCAL LIST FOR A DOMAIN
.***********************************

:p.:hp2.Notes for the multidomain case:ehp2.

:p.Although this notebook page is used for both the single-domain
mode and the multidomain mode, there is an important difference.
In single-domain mode, Weasel automatically binds itself to all
the local interfaces it can find, so very commonly you don't have
to enter anything at all on the 'Local' page. We cannot do this
in multidomain mode, because if you have multiple network interfaces you
will probably want to partition them such that each domain is bound
to only one (or perhaps more in rare cases) IP address. Because
of this, you must explicitly say which addresses belong to this
domain.

:p.Entries in the listbox can be either textual hostnames, such
as uvw.xyz.com, or numeric IP addresses, such as [10.0.1.5].
It is convenient to describe these two possibilities separately,
because they are used for different purposes.

:p.Note, however, that Weasel uses nameserver lookups to expand
the information you have entered. For each textual entry, the
corresponding numeric address is added to Weasel's internal
'Local' list. (But not conversely. Translating numeric addresses
to domain names is possible, but would cause trouble when, as is
often the case, two or more domains share the same IP address.)
Thus, the internal list can be
longer than the list you see on the 'Local' notebook page.

:p.:hp2.Numeric IP addresses:ehp2.

:p.A numeric entry is an IP address in the conventional dotted
notation, for example [192.168.3.7]. The brackets are optional,
but are usually preferred for neatness. In addition, there will
be 'implied' IP addresses determined at run-time, as the result
of nameserver translation of the textual hostnames.

:p.These addresses are used to construct internal tables showing
which domains belong with which IP addresses. When a POP3 user
logs into Weasel, he or she is connecting to a specific IP
address, so from this Weasel can deduce that this user belongs
to a subset of the possible domains.

:p.:hp2.Textual entries:ehp2.

:p.Every mail domain has a name, and the official name should
match the name on the 'Domains' page of the Weasel setup
notebook. Sometimes, however, you might give several names
to the same domain. In that case, the extra names should appear
on this 'Local' page.

:p.These entries are consulted whenever it is necessary to
resolve the 'domain' part of an e-mail address in the form user@domain,
in order to work out whether this is a local address and, if so, what
local domain it belongs to.
We consider that we have a match if we match the domain name itself,
or one of the entries in that domain's 'Local' list.

:p.Of course the numeric entries are also checked if the address
is specified numerically. That is, if mail arrives addressed to
user1@123.45.6.7 or user1@[123.45.6.7], we will accept a match if
one of this domain's IP addresses happens to be 123.45.6.7.

:p.:hp2.Wildcards in textual entries:ehp2.

:p.The Setup program allows you to put textual entries into the 'Local' list
that contain '?' and '*' wildcards. If you use such entries,
addresses will be checked using the usual rules for wildcard matches.
(The '?' matches any single character, and the '*' matches any
substring of zero or more characters.)

:p.In practice, however, there are not many situations where you
would be justified in using wildcards in this list. The wildcard
facility is really intended for use in the more global lists such
as the list of banned hosts.

.***********************************
.*   THE LOGGING PAGE
.***********************************

:h3 id=pmlogging.Logging
:hp2.Logging:ehp2.

:p.Weasel permits you to have several different log files. This page is
for specifying how much logging you want.

:dl break=all.
:dt.     SMTP logging
:dd.If this feature is enabled a record of received mail is written to the file
whose name you specify here. The default filename is SMTP.LOG. Each line in this
file described one received mail item: date,
time, sending host, size, and a list of recipients.
:dt.     POP user logging
:dd.If this feature is enabled a brief summary of each POP session is written
to the file whose name you specify here. The default filename is POP.LOG.
:dt.     Outgoing mail
:dd.If this feature is enabled a record of outgoing mail is written to a file
called SMTPOUT.LOG, or whatever other filename you specify here.
:dt.     Detailed transaction log
:dd.You can choose to send a detailed log to the screen, to a disk file,
to a pipe, to the "syslog" system log, or to any combination of these.
:p.If logging to disk, you can also choose the name of the log file; the default
name is WEASEL.LOG, and it is updated approximately once every
minute if this feature is enabled.

:p.If the syslog option is enabled, you would normally want to leave
the syslog host name set at 127.0.0.1 (the local host). You may, however,
log to a different machine by specifying a hostname or a numeric IP
address in the "Syslog host" field.

:p.The "more detailed logging" checkbox under the log file name puts
extra detail into the transaction log. This option is mainly for testing
and debugging, so exactly what extra details you get varies from one
version to another. It is a good idea to have this option turned on if
you are logging for the purpose of reporting a problem.

:p.Sending the log to a pipe is not a useful thing to do unless you
have a separate program reading the data from the other end of the pipe.
The pipe option is designed for cases where you want to do your own
real-time analysis of the log entries, or where you want to change the
log to a different format, or similar applications. To get some ideas
of what can be done, fetch the "pipelog" package from the Weasel Tools
web site.

:edl.

:p.The transaction logging section includes an option "Suppress POP sessions in
transaction log". The reason for this option is that POP sessions put a large
amount of repetitive information into the transaction log, mostly because POP
clients keep logging in every few minutes. (It's even worse if some of those
clients leave mail on the server; those clients create a huge load on the server
because they keep requesting lists of files they have already downloaded, just
so that they can work out which ones they already have. In extreme cases, this
can put a major load on the network.) Probably the only time you need to check
the POP session logs is when you suspect a break-in using a stolen password.
The rest of the time, the main thing you want to check are the SMTP sessions,
since this is the main place you will get information about spammers. Thus, you
might as well leave the POP information out of the transaction log.

:p.Note that this option affects only the transaction log. You can still have
a separate POP log file, as noted earlier in the "Transfer logging" section of this page.

:p.Note 1. The default location for all log files is in the working directory;
that is, the directory you were in when you started Weasel.exe.
You may, of course, include a drive and/or directory
specification when setting these file names, if you prefer to keep the
log files in a different directory.

:p.Note 2. The reason why the transaction log file is updated periodically
rather than continuously is to avoid the complications that can arise if
two programs have a file open simultaneously. The actual log entries go
to a temporary file with a name like WEASEL.$$$. Every minute the
temporary data are appended to the main log file, and a new temporary file
is created.

:p.Note 3. Transaction logging can create very large log files. I suggest that
you don't write a transaction log to disk unless you are trying to track down a
problem, or unless you regularly move your logs to an archive. Logging to the
screen, on the other hand, creates only minor overheads and is a good way of
seeing what the current operation is.

:p.If you do want detailed logging, but don't want large log files, the
sensible option is to archive the log files every month (or every week,
or every day, depending on the traffic level at your site). You can find a
suitable archiving script at http&colon.//www.pmoylan.org.

:p.In the longer term, once you are satisfied that Weasel is doing what it is
supposed to be doing, you might want to run Weasel
:link reftype=hd refid=detached.detached:elink..
In that case, you might as well disable logging to the screen, because
detached programs never write to the screen anyway.

.***********************************
.*   THE FILTER SETUP OPTION PAGE
.***********************************

:h3 id=pmconfigfilters toc=1234.Filters
:hp2.Filters:ehp2.
:p.
A filter is an external program (or Rexx script, Perl script, etc.) that
checks incoming mail as it is being received. Weasel allows you to do
the check at any of five stages.

:dl break=all.
:dt.     :hp2.Stage 0 filter:ehp2.
:dd.The filter that is called on initial connection, where we know the IP
address of the other machine but not yet any other details.
:dt.     :hp2.Stage 1 filter:ehp2.
:dd.The filter that is called after we have received the HELO or EHLO
command from the remote host.
:dt.     :hp2.Stage 2 filter:ehp2.
:dd.The filter that is called after we have received the MAIL FROM command,
which gives the (real or faked) identity of the sender.
:dt.     :hp2.Stage 3 filter:ehp2.
:dd.The filter that is called when we know the sender and the list of
recipients, but we don't yet have the body of the message.
:dt.     :hp2.Stage 4 filter:ehp2.
:dd.The filter that is called after we have the complete text of the
incoming message.
:edl.

:p.Any of these can be (and usually are) empty strings. If the filter
name is empty, we skip that filtering stage.

:p.This page also contains a 'Serialize filter operations' option.
If you use a filter then you should probably enable this option. Filters can
misbehave in unexpected ways when several instances of the filter are run in
parallel. (Skilled programmers can get around this problem, but it is not always
easy, especially in a scripting language such as Rexx or Perl.) The "serialize" option ensures
that multiple instances of the filter, invoked by simultaneously incoming mails,
are queued up to run one at a time rather than being run in parallel. This can create
a slight delay for the sender, but this does not matter unless the
filter is doing very time-consuming operations. Senders don't mind being delayed
for a few seconds, unless they are junk mailers who are trying to send out
thousands of items in rapid succession.

:p.For more details, see the
:link reftype=hd refid=filters.Using filters:elink. page.

.***********************************
.*   SETUP OPTIONAL SETTINGS
.***********************************

:h3 id=pmconfigoptions1 toc=1234.Options 1
:hp2.Options 1:ehp2.
:p.
This page covers several miscellaneous options. Make sure that you understand
the options before enabling them.

:dl break=all.
:dt.     :hp2.Accept mail for unknown users (obsolete):ehp2.
:dd.If you see this option, you are running an obsolete version of Setup.
If you want to accept mail for unknown users, it is better
to use :link reftype=hd refid=pmwildalias.wildcard aliases:elink.
to direct such mail to (for example) the username 'unknown'.
:dt.     :hp2.Bad password limit:ehp2.
:dd.Specifies how many login failures are permitted before a POP3 session
is forcibly terminated. (To disable this test completely, turn off the
"Enabled" flag.) Ideally you should set this to a small number, for example
3 or 4, to slow down attacks by robots that use trial and error to steal
passwords. This does, however, confuse some Thunderbird users, because of
a bug in Thunderbird that prevents the fetch from happening if the user
supplies a correct password after a failure due to an aborted login attempt.
If this is a problem, and you are willing to tolerate the reduced security,
then you can increase the limit (note that Thunderbird is less confusing if
you specify an even number) or even disable the test.
:dt.     :hp2.Check MAIL FROM address for banned host/domain:ehp2.
:dd.When receiving a message, Weasel checks whether the sender is banned on
initial connection (when we know the IP address of the sender), and again
when it receives a HELO or EHLO command (which specifies the sender's
hostname, although this can be faked). If you enable this option, then Weasel
also checks the domain name specified in the MAIL FROM command. (It checks the
domain name itself, and also the IP addresses in the MX record for this
domain.) This too can be faked, but if it is correct then it specifies the
ultimate source of the message, rather than the
host that is relaying the mail to your computer.
:dt.     :hp2.Apply SPF check to incoming mail:ehp2.
:dd.SPF (Sender Policy Framework) is a mechanism where a mail domain can
specify which IP addresses can legitimately send mail for that domain. This is done
by publishing rules via nameserver records.
Its aim is to stop spammers from pretending to be from the large mail servers
like gmail. Its value is, at present, doubtful, because the large spam
factories make sure that their spam will pass the SPF test. Nevertheless, it
is probably worth using because it will block at least some spam. You might want
to disable this option if, for example, your filters are already doing SPF checks.
:dt.     :hp2.Authentication for relay mail:ehp2.
:dd.This group of options allows your users to gain relay authentication,
i.e. to get permission to send relay mail through your server. Users
with a fixed internet address can be given this permission via the
:link reftype=hd refid=pmconfiguser4.trusted hosts:elink. option,
but it is becoming more common to want "roaming access", where the
authentication is based on a username/password combination rather
than on an address.
:p.Weasel offers two kinds of authentication, and these are explained in the
:link reftype=hd refid=smtpauth.SMTP authentication:elink. section.
You can enable either or both of these mechanisms on this page.
:dl break=all.
:dt.     POP login authenticates SMTP
:dd.Enable this if you want :link reftype=hd refid=popbeforesmtp.POP-before-SMTP authentication:elink..
You have to specify how many minutes the authentication remains valid for.
(If the time value is zero, this effectively disables this form of
authentication, even if a check-mark appears in the box.) Note that this
method of authentication is unnecessary if your users have mail clients
that support the SMTP AUTH command.
:dt.     Allow SMTP AUTH authentication
:dd.The SMTP AUTH form of authentication is explained on the
:link reftype=hd refid=SMTPAUTHcommand.SMTP AUTH:elink. page.
There are actually several different kinds of SMTP AUTH authentication
supported by Weasel, of different security levels, so we allow you
to specify which of those mechanisms with be accepted, depending on
how cautious you are.
:note.Not all mail clients support these options, so you have to
decide for yourself the tradeoff between security levels and how
many users will be able to take advantage of those levels.
:edl.

:note.The authentication options specified here apply to incoming mail
(which wants to use Weasel as a relay) only. Authentication for outgoing mail is an entirely separate
topic, and this is covered on the
:link reftype=hd refid=pmrelay.Relay host:elink.
page.

:dt.     :hp2.Always report our hostname as:ehp2.
:dd.Normally you do not need to activate this option. Weasel needs to
identify the machine it is running on at various stages - for example,
in the "Received:" header lines of mail - and normally it can work
this out for itself. You can, however, use this option to specify
a fixed hostname if Weasel seems to be making an inappropriate
choice. (But beware of faking your address here, because that can cause
other servers to reject mail from you.) The main situation where this
option is needed is where you have a firewall or router that implements Network
Address Translation (NAT), and Weasel is picking up the internal
address rather than the externally visible address.
:edl.

.***********************************
.*   SECOND PAGE OF OPTIONS
.***********************************

:h3 id=pmconfigoptions2 toc=1234.Options 2

:hp2.When to go online:ehp2.

:p.
The "When to go online" option on this page controls how Weasel decides that it is online and can send
outgoing mail. (When it is offline it still works, but it saves any
outgoing mail to be sent later.) You have the choice of three
possibilities.

:dl break=all.
:dt.    When a file called ONLINE is present
:dd.If you choose this option, the server looks for a file called ONLINE
in the same directory as Weasel.exe, and goes online if that file
exists. (The content of the file is not important.) That is, you force
the server to go online by creating this file (or copying it from
another directory), and force it offline by deleting the ONLINE file.
This option is for the case where you want manual control over when
to go online.
:dt.    When a dialup connection is detected
:dd.This is the best choice if your outgoing mail relies on a dialup
connection. Weasel checks periodically to see whether the connection
is active.
:dt.    Always
:dd.This is the simplest case, and the obvious option to choose if you
have a permanent internet connection. With this option Weasel goes
online a few seconds after it is started, and remains online until it
is shut down.
:edl.

:p.:hp2.Postmaster check:ehp2.

:p.This is an anti-spam measure. If it is enabled, Weasel checks the
domain in the MAIL FROM command to see whether it has a valid "postmaster"
account. The SMTP standards require that every mail server have an
account called "postmaster", but spammers often ignore that requirement.

:dl break=all.
:dt.     disabled
:dd.Use this option if you don't want to do the check.
:dt.     mark failures as suspect
:dd.With this choice, messages that fail the check are still accepted,
but Weasel adds a header saying
:xmp.    X-PostmasterCheck: FAIL
:exmp.
or
:xmp.    X-PostmasterCheck: DEFERRED
:exmp.
The optional stage 4 filter called postmastercheck.cmd will, if installed,
make a copy of such messages in a directory called 'suspect' or a directory called 'deferred', which you
may examine manually to see whether the check is truly detecting spam.
(To download optional filters, go to
http&colon.//pmoylan.org/pages/os2/wfilters.html.)
The 'deferred' result is for the case where the postmaster check resulted in
a "try again later" response from the other end. This is not necessarily a
failure.
:dt.     reject failures
:dd.If you choose this option, mail that fails the postmaster check will
be rejected.
:edl.

:p.It is probably a good idea to select the "mark failures as suspect"
option initially, to let you see whether any genuine mail is being
rejected by this test. Once you trust the postmaster check algorithm,
you can switch to rejecting the failures.

:p.Remark: there appear to be an increasing number of legitimate mail
domains that do not have a postmaster account, possibly because some
mail hosting is being done by non-experts who do not understand the
standards. If you are getting mail from one or two domains with that
problem, a simple solution is to
:link reftype=hd refid=pmconfigwhitelist.whitelist:elink.
those domains. If the problem is more extensive, you might have to
disable the postmaster check.

:p.:hp2.Maximum recipients per mail item:ehp2.

:p.A single mail item can have multiple recipients. (Most obviously when
it is spam, but there are plenty of more legitimate examples.)
If we had no limit on the number of recipients it could potentially
cause problems for the receiving mail server. The "Max recipients
per item" specifies a limit on the number of recipients. If there
are more than this many recipients, Weasel breaks up the recipients
into batches, by duplicating the message if necessary.

:p.The SMTP standards require that any mail server MUST be
able to handle at least 100 recipients, and that a sending server
SHOULD NOT where possible send a message with more than 100 recipients.
Strict adherence to this rule would suggest that the only reasonable
value for this parameter is 100, and this is indeed the default.
It appears, however, that some mail servers are now imposing
smaller limits, in violation of the standard but probably as an
anti-spam measure. To deal with such servers, it might be desirable
to reduce the limit from 100 to 30. Increasing the value above
100 would not be a good idea.

:p.Getting this value right is not as critical as it might seem.
Suppose that you have a message with 100 recipients, but the
receiving server will only accept up to 30 recipients. What will
happen in this case is that the message will be delivered to the
first 30 recipients, and that 70 recipients will be rejected.
Seeing this, Weasel will keep the message, now with only 70
recipients, on the retry list. On the next delivery attempt,
the message will go to the next 30 recipients, and so on. The
only negative outcome will be a time delay for some of the
recipients, but eventually they will all receive the message.

:p.:hp2.Time to keep trying outgoing mail:ehp2.

:p.Ideally, Weasel should send outgoing mail as soon as it arrives,
unless we are not yet online. In practice, most mail is delivered
immediately (or as soon as we go online), but some destinations are
unreachable for reasons like network failures, remote servers that
are not responding, and so on. This means that Weasel has to be
prepared to make multiple attempts to deliver the "hard to deliver" mail.

:p.The first few attempts are made fairly quickly, on the grounds
that an initial failure is likely to be caused by a transient error,
but the time between re-attempts is gradually increased, up to a
steady-state limit of five hours between attempts. The present option
allows you to say how long Weasel should keep trying before it
decides that the mail is undeliverable.

:p.The recommended time to keep trying is four to five days (96 to
120 hours). You may, however, specify a longer or shorter time
depending on your own local circumstances.

:p.If the mail is finally undeliverable, Weasel sends a message to
the original sender giving the reason why the delivery failed. In
addition, the original sender gets a warning message, after about
one quarter of the time specified here, to say that the mail has
not yet been delivered but that the server is still trying.

:p.:hp2.Identifying a local user:ehp2.

:p.If you have multidomain mode activated (via the
:link reftype=hd refid=pmconfiguser1.Basic:elink. page) you will also see a
section called "Identifying a POP3 user", with an option labelled
"Accept only the first username/domain match". This option is relevant
only when a POP or IMAP user logs in with a username that is valid in two
or more domains, and those domains have the same IP address. If the
option is enabled, only the first found domain is checked, and the
user is allowed to log in if and only if he or she supplies the
correct password for that first domain. If the option is not enabled,
the password is used to resolve the ambiguity&colon. all candidate domains are checked,
and we choose the first one for which both the username and password match.

:p.Whether you enable this option depends on how cautious you are.
I personally prefer to disable it, so that we can use the password
as a way of deciding which domain the user belongs to; but some
system managers see this as a security risk.

:p.:hp2.Number of threads handling outgoing mail:ehp2.

:p.The next option on this page is the number of threads that will
handle outgoing mail. It is desirable to have more than one thread
sending the mail out because
:ul.
:li.Some mail items can take a long time to send, either because
they are abnormally large or because
of factors like hard-to-reach destinations. With multiple threads,
the rest of your mail continues to go out normally rather than
being stuck in the queue behind the slow job.
:li.If you have a busy server, or if your outbound mail appears
in batches, the congestion is cleared more quickly if several
jobs can be handled in parallel.
:eul.

:p.In other words: although Weasel will work with only one thread
handling the outbound mail, it will work a lot faster with the
extra threads. The Setup program allows you to specify the number
of threads for outbound mail. The number has to be between 1 and 64.
If you try to go outside these limits, the value will be truncated
at the limit.

:p.Note that setting the value too high can be a bad decision.
Each thread consumes some system resources, and those resources
might better be used for other things. In addition, there are almost
always other factors that limit how much faster you can make the mail
go out. Most commonly the bottleneck is the network connection. Once
you are using all available network bandwidth, adding more threads
will only make the existing threads run more slowly. Besides, you
probably have applications other than Weasel that use the
network, so you don't want to make Weasel a 'greedy' application that
slows down everything else.

:p.For a typical ethernet connection, 16 threads seems to be a
reasonable compromise. If you have a slow network, and most
particularly if you have a dial-up connection, you should choose
a lower number. If you have fast hardware you might want to
experiment with higher numbers, but it is doubtful that you would
ever want to go as high as 64. Remember, these are not the only
threads that Weasel uses; it also spawns new threads to handle
incoming mail and POP3 and IMAP4 connections.

:note.If you do change the number of threads handling outbound mail,
the change might happen gradually, as the outbound threads work out
among themselves whether new threads should be started or existing
ones terminated. Naturally, a thread that is in the process of
handling a mail item will not terminate itself until that operation
is complete.

.***********************************
.*   THIRD PAGE OF OPTIONS
.***********************************

:h3 id=pmconfigoptions3 toc=1234.Options 3

:hp2.Options page 3:ehp2.

:p.:hp2.Bind server to specific address:ehp2.

:p.If your machine has multiple network interfaces, Weasel is able
to accept connections on any of the interfaces. In almost all cases this is
acceptable, and in fact you might even want to
:link reftype=hd refid=domainIP.allocate different addresses to different domains:elink..

:p.If you are also happy to use any interface for outgoing mail,
you should chose the "All local interfaces" option
in the "Bind server to" box.

:p.Sometimes, however, you might want to use only one of your network
addresses for outgoing mail. You might, for example, have a firewall
that only lets SMTP traffic through a single address. In that case you need to select the
"Specific address" option.

:p.Note that the OS/2 tcp/ip stack does not allow you to bind to
several but not all addresses. It is either one specific address,
or all.

:p.:hp2.Non-delivery notifications:ehp2.

:p.
Sometimes a mail item cannot be delivered. In such cases Weasel decides,
based on the reason for non-delivery, either to give up trying or to
make multiple attempts, possibly spanning several days, to deliver the
message. In the latter case, a message is sent back to the sender after
several attempts, to say that the server is still trying to deliver
the message. In either case, a message is sent back to the sender when
Weasel finally decides that the original message cannot be delivered.

:p.The sender needs to know :hp1.which:ehp1. message could not be
delivered, so Weasel has to include some or all of the original
message in its "bounce" message.

:p.If the checkbox on this Setup page is enabled, the entire original
message is sent back as an attachment. This provides a maximum amount
of information to the sender, but of course it is wasteful if the
original was a very long message.

:p.If the checkbox is not enabled, then you can choose how many bytes of
the original message are sent back. In this case the first part of the
message is sent in "plain text" form, not as an attachment. The count includes the header lines,
so you should make this count large enough to include part of the
message body as well. Apart from this consideration, the decision as to
how big to make this count is an arbitrary one.

:p.:hp2.Maximum incoming message size:ehp2.

:p.The standards document RFC 1870, an extension to the SMTP standard,
allows the server to put a limit on the size of a message that will be
accepted. You can specify this limit in bytes. The default is about 4
gigabytes, but you will probably want to specify a smaller size to
prevent unreasonable demands on your server.

:p.A mail client can optionally give a size estimate in the MAIL FROM command,
but most clients don't do this because they have already been told the limit as a
response to the EHLO command. Typically Weasel doesn't know about a size
violation until the message is being received. If it finds that the limit is
being exceeded then it continues to receive the message body (in order to
find the "end of data" marker) but does not store the rest of the message.
Then, of course, it deletes what it has already stored and gives an
error message to the client.

.***********************************
.*   RELAY PAGE
.***********************************

:h3 id=pmrelay.Relay host for outgoing mail

:hp2.Relay host for outgoing mail:ehp2.

:p.This page controls how you send your mail through a relay host,
i.e. another SMTP server that will take care of your outgoing mail.
If you have a dial-up connection you will probably need to relay
mail through your ISP (Internet Service Provider), either permanently
or as a backup solution, because many mail servers reject connections
from dial-up lines as part of their anti-junk policies. (Weasel can
also do this, via the
:link reftype=hd refid=pmconfiguser6.blacklist:elink. option.)

:p.:hp2.Note:ehp2.&colon. The blacklist servers that list dial-up lines
have a very liberal interpretation of what is meant by "dial-up". If your
ISP is one that offers accounts to the general public, it is very
likely that you are listed in one or more dial-up blacklists, even if
you have an ADSL or cable connection. I myself pay my ISP for a fixed
IP address, which should take me out of the "dial-up" category, but some
mail servers still refuse to accept mail directly from me. Thus, I need
to nominate my ISP's mail server as the backup relay host.

:p.If you have a permanent internet connection you do not, strictly
speaking, need a relay host, so you can specify "never" as the first
option on this page, and then the rest of the page becomes irrelevant.
Even then, however, it is a good idea to use a backup server, if you
have access to one, to relieve your own server of the load caused by
hard-to-deliver mail.

:p.:hp2.Note:ehp2.&colon. This option is provided for the case where, for example, you have
to send all your mail through a gateway, or via your ISP's mail server. If you use it, make sure that you
have permission to use the relay host this way. If you abuse a relay
facility, you might end up discovering that you have been blacklisted and
can no longer send mail to anyone.

:p.To use a relay host, you need to specify the following options.

:dl break=all.
:dt.     Use relay host
:dd.If you specify "never" here, a relay host will not be used, and then
the remaining entries on this page become irrelevant. If you specify
"always", the nominated relay host will be used to relay all mail.
The "as backup" option (which is the best choice in most situations) is
a compromise&colon. Weasel attempts to send the mail directly, but if
this fails then the mail is sent to the relay host.
:dt.     Hostname
:dd.This option lets you specify
the hostname of another computer that is running an SMTP server and
that will accept relay mail. Outgoing mail from Weasel will be sent to
that computer, which should accept responsibility for forwarding it on to
its final destination.
:p.If you need to relay through a nonstandard port, put a colon (&colon.) at the
end of the host name, followed by the port number. For example, the name
:xmp.          smtp.example.com&colon.5001

:exmp.
specifies that relay mail should go to port 5001 on host smtp.example.com.
:dt.     Relay everything
:dd.This option causes Weasel to bypass the check to see whether incoming
mail is for a local user; all incoming mail, regardless of its address,
is relayed on to the relay host. You should normally :hp2.not:ehp2. enable
this option, because it prevents you from having any local POP or IMAP users.
The option is intended for the case where you are using Weasel as a
:link reftype=hd refid=frontend.front end:elink. for another mail server.
:dt.     Authentication for outgoing mail
:dd.What you need to specify here depends on the policies of the
server you are using as a relay host. (If you are not using a relay host,
you can specify "none" as the authentication mechanism and skip the rest
of this page.) Some servers do not require any authentication, because
they use your IP address as a confirmation that you are one of their
customers, and in that case you can specify "none" as the authentication
mechanism. Otherwise, you need to specify "SMTP AUTH" or "POP before SMTP",
depending on the policies of the relay host. (These authentication mechanisms are explained on the
:link reftype=hd refid=smtpauth.SMTP authentication:elink. page.)
If you specify either of these,
you need to fill in some further fields&colon.
:dl break=all.
:dt.Username
:dd.Normally this is the username of a mail account on the relay server,
but the system manager of that server might tell you to use something different.
:dt.Password
:dd.The password for the above account.
:dt.POP host
:dd.This is needed only if you are using POP-before-SMTP authentication.
You have to specify the name (or IP address) of the POP server that
you have to log into before sending mail.
:p.If the POP server uses a nonstandard port, put a colon (&colon.) at the
end of the host name, followed by the port number. For example, the name
:xmp.          example.com&colon.5002

:exmp.
specifies that the POP server is running on port 5002 of host example.com.
:edl.

:p.Note that authentication (for outbound mail) is used only when using a
relay host. (Unconditional authentication would be a bad idea, because then
you would be providing a username and password to servers that do not need them.)
If you have chosen the "as backup" option, authentication is not used for the
initial attempt to deliver the mail directly. It is used only if we have to
fall back to the backup server.

:p.The authentication options specified on this page apply to outbound mail
(via a relay) only. Authentication for incoming mail is an entirely separate
topic, and this is covered on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink.
page.

:edl.

.***********************************
.*   THE WHITELIST
.***********************************

:h3 id=pmconfigwhitelist.Whitelist
:hp2.Whitelist:ehp2.

:p.
This notebook page lists the hosts that are allowed to bypass many
of the anti-spam checks for incoming mail, because you know that
they are "safe" sources of mail.

:p.Of course, spammers can and do fake their identities in the HELO/EHLO
commands and the MAIL FROM command, so we can't use those for
whitelisting.

:p.The one thing that they cannot fake is the IP address
that the connection is coming from. Thus, all of our whitelisting
decisions are based on the numeric addresses derived from the whitelist
on this page. You may certainly use textual hostnames in this
list; but, for the purposes of checking, Weasel converts those names
into IP addresses by consulting a nameserver.

:p.This does mean, unfortunately, that you cannot use wildcards in
the textual names, because there is no efficient way to convert those
into IP addresses via a nameserver. You may, of course, use the
mechanisms that Setup provides for specifying a range of addresses.

:note.Putting a host into this list means that you are confident that
spam is not likely to come from that address. It does not, however,
give that client permission to send relay mail. For relay mail
permission you must use the
:link reftype=hd refid=pmconfiguser4.Trusted:elink.
page.

:p.In the present version, the whitelist is used only to skip the
tests that are made when processing the MAIL FROM command for
incoming mail. Specifically,
:ul.
:li.We skip checking whether the FROM address is banned or blacklisted;
:li.We skip checking whether the domain in that address has a postmaster account.
:eul.
:p.Whitelisting also causes the "banned" and "blacklisted" checks to be
skipped on the initial connection, but that is more for efficiency than
anything else, because it is highly unlikely that you would have the host
simultaneously whitelisted and blacklisted.

:p.The whitelist does :hp2.not:ehp2. give exemption from filtering, if
you have defined any filters.

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=pmhostlist.Editing a list of host names:elink..

.***********************************
.*   SOURCES FOR RELAY MAIL
.***********************************

:h3 id=pmconfiguser4.Trusted
:hp2.Trusted:ehp2.

:p.
This notebook page lists the hosts that are "trusted", in the sense that
they are allowed to send relay mail via Weasel.

:p.Relay mail is mail that the SMTP server accepts and agrees to
pass on to another host. The Weasel philosophy is that unlimited
relay mail should not be permitted. It puts an extra load on your
computer, and it helps junk mailers to hide the real origin of
their junk mail.

:p.There are just three cases where it can make sense to permit
your machine to be used as a relay host.
:ul.
:li.Where your computer is acting as a gateway to a network
(which contains other mail servers) that
would otherwise be unable to receive mail from the outside world,
or where it is acting as a backup server for another server.
This "gateway" function is described on the following page.
:li.Where your machine is acting as the "outgoing mail" server
for a group of local machines. This function is the subject
of the current page.
:li.Where you want to offer an "outgoing mail" service to the users
who have accounts on your POP/IMAP server. This function is described on the page about
:link reftype=hd refid=smtpauth.SMTP authentication:elink..
:eul.
:p.The "Trusted" list is a list of host names. Mail sent
from those hosts, and addressed to some third host, will be accepted
to be forwarded. Mail sent from elsewhere will not be accepted
unless it is addressed to the machine on which Weasel is running, or
is coming from a host which has been authenticated using POP-before-SMTP
authentication or SMTP AUTH authentication, or is addressed to a domain on the "GateFor" list.
In other words, Weasel will not accept mail for relaying except
when it comes from a trusted host, or when it is
going to an approved destination.

:note.The only default trusted address is the loopback address
[127.0.0.1], which must remain enabled because Weasel sometimes needs
to send mail back to itself. Apart from this special case all relay mail, even mail from your
own computer, will be rejected if it does not satisfy one of the
above conditions.

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=pmhostlist.Editing a list of host names:elink..

.***********************************
.*   ACCEPTABLE RELAY DESTINATIONS
.***********************************

:h3 id=pmconfiguser4a.GateFor
:hp2.GateFor:ehp2.

:p.
This page lists the acceptable destinations for relay mail&colon. the
addresses for which Weasel will accept relay mail even from
non-trusted hosts.

:p.Relay mail is mail that the SMTP server accepts and agrees to
pass on to another host. The Weasel philosophy is that unlimited
relay mail should not be permitted. It puts an extra load on your
computer, and it helps junk mailers to hide the real origin of
their junk mail.

:p.:hp3.IN THE MAJORITY OF WEASEL INSTALLATIONS, THE GateFor LIST SHOULD
BE EMPTY:ehp3.. Relaying on behalf of your own users is already taken
care of by the "Trusted" list and/or the SMTP authentication.
You do not need to do anything further for those users. Putting
entries into the "GateFor" list creates the risk of making your
server an open relay.

:p.There are just two cases where you need to have entries in
this list.
:ul.
:li.Where your computer is acting as a gateway to a network that
would otherwise be unable to receive mail from the outside world.
Typically this happens when you have a firewall to protect a
private network, and you are running Weasel to pass mail across the
firewall. If this is your situation, then you can list the
names and/or IP addresses of the other mail servers in the private network
in the "GateFor" list.
:li.Where your server is acting as a backup server for another
SMTP server. That is, your machine is listed (with your permission)
in one of the nameserver MX records for another domain. In that
case you can put addresses in that domain into your "GateFor" list.
:eul.
:p.The "acceptable destinations" list is a list of domain names. Mail
addressed to those domains will be accepted
to be forwarded, whether or not it comes from one of the "trusted hosts".

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=pmhostlist.Editing a list of host names:elink..

.***********************************
.*   BANNED HOSTS
.***********************************

:h3 id=pmconfiguser5.Banned
:hp2.Banned:ehp2.

:p.This page contains your personal blacklist of hosts that are not
allowed to send mail to (or via) your server. Weasel will refuse to accept any mail
from hosts on this list.

:p.Instructions for creating or modifying this list are given on
the manual page called :link reftype=hd refid=pmhostlist.Editing a list of host names:elink..

.***********************************
.*   REGISTRATION PAGE
.***********************************

:h3 id=pmregister.Register

:hp2.Register:ehp2.

:p.As of version 2.0 of Weasel, the Registration page no longer exists.
If you see this page, you are running an obsolete version of Setup.

.***********************************
.*   EDITING A HOST LIST
.***********************************

:h3 id=pmhostlist.Editing a list of host names
:hp2.Editing a list of host names:ehp2.

:p.Several of the Setup notebook pages have a list of host names. On
those pages you have the following options:

:dl tsize=15.
:dt.   Add
:dd.Add a new entry. Once you have typed the new entry, use the
Enter key to complete the operation.
:dt.   Promote
:dd.Move the current entry up in the list. You can use
this for changing the order of the entries in the list.
:dt.   Delete
:dd.Deletes the current entry.
:edl.

:p.:hp2.The format of a list entry:ehp2.

:p.Entries in the list can be in one of several forms:

:ul.
:li.An IP address optionally enclosed in square brackets, for example [123.45.6.78].
If wildcards are used they must come at the end and must refer to an
entire 8-bit number that is one of the four components of the address.
For example, [123.45.*.*] is legal, but [123.45.*.6] is not legal.
In addition, forms like [123.4*.*.*] are not legal.
:li.An IP address range, for example [123.45.6.78-99]. This means all
addresses in the range [123.45.6.78] to [123.45.6.99], inclusive. You
may only have a single number after the '-' character. As a more
complicated example, [1.2.3-8] means all addresses in the range from
[1.2.3.0] up to [1.2.8.255].

:li.An IP address range in the format CIDR [a.b.c.d/N], where again the square
brackets are optional.  Here N (a number in the range 1 to 32) specifies the
number of high-order bits that remain constant while the remaining bits take
on all possible values.  For example, CIDR [123.45.6.0/23] refers to the
range from [123.45.6.0] to [123.45.7.255], inclusive. This format is a common
way of referring to a subnet of 2^(32-N) addresses. In the present version the
leading string "CIDR" is compulsory, but this requirement will be dropped once
the following option has been phased out.

:li.An IP address range in the format a.b.c.d/N, where N (a number in
the range 1 to 32) specifies how many of the least significant bits
are "don't care" bits. For example, [123.45.254.0/9] refers to the
range from [123.45.254.0] to [123.45.255.255], inclusive. This option is
supported for those who used it in earlier versions of Weasel, but
will be phased out in time because of the potential for confusion
with the CIDR form. If Setup finds such an entry, it will change it to
CIDR format.

:li.A hostname, for example alpha.beta.com. In this case unrestricted
use of wildcards is possible - see below.
:li.A domain name starting with the '.' character. This is a "wildcard"
entry that will match any hostname that ends with that domain name.
This form is supported for compatibility with older versions of Weasel,
but it is being phased out now that the '*' form of wildcard is
available.
:eul.

:p.Where appropriate - that is, when ranges or wildcard characters are not used -
Weasel will query the local nameserver, as it
is reading in the list, to find out whether the specified machine has
alternative names or multiple IP addresses. This means that you don't
normally need to specify aliases when setting up the list of names.

:p.:hp2.Wildcards:ehp2.

:p.When you are specifying a host name or domain name string, you may
include the wildcard characters '?' and '*'. When comparing names, the
character '?' matches any single character. The wildcard character '*'
matches a substring of any length, including a string of zero length.

:p.For example, the string x*.com* would match things like xyz.com,
or x.y.com, and it would also match things like xxx.com.au.

:p.Note that
:ul.
:li.Alphabetic case is not significant in host and domain names; 'Abc' is
considered to be the same as 'abc'.
:li.For the purposes of wildcard matching, punctuation marks like '.' do
not have any special status; they are treated like any other character.
:li.There are restrictions on how you may use wildcards when you specify
a numeric IP address, as explained above.
:eul.

.***********************************
.*   BLACKLISTS
.***********************************

:h3 id=pmconfiguser6.Blacklists
:hp2.Blacklists:ehp2.
:p.
The "banned hosts" facility of Weasel should be a good way of
controlling junk mail, but it has a serious shortcoming&colon.
because the junk mailers know that everyone is trying to block their
mail, they keep changing their addresses.

:p.In response to this, some organisations now maintain "blacklist"
databases that attempt to track the sources of junk mail, and/or the
open relays that are allowing the junk mail to propagate. These
are further explained on the page about
:link reftype=hd refid=realtimeblacklist.realtime blacklist databases:elink..

:p.Earlier versions of Weasel included hard-coded links to the
blacklist sites, but those addresses have now become obsolete.
The MAPS site changed its addresses when it changed from being a free
site to being a subscription site, and the ORBS database of open relays
was shut down as the result of legal action. We will probably see
further changes as the war between the spammers and their victims evolves.
To allow for the current and possible future changes, you now have
to specify the domain names of the blacklist checkers.

:p.You are allowed to use up to eight such sites. To activate the
checking, enter the domain name of the blacklist checker, and select
the checkbox next to that name. If a name is specified but the
checkbox is not checked, that site is not used, but its name is left
there in case you want to activate it at some later stage. The names
that initially appear are simply suggestions; you can of course change them.

:p.Note that many of the blacklist sites are subscription sites,
which means that they won't work for you unless you are a subscriber.

.*************************************************************************
.*   MULTIDOMAIN MODE
.***********************************

:h1 id=multidomainmode.Multidomain Mode

:hp2.Multidomain Mode:ehp2.

:p.This section covers some extra details you need to know about the multidomain mode.

:ul.
:li.:link reftype=hd refid=turningmultion.Turning on the multidomain mode:elink.
:li.:link reftype=hd refid=originaldomain.The original domain:elink.
:li.:link reftype=hd refid=turningmultioff.Turning off the multidomain mode:elink.
:li.:link reftype=hd refid=domainIP.Associating domains with IP addresses:elink.
:li.:link reftype=hd refid=identifyusers.Matching users with domains:elink.
:eul.

.***********************************
.*   TURNING ON MULTIDOMAIN MODE
.***********************************

:h2 id=turningmultion.Turning on the multidomain mode

:hp2.Turning on the multidomain mode:ehp2.

:p.The "Basic" page of the Setup notebook includes a
checkbox labelled "Enable multiple domain mode". Once you check
that checkbox, several things happen.

:ul.
:li.The "Users", "Aliases", and "Local" pages disappear from the
Setup notebook.
:li.Any existing information from those three pages is transferred
to what we call :link reftype=hd refid=originaldomain.the original
domain:elink..
:li.An extra option appears on the "Options 2" notebook page.
This will be explained when we discuss the topic of
:link reftype=hd refid=identifyusers.matching usernames with domains:elink..
:li.A new notebook page called "Domains" appears. If this is the
first time you have activated this feature, it will contain one entry
with a fictional domain name. (You should change this to a real domain name.)
If you have previously had multiple domain mode
enabled then it might list several domains.
:eul.

:p.These changes are reversible. See also the topic of
:link reftype=hd refid=turningmultioff.Turning off the multidomain mode:elink..

.***********************************
.*   THE ORIGINAL DOMAIN
.***********************************

:h3 id=originaldomain.The original domain

:hp2.The original domain:ehp2.

:p.When you first switch from single-domain mode to multidomain
mode, a single domain with a fictional name is created, and all of your
existing users are migrated to that domain.

:p.One of the first things you will want to do, almost certainly,
is to change that name to the name of a domain that
belongs to you. When you do that, the Setup program remembers the
name change, and the renamed domain is now considered to be the
original domain. As you add domains, the domain that was first
created continues to be called the original domain.

:p.The :hp1.only:ehp1. significance of the original domain is that
it will be the one used if you ever switch back to single-domain
mode. In all other respects, it is the same as any other domain.
If you never switch back to single-domain mode, then the concept
of the original domain has no importance.

.***********************************
.*   TURNING OFF MULTIDOMAIN MODE
.***********************************

:h3 id=turningmultioff.Turning off the multidomain mode

:hp2.Turning off the multidomain mode:ehp2.

:p.Once you have changed to multidomain mode, there is no great
real motivation for going back to single-domain mode. Even if you
are hosting only one domain, you might as well stay in multidomain
mode permanently. Still, you might have activated multidomain mode
to see what happens, and decided that it is not for you.

:p.There are two ways to revert to single-domain mode. You can go to the "Basic" page of the
Setup notebook, and uncheck the box labelled "Enable multiple domain mode".
Alternatively, there is an option on the "Domains" page that lets
you switch back.

:p.When you do this, the information in the original domain, including
the list of users, is moved back to the single-domain configuration.
If you have any other domains defined at that point, the users in those
domains are not migrated back. The information for those domains is
in subdirectories of the Mailroot directory, and if you want to migrate
any of that information you will have to do it manually.

:p.When you switch off the multidomain mode, you might get a warning
message that the original domain could not be fully deleted. There
are two likely reasons for this&colon.

:ul.
:li.Deleting a domain requires, among other things, deleting a file
called DOMAIN.INI. This deletion can fail because of the way that OS/2
keeps INI files cached in main memory. You can safely delete that
file manually.
:li.User directories will not be deleted if they still contain some
undelivered mail. If a user directory has not been deleted, you need
to check the files in that directory and make a decision about what
to do about the unfetched mail. Normally the most sensible decision is
to move the mail to that user's new directory.
:eul.

.***********************************
.*   ASSOCIATING DOMAINS WITH IP ADDRESSES
.***********************************

:h2 id=domainIP.Associating domains with IP addresses

:hp2.Associating domains with IP addresses:ehp2.

:p.A mail domain has three important attributes&colon.

:ul.
:li.The name of the domain, for example coyote.acme.net
:li.The alternative names that will also be accepted as destination
addresses for this domain. In particular, this includes the hostname
for the machine on which the mail server is running, for example
mail1.coyote.acme.net
:li.The numeric IP addresses belonging to the hostnames mentioned above.
:eul.

:p.The associations among these are defined when you
:link reftype=hd refid=editdomain.edit the domain properties:elink..
The name of the domain is given on the "Domains" page of the Setup
notebook, and the other names and IP addresses are given on the
"Local" page of the properties for that domain.

:p.Ideally the hostnames and IP addresses should not overlap.
That is, two domains should not be associated with the same hostname
or the same IP address. If you are using a single machine to host
multiple mail domains, however, some compromises are usually necessary.

:p.The most desirable situation is where your server has multiple
network interfaces, and therefore multiple IP addresses, and you
use a separate IP address for each mail domain that you are hosting.
In this case, the domains are separated from one another both
conceptually and physically. Weasel does not need to search through
the list of domains to identify a user, because the IP address
uniquely identifies a domain.

:p.More typically, you will have more domains than IP addresses.
For example, you might be hosting six domains but have only three
IP addresses. In that case, you have to decide how to partition
the address space. The most obvious decision is to allocate two
domains per IP address. On the other hand, you might decide that
you have two important customers and four less important ones; in
that case, you would probably give each important customer a
unique IP address, and let the other four share a single IP address.

:p.The extreme (but very common) case is where you have only one
IP address. In that case, you probably have no choice but to force
all users to specify their domain when logging in, by using a
POP3 login name of the form user@domain. Weasel does have a feature
where they can simply log in as 'user', with the password used to
work out which domain they belong to, but some system managers
will want to disable this feature on the grounds that it weakens
security.

.***********************************
.*   MATCHING USERS WITH DOMAINS
.***********************************

:h2 id=identifyusers.Matching users with domains

:hp2.Matching users with domains:ehp2.

:p.When you are hosting multiple domains, there is a possibility that
a username occurs in two or more domains. (In fact, this is certain,
because the username 'postmaster' exists in every domain.) Thus, we
need a strategy for deciding which domain this user belongs to.

:p.There are three cases to consider&colon. incoming mail, outgoing
mail, and mail being fetched by a user. These three cases are
described below. Of these, the third case is the most complicated.

:p.:hp2.Incoming mail:ehp2.

:p.Incoming mail has a "To" address of the form user@domain. That is,
the domain is specified explicitly. Weasel matches the domain by
checking whether 'domain' is the name of one of our domains, and
if not whether it is in the "Local" list for any domain. Normally this
should uniquely identify the domain.

:p.There are just two cases where an ambiguity might arise&colon.
:ul.
:li.If a name occurs in the "Local" list for more than one domain, the
first match is chosen. This is, in effect, a configuration error, and
you should not allow it to happen.
:li.If the domain name is unspecified or is an empty string, Weasel
will check only the first domain in the domain list. If there is
a username match for that domain, the mail is accepted. Otherwise,
it is rejected with a 'no such user' error.
:eul.

:p.:hp2.Outgoing mail:ehp2.

:p.Outbound mail is normally mail that has come in from elsewhere
- for example, from one of your users - and is being relayed to
another server. Technically, the relay function is handled by a
'host' rather than by a 'mail domain'. In this case, therefore, the
question of 'what domain does this mail item belong to?' does not
arise.

:p.:hp2.Mail being fetched by a user:ehp2.

:p.The two preceding cases are handled by the SMTP component of
Weasel. A 'fetch mail' operation by a user uses the POP3
or IMAP4 protocol rather than the SMTP protocol. The details of how we
match a user with a domain is a little more complicated.
This case is more fully described on the
:link reftype=hd refid=identifyPOPuser.following page:elink..

.***********************************
.*   IDENTIFYING A MAIL USER
.***********************************

:h3 id=identifyPOPuser.Identifying a mail user

:hp2.Identifying a mail user:ehp2.

:p.When a POP3 user or IMAP user logs in, he or she supplies a username and
a password. (In the IMAP case, or if the APOP or AUTH command is used, there is an additional
encryption operation on the password, but this does not change
the fact that just two pieces of information have been supplied.)
If your server has multiple interfaces then Weasel is also
able to find out which IP address the client connected to. It
does not, however, know what domain is being addressed, because
there is nothing in the POP3 or IMAP4 protocols that supplies that information.

:p.There are basically two ways to resolve this problem&colon.

:ol.
:li.Require the user to log in with a username of the form
user@domain. That is, the login name includes a specification of
which domain that user is logging into.
:li.Allow the user to log in with a username in the usual way,
without specifying a domain, and let Weasel work out which domain
to use from the available clues&colon. the IP address that the
user has used to contact the POP or IMAP server, and the user's password.
:eol.

:p.It is up to you, as system manager, to decide which of these
two methods will be used. Method 1 gives an unambiguous
specification of the domain (provided that you haven't duplicated
a domain name), but is less convenient for the user. Method 2
gives the user the impression that the POP or IMAP server is entirely
dedicated to his or her domain, but it does create the risk that
the user will log into the wrong domain if the same username
occurs in more than one of the domains that your mail server
is hosting.

:p.The two methods are not mutually exclusive. You could, for example,
allow most users to log in using method 2, but require the
user@domain form of login for those users and/or domains for which
there is a risk of ambiguity.

:note.The presence of firewalls and proxies can, in some cases,
make it impossible for users to include the @ character in their
login name. To get around this, Weasel allows alternative
forms user'domain and user%domain. If the single quote (') character
or the percent character (%) is found, Weasel does not search
further for an @ character.

:p.The precise sequence of steps used by Weasel to deduce
the domain is described below.

:p.:hp2.The identification process:ehp2.

:p.When a user logs in, by any of the several methods provided by
POP3 or IMAP4, Weasel performs the following checks.

:ul.
:li.The login must have occurred with the user connecting to
a specific address. If your server has only one IP address then
this provides no useful information. If, however, it has several
IP addresses then this restricts the set of possible domains.
Only the domains that have this IP address (either explicitly, as
a numeric address, or implicitly, as the result of nameserver
lookup) on their "Local" Setup page are considered in the remainder of this
checking process.
:li.If the user has supplied an explicit domain name, by logging
in with the user@domain form of username, this further
restricts the set of possible domains. Normally this would mean
that there is only one domain that is a possible candidate.
:li.From the domains remaining after these checks, we keep the ones
for which there is a username match. Often, but not always,
there will be only one domain left to consider at this stage.
:eul.

:p.What happens next depends on whether you have specified
"Accept only the first user/domain match"
on the "Options 2" page of the Setup notebook. (This option is
visible only when you have activated multiple domain mode.)
If you have checked this option, and the search described above
has found multiple domains, only the first of these is used.
The login is successful if and only if we get a username and
password match for that domain.

:p.If, on the other hand, you leave that option unchecked, the
password is used to resolve any ambiguities. We select the
first domain for which the username matches :hp2.and:ehp2.
the password is correct. The probability that the same
username will occur in more than one domain is fairly high.
The probability that the same password is used for more
than one of those instances is extremely low. Thus, using
the password to resolve ambiguities will almost certainly
pick the correct domain. In the remotely possible case that
it gets the wrong domain, this will be detected the first
time that the user tries to log in, and the problem can be
fixed by changing the password for one of the affected users.

:p.The strategy of using passwords to resolve ambiguities does
slightly increase the chance that someone can break into
your system by guessing usernames and passwords.
Most people would consider the risk still to be acceptably
low. If you consider that risk to be unacceptable, you should
enable the "Accept only the first user/domain match" option.

.*************************************************************************
.*   REMOTE CONFIGURATION
.***********************************

:h1 id=remoteconfig.Remote configuration

:p.Setup also offers the option of remote setup. That
is, you can run Setup on one computer and use it to configure a copy
of Weasel that is installed on a different computer. To do this, you have
to have the freeware utility INIServe running on the same computer as
Weasel. You can find INIServe at http&colon.&slash.&slash.www.pmoylan.org&slash.pages/os2.

:p.If you select the "Remote" radio button after starting Setup, a "Setup"
pushbutton is enabled. Clicking on this gives you four fields to fill in&colon.

:dl break=all.
:dt.     Hostname
:dd.The name (or IP address) of the machine on which Weasel is running.
:dt.     INIServe port
:dd.The TCP port that INIServe has been configured to listen on. The default
value is 3560. Earlier versions of Weasel defaulted to port 5000 for this
value, but INIServe now has an official IANA-registered port of 3560, so
this should be preferred for all INIServe installations.
:dt.     INIServe password
:dd.The password needed to log in to your copy of INIServe.
:dt.     Weasel directory
:dd.The full path name of the directory, on the remote machine, where Weasel
is installed.
:edl.

:p.When you close the Setup window, you can click on the "GO" button to connect
to the remote machine. If this gives a "failed to connect" or similar error
message, it probably means that you don't have INIServe running on the
remote machine, or that you have done something like specifying an incorrect
port number.

:p.Once the connection is made, the operation is the same as for the
case of local configuration.

.*************************************************************************
.*   REALTIME BLACKLIST DATABASES
.***********************************

:h1 id=realtimeblacklist.Realtime Blacklist databases

:hp2.Realtime blacklist databases:ehp2.

:p.A blacklist database is a list of hosts that are known to be 'undesirable'
in some way. Usually this means that they are sources or potential
sources of junk mail. Because junk mailers move quickly from one site
to another, these databases have to be updated frequently to be of
any use.

:p.These databases are set up to accept queries from mail servers such
as Weasel. The time overhead is similar to the overhead of looking up
a hostname on a nameserver; that is, from seconds to a couple of minutes,
depending on whether the entry you want has recently been cached.
Note, however, that some of the databases will work for you only if you
have a subscription to them.

:p.There are three common kinds of realtime blacklists.

:ul.
:li.Lists of IP addresses for internet nodes that are known to be
sources of spam, or places that are allowing spam to pass through.
This is the most direct form of blacklist, because all of the hosts
on the list are known to have been used in recent cases of
spamming.

:li.Lists of IP addresses that have been identified as addresses
allocated to dial-up lines. The reason for doing this test is that
junk mailers often operate from dial-up connections so that they are
harder to identify. (Meanwhile, the legitimate dial-up users are
probably not sending mail directly; they are more likely to be sending
it via their ISP.) If you block dial-up users
you might need to give an exemption to your own dial-up users; the way
to do this is to include your own dial-up addresses in the
:link reftype=hd refid=pmconfiguser4.Trusted:elink. list.

:li.Lists of open relays. An open relay is a mail server that allows
mail to be relayed through
it from arbitrary sources. Some SMTP relays are open because they are
spammer-friendly, and are helping the junk mailers to distribute their
junk. Others are open because their operators have not taken enough
precautions. Many are open because the system manager has not
realised that they are open.

:p.There is probably no direct threat to you from open relays. The
theory behind banning them is not that they are directly harmful, but
that they are indirectly harmful in that they can become (perhaps
unknowingly) accomplices in network abuse. If you ban mail from them,
this is likely to cause enough complaints that their operators will
eventually plug the security hole. This sort of incentive has
already caused a great many owners of SMTP servers to upgrade their
security. If we can cut down the number of open relays there are in
the world, we'll make life more difficult for those who rely on
theft-of-service attacks.

:p.Note that you yourself could end up in the "open relay" databases if
you fail to configure Weasel properly. You should be careful to
restrict the relaying options, because spammers manage to find new
open relays surprisingly quickly. :hp3.Note in particular that the great
majority of Weasel installations should have an empty 'GateFor' list.
There are very few cases where you legitimately need to have entries
in that list:ehp3..
:eul.

:p.:hp2.Where to find the blacklists:ehp2.

:p.At the time of writing this, I was aware of the following realtime blacklist
sites.

:ul.
:li.The MAPS site at http&colon.//mail-abuse.org/
:li.The ORBZ list of open relays at http&colon.//orbz.gst-group.co.uk/
:li.The Open Relay Database at http&colon.//www.ordb.org/
:li.There appears to be a useful database of spammers maintained by
http&colon.//www.spews.org/, but I haven't yet figured out whether this
is available to us as a realtime blacklist.
:eul.

:p.You can find a longer list at http&colon.//www.sdsc.edu/~jeff/spam/Blacklists_Compared.html.
I have no official opinion about the quality of any of these blacklists,
because I haven't put in the time to do evaluations. You'll have to
look at the available evidence and make your own decision. A web search
on topics like 'blacklists' will probably turn up a lot more information
than I have given here.

:p.:hp2.Should I enable these checks?:ehp2.

:p.It is up to you to decide whether you want to use these services. If you
do, you can tell Weasel to use it by enabling one or more of the realtime
databases in the Setup program.
There is a limit on how many of these you may specify, because an excessive
number of checks will slow down the
incoming mail too much. If you enable any of these, Weasel will do the
checks on each incoming SMTP connection attempt. If the SMTP client is
found on the blacklist, the connection will be refused.

:p.Before making this decision, you should consult the web pages
(see above) of the various blacklist providers, to see what they offer
and also to see whether a subscription is required.

:p.Disadvantages of using the blacklists include the following points.
:ul.
:li.Enabling the checks means extra nameserver lookups for each
mail item, which slows down the reception slightly. If you are getting
rapid nameserver responses then this doesn't matter much. If you have
an unreliable nameserver, or very slow network connections, it could
be a problem for you.
:li.If you are doing these checks then you are doing them on behalf of all
your users. You should inform your users that you have implemented
anti-spam measures, that this will cause some incoming mail to be
rejected, and that acceptance of this condition is part of the conditions
of use of your mail system.
:li.Inevitably some genuine senders will be locked out as well as the
spammers. This happens, for example, when a spammer-friendly ISP also
has some legitimate customers. The rejection message from Weasel tells
senders why they have been locked out, and the blacklist maintainers
have mechanisms for
removing sites from the blacklists when the senders can show that they are
not a spamming site. Nevertheless, some false alarms will always be
a feature of any anti-spam measure.
:eul.

:p.The advantages of using the blacklists are obvious: you cut down on the
amount of spam you receive, and you are helping to rid the world of
theft-of-service attacks. For most people the advantages outweigh the
disadvantages, and this is why Weasel allows you to use these features.

.***********************************
.*        SMTP AUTHENTICATION
.***********************************

:h1 id=smtpauth.SMTP authentication
:hp2.SMTP authentication:ehp2.
:p.For security reasons, Weasel allows relay mail only in some special
situations. The special cases are based on source and destination
host names and IP addresses.

:p.Many system managers would like to give relay privilege to their
own clients, namely those who have a POP account. In some cases this
can be done by basing the relay privilege on the client's address.
In others, where for example a client can log in from many different
addresses, this does not work. To allow for this case, Weasel allows for
two authentication mechanisms&colon.
:ul compact.
:li.:link reftype=hd refid=SMTPAUTHcommand.SMTP AUTH:elink..
:li.:link reftype=hd refid=popbeforesmtp.POP before SMTP:elink..
:eul.

:p.:hp2.Incoming SMTP:ehp2.

:p.If incoming mail is addressed to a local user, we do not require any
authentication. If, however, the mail is to be relayed, we require that
it be
:ul.
:li.coming from a trusted host, as specified on the
:link reftype=hd refid=pmconfiguser4.Trusted:elink. page, or
:li.addressed to a special destination, as specified on the
:link reftype=hd refid=pmconfiguser4a.GateFor:elink. page, or
:li.coming from an authenticated user, as specified on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink.
page. The available authentication methods are explained on the
next two pages of this manual.
:eul.

:p.:hp2.Outgoing SMTP:ehp2.

:p.The authentication rules for outgoing SMTP are set by the
manager of the receiving mail server. In most cases you will find that
there are no restrictions if the mail recipient is local to that server,
but that there are restrictions for non-local mail. If you are relaying
through another mail server, you need to check the rules for that server,
and then make the appropriate changes to the
:link reftype=hd refid=pmrelay.Relay host page:elink..

.***********************************
.*        SMTP AUTHENTICATION
.***********************************

:h2 id=SMTPAUTHcommand.The SMTP AUTH command
:hp2.The SMTP AUTH command:ehp2.

:p.The original SMTP standard has no provision for passwords or
anything similar. There is, however, a mechanism called ESMTP
(extended SMTP) that permits a mail server to tell clients which
extensions to the standard it accepts. One of those extensions
is called the AUTH command.

:p.In effect, the AUTH command is a way for a client to supply
a username and password as part of a mail transaction. As a
practical matter, that means that you can only use it when
sending mail to (or via) a server on which you have a mail
account. The most common application of the AUTH command is
to allow end users to use their ISP's mail server as a
relay host.

:p.There are several variants of AUTH, providing different
levels of security. For outgoing mail through a relay host,
Weasel works out which of these variants it has in common
with the other server, and then uses the most secure available
variant. For incoming mail, you specify which of the variants
you are willing to accept on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink.
page.

:p.At present, Weasel supports three varieties of AUTH for
both incoming and outgoing mail.

:dl break=all.
:dt.     PLAIN
:dd.This is the simplest case, where the username and password
are supplied in what amounts to plain text. (They are actually
base64 encoded, but anyone who intercepted the communication
would find it elementary to reverse the encoding.) It is not
a highly secure method, but it might be the only method
available to some clients.
:dt.     LOGIN
:dd.This variant is not, as far as I know, part of any mail
standard, but it needs to be supported because apparently some
mail software insists on using it. It is, in effect, the
same as PLAIN but with a less efficient way of transferring the
information.
:dt.     CRAM-MD5
:dd.This is a much-improved method, because it encrypts the
information. Furthermore, the encryption depends on a timestamp
that changes every second, so it would do an attacker no good
to copy the information and try to use it later. Unfortunately
it is not as widely implemented as the PLAIN and LOGIN methods.
:edl.

.***********************************
.*        POP BEFORE SMTP
.***********************************

:h2 id=popbeforesmtp.POP before SMTP authentication
:hp2.POP before SMTP authentication:ehp2.

:p.POP-before-SMTP authentication is less convenient than
using the SMTP AUTH command, but it has the advantage that it is
likely to work even for clients who have primitive mail
software. The only requirement is that the users have a
POP mail account on your server.

:p.If you enable this authentication method, it works as follows.
When a POP user logs in to fetch mail, Weasel takes note of the
IP address that the login came from. For a short time after that,
relay mail from that address will be permitted. This means that
the user is able to send outgoing mail after fetching incoming
mail. The authorisation expires after a few minutes, but it can
be renewed by another POP login. If, for example, the user's mail
program is checking for new mail every three minutes, and you have
set the authentication to expire after five minutes, the
authentication will remain "alive" for as long as the user is
running the mail program.

:p.In addition, the authorisation remains valid if the client
does a number of SMTP operations in succession, provided that
the time gap between those operations does not exceed the
expiry time. This means, for example, that only one POP login
is needed before sending out a batch of queued mail.

:p.You can enable this feature, and set the expiry time, on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink.
page of Setup.

:p.You should be aware of one potential security hole. If a
user is fetching mail via one of the free webmail services, you
might be giving relay authority to a junk mailer who just happens
to be using the same service and therefore the same IP address.
Keeping the expiry time short will keep this risk small, but
there is always some risk.

:note.If your users are able to use the SMTP AUTH command,
they will not need to use POP-before-SMTP authentication.
At the time of writing this, a large number
of mail clients are able to use the AUTH command, and the
number appears to be growing.

:p.There has also been a rapid growth in the number of service
providers who insist that their customers use the AUTH method.
If you have such a provider, and your mail software does not yet
support SMTP AUTH, you will have to install a mail server (such
as Weasel), or a proxy, that can relay your outbound mail while
supplying the necessary authentication.

.***********************************
.*   RUNNING THE SERVER
.***********************************

:h1 id=RunningWeasel.Running Weasel
:hp2.Running Weasel:ehp2.
:p.
The server executable is called WEASEL.EXE. You can run this the way you run
any other OS/2 program: from the command-line, by clicking on an icon, from
the Startup folder, etc. If you are running several server applications, then
the most obvious choice is to put a command to start the server in the
command file \TCPIP\BIN\TCPSTART.CMD, or in \TCPIP\BIN\TCPEXIT.CMD. The
file TCPEXIT.CMD is invoked by TCPSTART.CMD just before it completes. It
is probably the most sensible choice; the TCP/IP configuration
notebook is badly written, and has the bad habit of disabling any
non-IBM servers you have installed, but (at least in the current
version) it does not delete what you have put in TCPEXIT.CMD.
:p.
You also have the options of :link reftype=hd refid=inetd.running the server from inetd:elink.
or :link reftype=hd refid=detached.running the server as a detached program:elink..

:p.Note that, before running Weasel, you must set the working directory to
the directory that contains WEASEL.INI. (Or WEASEL.TNI, if you use the 't'
command-line option.) If you are starting Weasel from a Program Object - which is
the case when, for example, you start it from the Startup folder - then the
working directory is specified in the Program Object. If you are starting it
from a command file, you might need explicit "change directory" commands to
set the working directory. If this is inconvenient, you can use the 'f'
command-line option, as described below.

:p.There are three optional command-line parameters. If you start Weasel with
the command
:xmp.

           weasel -t
:exmp.
:p.this directs Weasel to obtain its configuration data from a text file
called WEASEL.TNI, rather than from the traditional binary file WEASEL.INI.
The binary format is more efficient, but only marginally, and some
people prefer to avoid INI files because of problems that occur when the
operating system is running low on shared memory.

:p.If you start Weasel with the command
:xmp.

           weasel -i
:exmp.
:p.this directs Weasel to obtain its configuration data from WEASEL.INI
rather than WEASEL.TNI. This parameter is redundant in the present
version, because WEASEL.INI is the default anyway; but it allows for
a possible future change in the default.

:p.If you start Weasel with the command
:xmp.

           weasel -f
:exmp.
:p.this will force the working directory to be the same as the directory
that contains WEASEL.EXE.

:p.The command-line arguments may be in either upper case, and they may be
combined. For example, both of the commands
:xmp.
           weasel -f -t
           weasel -TF
:exmp.are legal. The result is however undefined if you try to specify
both of the "t" and "i" options.

:p.
To shut down the program, type Ctrl/C. (That is, hold down the Ctrl key while
typing 'C'.) There might be a delay before the program terminates, because
Weasel will allow any transactions in progress to complete before shutting down.

:p.If you type Ctrl/C more than once, Weasel shuts down even if there are
POP clients who have not yet logged out. Even in this case there can still
be a delay if the program is in the middle of a slow operation. If you
really need to shut down Weasel without a delay, use a process killer (e.g.
the one that comes with the WarpCenter); but if you do this there is a risk
of lost mail because the program has not tidied up properly.

:p.The 'G' and 'Q' keyboard commands, which were used for shutdown in older
versions of Weasel, are no longer supported. (Keyboard support was removed
because it was conflicting with the spawning of user-written filters.)

:p.External software can shut down Weasel by signalling on the global event
semaphore \SEM32\WEASEL\SHUTDOWN. This has the same effect as typing Ctrl/C
on the keyboard. Posting the semaphore twice has the same effect as typing
Ctrl/C twice.

:p.If that external software needs to confirm that shutdown has completed,
it can wait on the event semaphore \SEM32\WEASEL\FINISHED.

:p.If you are running Weasel from a Rexx *.cmd file, you can check the value
of a variable called RC to check whether Weasel has shut down normally.
Its value is zero for a normal shutdown, and nonzero if Weasel has crashed.
(It shouldn't, but every fully debugged program has at least one more bug.)

:p.

:h2 id=inetd.Running from inetd
:hp2.Running the server from inetd:ehp2.

:note.The inetd option makes sense only in the case of very low traffic
levels. Most users will find it better to run Weasel directly, rather
than using inetd to start it.

:p.
Inetd, which is part of the Warp 4 distribution, is a "listener"
program that can intercept incoming connection attempts, and
start up a server when needed.
:p.
The advantage is that Weasel doesn't actually get loaded into
main memory until a client wants to connect. Thus, it might be
a good option if you expect clients to connect only occasionally.
:p.
The disadvantage is that a separate copy of the server is
started for each logged-in user. This makes inetd a bad choice
if you expect lots of connections.
:p.
If you want to run Weasel from inetd, the way to do it is
as follows:
:ol.
:li.Ensure that inetd will be run the next time you boot.
The usual way of doing this is to include the line
.br
           start /min inetd
.br
in your TCPSTART.CMD, and to invoke TCPSTART.CMD from
your startup folder. TCPSTART.CMD may be found in the
directory \tcpip\bin.

:li.Edit the file \mptn\etc\inetd.lst so that it contains one
or more of the lines
.br
     pop3 tcp start /C /min d&colon.\Apps2\Weasel\weasel.exe
.br
     smtp tcp start /C /min d&colon.\Apps2\Weasel\weasel.exe
.br
     imap tcp start /C /min d&colon.\Apps2\Weasel\imapd.exe
.br
(adjusting the path so that it refers to the directory
where you have installed Weasel).
:eol.

:p.In principle you can now start inetd. In practice I have found
that inetd doesn't release ports reliably, so if you already
have inetd running you will probably have to re-boot.
:p.
Remark: I'm starting to suspect that inetd adds more overhead
than it saves, so I have reverted to not using it on my own
machine.

:h2 id=detached.Running Weasel detached
:hp2.Running the server as a detached program:ehp2.

:p.If you want to run the server detached, the appropriate command is
.br
       DETACH WEASEL.EXE
.br
(You will probably want to put this command into your STARTUP.CMD file.)
The difference between doing this and simply running WEASEL.EXE is
that a detached program runs without a screen window being created.
That is, the server does its job behind the scenes without having
any visible presence on the desktop. (This mode of operation is
probably the reason, historically, why a server program is
commonly called a 'daemon'.)
Note that a detached program
does not have any way of doing screen output or keyboard input,
so you can't get any screen messages in this case.

:p.The only time that you need screen output from this program is
when you are testing it. Once you have decided to use it permanently,
it's probably a good idea to leave it detached, so that it doesn't
get in the way of the things you really want on your screen. Indeed,
for serious professional use it makes sense to run all your network
server software on a machine that doesn't have a keyboard or screen.
Removing the user interface removes a huge load in terms of
processor power and main memory usage, letting you use the
hardware more effectively. Meanwhile, you can still use the
:link reftype=hd refid=remoteconfig.Remote configuration:elink.
option of Weasel to control it from a different machine.

:p.Without any screen window, you obviously cannot use Ctrl/C to
shut down the program. You can, however, order it to terminate by
signalling on a global event semaphore. This possibility is explained in the
section on
:link reftype=hd refid=RunningWeasel.running Weasel:elink..

.*************************************************
.*        RUNNING AS A FRONT END TO ANOTHER SERVER
.*************************************************

:h2 id=frontend.Running as a 'front end' server
:hp2.Running as a 'front end' server:ehp2.

:p.There are some situations where you do not want to run Weasel
as an 'post office' server, but instead use it as a sort of
buffer that sits in front of an organisation's main mail server.
This can free the main server from overheads such as authentication,
virus checking, junk mail filtering, and the like. It might seem
strange to run two mail servers in tandem, but the need for such
an arrangement appears to be growing. Because of market forces, or
possibly because of a perception that OS/2 and Unix are obsolete,
it has become common for organisations to replace high-performance
servers by so-called upgrades that simply can't handle the load.
Often the cheapest solution to this problem is to put an efficient
mail server in front of the inefficient one.

:p.The :link reftype=hd refid=pmrelay.relay everything:elink.
option of Weasel is designed for this situation. This option forces
Weasel to pass on all mail (except, of course, the mail that
fails the various checks on incoming mail) to another host,
even if it looks as if it is mail for a local user.

:p.For the sake of illustration, let us suppose that the mail
domain is deadcat.org, that its main mail server is running on
the machine called mail1.deadcat.org, and that you are installing
Weasel on another machine called os2.deadcat.org. To make this
configuration work, the first thing you must do is to ensure
that all mail to this domain is actually sent to the os2.deadcat.org
machine. That is, you must insert an MX record into the nameserver
that says that os2.deadcat.org is the primary mail server for
the domain deadcat.org. There is probably already an MX record
identifying mail1.deadcat.org as the mail server for this domain.
You should either delete this record, or give it a lower priority.

:p.Next, you must configure Weasel, via the Setup notebook, as
follows. The list below assumes you are running in single-domain
mode. Multidomain mode would also work, but in this mode of operation
there is not much point in enabling that feature.

:dl break=all.
:dt.    Basic
:dd.Set this up in the usual way, except that the POP section should
be disabled. You cannot have local users in this mode, therefore there
is no point in allowing POP logins. You may activate multidomain mode
if you wish, but for this sort of application single-domain operation
is just as good.
:dt.    Logging
:dd.Set this up in any way you wish, depending on what sort of logging you want.
:dt.    Filters
:dd.Set this up in the usual way, specifying any filters that you want
to run on incoming mail.
:dt.    Options 1
:dd.As usual, you decide here what sort of authentication you want
to insist on. If you want to implement authentication then it should be
done on the Weasel machine rather than on the ultimate server, because
this is the point at which mail enters your system.
:dt.    Options 2
:dd.Set this up in any way you wish, but you will probably want to
choose the 'online always' option, because it's unlikely that you would
want to use this mode of operation with a dial-up connection. Since
all outgoing mail will be going to the main mail server, the number of
outbound threads might have to be limited to the traffic level that
that other server can handle.
:dt.    Relay
:dd.The critical settings are on this page. Choose 'always' as the relay
host option, and specify mail1.deadcat.org as the relay host. Obviously
you should check the 'Relay everything' option, since that is the whole
point of this exercise. The authentication options will depend on what
is required by the main server.
:dt.    Users and Aliases
:dd.Leave these pages empty. In this mode of operation, there are no local
users and no local aliases.
:dt.    Local
:dd.Entries on this page will be ignored, so you might as well leave the list empty.
:dt.    Trusted
:dd.This page should normally be left empty, because in this mode of
operation you want to define legality in terms of 'legal destinations'
rather than 'legal sources'.
:dt.    GateFor
:dd.Here is where you put the entry deadcat.org. You can put several
entries here if the destination machine supports several domains.
:dt.    Banned and Blacklists
:dd.Set these up in any way you wish.
:edl.

.*************************************************
.*        DIAL-UP OPERATION
.*************************************************

:h2.Operating with a dial-up connection
:hp2.Operating with a dial-up connection:ehp2.

:p.A mail server normally needs a permanent network connection, so
that it can accept incoming mail. If, however, you can arrange for
another machine to act as an MX relay for you then it might be
feasible to run Weasel with only an intermittent network connection.
In such a situation we would like to be able to specify that
outbound mail be stored for later delivery.

:p.Periodically - usually three or four times per minute
- Weasel checks whether it is online. There are several ways to
do this, depending on the option you chose in the 'Options 2' page
of the Setup notebook. For a dial-up connection, the best choice
is usually to ask Weasel to do its own check for when the dial-up
interface is active.

:p.If you want manual control, you could instead choose the 'Options 2'
option that makes Weasel check for the existence of a file called 'ONLINE'
(without the quote marks) in its own directory. The content of this file is
not important, only the question of whether the file exists. If it exists,
Weasel assumes that there is an on-line connection and it will attempt to
deliver waiting mail. If the file does not exist, Weasel queues the outbound
mail for later delivery.

:p.With this choice, therefore, you should create the file ONLINE
when you have established a connection, and delete that file (or rename it, or move it to
another directory) when you disconnect from the network. You can do this
manually, or - depending on what sort of dialler software you are using - you
can make it a part of your dial-up script.

:h2.Setting the time zone
:hp2.Setting the time zone:ehp2.

:p.Incoming mail has a "Received:" line added as the first line of
the header. (If the mail went through one or more relay hosts, there
will be several "Received:" lines. If you are trying to trace the origin
of junk mail, this is the first place to look.) The last thing that
Weasel puts on that line is a time zone indicator, for example +1000.
Many OS/2 installations, however, don't have their time zone set.
If you find that the "Received:" header line has a date and time, but
no time zone, you need to set the time zone on your machine.

:p.One way to do this is with my (free) TZSet utility. You can find this
at http&colon.&slash.&slash.www.pmoylan.org&slash.pages/os2.

:p.With some (but not all) releases of eComStation you don't need TZSet, because
the eClock program that comes with eComStation will look after
setting the time zone information. The way to check this is to look at
the first "Received:" header line of any message received by Weasel,
to see whether a time zone is specified.

.***********************************
.*   USING FILTERS
.***********************************

:h1 id=filters.Using filters
:hp2.Using filters:ehp2.

:p.You have the option in the Weasel setup of specifying
pre-delivery filters that can be invoked at any (or all) of several
stages during the reception of incoming mail.
:dl break=all.
:dt.    :hp2.The stage 0 filter:ehp2.
:dd.This filter, if present, is executed as soon as an incoming
SMTP connection is established. At this stage, the only
information we have is the IP address of the other machine, and
its hostname as determined by reverse DNS lookup.
:dt.    :hp2.The stage 1 filter:ehp2.
:dd.This filter, if present, is executed after the sender has
given the HELO or EHLO command, which specifies the (real or faked)
hostname of the sending machine.
:dt.    :hp2.The stage 2 filter:ehp2.
:dd.This filter, if present, is executed after the sender has
supplied a MAIL FROM command, which specifies the (real or faked)
e-mail address of the sender.
:dt.    :hp2.The stage 3 filter:ehp2.
:dd.This filter, if present, is executed after the sender has
specified the 'From' and 'To' addresses, and just before the
sender sends us the actual message. Note that at this stage the
sender has already sent the DATA command - this is the only way we
have of knowing that there are no more 'To' addresses still to
come - but we have not yet replied to that command, therefore the
actual data transfer has not yet commenced.
:dt.    :hp2.The stage 4 filter:ehp2.
:dd.This filter, if present, is executed after
the Weasel SMTP server has received a mail item (header + body), but before
it has put it into a local mailbox or forwarded it.

:p.At this point the message is in a temporary message file, in
the standard e-mail format: the header lines, one blank line, and
then the message body. The first header line is the "Return-path:"
line, and this is followed by one or more "Received:" lines.
Header lines after this can occur in any order, depending on
the sender.
:edl.

:p.Filters can be used for virus scanning, for detecting and
rejecting junk mail, for redirecting messages, and a variety of
other applications. At each stage the filter returns a reply
code to say whether processing should continue normally, or
whether the sender should be told via an error reply that the
mail is being rejected. The choice of the stage at which the
filter should be called will normally be obvious to the
designer of the filter, depending mostly on whether the filter
needs to look at the content of the message. In the most
complex cases, you might want to call five different filters,
one for each stage.

:p.In all cases, of course, a filter is called only if a filter
name has been specified (in the Setup program) for that stage.
If the filter name is blank, filtering for that stage is skipped.

:p.A stage 4 filter can examine the message file, optionally
copy it or change its contents, and then return a reply code to
Weasel saying whether or not the message should be delivered.
It can also alter the list of recipients.
Similar rules apply to the earlier stages, except that in those
cases there is no message file.

:p.The filters can be written as REXX or Perl scripts, or alternatively they
can be written in any programming language you like and
compiled and linked into EXE format.

:p.To give you the widest choice of programming languages, the filter
is not called directly, but is instead invoked by starting a command
shell. That is, Weasel does the equivalent of executing
:xmp.

         CMD.EXE /C filterprog namefile messagefile

:exmp.
where "filterprog" is the name of the CMD or EXE file to be
executed, "messagefile" is the full pathname of the
temporary message file, and "namefile" is the full pathname of a
text file containing information about the sender and the
recipients. (The line terminator is carriage return followed by line feed.)
The format of that information is&colon.

:dl break=fit.
:dt.    line 1
:dd.the client's IP address and hostname, in the form
:xmp.
   [nnn.nnn.nnn.nnn] hostname
:exmp.
with a single space between the numeric name and the textual hostname.
If the reverse DNS operation fails to find a hostname, the hostname part
is just a repeat of the numeric name.
:dt.    line 2
:dd.the sending hostname, as specified in the HELO or EHLO command.
:dt.    line 3
:dd.the sender's e-mail address, as specified in the MAIL FROM command.
:dt.    line 4
:dd.an empty line.
:dt.    remaining lines&colon.
:dd.the e-mail addresses of the recipients, one per line.
:edl.

:p.
This is for a stage 3 or stage 4 filter. At stage 0 we have
nothing except line 1, because the remaining information has
not yet been received. At stage 1 we have lines 1 and 2, but
nothing else.  At stage 2 we have lines 1 to 3. It is only in
the later stages that we have the full information.

:p.You should be aware that line 1 is the :hp1.only:ehp1. line whose
information is completely reliable. The 'sender' information in
lines 2 and 3 can usually be trusted for genuine e-mail, but it is
almost invariably faked by junk mailers. There is no way for the
sender to fake the IP address as shown in line 1, but even that is
not always helpful; the address tells us which mail server is sending
us this mail, but it does not tell us the original sender, because
the mail might have been routed through one or more spammer-friendly
relays.

:p.
The reason for an empty line 4 is to allow for changes in future
versions.  If we change the format of the leading lines in some
future version of Weasel, we can still
ensure that the list of recipients starts after the first blank line,
making it easier to update filters if the rules change.

:p.As an additional piece of information, the :hp2.name:ehp2. of the
namefile is constructed in such a way that the part after the '.'
in the file name is the SMTP sesssion ID. The session ID is the number
that comes after the 'S' flag in the transaction log. This is done in
case you want to correlate the information in the transaction log with information
produced by the filter. If you don't need this information, then of
course you can ignore the structure of the file name.

:p.The filter should return one of the following values.

:dl.
:dt.    0
:dd.handle message normally, i.e. continue processing it as if
there had been no filter. This reply code is valid at any stage.
:dt.    1
:dd.reconstruct the list of recipients from the namefile,
because the filter has altered the namefile; and then
continue handling the message normally. The sender address is
also updated from line 3 of the namefile. (Lines 1 and 2 are ignored.)
This reply code makes sense only at stages 2, 3, and 4.
:dt.    2
:dd.don't deliver the message (we presume that the filter
has already taken care of the delivery, if desired), and return the reply
:xmp.           250 OK
:exmp.
to the client. This reply code makes sense only at stage 4.
:dt.    3
:dd.don't deliver the message, and return the default rejection
message to the client. At stage 0, 1, and 2 the default rejection message is
:xmp.           421 Spammers not welcome here
:exmp.
At stages 3 and 4 the default rejection message is
:xmp.           554 Mail rejected by filter
:exmp.
The reason why these are different is for maximal compatibility
with the SMTP standard. The standard does not say what the text
of the rejection message should be, but it does specify which of
the three-digit reply codes is valid at each stage.

:dt.    4
:dd.don't deliver the message, and return a reply which is taken
from the first line of the namefile. That is, the filter should in
this case have overwritten the first line of the namefile with the
message, starting with a three-digit code, that it wishes to send
back to the sender. (The remaining information in the namefile can be
corrupted by this operation; this does not matter, because we have
no further need of that information in the case of a rejected message.)
:dt.    5-15
:dd.unused codes, reserved for future use.
:dt.    16
:dd.like 0, but in addition all further filtering steps are skipped
for this mail item. (Unless the sender cheats by, for example,
changing the sender address after having already specified a sender
address.) This option is for the case where you already have enough
information to know that the mail should be accepted, so don't need
the overhead of running the later-stage filters.
:edl.

:p.NOTE: In all cases, the temporary message file is deleted
after the filter has seen it and Weasel has copied it into the
user's mailboxes. If the filter wants to take care
of delivery (case 2) then it must make a copy of the message file.

:p.Remark: some junk mail software is designed to send the mail
even if the receiving server sends a rejection reply. (This is in
violation of the standard, but junk mailers are notorious for
ignoring standards.) As a protection against such antisocial
behaviour, Weasel will return a "421 closing connection" reply,
and then break the connection, if a "reject" decision is returned
by any filter.

:p.Another possible cause of malfunction, which does not occur with
standards-conforming mail software but which does sometimes occur
with junk mail senders, is a situation where the mail client sends
a message body at a time when the server is expecting a command.
If this happens, the mail will be rejected but
the Weasel transaction log (if enabled) will contain error messages
for further "commands" that do not make sense. Weasel will break
the connection once it has found three faulty commands in
succession.

:p.If the filter exits with an error, or returns a result code
that is not one of the valid result codes, Weasel acts as if the result had
been 0, i.e. the filter has no effect. This is to guard against
errors in the filters.

:p.There is a collection of filters that various people have
written at the web page
:xmp.    http&colon.&slash.&slash.www.pmoylan.org/pages/os2/wfilters.html

:exmp.
If you have filters of your own that you think might be of general
interest, I would be happy to add them to that listing.

:p.If you want to run multiple filters, get the script Multifilter.cmd
from that collection. This is a sequencing filter whose only function is
to call a list of other filters.

:p.:hp2.SPECIAL NOTE:ehp2.. A three-stage model of filtering was
introduced in Weasel version 1.643, and one result of this change
was to change the order of parameters passed to the filter.
The format of the namefile information was further revised in version
1.645 (which altered the namefile format) and version 1.646 (which
introduced the five-stage model.) An extra line was inserted into
the namefile in version 1.786.
If you have a filter that was designed for an earlier version of
Weasel, you will have to modify the way it handles its parameters.
Fortunately, this is a very minor change in terms of programming.

:p.It is possible to specify an alternative stage 4 filter for specific
local destination addresses. This includes the possibility of bypassing
the filter for those addresses. For details, see the
:link reftype=hd res=1003.Editing a user record:elink.
page.

:p.It is worth noting that the 'stage' of a filter can be deduced
from the parameters. For stages 0 to 2 the messagefile name is
an empty string. Stage 0 is the only stage where the 'namefile'
file is only one line long, and so on. Because
of this, it is perfectly possible to use the same filter at all
five stages, with the filter program working out for itself what
stage it is up to. In terms of efficiency, however, it is probably
better to have five separate filter programs.

:p.This is assuming that you really need to filter at all five
stages. A more typical situation would be where only one filter is
used, and the filter name is blank for the other stages.

.***********************************
.*   TOOLS AND ACCESSORIES
.***********************************

:h1 id=wtools.Tools and accessories
:hp2.Tools and accessories:ehp2.

:p.On the web page
:xmp.

      http&colon.&slash.&slash.www.pmoylan.org/pages/os2/waccess.html

:exmp.
you can find some extra software that is useful in conjunction
with Weasel. Some of this was written specifically for Weasel, and
some will work with any mail server.

.***********************************
.*   DUMPINI AND LOADINI
.***********************************

:h2 id=dumploadini.Backing up the INI file data
:hp2.Backing up the INI file data:ehp2.

:p.The configuration information for Weasel, i.e. the options that
you have specified by running the Setup program, is kept in a
file called WEASEL.INI. For efficiency, this is a binary file that
is not human-readable. As an alternative, Weasel also supports the
use of a human-readable configuration file called WEASEL.TNI.
To convert between those two formats, you need two extra
utilities called DumpINI and LoadINI.
:ul.
:li.DumpINI.exe reads the INI file data and produces a human-readable
text file, which you can then move to a backup directory, edit with
a text editor, etc.
:li.LoadINI.exe takes the text file that DumpINI created - possibly
with modifications that you have made with a text editor - and loads
that data back into the INI file.
:eul.

:p.Earlier versions of Weasel included these two utilities in
the 'tools' subdirectory, but those versions are now obsolete, and
should be deleted if you have them left over from an earlier
distribution. Newer versions of DumpINI and LoadINI are now contained
in a separate freeware package called GenINI. You can fetch GenINI
from the same place where you got Weasel. The home site for GenINI is
http&colon.//www.pmoylan.org/pages/os2/genini.html.

:p.If you want to use GenINI only with Weasel, you can safely unzip
the GenINI package into the Weasel directory. However, since GenINI
is now "generic" and not tied specifically to Weasel, you might want
to put it in its own directory.

LoadINI and DumpINI are no longer specialised
to one application, but can be used for conversion or backup of any OS/2
INI files.

:p.:hp2.Backing up in the multidomain mode:ehp2.

:p.If you are running Weasel in multidomain mode, the INI file information
is spread across several files. The 'global' options are kept in
WEASEL.INI, as in the single-domain case. Data specific to one domain,
such as the user data, are kept in a file called DOMAIN.INI.

:p.To handle this case, the Tools directory includes two Rexx scripts
called mloadini.cmd and mdumpini.cmd. As you will see from reading the
scripts, mloadini.cmd firsts invokes LoadINI to translate WEASEL.TNI
to WEASEL.INI, and then it continues the translation for all of the
DOMAIN.TNI files. (And similarly for mdumpini.cmd.) This will save
you the trouble of manually invoking LoadINI or DumpINI for each
of your domains.

:p.If you want only a partial backup or restoration you can use DumpINI
directly on one of the DOMAIN.INI files, and LoadINI directly on one of
the DOMAIN.TNI files. For example, you can execute the command
:xmp.

       ..\..\dumpini domain.ini

:exmp.
to produce a text file called DOMAIN.TNI. This example assumes that
the domain.ini file is in a subdirectory two levels down from the
directory that contains DUMPINI.EXE, which is a common scenario.
The directories might be different in your case, because different
system managers have different ideas about the best place to locate
these files. One common scenario, for example, is to have one
directory entirely devoted to holding backup scripts and backup data
for a whole variety of software. In any case, it should be obvious
how to adjust the paths to make the above example work in your
preferred directory layout.

.***********************************
.*   SCAVENGE
.***********************************

:h2 id=scavenge.The SCAVENGE utility
:hp2.The SCAVENGE utility:ehp2.

:p.The Scavenge utility is relevant only if you are upgrading from
Weasel version 0.83 or earlier. It converts unsent mail from an old
format to a newer format. Because this is such an old version,
the utility is no longer distributed with Weasel, and support for it
will soon be dropped completely. If you need it, look
at the Weasel tools at http&colon.//www.pmoylan.org.

.***********************************
.*   USING A DIFFERENT SMTP SERVER
.***********************************

:h1 id=otherSMTP.Using a different SMTP server

:p.It is possible to run the Weasel POP server without running the
Weasel SMTP server. (Or vice versa.) To do this, use the Setup
program and uncheck the "enabled" checkbox in the SMTP section
on the first page.
The change will take effect the next time you start Weasel.

:p.You have to make sure, of course, that the other SMTP server
you are running will store mail in a place where Weasel can find it.
The details depend on which SMTP server you are using. See

:ul compact.
:li.:link reftype=hd refid=SMTPsendmail.Using IBM's sendmail:elink.
:li.:link reftype=hd refid=SMTPother.Using a third-party SMTP server:elink.
:eul.

:p.Note that these options are included only to handle some very
unusual cases. In the majority of applications there would be no
point in replacing Weasel by a less powerful server. In fact, from
the feedback I have received it appears that nobody ever uses this
option, so it might be removed from a future version of Weasel.

:p.Note, too, that the file ENDMAIL.EXE that is included in the 'tools'
subdirectory is needed :hp1.only:ehp1.  to allow sendmail to transfer
mail to the Weasel mail directories. Since most Weasel users will not be
using sendmail, ENDMAIL.EXE will never be needed in the majority of
installations.

:h2 id=SMTPsendmail.Using IBM's sendmail
:hp2.Using IBM's sendmail:ehp2.

:p.The instructions for using IBM's sendmail in conjunction with
Weasel may be found at the web site http&colon.//www.pmoylan.org.
This is for version 2.02 of sendmail. In principle we could provide
support for later versions of sendmail, but in practice there does
not seem to be any demand for this. Since Weasel includes full SMTP
support, you are unlikely ever to want to use sendmail.

:h2 id=SMTPother.Using a third-party SMTP server

:p.The Weasel POP server can work in conjunction with any SMTP server that
satisfies the following conditions.

:ul.
:li.A user's mail is stored in a directory whose name has the form mailroot\user,
where "mailroot" is a fixed directory and "user" matches the user's e-mail
address. If Weasel is running in multidomain mode, the directory is
mailroot\domain\user.
:li.Each e-mail message is stored as a single "plain text" file, in the
standard e-mail format (header, blank line, body); and the file name is
of the form "something.MSG".
:eul.

:p.Of course, there is no guarantee that any given SMTP server will satisfy
these conditions; but it is often easy to write a little conversion
program that will copy the files across. (If there's enough demand,
I might be able to write the conversion programs. I won't do it for something that's
used only by two or three people, but I'll certainly consider it if
some particular program turns out to be widely used.)


.***********************************
.*   TROUBLESHOOTING
.***********************************

:h1.Troubleshooting

:p.:hp2.If Weasel crashes:ehp2.

:p.If the program crashes, see the
:link reftype=hd refid=reporting.reporting errors:elink. section.

:p.:hp2.Setup windows going off-screen:ehp2.

:p.Occasionally you might find that you have a window that has moved
so far that you can't reach the title bar to drag it back. This can
happen, for example, when a new release of Weasel uses a window that
is larger than in the version you were using.

:p.The easiest way to move a window that is partially off the screen is to
click on it and then type Alt/F7 (i.e. the F7 function key while
holding down Alt). You can then move the window with either the cursor
keys or the mouse. To finish the move, release the mouse or type the
Esc key.

:p.If this doesn't work, use the "resetpos" utility that you can
find at the Weasel
:link reftype=hd refid=wtools.tools and accessories:elink. web page.
This resets all window positions back to the bottom left of the screen.
After that, you can run Setup and move the windows to where you really
want them to be.

:p.When a window goes completely off-screen, the simplest solution is to
install one of the several free utilities that give you multiple screen
windows. I use PC/2 for this, mostly because I like the PC/2 feature of
being able to put my most-used applications on a one-click menu. Other
programs that have this feature are XWorkplace, 9Lives, and a few
others.

:p.:hp2.Unlocking a locked mailbox:ehp2.

:p.Sometimes a POP client will get a "mailbox is locked" message
while trying to fetch mail. If this happens, go to the user's mail
directory and delete the file LOCK.!!!

:p.Normally Weasel deletes this file when a POP session finishes.
The lock file might, however, be left undeleted if your computer
crashes, or if you shut down Weasel while a POP session is in progress.
Although Weasel checks for locked mailboxes on startup, it might
fail to clear the lock in unusual circumstances - e.g. if the
lock file attributes somehow got set to be read-only.

:p.One very common reason for a locked mailbox is that a dial-up
user was in the middle of fetching mail when the network connection
was lost. In this case the user has basically three recovery options&colon.
:ul.
:li.Use telnet or ftp to delete the LOCK.!!! file.
:li.If the user doesn't know how to do this, or does not have sufficient
privileges, call the system manager
and ask for the LOCK.!!! file to be deleted.
:li.If all else fails, wait for the timeout period (usually 15 minutes)
to expire before fetching mail again.
:eul.

:p.:hp2.Slow startup?:ehp2.

:p.If you are getting a long time delay while Weasel is starting,
it's probably caused by Weasel having to wait for responses from
your local nameserver. There are two likely causes for a slow
response:
:ul.
:li.The nameserver is not responding, because of a network problem
or because the nameserver has crashed. If this is the problem then
you'll simply have to accept the delay, because the problem is on
some other computer.
:li.One of the host names in your master lists (local hosts, acceptable
relay hosts, banned hosts) does not exist. In this case the nameserver
responds slowly because it must, in effect, search the entire world
for the nonexistent name. The solution for this is to run Setup and
remove the useless name from the list, or make sure that it is present
only as a numeric IP address rather than a hostname.
:eul.

:p.:hp2.Outgoing mail is rejected?:ehp2.

:p.If outgoing mail is accumulating in the "forward" directory without
being sent out, the likely problem is that Weasel is not discovering
that it is online. If it does not know that it is online,
Weasel won't try to send out the mail. See the
:link reftype=hd refid=pmconfigoptions2.Options 2:elink.
Setup page for ways to control when we go online.

:p.If Weasel is rejecting mail with a message "Relaying not available" or
"User not local,
please try ...", it means that the sender is not authorised to send relay
mail. To authorise it, you have to include the sending host in the
list of "Acceptable sources for relay mail". (By default, nobody - not
even the local host - is trusted.) Alternatively, enable SMTP
authentication as explained on the
:link reftype=hd refid=pmconfigoptions1.Options 1:elink. page.

:p.:hp2.Your ISP is refusing to accept mail from Weasel:ehp2.

:p.When Weasel has accepted a mail for forwarding, and you are using a relay host
which normally accepts mail from your system, there are some situations where
the relay host will refuse the mail. Some common reasons are:
:ol.
:li.Your provider uses "SMTP after POP" to make sure you are really who you are.
In this case you must do a "fetch" before sending out any mail. One way to do
this is to run PopGet.Cmd directly after dial-up on your account in the system
of this provider.
:li.Your provider insists on SMTP AUTH authentication. If this is your
problem, re-read the page about
:link reftype=hd refid=pmrelay.using a relay host:elink..
:li.Your provider will not relay unless it is to or from a local user.
All you can do in this case is to use only the mail address given to you
by this provider. If this is unsatisfactory, then the only solution is to
change providers.
:eol.

:p.:hp2.Weasel cannot send mail through a dial-up or ADSL connection:ehp2.

:p.Many e-mail systems have a way of
detecting that you are using a dial-up line, and will refuse to accept mail
on such a connection. (This is an anti-junk strategy; some junk mailers
use dial-up into the big mail organisations in order to hide their identity.)
You should be able to solve this problem by specifying your ISP's mail
system as a backup relay for outgoing mail. Note that some DSL and cable
addresses will also be detected as 'dial-up' by the blacklisting software.

:p.:hp2.Stopping PMMail from crashing:ehp2.

:p.There is something that older versions of PMMail don't like about Weasel's
response to the TOP command, and I haven't figured out what it is.
If you find that PMMail crashes when trying to fetch mail, open the
PMMail Account Settings, go to the Receive page, and enable the
"Quick Interrogation" option.

:p.The problem appears to have been fixed in the latest version of PMMail.

:p.:hp2.Long delays when Weasel is on a LAN:ehp2.

:p.Weasel expects to be able to do nameserver lookups as a normal part
of its operation, but it is becoming increasingly common to run small LANs
that do not include any nameserver. This will lead to delays caused by
waiting for the nameserver requests to time out.

:p.The way around this problem is to ensure that local names can always
be resolved. This requires the following steps.
:ul.
:li.In your CONFIG.SYS, make sure that the lines
:xmp.        SET USE_HOSTS_FIRST=1
        SET HOSTNAME=MyMachine

:exmp.
are included, where 'MyMachine' is the name you have given to the local
machine.
:li.Edit the file \MPTN\ETC\HOSTS so that it contains lines like the
following.
:xmp.       127.0.0.1             localhost
       192.168.1.1           MyMachine

:exmp.
This gives the relationship between name and IP address for all addresses
that you want to be able to reach without using a nameserver.
:eul.

:p.An even better solution would be to run a nameserver inside the LAN,
but not all system administrators know how to do this.

.***********************************
.*   DEVELOPMENT NOTES
.***********************************

:h1.Development notes

:ul.
:li.:link reftype=hd refid=tools.Development tools:elink.
:li.:link reftype=hd refid=whyM2.Why Modula-2?:elink.
:li.:link reftype=hd refid=bugs.Known bugs:elink.
:li.:link reftype=hd refid=reporting.Reporting errors:elink.
:eul.

:h2 id=tools.Development tools
:hp2.Development tools:ehp2.

:p.Some people have asked about the compiler I'm using. (I guess a
lot of people didn't realise that there were Modula-2 compilers
for OS/2.) It's XDS Modula-2, OS/2 native mode version.

You can find out about this, and other Modula-2 compilers for OS/2,
at the web page
.br
     http&colon.//www.pmoylan.org/pages/m2/Modula2.html
.br
Note, however, that a lot of this information is seriously out of date.

:p.The XDS compilers were marketed by Excelsior, see
.br
      http&colon.//www.excelsior-usa.com
.br
Excelsior has abandoned Modula-2, but has free versions of their compilers available for download.
Unfortunately the OS/2 version has still not been released.
:p.
Weasel uses some of the modules from the PMOS/2 library.
If you want to know more about PMOS/2, you'll also find that on
my web pages. Source code is available. My web pages are at
.br
     http&colon.//www.pmoylan.org
.br

:p.The Setup utility was built with the aid of a dialogue editor (available free
from Hobbes) called DrDialog.

:p.
This documentation was prepared with IBM's IPFC help compiler.

:h2 id=whyM2.Why Modula-2?
:hp2.Why Modula-2?:ehp2.
:p.
I'm often asked why I chose to code my software in Modula-2. Everyone
else seems to be using C or C++, so why don't I? (Only the raw beginners
ask why I don't use Java.)
:p.
The short answer is that I don't think much of the "everyone else uses it"
argument. If popularity was more important to me than technical merit,
I wouldn't be using eCS.
:p.
The long answer is contained in a document called "The Case Against C",
which can be found at
ftp&colon.//pmoylan.org/papers/
:p.
And the medium-length answer is on this page.
:p.
To begin with, run-time efficiency is not as big an issue as most people
seem to think it is. With modern compiler technology, the main programming
languages (apart from things like BASIC and its derivatives, and semi-interpreted
languages like Java, C#, Perl, and Rexx) give about
the same run-time efficiency. C and C++ lose out a little because their
low-level constructs make it hard for the compiler to do a good job at
optimisation; the figures I've seen tend to suggest that a program written in
Modula-2 runs a little faster than the same program written in C or C++.
However, the difference is typically less than 5%, and hardly worth worrying
about.
:p.
So the big issue is development efficiency. For a job like this we can
rule out languages like BASIC and REXX because they are a little too crude;
and we can rule out languages like Fortran because of their poor support
for "systems programming" tasks. We can also rule out a host of lesser-known
languages because of the unavailability of OS/2 compilers. That leaves us
with Pascal, Ada, Oberon, Modula-2, C, and C++.

:p.Oh, and Java. But Java is almost totally object-oriented, and
"object oriented" is one of those jargon terms meaning "probably buggy".
I've done a lot of looking into OO technologies - it's a topic that
programmers can hardly avoid these days - and have come to the
conclusion that OO is a mechanism for ensuring that programmers don't
understand their own code. The inventors of object orientation were
looking for a way to implement modularity, but in hindsight it
appears that they wandered into an evolutionary dead-end. A pity,
because Java could have been promising if it had adopted
"modularity" rather than "object orientation" as a goal, and had avoided
that slavish imitation of the unclear C++ notation.

:p.
I don't use Pascal because Modula-2 is basically an upgraded Pascal, and I
might as well use the improved version.
:p.
I haven't looked into the availability of Ada compilers for OS/2; but in any
case I don't like Ada because of its complexity. The bigger a language is,
the more things there are to go wrong.
:p.
Oberon is a more subjective matter. Some people will tell you that Oberon
is the successor to Modula-2, and is a superior programming language. My
personal opinion is that Oberon has deleted some of the features that make
Modula-2 a good language. I agree, however, that this issue is not entirely
clear-cut.
:p.
That brings us to C and C++. I've done a lot of C and C++ programming over
the years, and it's left me with the feeling that those languages are major
barriers to programming efficiency. It takes me roughly twice the time
to get a C or C++ program working as it does to get a comparable Modula-2
program working. (On some projects I've kept logs to verify this.) The
coding time is roughly the same, but there's a major difference in
debugging time. Everyone I know writes buggy software in C and C++, and
then they take forever trying to track down the bugs. Some developers
give up, and sell the software with the bugs still included.
:p.
There are two main reasons why C software is so bug-prone.
:ol.
:li.Lack of type safety. C is designed in such a way that the compiler can't
do much error checking, so the compiler gives no warnings for things that, in
a type-safe language, would be reported as errors at compile time. You don't
see the errors until execution time, and then you are left wondering what caused
the error.
:li.Poor support for modular programming. You can break up a C program into
modules, but they are not truly independent of one another. A slight change
in one module can have catastrophic effects on other modules. Once a project
grows moderately large, you lose control of your own code.
:eol.
:p.
C++ is a little better in these two respects, but C++ has problems of its own.
The language designers tried to graft high-level features onto a low-level
language, and the result is a mass of inconsistency. A C++ reference manual
is typically several times as thick as manuals for other programming languages,
because every rule has a maze of exceptions and special cases.
:p.
In addition, I've noticed that a lot of C++ programmers seem to have
adopted the philosophy of "let's try this, and hope that it works". The notion
that you shouldn't write code that you don't understand seems to have become
unfashionable. Maybe that's the fault of the language (and its libraries),
maybe not. In any case, it's not the way I prefer to work.
:p.
Ultimately, the reason I use Modula-2 is that it lets me get applications
working quickly, it gives me control of large projects, and it doesn't force
me to spend huge amounts of time on debugging. I'm too old to enjoy the
thrill of tracking down obscure bugs. I like to get something working, and
then be free to move on to other projects.
:p.
Of course, it's difficult to guarantee that any piece of software is bug-free,
no matter what development tools you use. But I can have the next-best
thing, which is an acceptably small error rate.

:h2 id=bugs.Known bugs
:p.:hp2.Known bugs and limitations in Weasel:ehp2.

:ul.
:li.The option of running from inetd is untested.
:eul.

:h2 id=reporting.Reporting errors

:p.:hp2.Reporting errors:ehp2.

:p.If you find any error that's not mentioned
in this document, please report it to peter@pmoylan.org.

:p.If the program crashes, it will normally leave one or two files in
the working directory: a file called errinfo.$$$, and a file with a &per.trp
extension. (More serious crashes might leave information in the file
POPUPLOG.OS2 in the root directory of your system drive.)
To help me track down the error, the most immediately useful information
is the errinfo.$$$ file, together with the Weasel version number. The
&per.trp file will become useful if a more detailed investigation is needed.

:p.The &per.trp file is generated by a utility called exceptq. Some Weasel
distributions include a file called exceptq.dll, but for proper exceptq
support you should really install exceptq properly by putting two DLLs to
your LIBPATH. To do this, go to Hobbes.nmsu.edu and search for exceptq, download the latest
version, and follow the instructions contained in the zip file. It is sufficient to
install the user's package; the developer's version is needed only by programmers.

.***********************************
.*   POP GOES THE WEASEL
.***********************************

:h1.Why did the weasel go pop?
:hp2.Why did the weasel go pop?:ehp2.

:p.Many children learn a traditional song from England called
"Pop goes the weasel". There are many versions; this is the one
that I learnt as a child.
:xmp.

        Half a pound of tuppeny rice
        Half a pound of treacle
        That's the way the money goes
        Pop goes the weasel
:exmp.
:p.If English is not your native language, you won't understand this. It uses
words that aren't properly explained in the dictionary.

:p.Tuppeny rice is rice that costs two pennies per pound. This doesn't
sound like much money, but it's a very old song. Treacle is another food,
similar (but not identical) to honey.

:p.Your dictionary will probably tell you that a weasel is a small
animal. It's true, that is the most common meaning. But not in this song.
There are a few different theories on the meaning here. The most
commonly accepted theory is that a weasel was a tailor's iron.
This is such an old and unusual meaning of the word that it's
missing from even relatively large dictionaries.

:p."Pop" is the noise that you hear when you stick a pin into a balloon.
Perhaps it's also the noise that happens when you pump too much air into
a small furry animal; but I'm not sure, because I've never tried that
experiment.

:p.In English, though, short words usually have more than one meaning.
In this song, "pop" means "pawn". You pawn something by taking it to a
pawnshop. A pawnshop is a place that will lend you money in exchange for
anything valuable. Maybe next week you'll have enough money to buy back
the items that you pawned. Maybe not. A pawnshop is a method for making
poor people even poorer.

:p.By now you should be able to guess the meaning of the song. The
poor fellow has had to pawn his weasel in order to buy food (rice and treacle).

:p.In case you are feeling sorry for him, I'll tell you the second
verse of the song.
:xmp.

        Up and down the City Road,
        In and out of the Eagle
        That's the way the money goes
        Pop goes the weasel
:exmp.
:p.In this verse, "The Eagle" is the name of a pub: a place that
sells alcoholic drinks. Alcohol is usually more expensive
than rice and treacle. I'll leave you to draw your own conclusions.

:p.There is, I've been told, still a pub called "The Eagle" near the
City Road in London. That one was built somewhere near the beginning
of the 19th century. (In fact, somebody sent me a photo of it, but I'm
so disorganised that I lost the photograph.) Before that, there was
another pub with the
same name and in approximately the same location. The song "Pop
goes the weasel" probably comes from a long time earlier than the
19th century; I don't know whether anyone knows exactly how old
it is.

:p.The more modern version is, of course
:xmp.

        POP3 goes the Weasel
:exmp.

:euserdoc.

