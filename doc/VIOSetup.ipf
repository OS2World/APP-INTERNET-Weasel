:userdoc.
:title.The VIOSetup utility
:docprof toc=1234.

.***********************************
.*   INTRODUCTION
.***********************************

:h1 res=1003 id=1003 global.The VIOSetup utility
:hp2.The VIOSetup utility:ehp2.

:p.The program VIOSETUP.EXE is provided as an alternative to SETUP.EXE,
for the benefit of those people who prefer not to use a GUI-based
utility. (If you do want to use the GUI version, see
:link reftype=hd database='weasel.inf' refid=1001.the main Weasel manual:elink..
The functions of VIOSetup are:
:ul compact.
:li.To set the parameters that Weasel.exe will use when it starts up.
:li.To define which users may access the POP server.
:eul.

:p.You should be aware that VIOSETUP is not updated as
often as SETUP, so you might find that the newer features of
Weasel are not fully supported. In particular, VIOSETUP cannot
deal with a multidomain configuration.

:p.The configuration details are split up into a number of different
screens. Use the F4 and F5 function keys on the keyboard to cycle
around the different screens. Within any one screen, use the cursor
movement keys to move to the item you want to change, and also to
select from a menu. Alternatively, you can use the Enter key to move
to the next item. Once you have changed an option, use the cursor
movement keys or the Enter key to move to a different item, thereby
confirming the change.

:p.
Now read

:ul compact.
:li.:link reftype=hd refid=configuser1.Setting the server parameters:elink.
:li.:link reftype=hd refid=viofilters.Filters:elink.
:li.:link reftype=hd refid=configoptions1.Options 1:elink.
:li.:link reftype=hd refid=configoptions2.Options 2:elink.
:li.:link reftype=hd refid=viorelay.Relay host for outgoing mail:elink.
:li.:link reftype=hd refid=viousers.Modifying the list of users:elink.
:li.:link reftype=hd refid=aliases.Aliases:elink.
:li.:link reftype=hd refid=configuser3.Names for the local host:elink.
:li.:link reftype=hd refid=configuser4.Trusted sources for relay mail:elink.
:li.:link reftype=hd refid=configuser4a.Acceptable destinations for relay mail:elink.
:li.:link reftype=hd refid=configuser5.Banned hosts:elink.
:li.:link reftype=hd refid=vioblacklists.Blacklists:elink.
:eul.

:h2 id=configuser1.Setting the server parameters
:hp2.Setting the server parameters:ehp2.
:p.
When you run VIOSETUP.EXE, you get a screen showing the following items.

:dl break=all.
:dt.SMTP port
:dd.Unless you are doing something nonstandard (for example, running two mail servers
on the same machine) this should always be 25.
:p.Hint: One use for nonstandard ports is to allow you to test Weasel without
disabling your existing mail software. You don't have to switch to the standard
port numbers until you have decided that you trust Weasel not to lose your mail.
:dt.Maximum number of SMTP users
:dd.This specifies how many SMTP clients will be allowed to use the server simultaneously.
I usually set this to 10. The number is not particularly critical; you would have to
have a very busy mail node before getting simultaneous access from many clients.
:dt.SMTP timeout (seconds)
:dd.The time that an SMTP session may remain idle before it is forcibly closed.
Most SMTP clients will log out cleanly, but occasionally the server has to
terminate a "dangling" session where, for example, the client machine crashed.
:dt.POP port
:dd.Unless you are doing something nonstandard (for example, running two mail servers
on the same machine) this should always be 110.
:dt.Maximum number of POP users
:dd.This specifies how many POP clients will be allowed to use the server simultaneously.
I usually set this to 10. The number is not particularly critical; you would have to
have a very busy mail node before getting simultaneous access from many clients.
:dt.POP timeout (seconds)
:dd.The time that an POP session may remain idle before it is forcibly closed.
Most POP clients will log out cleanly, but occasionally the server has to
terminate a "dangling" session where, for example, the client machine crashed.
:dt.Enable servers
:dd.This specifies whether Weasel should run as an SMTP server, or as a POP server,
or as both kinds of server simultaneously. Choose the "Both" option if Weasel
is the only mail server software that you are running.
:dt.Root directory for mail
:dd.The full path name of the directory that will be used for storing users'
mail files. This directory should exist, and preferably should not be used
for anything else.
:dt.SMTP logging
:dd.If this feature is enabled a record of received mail is written to a file
called SMTP.LOG. Each line in this file described one received mail item: date,
time, sending host, size, and a list of recipients.
:dt.POP user logging
:dd.If this feature is enabled a brief summary of each POP session is written
to a file called POP.LOG.
:dt.Log outgoing mail
:dd.If this feature is enabled a log of outgoing mail is written
to a file called SMTPOUT.LOG.
:dt.Detailed transaction log
:dd.You can choose to send a detailed log to the screen, or to a disk file, or both.
The disk file is called WEASEL.LOG, and it is updated approximately once every
minute if this feature is enabled.
:p.Warning: Transaction logging can create very large log files. I suggest that
you don't enable this feature unless you are trying to track down a problem.
:edl.

:p.
To modify any of these parameters, use the up/down arrow keys to get to the
desired item, then type in the new value. (The backspace, Insert, Delete, Home,
and End keys
will also work during editing.) The new value is accepted when you type the
Enter key, or when you use the function keys to go to another field.
:p.
When you have finished editing, use the Esc key to exit from the VIOSetup program, or
type F5 to get to the :link reftype=hd refid=viofilters.Filters:elink. page.

.***********************************
.*   FILTERS (VIO)
.***********************************

:h2 id=viofilters.Filters
:hp2.Filters:ehp2.
:p.
You get to this page by running VIOSETUP.EXE, and then pressing the F5 function
key until the "Filters" page appears. For more details, see the
"Using filters" section of
:link reftype=hd database='weasel.inf' refid=1001.the main Weasel manual:elink..

:p.The "Serialise filter operations" should normally be set to "Yes", to ensure
that different invocations of a filter do not interfere with one another when
several mail items are being processed simultaneously. You should turn this
option off only if you are certain that the filter programs contain their
own critical section protection.

:p.
When you have finished editing, use the Esc key to exit from the VIOSetup program, or
type F5 to get to the :link reftype=hd refid=configoptions1.Options 1:elink. page.


.***********************************
.*   OPTIONS 1
.***********************************

:h2 id=configoptions1.Options 1
:hp2.Options 1:ehp2.
:p.
You get to this page by running VIOSETUP.EXE, and then pressing the F5 function
key until the "Options 1" page appears. This gives you a screen showing the following items.
This page covers several miscellaneous options. Make sure that you understand
the options before enabling them.

:dl break=all.
:dt.Mail for unknown users
:dd.If you see this option, you are using an obsolete version of
VIOSetup. If you want to accept mail for unknown users, the
way to do it is to use :link reftype=hd refid=wildalias.wildcard aliases:elink..
:dt.Apply host tests to MAIL FROM address
:dd.If you choose "yes" for this option you will definitely slow down mail reception,
but it enables stricter anti-junk discrimination.
Normally Weasel refuses connections from a host under either of two conditions:
:ul.
:li.That host appears on your "banned hosts" list.
:li.The host is on one of the :link reftype=hd refid=vioblacklists.blacklists:elink.
for which you have enabled realtime blacklist checking.
:eul.
:p.If you enable the "Apply to MAIL FROM address" option, then these tests are
also done on the originating host, as shown in the SMTP command "MAIL FROM".
(More precisely, they are done on all hosts that act as MX relays for that
domain.)
That is, you are checking the ultimate source of the mail, rather than the
host that is relaying the mail to your computer.
:p.Note: you should be aware that it is possible for the sender to supply
a fake "MAIL FROM" address. Because of the increasing use of faked addresses
by junk mailers, this option is becoming less useful and might be removed
from future releases of Weasel. It is an unfortunate fact of life that, in
the ongoing battle between junk mailers and their victims, we have to
keep changing our anti-junk strategies.
:dt.POP login authenticates SMTP
:dd.This option is to support a feature called "POP-before-SMTP" authentication.
The idea behind it is that a POP login requires a password, but the standard
version of SMTP does
not use a password since incoming mail could come from anyone. If you
enable this feature then, each time one of your POP users logs in, that
IP address becomes a "trusted host" which is able to send relay mail.
After a specified number of minutes of idle time, the permission
disappears again.
:p.To disable this feature, set the time to 0. To enable it, use a
positive time; I suggest about five minutes.

:dt.     Allow SMTP AUTH authentication
:dd.The SMTP AUTH form of authentication is an extension to the SMTP
standard, and is explained on the SMTP Authentication page of.
:link reftype=hd database='weasel.inf' refid=1001.the main Weasel manual:elink..
There are actually several different kinds of SMTP AUTH authentication
supported by Weasel, of different security levels, so we allow you
to specify which of those mechanisms with be accepted, depending on
how cautious you are.
:note.Not all mail clients support these options, so you have to
decide for yourself the tradeoff between security levels and how
many users will be able to take advantage of those levels. The most
popular mail clients support only the LOGIN method, which is the worst
possible choice, and one that is not even supported by an official
standard. You will probably have to enable this method on the grounds
that it is the only method that your users' software will support.
If you do have control over the software that your users use, you
should enable the CRAM-MD5 method and disable the others.

:edl.

:p.
When you have finished editing, use the Esc key to exit from the VIOSetup program, or
use the F4 and F5 function keys to get to the
:link reftype=hd refid=configoptions2.Options 2:elink. page.

.***********************************
.*   OPTIONS 2
.***********************************

:h2 id=configoptions2.Options 2
:hp2.Options 2:ehp2.
:p.
You get to this page by running VIOSETUP.EXE, and then pressing the F5 function
key until the "Options 2" page appears. This gives you a screen showing the
following items.

:p.:hp2.When to go online:ehp2.

:p.
The "When to go online" option on this page controls how Weasel decides that it is online and can send
outgoing mail. (When it is offline it still works, but it saves any
outgoing mail to be sent later. The primary purpose of the "online" decision
is for deciding whether we should attempt to send outgoing mail.) You have the choice of three
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

:p.:hp2.Identifying a POP3 user:ehp2.

:p.If you are running in single-domain mode, this option is irrelevant.
If you have multidomain mode activated, which in this version can be done
only by running Setup rather than VIOSetup, then this option controls
how we work out the domain to which a POP3 user belongs.

:p.This option is relevant
only when a POP3 user logs in with a username that is valid in two
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
:li.Mail to some hard-to-reach destinations can be slow, because
of factors like slow network connections. With multiple threads,
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
incoming mail and POP3 connections.

:note.If you do change the number of threads handling outbound mail,
the change will not take effect until the next time you restart
Weasel.

:p.
When you have finished editing, use the Esc key to exit from the VIOSetup program, or
use the F4 and F5 function keys to get to the other VIOSetup pages.

.***********************************
.*   RELAY PAGE
.***********************************

:h2 id=viorelay.Relay host for outgoing mail

:hp2.Relay host for outgoing mail:ehp2.

:p.This page controls how you send your mail through a relay host,
i.e. another SMTP server that will take care of your outgoing mail.
If you have a dial-up connection you will probably need to relay
mail through your ISP (Internet Service Provider), either permanently
or as a backup solution, because many mail servers reject connections
from dial-up lines as part of their anti-junk policies. (Weasel can
also do this, via the
:link reftype=hd refid=vioblacklists.blacklist:elink. option.)

:p.If you have a permanent internet connection you do not, strictly
speaking, need a relay host, so you can specify "never" as the first
option on this page, and then the rest of the page becomes irrelevant.
Even then, however, it is a good idea to use a backup server, if you
have access to one, to relieve your own server of the load caused by
hard-to-deliver mail.

:p.:hp2.Note:ehp2.&colon. This option is provided for the case where, for example, you have
to send all your mail through a gateway. If you use it, make sure that you
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
:xmp.          example.com&colon.5001

:exmp.
specifies that relay mail should go to port 5001 on host example.com.
:dt.     Authentication for outgoing mail
:dd.What you need to specify here depends on the policies of the
server you are using as a relay host. (If you are not using a relay host,
you can specify "none" as the authentication mechanism and skip the rest
of this page.) Some servers do not require any authentication, because
they use your IP address as a confirmation that you are one of their
customers, and in that case you can specify "none" as the authentication
mechanism. Otherwise, you need to specify "SMTP AUTH" or "POP before SMTP",
depending on the policies of the relay host. (These authentication mechanisms are explained on the
SMTP authentication section of
:link reftype=hd database='weasel.inf' refid=1001.the main Weasel manual:elink.).
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
:edl.

.***********************************
.*        USER EDITOR
.***********************************

:h2 id=viousers toc=1234.Modifying the list of users
:hp2.Modifying the list of users:ehp2.
:p.
To modify the user list, run VIOSETUP.EXE, and then type the F5 function key
until you get the the "Users" screen page. This will give you a list of
all existing users. (The first time you run the program, the list will probably
be empty.)
:p.
From this screen, you can add, delete, or modify users. When you have finished,
type X to exit from the VIOSetup program, or use the F4 and F5 function keys
to move to the other configuration pages.
:p.
:hp2.Deleting a user:ehp2.
:p.
Use the up/down arrow keys to get to the user you want to delete, and type
the Del (delete) key.
:p.
:hp2.Adding a new user:ehp2.
:p.
Type A, and then proceed as for :link reftype=hd refid=edituser.Editing a user record:elink..
:p.
:hp2.Editing the details for an existing user:ehp2.
:p.
Type E, and then follow the instructions in the section :link reftype=hd refid=edituser.Editing a user record:elink..

:h3 id=edituser.Editing a user record
:hp2.Editing a user record:ehp2.
:p.
You get to this point by running the VIOSetup program, typing F5 to get to
the user editor, and then using one of the "A" (add user) or "E" (edit user)
options.
:p.
By now you should have two fields near the top of the screen.
:dl break=all.
:dt.    User name
:dd.The name that the user will use when logging in.
:dt.    Password
:dd.This user's password.
:edl.
:p.
Use the up/down arrow keys to get to the field you want to edit, and
then modify it as necessary. When you have finished, use the Enter key
or the Cursor Down key to confirm the changes.

:warning.If you change the user name, the data for the previous
user name will be deleted. You should also avoid using a user name
that is the same as for some other user.:ewarning.
:p.
When you have finished setting up all users, type the F5 key to get
to the next page of the VIOSetup program, or type X to leave
the VIOSetup program.

.***********************************
.*           ALIASES
.***********************************

:h2 id=aliases toc=1234.Aliases
:hp2.Aliases:ehp2.
:p.
An alias looks like a username from the viewpoint of incoming mail,
but the name does not correspond to the name of any POP user. Instead,
the alias refers to a list of e-mail addresses. Whenever an e-mail
arrives addressed to that alias, a copy is sent to everyone on the
list.

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

:h3.Adding and changing aliases
:hp2.Adding and changing aliases:ehp2.
:p.
To create or change aliases, run VIOSETUP.EXE, and then type the F5 function key
until you get to the "Aliases" screen page. This will give you a list of
all existing aliases. (The first time you run the program, the list will probably
be empty.)
:p.
From this screen, you can add, delete, or modify aliases. When you have finished,
use the F4 or F5 function key to go to other sections of the VIOSetup program, or
type X to exit completely from VIOSetup.
:p.
:hp2.Deleting an alias:ehp2.
:p.
Use the up/down arrow keys to get to the alias you want to delete, and type
the Del (delete) key.
:p.
:hp2.Adding a new alias:ehp2.
:p.
Type A, and then proceed as for :link reftype=hd refid=editalias.Editing an alias expansion:elink..
:p.
:hp2.Editing the expansion of an existing alias:ehp2.
:p.
Type E, and then follow the instructions in the section :link reftype=hd refid=editalias.Editing an alias expansion:elink..
:p.
:hp2.Renaming an existing alias:ehp2.
:p.
Type R, and then type the new name. The name will be changed when you type the
<Enter> key, or when you use the up/down arrow keys to move to another entry.
:p.
:hp2.Changing the order of the entries:ehp2.
:p.
Move the cursor to any entry except the first, then type P.
This will swap the selected entry with the one above it. By doing
this enough times, you can sort the entries in any way you wish.
The order is important only when some of the entries contain
:link reftype=hd refid=wildalias.wildcards:elink..

.***********************************
.*     EDITING AN ALIAS EXPANSION
.***********************************

:h3 id=editalias.Editing an alias expansion
:hp2.Editing an alias expansion:ehp2.
:p.
You get to this point by running the VIOSetup program, typing F5 until you get to
the alias editor, and then using one of the "A" (add alias) or "E" (edit alias)
options.
:p.
Initially the word "Private" or "Public" will be highlighted at the top left
of the screen. If you want to change this, use the "cursor left" or
"cursor right" key. Then type the "cursor down" key to get to the list itself.

:p.At this point you have the following choices.
:p.
:hp2.Adding a new entry:ehp2.
:p.
Type A, and then type an e-mail address. Use the <Enter> key or the up/down
arrow keys to confirm your choice, or the Esc key if you change your mind.
:p.
:hp2.Editing an existing entry:ehp2.
:p.
Type E, and then modify the e-mail address. Use the <Enter> key or the up/down
arrow keys to confirm your choice, or the Esc key if you change your mind.
:p.
:hp2.Deleting an existing entry:ehp2.
:p.
Type the Delete key.
:p.
:hp2.Changing the Public/Private status of the alias:ehp2.
:p.
Use the Home, PageUp, and/or the up arrow key until the cursor is at the
top of the screen, and then use the left or right arrow key.

:p.
When you have finished making modifications on this page, type Esc to
complete the operation. If you leave the VIOSetup program before doing this,
your changes will not be saved.

.***********************************
.*        WILDCARD ALIASES
.***********************************

:h3 id=wildalias.Wildcard aliases
:hp2.Wildcard aliases:ehp2.

:p.One special case is where the name of an
:link reftype=hd refid=aliases.alias:elink. contains the wildcard
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
mail for all usernames for which you have :hp1.not:ehp1. created an
account. This can be useful for things like junk mail checking.

:p.When you use wildcards, it is obviously possible for an incoming
address to match more than one alias. In that case, the one that
occurs first in the list of alias names is the one that is used.
For that reason, you should normally order the names such that the
wildcard entries come towards the end of the list.

.***********************************
.*   NAMES FOR THE LOCAL HOST
.***********************************

:h2 id=configuser3.Names for the local host
:hp2.Names for the local host:ehp2.

:p.
To reach this page, run VIOSetup and type F5 several times, until the
"local host" page appears.

:p.An e-mail address has the form "user@domain". If your local
nameserver has an MX record for your machine that matches the "domain"
part, then this is mail to be delivered locally.

:p.In the case where the "domain" part is actually the hostname of
your machine, or an alias known to your local nameserver, Weasel can work
this out for itself. Many users, therefore, can afford to leave this
list empty. Unfortunately Weasel can't (yet) handle the case where the
domain is not a hostname, but is instead something mapped to a hostname
by a nameserver MX record. (The catch here is that we can't tell
whether the MX record identifies the final destination, or simply a
relay host. If there's a way to make this distinction, I haven't
yet worked it out.) In such cases, then, you must explicitly list
the domain name(s) on this page.

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=hostlist.Editing a list of host names:elink..

:p.Note, by the way, that you can't define hostname aliases simply by adding
extra entries to this list. The aliases are no good unless they are
also recognised by the nameserver. If the nameserver does not
define a name, then there's no way for people at other sites to use
that name as part of an e-mail address; their own SMTP client will
report an "unknown host" error.

:p.Note, too, that some mail clients will refuse to send mail to
your machine if they can't find an MX record for it. You should ask
your local network manager - the person who looks after the
nameserver - to include MX records for your computer in the
nameserver tables.

.***********************************
.*   SOURCES FOR RELAY MAIL
.***********************************

:h2 id=configuser4.Trusted sources for relay mail
:hp2.Trusted sources for relay mail:ehp2.

:p.
To reach this page, run VIOSetup and type F5 several times, until the
"acceptable sources for relay mail" page appears.

:p.Relay mail is mail that the SMTP server accepts and agrees to
pass on to another host. The Weasel philosophy is that unlimited
relay mail should not be permitted. It puts an extra load on your
computer, and it helps junk mailers to hide the real origin of
their junk mail.

:p.There are just two cases where it can make sense to permit
your machine to be used as a relay host.
:ul.
:li.Where your computer is acting as a gateway to a network that
would otherwise be unable to receive mail from the outside world.
This "gateway" function is described on the following page.
:li.Where you want to offer an "outgoing mail" service to the users
that have accounts on your POP server. This function is the subject
of the current page.
:eul.
:p.The "trusted sources" list is a list of host names. Mail sent
from those hosts, and addressed to some third host, will be accepted
to be forwarded. Mail sent from elsewhere will not be accepted
unless it is addressed to the machine on which Weasel is running, or
when it is sent from an authenticated client, or when it is addressed
to a domain on the "acceptable destinations" list.
In other words, Weasel will not accept mail for relaying except
when it is authenticated, or when it comes from one of the hosts
on this list, or when it is going to an approved destination.

:note.The only default trusted address is the loopback address
[127.0.0.1], which must remain enabled because Weasel sometimes needs
to send mail back to itself. Apart from this special case all relay mail, even mail from your
own computer, will be rejected if it does not satisfy one of the
above conditions.

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=hostlist.Editing a list of host names:elink..

.***********************************
.*   ACCEPTABLE RELAY DESTINATIONS
.***********************************

:h2 id=configuser4a.Acceptable destinations for relay mail
:hp2.Acceptable destinations for relay mail:ehp2.

:p.
To reach this page, run VIOSetup and type F5 several times, until the
"acceptable destinations for relay mail" page appears.

:note.This list should normally be empty. In the majority of
cases, the authentication should come from the sending side, not
from the receiving side.

:p.Relay mail is mail that the SMTP server accepts and agrees to
pass on to another host.  The Weasel philosophy is that unlimited
relay mail should not be permitted. It puts an extra load on your
computer, and it helps junk mailers to hide the real origin of
their junk mail.

:p.There are just two cases where it can make sense to permit
your machine to be used as a relay host.
:ul.
:li.Where your computer is acting as a gateway to a network that
would otherwise be unable to receive mail from the outside world.
This "gateway" function is the subject of the present page.
:li.Where you want to offer an "outgoing mail" service to the users
that have accounts on your POP server. This function was described
on the previous page.
:eul.
:p.The "acceptable destinations" list is a list of domain names. Mail
addressed to those domains will be accepted
to be forwarded, whether or not it comes from one of the "trusted hosts".

:p.Instructions for creating or modifying the list are given on
the manual page called :link reftype=hd refid=hostlist.Editing a list of host names:elink..

.***********************************
.*   BANNED HOSTS
.***********************************

:h2 id=configuser5.Banned hosts
:hp2.Banned hosts:ehp2.

:p.
To reach this page, run VIOSetup and type F5 several times, until the
"banned hosts" page appears.

:p.This page contains your personal blacklist of hosts that are not
allowed to send you mail. Weasel will refuse to accept any mail
from hosts on this list.

:p.Instructions for creating or modifying this list are given on
the manual page called :link reftype=hd refid=hostlist.Editing a list of host names:elink..

.***********************************
.*   EDITING A HOST LIST
.***********************************

:h2 id=hostlist.Editing a list of host names
:hp2.Editing a list of host names:ehp2.

:p.When VIOSetup presents you with a list of host names to be edited,
you have the following options:

:dl.
:dt.   A
:dd.Add a new entry. Once you have typed the new entry, use the
Enter key or the "cursor up" key to complete the operation.
:dt.   P
:dd.Promote: move the current entry up in the list. You can use
this for changing the order of the entries in the list.
:dt.   Del
:dd.(The Delete key.) Deletes the current entry.
:dt.   X
:dd.Exit completely from the VIOSetup program.
:dt.   F5
:dd.Move to the next page of the VIOSetup options.
:edl.

:p.In addition, you can of course use the cursor up/down keys to
move through the entries on the list. The Home, End, PageUp,
and PageDown keys may also be used.

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
:li.An IP address range in the format a.b.c.d/N, where N (a number in
the range 1 to 32) specifies how many of the least significant bits
are "don't care" bits. For example, [123.45.254.0/9] refers to the
range from [123.45.254.0] to [123.45.255.255], inclusive. This format is a common
way of referring to a subnet of 2^N addresses.
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

:h2 id=vioblacklists.Blacklists
:hp2.Blacklists:ehp2.
:p.
The "banned hosts" facility of Weasel should be a good way of
controlling junk mail, but it has a serious shortcoming&colon.
because the junk mailers know that everyone is trying to block their
mail, they keep changing their addresses.

:p.In response to this, some organisations now maintain "blacklist"
databases that attempt to track the sources of junk mail, and/or the
open relays that are allowing the junk mail to propagate. These
are further explained on the page about realtime blacklist databases in
:link reftype=hd database='weasel.inf' refid=1001.the main Weasel manual:elink..

:p.Earlier versions of Weasel included hard-coded links to the
blacklist sites, but those addresses have now become obsolete.
The MAPS site changed its addresses when it changed from being a free
site to being a subscription site, and the ORBS database of open relays
was shut down as the result of legal action. We will probably see
further changes as the war between the spammers and their victims evolves.
To allow for the current and possible future changes, you now have
to specify the domain names of the blacklist checkers.

:p.You are allowed to use several such sites. To activate the
checking, use the cursor up/down keys to go to one of the
entries; use the cursor right key to switch its status from
'Disabled' to 'Enabled'; use the cursor down key to go to the
text entry field, and there type the domain name that you
obtained from the provider of the blacklist. Finally, use
cursor up or cursor down to move off the field you have just
edited.

:p.To deactivate the checking, go to one of the entries
and use the cursor left key to change its status from 'Enabled'
to 'Disabled'. The name is left there in case you want to
reactivate it at some later stage, but that name will not be
used as long as its status remains at 'Disabled'.

:p.Note that some of the blacklist sites are subscription sites,
which means that they won't work for you unless you are a subscriber.

.*************************************************************************

:euserdoc.

