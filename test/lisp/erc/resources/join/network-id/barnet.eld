;; -*- mode: lisp-data; -*-
((pass 10 "PASS :barnet:changeme"))
((nick 2 "NICK tester"))
((user 1 "USER user 0 * :tester")
 (0 ":irc.barnet.org 001 tester :Welcome to the barnet IRC Network tester")
 (0 ":irc.barnet.org 002 tester :Your host is irc.barnet.org, running version oragono-2.6.0-7481bf0385b95b16")
 (0 ":irc.barnet.org 003 tester :This server was created Mon, 10 May 2021 00:58:22 UTC")
 (0 ":irc.barnet.org 004 tester irc.barnet.org oragono-2.6.0-7481bf0385b95b16 BERTZios CEIMRUabefhiklmnoqstuv Iabefhkloqv")
 (0 ":irc.barnet.org 005 tester AWAYLEN=390 BOT=B CASEMAPPING=ascii CHANLIMIT=#:100 CHANMODES=Ibe,k,fl,CEMRUimnstu CHANNELLEN=64 CHANTYPES=# ELIST=U EXCEPTS EXTBAN=,m FORWARD=f INVEX KICKLEN=390 :are supported by this server")
 (0 ":irc.barnet.org 005 tester MAXLIST=beI:60 MAXTARGETS=4 MODES MONITOR=100 NETWORK=barnet NICKLEN=32 PREFIX=(qaohv)~&@%+ STATUSMSG=~&@%+ TARGMAX=NAMES:1,LIST:1,KICK:1,WHOIS:1,USERHOST:10,PRIVMSG:4,TAGMSG:4,NOTICE:4,MONITOR:100 TOPICLEN=390 UTF8MAPPING=rfc8265 UTF8ONLY WHOX :are supported by this server")
 (0 ":irc.barnet.org 005 tester draft/CHATHISTORY=100 :are supported by this server")
 (0 ":irc.barnet.org 251 tester :There are 0 users and 3 invisible on 1 server(s)")
 (0 ":irc.barnet.org 252 tester 0 :IRC Operators online")
 (0 ":irc.barnet.org 254 tester 1 :channels formed")
 (0 ":irc.barnet.org 255 tester :I have 3 clients and 0 servers")
 (0 ":irc.barnet.org 265 tester 3 3 :Current local users 3, max 3")
 (0 ":irc.barnet.org 266 tester 3 3 :Current global users 3, max 3")
 (0 ":irc.barnet.org 422 tester :MOTD File is missing"))

((mode-user 12 "MODE tester +i"))
;; No mode answer

((join 2 "JOIN #chan")
 (0 ":tester!~u@6yximxrnkg65a.irc JOIN #chan")
 (0 ":irc.barnet.org 353 tester = #chan :@joe mike tester")
 (0 ":irc.barnet.org 366 tester #chan :End of NAMES list")
 (0.1 ":joe!~u@6yximxrnkg65a.irc PRIVMSG #chan :tester, welcome!")
 (0 ":mike!~u@6yximxrnkg65a.irc PRIVMSG #chan :tester, welcome!"))

((mode 1 "MODE #chan")
 (0 ":irc.barnet.org 324 tester #chan +nt")
 (0 ":irc.barnet.org 329 tester #chan 1620608304")
 ;; Wait for foonet's buffer playback
 (0.1 ":mike!~u@6yximxrnkg65a.irc PRIVMSG #chan :joe: Go take her hence, and marry her instantly.")
 (0.1 ":joe!~u@6yximxrnkg65a.irc PRIVMSG #chan :mike: Of all the four, or the three, or the two, or one of the four.")
 (0.1 ":mike!~u@6yximxrnkg65a.irc PRIVMSG #chan :joe: And gives the crutch the cradle's infancy.")
 (0.1 ":joe!~u@6yximxrnkg65a.irc PRIVMSG #chan :mike: Such is the simplicity of man to hearken after the flesh.")
 (0.05 ":mike!~u@6yximxrnkg65a.irc PRIVMSG #chan :joe: The leaf to read them. Let us toward the king.")
 (0.05 ":joe!~u@6yximxrnkg65a.irc PRIVMSG #chan :mike: Many can brook the weather that love not the wind.")
 (0.05 ":mike!~u@6yximxrnkg65a.irc PRIVMSG #chan :joe: And now, dear maid, be you as free to us.")
 (0.00 ":joe!~u@6yximxrnkg65a.irc PRIVMSG #chan :mike: He hath an uncle here in Messina will be very much glad of it."))

((linger 30 LINGER))
