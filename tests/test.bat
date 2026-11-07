;;; Runs every case under every conflict resolution strategy CLIPS offers.  A
;;; COOL codec keeps only one rule on the agenda at a time, so it decodes the
;;; same way under each strategy; a case that fails under one strategy and not
;;; another points at a rule that relies on firing order.  All the cases
;;; decode in one run, so a strategy like breadth advances every stream a
;;; character at a time in lockstep: a rule whose patterns are not all tied to
;;; one stream then matches facts from two streams at once, which decoding
;;; one case at a time cannot show.  The expected files decode in the same
;;; run, for the round trip.
;(watch rules)
;(watch facts)
;(watch activations)
;(watch statistics)
(load* compline.clp)
(load* ../program.clp)
(load* cases.clp)

(defglobal ?*strategies* = (create$ depth breadth lex mea complexity simplicity random))

;;; the strategy under test, named in each failure.  It survives each reset.
(set-reset-globals FALSE)
(defglobal ?*strategy* = depth)

;;; the maximum number of messages a stream reads
(defglobal ?*max-messages* = 1000)

;;; TRUE if the codec has read the whole stream
(deffunction at-eof (?id)
	(any-factp ((?e ftp-eof)) (eq ?e:id ?id)))

;;; Decodes every case and expected file at once.  Each path opens under
;;; its own name, with the parameters of its case.  A message retracts its
;;; ready fact.  Each round asserts one for every stream not at eof, and
;;; reads one message from each, as a program that serves many connections
;;; does.
(deffunction decode-all (?files)
	(reset)
	(bind ?ids (create$))
	(foreach ?file ?files
		(foreach ?id (create$ (sym-cat cases/ ?file) (sym-cat expected/ ?file))
			(open ?id ?id)
			(ftp-parse-case ?file ?id)
			(bind ?ids (create$ ?ids ?id))))
	(bind ?reading TRUE)
	(bind ?rounds 0)
	(while (and ?reading (< ?rounds ?*max-messages*)) do
		(bind ?reading FALSE)
		(bind ?rounds (+ ?rounds 1))
		(foreach ?id ?ids
			(if (not (at-eof ?id)) then
				(assert (ftp-ready (id ?id)))
				(bind ?reading TRUE)))
		(run))
	(foreach ?id ?ids (close ?id)))

;;; Compares what the codec decoded from stream ?id with the file of the
;;; same name as ?file in expected.  If they differ, prints the failure
;;; with ?reason after the strategy.  Returns TRUE if they match.
(deffunction check-stream (?file ?id ?reason)
	(if (not (at-eof ?id)) then
		(println "FAIL " ?file " (" ?*strategy* "): " ?id " did not reach ftp-eof")
		(return FALSE))
	(open actual actual "w")
	(foreach ?message (ftp-messages ?id)
		(send ?message pprint actual))
	(close actual)
	(bind ?differences (compare-files (sym-cat expected/ ?file) actual))
	(if (> (length$ ?differences) 0) then
		(println "FAIL " ?file " (" ?*strategy* ")" ?reason)
		(foreach ?line ?differences (println ?line))
		(return FALSE))
	TRUE)

;;; A case passes if its messages print as the expected file, and the
;;; expected file prints back unchanged.  A case with no messages prints
;;; nothing.
(deffunction check-case (?file)
	(and (check-stream ?file (sym-cat cases/ ?file) "")
	     (check-stream ?file (sym-cat expected/ ?file) ": expected output does not round trip")))

;;; the messages the run decoded from a case file, in order
(deffunction case-messages (?file)
	(ftp-messages (sym-cat cases/ ?file)))

;;; Joins strings and character codes into one string.  Call it after the
;;; run: a string made before (run) is temporary.
(deffunction joined ($?pieces)
	(bind ?value "")
	(foreach ?piece ?pieces
		(if (integerp ?piece)
			then (bind ?value (str-cat ?value (format nil "%c" ?piece)))
			else (bind ?value (str-cat ?value ?piece))))
	?value)

;;; The answer of ?message to ?query, with ?arguments if there are some.
;;; A message of the wrong class has no handler for the query.  Then the
;;; answer is the class, so the check fails and the batch does not stop
;;; before it exits.
(deffunction answer (?message ?query $?arguments)
	(bind ?answers FALSE)
	(foreach ?class (create$ (class ?message) (class-superclasses (class ?message) inherit))
		(if (message-handler-existp ?class ?query primary) then (bind ?answers TRUE)))
	(if ?answers
		then (send ?message ?query (expand$ ?arguments))
		else (sym-cat (class ?message) " has no " ?query)))

(deffunction report (?file ?what ?expected ?actual)
	(println "FAIL " ?file " " ?what " (" ?*strategy* ")")
	(println "   expected: " ?expected)
	(println "   actual:   " ?actual)
	1)

;;; Checks the message count of a case file.  Returns 1 on a failure, 0 on
;;; a pass.
(deffunction check-count (?file ?expected)
	(bind ?actual (length$ (case-messages ?file)))
	(if (neq ?actual ?expected) then (return (report ?file "count" ?expected ?actual)))
	0)

;;; Checks the class of message ?n of a case file.
(deffunction check-class (?file ?n ?expected)
	(bind ?messages (case-messages ?file))
	(bind ?actual (if (> ?n (length$ ?messages)) then MISSING else (class (nth$ ?n ?messages))))
	(if (neq ?actual ?expected) then (return (report ?file (str-cat ?n " class") ?expected ?actual)))
	0)

;;; Checks the answer of message ?n to ?query: a slot accessor such as
;;; get-verb, or a query such as host-port.  The check compares the answer
;;; as a multifield.  Give no expected values for an empty answer.
(deffunction check-message (?file ?n ?query $?expected)
	(bind ?messages (case-messages ?file))
	(bind ?actual (if (> ?n (length$ ?messages))
		then (create$ MISSING)
		else (create$ (answer (nth$ ?n ?messages) ?query))))
	(if (neq ?actual ?expected) then
		(return (report ?file (str-cat ?n " " ?query) (implode$ ?expected) (implode$ ?actual))))
	0)

;;; The same for a query that takes an argument, such as outcome with a
;;; verb.
(deffunction check-query (?file ?n ?query ?argument $?expected)
	(bind ?messages (case-messages ?file))
	(bind ?actual (if (> ?n (length$ ?messages))
		then (create$ MISSING)
		else (create$ (answer (nth$ ?n ?messages) ?query ?argument))))
	(if (neq ?actual ?expected) then
		(return (report ?file (str-cat ?n " " ?query " " ?argument) (implode$ ?expected) (implode$ ?actual))))
	0)

;;; The same for a query that answers one string.  Give the string as
;;; pieces: strings and character codes.
(deffunction check-text (?file ?n ?query $?pieces)
	(bind ?messages (case-messages ?file))
	(bind ?expected (joined (expand$ ?pieces)))
	(bind ?actual (if (> ?n (length$ ?messages))
		then MISSING
		else (answer (nth$ ?n ?messages) ?query)))
	(if (neq ?actual ?expected) then
		(return (report ?file (str-cat ?n " " ?query) ?expected ?actual)))
	0)

;;; Checks that a hand-made message prints as the pieces.
(deffunction check-pprint (?message $?pieces)
	(open actual actual "w")
	(send ?message pprint actual)
	(close actual)
	(open actual actual)
	(bind ?actual "")
	(bind ?byte (get-char actual))
	(while (neq ?byte -1) do
		(bind ?actual (format nil "%s%c" ?actual ?byte))
		(bind ?byte (get-char actual)))
	(close actual)
	(bind ?expected (joined (expand$ ?pieces)))
	(if (neq ?actual ?expected) then
		(return (report (instance-name ?message) "pprint" ?expected ?actual)))
	0)

;;; Checks the values the run decoded.  Returns the number of checks and
;;; the number that failed.
(deffunction run-value-tests ()
	(bind ?results (create$
		(check-count test.ftp 5)
		(check-count test5.ftp 9)
		(check-count test11.ftp 0)
		(check-count test12.ftp 3)
		(check-count test16.ftp 1)
		(check-count test17.ftp 4)
		(check-count test21.ftp 5)
		(check-count test30.ftp 2)
		(check-class test.ftp 1 FTP-COMMAND)
		(check-class test2.ftp 1 FTP-REPLY)
		(check-class test18.ftp 1 FTP-REPLY)
		(check-class test21.ftp 1 FTP-COMMAND)
		(check-class test21.ftp 4 FTP-COMMAND)
		(check-message test.ftp 1 get-verb USER)
		(check-message test.ftp 1 get-argument "anonymous")
		(check-message test.ftp 3 get-argument "")
		(check-message test.ftp 5 get-sequence 5)
		(check-message test2.ftp 1 get-code 220)
		(check-message test2.ftp 5 get-text "\"/\" is the current directory")
		(check-message test2.ftp 5 pathname "/")
		(check-message test2.ftp 1 pathname FALSE)
		(check-message test2.ftp 1 get-lines)
		(check-message test3.ftp 1 get-code 220)
		(check-message test3.ftp 1 get-lines "Welcome to the COOLFTP test server."
			" Unauthorized access is prohibited." " All transfers are logged.")
		(check-message test3.ftp 1 get-text "Ready for a new user.")
		(check-text test3.ftp 1 full-text "Welcome to the COOLFTP test server." 10
			" Unauthorized access is prohibited." 10 " All transfers are logged." 10
			"Ready for a new user.")
		(check-text test2.ftp 6 full-text "Goodbye.")
		(check-message test4.ftp 1 features "MLST type*;size*;modify*;" "REST STREAM" "SIZE" "UTF8")
		(check-message test2.ftp 1 features)
		(check-message test6.ftp 1 host-port)
		(check-message test6.ftp 2 host-port "192.168.1.10" 4872)
		(check-message test6.ftp 3 host-port "10.0.0.1" 1025)
		(check-message test6.ftp 4 host-port)
		(check-message test7.ftp 2 host-port "" 6446)
		(check-message test7.ftp 3 host-port "132.235.1.2" 6275)
		(check-message test7.ftp 5 host-port "1080::8:800:200C:417A" 5282)
		(check-message test8.ftp 1 get-verb USER)
		(check-message test8.ftp 3 get-verb NOOP)
		(check-message test9.ftp 2 get-code 331)
		(check-message test10.ftp 3 get-verb QUIT)
		(check-message test12.ftp 3 get-verb QUIT)
		(check-message test13.ftp 1 get-verb SITE)
		(check-message test13.ftp 1 get-argument "CHMOD 755 file.txt")
		(check-message test13.ftp 2 get-argument "-la /pub/some dir")
		(check-message test14.ftp 1 get-lines "Welcome" "220-to the" "220-server")
		(check-message test15.ftp 1 get-lines "Status follows:" " 123 files" "2 directories"
			"21 links" "2131 is not the end" "213x neither" "213-nor this")
		(check-message test15.ftp 1 get-text "End of status")
		(check-message test16.ftp 1 get-lines "Welcome" " to the server" "")
		(check-message test16.ftp 1 get-text "")
		(check-message test17.ftp 2 get-verb ABOR)
		(check-message test17.ftp 3 get-code 426)
		(check-message test18.ftp 1 get-text "")
		(check-message test18.ftp 2 get-argument "")
		(check-message test18.ftp 3 get-text "")
		(check-message test19.ftp 2 pathname "/home/quote\"d")
		(check-message test19.ftp 4 pathname "/home/quote\"d")
		(check-message test20.ftp 1 get-argument "/données/北京")
		(check-message test20.ftp 2 pathname "/données/北京")
		(check-message test21.ftp 1 get-argument "foo")
		(check-message test21.ftp 3 get-verb 2X)
		(check-message test21.ftp 4 get-argument "y")
		(check-message test22.ftp 1 get-lines "" "" " middle" "")
		(check-message test22.ftp 1 get-text "ok")
		(check-message test23.ftp 4 host-port "127.0.0.1" 50000)
		(check-message test23.ftp 6 category positive-preliminary)
		(check-message test23.ftp 7 category positive-completion)
		(check-message test2.ftp 2 category positive-intermediate)
		(check-message test17.ftp 3 category transient-negative-completion)
		(check-message test23.ftp 9 category permanent-negative-completion)
		(check-message test25.ftp 7 category protected)
		(check-message test6.ftp 4 group syntax)
		(check-message test2.ftp 4 group information)
		(check-message test2.ftp 1 group connections)
		(check-message test2.ftp 2 group authentication)
		(check-message test2.ftp 5 group file-system)
		(check-message test2.ftp 1 positive TRUE)
		(check-message test17.ftp 3 positive FALSE)
		(check-message test23.ftp 9 positive FALSE)
		(check-message test24.ftp 1 get-verb NOOP)
		(check-message test24.ftp 2 get-argument "bob")
		(check-message test26.ftp 2 features "type=file;size=1024;modify=20260905120000; /pub/file.txt")
		(check-message test27.ftp 1 get-text "OK   ")
		(check-message test27.ftp 2 get-argument "bob  ")
		(check-message test28.ftp 1 get-lines "Welcome" "220" "220-")
		(check-message test29.ftp 1 get-verb STOR)
		(check-message test30.ftp 1 get-argument "bob")
		(check-message test30.ftp 2 get-text "Password")
		(check-pprint (make-instance of FTP-COMMAND (verb QUIT)) "QUIT" 13 10)
		(check-pprint (make-instance of FTP-COMMAND (verb CWD) (argument "/pub")) "CWD /pub" 13 10)
		(check-pprint (make-instance of FTP-REPLY (code 221) (text "Goodbye")) "221 Goodbye" 13 10)
		(check-pprint (make-instance of FTP-REPLY (code 220) (lines "Hello" " there") (text "Ready"))
			"220-Hello" 13 10 " there" 13 10 "220 Ready" 13 10)
		; RFC 959 section 7
		(check-count test31.ftp 13)
		(check-message test31.ftp 6 get-verb RETR)
		(check-message test31.ftp 6 pathname "test.pl1")
		(check-query test31.ftp 7 outcome RETR wait)
		(check-query test31.ftp 8 outcome RETR success)
		(check-query test31.ftp 8 answers RETR TRUE)
		(check-query test31.ftp 12 outcome STOR failure)
		(check-query test31.ftp 12 answers STOR FALSE)
		(check-query test31.ftp 1 outcome CONNECTION success)
		(check-query test31.ftp 1 answers CONNECTION TRUE)
		(check-query test31.ftp 3 outcome USER continue)
		(check-query test31.ftp 3 answers USER TRUE)
		(check-query test31.ftp 5 outcome PASS success)
		(check-message test31.ftp 2 next PASS)
		(check-message test31.ftp 4 next ACCT)
		(check-message test31.ftp 6 next)
		(check-message test31.ftp 13 replies 221 500)
		(check-message test31.ftp 2 minimum TRUE)
		(check-message test31.ftp 6 minimum TRUE)
		(check-message test31.ftp 9 minimum FALSE)
		(check-message test31.ftp 2 group access-control)
		(check-message test31.ftp 9 group transfer-parameter)
		(check-message test31.ftp 6 group service)
		(check-message test31.ftp 2 standard 959)
		(check-message test31.ftp 1 standard 959)
		(check-message test31.ftp 1 meaning "Service ready for new user.")
		(check-message test31.ftp 12 meaning "Requested action not taken.")
		; the transfer parameters
		(check-message test32.ftp 1 type-code ascii non-print)
		(check-message test32.ftp 1 valid TRUE)
		(check-message test32.ftp 1 minimum TRUE)
		(check-query test32.ftp 2 answers TYPE TRUE)
		(check-query test32.ftp 2 outcome TYPE success)
		(check-message test32.ftp 3 type-code local 36)
		(check-message test32.ftp 3 minimum FALSE)
		(check-message test32.ftp 4 structure-code record)
		(check-message test32.ftp 4 minimum TRUE)
		(check-message test32.ftp 5 mode-code block)
		(check-message test32.ftp 5 minimum FALSE)
		(check-message test32.ftp 5 structure-code FALSE)
		(check-message test32.ftp 6 allocation 1024 128)
		(check-message test32.ftp 6 valid TRUE)
		(check-message test32.ftp 7 marker "1234")
		(check-message test32.ftp 7 valid TRUE)
		(check-message test32.ftp 7 next RETR STOR APPE)
		(check-message test32.ftp 8 restart-markers "1234" "5678")
		(check-query test32.ftp 8 answers RETR TRUE)
		(check-query test32.ftp 8 outcome REST error)
		(check-message test32.ftp 9 delay 5)
		(check-query test32.ftp 9 outcome CONNECTION wait)
		(check-message test32.ftp 10 system UNIX)
		(check-message test32.ftp 10 restart-markers)
		(check-message test32.ftp 11 valid FALSE)
		(check-message test32.ftp 11 host-port "300.0.0.1" 1025)
		(check-message test32.ftp 12 type-code)
		(check-message test32.ftp 12 valid FALSE)
		(check-message test32.ftp 13 pathname "a")
		(check-query test32.ftp 14 outcome RNFR continue)
		(check-query test32.ftp 14 answers RNFR TRUE)
		(check-query test32.ftp 16 outcome RNTO success)
		(check-query test32.ftp 16 answers RNTO TRUE)
		(check-message test32.ftp 16 standard 959)
		(check-message test25.ftp 1 standard 2228)
		(check-message test25.ftp 1 group access-control)
		(check-message test25.ftp 1 valid TRUE)
		(check-message test25.ftp 2 standard 2228)
		(check-query test25.ftp 2 outcome AUTH unknown)
		(check-message test25.ftp 7 meaning "Integrity protected reply.")
		(check-message test7.ftp 3 valid TRUE)
		(check-message test7.ftp 2 standard 2428)
		(check-message test7.ftp 2 meaning "Entering Extended Passive Mode (|||port|).")
		(check-message test13.ftp 2 pathname "-la /pub/some dir")
		(check-message test13.ftp 5 valid TRUE)
		(check-message test21.ftp 3 standard FALSE)
		(check-message test21.ftp 3 group unknown)
		(check-message test21.ftp 3 valid FALSE)
		(check-message test21.ftp 3 replies)
		(check-message test18.ftp 2 valid TRUE)
		(check-message test18.ftp 3 meaning "Command okay.")
		(check-message test6.ftp 3 valid TRUE)
		(check-message test6.ftp 1 valid TRUE)
		(check-query test6.ftp 2 outcome PASV success)
		(check-message test2.ftp 4 system UNIX)
		(check-message test2.ftp 1 delay FALSE)
		(check-message test2.ftp 5 meaning "\"PATHNAME\" created.")
		(check-query test4.ftp 1 answers FEAT TRUE)
		(check-query test4.ftp 1 outcome FEAT success)
		; the data connection
		(check-count data1.ftp 1)
		(check-class data1.ftp 1 FTP-RECORD)
		(check-message data1.ftp 1 get-octets 104 101 108 108 111 13 10 119 111 114 108 100)
		(check-message data1.ftp 1 get-eor FALSE)
		(check-message data1.ftp 1 get-eof TRUE)
		(check-message data1.ftp 1 get-type image)
		(check-message data1.ftp 1 get-structure file)
		(check-message data1.ftp 1 get-mode stream)
		(check-text data1.ftp 1 text "hello" 13 10 "world")
		(check-message data1.ftp 1 lines "hello" "world")
		(check-count data2.ftp 2)
		(check-message data2.ftp 1 get-octets 97 98)
		(check-message data2.ftp 1 get-eor TRUE)
		(check-message data2.ftp 1 get-eof FALSE)
		(check-message data2.ftp 2 get-octets 99 255 100)
		(check-message data2.ftp 2 get-eor TRUE)
		(check-message data2.ftp 2 get-eof TRUE)
		(check-count data3.ftp 2)
		(check-message data3.ftp 2 get-octets)
		(check-message data3.ftp 2 get-eof TRUE)
		(check-count data4.ftp 3)
		(check-class data4.ftp 2 FTP-MARKER)
		(check-message data4.ftp 2 get-marker "M001")
		(check-message data4.ftp 2 get-mode block)
		(check-message data4.ftp 3 get-octets 100 101)
		(check-message data4.ftp 3 get-eor TRUE)
		(check-message data4.ftp 3 get-eof TRUE)
		(check-count data5.ftp 1)
		(check-text data5.ftp 1 text "abcde")
		(check-message data5.ftp 1 get-suspect FALSE)
		(check-count data6.ftp 1)
		(check-message data6.ftp 1 get-suspect TRUE)
		(check-message data6.ftp 1 get-eor TRUE)
		(check-count data7.ftp 1)
		(check-text data7.ftp 1 text "abc    xxxz")
		(check-message data7.ftp 1 get-eof TRUE)
		(check-count data8.ftp 2)
		(check-text data8.ftp 1 text "abc")
		(check-message data8.ftp 1 get-eor TRUE)
		(check-text data8.ftp 2 text "de")
		(check-message data8.ftp 2 get-eof TRUE)
		(check-count data9.ftp 2)
		(check-class data9.ftp 1 FTP-MARKER)
		(check-message data9.ftp 1 get-marker "M002")
		(check-text data9.ftp 2 text "hi!")
		(check-count data10.ftp 3)
		(check-class data10.ftp 1 FTP-PAGE)
		(check-message data10.ftp 1 get-index 1)
		(check-message data10.ftp 1 kind simple)
		(check-message data10.ftp 1 get-octets 97 98 99)
		(check-message data10.ftp 1 get-eof FALSE)
		(check-message data10.ftp 2 get-index 2)
		(check-message data10.ftp 2 kind access-controlled)
		(check-message data10.ftp 2 get-header 7)
		(check-message data10.ftp 2 get-octets 100 101)
		(check-message data10.ftp 3 kind last)
		(check-message data10.ftp 3 get-page-type 0)
		(check-message data10.ftp 3 get-eof TRUE)
		(check-message data10.ftp 3 get-octets)
		(check-count data11.ftp 1)
		(check-message data11.ftp 1 get-octets 2748 3567 291)
		(check-message data11.ftp 1 get-byte-size 12)
		(check-count data12.ftp 2)
		(check-message data12.ftp 1 kind simple)
		(check-message data12.ftp 2 kind last)
		(check-count data13.ftp 1)
		(check-message data13.ftp 1 get-octets)
		(check-message data13.ftp 1 get-eof TRUE)
		(check-message data13.ftp 1 lines)
		(check-count data14.ftp 1)
		(check-text data14.ftp 1 text "ab")
		(check-message data14.ftp 1 get-eof TRUE)
		(check-count data15.ftp 1)
		(check-message data15.ftp 1 get-form carriage-control)
		(check-message data15.ftp 1 lines "1Title" " Line" "0Skip" "+Over")
		(check-message data15.ftp 1 carriage-controls new-page one-line two-lines overprint)
		(check-count data16.ftp 1)
		(check-text data16.ftp 1 text "abc")
		(check-message data16.ftp 1 get-eor TRUE)
		(check-message data16.ftp 1 get-eof FALSE)
		(check-count data17.ftp 1)
		(check-text data17.ftp 1 text "abc")
		(check-message data17.ftp 1 get-eof TRUE)
		(check-count data18.ftp 1)
		(check-message data18.ftp 1 get-octets 64 64 64 193)
		(check-count data19.ftp 2)
		(check-text data19.ftp 2 text "cd")
		(check-message data19.ftp 2 get-eor FALSE)
		(check-message data19.ftp 2 get-eof TRUE)
		(check-count data20.ftp 2)
		(check-message data20.ftp 1 get-octets 2748 3567)
		(check-message data20.ftp 1 get-eor TRUE)
		(check-message data20.ftp 2 get-octets)
		(check-message data20.ftp 2 get-eof TRUE)
		(check-count data21.ftp 1)
		(check-text data21.ftp 1 text "a")
		(check-count data22.ftp 2)
		(check-message data22.ftp 1 get-octets)
		(check-message data22.ftp 1 kind simple)
		(check-count data23.ftp 1)
		(check-text data23.ftp 1 text "ab")
		; a stream that ends in a message: the codec gives the complete messages before the cut, then ftp-eof
		(check-count test33.ftp 1)
		(check-count test34.ftp 1)
		(check-count test35.ftp 0)
		(check-count test36.ftp 1)
		(check-count data24.ftp 0)
		(check-count data25.ftp 0)
		(check-count data26.ftp 0)
		(check-count data27.ftp 1)
		(check-count data28.ftp 1)
		; the encoders
		(check-pprint (make-instance of FTP-RECORD (octets 97 98 13 10)) "ab" 13 10)
		(check-pprint (make-instance of FTP-RECORD (structure record) (octets 97 255) (eor TRUE)) "a" 255 255 255 1)
		(check-pprint (make-instance of FTP-RECORD (structure record) (eof TRUE)) 255 2)
		(check-pprint (make-instance of FTP-RECORD (structure record) (octets 97) (eor TRUE) (eof TRUE)) "a" 255 3)
		(check-pprint (make-instance of FTP-RECORD (mode block) (structure record) (octets 97 98) (eor TRUE)) 128 0 2 "ab")
		(check-pprint (make-instance of FTP-RECORD (mode block) (octets 97) (eof TRUE) (suspect TRUE)) 96 0 1 "a")
		(check-pprint (make-instance of FTP-MARKER (mode block) (marker "M1")) 16 0 2 "M1")
		(check-pprint (make-instance of FTP-MARKER (mode compressed) (marker "M1")) 0 16 2 "M1")
		(check-pprint (make-instance of FTP-MARKER (mode stream) (marker "M1")))
		(check-pprint (make-instance of FTP-RECORD (mode compressed) (octets 97 98 32 32 32 120 120 120 120 121) (eof TRUE))
			2 "ab" 195 132 "x" 0 64 1 "y")
		(check-pprint (make-instance of FTP-RECORD (mode compressed) (type image) (octets 0 0 0 0 0) (eor TRUE)) 0 128 197)
		(check-pprint (make-instance of FTP-RECORD (mode compressed) (type ebcdic) (octets 64 64 193)) 194 1 193)
		(check-pprint (make-instance of FTP-RECORD (type local) (byte-size 12) (octets 2748 3567 291)) 171 205 239 18 48)
		(check-pprint (make-instance of FTP-RECORD (type local) (byte-size 36) (mode block) (octets 1) (eof TRUE)) 64 0 5 0 0 0 0 16)
		(check-pprint (make-instance of FTP-PAGE (structure page) (index 3) (page-type 1) (octets 97 98)) 4 3 2 1 "ab")
		(check-pprint (make-instance of FTP-PAGE (structure page) (index 0) (page-type 0) (eof TRUE)) 4 0 0 0)
		(check-pprint (make-instance of FTP-PAGE (structure page) (mode block) (index 1) (page-type 3) (header 9) (octets 97) (eof TRUE))
			64 0 6 5 1 1 3 9 "a")))
	(bind ?failed 0)
	(foreach ?result ?results (bind ?failed (+ ?failed ?result)))
	(create$ (length$ ?results) ?failed))

;;; Decodes every case and expected file at once under one strategy.  Then
;;; checks each case and the values.  Returns the number of cases that
;;; failed, the number of value checks, and the number of those that failed.
(deffunction run-strategy (?strategy ?files)
	(bind ?*strategy* ?strategy)
	(set-strategy ?strategy)
	(decode-all ?files)
	(bind ?failed 0)
	(foreach ?file ?files
		(if (not (check-case ?file)) then
			(bind ?failed (+ ?failed 1))))
	(create$ ?failed (run-value-tests)))

;;; Runs every strategy.  Reports only the failures, a one line summary for
;;; the cases and one for the values.  Exits with 1 on a failure, so make
;;; test and CI fail.
(deffunction run-tests (?files)
	(bind ?failed 0)
	(bind ?values 0)
	(bind ?values-failed 0)
	(foreach ?strategy ?*strategies*
		(bind ?counts (run-strategy ?strategy ?files))
		(bind ?failed (+ ?failed (nth$ 1 ?counts)))
		(bind ?values (nth$ 2 ?counts))
		(bind ?values-failed (+ ?values-failed (nth$ 3 ?counts))))
	(bind ?strategies (length$ ?*strategies*))
	(bind ?total (* (length$ ?files) ?strategies))
	(println (length$ ?files) " cases x " ?strategies " strategies = "
	         ?total " tests, " (- ?total ?failed) " passed, " ?failed " failed")
	(bind ?total (* ?values ?strategies))
	(println ?values " value tests x " ?strategies " strategies = "
	         ?total " tests, " (- ?total ?values-failed) " passed, " ?values-failed " failed")
	(if (> (+ ?failed ?values-failed) 0) then (exit 1) else (exit 0)))

(run-tests (create$
test.ftp
test2.ftp
test3.ftp
test4.ftp
test5.ftp
test6.ftp
test7.ftp
test8.ftp
test9.ftp
test10.ftp
test11.ftp
test12.ftp
test13.ftp
test14.ftp
test15.ftp
test16.ftp
test17.ftp
test18.ftp
test19.ftp
test20.ftp
test21.ftp
test22.ftp
test23.ftp
test24.ftp
test25.ftp
test26.ftp
test27.ftp
test28.ftp
test29.ftp
test30.ftp
test31.ftp
test32.ftp
test33.ftp
test34.ftp
test35.ftp
test36.ftp
data1.ftp
data2.ftp
data3.ftp
data4.ftp
data5.ftp
data6.ftp
data7.ftp
data8.ftp
data9.ftp
data10.ftp
data11.ftp
data12.ftp
data13.ftp
data14.ftp
data15.ftp
data16.ftp
data17.ftp
data18.ftp
data19.ftp
data20.ftp
data21.ftp
data22.ftp
data23.ftp
data24.ftp
data25.ftp
data26.ftp
data27.ftp
data28.ftp
))
