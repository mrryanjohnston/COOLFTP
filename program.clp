; A codec for the File Transfer Protocol of RFC 959
; (https://www.rfc-editor.org/rfc/rfc959), with RFC 2228, RFC 2389,
; RFC 2428 and RFC 3659.
;
; The rules read one octet at a time.  On the control connection, each
; command line makes an FTP-COMMAND.  Each reply makes an FTP-REPLY.  On
; the data connection, each record makes an FTP-RECORD.  Each page makes
; an FTP-PAGE.  Each restart marker makes an FTP-MARKER.  The pprint
; handlers write the instances to a stream.  Refer to API.md for the
; facts and the instances.

;;; ------------------------------------------------------------------
;;; the messages
;;; ------------------------------------------------------------------

(defclass FTP-MESSAGE (is-a USER)
	(slot id (visibility public))
	; the position in the stream, from 1
	(slot sequence (default 0) (visibility public)))

;;; a command line: a verb, one space and an argument
(defclass FTP-COMMAND (is-a FTP-MESSAGE)
	; an upper case symbol: USER, PASS, RETR
	(slot verb (type SYMBOL) (default NOOP))
	; the text after the first space, or ""
	(slot argument (type STRING) (default "")))

;;; a reply: a three digit code and its text.  A multi-line reply keeps
;;; the lines before the last in `lines`.  The first is the text after
;;; "xyz-".  The last line is `text`.
(defclass FTP-REPLY (is-a FTP-MESSAGE)
	(slot code (type INTEGER) (default 200))
	(multislot lines (type STRING))
	(slot text (type STRING) (default "")))

;;; the data of the data connection.  The default transfer parameters
;;; are those of RFC 959 5.1: TYPE A N, STRU F, MODE S.  pprint encodes
;;; the message with them.
(defclass FTP-DATA (is-a FTP-MESSAGE)
	; ascii, ebcdic, image or local
	(slot type (type SYMBOL) (default ascii) (visibility public))
	; non-print, telnet or carriage-control
	(slot form (type SYMBOL) (default non-print) (visibility public))
	; the bits in a logical byte: the n of TYPE L n
	(slot byte-size (type INTEGER) (default 8) (visibility public))
	; file, record or page
	(slot structure (type SYMBOL) (default file) (visibility public))
	; stream, block or compressed
	(slot mode (type SYMBOL) (default stream) (visibility public)))

;;; a record: its logical bytes, as integers.  A file structure makes
;;; one record of the whole file.
(defclass FTP-RECORD (is-a FTP-DATA)
	(multislot octets (type INTEGER) (visibility public))
	; TRUE if an end-of-record ended it
	(slot eor (default FALSE) (visibility public))
	; TRUE if an end-of-file or the close of the connection ended it
	(slot eof (default FALSE) (visibility public))
	; TRUE if a block or string of the record had the suspect code
	(slot suspect (default FALSE) (visibility public)))

;;; a page of a page structure.  The record is its data.
(defclass FTP-PAGE (is-a FTP-RECORD)
	(slot index (type INTEGER) (default 0))
	; 0 last page, 1 simple, 2 descriptor, 3 access controlled
	(slot page-type (type INTEGER) (default 1))
	; the optional header fields, as logical bytes
	(multislot header (type INTEGER)))

;;; a restart marker of block or compressed mode
(defclass FTP-MARKER (is-a FTP-DATA)
	(slot marker (type STRING) (default "")))

;;; ------------------------------------------------------------------
;;; the state of a parse
;;; ------------------------------------------------------------------

;;; the parser state of one stream
(deftemplate ftp-parser
	(slot name)
	; control or data
	(slot connection (default control))
	(slot state (default start))
	; the number of messages read
	(slot sequence (default 0))
	; the verb, code or line in progress
	(slot text (default ""))
	; the verb of the command in progress, set when its argument starts
	(slot verb (default ""))
	; the reply code digits, as character codes
	(slot d1 (default 0))
	(slot d2 (default 0))
	(slot d3 (default 0))
	; complete lines of a multi-line reply
	(multislot lines)
	; the state after a Telnet command
	(slot next (default start))
	; the transfer parameters of a data connection
	(slot type (default ascii))
	(slot form (default non-print))
	(slot byte-size (default 8))
	(slot structure (default file))
	(slot mode (default stream))
	; data if a transfer byte is a logical byte.  transport if the codec
	; unpacks logical bytes from transfer bytes.
	(slot byte-kind (default data))
	; the filler byte of compressed mode
	(slot filler (default 32))
	; the logical bytes of the record or page in progress
	(multislot octets)
	; transfer bytes left in the block or string in progress
	(slot count (default 0))
	; the descriptor code of the block or string in progress
	(slot descriptor (default 0))
	; TRUE if a block or string of the record had the suspect code
	(slot suspect (default FALSE))
	; the restart marker in progress
	(slot marker (default ""))
	; the byte a compressed string replicates
	(slot repeat (default 0))
	; transfer bits that are not yet a logical byte, and their count
	(slot bits (default 0))
	(slot bit-count (default 0))
	; the page structure: its state, the header fields read, and the
	; logical bytes left in the header or the data
	(slot page-state (default page-length))
	(slot page-length (default 0))
	(slot page-index (default 0))
	(slot page-data-length (default 0))
	(slot page-type (default 1))
	(multislot page-header)
	(slot page-count (default 0)))

;;; ------------------------------------------------------------------
;;; helpers.  They have no conditions.  The rules decide.
;;; ------------------------------------------------------------------

(deffunction ftp-char-string (?char)
	(format nil "%c" ?char))

(deffunction ftp-append-char (?string ?char)
	(format nil "%s%c" ?string ?char))

(deffunction ftp-line-break ()
	"CR LF: the end of each line"
	(format nil "%c%c" 13 10))

(deffunction ftp-flag (?descriptor ?code)
	"TRUE if the descriptor has the code: 128 EOR, 64 EOF, 32 suspect, 16 marker"
	(= 1 (mod (div ?descriptor ?code) 2)))

(deffunction ftp-power-of-two (?n)
	(bind ?result 1)
	(loop-for-count ?n (bind ?result (* ?result 2)))
	?result)

(deffunction ftp-filler (?type)
	"the filler byte of compressed mode: a space in ASCII and EBCDIC, zero in image and local"
	(nth$ (member$ ?type (create$ ascii ebcdic image local)) (create$ 32 64 0 0)))

(deffunction ftp-byte-kind (?size)
	"data if the transfer byte is the logical byte.  If not, transport."
	(nth$ (+ 1 (min 1 (abs (- ?size 8)))) (create$ data transport)))

(deffunction ftp-initial-state (?mode)
	"the first state of a data connection in the mode"
	(nth$ (member$ ?mode (create$ stream block compressed))
	      (create$ stream block-descriptor compressed-code)))

(deffunction ftp-ends-data (?structure ?descriptor)
	"TRUE if the descriptor ends a record of the structure: EOF, or EOR of a record structure"
	(or (and (eq ?structure record) (ftp-flag ?descriptor 128))
	    (and (neq ?structure page) (ftp-flag ?descriptor 64))))

;;; the start fact.  Assert it with the logical name of an open stream
;;; and an id for the results.  Then run.  A data connection also gives
;;; its transfer parameters.  The defaults are those of RFC 959.
(deftemplate ftp-parse
	(slot stream)
	(slot id)
	(slot connection (default control) (allowed-symbols control data))
	(slot type (default ascii) (allowed-symbols ascii ebcdic image local))
	(slot form (default non-print) (allowed-symbols non-print telnet carriage-control))
	(slot byte-size (default 8) (type INTEGER) (range 1 56))
	(slot structure (default file) (allowed-symbols file record page))
	(slot mode (default stream) (allowed-symbols stream block compressed)))

;;; the codec reads only while a ready fact for the id exists.  A complete
;;; message retracts it.  Assert it again until ftp-eof.
(deftemplate ftp-ready
	(slot id))

;;; a stream the codec reads: its id, logical name and parameters
(deftemplate ftp-stream
	(slot id)
	(slot stream)
	(slot connection (default control))
	(slot type (default ascii))
	(slot form (default non-print))
	(slot byte-size (default 8))
	(slot structure (default file))
	(slot mode (default stream)))

;;; the end facts: one for each message, and one at the end of the stream.
;;; At the end of the stream, the codec changes no other fact: the parts
;;; of a cut message stay.  The program that matches these facts must
;;; retract them.
(deftemplate ftp-done
	(slot id)
	(slot message))

(deftemplate ftp-eof
	(slot id))

(defrule ftp-start
	?p <- (ftp-parse (stream ?stream) (id ?name) (connection ?connection) (type ?type)
	                 (form ?form) (byte-size ?size) (structure ?structure) (mode ?mode))
	=>
	(retract ?p)
	(assert (ftp-stream (id ?name) (stream ?stream) (connection ?connection) (type ?type)
	                    (form ?form) (byte-size ?size) (structure ?structure) (mode ?mode))))

(defrule ftp-initialize-parser
	(ftp-stream (id ?name) (connection control))
	(not (ftp-eof (id ?name)))
	(not (ftp-parser (name ?name)))
	=>
	(assert (ftp-parser (name ?name))))

(defrule ftp-initialize-data-parser
	(ftp-stream (id ?name) (connection data) (type ?type) (form ?form) (byte-size ?size)
	            (structure ?structure) (mode ?mode))
	(not (ftp-eof (id ?name)))
	(not (ftp-parser (name ?name)))
	=>
	(assert (ftp-parser (name ?name) (connection data) (state (ftp-initial-state ?mode))
	                    (type ?type) (form ?form) (byte-size ?size) (structure ?structure) (mode ?mode)
	                    (byte-kind (ftp-byte-kind ?size)) (filler (ftp-filler ?type)))))

;;; The codec reads a character only if nothing waits: no character, no
;;; transfer byte, no descriptor code, no logical byte to unpack, and no
;;; compressed string to expand.
(defrule ftp-get-next-char
	(ftp-ready (id ?name))
	(not (ftp-eof (id ?name)))
	(ftp-stream (id ?name) (stream ?stream))
	(ftp-parser (name ?name) (state ~compressed-repeat&~compressed-filler&~compressed-end)
	            (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-char ?name ?))
	(not (ftp-byte ?name ? ?))
	(not (ftp-signal ?name ?))
	=>
	(assert (ftp-char ?name (get-char ?stream))))

(deffunction ftp-message-done (?p ?m)
	"asserts ftp-done for a complete message"
	(assert (ftp-done (id (fact-slot-value ?p name)) (message ?m)))
	?m)

(deffunction ftp-command-instance (?p ?verb ?argument)
	"makes the FTP-COMMAND"
	(ftp-message-done ?p (make-instance of FTP-COMMAND
		(id (fact-slot-value ?p name))
		(sequence (+ 1 (fact-slot-value ?p sequence)))
		(verb (upcase (sym-cat ?verb)))
		(argument ?argument))))

(deffunction ftp-reply-instance (?p ?text)
	"makes the FTP-REPLY with last line ?text"
	(ftp-message-done ?p (make-instance of FTP-REPLY
		(id (fact-slot-value ?p name))
		(sequence (+ 1 (fact-slot-value ?p sequence)))
		(code (+ (* 100 (- (fact-slot-value ?p d1) 48))
		         (* 10 (- (fact-slot-value ?p d2) 48))
		         (- (fact-slot-value ?p d3) 48)))
		(lines (fact-slot-value ?p lines))
		(text ?text))))

(deffunction ftp-record-instance (?p ?octets ?eor ?eof ?suspect)
	"makes the FTP-RECORD with the transfer parameters of the parser"
	(ftp-message-done ?p (make-instance of FTP-RECORD
		(id (fact-slot-value ?p name))
		(sequence (+ 1 (fact-slot-value ?p sequence)))
		(type (fact-slot-value ?p type))
		(form (fact-slot-value ?p form))
		(byte-size (fact-slot-value ?p byte-size))
		(structure (fact-slot-value ?p structure))
		(mode (fact-slot-value ?p mode))
		(octets ?octets)
		(eor ?eor)
		(eof ?eof)
		(suspect ?suspect))))

(deffunction ftp-page-instance (?p ?page-type ?header ?octets)
	"makes the FTP-PAGE.  A last page, type 0, is the end of file."
	(ftp-message-done ?p (make-instance of FTP-PAGE
		(id (fact-slot-value ?p name))
		(sequence (+ 1 (fact-slot-value ?p sequence)))
		(type (fact-slot-value ?p type))
		(form (fact-slot-value ?p form))
		(byte-size (fact-slot-value ?p byte-size))
		(structure (fact-slot-value ?p structure))
		(mode (fact-slot-value ?p mode))
		(index (fact-slot-value ?p page-index))
		(page-type ?page-type)
		(header ?header)
		(octets ?octets)
		(eof (= ?page-type 0))
		(suspect (fact-slot-value ?p suspect)))))

(deffunction ftp-marker-instance (?p)
	"makes the FTP-MARKER"
	(ftp-message-done ?p (make-instance of FTP-MARKER
		(id (fact-slot-value ?p name))
		(sequence (+ 1 (fact-slot-value ?p sequence)))
		(type (fact-slot-value ?p type))
		(form (fact-slot-value ?p form))
		(byte-size (fact-slot-value ?p byte-size))
		(structure (fact-slot-value ?p structure))
		(mode (fact-slot-value ?p mode))
		(marker (fact-slot-value ?p marker)))))

;;; ------------------------------------------------------------------
;;; reading the control connection
;;; ------------------------------------------------------------------

;;;; CR is never text.  Lines end at LF.

(defrule ftp-cr
	?c <- (ftp-char ?name 13)
	(ftp-parser (name ?name) (connection control) (state ~iac&~iac-option))
	=>
	(retract ?c))

;;;; Telnet commands (RFC 854).  The codec drops IAC and the octet after
;;;; it.  After WILL, WONT, DO and DONT, it also drops the option octet.

(defrule ftp-iac
	?c <- (ftp-char ?name 255)
	?p <- (ftp-parser (name ?name) (connection control) (state ?state&~iac&~iac-option))
	=>
	(retract ?c)
	(modify ?p (state iac) (next ?state)))

(defrule ftp-iac-command
	?c <- (ftp-char ?name ~251&~252&~253&~254&~-1)
	?p <- (ftp-parser (name ?name) (state iac) (next ?next))
	=>
	(retract ?c)
	(modify ?p (state ?next)))

(defrule ftp-iac-negotiate
	?c <- (ftp-char ?name 251|252|253|254)
	?p <- (ftp-parser (name ?name) (state iac))
	=>
	(retract ?c)
	(modify ?p (state iac-option)))

(defrule ftp-iac-option
	?c <- (ftp-char ?name ~-1)
	?p <- (ftp-parser (name ?name) (state iac-option) (next ?next))
	=>
	(retract ?c)
	(modify ?p (state ?next)))

;;;; the start of a message: a digit starts a reply code.  Each other
;;;; character starts a verb.  The codec drops blank lines and leading
;;;; spaces.

;;;; the end of the stream: get-char gave -1.  Between messages, the parser
;;;; waits in its start state.  In a message, the line has no LF, so it is
;;;; not a message.  On a data connection, the close is the end of file only
;;;; in stream mode with file structure (RFC 959 3.4.1).  The two rules at
;;;; the end of the data section make that record first.  In all other
;;;; conditions, the codec asserts ftp-eof and changes no other fact.  The
;;;; parser and the parts of a cut message stay.  The program decides what
;;;; occurs then.

(defrule ftp-end-of-stream
	?c <- (ftp-char ?name -1)
	?r <- (ftp-ready (id ?name))
	(not (ftp-parser (name ?name) (state stream) (structure file) (octets ? $?)))
	(not (ftp-parser (name ?name) (state stream) (structure file) (sequence 0)))
	=>
	(retract ?c ?r)
	(assert (ftp-eof (id ?name))))

(defrule ftp-start-newline
	?c <- (ftp-char ?name 10)
	(ftp-parser (name ?name) (state start))
	=>
	(retract ?c))

(defrule ftp-start-space
	?c <- (ftp-char ?name 32)
	(ftp-parser (name ?name) (state start))
	=>
	(retract ?c))

(defrule ftp-start-digit
	?c <- (ftp-char ?name ?char&48|49|50|51|52|53|54|55|56|57)
	?p <- (ftp-parser (name ?name) (state start))
	=>
	(retract ?c)
	(modify ?p (state code1) (text (ftp-char-string ?char)) (d1 ?char)))

(defrule ftp-start-char
	?c <- (ftp-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~32&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state start))
	=>
	(retract ?c)
	(modify ?p (state verb) (text (ftp-char-string ?char))))

;;;; a reply code: three digits, then a space for a single line reply or a
;;;; hyphen for a multi-line reply.  Digits that do not make a reply code
;;;; start a verb.

(defrule ftp-code1-digit
	?c <- (ftp-char ?name ?char&48|49|50|51|52|53|54|55|56|57)
	?p <- (ftp-parser (name ?name) (state code1) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state code2) (text (ftp-append-char ?text ?char)) (d2 ?char)))

(defrule ftp-code2-digit
	?c <- (ftp-char ?name ?char&48|49|50|51|52|53|54|55|56|57)
	?p <- (ftp-parser (name ?name) (state code2) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state code3) (text (ftp-append-char ?text ?char)) (d3 ?char)))

(defrule ftp-code-char
	"fewer than three digits, then another character: a verb"
	?c <- (ftp-char ?name ?char&~48&~49&~50&~51&~52&~53&~54&~55&~56&~57&~32&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state code1|code2) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state verb) (text (ftp-append-char ?text ?char))))

(defrule ftp-code3-space
	?c <- (ftp-char ?name 32)
	?p <- (ftp-parser (name ?name) (state code3))
	=>
	(retract ?c)
	(modify ?p (state reply-text) (text "")))

(defrule ftp-code3-hyphen
	?c <- (ftp-char ?name 45) ; -
	?p <- (ftp-parser (name ?name) (state code3))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text "")))

(defrule ftp-code3-end
	"a code alone on a line: a reply with no text"
	(ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state code3) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-reply-instance ?p "")
	(modify ?p (state line-end) (text "") (sequence (+ ?sequence 1))))

(defrule ftp-code3-char
	"three digits, then another character: a verb"
	?c <- (ftp-char ?name ?char&~32&~45&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state code3) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state verb) (text (ftp-append-char ?text ?char))))

;;;; a command: the verb ends at the first space.  The argument ends at the
;;;; end of the line.

(defrule ftp-verb-char
	?c <- (ftp-char ?name ?char&~32&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state verb) (text ?text))
	=>
	(retract ?c)
	(modify ?p (text (ftp-append-char ?text ?char))))

(defrule ftp-verb-space
	?c <- (ftp-char ?name 32)
	?p <- (ftp-parser (name ?name) (state verb|code1|code2) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state argument) (verb ?text) (text "")))

(defrule ftp-verb-end
	(ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state verb|code1|code2) (text ?text) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-command-instance ?p ?text "")
	(modify ?p (state line-end) (text "") (sequence (+ ?sequence 1))))

(defrule ftp-argument-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state argument) (text ?text))
	=>
	(retract ?c)
	(modify ?p (text (ftp-append-char ?text ?char))))

(defrule ftp-argument-end
	(ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state argument) (verb ?verb) (text ?text) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-command-instance ?p ?verb ?text)
	(modify ?p (state line-end) (verb "") (text "") (sequence (+ ?sequence 1))))

;;;; the last line of a reply.  A single line reply has only this line.

(defrule ftp-reply-text-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-text) (text ?text))
	=>
	(retract ?c)
	(modify ?p (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-text-end
	(ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state reply-text) (text ?text) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-reply-instance ?p ?text)
	(modify ?p (state line-end) (text "") (lines) (sequence (+ ?sequence 1))))

;;;; the middle lines of a multi-line reply.  The last line starts with
;;;; the code and a space.  The codec reads each line digit by digit until
;;;; it knows that the line is not the last.

(defrule ftp-reply-line-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-line) (text ?text))
	=>
	(retract ?c)
	(modify ?p (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-line-newline
	?c <- (ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state reply-line-start|reply-line|reply-end1|reply-end2|reply-end3)
	              (text ?text) (lines $?lines))
	=>
	(retract ?c)
	(modify ?p (state reply-line-start) (text "") (lines ?lines ?text)))

(defrule ftp-reply-line-start-digit
	?c <- (ftp-char ?name ?char)
	?p <- (ftp-parser (name ?name) (state reply-line-start) (d1 ?char))
	=>
	(retract ?c)
	(modify ?p (state reply-end1) (text (ftp-char-string ?char))))

(defrule ftp-reply-line-start-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-line-start) (d1 ~?char))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text (ftp-char-string ?char))))

(defrule ftp-reply-end1-digit
	?c <- (ftp-char ?name ?char)
	?p <- (ftp-parser (name ?name) (state reply-end1) (d2 ?char) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-end2) (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-end1-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-end1) (d2 ~?char) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-end2-digit
	?c <- (ftp-char ?name ?char)
	?p <- (ftp-parser (name ?name) (state reply-end2) (d3 ?char) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-end3) (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-end2-char
	?c <- (ftp-char ?name ?char&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-end2) (d3 ~?char) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text (ftp-append-char ?text ?char))))

(defrule ftp-reply-end3-space
	"the code and a space: the last line"
	?c <- (ftp-char ?name 32)
	?p <- (ftp-parser (name ?name) (state reply-end3))
	=>
	(retract ?c)
	(modify ?p (state reply-text) (text "")))

(defrule ftp-reply-end3-hyphen
	"the code and a hyphen: a middle line.  The codec keeps it as sent."
	?c <- (ftp-char ?name 45) ; -
	?p <- (ftp-parser (name ?name) (state reply-end3) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text (ftp-append-char ?text 45))))

(defrule ftp-reply-end3-char
	?c <- (ftp-char ?name ?char&~32&~45&~10&~13&~-1&~255)
	?p <- (ftp-parser (name ?name) (state reply-end3) (text ?text))
	=>
	(retract ?c)
	(modify ?p (state reply-line) (text (ftp-append-char ?text ?char))))

;;;; the end of a message.  The rule that made the message retracted the
;;;; ready fact.  The line break or the end of stream is the only character
;;;; left.  Nothing reads another character until the next ready fact.

(defrule ftp-line-end-newline
	?c <- (ftp-char ?name 10)
	?p <- (ftp-parser (name ?name) (state line-end))
	=>
	(retract ?c)
	(modify ?p (state start)))

;;; ------------------------------------------------------------------
;;; reading the data connection (RFC 959 3)
;;;
;;; There are three layers.  The transmission mode reads the stream.  It
;;; makes a transfer byte, (ftp-byte name byte kind), or a descriptor
;;; code, (ftp-signal name descriptor).  TYPE L n unpacks transfer bytes
;;; into logical bytes.  The structure takes the logical bytes and the
;;; codes.  It makes the records, the pages and the markers.
;;; ------------------------------------------------------------------

;;;; stream mode (3.4.1): each byte is data.  A record structure has a
;;;; two byte control code: 255, then 1 for EOR, 2 for EOF or 3 for both.
;;;; 255 255 is one 255.

(defrule ftp-stream-byte
	?c <- (ftp-char ?name ?byte&~-1)
	(ftp-parser (name ?name) (state stream) (structure file|page) (byte-kind ?kind))
	=>
	(retract ?c)
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-stream-record-byte
	?c <- (ftp-char ?name ?byte&~255&~-1)
	(ftp-parser (name ?name) (state stream) (structure record) (byte-kind ?kind))
	=>
	(retract ?c)
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-stream-record-escape
	?c <- (ftp-char ?name 255)
	?p <- (ftp-parser (name ?name) (state stream) (structure record))
	=>
	(retract ?c)
	(modify ?p (state stream-escape)))

(defrule ftp-stream-escape-escape
	"255 255: one 255 of data"
	?c <- (ftp-char ?name 255)
	?p <- (ftp-parser (name ?name) (state stream-escape) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (state stream))
	(assert (ftp-byte ?name 255 ?kind)))

(defrule ftp-stream-escape-control
	"1 is EOR, 2 is EOF, 3 is both: the descriptor codes 128, 64 and 192"
	?c <- (ftp-char ?name ?byte&1|2|3)
	?p <- (ftp-parser (name ?name) (state stream-escape))
	=>
	(retract ?c)
	(modify ?p (state stream))
	(assert (ftp-signal ?name (+ (* 128 (mod ?byte 2)) (* 64 (div ?byte 2))))))

(defrule ftp-stream-escape-other
	"each other byte after the escape is data.  The codec drops the escape."
	?c <- (ftp-char ?name ?byte&~1&~2&~3&~255&~-1)
	?p <- (ftp-parser (name ?name) (state stream-escape) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (state stream))
	(assert (ftp-byte ?name ?byte ?kind)))

;;;; block mode (3.4.2): a descriptor byte, a two byte count, then that
;;;; number of bytes.  The descriptor applies at the end of the block.  A
;;;; block with the marker code, 16, carries a restart marker.

(defrule ftp-block-descriptor
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-descriptor))
	=>
	(retract ?c)
	(modify ?p (state block-count-high) (descriptor ?byte)))

(defrule ftp-block-count-high
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-count-high))
	=>
	(retract ?c)
	(modify ?p (state block-count-low) (count (* 256 ?byte))))

(defrule ftp-block-count-low-data
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-count-low) (count ?count)
	                  (descriptor ?descriptor&:(not (ftp-flag ?descriptor 16))))
	(test (> (+ ?count ?byte) 0))
	=>
	(retract ?c)
	(modify ?p (state block-data) (count (+ ?count ?byte))))

(defrule ftp-block-count-low-marker
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-count-low) (count ?count)
	                  (descriptor ?descriptor&:(ftp-flag ?descriptor 16)))
	(test (> (+ ?count ?byte) 0))
	=>
	(retract ?c)
	(modify ?p (state block-marker) (count (+ ?count ?byte))))

(defrule ftp-block-count-low-empty
	"a block with no data: its descriptor applies at once"
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-count-low) (count ?count) (descriptor ?descriptor))
	(test (= (+ ?count ?byte) 0))
	=>
	(retract ?c)
	(modify ?p (state block-descriptor) (count 0) (descriptor 0))
	(assert (ftp-signal ?name ?descriptor)))

(defrule ftp-block-data-byte
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-data) (count ?count&:(> ?count 1)) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (count (- ?count 1)))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-block-data-last
	"the last byte of a plain block"
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-data) (count 1) (descriptor 0) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (state block-descriptor) (count 0))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-block-data-last-descriptor
	"the last byte of a block with a descriptor: the byte, then the codes"
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-data) (count 1) (descriptor ?descriptor&~0) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (state block-descriptor) (count 0) (descriptor 0))
	(assert (ftp-byte ?name ?byte ?kind) (ftp-signal ?name ?descriptor)))

(defrule ftp-block-marker-byte
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-marker) (count ?count&:(> ?count 1)) (marker ?marker))
	=>
	(retract ?c)
	(modify ?p (count (- ?count 1)) (marker (ftp-append-char ?marker ?byte))))

(defrule ftp-block-marker-last
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state block-marker) (count 1) (descriptor ?descriptor) (marker ?marker))
	=>
	(retract ?c)
	(modify ?p (state block-descriptor) (count 0) (descriptor 0) (marker (ftp-append-char ?marker ?byte)))
	(assert (ftp-signal ?name ?descriptor)))

;;;; compressed mode (3.4.3): strings.  A byte 1 to 127 is the count of
;;;; a byte string.  128 to 191 is 0 to 63 copies of the next byte.  192
;;;; to 255 is 0 to 63 filler bytes.  0 is an escape: the next byte is a
;;;; descriptor.  It applies to the string that follows.  The string after
;;;; a marker code is the restart marker.

(defrule ftp-compressed-escape
	?c <- (ftp-char ?name 0)
	?p <- (ftp-parser (name ?name) (state compressed-code))
	=>
	(retract ?c)
	(modify ?p (state compressed-escape)))

(defrule ftp-compressed-descriptor
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-escape))
	=>
	(retract ?c)
	(modify ?p (state compressed-code) (descriptor ?byte)))

(defrule ftp-compressed-string
	?c <- (ftp-char ?name ?byte&:(and (> ?byte 0) (< ?byte 128)))
	?p <- (ftp-parser (name ?name) (state compressed-code))
	=>
	(retract ?c)
	(modify ?p (state compressed-data) (count ?byte)))

(defrule ftp-compressed-replicate
	?c <- (ftp-char ?name ?byte&:(and (> ?byte 127) (< ?byte 192)))
	?p <- (ftp-parser (name ?name) (state compressed-code))
	=>
	(retract ?c)
	(modify ?p (state compressed-replicate-byte) (count (- ?byte 128))))

(defrule ftp-compressed-filler
	?c <- (ftp-char ?name ?byte&:(> ?byte 191))
	?p <- (ftp-parser (name ?name) (state compressed-code))
	=>
	(retract ?c)
	(modify ?p (state compressed-filler) (count (- ?byte 192))))

(defrule ftp-compressed-data-byte
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-data) (count ?count&:(> ?count 1))
	                  (descriptor ?descriptor&:(not (ftp-flag ?descriptor 16))) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (count (- ?count 1)))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-compressed-data-last
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-data) (count 1)
	                  (descriptor ?descriptor&:(not (ftp-flag ?descriptor 16))) (byte-kind ?kind))
	=>
	(retract ?c)
	(modify ?p (state compressed-end) (count 0))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-compressed-marker-byte
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-data) (count ?count&:(> ?count 1))
	                  (descriptor ?descriptor&:(ftp-flag ?descriptor 16)) (marker ?marker))
	=>
	(retract ?c)
	(modify ?p (count (- ?count 1)) (marker (ftp-append-char ?marker ?byte))))

(defrule ftp-compressed-marker-last
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-data) (count 1)
	                  (descriptor ?descriptor&:(ftp-flag ?descriptor 16)) (marker ?marker))
	=>
	(retract ?c)
	(modify ?p (state compressed-end) (count 0) (marker (ftp-append-char ?marker ?byte))))

(defrule ftp-compressed-replicate-byte
	?c <- (ftp-char ?name ?byte&~-1)
	?p <- (ftp-parser (name ?name) (state compressed-replicate-byte))
	=>
	(retract ?c)
	(modify ?p (state compressed-repeat) (repeat ?byte)))

;;;; the codec makes the copies and the filler bytes one at a time.  It
;;;; makes each one after the structure took the last.

(defrule ftp-compressed-repeat
	?p <- (ftp-parser (name ?name) (state compressed-repeat) (count ?count&:(> ?count 0)) (repeat ?byte)
	                  (byte-kind ?kind) (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (count (- ?count 1)))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-compressed-repeat-done
	?p <- (ftp-parser (name ?name) (state compressed-repeat) (count 0)
	                  (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (state compressed-end)))

(defrule ftp-compressed-fill
	?p <- (ftp-parser (name ?name) (state compressed-filler) (count ?count&:(> ?count 0)) (filler ?byte)
	                  (byte-kind ?kind) (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (count (- ?count 1)))
	(assert (ftp-byte ?name ?byte ?kind)))

(defrule ftp-compressed-fill-done
	?p <- (ftp-parser (name ?name) (state compressed-filler) (count 0)
	                  (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (state compressed-end)))

;;;; the end of a string: the descriptor of the escape before it applies

(defrule ftp-compressed-end
	?p <- (ftp-parser (name ?name) (state compressed-end) (descriptor 0)
	                  (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (state compressed-code)))

(defrule ftp-compressed-end-descriptor
	?p <- (ftp-parser (name ?name) (state compressed-end) (descriptor ?descriptor&~0)
	                  (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(modify ?p (state compressed-code) (descriptor 0))
	(assert (ftp-signal ?name ?descriptor)))

;;;; TYPE L n (3.1.1.4): logical bytes of n bits packed into transfer
;;;; bytes.  Each transfer byte adds eight bits.  The codec takes one
;;;; logical byte at a time.  The bits left at the end of a record or
;;;; page are padding.

(defrule ftp-bits-byte
	?b <- (ftp-byte ?name ?byte transport)
	?p <- (ftp-parser (name ?name) (bits ?bits) (bit-count ?count))
	=>
	(retract ?b)
	(modify ?p (bits (+ (* 256 ?bits) ?byte)) (bit-count (+ ?count 8))))

(defrule ftp-bits-logical-byte
	?p <- (ftp-parser (name ?name) (bits ?bits) (bit-count ?count) (byte-size ?size&:(>= ?count ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(bind ?rest (ftp-power-of-two (- ?count ?size)))
	(modify ?p (bits (mod ?bits ?rest)) (bit-count (- ?count ?size)))
	(assert (ftp-byte ?name (div ?bits ?rest) data)))

;;;; file and record structures (3.1.2.1, 3.1.2.2): each logical byte
;;;; goes into the record.  A descriptor code ends the record.

(defrule ftp-data-byte
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure file|record) (octets $?octets))
	=>
	(retract ?b)
	(modify ?p (octets ?octets ?byte)))

(defrule ftp-signal-marker
	"a restart marker alone"
	?s <- (ftp-signal ?name 16)
	?p <- (ftp-parser (name ?name) (sequence ?sequence) (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?s ?r)
	(ftp-marker-instance ?p)
	(modify ?p (marker "") (sequence (+ ?sequence 1))))

(defrule ftp-signal-marker-and-more
	"a restart marker with other codes: the marker, then the rest"
	?s <- (ftp-signal ?name ?descriptor&~16&:(ftp-flag ?descriptor 16))
	?p <- (ftp-parser (name ?name) (sequence ?sequence) (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?s ?r)
	(ftp-marker-instance ?p)
	(modify ?p (marker "") (sequence (+ ?sequence 1)))
	(assert (ftp-signal ?name (- ?descriptor 16))))

(defrule ftp-signal-end
	"EOR or EOF: the record is complete"
	?s <- (ftp-signal ?name ?descriptor&:(not (ftp-flag ?descriptor 16)))
	?p <- (ftp-parser (name ?name) (structure ?structure&:(ftp-ends-data ?structure ?descriptor))
	                  (octets $?octets) (suspect ?suspect) (sequence ?sequence)
	                  (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?s ?r)
	(ftp-record-instance ?p ?octets (ftp-flag ?descriptor 128) (ftp-flag ?descriptor 64)
	                     (or ?suspect (ftp-flag ?descriptor 32)))
	(modify ?p (octets) (suspect FALSE) (bits 0) (bit-count 0) (sequence (+ ?sequence 1))))

(defrule ftp-signal-other
	"no end code: only the suspect code applies"
	?s <- (ftp-signal ?name ?descriptor&:(not (ftp-flag ?descriptor 16)))
	?p <- (ftp-parser (name ?name) (structure ?structure&:(not (ftp-ends-data ?structure ?descriptor)))
	                  (suspect ?suspect) (bit-count ?bits) (byte-size ?size&:(< ?bits ?size)))
	(not (ftp-byte ?name ? ?))
	=>
	(retract ?s)
	(modify ?p (suspect (or ?suspect (ftp-flag ?descriptor 32)))))

;;;; page structure (3.1.2.3): a header of logical bytes, then the data.
;;;; The header is its length, the page index, the data length, the page
;;;; type, and the optional fields.  A last page, type 0, ends the file.

(defrule ftp-page-length
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-length))
	=>
	(retract ?b)
	(modify ?p (page-state page-index) (page-length ?byte)))

(defrule ftp-page-index
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-index))
	=>
	(retract ?b)
	(modify ?p (page-state page-data-length) (page-index ?byte)))

(defrule ftp-page-data-length
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-data-length))
	=>
	(retract ?b)
	(modify ?p (page-state page-type) (page-data-length ?byte)))

(defrule ftp-page-type-header
	"a header longer than four: optional fields follow"
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-type)
	                  (page-length ?length&:(> ?length 4)))
	=>
	(retract ?b)
	(modify ?p (page-state page-header) (page-type ?byte) (page-count (- ?length 4))))

(defrule ftp-page-type-data
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-type)
	                  (page-length ?length&:(<= ?length 4)) (page-data-length ?data&:(> ?data 0)))
	=>
	(retract ?b)
	(modify ?p (page-state page-data) (page-type ?byte) (page-count ?data)))

(defrule ftp-page-type-end
	"no optional fields and no data: the page is complete"
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-type)
	                  (page-length ?length&:(<= ?length 4)) (page-data-length 0) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?b ?r)
	(ftp-page-instance ?p ?byte (create$) (create$))
	(modify ?p (page-state page-length) (page-type 1) (suspect FALSE) (bits 0) (bit-count 0)
	           (sequence (+ ?sequence 1))))

(defrule ftp-page-header-byte
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-header)
	                  (page-count ?count&:(> ?count 1)) (page-header $?header))
	=>
	(retract ?b)
	(modify ?p (page-count (- ?count 1)) (page-header ?header ?byte)))

(defrule ftp-page-header-last-data
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-header) (page-count 1)
	                  (page-data-length ?data&:(> ?data 0)) (page-header $?header))
	=>
	(retract ?b)
	(modify ?p (page-state page-data) (page-count ?data) (page-header ?header ?byte)))

(defrule ftp-page-header-last-end
	"the last optional field and no data: the page is complete"
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-header) (page-count 1)
	                  (page-data-length 0) (page-type ?type) (page-header $?header) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?b ?r)
	(ftp-page-instance ?p ?type (create$ ?header ?byte) (create$))
	(modify ?p (page-state page-length) (page-type 1) (page-header) (page-count 0) (suspect FALSE)
	           (bits 0) (bit-count 0) (sequence (+ ?sequence 1))))

(defrule ftp-page-data-byte
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-data)
	                  (page-count ?count&:(> ?count 1)) (octets $?octets))
	=>
	(retract ?b)
	(modify ?p (page-count (- ?count 1)) (octets ?octets ?byte)))

(defrule ftp-page-data-last
	?b <- (ftp-byte ?name ?byte data)
	?p <- (ftp-parser (name ?name) (structure page) (page-state page-data) (page-count 1)
	                  (page-type ?type) (page-header $?header) (octets $?octets) (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?b ?r)
	(ftp-page-instance ?p ?type ?header (create$ ?octets ?byte))
	(modify ?p (page-state page-length) (page-type 1) (page-header) (page-count 0) (octets) (suspect FALSE)
	           (bits 0) (bit-count 0) (sequence (+ ?sequence 1))))

;;;; the close of the data connection in stream mode with file structure
;;;; is the end of file (RFC 959 3.4.1).  The data read is the record.
;;;; Then ftp-end-of-stream asserts ftp-eof at the next ready fact.  In
;;;; each other mode and structure, a descriptor code gives the end of
;;;; file.  The close in a record or a page is the end of the stream.

(defrule ftp-data-close-record
	"stream mode, file structure: the close is the end of file.  The data read is the record."
	(ftp-char ?name -1)
	?p <- (ftp-parser (name ?name) (state stream) (structure file) (octets ?first $?rest) (suspect ?suspect)
	                  (sequence ?sequence))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-record-instance ?p (create$ ?first ?rest) FALSE TRUE ?suspect)
	(modify ?p (octets) (suspect FALSE) (bits 0) (bit-count 0) (sequence (+ ?sequence 1))))

(defrule ftp-data-close-empty-file
	"stream mode, file structure: an empty stream is an empty file, one record with no data"
	(ftp-char ?name -1)
	?p <- (ftp-parser (name ?name) (state stream) (structure file) (octets) (sequence 0))
	?r <- (ftp-ready (id ?name))
	=>
	(retract ?r)
	(ftp-record-instance ?p (create$) FALSE TRUE FALSE)
	(modify ?p (bits 0) (bit-count 0) (sequence 1)))

;;; ------------------------------------------------------------------
;;; writing.  Rules do not call these handlers, so the handlers can branch.
;;; ------------------------------------------------------------------

;;; the verb, then a space and the argument if it is not empty, then CR LF
(defmessage-handler FTP-COMMAND pprint (?t)
	(printout ?t ?self:verb)
	(if (neq ?self:argument "") then (printout ?t " " ?self:argument))
	(printout ?t (ftp-line-break)))

;;; "xyz-" and the middle lines if there are some, then "xyz " and the
;;; text.  CR LF ends each line.
(defmessage-handler FTP-REPLY pprint (?t)
	(bind ?code (format nil "%03d" ?self:code))
	(if (> (length$ ?self:lines) 0) then
		(printout ?t ?code "-" (nth$ 1 ?self:lines) (ftp-line-break))
		(foreach ?line (rest$ ?self:lines)
			(printout ?t ?line (ftp-line-break))))
	(printout ?t ?code " " ?self:text (ftp-line-break)))

;;;; the data connection: the logical bytes packed into transfer bytes,
;;;; then the mode

(deffunction ftp-put-bytes (?t ?bytes)
	(foreach ?byte ?bytes (put-char ?t ?byte)))

(deffunction ftp-pack (?values ?size)
	"logical bytes of ?size bits packed into transfer bytes, with padding at the end"
	(if (= ?size 8) then (return ?values))
	(bind ?bytes (create$))
	(bind ?bits 0)
	(bind ?count 0)
	(foreach ?value ?values
		(bind ?bits (+ (* ?bits (ftp-power-of-two ?size)) ?value))
		(bind ?count (+ ?count ?size))
		(while (>= ?count 8) do
			(bind ?rest (ftp-power-of-two (- ?count 8)))
			(bind ?bytes (create$ ?bytes (div ?bits ?rest)))
			(bind ?bits (mod ?bits ?rest))
			(bind ?count (- ?count 8))))
	(if (> ?count 0) then
		(bind ?bytes (create$ ?bytes (* ?bits (ftp-power-of-two (- 8 ?count))))))
	?bytes)

(deffunction ftp-descriptor (?eor ?eof ?suspect)
	"the descriptor code of the flags"
	(+ (if ?eor then 128 else 0) (if ?eof then 64 else 0) (if ?suspect then 32 else 0)))

(deffunction ftp-put-stream (?t ?structure ?descriptor ?bytes)
	"stream mode: the bytes.  A record structure escapes 255 and ends with the control code."
	(if (neq ?structure record) then (return (ftp-put-bytes ?t ?bytes)))
	(foreach ?byte ?bytes
		(put-char ?t ?byte)
		(if (= ?byte 255) then (put-char ?t 255)))
	(bind ?code (+ (mod (div ?descriptor 128) 2) (* 2 (mod (div ?descriptor 64) 2))))
	(if (> ?code 0) then
		(put-char ?t 255)
		(put-char ?t ?code)))

(deffunction ftp-put-block (?t ?descriptor ?bytes)
	(put-char ?t ?descriptor)
	(put-char ?t (div (length$ ?bytes) 256))
	(put-char ?t (mod (length$ ?bytes) 256))
	(ftp-put-bytes ?t ?bytes))

(deffunction ftp-put-blocks (?t ?descriptor ?bytes)
	"block mode: blocks of 65535 bytes maximum.  Each block gets the suspect code.  The last block gets the other codes."
	(bind ?suspect (* 32 (mod (div ?descriptor 32) 2)))
	(while (> (length$ ?bytes) 65535) do
		(ftp-put-block ?t ?suspect (subseq$ ?bytes 1 65535))
		(bind ?bytes (subseq$ ?bytes 65536 (length$ ?bytes))))
	(ftp-put-block ?t ?descriptor ?bytes))

(deffunction ftp-compress (?bytes ?filler)
	"the strings of compressed mode: the start of the last string, then the bytes.  Two or more fillers make a filler string.  Three or more of one byte make a replicated byte.  The other bytes make byte strings."
	(bind ?out (create$))
	(bind ?last 1)
	(bind ?pending (create$))
	(bind ?i 1)
	(bind ?n (length$ ?bytes))
	(while (<= ?i ?n) do
		(bind ?byte (nth$ ?i ?bytes))
		(bind ?run 1)
		(while (and (<= (+ ?i ?run) ?n) (= (nth$ (+ ?i ?run) ?bytes) ?byte)) do
			(bind ?run (+ ?run 1)))
		(if (or (and (= ?byte ?filler) (>= ?run 2)) (>= ?run 3))
			then
				(while (> (length$ ?pending) 0) do
					(bind ?last (+ (length$ ?out) 1))
					(bind ?k (min 127 (length$ ?pending)))
					(bind ?out (create$ ?out ?k (subseq$ ?pending 1 ?k)))
					(bind ?pending (subseq$ ?pending (+ ?k 1) (length$ ?pending))))
				(bind ?left ?run)
				(while (> ?left 0) do
					(bind ?last (+ (length$ ?out) 1))
					(bind ?k (min 63 ?left))
					(if (= ?byte ?filler)
						then (bind ?out (create$ ?out (+ 192 ?k)))
						else (bind ?out (create$ ?out (+ 128 ?k) ?byte)))
					(bind ?left (- ?left ?k)))
				(bind ?i (+ ?i ?run))
			else
				(bind ?pending (create$ ?pending ?byte))
				(bind ?i (+ ?i 1))))
	(while (> (length$ ?pending) 0) do
		(bind ?last (+ (length$ ?out) 1))
		(bind ?k (min 127 (length$ ?pending)))
		(bind ?out (create$ ?out ?k (subseq$ ?pending 1 ?k)))
		(bind ?pending (subseq$ ?pending (+ ?k 1) (length$ ?pending))))
	(create$ ?last ?out))

(deffunction ftp-put-compressed (?t ?descriptor ?bytes ?filler)
	"compressed mode: the strings, with the escape sequence before the last string if there is a descriptor"
	(bind ?strings (ftp-compress ?bytes ?filler))
	(bind ?last (nth$ 1 ?strings))
	(bind ?out (rest$ ?strings))
	(if (= ?descriptor 0) then (return (ftp-put-bytes ?t ?out)))
	(ftp-put-bytes ?t (subseq$ ?out 1 (- ?last 1)))
	(put-char ?t 0)
	(put-char ?t ?descriptor)
	(ftp-put-bytes ?t (subseq$ ?out ?last (length$ ?out))))

(deffunction ftp-put-data (?t ?mode ?structure ?descriptor ?bytes ?filler)
	(switch ?mode
		(case stream then (ftp-put-stream ?t ?structure ?descriptor ?bytes))
		(case block then (ftp-put-blocks ?t ?descriptor ?bytes))
		(case compressed then (ftp-put-compressed ?t ?descriptor ?bytes ?filler))))

(deffunction ftp-ascii-table ()
	"the characters of codes 1 to 127, in order"
	(bind ?table (create$))
	(loop-for-count (?code 1 127)
		(bind ?table (create$ ?table (ftp-char-string ?code))))
	?table)

(defglobal ?*ftp-ascii* = (ftp-ascii-table))

(deffunction ftp-string-bytes (?text)
	"the codes of the characters of an ASCII string.  Each other character is a ?."
	(bind ?bytes (create$))
	(loop-for-count (?i 1 (str-length ?text))
		(bind ?code (member$ (sub-string ?i ?i ?text) ?*ftp-ascii*))
		(bind ?bytes (create$ ?bytes (if ?code then ?code else 63))))
	?bytes)

;;; the logical bytes a message carries: the data of a record
(defmessage-handler FTP-RECORD logical-bytes ()
	?self:octets)

;;; a page: the header fields, then the data
(defmessage-handler FTP-PAGE logical-bytes ()
	(create$ (+ 4 (length$ ?self:header)) ?self:index (length$ ?self:octets) ?self:page-type
	         ?self:header ?self:octets))

;;; the record or page as sent: packed, then in the mode and structure
(defmessage-handler FTP-RECORD pprint (?t)
	(ftp-put-data ?t ?self:mode ?self:structure
		(ftp-descriptor ?self:eor ?self:eof ?self:suspect)
		(ftp-pack (send ?self logical-bytes) ?self:byte-size)
		(ftp-filler ?self:type)))

;;; a marker block, or an escape sequence and a byte string.  Stream mode
;;; has no markers: the handler writes nothing.
(defmessage-handler FTP-MARKER pprint (?t)
	(bind ?bytes (ftp-string-bytes ?self:marker))
	(switch ?self:mode
		(case block then (ftp-put-block ?t 16 ?bytes))
		(case compressed then
			(bind ?bytes (subseq$ ?bytes 1 (min 127 (length$ ?bytes))))
			(ftp-put-bytes ?t (create$ 0 16 (length$ ?bytes) ?bytes)))))

;;; ------------------------------------------------------------------
;;; queries
;;; ------------------------------------------------------------------

(deffunction ftp-messages (?filename)
	"the messages of a stream, in order"
	(bind ?messages (create$))
	(bind ?sequence 1)
	(bind ?found TRUE)
	(while ?found do
		(bind ?found FALSE)
		(do-for-instance ((?m FTP-MESSAGE))
			(and (eq ?m:id ?filename) (= ?m:sequence ?sequence))
			(bind ?messages (create$ ?messages ?m))
			(bind ?found TRUE))
		(bind ?sequence (+ ?sequence 1)))
	?messages)

(deffunction ftp-numbers-in (?text)
	"each run of digits in ?text, as integers"
	(bind ?numbers (create$))
	(bind ?digits "")
	(loop-for-count (?i 1 (str-length ?text)) do
		(bind ?c (sub-string ?i ?i ?text))
		(if (str-index ?c "0123456789")
			then (bind ?digits (str-cat ?digits ?c))
			else (if (neq ?digits "") then
				(bind ?numbers (create$ ?numbers (string-to-field ?digits)))
				(bind ?digits ""))))
	(if (neq ?digits "") then (bind ?numbers (create$ ?numbers (string-to-field ?digits))))
	?numbers)

(deffunction ftp-split (?text ?delimiter)
	"?text split at ?delimiter, with the empty pieces"
	(bind ?pieces (create$))
	(bind ?at (str-index ?delimiter ?text))
	(while ?at do
		(bind ?pieces (create$ ?pieces (sub-string 1 (- ?at 1) ?text)))
		(bind ?text (sub-string (+ ?at (str-length ?delimiter)) (str-length ?text) ?text))
		(bind ?at (str-index ?delimiter ?text)))
	(create$ ?pieces ?text))

(deffunction ftp-words (?text)
	"the words of ?text: the pieces between spaces, without the empty pieces"
	(bind ?words (create$))
	(foreach ?piece (ftp-split ?text " ")
		(if (neq ?piece "") then (bind ?words (create$ ?words ?piece))))
	?words)

(deffunction ftp-trim-left (?text)
	"?text without leading spaces"
	(bind ?i 1)
	(while (and (<= ?i (str-length ?text)) (eq (sub-string ?i ?i ?text) " ")) do
		(bind ?i (+ ?i 1)))
	(sub-string ?i (str-length ?text) ?text))

(deffunction ftp-digits-p (?text)
	"TRUE if ?text is one or more decimal digits"
	(if (eq ?text "") then (return FALSE))
	(loop-for-count (?i 1 (str-length ?text))
		(if (not (str-index (sub-string ?i ?i ?text) "0123456789")) then (return FALSE)))
	TRUE)

(deffunction ftp-printable-p (?text)
	"TRUE if ?text is one or more printable ASCII characters, codes 33 to 126"
	(if (eq ?text "") then (return FALSE))
	(loop-for-count (?i 1 (str-length ?text))
		(bind ?code (member$ (sub-string ?i ?i ?text) ?*ftp-ascii*))
		(if (or (not ?code) (< ?code 33) (> ?code 126)) then (return FALSE)))
	TRUE)

(deffunction ftp-host-port-p (?text)
	"TRUE if ?text is h1,h2,h3,h4,p1,p2 with each number 0 to 255"
	(bind ?pieces (ftp-split ?text ","))
	(if (<> (length$ ?pieces) 6) then (return FALSE))
	(foreach ?piece ?pieces
		(if (not (ftp-digits-p ?piece)) then (return FALSE))
		(if (> (string-to-field ?piece) 255) then (return FALSE)))
	TRUE)

(deffunction ftp-classic-host-port (?text)
	"host and port from h1,h2,h3,h4,p1,p2 (PORT, 227)"
	(bind ?n (ftp-numbers-in ?text))
	(if (< (length$ ?n) 6) then (return (create$)))
	(create$ (format nil "%d.%d.%d.%d" (nth$ 1 ?n) (nth$ 2 ?n) (nth$ 3 ?n) (nth$ 4 ?n))
	         (+ (* 256 (nth$ 5 ?n)) (nth$ 6 ?n))))

(deffunction ftp-extended-host-port (?text)
	"host and port from |prt|host|port| (EPRT, 229)"
	(bind ?pieces (ftp-split ?text "|"))
	(if (< (length$ ?pieces) 5) then (return (create$)))
	(bind ?port (string-to-field (nth$ 4 ?pieces)))
	(if (not (integerp ?port)) then (return (create$)))
	(create$ (nth$ 3 ?pieces) ?port))

(deffunction ftp-quoted-pathname (?text)
	"the text in the first pair of quotes, or FALSE.  A doubled quote is one quote."
	(bind ?start (str-index "\"" ?text))
	(if (not ?start) then (return FALSE))
	(bind ?result "")
	(bind ?i (+ ?start 1))
	(bind ?n (str-length ?text))
	(while (<= ?i ?n) do
		(bind ?c (sub-string ?i ?i ?text))
		(if (neq ?c "\"")
			then (bind ?result (str-cat ?result ?c))
			     (bind ?i (+ ?i 1))
			else (if (and (< ?i ?n) (eq (sub-string (+ ?i 1) (+ ?i 1) ?text) "\""))
				then (bind ?result (str-cat ?result "\""))
				     (bind ?i (+ ?i 2))
				else (return ?result))))
	?result)

;;;; the commands (RFC 959 4.1, 5.3)

;;; the verbs of RFC 959 and of the extensions, by RFC
(defglobal ?*ftp-verbs-959* = (create$ USER PASS ACCT CWD CDUP SMNT QUIT REIN PORT PASV TYPE STRU MODE
	RETR STOR STOU APPE ALLO REST RNFR RNTO ABOR DELE RMD MKD PWD LIST NLST SITE SYST STAT HELP NOOP))
(defglobal ?*ftp-verbs-2228* = (create$ AUTH ADAT PROT PBSZ CCC MIC CONF ENC))
(defglobal ?*ftp-verbs-2389* = (create$ FEAT OPTS))
(defglobal ?*ftp-verbs-2428* = (create$ EPRT EPSV))
(defglobal ?*ftp-verbs-3659* = (create$ MLST MLSD SIZE MDTM))

;;; the RFC that defines the verb: 959, 2228, 2389, 2428, 3659, or FALSE
(defmessage-handler FTP-COMMAND standard ()
	(if (member$ ?self:verb ?*ftp-verbs-959*) then (return 959))
	(if (member$ ?self:verb ?*ftp-verbs-2228*) then (return 2228))
	(if (member$ ?self:verb ?*ftp-verbs-2389*) then (return 2389))
	(if (member$ ?self:verb ?*ftp-verbs-2428*) then (return 2428))
	(if (member$ ?self:verb ?*ftp-verbs-3659*) then (return 3659))
	FALSE)

;;; the group of RFC 959 4.1: access-control, transfer-parameter,
;;; service, or unknown.  The security commands of RFC 2228 are access
;;; control.  EPRT and EPSV are transfer parameters.  The other extension
;;; verbs are service.
(defmessage-handler FTP-COMMAND group ()
	(if (member$ ?self:verb (create$ USER PASS ACCT CWD CDUP SMNT REIN QUIT ?*ftp-verbs-2228*))
		then (return access-control))
	(if (member$ ?self:verb (create$ PORT PASV TYPE STRU MODE EPRT EPSV))
		then (return transfer-parameter))
	(if (member$ ?self:verb (create$ RETR STOR STOU APPE ALLO REST RNFR RNTO ABOR DELE RMD MKD PWD
	                                 LIST NLST SITE SYST STAT HELP NOOP FEAT OPTS MLST MLSD SIZE MDTM))
		then (return service))
	unknown)

;;; the representation type of a TYPE command.  ascii or ebcdic comes
;;; with its form: non-print, telnet or carriage-control.  local comes
;;; with its byte size.  Nothing for another verb or a bad argument.
(defmessage-handler FTP-COMMAND type-code ()
	(if (neq ?self:verb TYPE) then (return (create$)))
	(bind ?words (ftp-words (upcase ?self:argument)))
	(bind ?n (length$ ?words))
	(if (= ?n 0) then (return (create$)))
	(bind ?code (nth$ 1 ?words))
	(if (eq ?code "I") then
		(return (if (= ?n 1) then (create$ image) else (create$))))
	(if (eq ?code "L") then
		(if (and (= ?n 2) (ftp-digits-p (nth$ 2 ?words)) (> (string-to-field (nth$ 2 ?words)) 0))
			then (return (create$ local (string-to-field (nth$ 2 ?words))))
			else (return (create$))))
	(bind ?type (switch ?code (case "A" then ascii) (case "E" then ebcdic) (default FALSE)))
	(if (not ?type) then (return (create$)))
	(if (= ?n 1) then (return (create$ ?type non-print)))
	(if (> ?n 2) then (return (create$)))
	(bind ?form (switch (nth$ 2 ?words)
		(case "N" then non-print)
		(case "T" then telnet)
		(case "C" then carriage-control)
		(default FALSE)))
	(if ?form then (create$ ?type ?form) else (create$)))

;;; the structure of a STRU command: file, record or page, or FALSE
(defmessage-handler FTP-COMMAND structure-code ()
	(if (neq ?self:verb STRU) then (return FALSE))
	(switch (upcase ?self:argument)
		(case "F" then file)
		(case "R" then record)
		(case "P" then page)
		(default FALSE)))

;;; the mode of a MODE command: stream, block or compressed, or FALSE
(defmessage-handler FTP-COMMAND mode-code ()
	(if (neq ?self:verb MODE) then (return FALSE))
	(switch (upcase ?self:argument)
		(case "S" then stream)
		(case "B" then block)
		(case "C" then compressed)
		(default FALSE)))

;;; the size of an ALLO command, and the maximum record or page size if
;;; given: ALLO 1024 R 128 is 1024 128.  Nothing for another verb or a
;;; bad argument.
(defmessage-handler FTP-COMMAND allocation ()
	(if (neq ?self:verb ALLO) then (return (create$)))
	(bind ?words (ftp-words ?self:argument))
	(if (or (< (length$ ?words) 1) (not (ftp-digits-p (nth$ 1 ?words)))) then (return (create$)))
	(if (= (length$ ?words) 1) then (return (create$ (string-to-field (nth$ 1 ?words)))))
	(if (and (= (length$ ?words) 3) (eq (upcase (nth$ 2 ?words)) "R") (ftp-digits-p (nth$ 3 ?words)))
		then (return (create$ (string-to-field (nth$ 1 ?words)) (string-to-field (nth$ 3 ?words)))))
	(create$))

;;; the marker of a REST command, or FALSE
(defmessage-handler FTP-COMMAND marker ()
	(if (and (eq ?self:verb REST) (neq ?self:argument "")) then ?self:argument else FALSE))

;;; the pathname of a command that takes one, or FALSE
(defmessage-handler FTP-COMMAND pathname ()
	(if (and (member$ ?self:verb (create$ CWD SMNT RETR STOR APPE RNFR RNTO DELE RMD MKD LIST NLST STAT
	                                      MLST MLSD SIZE MDTM))
	         (neq ?self:argument ""))
		then ?self:argument
		else FALSE))

;;; TRUE if the argument has the syntax of RFC 959 5.3, or of the
;;; extension RFC, for the verb.  An unknown verb is not valid.
(defmessage-handler FTP-COMMAND valid ()
	(bind ?a ?self:argument)
	(if (member$ ?self:verb (create$ CDUP QUIT REIN PASV STOU ABOR PWD SYST NOOP FEAT CCC))
		then (return (eq ?a "")))
	(if (member$ ?self:verb (create$ USER PASS ACCT CWD SMNT RETR STOR APPE RNFR RNTO DELE RMD MKD SITE
	                                 OPTS SIZE MDTM AUTH ADAT MIC CONF ENC))
		then (return (neq ?a "")))
	(if (member$ ?self:verb (create$ LIST NLST STAT HELP MLST MLSD EPSV))
		then (return TRUE))
	(switch ?self:verb
		(case PORT then (ftp-host-port-p ?a))
		(case TYPE then (> (length$ (send ?self type-code)) 0))
		(case STRU then (neq (send ?self structure-code) FALSE))
		(case MODE then (neq (send ?self mode-code) FALSE))
		(case ALLO then (> (length$ (send ?self allocation)) 0))
		(case REST then (ftp-printable-p ?a))
		(case EPRT then (= (length$ (send ?self host-port)) 2))
		(case PBSZ then (ftp-digits-p ?a))
		(case PROT then (neq (member$ (upcase ?a) (create$ "C" "S" "E" "P")) FALSE))
		(default FALSE)))

;;; TRUE for a command of the minimum implementation of RFC 959 5.1:
;;; USER, QUIT, PORT, RETR, STOR, NOOP, and TYPE, MODE and STRU with
;;; the values that each server accepts
(defmessage-handler FTP-COMMAND minimum ()
	(if (member$ ?self:verb (create$ USER QUIT PORT RETR STOR NOOP)) then (return TRUE))
	(switch ?self:verb
		(case TYPE then (eq (send ?self type-code) (create$ ascii non-print)))
		(case MODE then (eq (send ?self mode-code) stream))
		(case STRU then (neq (member$ (send ?self structure-code) (create$ file record)) FALSE))
		(default FALSE)))

;;; the reply codes a verb can get (RFC 959 5.4).  CONNECTION is the
;;; greeting.  The verbs of RFC 2389, RFC 2428 and RFC 3659 have their
;;; own lists.  Nothing for another verb.
(deffunction ftp-replies-to (?verb)
	(switch ?verb
		(case CONNECTION then (create$ 120 220 421))
		(case USER then (create$ 230 530 500 501 421 331 332))
		(case PASS then (create$ 230 202 530 500 501 503 421 332))
		(case ACCT then (create$ 230 202 530 500 501 503 421))
		(case CWD then (create$ 250 500 501 502 421 530 550))
		(case CDUP then (create$ 200 500 501 502 421 530 550))
		(case SMNT then (create$ 202 250 500 501 502 421 530 550))
		(case REIN then (create$ 120 220 421 500 502))
		(case QUIT then (create$ 221 500))
		(case PORT then (create$ 200 500 501 421 530))
		(case PASV then (create$ 227 500 501 502 421 530))
		(case MODE then (create$ 200 500 501 504 421 530))
		(case TYPE then (create$ 200 500 501 504 421 530))
		(case STRU then (create$ 200 500 501 504 421 530))
		(case ALLO then (create$ 200 202 500 501 504 421 530))
		(case REST then (create$ 500 501 502 421 530 350))
		(case STOR then (create$ 125 150 110 226 250 425 426 451 551 552 532 450 452 553 500 501 421 530))
		(case STOU then (create$ 125 150 110 226 250 425 426 451 551 552 532 450 452 553 500 501 421 530))
		(case RETR then (create$ 125 150 110 226 250 425 426 451 450 550 500 501 421 530))
		(case LIST then (create$ 125 150 226 250 425 426 451 450 500 501 502 421 530))
		(case NLST then (create$ 125 150 226 250 425 426 451 450 500 501 502 421 530))
		(case APPE then (create$ 125 150 110 226 250 425 426 451 551 552 532 450 550 452 553 500 501 502 421 530))
		(case RNFR then (create$ 450 550 500 501 502 421 530 350))
		(case RNTO then (create$ 250 532 553 500 501 502 503 421 530))
		(case DELE then (create$ 250 450 550 500 501 502 421 530))
		(case RMD then (create$ 250 500 501 502 421 530 550))
		(case MKD then (create$ 257 500 501 502 421 530 550))
		(case PWD then (create$ 257 500 501 502 421 550))
		(case ABOR then (create$ 225 226 500 501 502 421))
		(case SYST then (create$ 215 500 501 502 421))
		(case STAT then (create$ 211 212 213 450 500 501 502 421 530))
		(case HELP then (create$ 211 214 500 501 502 421))
		(case SITE then (create$ 200 202 500 501 530))
		(case NOOP then (create$ 200 500 421))
		(case FEAT then (create$ 211 500 502 421))
		(case OPTS then (create$ 200 451 500 501 502 421))
		(case EPRT then (create$ 200 500 501 522 421 530))
		(case EPSV then (create$ 229 500 501 522 421 530))
		(case SIZE then (create$ 213 550 500 501 502 421 530))
		(case MDTM then (create$ 213 550 500 501 502 421 530))
		(case MLST then (create$ 250 550 500 501 502 421 530))
		(case MLSD then (create$ 125 150 226 250 425 426 451 450 550 500 501 502 421 530))
		(default (create$))))

;;; the reply codes the command can get
(defmessage-handler FTP-COMMAND replies ()
	(ftp-replies-to ?self:verb))

;;; the verbs that follow a 3yz reply to the command (RFC 959 6): PASS
;;; after USER, ACCT after PASS, RNTO after RNFR, and a transfer after
;;; REST.  Nothing for the other verbs.
(defmessage-handler FTP-COMMAND next ()
	(switch ?self:verb
		(case USER then (create$ PASS))
		(case PASS then (create$ ACCT))
		(case RNFR then (create$ RNTO))
		(case REST then (create$ RETR STOR APPE))
		(default (create$))))

;;;; the replies (RFC 959 4.2)

;;; the host and port of a PORT or EPRT command, or nothing.  EPRT keeps
;;; the host as sent, IPv4 or IPv6.
(defmessage-handler FTP-COMMAND host-port ()
	(switch ?self:verb
		(case PORT then (ftp-classic-host-port ?self:argument))
		(case EPRT then (ftp-extended-host-port ?self:argument))
		(default (create$))))

;;; the host and port of a 227 or 229 reply, or nothing.  A 229 reply has
;;; no host, so the host is "": use the same server.
(defmessage-handler FTP-REPLY host-port ()
	(switch ?self:code
		(case 227 then (ftp-classic-host-port ?self:text))
		(case 229 then (ftp-extended-host-port ?self:text))
		(default (create$))))

;;; the meaning of the first digit (RFC 959 4.2, RFC 2228)
(defmessage-handler FTP-REPLY category ()
	(switch (div ?self:code 100)
		(case 1 then positive-preliminary)
		(case 2 then positive-completion)
		(case 3 then positive-intermediate)
		(case 4 then transient-negative-completion)
		(case 5 then permanent-negative-completion)
		(case 6 then protected)
		(default unknown)))

;;; the meaning of the second digit (RFC 959 4.2)
(defmessage-handler FTP-REPLY group ()
	(switch (mod (div ?self:code 10) 10)
		(case 0 then syntax)
		(case 1 then information)
		(case 2 then connections)
		(case 3 then authentication)
		(case 4 then unspecified)
		(case 5 then file-system)
		(default unknown)))

;;; TRUE for a 1yz, 2yz or 3yz reply
(defmessage-handler FTP-REPLY positive ()
	(and (>= ?self:code 100) (< ?self:code 400)))

;;; the meaning of the code (RFC 959 4.2.2, and the extensions), or FALSE
(defmessage-handler FTP-REPLY meaning ()
	(switch ?self:code
		(case 110 then "Restart marker reply.")
		(case 120 then "Service ready in nnn minutes.")
		(case 125 then "Data connection already open; transfer starting.")
		(case 150 then "File status okay; about to open data connection.")
		(case 200 then "Command okay.")
		(case 202 then "Command not implemented, superfluous at this site.")
		(case 211 then "System status, or system help reply.")
		(case 212 then "Directory status.")
		(case 213 then "File status.")
		(case 214 then "Help message.")
		(case 215 then "NAME system type.")
		(case 220 then "Service ready for new user.")
		(case 221 then "Service closing control connection.")
		(case 225 then "Data connection open; no transfer in progress.")
		(case 226 then "Closing data connection.")
		(case 227 then "Entering Passive Mode (h1,h2,h3,h4,p1,p2).")
		(case 229 then "Entering Extended Passive Mode (|||port|).")
		(case 230 then "User logged in, proceed.")
		(case 232 then "User logged in, authorized by security data exchange.")
		(case 234 then "Security data exchange complete.")
		(case 235 then "Security data exchange completed successfully.")
		(case 250 then "Requested file action okay, completed.")
		(case 257 then "\"PATHNAME\" created.")
		(case 331 then "User name okay, need password.")
		(case 332 then "Need account for login.")
		(case 334 then "Security data exchange continues.")
		(case 335 then "Security data exchange continues; data in reply.")
		(case 336 then "Username okay, need password. Challenge in reply.")
		(case 350 then "Requested file action pending further information.")
		(case 421 then "Service not available, closing control connection.")
		(case 425 then "Can't open data connection.")
		(case 426 then "Connection closed; transfer aborted.")
		(case 431 then "Need some unavailable resource to process security.")
		(case 450 then "Requested file action not taken.")
		(case 451 then "Requested action aborted: local error in processing.")
		(case 452 then "Requested action not taken.")
		(case 500 then "Syntax error, command unrecognized.")
		(case 501 then "Syntax error in parameters or arguments.")
		(case 502 then "Command not implemented.")
		(case 503 then "Bad sequence of commands.")
		(case 504 then "Command not implemented for that parameter.")
		(case 522 then "Network protocol not supported.")
		(case 530 then "Not logged in.")
		(case 532 then "Need account for storing files.")
		(case 533 then "Command protection level denied for policy reasons.")
		(case 534 then "Request denied for policy reasons.")
		(case 535 then "Failed security check.")
		(case 536 then "Data protection level not supported by security mechanism.")
		(case 537 then "Command protection level not supported by security mechanism.")
		(case 550 then "Requested action not taken.")
		(case 551 then "Requested action aborted: page type unknown.")
		(case 552 then "Requested file action aborted.")
		(case 553 then "Requested action not taken.")
		(case 631 then "Integrity protected reply.")
		(case 632 then "Confidentiality and integrity protected reply.")
		(case 633 then "Confidentiality protected reply.")
		(default FALSE)))

;;; the RFC that defines the code: 959, 2228, 2428, or FALSE
(defmessage-handler FTP-REPLY standard ()
	(if (member$ ?self:code (create$ 110 120 125 150 200 202 211 212 213 214 215 220 221 225 226 227 230
	                                 250 257 331 332 350 421 425 426 450 451 452 500 501 502 503 504 530
	                                 532 550 551 552 553))
		then (return 959))
	(if (member$ ?self:code (create$ 232 234 235 334 335 336 431 533 534 535 536 537 631 632 633))
		then (return 2228))
	(if (member$ ?self:code (create$ 229 522)) then (return 2428))
	FALSE)

;;; TRUE if the code is a reply the verb can get (RFC 959 5.4).  Give
;;; the verb as a symbol, or CONNECTION for the greeting.
(defmessage-handler FTP-REPLY answers (?verb)
	(neq (member$ ?self:code (ftp-replies-to ?verb)) FALSE))

;;; the state diagram of a verb (RFC 959 6): simple, transfer, login,
;;; pending, connection, or FALSE.  In the pending diagram, a 3yz reply
;;; asks for the next command.
(deffunction ftp-diagram (?verb)
	(if (member$ ?verb (create$ ABOR ALLO DELE CWD CDUP SMNT HELP MODE NOOP PASV QUIT SITE PORT SYST STAT
	                            RMD MKD PWD STRU TYPE ACCT RNTO FEAT OPTS EPRT EPSV SIZE MDTM MLST))
		then (return simple))
	(if (member$ ?verb (create$ APPE LIST NLST REIN RETR STOR STOU MLSD)) then (return transfer))
	(if (member$ ?verb (create$ USER PASS)) then (return login))
	(if (member$ ?verb (create$ RNFR REST)) then (return pending))
	(if (eq ?verb CONNECTION) then (return connection))
	FALSE)

;;; what the reply means for the verb, by the state diagrams of RFC 959
;;; 6: success, failure, error, wait, continue, or unknown.  wait is a
;;; 1yz reply to a transfer: another reply follows.  continue is a 3yz
;;; reply: send the next command.  unknown is for a verb or a code
;;; outside the diagrams.
(defmessage-handler FTP-REPLY outcome (?verb)
	(bind ?digit (div ?self:code 100))
	(bind ?diagram (ftp-diagram ?verb))
	(if (or (not ?diagram) (< ?digit 1) (> ?digit 5)) then (return unknown))
	(nth$ ?digit (switch ?diagram
		(case simple then (create$ error success error failure failure))
		(case transfer then (create$ wait success error failure failure))
		(case login then (create$ error success continue failure failure))
		(case pending then (create$ error error continue failure failure))
		(case connection then (create$ wait success error failure failure)))))

;;; all lines of the reply, with a line break between each
(defmessage-handler FTP-REPLY full-text ()
	(bind ?result "")
	(foreach ?line ?self:lines
		(bind ?result (format nil "%s%s%n" ?result ?line)))
	(str-cat ?result ?self:text))

;;; the quoted pathname of a 257 reply, or FALSE.  A doubled quote is one quote.
(defmessage-handler FTP-REPLY pathname ()
	(ftp-quoted-pathname ?self:text))

;;; the middle lines without leading spaces: the features of a 211 reply
;;; to FEAT (RFC 2389)
(defmessage-handler FTP-REPLY features ()
	(bind ?features (create$))
	(foreach ?line (rest$ ?self:lines)
		(bind ?features (create$ ?features (ftp-trim-left ?line))))
	?features)

;;; the markers of a 110 reply, MARK yyyy = mmmm: the user's marker,
;;; then the server's.  Nothing for another reply.
(defmessage-handler FTP-REPLY restart-markers ()
	(bind ?words (ftp-words ?self:text))
	(if (and (= ?self:code 110) (= (length$ ?words) 4) (eq (upcase (nth$ 1 ?words)) "MARK")
	         (eq (nth$ 3 ?words) "="))
		then (create$ (nth$ 2 ?words) (nth$ 4 ?words))
		else (create$)))

;;; the minutes of a 120 reply, Service ready in nnn minutes, or FALSE
(defmessage-handler FTP-REPLY delay ()
	(bind ?numbers (ftp-numbers-in ?self:text))
	(if (and (= ?self:code 120) (> (length$ ?numbers) 0)) then (nth$ 1 ?numbers) else FALSE))

;;; the system name of a 215 reply, NAME system type, or FALSE
(defmessage-handler FTP-REPLY system ()
	(bind ?words (ftp-words ?self:text))
	(if (and (= ?self:code 215) (> (length$ ?words) 0)) then (sym-cat (nth$ 1 ?words)) else FALSE))

;;;; the data

;;; the octets as a string.  The handler drops a zero octet: a CLIPS
;;; string cannot hold it.
(defmessage-handler FTP-RECORD text ()
	(bind ?text "")
	(foreach ?byte ?self:octets
		(bind ?text (ftp-append-char ?text ?byte)))
	?text)

;;; the lines of the text, split at CR LF.  A last line without CR LF
;;; counts.
(defmessage-handler FTP-RECORD lines ()
	(bind ?lines (create$))
	(bind ?line "")
	(bind ?previous 0)
	(foreach ?byte ?self:octets
		(if (and (= ?byte 10) (= ?previous 13))
			then (bind ?lines (create$ ?lines (sub-string 1 (- (str-length ?line) 1) ?line)))
			     (bind ?line "")
			else (bind ?line (ftp-append-char ?line ?byte)))
		(bind ?previous ?byte))
	(if (neq ?line "") then (bind ?lines (create$ ?lines ?line)))
	?lines)

(deffunction ftp-carriage-control (?line)
	"the vertical spacing that an ASA control character asks for"
	(if (eq ?line "") then (return FALSE))
	(switch (sub-string 1 1 ?line)
		(case " " then one-line)
		(case "0" then two-lines)
		(case "1" then new-page)
		(case "+" then overprint)
		(default FALSE)))

;;; the vertical spacing of each line, from its first character, for the
;;; carriage control form (RFC 959 3.1.1.5.2): one-line, two-lines,
;;; new-page, overprint, or FALSE for an empty line or an unknown control
(defmessage-handler FTP-RECORD carriage-controls ()
	(bind ?controls (create$))
	(foreach ?line (send ?self lines)
		(bind ?controls (create$ ?controls (ftp-carriage-control ?line))))
	?controls)

;;; the page type by name: last, simple, descriptor, access-controlled,
;;; or the number of an unknown type
(defmessage-handler FTP-PAGE kind ()
	(switch ?self:page-type
		(case 0 then last)
		(case 1 then simple)
		(case 2 then descriptor)
		(case 3 then access-controlled)
		(default ?self:page-type)))
