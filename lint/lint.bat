;;; Lints the rules of the codec against the practices a COOL codec follows:
;;;
;;;   1. only one rule is activated at a time while one case is decoded
;;;   2. all the cases decode at once, each to the same result as alone; a
;;;      case that is read to the end of its stream gives one ftp-eof fact
;;;   3. the right hand side of a rule holds no (if ...)
;;;   4. no deffunction called from the right hand side of a rule, directly
;;;      or through another deffunction, holds an (if ...)
;;;   5. the right hand side of a rule holds no (find-fact or (do-for-fact,
;;;      directly or through a deffunction: a single fact a rule acts on, it
;;;      matches on the left hand side
;;;
;;; Prints one line per violation and a summary, then exits with 1 when it
;;; found a violation and 0 when it found none.
(load* ../tests/compline.clp)
(load* ../program.clp)
(load* ../tests/cases.clp)

;;; the count survives the reset before each run
(set-reset-globals FALSE)
(defglobal ?*violations* = 0)
;;; guards against a rule set that never empties its agenda
(defglobal ?*max-steps* = 1000000)
;;; the functions that look up a single fact instead of matching it
(defglobal ?*fact-queries* = (create$ find-fact do-for-fact))

(deffunction violation (?message)
	(bind ?*violations* (+ ?*violations* 1))
	(println "LINT " ?message))

;;; the cases in tests/cases: test.ftp, test2.ftp, test3.ftp ... up to the
;;; first number that has no file, then data1.ftp, data2.ftp ... the same way
(deffunction find-cases ()
	(bind ?cases (create$))
	(foreach ?prefix (create$ test data)
		(bind ?n 1)
		(bind ?file (if (eq ?prefix test) then test.ftp else data1.ftp))
		(while (open (sym-cat ../tests/cases/ ?file) probe) do
			(close probe)
			(bind ?cases (create$ ?cases ?file))
			(bind ?n (+ ?n 1))
			(bind ?file (sym-cat ?prefix ?n .ftp))))
	?cases)

(deffunction start-case (?file)
	(open (sym-cat ../tests/cases/ ?file) ?file)
	(ftp-parse-case ?file ?file))

;;; TRUE when the stream of a case has reached ftp-eof
(deffunction at-eof (?file)
	(any-factp ((?e ftp-eof)) (eq ?e:id ?file)))

(deffunction all-at-eof (?cases)
	(bind ?all TRUE)
	(foreach ?file ?cases
		(if (not (at-eof ?file)) then (bind ?all FALSE)))
	?all)

;;; Asserts a ready fact for each case that has not reached ftp-eof, as a
;;; program that reads a file does.  The codec reads each message only after
;;; a ready fact.  Returns the number asserted.
(deffunction ready-again (?cases)
	(bind ?asserted 0)
	(foreach ?file ?cases
		(if (not (at-eof ?file)) then
			(assert (ftp-ready (id ?file)))
			(bind ?asserted (+ ?asserted 1))))
	?asserted)

;;; the name of every rule on the agenda, once per activation
(deffunction activations ()
	(bind ?active (create$))
	(foreach ?rule (get-defrule-list)
		(loop-for-count (nth$ 3 (matches ?rule terse))
			(bind ?active (create$ ?active ?rule))))
	?active)

;;; 1. decodes one case a rule at a time and checks the agenda after every
;;; firing.  Every case is read to the end of its stream.  Returns the number
;;; of rules that fired.
(deffunction lint-one-activation (?file)
	(reset)
	(start-case ?file)
	(bind ?fired 0)
	(bind ?offending-steps 0)
	(bind ?stalled FALSE)
	(while (and (not ?stalled) (not (at-eof ?file)) (< ?fired ?*max-steps*)) do
		(ready-again (create$ ?file))
		(bind ?active (activations))
		(bind ?stalled (= (length$ ?active) 0))
		(while (and (> (length$ ?active) 0) (< ?fired ?*max-steps*)) do
			(if (> (length$ ?active) 1) then
				(bind ?offending-steps (+ ?offending-steps 1))
				(if (= ?offending-steps 1) then
					(violation (format nil "%s: %d activations after %d rules fired: %s"
					                   ?file (length$ ?active) ?fired (implode$ ?active)))))
			(run 1)
			(bind ?fired (+ ?fired 1))
			(bind ?active (activations))))
	(close ?file)
	(if (> ?offending-steps 1) then
		(println "     " ?file ": " ?offending-steps " steps had more than one activation"))
	(if ?stalled then
		(violation (format nil "%s: the agenda was empty before ftp-eof after %d rules fired" ?file ?fired)))
	(if (>= ?fired ?*max-steps*) then
		(violation (format nil "%s: the agenda was not empty after %d rules fired" ?file ?fired)))
	?fired)

;;; 2. decodes every case in one run, then checks that each one printed
;;; what it prints alone and that the run fired as many rules as the cases
;;; fired alone put together.  Every case is read to the end of its stream
;;; and gives one ftp-eof fact.
(deffunction lint-many-at-once (?cases ?fired-alone)
	(reset)
	(foreach ?file ?cases (start-case ?file))
	(bind ?fired 0)
	(bind ?stalled FALSE)
	(while (and (not ?stalled) (not (all-at-eof ?cases)) (< ?fired ?*max-steps*)) do
		(ready-again ?cases)
		(bind ?stalled (= (length$ (activations)) 0))
		(while (and (> (length$ (activations)) 0) (< ?fired ?*max-steps*)) do
			(run 1)
			(bind ?fired (+ ?fired 1))))
	(if ?stalled then
		(violation (format nil "the agenda was empty before every case reached ftp-eof after %d rules fired" ?fired)))
	(if (<> ?fired ?fired-alone) then
		(violation (format nil "%d rules fired decoding the cases together, %d fired decoding them alone"
		                   ?fired ?fired-alone)))
	(foreach ?file ?cases
		(bind ?ends (length$ (find-all-facts ((?e ftp-eof)) (eq ?e:id ?file))))
		(if (<> ?ends 1) then
			(violation (format nil "%s: found %d ftp-eof facts decoding the cases together" ?file ?ends))))
	(foreach ?file ?cases
		(bind ?messages (ftp-messages ?file))
		(bind ?done (find-all-facts ((?f ftp-done)) (eq ?f:id ?file)))
		(if (<> (length$ ?done) (length$ ?messages))
		    then (violation (format nil "%s: found %d ftp-done facts for %d messages decoding the cases together"
		                            ?file (length$ ?done) (length$ ?messages)))
		    else (open actual actual "w")
		         (foreach ?message ?messages (send ?message pprint actual))
		         (close actual)
		         (bind ?differences (compare-files (sym-cat ../tests/expected/ ?file) actual))
		         (if (> (length$ ?differences) 0) then
		             (violation (format nil "%s: decoded differently together with the other cases" ?file))
		             (foreach ?line ?differences (println ?line))))
		(close ?file)))

;;; the text of the rule from => on
(deffunction rhs (?rule)
	(bind ?text (ppdefrule ?rule nil))
	(sub-string (str-index "=>" ?text) (str-length ?text) ?text))

;;; TRUE when the text of a construct holds an (if
(deffunction holds-if (?text)
	(or (str-index "(if " ?text) (str-index (format nil "(if%n") ?text)))

;;; TRUE when the text of a construct calls the function ?name
(deffunction calls (?text ?name)
	(or (str-index (str-cat "(" ?name " ") ?text)
	    (str-index (str-cat "(" ?name ")") ?text)
	    (str-index (str-cat "(" ?name (format nil "%n")) ?text)))

;;; the deffunctions the text calls, the deffunctions those call, and so on
(deffunction called-deffunctions (?text)
	(bind ?found (create$))
	(bind ?pending (create$ ?text))
	(while (> (length$ ?pending) 0) do
		(bind ?text (nth$ 1 ?pending))
		(bind ?pending (rest$ ?pending))
		(foreach ?function (get-deffunction-list)
			(if (and (not (member$ ?function ?found)) (calls ?text ?function)) then
				(bind ?found (create$ ?found ?function))
				(bind ?pending (create$ ?pending (ppdeffunction ?function nil))))))
	?found)

;;; 3. the right hand side holds no (if
(deffunction lint-rhs (?rule)
	(if (holds-if (rhs ?rule)) then
		(violation (format nil "%s: (if in the right hand side" ?rule))))

;;; 4. no deffunction the right hand side calls holds an (if
(deffunction lint-rhs-deffunctions (?rule)
	(foreach ?function (called-deffunctions (rhs ?rule))
		(if (holds-if (ppdeffunction ?function nil)) then
			(violation (format nil "%s: (if in deffunction %s called from the right hand side"
			                   ?rule ?function)))))

;;; the first of ?*fact-queries* the text calls, or FALSE
(deffunction fact-query (?text)
	(foreach ?query ?*fact-queries*
		(if (calls ?text ?query) then (return ?query)))
	FALSE)

;;; 5. neither the right hand side nor a deffunction it calls holds a
;;; (find-fact or (do-for-fact
(deffunction lint-rhs-fact-queries (?rule)
	(bind ?query (fact-query (rhs ?rule)))
	(if ?query then
		(violation (format nil "%s: (%s in the right hand side" ?rule ?query)))
	(foreach ?function (called-deffunctions (rhs ?rule))
		(bind ?query (fact-query (ppdeffunction ?function nil)))
		(if ?query then
			(violation (format nil "%s: (%s in deffunction %s called from the right hand side"
			                   ?rule ?query ?function)))))

(deffunction lint ()
	(bind ?cases (find-cases))
	(bind ?fired-alone 0)
	(foreach ?file ?cases
		(bind ?fired-alone (+ ?fired-alone (lint-one-activation ?file))))
	(lint-many-at-once ?cases ?fired-alone)
	(foreach ?rule (get-defrule-list)
		(lint-rhs ?rule)
		(lint-rhs-deffunctions ?rule)
		(lint-rhs-fact-queries ?rule))
	(println (length$ (get-defrule-list)) " rules, " (length$ ?cases) " cases, "
	         ?*violations* " violations")
	(if (> ?*violations* 0) then (exit 1) else (exit 0)))

(lint)
