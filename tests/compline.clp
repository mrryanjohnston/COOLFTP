;;; Compares two files octet by octet.  Returns a multifield of report
;;; lines, one for each difference.  An empty multifield means that the
;;; files match.  Each line of the control connection must end in CR LF, so
;;; the comparison is by octet, not by line: a lost CR must show.
(deffunction compare-files (?base-file ?test-file)
   (open ?base-file base)
   (open ?test-file test)
   (bind ?i 0)
   (bind ?count 0)
   (bind ?differences (create$))
   (bind ?base-byte (get-char base))
   (bind ?test-byte (get-char test))
   (while (and (neq ?base-byte -1) (neq ?test-byte -1)) do
      (bind ?i (+ ?i 1))
      (if (neq ?base-byte ?test-byte) then
          (bind ?count (+ ?count 1))
          (if (<= ?count 8) then
              (bind ?differences (create$ ?differences
                 (format nil "   octet %4d expected: %3d actual: %3d"
                         ?i ?base-byte ?test-byte)))))
      (bind ?base-byte (get-char base))
      (bind ?test-byte (get-char test)))
   (if (> ?count 8) then
      (bind ?differences (create$ ?differences
         (format nil "   ...and %d more differing octets" (- ?count 8)))))
   (if (or (neq ?base-byte -1) (neq ?test-byte -1)) then
      (bind ?differences (create$ ?differences
         "   files do not have the same # of octets:"
         (format nil "   expected octet %4d: %3d" (+ ?i 1) ?base-byte)
         (format nil "   actual   octet %4d: %3d" (+ ?i 1) ?test-byte))))
   (close base)
   (close test)
   ?differences)
