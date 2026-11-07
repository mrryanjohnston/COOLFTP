;;; The transfer parameters of the data connection cases: the file, then
;;; its type, form, byte size, structure and mode.  A case not in the list
;;; is a control connection.
(defglobal ?*data-cases* = (create$
	data1.ftp image non-print 8 file stream
	data2.ftp ascii non-print 8 record stream
	data3.ftp ascii non-print 8 record stream
	data4.ftp image non-print 8 record block
	data5.ftp ascii non-print 8 file block
	data6.ftp image non-print 8 record block
	data7.ftp ascii non-print 8 file compressed
	data8.ftp ascii non-print 8 record compressed
	data9.ftp image non-print 8 file compressed
	data10.ftp image non-print 8 page stream
	data11.ftp local non-print 12 file stream
	data12.ftp image non-print 8 page block
	data13.ftp ascii non-print 8 file stream
	data14.ftp ascii non-print 8 file compressed
	data15.ftp ascii carriage-control 8 file stream
	data16.ftp image non-print 8 record block
	data17.ftp image non-print 8 file block
	data18.ftp ebcdic non-print 8 file compressed
	data19.ftp ascii non-print 8 record stream
	data20.ftp local non-print 12 record block
	data21.ftp ascii non-print 8 file compressed
	data22.ftp image non-print 8 page stream
	data23.ftp ascii non-print 8 record stream
	data24.ftp image non-print 8 file block
	data25.ftp image non-print 8 file block
	data26.ftp ascii non-print 8 file compressed
	data27.ftp image non-print 8 page stream
	data28.ftp ascii non-print 8 record stream))

;;; asserts the start fact for case ?file, open under the logical name
;;; ?id, with the parameters of the case
(deffunction ftp-parse-case (?file ?id)
	(bind ?at (member$ ?file ?*data-cases*))
	(if ?at
		then (assert (ftp-parse (stream ?id) (id ?id) (connection data)
			(type (nth$ (+ ?at 1) ?*data-cases*))
			(form (nth$ (+ ?at 2) ?*data-cases*))
			(byte-size (nth$ (+ ?at 3) ?*data-cases*))
			(structure (nth$ (+ ?at 4) ?*data-cases*))
			(mode (nth$ (+ ?at 5) ?*data-cases*))))
		else (assert (ftp-parse (stream ?id) (id ?id)))))
