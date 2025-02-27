(defpackage :read-csv
  (:documentation "A simple CSV file reader.")
  (:use :common-lisp)
  (:export
   #:read-csv
   #:parse-csv))

(in-package :read-csv)

(defvar *records*)
(defvar *record*)
(defvar *white-char-count*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun noop (c) (declare (ignore c)))
  (defun addc (c) (push c *record*))
  (defun addl (c) (declare (ignore c)) (push #\Linefeed *record*))
  (defun next (c) 
    (declare (ignore c))
    (let* ((eow
             (loop with length-so-far fixnum = 0
                   as c character in *record*
                   while (or (char= c #\Space) (char= c #\Tab))
                   do (incf length-so-far)
                   finally (return length-so-far)))
           (n (max 0 (min eow (1- *white-char-count*))))
           (string-contents-in-reverse
             (nthcdr n *record*))
           (length (length string-contents-in-reverse))
           (string
             (make-array length :element-type 'character)))
      (loop for i from (1- length) downto 0
            as char in string-contents-in-reverse
            do (setf (schar string i) char))
      (push string *records*))
    (setf *record* nil))
  (defun ship (c) (next c) (setf *records* (nreverse *records*))))

(defconstant done! -1)
(defconstant start 0) ;; Start
(defconstant retur 1)  ;; Return (as in CRLF seen)
;;^ ---*** no longer used; Return handled in read-csv-char; clean up some day
(defconstant unquo 2)  ;; Unquoted text
(defconstant myquo 3)  ;; Quoted text
(defconstant q+ret 4)  ;; Quoted and have seen return
(defconstant q+quo 5)  ;; Quoted and have seen quote
(defconstant q+q&w 6)  ;; Quoted and have seen quote, in following whitespace

(defconstant +csv-table+
  (if (boundp '+csv-table+) (symbol-value '+csv-table+)
      (make-array '(7 6 2) 
       :initial-contents
       ;;WHITE,         RETURN,       LF,           QUOTE,        SEP,          OTHER           ;; STATE 
       `(((noop ,start) (ship ,retur) (ship ,done!) (noop ,myquo) (next ,start) (addc ,unquo))  ;; 0 start
	 ((noop ,start) (ship ,retur) (noop ,done!) (noop ,start) (next ,start) (addc ,unquo))  ;; 1 return seen
	 ((addc ,unquo) (ship ,retur) (ship ,done!) (addc ,unquo) (next ,start) (addc ,unquo))  ;; 2 unquoted text
	 ((addc ,myquo) (noop ,q+ret) (addl ,myquo) (noop ,q+quo) (addc ,myquo) (addc ,myquo))  ;; 3 in-quote
	 ((addc ,myquo) (noop ,q+ret) (addl ,myquo) (noop ,q+quo) (addc ,myquo) (addc ,myquo))  ;; 4 in-quote, seen return
	 ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,myquo) (next ,start) (addc ,unquo))  ;; 5 in-quote, seen quote
	 ((noop ,q+q&w) (ship ,retur) (ship ,done!) (addc ,myquo) (next ,start) (addc ,unquo)))))) ;; 6 in quote, seen quote, now whitespace

(declaim (inline char-class))
(defun char-class (sep char)
  (declare (type character sep char))
  (case char (#\Space 0) (#\Return 1) (#\Linefeed 2) (#\" 3) (otherwise (if (char= sep char) 4 5))))

(declaim (inline read-csv-char))
(defun read-csv-char (stream eof-error-p)
  (let ((char-or-eof (read-char stream eof-error-p :eof)))
    (case char-or-eof
      (#\Return
       ;; CR: if followed by LF, read past it. Either way, return LF.
       (when (eql (peek-char nil stream eof-error-p :eof) #\Linefeed)
         (read-char stream))
       #\Linefeed)
      (otherwise char-or-eof))))

(defun read-csv (stream &optional (sep #\,) (eof-error-p t) eof-value)
  "Read a single line of CSV data from stream. Return the parsed CSV
   data and a boolean that is true when you've just read the last
   record in the stream.

   eof-error-p controls how this function behaves if the first
   character read is end-of-file.  If true, an error is thrown, if
   false, the eof-value is returned."
  (let ((*records* nil)
        (*record* nil)
        (*white-char-count* 0))
    (declare (special *record* *records* *white-char-count*)
             (type fixnum *white-char-count*))
    (loop with state fixnum = start
          for char = (read-csv-char stream (and (null *records*) eof-error-p))
          when (eq char :eof) 
          do (return-from read-csv (values (if *records* (ship :eof) eof-value) t))
          do (incf *white-char-count*)
          do (let ((class (char-class sep char)))
               (declare (type fixnum class))
               (when (= class myquo) (setf *white-char-count* 0))
               (funcall (aref +csv-table+ state class 0) char)
               (setf state (aref +csv-table+ state class 1)))
          until (= state done!))
    (values *records* (eq :eof (peek-char nil stream nil :eof)))))
 
(defun parse-csv (stream  &optional (sep #\,))
  "Read CSV data from a stream until end-of-file is encountered."
  (loop with line
        with end-p
        do (multiple-value-setq (line end-p)
             (read-csv stream sep nil :eof))
        unless (eq line :eof)
          collect line
        until end-p))
