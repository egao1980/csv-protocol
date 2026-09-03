(in-package #:csv-protocol/tests)

(deftest stream-row-roundtrip
  (let ((raw (with-output-to-string (o)
               (let ((out (make-csv-output-stream o :header t)))
                 (serdes-protocol:stream-encode-value out '(("name" . "alice") ("n" . "1")))
                 (serdes-protocol:stream-encode-value out '(("name" . "bob") ("n" . "2")))))))
    (with-input-from-string (i raw)
      (let ((in (make-csv-input-stream i :header t)))
        (let ((r1 (serdes-protocol:stream-decode-value in))
              (r2 (serdes-protocol:stream-decode-value in)))
          (ok (hash-table-p r1))
          (ok (string= "alice" (gethash "name" r1)))
          (ok (string= "bob" (gethash "name" r2)))
          (ok (eq :eof (serdes-protocol:stream-decode-value in))))))))

(deftest stream-quoted-newline
  (let ((raw (format nil "note~C\"a~Cb\"~C" #\Newline #\Newline #\Newline)))
    (with-input-from-string (i raw)
      (let ((in (make-csv-input-stream i :header t)))
        (let ((row (serdes-protocol:stream-decode-value in)))
          (ok (string= (format nil "a~Cb" #\Newline) (gethash "note" row)))
          (ok (eq :eof (serdes-protocol:stream-decode-value in))))))))

(deftest stream-header-nil-vectors
  (let ((raw (with-output-to-string (o)
               (let ((out (make-csv-output-stream o :header nil)))
                 (serdes-protocol:stream-encode-value out #("a" "b"))
                 (serdes-protocol:stream-encode-value out #("1" "2"))))))
    (with-input-from-string (i raw)
      (let ((in (make-csv-input-stream i :header nil)))
        (ok (equalp #("a" "b") (serdes-protocol:stream-decode-value in)))
        (ok (equalp #("1" "2") (serdes-protocol:stream-decode-value in)))))))

(deftest stream-delimiter-override
  (let ((raw (with-output-to-string (o)
               (let ((out (make-csv-output-stream o :header nil :delimiter #\|)))
                 (serdes-protocol:stream-encode-value out #("a" "b"))))))
    (ok (search "a|b" raw))))

(deftest event-parser-sequence
  (let ((parser (make-csv-event-parser (format nil "name,age~Calice,30" #\Newline)))
        (events '()))
    (loop
      (multiple-value-bind (ev val) (serdes-protocol:parse-next-event parser)
        (unless ev (return))
        (push (list ev val) events)))
    (setf events (nreverse events))
    (ok (eq :header (first (first events))))
    (ok (equalp #("name" "age") (second (first events))))
    (ok (eq :begin-row (first (second events))))
    (ok (eq :field (first (third events))))
    (ok (string= "alice" (second (third events))))
    (ok (eq :end-row (first (car (last events)))))))

(deftest event-parse-next-element
  (let ((parser (make-csv-event-parser (format nil "name~Calice~Cbob" #\Newline #\Newline))))
    (let ((r1 (serdes-protocol:parse-next-element parser))
          (r2 (serdes-protocol:parse-next-element parser)))
      (ok (string= "alice" (gethash "name" r1)))
      (ok (string= "bob" (gethash "name" r2)))
      (ok (eq :eof (serdes-protocol:parse-next-element parser))))))

(deftest event-header-nil
  (let ((parser (make-csv-event-parser "a,b" :header nil))
        (events '()))
    (loop
      (multiple-value-bind (ev val) (serdes-protocol:parse-next-event parser)
        (declare (ignore val))
        (unless ev (return))
        (push ev events)))
    (ok (find :begin-row events))
    (ok (not (find :header events)))))
