(in-package #:csv-protocol/tests)

(deftest serdes-csv-roundtrip
  (let* ((rows (vector (let ((h (make-hash-table :test #'equal)))
                         (setf (gethash "k" h) "v")
                         h)))
         (decoded (serdes-protocol:decode
                   (serdes-protocol:encode rows :format :csv)
                   :format :csv)))
    (ok (string= "v" (gethash "k" (aref decoded 0))))))

(deftest serdes-tsv-format
  (let ((raw (serdes-protocol:encode (list #("a" "b")) :format :tsv)))
    (ok (search (string #\Tab) raw))
    (ok (equalp #("a" "b") (aref (decode raw :dialect :tsv :header nil) 0)))))

(deftest serdes-star-dialect
  (let* ((*csv-dialect* :excel-eu)
         (raw (serdes-protocol:encode (list #("a" "b")) :format :csv)))
    (ok (search "a;b" raw))))

(deftest serdes-stream-via-format
  (let ((raw (with-output-to-string (o)
               (let ((out (serdes-protocol:make-output-stream o :format :csv)))
                 (serdes-protocol:stream-encode-value out '(("x" . "1")))))))
    (with-input-from-string (i raw)
      (let ((in (serdes-protocol:make-input-stream i :format :csv)))
        (ok (string= "1" (gethash "x" (serdes-protocol:stream-decode-value in))))))))

(deftest serdes-event-parser
  (let ((parser (serdes-protocol:make-event-parser
                 (format nil "h~Cv" #\Newline) :format :csv))
        (seen nil))
    (loop
      (multiple-value-bind (ev val) (serdes-protocol:parse-next-event parser)
        (declare (ignore val))
        (unless ev (return))
        (push ev seen)))
    (ok (find :header seen))
    (ok (find :field seen))))

(deftest serdes-octets
  (let* ((rows (vector (let ((h (make-hash-table :test #'equal)))
                         (setf (gethash "n" h) "9")
                         h)))
         (octets (serdes-protocol:encode-to-octets rows :format :csv)))
    (ok (string= "9" (gethash "n" (aref (serdes-protocol:decode-octets octets :format :csv)
                                        0))))))

(deftest serdes-decode-error-wraps
  (ok (signals (serdes-protocol:decode "\"abc" :format :csv)
               'serdes-protocol:serdes-decode-error)))
