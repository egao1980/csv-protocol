(in-package #:csv-protocol/tests)

(defun %ht (&rest pairs)
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          do (setf (gethash k h) v))
    h))

(defun %row-get (row key)
  (gethash key row))

(deftest-parametrize decode-rfc-vectors
    ((input header expected-first)
     ("name,age" t nil)
     ("name,age\r\nalice,30" t ("alice" "30"))
     ("name,age\nalice,30" t ("alice" "30"))
     ("1,2" nil (#("1" "2")))
     ("a,\"b,c\"" nil (#("a" "b,c")))
     ("\"a\"\"b\",c" nil (#("a\"b" "c")))
     (",," nil (#("" "" "")))
     ("a," nil (#("a" ""))))
  (let ((doc (decode input :header header)))
    (if (null expected-first)
        (ok (zerop (length doc)))
        (if header
            (let ((row (aref doc 0)))
              (ok (hash-table-p row))
              (ok (string= (first expected-first) (%row-get row "name")))
              (when (second expected-first)
                (ok (string= (second expected-first) (%row-get row "age")))))
            (ok (equalp expected-first (list (aref doc 0))))))))

(deftest quoted-embedded-newline
  (let ((doc (decode (format nil "name,note~C\"alice\",\"hello~Cworld\"" #\Newline #\Newline)
                     :header t)))
    (ok (= 1 (length doc)))
    (ok (string= (format nil "hello~Cworld" #\Newline) (%row-get (aref doc 0) "note")))))

(deftest crlf-record-terminator
  (let ((doc (decode (coerce '(#\a #\, #\b #\Return #\Newline #\c #\, #\d) 'string)
                     :header nil)))
    (ok (= 2 (length doc)))
    (ok (equalp #("a" "b") (aref doc 0)))
    (ok (equalp #("c" "d") (aref doc 1)))))

(deftest utf8-and-bom
  (let* ((text (format nil "~Cname,city~C東京,大阪" #\ufeff #\Newline))
         (doc (decode text :header t)))
    (ok (= 1 (length doc)))
    (ok (string= "東京" (%row-get (aref doc 0) "name")))
    (ok (string= "大阪" (%row-get (aref doc 0) "city")))))

(deftest octets-roundtrip
  (let* ((rows (vector (%ht "x" "1")))
         (octets (encode-to-octets rows))
         (decoded (decode-octets octets)))
    (ok (string= "1" (%row-get (aref decoded 0) "x")))))

(deftest encode-alist-header
  (let ((out (encode (list '(("name" . "alice") ("age" . "30"))))))
    (ok (search "name" out))
    (ok (search "alice" out))))

(deftest encode-vector-no-header
  (let ((out (encode (list #("a" "b") #("1" "2")) :header nil)))
    (ok (string= (format nil "a,b~C~C1,2~C~C" #\Return #\Newline #\Return #\Newline) out))))

(deftest encode-lone-hash-table-errors
  (ok (signals (encode (%ht "a" "1")) 'csv-encode-error)))

(deftest empty-source
  (ok (equalp #() (decode "" :header t)))
  (ok (equalp #() (decode "" :header nil))))

(deftest unclosed-quote
  (ok (signals (decode "\"abc" :header nil) 'csv-parse-error)))

(deftest use-value-restart
  (let ((doc (handler-bind ((csv-parse-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'use-value '("patched")))))
               (decode "\"abc" :header nil))))
    (ok (equalp #("patched") (aref doc 0)))))

(deftest continue-skip-row
  (let ((doc (handler-bind ((csv-parse-error
                             (lambda (c)
                               (declare (ignore c))
                               (invoke-restart 'continue))))
               (decode (format nil "\"abc\"xxx~Cok,yes" #\Newline) :header nil))))
    (ok (= 1 (length doc)))
    (ok (equalp #("ok" "yes") (aref doc 0)))))

(deftest excel-eu-delimiter
  (let ((doc (decode "a;b;c" :dialect :excel-eu :header nil)))
    (ok (equalp #("a" "b" "c") (aref doc 0)))))

(deftest tsv-dialect
  (let ((doc (decode (format nil "a~Cb" #\Tab) :dialect :tsv :header nil)))
    (ok (equalp #("a" "b") (aref doc 0)))))

(deftest per-call-delimiter
  (let ((doc (decode "a|b|c" :delimiter #\| :header nil)))
    (ok (equalp #("a" "b" "c") (aref doc 0)))))

(deftest skip-initial-space
  (let ((doc (decode "a, b,  c" :skip-initial-space t :header nil)))
    (ok (equalp #("a" "b" "c") (aref doc 0)))))

(deftest quoting-none-escape
  (let ((out (encode (list #("a,b" "c"))
                     :header nil
                     :quoting :none
                     :escape-char #\\)))
    (ok (search "a\\,b" out))))

(deftest quoting-all-unix
  (let ((out (encode (list #("a" "b")) :header nil :dialect :unix)))
    (ok (string= (format nil "\"a\",\"b\"~%" ) out))))

(deftest nonnumeric-decode
  (let ((doc (decode "1,\"x\",2.5" :header nil :quoting :nonnumeric)))
    (ok (= 1 (aref (aref doc 0) 0)))
    (ok (string= "x" (aref (aref doc 0) 1)))
    (ok (= 2.5d0 (aref (aref doc 0) 2)))))

(deftest hash-table-roundtrip
  (let* ((rows (vector (%ht "name" "bob") (%ht "name" "ann" "extra" "z")))
         (decoded (decode (encode rows))))
    (ok (string= "bob" (%row-get (aref decoded 0) "name")))
    (ok (string= "ann" (%row-get (aref decoded 1) "name")))
    (ok (string= "z" (%row-get (aref decoded 1) "extra")))))

(deftest explicit-header-names
  (let ((doc (decode "1,2" :header '("id" "n"))))
    (ok (string= "1" (%row-get (aref doc 0) "id")))
    (ok (string= "2" (%row-get (aref doc 0) "n")))))

(deftest star-dialect-binding
  (let ((*csv-dialect* :excel-eu)
        (out (encode (list #("a" "b")) :header nil)))
    (ok (search "a;b" out))))
