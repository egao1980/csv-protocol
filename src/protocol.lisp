(in-package #:csv-protocol)

(defvar *csv-backend* nil
  "Current CSV backend (CSV-BACKEND). Bound at load.")

(defclass csv-backend ()
  ((dialect :initarg :dialect :initform nil :accessor csv-backend-dialect)
   (header :initarg :header :initform t :accessor csv-backend-header))
  (:documentation "Holds default dialect designator and :header reader option."))

(defun make-csv-backend (&key dialect (header t))
  (make-instance 'csv-backend :dialect dialect :header header))

(defun %call-with-input (source function)
  (etypecase source
    (stream
     (funcall function source))
    (string
     (with-input-from-string (in source)
       (funcall function in)))
    ((vector (unsigned-byte 8))
     (with-input-from-string (in (babel:octets-to-string source :encoding :utf-8))
       (funcall function in)))
    (pathname
     (with-open-file (in source :direction :input :external-format :utf-8)
       (funcall function in)))))

(defun %key-string (key)
  (etypecase key
    (string key)
    (symbol (string-downcase (symbol-name key)))))

(defun %alist-p (value)
  (and (consp value)
       (every (lambda (entry)
                (and (consp entry)
                     (or (stringp (car entry)) (symbolp (car entry)))))
              value)))

(defun %plist-p (value)
  (and (listp value)
       (evenp (length value))
       (plusp (length value))
       (loop for tail on value by #'cddr
             for key = (first tail)
             always (symbolp key))))

(defun %row-mapping-p (row)
  (or (hash-table-p row)
      (%alist-p row)
      (and (not (stringp row))
           (listp row)
           (%plist-p row))))

(defun %row-keys (row)
  (cond
    ((hash-table-p row)
     (loop for key being the hash-keys of row
           collect (%key-string key)))
    ((%alist-p row)
     (mapcar (lambda (entry) (%key-string (car entry))) row))
    ((%plist-p row)
     (loop for (key) on row by #'cddr
           collect (%key-string key)))
    (t nil)))

(defun %row-get (row key)
  (cond
    ((hash-table-p row)
     (or (gethash key row)
         (let ((found nil)
               (hit nil))
           (maphash (lambda (k v)
                      (when (string= (%key-string k) key)
                        (setf found v hit t)))
                    row)
           (if hit found ""))))
    ((%alist-p row)
     (let ((hit (find key row :key (lambda (e) (%key-string (car e))) :test #'string=)))
       (if hit (cdr hit) "")))
    ((%plist-p row)
     (or (loop for (k v) on row by #'cddr
               when (string= (%key-string k) key)
                 do (return v))
         ""))
    (t "")))

(defun %ensure-row-sequence (value)
  (when (or (stringp value)
            (hash-table-p value)
            (and (consp value) (not (listp (cdr value)))))
    (error 'csv-encode-error
           :message "encode expects a sequence of rows (not a single row)"))
  (unless (typep value 'sequence)
    (error 'csv-encode-error
           :message (format nil "encode expects a sequence of rows, got ~S"
                            (type-of value))))
  value)

(defun %row-fields (row header-names)
  (cond
    ((%row-mapping-p row)
     (mapcar (lambda (key) (%row-get row key)) header-names))
    ((or (vectorp row) (listp row))
     (let* ((vals (coerce row 'list))
            (n (length header-names)))
       (if (and header-names (plusp n))
           (loop for i from 0 below n
                 collect (if (< i (length vals)) (nth i vals) ""))
           vals)))
    (t
     (error 'csv-encode-error
            :message (format nil "not a CSV row: ~S" (type-of row))))))

(defun %collect-header-names (rows)
  (let ((seen (make-hash-table :test #'equal))
        (names nil))
    (dolist (row rows)
      (when (%row-mapping-p row)
        (dolist (key (%row-keys row))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (push key names)))))
    (nreverse names)))

(defun %explicit-header-p (header)
  (and header
       (not (eq header t))
       (or (vectorp header) (listp header))))

(defun %prepare-encode (rows header)
  (let* ((row-list (coerce rows 'list))
         (first (first row-list))
         (names (cond
                  ((%explicit-header-p header)
                   (mapcar #'%key-string (coerce header 'list)))
                  ((and first (%row-mapping-p first))
                   (%collect-header-names row-list))
                  (t nil)))
         (emit-header (cond
                        ((null header) nil)
                        ((%explicit-header-p header) t)
                        ((eq header t) (and names t))
                        (t nil))))
    (values names row-list emit-header)))

(defun %header-names-from-record (record dialect)
  (mapcar (lambda (field) (%field-string (coerce-field field dialect))) record))

(defun %row-from-values (values header-names)
  (if header-names
      (let ((table (make-hash-table :test #'equal)))
        (loop for key in header-names
              for value in values
              do (setf (gethash key table) value))
        table)
      (coerce values 'vector)))

(defun %read-all-records (stream dialect)
  (let ((acc nil))
    (loop
      (let ((record (read-csv-record stream dialect)))
        (cond
          ((eq record :eof) (return (nreverse acc)))
          ((eq record :skip))
          (t (push record acc)))))))

(defun %records-to-document (records header dialect)
  (cond
    ((null records)
     #())
    ((null header)
     (map 'vector
          (lambda (record) (coerce (coerce-record record dialect) 'vector))
          records))
    ((%explicit-header-p header)
     (let ((names (mapcar #'%key-string (coerce header 'list))))
       (map 'vector
            (lambda (record)
              (%row-from-values (coerce-record record dialect) names))
            records)))
    (t
     (let ((names (%header-names-from-record (first records) dialect)))
       (map 'vector
            (lambda (record)
              (%row-from-values (coerce-record record dialect) names))
            (rest records))))))

(defun %resolve-for-call (backend dialect overrides)
  (apply #'resolve-csv-dialect
         (or dialect
             (and backend (csv-backend-dialect backend))
             *csv-dialect*)
         overrides))

(defun encode (value &key stream dialect
                       (header (if *csv-backend* (csv-backend-header *csv-backend*) t))
                       (delimiter nil delimiter-p)
                       (quote-char nil quote-char-p)
                       (escape-char nil escape-char-p)
                       (double-quote nil double-quote-p)
                       (skip-initial-space nil skip-initial-space-p)
                       (line-terminator nil line-terminator-p)
                       (quoting nil quoting-p))
  "Encode a sequence of rows. Returns a string unless STREAM is supplied."
  (let* ((overrides (append (when delimiter-p (list :delimiter delimiter))
                            (when quote-char-p (list :quote-char quote-char))
                            (when escape-char-p (list :escape-char escape-char))
                            (when double-quote-p (list :double-quote double-quote))
                            (when skip-initial-space-p
                              (list :skip-initial-space skip-initial-space))
                            (when line-terminator-p (list :line-terminator line-terminator))
                            (when quoting-p (list :quoting quoting))))
         (dialect (%resolve-for-call *csv-backend* dialect overrides))
         (rows (%ensure-row-sequence value)))
    (multiple-value-bind (header-names data-rows emit-header)
        (%prepare-encode rows header)
      (flet ((write-doc (out)
               (when emit-header
                 (write-csv-record out header-names dialect))
               (dolist (row data-rows)
                 (write-csv-record out (%row-fields row header-names) dialect))))
        (if stream
            (progn
              (write-doc stream)
              (values))
            (with-output-to-string (out)
              (write-doc out)))))))

(defun decode (source &key dialect
                        (header (if *csv-backend* (csv-backend-header *csv-backend*) t))
                        (delimiter nil delimiter-p)
                        (quote-char nil quote-char-p)
                        (escape-char nil escape-char-p)
                        (double-quote nil double-quote-p)
                        (skip-initial-space nil skip-initial-space-p)
                        (line-terminator nil line-terminator-p)
                        (quoting nil quoting-p))
  "Decode SOURCE (string, octets, stream, or pathname) to a vector of rows."
  (let* ((overrides (append (when delimiter-p (list :delimiter delimiter))
                            (when quote-char-p (list :quote-char quote-char))
                            (when escape-char-p (list :escape-char escape-char))
                            (when double-quote-p (list :double-quote double-quote))
                            (when skip-initial-space-p
                              (list :skip-initial-space skip-initial-space))
                            (when line-terminator-p (list :line-terminator line-terminator))
                            (when quoting-p (list :quoting quoting))))
         (dialect (%resolve-for-call *csv-backend* dialect overrides)))
    (%call-with-input
     source
     (lambda (in)
       (skip-utf8-bom in)
       (%records-to-document (%read-all-records in dialect) header dialect)))))

(defun encode-to-octets (value &rest args &key &allow-other-keys)
  "UTF-8 octets of (ENCODE VALUE …)."
  (babel:string-to-octets (apply #'encode value args) :encoding :utf-8))

(defun decode-octets (octets &rest args &key &allow-other-keys)
  "DECODE UTF-8 OCTETS."
  (apply #'decode octets args))
