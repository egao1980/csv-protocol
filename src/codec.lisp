(in-package #:csv-protocol)

(defun skip-utf8-bom (stream)
  (let ((c (peek-char nil stream nil nil)))
    (when (and c (char= c #\ufeff))
      (read-char stream)))
  (values))

(defun %record-terminator-p (char)
  (or (char= char #\Newline) (char= char #\Return)))

(defun %consume-record-terminator (stream first-char)
  (when (and (char= first-char #\Return)
             (eql (peek-char nil stream nil nil) #\Newline))
    (read-char stream))
  (values))

(defun %resync-to-next-record (stream)
  (loop for c = (read-char stream nil nil)
        while c
        when (%record-terminator-p c)
          do (%consume-record-terminator stream c)
             (return))
  (values))

(defun %push-field (fields buf quoted-p)
  (cons (cons (copy-seq buf) quoted-p) fields))

(defun %read-escaped (stream)
  (or (read-char stream nil nil)
      (error 'csv-parse-error :message "truncated escape at end of input")))

(defun %read-csv-record (stream dialect)
  (let* ((delimiter (csv-dialect-delimiter dialect))
         (quote-char (csv-dialect-quote-char dialect))
         (escape-char (csv-dialect-escape-char dialect))
         (double-quote (csv-dialect-double-quote dialect))
         (skip-space (csv-dialect-skip-initial-space dialect))
         (buf (make-array 16 :element-type 'character :adjustable t :fill-pointer 0))
         (fields nil)
         (quoted-p nil)
         (state :field-start)
         (saw-char nil))
    (labels ((clear-field ()
               (setf (fill-pointer buf) 0
                     quoted-p nil))
             (add (c)
               (vector-push-extend c buf))
             (finish-field ()
               (setf fields (%push-field fields buf quoted-p))
               (clear-field)
               (setf state :field-start))
             (finish-record ()
               (nreverse fields)))
      (loop
        (let ((c (read-char stream nil nil)))
          (unless c
            (return
              (cond
                ((eq state :in-quoted)
                 (error 'csv-parse-error :message "unclosed quoted field"))
                ((and (not saw-char) (null fields))
                 :eof)
                (t
                 (finish-field)
                 (finish-record)))))
          (setf saw-char t)
          (case state
            (:field-start
             (cond
               ((and skip-space (char= c #\Space))
                nil)
               ((%record-terminator-p c)
                (%consume-record-terminator stream c)
                (finish-field)
                (return (finish-record)))
               ((char= c delimiter)
                (finish-field))
               ((and quote-char (char= c quote-char))
                (setf quoted-p t
                      state :in-quoted))
               ((and escape-char (char= c escape-char))
                (add (%read-escaped stream))
                (setf state :in-field))
               (t
                (add c)
                (setf state :in-field))))
            (:in-field
             (cond
               ((and escape-char (char= c escape-char))
                (add (%read-escaped stream)))
               ((%record-terminator-p c)
                (%consume-record-terminator stream c)
                (finish-field)
                (return (finish-record)))
               ((char= c delimiter)
                (finish-field))
               (t
                (add c))))
            (:in-quoted
             (cond
               ((and escape-char (char= c escape-char))
                (add (%read-escaped stream)))
               ((and quote-char (char= c quote-char))
                (if (and double-quote
                         (eql (peek-char nil stream nil nil) quote-char))
                    (progn
                      (read-char stream)
                      (add quote-char))
                    (setf state :after-quoted)))
               (t
                (add c))))
            (:after-quoted
             (cond
               ((char= c #\Space)
                nil)
               ((%record-terminator-p c)
                (%consume-record-terminator stream c)
                (finish-field)
                (return (finish-record)))
               ((char= c delimiter)
                (finish-field))
               (t
                (error 'csv-parse-error
                       :message (format nil "data after closing quote: ~S" c)))))))))))

(defun %coerce-use-record (value)
  (cond
    ((eq value :skip) :skip)
    ((eq value :eof) :eof)
    ((listp value)
     (mapcar (lambda (item)
               (if (and (consp item) (stringp (car item)))
                   item
                   (cons (if (stringp item) item (princ-to-string item)) nil)))
             value))
    (t (error 'csv-parse-error :message "use-value expects a list of fields"))))

(defun read-csv-record (stream dialect)
  "Read one record from STREAM.
   Returns a list of (string . quoted-p), :eof, or :skip."
  (restart-case
      (%read-csv-record stream dialect)
    (use-value (value)
      :report "Use a supplied record (list of strings) instead"
      :interactive (lambda ()
                     (format *query-io* "Record (list of strings): ")
                     (force-output *query-io*)
                     (list (read *query-io*)))
      (%coerce-use-record value))
    (continue ()
      :report "Skip this record"
      (%resync-to-next-record stream)
      :skip)))

(defun %try-number (string)
  (let* ((trimmed (string-trim '(#\Space #\Tab) string)))
    (when (plusp (length trimmed))
      (or (ignore-errors (parse-integer trimmed :junk-allowed nil))
          (let ((*read-eval* nil)
                (*read-default-float-format* 'double-float))
            (multiple-value-bind (obj pos)
                (ignore-errors (read-from-string trimmed))
              (when (and (numberp obj) (eql pos (length trimmed)))
                obj)))))))

(defun coerce-field (field-info dialect)
  (destructuring-bind (string . quoted-p) field-info
    (if (and (eq (csv-dialect-quoting dialect) :nonnumeric)
             (not quoted-p)
             (plusp (length string)))
        (or (%try-number string) string)
        string)))

(defun coerce-record (record dialect)
  (mapcar (lambda (field) (coerce-field field dialect)) record))

(defun %field-string (value)
  (cond
    ((stringp value) value)
    ((null value) "")
    (t (princ-to-string value))))

(defun %needs-minimal-quote-p (string dialect)
  (let ((delimiter (csv-dialect-delimiter dialect))
        (quote-char (csv-dialect-quote-char dialect)))
    (loop for c across string
          thereis (or (char= c delimiter)
                      (and quote-char (char= c quote-char))
                      (char= c #\Newline)
                      (char= c #\Return)))))

(defun %quote-field-p (value string dialect)
  (ecase (csv-dialect-quoting dialect)
    (:all t)
    (:none nil)
    (:minimal (%needs-minimal-quote-p string dialect))
    (:nonnumeric (not (numberp value)))))

(defun write-csv-field (stream value dialect)
  (let* ((string (%field-string value))
         (quote-char (csv-dialect-quote-char dialect))
         (escape-char (csv-dialect-escape-char dialect))
         (double-quote (csv-dialect-double-quote dialect))
         (delimiter (csv-dialect-delimiter dialect)))
    (cond
      ((%quote-field-p value string dialect)
       (write-char quote-char stream)
       (loop for c across string
             do (cond
                  ((and quote-char (char= c quote-char) double-quote)
                   (write-char quote-char stream)
                   (write-char quote-char stream))
                  ((and quote-char (char= c quote-char) escape-char)
                   (write-char escape-char stream)
                   (write-char c stream))
                  (t
                   (write-char c stream))))
       (write-char quote-char stream))
      ((eq (csv-dialect-quoting dialect) :none)
       (loop for c across string
             do (when (or (char= c delimiter)
                          (and quote-char (char= c quote-char))
                          (char= c #\Newline)
                          (char= c #\Return)
                          (and escape-char (char= c escape-char)))
                  (write-char escape-char stream))
                (write-char c stream)))
      (t
       (write-string string stream)))))

(defun write-csv-record (stream fields dialect)
  (let ((first t))
    (dolist (field fields)
      (unless first
        (write-char (csv-dialect-delimiter dialect) stream))
      (setf first nil)
      (write-csv-field stream field dialect))
    (write-string (csv-dialect-line-terminator dialect) stream))
  (values))
