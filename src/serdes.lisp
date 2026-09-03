(in-package #:csv-protocol)

(defclass csv-serdes-backend (serdes-protocol:serdes-backend csv-backend) ()
  (:documentation "serdes-protocol implementor for :csv / :tsv."))

(defun make-csv-serdes-backend (&key dialect (header t))
  (make-instance 'csv-serdes-backend :dialect dialect :header header))

(defun %backend-dialect (backend)
  (or (csv-backend-dialect backend) *csv-dialect*))

(defmethod serdes-protocol:backend-encode ((backend csv-serdes-backend) value &key stream)
  (encode value :stream stream
                :dialect (%backend-dialect backend)
                :header (csv-backend-header backend)))

(defmethod serdes-protocol:backend-decode ((backend csv-serdes-backend) source &key)
  (decode source :dialect (%backend-dialect backend)
                 :header (csv-backend-header backend)))

(defclass csv-character-input-stream (serdes-protocol:serdes-character-input-stream)
  ((dialect :initarg :dialect :reader stream-dialect)
   (header :initarg :header :initform t :reader stream-header)
   (header-names :initform nil :accessor stream-header-names)
   (header-consumed :initform nil :accessor stream-header-consumed)
   (bom-skipped :initform nil :accessor stream-bom-skipped)))

(defclass csv-character-output-stream (serdes-protocol:serdes-character-output-stream)
  ((dialect :initarg :dialect :reader stream-dialect)
   (header :initarg :header :initform t :reader stream-header)
   (header-names :initform nil :accessor stream-header-names)
   (header-written :initform nil :accessor stream-header-written)))

(defun %ensure-character-element-type (element-type)
  (unless (subtypep element-type 'character)
    (error 'csv-error
           :message (format nil "csv streams are character, got ~S" element-type))))

(defmethod serdes-protocol:backend-make-input-stream ((backend csv-serdes-backend)
                                                      underlying
                                                      &key (element-type 'character))
  (%ensure-character-element-type element-type)
  (make-instance 'csv-character-input-stream
                 :underlying underlying
                 :backend backend
                 :dialect (resolve-csv-dialect (%backend-dialect backend))
                 :header (csv-backend-header backend)))

(defmethod serdes-protocol:backend-make-output-stream ((backend csv-serdes-backend)
                                                       underlying
                                                       &key (element-type 'character))
  (%ensure-character-element-type element-type)
  (make-instance 'csv-character-output-stream
                 :underlying underlying
                 :backend backend
                 :dialect (resolve-csv-dialect (%backend-dialect backend))
                 :header (csv-backend-header backend)))

(defun make-csv-input-stream (underlying &key dialect
                                           (header nil header-p)
                                           (element-type 'character)
                                           (delimiter nil delimiter-p)
                                           (quote-char nil quote-char-p)
                                           (escape-char nil escape-char-p)
                                           (double-quote nil double-quote-p)
                                           (skip-initial-space nil skip-initial-space-p)
                                           (line-terminator nil line-terminator-p)
                                           (quoting nil quoting-p))
  (%ensure-character-element-type element-type)
  (let* ((overrides (append (when delimiter-p (list :delimiter delimiter))
                            (when quote-char-p (list :quote-char quote-char))
                            (when escape-char-p (list :escape-char escape-char))
                            (when double-quote-p (list :double-quote double-quote))
                            (when skip-initial-space-p
                              (list :skip-initial-space skip-initial-space))
                            (when line-terminator-p (list :line-terminator line-terminator))
                            (when quoting-p (list :quoting quoting))))
         (resolved (apply #'resolve-csv-dialect
                          (or dialect
                              (and *csv-backend* (csv-backend-dialect *csv-backend*))
                              *csv-dialect*)
                          overrides))
         (hdr (if header-p
                  header
                  (if *csv-backend* (csv-backend-header *csv-backend*) t))))
    (make-instance 'csv-character-input-stream
                   :underlying underlying
                   :backend (or *csv-backend* (make-csv-serdes-backend :dialect resolved
                                                                       :header hdr))
                   :dialect resolved
                   :header hdr)))

(defun make-csv-output-stream (underlying &key dialect
                                            (header nil header-p)
                                            (element-type 'character)
                                            (delimiter nil delimiter-p)
                                            (quote-char nil quote-char-p)
                                            (escape-char nil escape-char-p)
                                            (double-quote nil double-quote-p)
                                            (skip-initial-space nil skip-initial-space-p)
                                            (line-terminator nil line-terminator-p)
                                            (quoting nil quoting-p))
  (%ensure-character-element-type element-type)
  (let* ((overrides (append (when delimiter-p (list :delimiter delimiter))
                            (when quote-char-p (list :quote-char quote-char))
                            (when escape-char-p (list :escape-char escape-char))
                            (when double-quote-p (list :double-quote double-quote))
                            (when skip-initial-space-p
                              (list :skip-initial-space skip-initial-space))
                            (when line-terminator-p (list :line-terminator line-terminator))
                            (when quoting-p (list :quoting quoting))))
         (resolved (apply #'resolve-csv-dialect
                          (or dialect
                              (and *csv-backend* (csv-backend-dialect *csv-backend*))
                              *csv-dialect*)
                          overrides))
         (hdr (if header-p
                  header
                  (if *csv-backend* (csv-backend-header *csv-backend*) t))))
    (when (%explicit-header-p hdr)
      (setf hdr (mapcar #'%key-string (coerce hdr 'list))))
    (make-instance 'csv-character-output-stream
                   :underlying underlying
                   :backend (or *csv-backend* (make-csv-serdes-backend :dialect resolved
                                                                       :header hdr))
                   :dialect resolved
                   :header hdr)))

(defun %maybe-skip-bom (stream)
  (unless (stream-bom-skipped stream)
    (skip-utf8-bom (serdes-protocol:underlying-stream stream))
    (setf (stream-bom-skipped stream) t)))

(defun %record-to-row (record dialect header-names)
  (%row-from-values (coerce-record record dialect) header-names))

(defmethod serdes-protocol:stream-decode-value ((stream csv-character-input-stream) &key)
  (%maybe-skip-bom stream)
  (loop
    (let ((record (read-csv-record (serdes-protocol:underlying-stream stream)
                                   (stream-dialect stream))))
      (cond
        ((eq record :eof) (return :eof))
        ((eq record :skip))
        ((and (stream-header stream)
              (not (stream-header-consumed stream)))
         (setf (stream-header-names stream)
               (if (%explicit-header-p (stream-header stream))
                   (mapcar #'%key-string (coerce (stream-header stream) 'list))
                   (%header-names-from-record record (stream-dialect stream)))
               (stream-header-consumed stream) t)
         (when (%explicit-header-p (stream-header stream))
           (return (%record-to-row record (stream-dialect stream)
                                   (stream-header-names stream)))))
        (t
         (return (%record-to-row record (stream-dialect stream)
                                 (if (stream-header stream)
                                     (stream-header-names stream)
                                     nil))))))))

(defmethod serdes-protocol:stream-encode-value ((stream csv-character-output-stream) value &key)
  (let* ((dialect (stream-dialect stream))
         (out (serdes-protocol:underlying-stream stream))
         (header (stream-header stream)))
    (unless (stream-header-written stream)
      (cond
        ((%explicit-header-p header)
         (let ((names (mapcar #'%key-string (coerce header 'list))))
           (setf (stream-header-names stream) names)
           (write-csv-record out names dialect)))
        ((and header (%row-mapping-p value))
         (let ((names (%row-keys value)))
           (setf (stream-header-names stream) names)
           (write-csv-record out names dialect)))
        (t
         (when (%row-mapping-p value)
           (setf (stream-header-names stream) (%row-keys value)))))
      (setf (stream-header-written stream) t))
    (write-csv-record out (%row-fields value (stream-header-names stream)) dialect)
    value))

(defclass csv-event-parser (serdes-protocol:serdes-event-parser)
  ((input :initarg :input :reader event-parser-input)
   (close-fn :initarg :close-fn :initform (constantly nil) :reader event-parser-close-fn)
   (dialect :initarg :dialect :reader event-parser-dialect)
   (header :initarg :header :reader event-parser-header)
   (header-names :initform nil :accessor event-parser-header-names)
   (started :initform nil :accessor event-parser-started)
   (queue :initform (make-array 8 :adjustable t :fill-pointer 0) :reader event-parser-queue)
   (qhead :initform 0 :accessor event-parser-qhead)))

(defun %event-enqueue (parser event value)
  (vector-push-extend (cons event value) (event-parser-queue parser)))

(defun %event-dequeue (parser)
  (let ((queue (event-parser-queue parser))
        (head (event-parser-qhead parser)))
    (when (< head (fill-pointer queue))
      (let ((cell (aref queue head)))
        (incf (event-parser-qhead parser))
        (when (= (event-parser-qhead parser) (fill-pointer queue))
          (setf (fill-pointer queue) 0
                (event-parser-qhead parser) 0))
        (values (car cell) (cdr cell))))))

(defun %open-event-source (source)
  (etypecase source
    (stream (values source (constantly nil)))
    (string
     (let ((s (make-string-input-stream source)))
       (values s (lambda () (close s)))))
    ((vector (unsigned-byte 8))
     (let ((s (make-string-input-stream
               (babel:octets-to-string source :encoding :utf-8))))
       (values s (lambda () (close s)))))
    (pathname
     (let ((s (open source :direction :input :external-format :utf-8)))
       (values s (lambda () (close s)))))))

(defun %refill-events (parser)
  (let ((record (read-csv-record (event-parser-input parser)
                                 (event-parser-dialect parser))))
    (cond
      ((eq record :eof) nil)
      ((eq record :skip) t)
      ((and (event-parser-header parser)
            (not (event-parser-started parser)))
       (setf (event-parser-started parser) t)
       (let ((names (if (%explicit-header-p (event-parser-header parser))
                        (mapcar #'%key-string (coerce (event-parser-header parser) 'list))
                        (%header-names-from-record record (event-parser-dialect parser)))))
         (setf (event-parser-header-names parser) names)
         (%event-enqueue parser :header (coerce names 'vector))
         (when (%explicit-header-p (event-parser-header parser))
           (%event-enqueue parser :begin-row nil)
           (dolist (field (coerce-record record (event-parser-dialect parser)))
             (%event-enqueue parser :field field))
           (%event-enqueue parser :end-row nil))
         t))
      (t
       (setf (event-parser-started parser) t)
       (%event-enqueue parser :begin-row nil)
       (dolist (field (coerce-record record (event-parser-dialect parser)))
         (%event-enqueue parser :field field))
       (%event-enqueue parser :end-row nil)
       t))))

(defmethod serdes-protocol:parse-next-event ((parser csv-event-parser))
  (loop
    (multiple-value-bind (event value) (%event-dequeue parser)
      (when event
        (return (values event value))))
    (unless (%refill-events parser)
      (return (values nil nil)))))

(defmethod serdes-protocol:parse-next-element ((parser csv-event-parser) &key)
  (let ((fields nil)
        (collecting nil))
    (loop
      (multiple-value-bind (event value) (serdes-protocol:parse-next-event parser)
        (cond
          ((null event)
           (return :eof))
          ((eq event :header)
           (setf (event-parser-header-names parser) (coerce value 'list)))
          ((eq event :begin-row)
           (setf fields nil collecting t))
          ((eq event :field)
           (when collecting
             (push value fields)))
          ((eq event :end-row)
           (return (%row-from-values (nreverse fields)
                                     (if (event-parser-header parser)
                                         (event-parser-header-names parser)
                                         nil)))))))))

(defun %make-csv-event-parser (source dialect header)
  (multiple-value-bind (in close) (%open-event-source source)
    (skip-utf8-bom in)
    (make-instance 'csv-event-parser
                   :backend (or *csv-backend* (make-csv-serdes-backend :dialect dialect
                                                                       :header header))
                   :source source
                   :input in
                   :close-fn close
                   :dialect (resolve-csv-dialect dialect)
                   :header header)))

(defmethod serdes-protocol:backend-make-event-parser ((backend csv-serdes-backend) source
                                                      &key max-depth max-string-length)
  (declare (ignore max-depth max-string-length))
  (%make-csv-event-parser source (%backend-dialect backend) (csv-backend-header backend)))

(defun make-csv-event-parser (source &key dialect
                                       (header nil header-p)
                                       (delimiter nil delimiter-p)
                                       (quote-char nil quote-char-p)
                                       (escape-char nil escape-char-p)
                                       (double-quote nil double-quote-p)
                                       (skip-initial-space nil skip-initial-space-p)
                                       (line-terminator nil line-terminator-p)
                                       (quoting nil quoting-p))
  (let* ((overrides (append (when delimiter-p (list :delimiter delimiter))
                            (when quote-char-p (list :quote-char quote-char))
                            (when escape-char-p (list :escape-char escape-char))
                            (when double-quote-p (list :double-quote double-quote))
                            (when skip-initial-space-p
                              (list :skip-initial-space skip-initial-space))
                            (when line-terminator-p (list :line-terminator line-terminator))
                            (when quoting-p (list :quoting quoting))))
         (resolved (apply #'resolve-csv-dialect
                          (or dialect
                              (and *csv-backend* (csv-backend-dialect *csv-backend*))
                              *csv-dialect*)
                          overrides))
         (hdr (if header-p
                  header
                  (if *csv-backend* (csv-backend-header *csv-backend*) t))))
    (%make-csv-event-parser source resolved hdr)))

(defun use-csv-backend (&key dialect (header t))
  "Bind *CSV-BACKEND* and register :csv. Returns the backend."
  (let ((backend (make-csv-serdes-backend :dialect dialect :header header)))
    (setf *csv-backend* backend)
    (serdes-protocol:register-format :csv backend)
    backend))

(defun use-csv-serdes-backend (&key dialect (header t))
  (use-csv-backend :dialect dialect :header header))

(defun %install-formats ()
  (let ((csv (make-csv-serdes-backend))
        (tsv (make-csv-serdes-backend :dialect :tsv)))
    (setf *csv-backend* csv)
    (serdes-protocol:register-format :csv csv)
    (serdes-protocol:register-format :tsv tsv)
    csv))

(%install-formats)
