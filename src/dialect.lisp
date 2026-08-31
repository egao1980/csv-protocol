(in-package #:csv-protocol)

(defvar *csv-dialects* (make-hash-table :test #'eq)
  "Map dialect keywords to CSV-DIALECT objects.")

(defvar *csv-dialect* :rfc4180
  "Default dialect designator (keyword or CSV-DIALECT).")

(defclass csv-dialect ()
  ((name :initarg :name :initform nil :reader csv-dialect-name)
   (delimiter :initarg :delimiter :initform #\, :reader csv-dialect-delimiter)
   (quote-char :initarg :quote-char :initform #\" :reader csv-dialect-quote-char)
   (escape-char :initarg :escape-char :initform nil :reader csv-dialect-escape-char)
   (double-quote :initarg :double-quote :initform t :reader csv-dialect-double-quote)
   (skip-initial-space :initarg :skip-initial-space :initform nil
                       :reader csv-dialect-skip-initial-space)
   (line-terminator :initarg :line-terminator
                    :initform (coerce '(#\Return #\Newline) 'string)
                    :reader csv-dialect-line-terminator)
   (quoting :initarg :quoting :initform :minimal :reader csv-dialect-quoting))
  (:documentation "Python csv.Dialect-shaped wire settings. Not sql-query-csv:csv-dialect."))

(defun register-dialect (name dialect)
  "Register DIALECT under NAME (keyword). Returns DIALECT."
  (check-type name keyword)
  (check-type dialect csv-dialect)
  (setf (gethash name *csv-dialects*) dialect)
  dialect)

(defun find-dialect (name &optional (errorp t))
  "Return the dialect registered for NAME."
  (let ((normalized (etypecase name
                      (keyword name)
                      (symbol (intern (symbol-name name) :keyword)))))
    (or (gethash normalized *csv-dialects*)
        (if errorp
            (error 'csv-error
                   :message (format nil "unknown CSV dialect ~S" normalized))
            nil))))

(defun %validate-dialect (dialect)
  (let ((delimiter (csv-dialect-delimiter dialect))
        (quote-char (csv-dialect-quote-char dialect))
        (escape-char (csv-dialect-escape-char dialect))
        (quoting (csv-dialect-quoting dialect))
        (line-terminator (csv-dialect-line-terminator dialect)))
    (unless (characterp delimiter)
      (error 'csv-error :message "delimiter must be a character"))
    (unless (or (null quote-char) (characterp quote-char))
      (error 'csv-error :message "quote-char must be a character or NIL"))
    (unless (or (null escape-char) (characterp escape-char))
      (error 'csv-error :message "escape-char must be a character or NIL"))
    (unless (stringp line-terminator)
      (error 'csv-error :message "line-terminator must be a string"))
    (unless (member quoting '(:minimal :all :nonnumeric :none) :test #'eq)
      (error 'csv-error
             :message (format nil "quoting must be :minimal/:all/:nonnumeric/:none, got ~S"
                              quoting)))
    (when (and quote-char (char= delimiter quote-char))
      (error 'csv-error :message "delimiter and quote-char must differ"))
    (when (and escape-char (char= delimiter escape-char))
      (error 'csv-error :message "delimiter and escape-char must differ"))
    (when (and (eq quoting :none) (null escape-char))
      (error 'csv-error :message "quoting :none requires escape-char"))
    (when (and (member quoting '(:minimal :all :nonnumeric) :test #'eq)
               (null quote-char))
      (error 'csv-error
             :message (format nil "quoting ~S requires quote-char" quoting)))
    dialect))

(defun %copy-dialect (base &key (name nil name-p)
                             (delimiter nil delimiter-p)
                             (quote-char nil quote-char-p)
                             (escape-char nil escape-char-p)
                             (double-quote nil double-quote-p)
                             (skip-initial-space nil skip-initial-space-p)
                             (line-terminator nil line-terminator-p)
                             (quoting nil quoting-p))
  (make-instance 'csv-dialect
                 :name (if name-p name (csv-dialect-name base))
                 :delimiter (if delimiter-p delimiter (csv-dialect-delimiter base))
                 :quote-char (if quote-char-p quote-char (csv-dialect-quote-char base))
                 :escape-char (if escape-char-p escape-char (csv-dialect-escape-char base))
                 :double-quote (if double-quote-p double-quote (csv-dialect-double-quote base))
                 :skip-initial-space (if skip-initial-space-p
                                         skip-initial-space
                                         (csv-dialect-skip-initial-space base))
                 :line-terminator (if line-terminator-p
                                      line-terminator
                                      (csv-dialect-line-terminator base))
                 :quoting (if quoting-p quoting (csv-dialect-quoting base))))

(defun resolve-csv-dialect (designator &key (delimiter nil delimiter-p)
                                         (quote-char nil quote-char-p)
                                         (escape-char nil escape-char-p)
                                         (double-quote nil double-quote-p)
                                         (skip-initial-space nil skip-initial-space-p)
                                         (line-terminator nil line-terminator-p)
                                         (quoting nil quoting-p)
                                         (name nil name-p))
  "Resolve DESIGNATOR (NIL / keyword / CSV-DIALECT) plus optional slot overrides."
  (let* ((base (cond
                 ((null designator)
                  (let ((fallback *csv-dialect*))
                    (cond
                      ((null fallback) (find-dialect :rfc4180))
                      ((typep fallback 'csv-dialect) fallback)
                      (t (find-dialect fallback)))))
                 ((typep designator 'csv-dialect) designator)
                 ((keywordp designator) (find-dialect designator))
                 ((symbolp designator)
                  (find-dialect (intern (symbol-name designator) :keyword)))
                 (t (error 'csv-error
                           :message (format nil "not a CSV dialect designator: ~S"
                                            designator)))))
         (overridden (or delimiter-p quote-char-p escape-char-p double-quote-p
                         skip-initial-space-p line-terminator-p quoting-p name-p))
         (dialect (if overridden
                      (%copy-dialect
                       base
                       :name (if name-p name (csv-dialect-name base))
                       :delimiter (if delimiter-p delimiter (csv-dialect-delimiter base))
                       :quote-char (if quote-char-p quote-char (csv-dialect-quote-char base))
                       :escape-char (if escape-char-p escape-char
                                        (csv-dialect-escape-char base))
                       :double-quote (if double-quote-p double-quote
                                         (csv-dialect-double-quote base))
                       :skip-initial-space (if skip-initial-space-p
                                               skip-initial-space
                                               (csv-dialect-skip-initial-space base))
                       :line-terminator (if line-terminator-p
                                            line-terminator
                                            (csv-dialect-line-terminator base))
                       :quoting (if quoting-p quoting (csv-dialect-quoting base)))
                      base)))
    (%validate-dialect dialect)))

(defun make-csv-dialect (&key (from nil from-p)
                           (name nil name-p)
                           (delimiter nil delimiter-p)
                           (quote-char nil quote-char-p)
                           (escape-char nil escape-char-p)
                           (double-quote nil double-quote-p)
                           (skip-initial-space nil skip-initial-space-p)
                           (line-terminator nil line-terminator-p)
                           (quoting nil quoting-p))
  "Copy FROM (default *CSV-DIALECT*) and apply slot overrides."
  (apply #'resolve-csv-dialect
         (if from-p from *csv-dialect*)
         (append (when name-p (list :name name))
                 (when delimiter-p (list :delimiter delimiter))
                 (when quote-char-p (list :quote-char quote-char))
                 (when escape-char-p (list :escape-char escape-char))
                 (when double-quote-p (list :double-quote double-quote))
                 (when skip-initial-space-p (list :skip-initial-space skip-initial-space))
                 (when line-terminator-p (list :line-terminator line-terminator))
                 (when quoting-p (list :quoting quoting)))))

(defun %crlf ()
  (coerce '(#\Return #\Newline) 'string))

(defun %register-preset (name &rest initargs)
  (register-dialect name (apply #'make-instance 'csv-dialect :name name initargs)))

(%register-preset :rfc4180
                  :delimiter #\,
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (%crlf)
                  :quoting :minimal)

(%register-preset :excel
                  :delimiter #\,
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (%crlf)
                  :quoting :minimal)

(%register-preset :excel-tab
                  :delimiter #\Tab
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (%crlf)
                  :quoting :minimal)

(%register-preset :tsv
                  :delimiter #\Tab
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (%crlf)
                  :quoting :minimal)

(%register-preset :excel-eu
                  :delimiter #\;
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (%crlf)
                  :quoting :minimal)

(%register-preset :unix
                  :delimiter #\,
                  :quote-char #\"
                  :escape-char nil
                  :double-quote t
                  :skip-initial-space nil
                  :line-terminator (string #\Newline)
                  :quoting :all)
