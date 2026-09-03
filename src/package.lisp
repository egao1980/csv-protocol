(defpackage #:csv-protocol
  (:use #:cl)
  (:nicknames #:stack-csv)
  (:export #:csv-error
           #:csv-encode-error
           #:csv-parse-error
           #:csv-error-message

           #:csv-dialect
           #:csv-dialect-name
           #:csv-dialect-delimiter
           #:csv-dialect-quote-char
           #:csv-dialect-escape-char
           #:csv-dialect-double-quote
           #:csv-dialect-skip-initial-space
           #:csv-dialect-line-terminator
           #:csv-dialect-quoting
           #:*csv-dialects*
           #:*csv-dialect*
           #:register-dialect
           #:find-dialect
           #:make-csv-dialect
           #:resolve-csv-dialect

           #:csv-backend
           #:csv-backend-dialect
           #:csv-backend-header
           #:*csv-backend*
           #:make-csv-backend
           #:use-csv-backend

           #:encode
           #:decode
           #:encode-to-octets
           #:decode-octets

           #:csv-serdes-backend
           #:make-csv-serdes-backend
           #:use-csv-serdes-backend

           #:csv-character-input-stream
           #:csv-character-output-stream
           #:make-csv-input-stream
           #:make-csv-output-stream
           #:csv-event-parser
           #:make-csv-event-parser))

(in-package #:csv-protocol)
