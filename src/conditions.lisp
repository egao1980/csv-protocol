(in-package #:csv-protocol)

(define-condition csv-error (error)
  ((message :initarg :message :reader csv-error-message :initform nil))
  (:report (lambda (c s)
             (format s "CSV error~@[: ~a~]" (csv-error-message c)))))

(define-condition csv-encode-error (csv-error) ())

(define-condition csv-parse-error (csv-error) ())
