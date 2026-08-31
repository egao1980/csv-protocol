(defsystem "csv-protocol"
  :version "0.1.0"
  :description "CLOS CSV encode/decode for cl-stack (RFC 4180 dialects); implements serdes-protocol :csv / :tsv"
  :author "egao1980"
  :license "MIT"
  :depends-on ("babel" "serdes-protocol")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "dialect")
               (:file "codec")
               (:file "protocol")
               (:file "serdes"))
  :in-order-to ((test-op (test-op "csv-protocol/tests"))))

(defsystem "csv-protocol/tests"
  :depends-on ("csv-protocol" "serdes-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "dialect-test")
               (:file "codec-test")
               (:file "stream-test")
               (:file "serdes-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
