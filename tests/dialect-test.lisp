(in-package #:csv-protocol/tests)

(deftest find-preset-dialects
  (ok (find-dialect :rfc4180))
  (ok (find-dialect :excel))
  (ok (find-dialect :excel-tab))
  (ok (find-dialect :tsv))
  (ok (find-dialect :excel-eu))
  (ok (find-dialect :unix))
  (ok (char= #\, (csv-dialect-delimiter (find-dialect :rfc4180))))
  (ok (char= #\Tab (csv-dialect-delimiter (find-dialect :tsv))))
  (ok (char= #\; (csv-dialect-delimiter (find-dialect :excel-eu))))
  (ok (eq :all (csv-dialect-quoting (find-dialect :unix)))))

(deftest resolve-nil-uses-star
  (let ((*csv-dialect* :excel-eu))
    (ok (char= #\; (csv-dialect-delimiter (resolve-csv-dialect nil))))))

(deftest override-after-preset
  (let ((d (resolve-csv-dialect :excel-eu :line-terminator (string #\Newline))))
    (ok (char= #\; (csv-dialect-delimiter d)))
    (ok (string= (string #\Newline) (csv-dialect-line-terminator d)))))

(deftest make-csv-dialect-from
  (let ((d (make-csv-dialect :from :tsv :quoting :all)))
    (ok (char= #\Tab (csv-dialect-delimiter d)))
    (ok (eq :all (csv-dialect-quoting d)))))

(deftest unknown-dialect-signals
  (ok (signals (find-dialect :no-such-dialect) 'csv-error)))

(deftest invalid-quoting-none-without-escape
  (ok (signals (resolve-csv-dialect :rfc4180 :quoting :none) 'csv-error)))

(deftest invalid-delimiter-equals-quote
  (ok (signals (resolve-csv-dialect :rfc4180 :delimiter #\") 'csv-error)))

(deftest register-custom-dialect
  (let ((d (make-csv-dialect :from :rfc4180 :delimiter #\| :name :pipe)))
    (register-dialect :pipe d)
    (ok (char= #\| (csv-dialect-delimiter (find-dialect :pipe))))))
