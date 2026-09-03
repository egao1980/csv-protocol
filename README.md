# csv-protocol

CLOS CSV encode/decode for [cl-stack](https://github.com/egao1980/cl-stack) (RFC 4180 dialects). Implements [`serdes-protocol`](https://github.com/egao1980/serdes-protocol) `:csv` and `:tsv`.

OCI **0.1.0** — `ghcr.io/egao1980/cl-systems/csv-protocol:0.1.0`

**Cookbook:** [csv.md](https://github.com/egao1980/cl-stack/blob/main/docs/cookbooks/csv.md) · Brief: [csv-protocol.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/csv-protocol.md)

```lisp
(asdf:load-system "csv-protocol")   ; nick stack-csv; registers :csv / :tsv

(stack-csv:encode '(#("a" "b") #("1" "2")) :header nil)
(stack-csv:decode "name,age
alice,30")
;; ⇒ vector of hash-tables, string keys

(stack-csv:encode rows :dialect :excel-eu)
(stack-csv:encode rows :delimiter #\|)
(let ((csv-protocol:*csv-dialect* :tsv))
  (serdes-protocol:encode rows :format :csv))
(serdes-protocol:encode rows :format :tsv)
```

Dialects: `:rfc4180` (default), `:excel`, `:excel-tab` / `:tsv`, `:excel-eu` (`;`), `:unix`. Slots: `delimiter`, `quote-char`, `escape-char`, `double-quote`, `skip-initial-space`, `line-terminator`, `quoting` (`:minimal` / `:all` / `:nonnumeric` / `:none`).

Streaming: `make-csv-input-stream` / `make-csv-output-stream` + `stream-encode-value` / `stream-decode-value` (RFC records, not `read-line`). Event pull: `make-csv-event-parser` / `parse-next-event` (`:header` `:begin-row` `:field` `:end-row`).

## License

MIT — see [LICENSE](LICENSE).
