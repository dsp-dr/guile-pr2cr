#!/usr/bin/env guile3
;;; Generate change request summary from PR and JIRA data
(use-modules (pr2cr common)
             (ice-9 format)
             (ice-9 match)
             (ice-9 getopt-long)
             (ice-9 textual-ports)
             (ice-9 rdelim)
             (json)
             (srfi srfi-1)
             (srfi srfi-19))

(define option-spec
  '((pr-metadata (value #t) (required? #t))
    (jira-data   (value #t) (required? #t))
    (template    (value #t) (required? #f))
    (output      (value #t) (required? #t))
    (start-time  (value #t) (required? #f))
    (end-time    (value #t) (required? #f))
    (help        (single-char #\h))))

(define (calculate-risk-level pr-data)
  "Calculate risk level based on PR characteristics"
  (let* ((files (or (assoc-ref pr-data "files") '()))
         (file-count (length files))
         (critical-files (filter (lambda (file)
                                  (or (string-contains (assoc-ref file "filename") "config")
                                      (string-contains (assoc-ref file "filename") "database")
                                      (string-contains (assoc-ref file "filename") "security")))
                               files))
         (additions (fold + 0 (map (lambda (f) (or (assoc-ref f "additions") 0)) files)))
         (deletions (fold + 0 (map (lambda (f) (or (assoc-ref f "deletions") 0)) files))))

    (cond
     ((or (> file-count 20) (> additions 500) (not (null? critical-files)))
      "High")
     ((or (> file-count 10) (> additions 200))
      "Medium")
     (else "Low"))))

(define (generate-testing-steps pr-data)
  "Generate testing steps based on PR changes"
  (let ((files (or (assoc-ref pr-data "files") '())))
    (append
     '("1. Deploy to staging environment"
       "2. Run automated test suite")
     (if (any (lambda (f) (string-contains (assoc-ref f "filename") "api")) files)
         '("3. Execute API integration tests"
           "4. Validate API response times")
         '())
     (if (any (lambda (f) (string-contains (assoc-ref f "filename") "database")) files)
         '("3. Run database migration tests"
           "4. Verify data integrity")
         '())
     '("5. Perform user acceptance testing"
       "6. Document test results"))))

(define (generate-rollback-steps)
  "Generate standard rollback steps"
  '("1. Stop deployment if in progress"
    "2. Revert to previous deployment"
    "3. Clear application caches"
    "4. Verify service restoration"
    "5. Notify stakeholders of rollback"))

(define (format-change-request pr-data jira-data start-time end-time)
  "Format the complete change request document"
  (let* ((primary-ticket (assoc-ref jira-data "primary_ticket"))
         (pr-title (assoc-ref pr-data "title"))
         (pr-number (assoc-ref pr-data "number"))
         (pr-author (assoc-ref (assoc-ref pr-data "author") "login"))
         (risk-level (calculate-risk-level pr-data))
         (testing-steps (generate-testing-steps pr-data))
         (rollback-steps (generate-rollback-steps)))

    (string-append
     "# Change Request: " pr-title "\n\n"
     "## Change Details\n\n"
     "**Change ID:** CR-PR-" (number->string pr-number) "\n"
     "**Related PR:** #" (number->string pr-number) "\n"
     (if primary-ticket
         (format #f "**JIRA Ticket:** ~a - ~a\n"
                 (assoc-ref primary-ticket "key")
                 (assoc-ref primary-ticket "summary"))
         "")
     "**Requester:** " pr-author "\n"
     "**Risk Level:** " risk-level "\n"
     "**Change Window:** " start-time " to " end-time "\n\n"

     "## Description\n\n"
     (or (assoc-ref pr-data "body") "No description provided.") "\n\n"

     "## Impact Analysis\n\n"
     "### Risk Assessment\n"
     "- **Risk Level:** " risk-level "\n"
     "- **Files Modified:** " (number->string (length (assoc-ref pr-data "files"))) "\n"
     "- **Lines Added:** " (number->string (fold + 0 (map (lambda (f)
                                                           (or (assoc-ref f "additions") 0))
                                                         (assoc-ref pr-data "files")))) "\n"
     "- **Lines Removed:** " (number->string (fold + 0 (map (lambda (f)
                                                             (or (assoc-ref f "deletions") 0))
                                                           (assoc-ref pr-data "files")))) "\n\n"

     "### Affected Components\n"
     (string-join (map (lambda (f) (format #f "- ~a" (assoc-ref f "filename")))
                      (take (assoc-ref pr-data "files")
                           (min 10 (length (assoc-ref pr-data "files")))))
                 "\n") "\n\n"

     "## Testing Plan\n\n"
     (string-join testing-steps "\n") "\n\n"

     "## Rollback Plan\n\n"
     (string-join rollback-steps "\n") "\n\n"

     "## Approval\n\n"
     "- [ ] Development Team Lead\n"
     "- [ ] Operations Team Lead\n"
     "- [ ] Change Advisory Board\n\n"

     "---\n"
     "*Generated: " (date->string (current-date) "~Y-~m-~d ~H:~M:~S UTC") "*\n")))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (pr-metadata-file (option-ref options 'pr-metadata #f))
         (jira-data-file (option-ref options 'jira-data #f))
         (output-file (option-ref options 'output #f))
         (start-time (or (option-ref options 'start-time #f)
                        (date->string (current-date) "~Y-~m-~d ~H:00:00 UTC")))
         (end-time (or (option-ref options 'end-time #f)
                      (date->string (current-date) "~Y-~m-~d ~H:00:00 UTC"))))

    (when (option-ref options 'help #f)
      (display "Usage: generate-summary.scm --pr-metadata FILE --jira-data FILE --output FILE\n")
      (exit 0))

    ;; Load data
    (let* ((pr-data (read-json-file pr-metadata-file))
           (jira-data (read-json-file jira-data-file))
           (change-request (format-change-request pr-data jira-data start-time end-time)))

      ;; Write output
      (call-with-output-file output-file
        (lambda (port)
          (display change-request port)))

      (log-info "Change request generated: ~a" output-file))))

(main (command-line))
