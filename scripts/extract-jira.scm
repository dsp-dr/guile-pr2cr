#!/usr/bin/env guile3
;;; Extract JIRA ticket information from GitHub PR metadata
(use-modules (pr2cr common)
             (ice-9 format)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 getopt-long)
             (json)
             (srfi srfi-1)
             (srfi srfi-26))

(define option-spec
  '((pr-metadata (value #t) (required? #t)
                (single-char #\p))
    (jira-url     (value #t) (required? #t)
                 (single-char #\j))
    (output       (value #t) (required? #t)
                 (single-char #\o))
    (jira-token   (value #t) (required? #f)
                 (single-char #\t))
    (help         (single-char #\h))))

(define (extract-ticket-ids pr-data)
  "Extract JIRA ticket IDs from PR data"
  (let* ((ticket-pattern "\\b([A-Z]{2,}-[0-9]+)\\b")
         (texts (filter string?
                       (list (assoc-ref pr-data "title")
                             (assoc-ref pr-data "body"))))
         (commit-messages
          (map (lambda (commit)
                 (assoc-ref (assoc-ref commit "commit") "message"))
               (or (assoc-ref pr-data "commits") '())))
         (all-text (string-join (append texts commit-messages) "\n")))
    (extract-regex-matches ticket-pattern all-text)))

(define (fetch-ticket-details jira-url ticket-id token)
  "Fetch ticket details from JIRA API"
  (let ((url (format #f "~a/rest/api/3/issue/~a" jira-url ticket-id)))
    (make-http-request url #:token token)))

(define (extract-relevant-fields ticket-data jira-url)
  "Extract relevant fields from JIRA ticket"
  (let ((fields (assoc-ref ticket-data "fields")))
    `(("key" . ,(assoc-ref ticket-data "key"))
      ("summary" . ,(or (assoc-ref fields "summary") ""))
      ("description" . ,(or (assoc-ref fields "description") ""))
      ("status" . ,(assoc-ref (or (assoc-ref fields "status") '()) "name"))
      ("priority" . ,(assoc-ref (or (assoc-ref fields "priority") '()) "name"))
      ("issue_type" . ,(assoc-ref (or (assoc-ref fields "issuetype") '()) "name"))
      ("assignee" . ,(assoc-ref (or (assoc-ref fields "assignee") '()) "displayName"))
      ("reporter" . ,(assoc-ref (or (assoc-ref fields "reporter") '()) "displayName"))
      ("created" . ,(or (assoc-ref fields "created") ""))
      ("updated" . ,(or (assoc-ref fields "updated") ""))
      ("components" . ,(map (cut assoc-ref <> "name")
                           (or (assoc-ref fields "components") '())))
      ("labels" . ,(or (assoc-ref fields "labels") '()))
      ("url" . ,(format #f "~a/browse/~a" jira-url (assoc-ref ticket-data "key"))))))

(define (sort-by-priority tickets)
  "Sort tickets by priority"
  (let ((priority-order '("Blocker" "Critical" "Major" "Minor" "Trivial")))
    (sort tickets
          (lambda (a b)
            (let ((pa (assoc-ref a "priority"))
                  (pb (assoc-ref b "priority")))
              (< (or (list-index (cut string=? pa <>) priority-order) 999)
                 (or (list-index (cut string=? pb <>) priority-order) 999)))))))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (pr-metadata-file (option-ref options 'pr-metadata #f))
         (jira-url (string-trim-right (option-ref options 'jira-url #f) #\/))
         (output-file (option-ref options 'output #f))
         (jira-token (or (option-ref options 'jira-token #f)
                        (getenv "JIRA_API_TOKEN"))))

    (when (option-ref options 'help #f)
      (display "Usage: extract-jira.scm --pr-metadata FILE --jira-url URL --output FILE [--jira-token TOKEN]\n")
      (exit 0))

    ;; Load PR metadata
    (let ((pr-data (read-json-file pr-metadata-file)))

      ;; Extract ticket IDs
      (let ((ticket-ids (extract-ticket-ids pr-data)))
        (log-info "Found ~a JIRA ticket(s): ~a"
                 (length ticket-ids)
                 (string-join ticket-ids ", "))

        ;; Fetch ticket details
        (let* ((tickets (filter-map
                        (lambda (id)
                          (let ((data (fetch-ticket-details jira-url id jira-token)))
                            (if data
                                (extract-relevant-fields data jira-url)
                                (begin
                                  (log-warning "Could not fetch ticket ~a" id)
                                  #f))))
                        ticket-ids))
               (sorted-tickets (sort-by-priority tickets))
               (jira-data `(("tickets" . ,sorted-tickets)
                          ("primary_ticket" . ,(if (null? sorted-tickets)
                                                  #f
                                                  (car sorted-tickets)))
                          ("pr_number" . ,(assoc-ref pr-data "number"))
                          ("pr_title" . ,(or (assoc-ref pr-data "title") "")))))

          ;; Save output
          (write-json-file output-file jira-data)
          (log-info "JIRA data saved to ~a" output-file))))))

(main (command-line))
