#!/usr/bin/env guile3
;;; Publish change request to JIRA
(use-modules (pr2cr common)
             (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 textual-ports)
             (json)
             (web client)
             (web uri))

(define option-spec
  '((change-request (value #t) (required? #t))
    (jira-url      (value #t) (required? #t))
    (jira-project  (value #t) (required? #t))
    (jira-token    (value #t) (required? #f))
    (dry-run       (single-char #\n))
    (help          (single-char #\h))))

(define (markdown->jira-markup content)
  "Convert Markdown to JIRA markup format"
  ;; Simple conversion - can be enhanced
  (let* ((text content)
         ;; Headers
         (text (regexp-substitute/global #f "^# (.+)$" text
                                        'pre "h1. " 1 'post))
         (text (regexp-substitute/global #f "^## (.+)$" text
                                        'pre "h2. " 1 'post))
         (text (regexp-substitute/global #f "^### (.+)$" text
                                        'pre "h3. " 1 'post))
         ;; Bold
         (text (regexp-substitute/global #f "\\*\\*(.+?)\\*\\*" text
                                        'pre "*" 1 "*" 'post))
         ;; Code blocks
         (text (regexp-substitute/global #f "```(.+?)```" text
                                        'pre "{code}" 1 "{code}" 'post))
         ;; Lists
         (text (regexp-substitute/global #f "^- (.+)$" text
                                        'pre "* " 1 'post)))
    text))

(define (create-jira-issue jira-url project-key summary description token)
  "Create a new JIRA issue"
  (let* ((url (format #f "~a/rest/api/3/issue" jira-url))
         (payload `(("fields" . (("project" . (("key" . ,project-key)))
                               ("summary" . ,summary)
                               ("description" . ,description)
                               ("issuetype" . (("name" . "Task")))))))
         (json-body (scm->json-string payload)))

    (make-http-request url
                      #:method 'POST
                      #:headers '((content-type . "application/json"))
                      #:body json-body
                      #:token token)))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (cr-file (option-ref options 'change-request #f))
         (jira-url (string-trim-right (option-ref options 'jira-url #f) #\/))
         (project-key (option-ref options 'jira-project #f))
         (jira-token (or (option-ref options 'jira-token #f)
                        (getenv "JIRA_API_TOKEN")))
         (dry-run (option-ref options 'dry-run #f)))

    (when (option-ref options 'help #f)
      (display "Usage: publish-to-jira.scm --change-request FILE --jira-url URL --jira-project KEY\n")
      (exit 0))

    ;; Read change request
    (let* ((content (call-with-input-file cr-file get-string-all))
           (lines (string-split content #\newline))
           (title-line (find (lambda (l) (string-prefix? "# " l)) lines))
           (summary (if title-line
                       (substring title-line 2)
                       "Change Request"))
           (jira-content (markdown->jira-markup content)))

      (if dry-run
          (begin
            (log-info "DRY RUN - Would create JIRA issue:")
            (log-info "  Project: ~a" project-key)
            (log-info "  Summary: ~a" summary)
            (log-info "  Content length: ~a chars" (string-length jira-content)))
          (let ((result (create-jira-issue jira-url project-key summary
                                         jira-content jira-token)))
            (if result
                (log-info "JIRA issue created: ~a"
                         (assoc-ref result "key"))
                (log-error "Failed to create JIRA issue")))))))

(main (command-line))
