#!/usr/bin/env guile3
;;; Finalize change request with validation
(use-modules (pr2cr common)
             (ice-9 format)
             (ice-9 getopt-long)
             (ice-9 textual-ports)
             (ice-9 popen)
             (srfi srfi-1))

(define option-spec
  '((draft     (value #t) (required? #t))
    (output    (value #t) (required? #t))
    (pr-number (value #t) (required? #t))
    (help      (single-char #\h))))

(define (run-vale-validation filepath)
  "Run Vale validation on the document"
  (let* ((command (format #f "vale --config=.vale-itil.ini ~a" filepath))
         (port (open-pipe* OPEN_READ "sh" "-c" command))
         (output (read-string port))
         (status (close-pipe port)))
    (if (zero? (status:exit-val status))
        (begin
          (log-info "Vale validation passed")
          #t)
        (begin
          (log-warning "Vale validation issues found:\n~a" output)
          #t)))) ; Continue even with warnings

(define (add-metadata draft-content pr-number)
  "Add final metadata to the change request"
  (string-append
   draft-content
   "\n## Metadata\n\n"
   "- **Document ID:** CR-" (number->string pr-number) "-"
   (date->string (current-date) "~Y~m~d") "\n"
   "- **Version:** 1.0\n"
   "- **Status:** Draft\n"
   "- **Created:** " (date->string (current-date) "~Y-~m-~d ~H:~M:~S UTC") "\n"
   "- **Generator:** PR2CR Guile Workflow v1.0\n"))

(define (main args)
  (let* ((options (getopt-long args option-spec))
         (draft-file (option-ref options 'draft #f))
         (output-file (option-ref options 'output #f))
         (pr-number (string->number (option-ref options 'pr-number #f))))

    (when (option-ref options 'help #f)
      (display "Usage: finalize-change-request.scm --draft FILE --output FILE --pr-number NUM\n")
      (exit 0))

    ;; Read draft content
    (let* ((draft-content (call-with-input-file draft-file get-string-all))
           (final-content (add-metadata draft-content pr-number)))

      ;; Write final version
      (call-with-output-file output-file
        (lambda (port)
          (display final-content port)))

      ;; Run validation
      (run-vale-validation output-file)

      (log-info "Change request finalized: ~a" output-file))))

(main (command-line))
