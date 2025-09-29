;;; Common utilities for PR2CR workflow
(define-module (pr2cr common)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (json-simple)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:export (read-json-file
            write-json-file
            get-env-var
            make-http-request
            extract-regex-matches
            log-info
            log-warning
            log-error))

(define (log-info msg . args)
  "Log info message to stderr"
  (format (current-error-port) "INFO: ~a~%"
          (if (null? args) msg (apply format #f msg args))))

(define (log-warning msg . args)
  "Log warning message to stderr"
  (format (current-error-port) "WARNING: ~a~%"
          (if (null? args) msg (apply format #f msg args))))

(define (log-error msg . args)
  "Log error message to stderr"
  (format (current-error-port) "ERROR: ~a~%"
          (if (null? args) msg (apply format #f msg args))))

(define (read-json-file filepath)
  "Read and parse JSON file"
  (call-with-input-file filepath
    (lambda (port)
      (json->scm port))))

(define (write-json-file filepath data)
  "Write data as JSON to file"
  (call-with-output-file filepath
    (lambda (port)
      (scm->json data port #:pretty #t))))

(define (get-env-var name . default)
  "Get environment variable with optional default"
  (or (getenv name)
      (if (null? default)
          (error "Environment variable not set:" name)
          (car default))))

(define (make-http-request url #:key
                          (method 'GET)
                          (headers '())
                          (body #f)
                          (token #f))
  "Make HTTP request with optional authentication"
  (let* ((uri (string->uri url))
         (auth-headers (if token
                          `((authorization . ,(string-append "Bearer " token))
                            . ,headers)
                          headers)))
    (catch #t
      (lambda ()
        (let-values (((response body-port)
                     (http-request uri
                                  #:method method
                                  #:headers auth-headers
                                  #:body body
                                  #:streaming? #t)))
          (let ((status (response-code response)))
            (if (and (>= status 200) (< status 300))
                (json->scm body-port)
                (begin
                  (log-warning "HTTP ~a failed with status ~a" method status)
                  #f)))))
      (lambda (key . args)
        (log-error "HTTP request failed: ~a ~a" key args)
        #f))))

(define (extract-regex-matches pattern text)
  "Extract all regex matches from text"
  (let ((rx (make-regexp pattern))
        (matches '()))
    (let loop ((start 0))
      (let ((match (regexp-exec rx text start)))
        (if match
            (begin
              (set! matches (cons (match:substring match 1) matches))
              (loop (match:end match)))
            (delete-duplicates (reverse matches) string=?))))))
