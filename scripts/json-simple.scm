nil
;;; Simple JSON parser and writer for Guile
(define-module (json-simple)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:export (json->scm
            scm->json
            scm->json-string))

(define (json->scm port)
  "Parse JSON from port using jq as external tool"
  (let* ((temp-file (tmpnam))
         (content (read-string port)))
    (call-with-output-file temp-file
      (lambda (p) (display content p)))
    (let* ((result-port (open-pipe* OPEN_READ "jq" "." temp-file))
           (result (read result-port)))
      (close-pipe result-port)
      (delete-file temp-file)
      result)))

(define (scm->json-string obj)
  "Convert Scheme object to JSON string"
  (cond
   ((null? obj) "null")
   ((boolean? obj) (if obj "true" "false"))
   ((number? obj) (number->string obj))
   ((string? obj) (format #f "~s" obj))
   ((symbol? obj) (format #f "~s" (symbol->string obj)))
   ((list? obj)
    (if (and (not (null? obj))
             (every pair? obj)
             (every (lambda (p) (or (string? (car p)) (symbol? (car p)))) obj))
        ;; It's an association list (object)
        (string-append "{"
                      (string-join
                       (map (lambda (pair)
                              (format #f "~s:~a"
                                     (if (symbol? (car pair))
                                         (symbol->string (car pair))
                                         (car pair))
                                     (scm->json-string (cdr pair))))
                            obj)
                       ",")
                      "}")
        ;; It's a regular list (array)
        (string-append "["
                      (string-join (map scm->json-string obj) ",")
                      "]")))
   (else "null")))

(define (scm->json obj port . args)
  "Write Scheme object as JSON to port"
  (let ((pretty (and (not (null? args))
                     (memq #:pretty args))))
    (display (scm->json-string obj) port)
    (when pretty (newline port))))
