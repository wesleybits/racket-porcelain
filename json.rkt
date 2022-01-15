#lang racket
(provide (all-defined-out))
(require json)

(define (jsexpr-walk expr . path)
  (match path
    ['()                                          expr]
    [(list (? symbol? idx) path ...)              #:when (and (hash? expr)
                                                              (hash-has-key? expr idx))
                                                  (apply jsexpr-walk (hash-ref expr idx) path)]
    [(list (? nonnegative-integer? idx) path ...) #:when (and (list? expr)
                                                              (< idx (length expr)))
                                                  (apply jsexpr-walk (list-ref expr idx) path)]
    [(list (? symbol? idx) path ...)              #:when (hash? expr)
                                                  (error 'jsexpr-walk "Field not found in object: ~a" idx)]
    [(list (? nonnegative-integer? idx) path ...) #:when (list? expr)
                                                  (error 'jsexpr-walk "Index too large for array: ~a" idx)]
    [(list idx _ ...)                             #:when (hash? expr)
                                                  (error 'jsexpr-walk "Inappropriate index type for object: ~a" idx)]
    [(list idx _ ...)                             #:when (list? expr)
                                                  (error 'jsexpr-walk "Inappropriate index type for array: ~a" idx)]
    [_                                            #:when (nor (list? expr) (hash? expr))
                                                  (error 'jsexpr-walk "Not a JSON structure type: ~w" expr)]))

(define (jsexpr-update-path expr . path-&-update)
  (match path-&-update
    ['()                             expr]
    [(list updater)                  (updater expr)]
    [(list (? index? idx) updater)   #:when (list? expr)
                                     (list-set expr idx (updater (list-ref expr idx)))]
    [(list (? symbol? key) updater)  #:when (hash? expr)
                                     (hash-update expr key updater)]
    [(list (? index? idx) more ...)  #:when (list? expr)
                                     (list-set expr idx (apply jsexpr-update-path (list-ref expr idx) more))]
    [(list (? symbol? key) more ...) #:when (hash? expr)
                                     (hash-set expr key (apply jsexpr-update-path (hash-ref expr idx) more))]
    [(list idx _ ...)
     (error 'jsexpr-update-path "Illegal index for expr\n  expr:~a\n  index:~a" expr idx)]))

(define (pretty-print-json jsexpr [indent-width 4] [out (current-output-port)])
  (define (print-indent width)
    (display (make-string width #\space) out))

  (define (print-json-object obj indent)
    (let ([indent* (+ indent indent-width)])
      (displayln "{" out)
      (let loop:keys ([keys (hash-keys obj)])
        (match keys
          ['()                      (void)]
          [(list key more-keys ...) (print-indent indent*)
                                    (write (symbol->string key) out)
                                    (display ": " out)
                                    (print-json (hash-ref obj key) indent*)
                                    (cond [(not (empty? more-keys)) (displayln "," out)
                                                                    (loop:keys more-keys)]
                                          [else                     (newline)])]))
      (print-indent indent)
      (display "}" out)))

  (define (print-json-array arr indent)
    (let ([indent* (+ indent indent-width)])
      (displayln "[" out)
      (let loop:elements ([elts arr])
        (match elts
          ['() (void)]
          [(list elt rest-elts ...) (print-indent indent*)
                                    (print-json elt indent*)
                                    (cond [(not (empty? rest-elts)) (displayln "," out)
                                                                    (loop:elements rest-elts)]
                                          [else                     (newline)])]))
      (print-indent indent)
      (display "]" out)))

  (define (print-json jsexpr indent)
    (cond [(hash? jsexpr)           (print-json-object jsexpr indent)]
          [(list? jsexpr)           (print-json-array jsexpr indent)]
          [(number? jsexpr)         (display jsexpr out)]
          [(boolean? jsexpr)        (display (if jsexpr "true" "false") out)]
          [(string? jsexpr)         (write jsexpr out)]
          [(eq? (json-null) jsexpr) (display "null" out)]
          [else                     (error 'pretty-print-json/print-json "Not a json object: ~a" jsexpr)]))

  (print-json jsexpr 0))
