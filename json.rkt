#lang racket
(provide (all-defined-out))

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

