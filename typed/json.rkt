#lang typed/racket

(define-type Jsexpr-Null 'null)
(define-type Jsexpr-Array (Listof Jsexpr))
(define-type Jsexpr-Object (Immutable-HashTable Symbol Jsexpr))
(define-type Jsexpr (U Integer
                       Inexact-Real
                       Boolean
                       String
                       Jsexpr-Null
                       Jsexpr-Array
                       Jsexpr-Object))

(define-predicate jsexpr-null? Jsexpr-Null)
(define-predicate jsexpr-array? Jsexpr-Array)
(define-predicate jsexpr-object? Jsexpr-Object)
(define-predicate jsexpr? Jsexpr)

(provide (all-defined-out))

(require/typed/provide json
  [read-json      (->* () (Input-Port) Jsexpr)]
  [write-json     (->* (Jsexpr) (Output-Port) Void)]
  [string->jsexpr (-> String Jsexpr)]
  [jsexpr->string (-> Jsexpr String)])

(require/typed "../json.rkt"
  [jsexpr-walk       (-> Jsexpr (U Index Symbol) * Jsexpr)]
  [pretty-print-json (->* (Jsexpr) (Positive-Integer Output-Port) Void)])
