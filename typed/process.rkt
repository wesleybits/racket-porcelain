#lang typed/racket

(require/typed/provide "../process.rkt"
  [#:opaque Proc  proc?]
  [stdout         (-> Proc Input-Port)]
  [stderr         (-> Proc Input-Port)]
  [stdin          (-> Proc Output-Port)]
  [pid            (-> Proc Nonnegative-Integer)]
  [fake-proc      (-> (-> Input-Port Output-Port Output-Port Any)
                      Proc)]
  [pipe           (-> Proc * Proc)]
  [proc-wait      (-> Proc Void)]
  [proc-status    (-> Proc (U 'running 'done-ok 'done-error))]
  [proc-kill      (-> Proc Void)]
  [proc-interrupt (-> Proc Void)]
  [read-output    (All (A)
                       (-> Proc
                           [#:port   (-> Proc Input-Port)]
                           [#:reader (-> Input-Port A)]
                           [#:close? Boolean]
                           A))]
  [trace-output   (-> Proc
                      [#:port   (-> Proc Input-Port)]
                      [#:tracer (-> String Any)]
                      [#:close? Boolean]
                      Void)]
  [close-proc     (-> Proc Void)]
  [tail           (->* (Proc) (Output-Port) Proc)])

(require/typed "../process.rkt"
  [proc (-> Input-Port Output-Port Input-Port Integer Subprocess
            Proc)])

(require (only-in "../process.rkt"
                  [allow-program untyped:allow-program]))

(define-type Program (-> [#:stdin  (Option Input-Port)]
                         [#:stdout (Option Output-Port)]
                         [#:stderr (Option Output-Port)]
                         Any *
                         Proc))

(define-syntax-rule (allow-program name path)
  (define (name #:stdin [stdin : (Option Input-Port) #f]
                #:stdout [stdout : (Option Output-Port) #f]
                #:stderr [stderr : (Option Output-Port) #f]
                . [args : Any *])
          : Proc
    (let-values ([(sp out in err)
                  (apply subprocess
                         stdout
                         stdin
                         stderr
                         (assert path path-string?)
                         (map ~a (flatten args)))])
      (proc (assert out input-port?)
            (assert in output-port?)
            (assert err input-port?)
            (subprocess-pid sp)
            sp))))

(define-syntax-rule (allow-command name)
  (allow-program name (find-executable-path (symbol->string (quote name)))))

(provide (all-defined-out)
         allow-program
         allow-command)
