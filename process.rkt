#lang racket
(provide (all-defined-out))

(define-syntax-rule (allow-program name path)
  (define (name . args)
    (apply process* path (map ~a (flatten args)))))

(define-syntax-rule (allow-command name)
  (allow-program name (find-executable-path (~a (quote name)))))

(define stdout first)
(define stdin second)
(define pid third)
(define stderr fourth)

(define (ctrl p sig)
  ((fifth p) sig))

(define (proc-wait p) (ctrl p 'wait))
(define (proc-status p) (ctrl p 'status))
(define (proc-exit-code p) (ctrl p 'exit-code))
(define (proc-kill p) (ctrl p 'kill))
(define (proc-interrupt p) (ctrl p 'interrupt))

(define (read-output p #:port [port stdout] #:reader [reader port->string])
  (reader (port p)))

(define (trace-output p #:port [port stdout] #:tracer [trace displayln])
  (for ([line (in-lines (port p))])
    (trace line)))

(define (tail p [out (current-output-port)])
  (let loop:tail ()
    (sync/timeout 1
                  (handle-evt (read-line-evt (stdout p))
                              (lambda (stdout-line)
                                (unless (eof-object? stdout-line)
                                  (displayln stdout-line out))))
                  (handle-evt (read-line-evt (stderr p))
                              (lambda (stderr-line)
                                (unless (eof-object? stderr-line)
                                  (display #\u001B out)
                                  (display "[31m" out)
                                  (display stderr-line out)
                                  (display #\u001B out)
                                  (display "[0m" out)
                                  (newline out)))))
    (flush-output out)
    (when (symbol=? 'running (proc-status p))
      (loop:tail)))
  p)

