#lang racket
(provide (all-defined-out))

(struct proc (stdout stdin pid stderr ctrl))

(define-syntax-rule (allow-program name path)
  (define (name . args)
    (apply proc (apply process* path (map ~a (flatten args))))))

(define-syntax-rule (allow-command name)
  (allow-program name (find-executable-path (~a (quote name)))))

(define stdout proc-stdout)
(define stdin proc-stdin)
(define pid proc-pid)
(define stderr proc-stderr)

(define (ctrl p sig)
  ((proc-ctrl p) sig))

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

(define (close-proc p)
  (proc-wait p)
  (close-input-port (stdout p))
  (close-input-port (stderr p))
  (close-output-port (stdin p)))

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

(define (pipe-procs p . ps)
  (foldl (lambda (cur prev)
           (copy-port (stdout prev)
                      (stdin cur))
           cur)
         p
         ps)
  (struct-copy proc p
               [stdout (stdout (last ps))]
               [pid #f]

               [ctrl (lambda (sig)
                       (cons (ctrl p sig)
                             (map (curryr ctrl sig) ps)))]))
