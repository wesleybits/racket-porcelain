#lang racket
(provide (all-defined-out))

(struct proc (stdout stdin stderr pid process))

(define-syntax-rule (allow-program name path)
  (define (name #:stdin [stdin #f]
                #:stdout [stdout #f]
                #:stderr [stderr #f]
                . args)
      (let-values ([(sp out in err) (apply subprocess stdout stdin stderr path (map ~a (flatten args)))])
        (proc out in err
              (subprocess-pid sp)
              sp))))

(define-syntax-rule (allow-command name)
  (allow-program name (find-executable-path (~a (quote name)))))

(define stdout proc-stdout)
(define stdin proc-stdin)
(define pid proc-pid)
(define stderr proc-stderr)

(define (fake-proc fn)
  (let-values ([(stdin/in stdin/out)   (make-pipe)]
               [(stdout/in stdout/out) (make-pipe)]
               [(stderr/in stderr/out) (make-pipe)])
    (let ([t (thread (lambda ()
                       (fn stdin/in stdout/out stderr/out)))])
      (proc stdin/out stdout/in stderr/in 0 t))))

(define (pipe . ps)
  (match ps
    ['()      #f]
    [(list p) p]

    [(and (list first-p rest-ps ...)
          (list _ ... last-p))
     (foldl (lambda (cur prev)
              (copy-port (stdout prev)
                         (stdin cur))
              cur)
            first-p
            rest-ps)
     (struct-copy proc first-p
                  [stdout (stdout last-p)]
                  [pid (map pid ps)]
                  [process ps])]))

(define (proc-wait p)
  (let ([sp (proc-process p)])
    (cond [(list? sp)       (map proc-wait sp)]
          [(subprocess? sp) (subprocess-wait sp)]
          [(thread? sp)     (thread-wait sp)]
          [else             (void)])))

(define (proc-status p)
  (let ([sp (proc-process p)])
    (cond [(subprocess? sp)
           (match (subprocess-status sp)
             ['running 'running]
             [0        'done-ok]
             [_        'done-error])]

          [(and (thread? sp)
                (thread-running? sp))
           'running]

          [(and (list? sp)
                (ormap (lambda (sp) (symbol=? 'done-error (proc-status sp)))
                       sp))
           'done-error]

          [(and (list? sp)
                (andmap (lambda (sp) (symbol=? 'running (proc-status sp)))
                        sp))
           'running]

          [else
           'done-ok])))

(define (proc-kill p)
  (let ([sp (proc-process p)])
    (cond [(subprocess? sp) (subprocess-kill sp #f)]
          [(list? sp)       (for-each proc-kill sp)]
          [(thread? sp)     (kill-thread sp)]
          [else             (void)])))

(define (proc-interrupt p)
  (let ([sp (proc-process p)])
    (cond [(subprocess? sp) (subprocess-kill (proc-process p) #t)]
          [(list? sp)       (for-each proc-kill sp)]
          [(thread? sp)     (kill-thread sp)]
          [else             (void)])))

(define (read-output p #:port [port stdout] #:reader [reader port->string] #:close? [close? #t])
  (let ([output (reader (port p))])
    (when close?
      (close-proc p))
    output))

(define (trace-output p #:port [port stdout] #:tracer [trace displayln] #:close? [close? #t])
  (for ([line (in-lines (port p))])
    (trace line))
  (when close? (close-proc p)))

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
