#lang racket
(provide (all-defined-out))

(define (poll reload-data ready? [backoff #f])
  (lambda ()
    (let loop:poll ()
      (let ([data (reload-data)])
        (cond [(ready? data) data]
              [else          (when backoff (sleep backoff))
                             (loop:poll)])))))

(define (poll-evt poll [max-wait 600])
  (define done-chan (make-channel))
  (define t (thread (lambda ()
                      (channel-put done-chan (poll)))))

  ;; watchdog to kill 't' should the poll get hung default kill-time is 10
  ;; minutes. ymmv, so it's a parameter.
  (thread (lambda ()
            (let ([done? (sync/timeout max-wait t)])
              (unless done?
                (kill-thread t)))))

  done-chan)

(define (poll-evt* reload-data ready? [backoff #f] [max-wait 600])
  (poll-evt (poll reload-data ready? backoff)
            max-wait))
