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
  (thread (lambda ()
            (let ([done? (sync/timeout max-wait done-chan)])
              (unless done?
                (kill-thread t)))))
  done-chan)
