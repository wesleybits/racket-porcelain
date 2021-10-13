#lang typed/racket

(define-type (Poll A) (-> A))

(require/typed/provide "../polling.rkt"
  [poll      (All (A)
                  (->* ((-> A)
                        (-> A Boolean))
                       ((Option Positive-Integer))
                       (Poll A)))]
  [poll-evt  (All (A)
                  (->* ((Poll A))
                       (Positive-Integer)
                       (Evtof A)))]
  [poll-evt* (All (A)
                  (->* ((-> A)
                        (-> A Boolean))
                       ((Option Positive-Integer)
                        Positive-Integer)
                       (Evtof A)))])
