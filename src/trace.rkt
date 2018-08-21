#lang racket

(require "alternative.rkt")
(require "points.rkt")
(require "programs.rkt")
(require "config.rkt")
(require "syntax/rules.rkt")
(require "core/alt-table.rkt")
(require "core/matcher.rkt")

(provide *trace-port* trace-alt)

;; Rewrite-step corresponds to a train data of the neural network
(struct rewrite-step (alternative point rule location)
  #:methods gen:custom-write
  [(define (write-proc step port mode)
    (for ([token (rewrite-step-alternative step)])
      (display token port)
      (display " " port))
    
    (for ([point (rewrite-step-point step)])
      (display point port)
      (display " " port))

    (display (rewrite-step-rule step) port)
    (display " " port)

    (display (rewrite-step-location step) port)
    (display "\n" port))])

(define *trace-port* (make-parameter (current-output-port)))

;; Trace all rewrite steps and return them as a list of #<rewrite-step>
(define (trace-rewrite-steps alt points)

  (define encoded-points-list
    (for/list ([point points])
      (points-encode point)))

  (define (get-taylor-rule rule)
    (let ([name (list-ref rule 1)])
      (match name
        ['0 taylor+0]
        ['inf taylor+inf]
        ['-inf taylor-inf])))

  (let loop ([steps empty] [alt alt])
    (let ([prev (alt-prev alt)])
      (if prev
        (let* ([change (alt-change alt)]

               ;; Taylor rule is special according to "taylor-alt" in "glue.rkt"
               [rule (if (change? change) (change-rule change) (get-taylor-rule change))]
               [location (if (change? change) (change-location change) (list-ref change 2))]

               [encoded-alt (alt-encode prev)]
               [encoded-rule (rule-encode rule)]
               [encoded-location (location-encode location)])

          (define steps*
            (for/list ([encoded-points encoded-points-list])
              (rewrite-step encoded-alt encoded-points encoded-rule encoded-location)))

          (loop (append steps steps*) prev))
        steps))))

(define (trace-alt alt points)
  (for ([step (trace-rewrite-steps alt points)])
    (display step (*trace-port*))))