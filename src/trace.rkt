#lang racket

(provide *trace-port* trace)

(define *trace-port* (make-parameter (current-output-port)))

(define (trace . msgs)
  (for ([msg msgs])
    (display msg (*trace-port*)))
  (display "\n" (*trace-port*)))