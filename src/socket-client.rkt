#lang racket

(require racket/tcp)
(require json)
(require "alternative.rkt")
(require "points.rkt")
(require "config.rkt")

(provide get-predict-results)

(define (get-predict-results altns points)
    (define-values (in out) (tcp-connect (*socket-host*) (*socket-port*)))

    (displayln (jsexpr->string
        (for/list ([altn altns] [point points])
            (append (alt-encode altn) (points-encode point)))) out)
    (flush-output out)

    (string->jsexpr (read-line in)))