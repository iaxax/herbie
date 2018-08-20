#lang racket

(require "alternative.rkt")
(require "programs.rkt")
(require "config.rkt")
(require "core/alt-table.rkt")
(require "syntax/rules.rkt")
(require "core/matcher.rkt")

(provide
  *trace-port* trace-alt alternative-encode points-encode location-encode location-decode
  id=>rule simplify-rule taylor+0 taylor+inf taylor-inf special-rule?)

;; rewrite-step corresponds to a train data of the neural network
;; input is alternative + point
;; output is rule + location
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

(define simplify-rule (rule 'simplify '() '()))
(define taylor+0   (rule 'taylor+0 '() '()))
(define taylor+inf (rule 'taylor+inf '() '()))
(define taylor-inf (rule 'taylor-inf '() '()))

(define (special-rule? rule)
  (cond
    [(equal? rule simplify-rule) #t]
    [(equal? rule taylor+0) #t]
    [(equal? rule taylor+inf) #t]
    [(equal? rule taylor-inf) #t]
    [else #f]))

;; a map from id to rule
(define id=>rule
  (let ([ht (make-hash)])
    (hash-set! ht 0 simplify-rule)
    (hash-set! ht 1 taylor+0)
    (hash-set! ht 2 taylor+inf)
    (hash-set! ht 3 taylor-inf)

    (for ([rule (*rules*)]
          [i (in-naturals)])
      (hash-set! ht (+ i 10) rule))
    ht))

;; a map from rule to id
(define rule=>id
  (let ([ht (make-hash)])
    (hash-set! ht simplify-rule 0)
    (hash-set! ht taylor+0 1)
    (hash-set! ht taylor+inf 2)
    (hash-set! ht taylor-inf 3)

    (for ([rule (*rules*)]
          [i (in-naturals)])
      (hash-set! ht rule (+ i 2)))
    ht))

(define symbol-set
  (list
    '+ '- '* '/ 'pow 'sqrt 'fabs 'sqr 'cbrt 'exp 'log 'E 'sin 'cos    
    'PI 'tan 'atan 'atan2 'asin 'acos 'sinh 'cosh 'tanh 'asinh
    'acosh 'atanh 'log1p 'expm1 'hypot 'fma 'TRUE 'FALSE 'not 'and 'or     
    '< '<= '> '>= 'if 're 'im 'complex 'erf 'erfc))
   
;; a map from symbol to id
(define symbol=>id
  (let ([ht (make-hash)]
        [operator-base (*operator-id-base*)])
    (for ([symbol symbol-set]
          [offset (in-naturals)])
      (hash-set! ht symbol (+ offset (*operator-id-base*))))
  ht))

(define (get-taylor-rule rule)
  (let ([name (list-ref rule 1)])
    (match name
      ['0 taylor+0]
      ['inf taylor+inf]
      ['-inf taylor-inf])))

;; create bindings from variable to id
(define (create-bindings vars)
  (let ([bindings (make-hash)]
        [base (*variable-id-base*)])
    (for ([var vars]
          [id (in-naturals)])
      (hash-set! bindings var (+ base id)))
    bindings))

;; encode alternative into a list of int
(define (alternative-encode altn)
  (let* ([program (alt-program altn)]
         [prog-bfs (program-bfs (program-body program))]
         [vars (program-variables program)]
         [var-bindings (create-bindings vars)])

    (define alt-encode
      (for/list ([token prog-bfs])
        (cond
          [(hash-has-key? var-bindings token) (hash-ref var-bindings token)]
          [(hash-has-key? symbol=>id   token) (hash-ref symbol=>id   token)]
          [(number? token) (exact->inexact token)]
          [else (error (string-append "unkown symbol '" (symbol->string token) "' in program"))])))

    (define padding (- (*program-max-length*) (length alt-encode)))

    (cond
      [(> padding 0) (append alt-encode (build-list padding (lambda (x) (*program-placeholder*))))]
      [(< padding 0) (take alt-encode (*program-max-length*))]
      [else alt-encode])))

;; encode points to a list of double
(define (points-encode points)
  (define padding (- (*variable-max-num*) (length points)))
  (cond 
    [(> padding 0) (append points (build-list padding (lambda (x) (*point-placeholder*))))]
    [(< padding 0) (take points (*variable-max-num*))]
    [else points]))

;; encode rule into a list of int
(define (rule-encode rule)
  (hash-ref rule=>id rule))

;; encode location into float
;; @param location is a list of index(1, 2)
;;        1 indicates left child, 2 indicates right child
;; we treat location as an integer in base 3
;; and apply sigmoid function to the final result
(define (location-encode location)
  (define (sigmoid x) (/ 1.0 (+ 1.0 (exp (- x)))))
  (sigmoid
    (foldl
      (lambda (x result) (+ (* result 3) x))
      0
      (reverse location))))

;; Inverse function of location-encode, change integer to list
(define (location-decode location)
  (define (logit x) (log (/ x (- 1 x))))
  (let loop ([loc (exact-round (max 2 (logit location)))] [result '()])
    (if (= loc 0)
      (reverse result)
      (loop (quotient loc 3) (cons (remainder loc 3) result)))))

;; trace all rewrite steps and return them as a list of #<rewrite-step>
(define (trace-rewrite-steps alt points)
  (define points-list
    (for/list ([point points])
      (points-encode point)))

  (let loop ([steps empty]
             [alt alt])
    (let ([prev (alt-prev alt)])
      (if prev
        (let* ([change (alt-change alt)]
               ;; taylor rule is special according to "taylor-alt" in "glue.rkt"
               [rule (if (change? change) (change-rule change) (get-taylor-rule change))]
               [location (if (change? change) (change-location change) (list-ref change 2))]
               [alt-encode (alternative-encode prev)]
               [rule-encode (rule-encode rule)]
               [location-encode (location-encode location)])

          (define steps*
            (for/list ([p points-list])
              (rewrite-step alt-encode p rule-encode location-encode)))

          (loop (append steps steps*) prev))
        steps))))

(define (trace-alt alt points)
  (for ([step (trace-rewrite-steps alt points)])
    (display step (*trace-port*))))