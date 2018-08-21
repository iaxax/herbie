#lang racket

(require "programs.rkt")
(require "points.rkt")
(require "core/matcher.rkt")
(require "common.rkt")

(provide (struct-out alt-delta) (struct-out alt-event) alternative?
         make-alt alt? alt-program alt-change alt-prev alt-add-event
         make-regime-alt alt-fix alt-encode
         alt-apply alt-apply-one alt-rewrite-expression
         alt-errors alt-error alt-cost alt-rewrite-rm alt-set-prev
	       alt-initial alt-changes alt-history-length)

;; Alts are a lightweight audit trail.
;; An alt records a low-level view of how Herbie got
;; from one program to another.
;; They are a labeled linked list of changes.

(struct alt-delta (program change prev)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt-delta " port)
           (write (alt-program alt) port)
           (display ">" port))])

(struct alt-event (program event prevs)
        #:methods gen:custom-write
        [(define (write-proc alt port mode)
           (display "#<alt-event " port)
           (write (alt-program alt) port)
           (display ">" port))])

(define alternative? (or/c alt-delta? alt-event?))

(define (make-alt prog)
  (alt-event prog 'start '()))

(define (alt? altn)
  (or (alt-delta? altn) (alt-event? altn)))

(define (alt-program altn)
  (match altn
    [(alt-delta prog _ _) prog]
    [(alt-event prog _ _) prog]))

(define (alt-change altn)
  (match altn
    [(alt-delta _ cng _) cng]
    [(alt-event _ _ '()) #f]
    [(alt-event _ event `(,prev ,_ ...)) event]))

(define (alt-prev altn)
  (match altn
    [(alt-delta _ _ prev) prev]
    [(alt-event _ _ '()) #f]
    [(alt-event _ _ `(,prev ,_ ...)) prev]))

;; Tell whether an alternative is computable
;; Computable alternative doesn't contain abnormal floating-point values
(define (alt-computable? alt)
  (let loop ([expr (program-body (alt-program alt))])
    (cond
      [(empty? expr) #t]
      [(list? expr) (and (loop (car expr)) (loop (cdr expr)))]
      [(number? expr)
        (for/and ([abnormal-num '(+inf.0 +inf.f -inf.0 -inf.f +nan.0 +nan.f)])
          (not (equal? abnormal-num expr)))]
      [else #t])))

;; Replace incomputable alternatives with default alternatives
(define (alt-fix defaults alts)
  (for/list ([default defaults] [alt alts])
    (if (alt-computable? alt) alt default)))

(define (alt-error point exact altn)
  (error (alt-program altn) point exact))

(define (alt-errors altn)
  (errors (alt-program altn) (*pcontext*)))

(define (alt-cost altn)
  (program-cost (alt-program altn)))

;; Apply a change to an alternative
(define (alt-apply-one change alt)
  (alt-delta (change-apply change (alt-program alt)) change alt))

(define (alt-apply altn . changes)
  (foldl alt-apply-one altn changes))

;; Gets the initial version of the current alt.
(define (alt-initial altn)
  (if (alt-prev altn)
      (alt-initial (alt-prev altn))
      altn))

;; Get a list of every change that's happened to the current alt, in application order.
(define (alt-changes altn)
  (let loop ([cur-alt altn] [acc '()])
    (if (alt-prev cur-alt)
	(loop (alt-prev cur-alt) (cons (alt-change cur-alt) acc))
	acc)))

(define (alt-rewrite-expression alt #:destruct [destruct? #f] #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry alt-apply alt)
         (rewrite-expression subtree #:destruct destruct? #:root root-loc))))

(define (alt-rewrite-rm alt #:root [root-loc '()])
  (let ([subtree (location-get root-loc (alt-program alt))])
    (map (curry apply alt-apply alt)
         (map reverse
              (rewrite-expression-head subtree #:root root-loc)))))

(define (alt-history-length alt)
  (if (alt-prev alt)
      (+ 1 (alt-history-length (alt-prev alt)))
      0))

(define (alt-set-prev altn prev)
  (alt-delta (alt-program altn) (alt-change altn) prev))

(define (alt-add-event altn event)
  (alt-event (alt-program altn) event (list altn)))

(define (make-regime-alt new-prog altns splitpoints)
  (alt-event new-prog (list 'regimes splitpoints) altns))

;; Encode alternative into a list of integer
(define (alt-encode altn)

  ;; Create bindings between variable and id
  (define (create-bindings vars)
    (let ([bindings (make-hash)]
          [base (*variable-id-base*)])
      (for ([var vars]
            [id (in-naturals)])
        (hash-set! bindings var (+ base id)))
      bindings))

  (let* ([program (alt-program altn)]
         [prog-bfs (program-bfs (program-body program))]
         [vars (program-variables program)]
         [var-bindings (create-bindings vars)])

    (define alt-encode
      (for/list ([token prog-bfs])
        (cond
          [(hash-has-key? var-bindings token) (hash-ref var-bindings token)]
          [(has-symbol? token) (symbol-encode token)]
          [(number? token) (exact->inexact token)]
          [else (error (string-append "unkown symbol '" (symbol->string token) "' in program"))])))

    (define padding (- (*program-max-length*) (length alt-encode)))

    (cond
      [(> padding 0) (append alt-encode (build-list padding (lambda (x) (*program-placeholder*))))]
      [(< padding 0) (take alt-encode (*program-max-length*))]
      [else alt-encode])))