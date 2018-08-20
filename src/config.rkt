#lang racket
(require racket/runtime-path)
(provide (all-defined-out))

(define-runtime-path viz-output-path "../www/viz/")
(define-runtime-path web-resource-path "web/")

;; Flag Stuff

(define all-flags
  #hash([precision . (double fallback)]
        [fn . (cbrt)] ;; TODO: This is a bad way to disable functions: figure out a better one
        [setup . (simplify early-exit)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error post-process binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic numerics complex special bools branches)]))

(define default-flags
  #hash([precision . (double fallback)]
        [fn . (cbrt)]
        [setup . (simplify)]
        [generate . (rr taylor simplify)]
        [reduce . (regimes taylor simplify avg-error binary-search branch-expressions)]
        [rules . (arithmetic polynomials fractions exponents trigonometry hyperbolic complex special bools branches)]))

(define (enable-flag! category flag)
  (define (update cat-flags) (set-add cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (disable-flag! category flag)
  (define (update cat-flags) (set-remove cat-flags flag))
  (*flags* (dict-update (*flags*) category update)))

(define (flag-set? class flag)
  (set-member? (dict-ref (*flags*) class) flag))

; `hash-copy` returns a mutable hash, which makes `dict-update` invalid
(define *flags* (make-parameter (make-immutable-hash (hash->list default-flags))))

(define (changed-flags)
  (filter identity
          (for*/list ([(class flags) all-flags] [flag flags])
            (match* ((flag-set? class flag)
                     (parameterize ([*flags* default-flags]) (flag-set? class flag)))
              [(#t #t) #f]
              [(#f #f) #f]
              [(#t #f) (list 'enabled class flag)]
              [(#f #t) (list 'disabled class flag)]))))

;; Number of points to sample for evaluating program accuracy
(define *num-points* (make-parameter 256))

;; Number of iterations of the core loop for improving program accuracy
(define *num-iterations* (make-parameter 4))

;; The step size with which arbitrary-precision precision is increased
;; DANGEROUS TO CHANGE
(define *precision-step* (make-parameter 256))

;; Maximum MPFR precision allowed during exact evaluation
(define *max-mpfr-prec* (make-parameter 10000))

;; In periodicity analysis,
;; this is how small the period of a function must be to count as periodic
(define *max-period-coeff* 20)

;; In localization, the maximum number of locations returned
(define *localize-expressions-limit* (make-parameter 4))

(define *binary-search-test-points* (make-parameter 16))

;; How accurate to make the binary search
(define *binary-search-accuracy* (make-parameter 48))

;;; About Herbie:
(define (run-command cmd)
  (parameterize ([current-error-port (open-output-nowhere)])
    (string-trim (with-output-to-string (λ () (system cmd))))))

(define (git-command #:default [default ""] gitcmd . args)
  (if (directory-exists? ".git")
      (let* ([cmd (format "git ~a ~a" gitcmd (string-join args " "))]
             [out (run-command cmd)])
          (if (equal? out "") default out))
      default))

(define *herbie-version* "1.2")

(define *hostname* (run-command "hostname"))

(define *herbie-commit*
  (git-command "rev-parse" "HEAD" #:default *herbie-version*))

(define *herbie-branch*
  (git-command "rev-parse" "--abbrev-ref" "HEAD" #:default "release"))

;; Symbols(like '+', '-', ...) are encoded into integer
;; encoded-value = *operator-id-base* + index
(define *operator-id-base* (make-parameter 65536))

;; Variables(like 'x', 'y', ...) are encoded into integer
;; encoded-value = *variable-id-base* + index
(define *variable-id-base* (make-parameter 60000))

;; Max length of a program
(define *program-max-length* (make-parameter 480))

;; Max number of variables in a program
(define *variable-max-num* (make-parameter 32))

;; Placeholder for program when the length of program
;; is less than *program-max-length*
(define *program-placeholder* (make-parameter 0))

;; Placeholder for points when the number of points
;; is less than *variable-max-num*
(define *point-placeholder* (make-parameter 0))

;; Host and port of socket server which provides rule selection service
(define *socket-host* (make-parameter "localhost"))
(define *socket-port* (make-parameter 4399))

;; Max errors between approximate and exact value
;; Error represents the number of floating-point values between the approximate and exact values
(define *double-error-threshold* (make-parameter 40))

;; Same as *double-error-threshold*,
;; but only works for single precision floating-point numbers
(define *float-error-threshold* (make-parameter 20))
