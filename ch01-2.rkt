; Re-implemented functions in ch01-1.rkt with more efficient code
#lang racket

(define (b2i b)
  (cond
    [b 1]
    [else 0]))

; rec funcs and syntactic sugars
(define add +)
(define mul *)
(define (sub x y)
  (let ([s (- x y)])
    (if (> s 0)
        s
        0)))
(define (diff x y) (add (sub x y) (sub y x)))

(define quo quotient)
(define rem remainder)

(define (Sum lo up f)
  (for/fold ([sum 0])
            ([i (in-range lo (add1 up))])
    (+ sum (f i))))

(define (Prod lo up f)
  (for/fold ([prod 1])
            ([i (in-range lo (add1 up))])
    (* prod (f i))))

; rels
(define (sg-op n)
  (cond
    [(zero? n) 1]
    [else      0]))

(define (sg n)
  (cond
    [(zero? n) 0]
    [else      1]))

(define eq (λ (n m) (b2i (eqv? n m))))
(define leq (λ (n m) (b2i (<= n m))))
(define lt (λ (n m) (b2i (< n m))))
(define geq (λ (n m) (b2i (>= n m))))
(define gt (λ (n m) (b2i (> n m))))
(define and (λ (n m) (sg (mul n m))))
(define or (λ (n m) (sg (add n m))))

;;
;; New Functions
;;
(define (divides n a)
  (sg-op (rem a n)))

; check primes by requiring not having proper divisors
(define (is-prime k)
  (and (gt k 1)
       (sg-op (Sum 2 (sub k 1)
                   (λ (i) (divides i k))))))

; check primes by requiring having n-2 number of divisors
; RF p31 `S(n)`
(define (is-prime^ k)
  (let ([s (Sum 1 k (λ (n)
                      (sg-op (rem k n))))])
    (eq s 2)))

; counting primes up to n
(define (π n)
  (Sum 2 n (λ (k) (is-prime k))))

; we can't have this with just prim rec.
; (define (least-such n p) (?))
; unless also giving an upper bound
(define (least-such-from-upto n p lower upper)
  (Sum lower upper
       (λ (i) (Prod lower i (λ (j)
                              (void))))))
