#lang racket

; 0. META FUNCTIONS
; we should be able to internalize them with recursions on nat eventually
(define (LIST-REC E C L)
  (cond
    [(null? L) E]
    [else (C L (LIST-REC E C (cdr L)))]))

(define (FOLDR E P L)
  (LIST-REC E
            (λ (L R)
              (P (car L) R))
            L))

(define (UNFOLD n f)
  (letrec ([go (λ (n acc)
                 (cond
                   [(zero? n) (cons (f 0) acc)]
                   [else (cons (f n) (go (sub1 n) acc))]))])
  (go n '())))

(define (ID x) x)

; 1. Recursive Functions on Nat

; 1.1 basecases ??
(define (const n)
  (λ (x) n))

(define (p-rec z s)
  (λ (n)
    (cond
      [(zero? n) z]
      [else (let ([n^ (sub1 n)])
              (s n ((p-rec z s) n^)))])))

(define (p-rec^ z s n)
    (cond
      [(zero? n) z]
      [(< n 0) (error "negative number")]
      [else (let ([n^ (sub1 n)])
              (s n ((p-rec z s) n^)))]))


; 1.2 usefularithmeticfunctions

; not zero?
(define sg (p-rec 0 (λ (n r) ((const 1) n))))
; eq zero? also serves as `not`
(define sg-op (p-rec 1 (λ (n r) ((const 0) n))))

; arithmetic funcs
; n + m (also as `or` for rels)
(define (add n m)
  (p-rec^ m (λ (n r) (add1 r)) n))
; n * m (also as `and` for rels)
(define (mul n m)
  (p-rec^ 0 (λ (n r) (add m r)) n))

(define (exp n m)
  (p-rec^ 1 (λ (m r) (mul n r)) m))

(define (sqr n)
  (exp n 2))

(define (pred n)
  (p-rec^ 0 (λ (n r) (sub1 n)) n))

; "arith. subtraction" := "positive part of" m - n
(define (sub m n)
  (p-rec^ m (λ (n r) (pred (sub m (sub1 n)))) n))

; absoulute difference
(define (diff n m)
  (add (sub n m) (sub m n)))

; 1.3 relations
; also some boolean algebra with sg
; n <= m iff  n - m <= 0
(define (leq n m)
  (sg-op (sub n m)))

; n < m iff 0 < m - n iff (sg (sub m n))
(define (lt n m)
  (sg (sub m n)))

; n = m
(define (eq n m)
  (sg-op (diff n m)))

; op rels 
(define (geq n m)
  (leq m n))

(define (gt n m)
  (lt m n))

; and p q iff p * q != 0
(define (and p q)
  (sg (mul p q)))

(define (or p q)
  (sg (add p q)))

; 1.4 SyntaticSugars

; case-split notation to define a function based on conditions up to user to guarantee independence
; here we use list recursions as meta functions but they should potentially be internalized

;; Case : [(val : nat . sg : nat)] -> nat
(define (Case L)
  (FOLDR 0
         (λ (x acc)
           (match-let ([(cons val sg) x])
             (add (mul val sg) acc)))
         L))
#;
(begin
  (define L1 `( (1 . ,(sg 1)) (2 . ,(sg 0)) (4 . ,(sg 1))))
  (case L1))

; Sum f(i) from i = 0 to n (also as n-ary `or`)
(define (Sum n f)
  (p-rec^ (f 0) (λ (n r) (add (f n) r)) n))

; Sum (f i) where i in [lo, up]
(define (SumRange lo up f)
  (Case `((0           . ,(gt lo up))
          (,(Sum up f) . ,(sg-op lo))
          (,(sub (Sum up f) (Sum (sub lo 1) f)) . ,(and (gt lo 0) (leq lo up))))))

; Prod is Not convenient if we start from zero since many functions map 0 to 0
(define (Prod lo up f)
  (p-rec^ 1
          (λ (n r)
            (cond
              [(< n lo) 1]
              [else (mul (f n) r)]))
          up))


; 1.5 Some more interesting cases
(define (div a n) ;; == floor(a/n) == largest number d  s.t.  n * d <= a
  (SumRange 1 a
            (λ (i)
              (Prod 1 i ;; this product zeroes iff i * n > a
                    (λ (j) (leq (mul j n) a))))))
; this `div` would take a million years to computer numbers above 20

(define (rem a n)
  (sub a (mul (div a n) n)))
