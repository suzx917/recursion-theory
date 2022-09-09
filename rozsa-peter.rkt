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

; 1. Recursive Functions on Nat

; 1.1 basecases ??
(define (const n)
  (λ (x) n))

(define (p-rec z s)
  (λ (n)
    (cond
      [(zero? n) z]
      [else (let ([n^ (sub1 n)])
              (s n^ ((p-rec z s) n^)))])))

(define (p-rec^ z s n)
    (cond
      [(zero? n) z]
      [else (let ([n^ (sub1 n)])
              (s n^ ((p-rec z s) n^)))]))


; 1.2 usefularithmeticfunctions

; signal fuction that maps 0 to 0 and everything else to 1
(define sg (p-rec 0 (λ (n r) ((const 1) n))))
; complement signal that maps 0 to 1 and others to 0
(define sg-op (p-rec 1 (λ (n r) ((const 0) n))))

; arithmetic funcs
(define (add n m)
  (p-rec^ m (λ (n r) (add1 r)) n))

(define (mul n m)
  (p-rec^ 0 (λ (n r) (add m r)) n))

(define (pred n)
  (p-rec^ 0 (λ (n r) sub1 n) n))

; "arith. subtraction" := "positive part of" m - n
(define (sub m n)
  (p-rec^ m (λ (n r) (pred r)) n))

; absoulute difference
(define (diff n m)
  (add (sub n m) (sub m n)))

; 1.3 relations
; also some boolean algebra with sg
; n <= m iff  <= 0
(define (leq n m)
  (sg-op (sub n m)))

; n < m iff 0 < m - n iff (sg-op (sub m n))
(define (lt n m)
  (sg-op (sub m n)))

; n = m
(define (eq n m)
  (sg-op (diff n m)))

; n >= m
(define (geq n m)
  (sg

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

; Sum (f i) where i in [lo, up]
(define (Sum i lo up f)
  (Case `(( . ,()
          ))