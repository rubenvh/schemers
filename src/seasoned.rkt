#lang racket
(require "littleschemer.rkt")

(define my-two-in-a-row?
  (λ (lat)
    (cond ((or (null? lat) (null? (cdr lat))) #f)
          (else (or (eq? (car lat) (cadr lat))
                    (my-two-in-a-row? (cdr lat)))))))

(define two-in-a-row-2?
  (λ (lat)
    (cond ((null? lat) #f)
          (else (or (is-first? (car lat) (cdr lat))
                    (two-in-a-row-2? (cdr lat)))))))
(define is-first?
  (λ (a lat)
    (cond ((null? lat) #f)
          (else (eq? a (car lat))))))

(define two-in-a-row-3?
  (λ (lat)
    (cond ((null? lat) #f)
          (else (is-first-b? (car lat) (cdr lat))))))
(define is-first-b?
  (λ (a lat)
    (cond ((null? lat) #f)
          (else (or (eq? a (car lat))
                    (two-in-a-row-3? lat))))))

(define two-in-a-row-b?
  (λ (preceding lat)
    (cond ((null? lat) #f)
          (else (or (eq? preceding (car lat))
                    (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row-final?
  (λ (lat)
    (cond ((null? lat) #f)
          (else (two-in-a-row-b? (car lat) (cdr lat))))))


(define sum-of-prefixes-old
  (λ (tup)
    (cond ((null? tup) '())
          (else (sum-of-prefixes-aux 0 tup)))))
(define sum-of-prefixes-aux
  (λ (p tup)
    (cond ((null? tup) '())
          (else (cons (+ p (car tup)) (sum-of-prefixes-aux (+ p (car tup)) (cdr tup)))))))

(define scramble-old
  (λ (tup) (scramble-b tup '())))
(define scramble-b
  (λ (tup rev-pre)
    (cond ((null? tup) '())
          (else
           (cons (pick (car tup) (cons (car tup) rev-pre))          
                 (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define multiremberY
  (λ (a lat)
    ((Y (λ (mr)
          (λ (lat)
            (cond ((null? lat) '())
                  ((eq? a (car lat)) (mr (cdr lat)))
                  (else (cons (car lat) (mr (cdr lat))))))))
     lat)))

(define multirember
  (λ (a lat)
    (letrec
     ([mr (λ (lat)
            (cond ((null? lat) '())
                  ((eq? a (car lat)) (mr (cdr lat)))
                  (else (cons (car lat) (mr (cdr lat))))))])
     (mr lat))))

(define multirember-f
  (λ (test?)
    (letrec
        ([mr (λ (a lat)
               (cond ((null? lat) '())
                     ((test? a (car lat)) (mr a (cdr lat)))
                     (else (cons (car lat)
                                 (mr a (cdr lat))))))])
      mr)))

(define member?
  (λ (a lat)
    (letrec ([m? (λ (lat)
                  (cond ((null? lat) #f)
                        (else (or (eq? a (car lat))
                                  (m? (cdr lat))))))])
      (m? lat))))

(define union
  (λ (set1 set2)
    (letrec ([U (lambda (set)
                   (cond ((null? set) set2)                         
                         ((M? (car set) set2) (U (cdr set)))
                         (else (cons (car set) (U (cdr set))))))]
             [M? (lambda (a lat)
                   (letrec ([N? (lambda (lat)
                                  (cond ((null? lat) #f)
                                        (else (or (eq? a (car lat))
                                                  (N? (cdr lat))))))])
                     (N? lat)))])
      (U set1))))

(define two-in-a-row?
  (letrec ([aux (λ (preceding lat)                    
                  (cond ((null? lat) #f)
                        (else (or (eq? preceding (car lat))
                                  (aux (car lat) (cdr lat))))))])
    (lambda (lat)
      (cond ((null? lat) #f)
            (else (aux (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (letrec ([aux (λ (p tup)
                  (cond ((null? tup) '())
                        (else (cons (+ p (car tup)) (aux (+ p (car tup)) (cdr tup))))))])
    (λ (tup)
      (cond ((null? tup) '())
            (else (aux 0 tup))))))

(define scramble
  (letrec ([aux (λ (tup rev-pre)
                  (cond ((null? tup) '())
                        (else
                         (cons (pick (car tup) (cons (car tup) rev-pre))          
                               (aux (cdr tup) (cons (car tup) rev-pre))))))])
    (λ (tup) (aux tup '()))))
  

(define intersect-before-letrec
  (lambda (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
    (letrec ([aux (lambda (set)
                    (cond ((null? set) '())
                          ((member? (car set) set2) (cons (car set) (aux (cdr set))))
                          (else (aux (cdr set)))))])
      (cond ((null? set2) '())
            (else (aux set1))))))

(define intersectall-toomanychecks
  (lambda (sets)
    (cond ((null? sets) '())
          ((null? (cdr sets)) (car sets))
          (else (intersect (car sets) (intersectall (cdr sets)))))))

(define intersectall-nocontinuation
  (letrec ([I (lambda (sets)
                (cond ((null? (cdr sets)) (car sets))
                      (else (intersect (car sets) (I (cdr sets))))))])
    (lambda (sets)
      (cond ((null? sets) '())
            (else (I sets))))))

(define intersectall
  (lambda (sets)
    (let/cc hop
      (letrec ([A (lambda (sets)
                (cond ((null? (car sets)) (hop '()))
                      ((null? (cdr sets)) (car sets))
                      (else (I (car sets) (A (cdr sets))))))]
               [I (lambda (set1 set2)
                    (letrec ([aux (lambda (set)
                                    (cond ((null? set) '())
                                          ((member? (car set) set2) (cons (car set) (aux (cdr set))))
                                          (else (aux (cdr set)))))])
                      (cond ((null? set2) (hop '()))
                            (else (aux set1)))))])    
        (cond ((null? sets) '())
              (else (A sets)))))))