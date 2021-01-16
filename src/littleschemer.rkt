#lang racket

(provide (all-defined-out))
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (x)
    (cond
      ((null? x) #t)
      ((atom? (car x)) (lat? (cdr x)))
      (else #f))))

(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (member? x (cdr l))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define firsts
  (lambda (lats)
    (cond
      ((null? lats) '())
      (else (cons (caar lats)
                  (firsts (cdr lats)))))))

(define seconds
  (lambda (lats)
    (cond
      ((null? lats) '())
      (else (cons (cadar lats)
                  (seconds (cdr lats)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat))
       (cons old
             (cons new
                   (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

; in most of these numerical functions we assume numbers >= 0 !!

(define add1 (lambda (n)(+ n 1)))
(define sub1 (lambda (n)(- n 1)))

(define o+
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (add1 (o+ x (sub1 y)))))))

(define plus
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (plus (add1 x) (sub1 y))))))

(define o-
  (lambda (x y)
    (cond
      ((zero? y) x)
      (else (sub1 (o- x (sub1 y)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (m n)
    (cond
      ((zero? n) 0)
      (else
       (o+ m (x m (sub1 n)))))))

(define x2
  (lambda (m n)
    (letrec ([aux (lambda (result counter)
            (cond
              ((zero? counter) 0)
              ((zero? (sub1 counter)) result)      
              (else
               (aux (+ m result) (sub1 counter)))))])
      (aux m n))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2))
                  (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond      
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)      
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond ((zero? m) (zero? n))
          ((zero? n) #f)
          (else (o= (sub1 n) (sub1 m))))))

(define o=2
  (lambda (n m)
    (cond ((or (o< m n) (o> m n)) #f)
          (else #t))))

(define exp
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (exp n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond ((o< n m) 0)
          (else (add1 (div (- n m) m))))))


(define length
  (lambda (lat)
    (cond ((null? lat) 0)
          (else (+ 1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (car lat))
          (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond ((zero? (sub1 n)) (cdr lat))
          (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (or (and (number? a1) (number? a2) (= a1 a2))
               (eq? a1 a2))))

(define occur
  (lambda (a lat)
    (cond ((null? lat) 0)
          ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
          (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (and (number? n) (eqan? 1 n))))

(define rempick2
  (lambda (n lat)
    (cond ((one? n) (cdr lat))
          (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))

;(define rember*
;  (lambda (a l)
;    (cond
;      ((null? l) '())
;      ((and (atom? (car l))
;            (eqan? a (car l)))
;       (rember* a (cdr l)))
;      ((not (atom? (car l)))
;       (cons (rember* a (car l))
;             (rember* a (cdr l))))
;      (else (cons (car l) (rember* a (cdr l)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eqan? a (car l))
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else
       (cons (rember* a (car l))
             (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old
                (cons new
                      (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new
                (cons old
                      (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond ((null? l) 0)
          ((atom? (car l))
           (cond
             ((eq? (car l) a) (add1 (occur* a (cdr l))))
             (else (occur* a (cdr l)))))
          (else
           (+ (occur* a (car l))
              (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond ((null? l) '())
          ((atom? (car l))
           (cond ((eq? old (car l))
                  (cons new
                        (subst* new old (cdr l))))
                 (else (cons (car l) (subst* new old (cdr l))))))
          (else
           (cons (subst* new old (car l))
                 (subst* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond ((null? l) #f)
          ((atom? (car l))
           (or
             (eq? (car l) a)
             (member* a (cdr l))))
          (else
           (or (member* a (car l))
              (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; first version (without equal?)
;(define eqlist?
;  (lambda (a b)
;    (cond ((and (null? a) (null? b)) #t)
;          ((or (null? a) (null? b)) #f)         
;          ((and
;            (atom? (car a))
;            (atom? (car b)))
;           (and (eqan? (car a) (car b))
;                (eqlist? (cdr a) (cdr b))))
;          ((or (atom? (car a)) (atom? (car b))) #f)
;          (else (and (eqlist? (car a) (car b))
;                     (eqlist? (cdr a) (cdr b)))))))

(define equal?
  (lambda (s1 s2)
    (cond ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
          ((or (atom? s1) (atom? s2)) #f)
          (else (eqlist? s1 s2)))))

(define eqlist?
  (lambda (a b)
    (cond ((and (null? a) (null? b)) #t)
          ((or (null? a) (null? b)) #f)
          (else (and (equal? (car a) (car b))
                     (eqlist? (cdr a) (cdr b)))))))

(define rember2
  (lambda (s l)
    (cond ((null? l) '())
          ((equal? s (car l)) (cdr l))
          (else (cons (car l) (rember2 s (cdr l)))))))

(define operations (make-hash
                    '(
                     (+ (lambda (a b) (+ a b)))
                     (× (lambda (a b) (* a b)))
                     (↑ (lambda (a b) (expt a b))))))
(define numbered?
  (λ (e)
    (cond
      ((atom? e) (number? e))
      ((hash-has-key? operations (cadr e))
       (and (numbered? (car e)) (numbered? (caddr e))))
      (else #f))))

(define value
  (λ (e)
    (cond
      ((atom? e) e)
      ((hash-has-key? operations (operator e))
       (apply (eval (car (hash-ref operations (operator e))))
              (list
               (value (1st-sub-expr e))
               (value (2nd-sub-expr e)))))
      (else (raise 'unknown-operation)))))

; prefix
(define operator (λ (e) (car e)))
(define 1st-sub-expr (λ (e) (cadr e)))
(define 2nd-sub-expr (λ (e) (caddr e)))
; infix
;(define operator (λ (e) (cadr e)))
;(define 1st-sub-expr (λ (e) (car e))) 
;(define 2nd-sub-expr (λ (e) (caddr e)))

(define sero? (λ (n) (null? n)))
(define edd1 (λ (n) (cons '() n)))
(define zub1 (λ (n) (cdr n)))
(define pluz
  (λ (n m)
    (cond ((sero? m) n)
          (else (edd1 (pluz n (zub1 m)))))))

(define set?
  (λ (lat)
    (cond ((null? lat) #t)
          ((member? (car lat) (cdr lat)) #f)
          (else
           (set? (cdr lat))))))

(define makeset
  (λ (lat)
    (cond ((null? lat)'())
          ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
          (else (cons (car lat) (makeset (cdr lat)))))))
(define makeset2
  (λ (lat)
    (cond ((null? lat)'())          
          (else (cons (car lat)
                      (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (λ (set1 set2)
    (cond ((null? set1) #t)
          (else (and (member? (car set1) set2)
                     (subset? (cdr set1) set2))))))

(define eqset?
  (λ (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (λ (set1 set2)
    (cond ((null? set1) #f)
          (else (or (member? (car set1) set2)
                    (intersect? (cdr set1) set2))))))

(define intersect
  (λ (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (cons (car set1)
                 (intersect (cdr set1) set2)))
          (else (intersect (cdr set1) set2)))))

(define union
  (λ (set1 set2)
    (cond ((null? set1) set2)
          ((member? (car set1) set2)
           (union (cdr set1) set2))
          (else (cons (car set1)
                      (union (cdr set1) set2))))))

(define except
  (λ (set1 set2)
    (cond ((null? set1) '())
          ((member? (car set1) set2)
           (except (cdr set1) set2))
          (else (cons (car set1)
                      (except (cdr set1) set2))))))

(define intersectall
  (λ (l-set)
    (cond ((null? l-set) '())
          ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set)
                           (intersectall (cdr l-set)))))))

(define a-pair?
  (λ (l)
    (cond ((atom? l) #f)
          ((null? l) #f)
          ((null? (cdr l)) #f)
          ((null? (cddr l)) #t)
          (else #f))))

(define first (λ (p) (car p)))
(define second (λ (p) (cadr p)))
(define build (λ (s1 s2) (cons s1 (cons s2 '()))))
(define fun? (λ (rel) (set? (firsts rel))))
(define revpair (λ (p) (build (second p) (first p))))

(define revrel
  (λ (rel)
    (cond
      ((null? rel) '())
      (else
       (cons (revpair (car rel))
             (revrel (cdr rel)))))))

(define fullfun?
  (λ (fun)
    (fun? (revrel fun))))
(define one-to-one? fullfun?)

;(define rember-f
;  (λ (test? s l)
;    (cond ((null? l) '())
;          ((test? s (car l)) (cdr l))
;          (else (cons (car l) (rember-f test? s (cdr l)))))))

(define eq?-c (λ (a) (λ (x) (eq? x a))))
(define eq?-salad (eq?-c 'salad))
(define rember-f
  (λ (test?)
    (λ (s l)
      (cond ((null? l) '())
            ((test? s (car l)) (cdr l))
             (else (cons (car l) ((rember-f test?) s (cdr l))))))))

(define rember-eq? (rember-f eq?))

(define insertL-f
  (λ (test?)
    (λ (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons new lat))
        (else (cons (car lat)
                    ((insertL-f test?) new old (cdr lat))))))))

(define insertR-f
  (λ (test?)
    (λ (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat)) (cons old (cons new (cdr lat))))
        (else (cons (car lat)
                    ((insertR-f test?) new old (cdr lat))))))))

(define seqL (λ (new old l) (cons new (cons old l))))
(define seqR (λ (new old l) (cons old (cons new l))))

(define insert-g
  (λ (seq)
    (λ (new old lat)
      (cond ((null? lat) '())
            ((eq? old (car lat)) (seq new old (cdr lat)))
            (else (cons (car lat)
                        ((insert-g seq) new old (cdr lat))))))))

(define insertL2 (insert-g
                  (λ (new old l)
                    (cons new (cons old l)))))
(define insertR2 (insert-g
                  (λ (new old l)
                    (cons old (cons new l)))))
                 
(define subst2 (insert-g (λ (new old l)
                            (cons new l))))

(define atom-to-function
  (λ (atom)
    (cond ((eq? atom '+) +)
          ((eq? atom '×) *)
          (else expt))))

(define value2
  (λ (nexp)
    (cond ((atom? nexp) nexp)
          (else ((atom-to-function (operator nexp))
                 (value2 (1st-sub-expr nexp))
                 (value2 (2nd-sub-expr nexp)))))))

(define multirember-f
  (λ (test?)
    (λ (a lat)
      (cond ((null? lat) '())
            ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
            (else (cons (car lat)
                        ((multirember-f test?) a (cdr lat))))))))

(define eq?-tuna (eq?-c ' tuna))
(define multiremberT
  (λ (test? lat)
    (cond ((null? lat) '())
          ((test? (car lat)) (multiremberT test? (cdr lat)))
          (else (cons (car lat)
                      (multiremberT test? (cdr lat)))))))

(define multirember&co
  (λ (a lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? (car lat) a)
           (multirember&co a (cdr lat) (λ (newlat seen)
                                         (col newlat (cons (car lat) seen)))))
          (else
           (multirember&co a (cdr lat) (λ (newlat seen)
                                         (col (cons (car lat) newlat) seen)))))))


(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL)
       (cons new
             (cons oldL
                   (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR)
       (cons oldR
             (cons new
                   (multiinsertLR new oldL oldR (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertLR new oldL oldR (cdr lat)))))))

(define multiinsertLR&co
  (λ (new oldL oldR lat col)
    (cond
      ((null? lat)
       (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                           (col (cons new (cons oldL newlat)) (+ 1 L) R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat)
                         (λ (newlat L R)
                           (col (cons oldR (cons new newlat)) L (+ 1 R)))))
      (else
       (multiinsertLR&co new oldL oldR (cdr lat)
                      (λ (newlat L R)
                        (col (cons (car lat) newlat) L R)))))))

(define evens-only*
  (λ (l)
    (cond ((null? l) '())
          ((atom? (car l)) (cond ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
                                 (else (evens-only* (cdr l)))))
          (else (cons (evens-only* (car l))
                      (evens-only* (cdr l)))))))

(define evens-only*&co
  (λ (l col)
    (cond ((null? l) (col '() 1 0))
          ((atom? (car l))
           (cond ((even? (car l))
                  (evens-only*&co (cdr l)
                                  (λ (new p s)
                                    (col (cons (car l) new) (* (car l) p) s))))
                 (else
                  (evens-only*&co (cdr l)
                                  (λ (new p s)
                                    (col new p (+ (car l) s)))))))
          (else
           (evens-only*&co (car l)
                           (λ (al ap as)
                             (evens-only*&co (cdr l)
                                             (λ (dl dp ds)
                                               (col (cons al dl) (* ap dp) (+ as ds))))))))))

(define looking
  (λ (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (λ (a n lat)
    (cond ((number? n) (keep-looking a (pick n lat) lat))
          (else (eq? a n)))))

(define shift
  (λ (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))

(define align
  (λ (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (align (shift pora)))
          (else (build (first pora)
                       (align (second pora)))))))

(define length*
  (λ (pora)
    (cond ((atom? pora) 1)
          (else (+ (length* (first pora))
                   (length* (second pora)))))))

(define weight*
  (λ (pora)
    (cond ((atom? pora) 1)
          (else (+ (* 2 (weight* (first pora)))
                   (weight* (second pora)))))))

(define shuffle
  (λ (pora)
    (cond ((atom? pora) pora)
          ((a-pair? (first pora))
           (shuffle (revpair pora)))
          (else (build (first pora)
                       (shuffle (second pora)))))))

(define eternity
  (λ (x) (eternity x)))

(define C
  (λ (n)
    (cond ((one? n) 1)
          ((even? n) (C (/ n 2)))
          (else (C (+ 1 (* 3 n)))))))

(define Ci
  (λ (n)
    (cond ((one? n) '(1))
          (else (let ([c (cond ((even? n)
                                (/ n 2))
                               (else (+ 1 (* 3 n))))])
                  (cons n (Ci c)))))))

;(require plot)
;(define plot-collatz
;  (λ (n)
;    (let* ([cs (Ci n)]
;           [f (make-fun cs)])
;      (plot (function f 0 (length cs))))))        

(define make-fun
  (λ (l)    
    (λ (x)
      (list-ref l (floor x)))))

(define A
  (λ (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
               (A n (sub1 m)))))))

(define will-stop?
  (λ (f)
    (and (f '()) #t)))

(((lambda (le)
    ((lambda (make-length)
       (make-length make-length))
     (lambda (make-length)
       (le
        (lambda (x) ((make-length make-length) x))))))
  (lambda (length)
    (lambda (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(1 2))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define new-entry build)

(define look-in-entry
  (λ (name entry entry-f)
    (letrec ([helper
           (λ (names values)
             (cond ((null? names) (entry-f name))
                   ((eq? name (car names)) (car values))
                   (else (helper (cdr names) (cdr values)))))])
      (helper (first entry) (second entry)))))

(define extend-table cons)

(define look-in-table
  (λ (name table table-f)
    (cond ((null? table) (table-f name))
          (else
           (look-in-entry name (car table) (λ (x) (look-in-table name (cdr table) table-f)))))))

(define expression-to-action
  (λ (e)
    (cond ((atom? e) (atom-to-action e))
          (else (list-to-action e)))))

(define atom-to-action
  (λ (e)
    (cond ((number? e) *const)
          ((eq? e #t) *const)
          ((eq? e #f) *const)
          ((eq? e (quote cons)) *const)
          ((eq? e (quote car)) *const)
          ((eq? e (quote cdr)) *const)
          ((eq? e (quote eq?)) *const)
          ((eq? e (quote null?)) *const)
          ((eq? e (quote atom?)) *const)
          ((eq? e (quote zero?)) *const)
          ((eq? e (quote add1)) *const)
          ((eq? e (quote sub1)) *const)
          ((eq? e (quote number?)) *const)
          (else *identifier))))

(define list-to-action
  (λ (e)
    (cond ((atom? (car e))
           (cond
             ((eq? (car e) (quote quote)) *quote)
             ((eq? (car e) (quote lambda)) *lambda)
             ((eq? (car e) (quote cond)) *cond)
             (else *application)))
          (else *application))))

(define val (λ (e)  (meaning e '())))

(define meaning
  (λ (e table)
    ((expression-to-action e) e table)))

(define *const
  (λ (x t)
    (cond ((number? x) x)
          ((eq? x #f) #f)
          ((eq? x #t) #t)
          (else (build (quote primitive) x)))))

(define text-of second)
(define *quote (λ (x t) (text-of x)))
(define *identifier (λ (x t) (look-in-table x t initial-table)))
(define initial-table (λ (name) (car '())))

(define *lambda (λ (x t) (build (quote non-primitive) (cons t (cdr x)))))
(define table-of first)
(define formals-of second)
(define body-of third)

(define *cond (λ (x t) (evcon (cond-lines-of x) t)))
(define evcon
  (λ (lines table)
    (cond ((else? (question-of (car lines)))
           (meaning (answer-of (car lines)) table))
          ((meaning (question-of (car lines)) table)
           (meaning (answer-of (car lines)) table))
          (else (evcon (cdr lines) table)))))
(define cond-lines-of cdr)
(define question-of first)
(define answer-of second)
(define else?
  (λ (x)
    (cond ((atom? x) (eq? x (quote else)))
          (else #f))))
               
(define *application
  (λ (e table)
    (plyapp (meaning (function-of e) table)
           (evlis (arguments-of e) table))))
(define evlis
  (λ (args table)
    (cond ((null? args) '())
          (else (cons (meaning (car args) table)
                      (evlis (cdr args) table))))))
(define function-of car)
(define arguments-of cdr)
(define primitive? (λ (l) (eq? (first l) 'primitive)))
(define non-primitive? (λ (l) (eq? (first l) 'non-primitive)))
(define plyapp
  (λ (fun vals)
    (cond ((primitive? fun) (apply-primitive (second fun) vals))
          ((non-primitive? fun) (apply-closure (second fun) vals)))))
(define apply-primitive
  (λ (name vals)
    (cond ((eq? name 'cons)
           (cons (first vals) (second vals)))
          ((eq? name 'car)
           (car (first vals)))
          ((eq? name 'cdr)
           (cdr (first vals)))
          ((eq? name 'null?)
           (null? (first vals)))
          ((eq? name 'eq?)
           (eq? (first vals) (second vals)))
          ((eq? name 'atom?)
           (:atom? (first vals)))
          ((eq? name 'zero?)
           (zero? (first vals)))
          ((eq? name 'add1)
           (add1 (first vals)))
          ((eq? name 'sub1)
           (sub1 (first vals)))
          ((eq? name 'number?)
           (number? (first vals))))))
(define :atom?
  (λ (x)
    (cond ((atom? x) #t)
          ((null? x) #f)
          ((eq? (car x) 'primitive) #t)
          ((eq? (car x) 'non-primitive) #t)
          (else #f))))
(define apply-closure
  (λ (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure)
                         vals)
              (table-of closure)))))


