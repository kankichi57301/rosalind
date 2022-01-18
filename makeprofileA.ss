(module rosalind racket/base
	(provide (all-defined-out))
	(require (only-in racket/list count))
	(require (only-in srfi/1 zip))
;;
;;filename:makeprofileA.ss
;;

(define (gap-count lst )
  (count(lambda(c)(equal? c #\-))
	lst))
;;
;;ex. '(#\B #\B #\D #\D #\D)=>'( 0 2 0 3 0)
;;

(define (sym-count symlst sigmas)
  (map (lambda(s)(count(lambda(c)(equal? c s))
		       symlst))
       sigmas))

(define (sym-count-m symlistlist sigmas)
  (sym-count (apply append symlistlist) sigmas)) 

(define (normalize-num nlist)
  (let ((sum (apply + nlist)))
    (map (lambda(x)(* 1.0 (/ x sum))) nlist)))
;;
;;
;;
)
