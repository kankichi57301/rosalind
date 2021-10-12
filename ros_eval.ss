#lang racket
;; rosalind
;; Expected Number of Restriction Sites
;; [EVAL]

(require srfi/13)
(require "readfileA.ss")
;(define *eval_out* "data\\eval_out.txt")

(define (myround x)
  (/ (round (* x 10000)) 10000))


(define (ros_eval . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_eval.txt"
		    (format "data\\rs_eval~a.txt" (car n)))))
	 (n (string->number (car data)))
	 (enkilist (string->list (cadr data)))
	 (cg-ratio-list (map string->number (string-tokenize (caddr data)))))

    ;;(list n enkilist cg-ratio-list)
    
    (for-each (lambda(x)(display (format "~s " x)))
    (map (lambda(x)(myround(* (- n 1) (enki-seq-prob enkilist x)))) cg-ratio-list))
    
   ))

(define (enki-seq-prob enkilis cg-ratio)
  (* (apply * (map (lambda(x)
		  (case x
		    [(#\A #\T) (/ (- 1 cg-ratio) 2)]
		    [(#\C #\G) (/ cg-ratio 2)]))
		enkilis))))


(define nuc-prob '((a 0.125)(b 0.375)(c 0.125)(d 0.375)))

(define (mycount lst1 lst2)
  (if (< (length lst1)(length lst2))
      0
      (+ (if (equal? (take lst1 (length lst2)) lst2) 1  0)
	 (mycount (cdr lst1) lst2))))

(define (power-cartesian-product set n)
  (if (= 1 n)
      (map list set)
      (map (lambda(x)(apply append x))
	   (cartesian-product (map list set) 
			      (power-cartesian-product set (- n 1))
	   )
      )   
  )
)  
#|
(define (include-ab-cnt-kitati n)
  (apply +
  (map (lambda(x)(apply * x))
	      (map (lambda(x)(list (mycount x '(a b)) (frag-prob x)))
		   (power-cartesian-product '(a b c d) n)))))
|#
(define (include-ab-cnt n)
  (map length
       (group-by identity
		 (map (lambda(x)(mycount x '(a b)))
		      (power-cartesian-product '(a b c d) n)))))

;;
;;times should be .LE. len/2
;;
(define *my-hash* #f)

(define (number-of-case-include-ab-ntimes len  kaisuu)
  (let ((it (hash-ref *my-hash* (list len kaisuu) #f)))
    (if it
	it
	(if (or (< len (* 2 kaisuu))(< kaisuu 0))
	    0
	    (let ((ret
		   (case len
		     
		     [(1)(if (= kaisuu 0)
			     4 0)
		      ]
		     [(2)
		(if (= kaisuu 1)
		    1
		    15)]
		     [else
		(+ (- (number-of-case-include-ab-ntimes (- len 1) kaisuu)
		      (number-of-case-include-ab-ntimes (- len 2) kaisuu)
		      )
		   (* 3 (number-of-case-include-ab-ntimes (- len 1) kaisuu))
		   (number-of-case-include-ab-ntimes (- len 2) (- kaisuu 1)))]
	       )))
	      ;;(displayln (format "arg=~s ~s = ~s" len kaisuu ret))
	      (hash-set! *my-hash* (list len kaisuu) ret)
	      ret
	      )))))

(define (n-i-ab len kaisuu)
  (set! *my-hash* (make-hash))
  (number-of-case-include-ab-ntimes len  kaisuu))

(define (kitaiti-ab n)
  (/
   (apply +
	  (map (lambda(x)(* x (n-i-ab n x)))(range 1 (+ 1 (/ n 2)))))
  (expt 4 n)))
