#lang racket
;; rosalind
;; The Wright-Fisher Model of Genetic Drift
;; [WFMD] 2020/12/26 AC
;; 2021/10/21 AC
(require srfi/1)
(require srfi/13)
(require math/number-theory)

(require "readfileA.ss")
(require "roslibA.ss")


(define (ros_wfmd . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_wfmd.txt"
		    (format "data\\rs_wfmd~a.txt" (car n))))))
    (apply solve74
	   (map string->number (string-tokenize (car data))))
   ))

;; N # of diploid
;; m copies of a dominant allele

(define (prob-001 N m)
  (let* ((2N (* N 2))
	 (p (/ m 2N))
	 (q (- 1 p)))
    (map (lambda(x)(* 1.0 (binomial 2N x)(expt q x)(expt p (- 2N x))))
	 (iota (+ 2N 1)))))

(define (prob-002 N t)
  (let ((2N (* N 2)))
    (map (lambda(i) (* (binomial 2N t)*(expt (/ i 2N) t)(expt (- 1 (/ i 2N))(- 2N t))))
	 (iota (+ 2N 1))
	 )))



(define (solve74-0 N m g)
  (if (= g 1)
      (prob-001 N m)
      (m-v* (mytest002 (* 2 N))
	    (solve74-0 N m (- g 1)))

  )
)

(define (solve74 N m g k)
  (/ (floor (* 10000
	       (apply + (drop (solve74-0 N m g) k)))) 10000)) 


(define (mytest001 n p)
  (map (lambda(i)(* (binomial n i)(expt p i)(expt (- 1 p)(- n i))))
       (iota (+ 1 n))))

(define (mytest002 n)
  (transpose
   (map (lambda(x)(mytest001 n (/ x n)))
	(iota (+ 1 n)))))


(define (m-v* matrix vector)
  (map (lambda(x)(inner-product vector x)) matrix))

(define (inner-product v1 v2)
  (apply + (map * v1 v2)))


