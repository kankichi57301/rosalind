#lang racket
;; rosalind
;;Creating a Restriction Map  
;;
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")

(define *pdpl_out* "data\\pdpl_out.txt")
(define *all* '())

(define (ros_pdpl . n)
      (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_pdpl.txt"
		    (format "data\\rs_pdpl~a.txt" (car n)))))
	     (nlist (sort (map string->number (string-tokenize (car data))) <))
	     (res '())
	     )
	(set! res (solve_pdpl nlist))
	(call-with-output-file *pdpl_out*
	  (lambda(out)
	    (for-each (lambda(x)(display (format "~a " x) out))
		      res))
	  #:exists 'truncate/replace)
	res
))

(define (read_pdpl . n)
      (let* ((data (read-file*
		(if (null? n)
		    "rosalind_pdpl.txt"
		    (format "rs_pdpl~a.txt" (car n)))))
	     (nlist (sort (map string->number (string-tokenize (car data))) <))
	     )
	(set! *all* nlist)
))




(define (all-diffs nset)
  (sort
   (map (lambda(x)(apply - x))
	(filter (lambda(x)(apply > x))
		(cartesian-product nset nset)))
   <))


(define (solve_pdpl nlist)
   (let* ((all nlist)
	  (maxval (apply max nlist))
	  (anslen (inexact->exact (+ 0.5 (* 0.5 (sqrt (+ 1 (* 8.0 (length nlist))))))))
	  (cands  (filter (lambda(x)(member (- maxval x) nlist))  nlist)))

     (define (solve_pdplx rest result min)
       (ormap (lambda(x)(solve_pdply x rest result))
	      (filter (lambda(x)(> x min)) rest)
	      ))
    
     (define (solve_pdply cand rest result)
       (if (not (member cand cands))
	   #f
	   (begin
	     (if (= anslen (length result))
		 result
		 (let ((diffs (map (lambda(x)(abs (- cand x))) result)))
		   (if (include? diffs all)
		       (let ((rest2 (exclude diffs rest)))
			 (if (null? rest2)
			     (cons cand result)
			     (solve_pdplx  rest2 (cons cand result) cand)))
		       #f))))))
    
     (sort (solve_pdplx (delete maxval nlist) `(0 ,maxval) 0) <)
  )
)
(define x '(0 1 3 5 10 12 14 19))
(define y (all-diffs x))


