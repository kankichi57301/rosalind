#lang racket
;; rosalind
;; Solve the Turnpike Problem
;; [BA4M] 2021/
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba4m_out* "data\\ba4m_out.txt")
(define *time* #f)
(define *ans* '())

(define (ros_ba4m . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba4m.txt"
		    (format "data\\rs_ba4m~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data))))
	 (nlist+  (filter positive? nlist))
	 (res '())
	 )
    (displayln (format "len=~a" (length nlist+)))
    ;;(set! res (sort (solve_pdpl nlist+) <))
    
    ;;(displayln (length nlist+))
    #|
    (call-with-output-file *ba4m_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))

      #:exists 'truncate/replace)
    |#
					;res
     
))


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
		   (if (include? diffs rest)
		       (let ((rest2 (exclude diffs rest)))
			 (if (null? rest2)
			     (cons cand result)
			     (solve_pdplx  rest2 (cons cand result) cand)))
		       #f))))))
    
     (sort (solve_pdplx (delete maxval nlist) `(0 ,maxval) 0) <)
  )
)

(define (all-diffs nset)
  (sort
   (map (lambda(x)(apply - x))
	(filter (lambda(x)(apply > x))
		(cartesian-product nset nset)))
   <))

(define x '(0 4 8 11 14 17 22 26 31 33 38 42 46 50 57 58 62 79 82 91))
(define y (all-diffs x))
