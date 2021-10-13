#lang racket
;;
;; Transitions and Transversions
;; [TRAN] 2021/10/13 AC
(require srfi/13)
(require "readfileA.ss")

;; 0:equal 1;tansition 2:transversion
(define (trans-trans c1 c2)
  (if (equal? c1 c2)
      0
      (if (or (and (equal? c1 #\A)(equal? c2 #\G))
	      (and (equal? c1 #\G)(equal? c2 #\A))
	      (and (equal? c1 #\C)(equal? c2 #\T))
	      (and (equal? c1 #\T)(equal? c2 #\C)))
	  1
	  2)))

(define (ros_tran . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_tran.txt"
		    (format "data\\rs_tran~a.txt" (car n)))))
	 (data2 (edit-fasta data))
	 
	 (trans-list     (map (lambda(x y)(trans-trans x y))
			      (string->list (car  data2))
			      (string->list (cadr data2)))))
    
	 (/ (count (lambda(x)(= 1 x))trans-list)
	    (count (lambda(x)(= 2 x))trans-list)
	    1.0
	    )
    
   ))
