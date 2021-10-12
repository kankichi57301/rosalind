#lang racket
;; rosalind
;; Creating a Distance Matrix
;; [PDST]
(require srfi/13)
(require "readfileA.ss")
(require math/flonum)
 
(define (ros_pdst . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_pdst.txt"
		    (format "data\\rs_pdst~a.txt" (car n)))))
	 (fasta-data (edit-fasta data)))
    (display-matrix (dist-matrix fasta-data))
   ))


(define (p-distance prot1 prot2)
  (fl (/
       (list-diff (string->list prot1)(string->list prot2))
       (string-length prot1))))
;; 
(define (list-diff lst1 lst2)
  (if (or (null? lst1)(null? lst2))
      0
      (+ (if (equal? (car lst1)(car lst2)) 0 1)
	 (list-diff (cdr lst1)(cdr lst2)))))

(define (p-dist1 prot protlist)
  (map (lambda(x) (p-distance prot x))
       protlist))

(define (dist-matrix protlist)
  (map (lambda(x)(p-dist1 x protlist))
       protlist))

(define prl-1 '("ABCDEFGH" "ABCDEFHX" "AXCDEFGH" "PBCDEFGH"))

(define (display-matrix numlistlist)
  (for-each
   (lambda(y) (display (format "~a\n"
			       (apply string-append
				      (map (lambda(x)(format "~a " x)) y)))))
	   numlistlist))
	     
