#lang racket
;; rosalind
;; Consensus and Profile 
;; [CONS] 20**/**/**
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")
(define *cons_out* "data\\cons_out.txt")

(define (ros_cons . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_cons.txt"
		    (format "data\\rs_cons~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res1 (map count-nuc (transpose (map string->list dnas))))
	 (ans1 (apply string (map consensus-enki res1)))
	 (ans2 (map (lambda(x y)(format "~a:~a" x (seisho-nlist y))) all-enki (transpose res1)))
	 )

    
    (call-with-output-file *cons_out*
      (lambda(out)
	(displayln ans1 out)
	(for-each
	 (lambda(x)(displayln x out))
	 ans2))
      #:exists 'truncate/replace)
    ans1
    
))

(define (count-nuc charlist)
  (map (lambda(x)(count (lambda(n)(equal? n x)) charlist)) all-enki))
;;;
;;; (1 2 3 4) => #\T (4 1 2 3) => #\A
;;;
(define (consensus-enki enki-count)
  (list-ref all-enki (max-index-of enki-count identity)))

(define (seisho-nlist nlist)
  (string-join (map number->string nlist) " "))
