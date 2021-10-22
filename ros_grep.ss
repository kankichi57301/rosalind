#lang racket
;; rosalind
;;Genome Assembly with Perfect Coverage and Repeats
;; [GREP]
;; 2012/10/21 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require (only-in "mktestA.ss" circle2kmers))
(require  "roslibA.ss" )

(define *grep_out* "data\\grep_out.txt")
(define *k*   3)
(define *k-1* 2)

(define (ros_grep . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_grep.txt"
		    (format "data\\rs_grep~a.txt" (car n)))))
	 (kmerslist (map string->list data))
	 (res '())
	 )
    (set! *k* (string-length (car data)))
    (set! *k-1* (- *k* 1))
    (set! res (solve-grep kmerslist))
    
    (call-with-output-file *grep_out*
      (lambda(out)
	(for-each
	 (lambda(x)
	   (displayln (format "~a" x) out))
	 res))
      #:exists 'truncate/replace)
    res
    ))

(define (kmer-connect? list1 list2)
  (equal? (take-right list1 *k-1*)(take list2 *k-1*)))

(define *res* #f)
(define (solve-grep kmerslist)
  (set! *res* (make-hash))
  (solve-grep0 (cdr kmerslist)(car kmerslist) )
  (hash-map *res* (lambda(x y) (apply string x)))
  )


(define (solve-grep0 kmerslist acc)
  ;(displayln acc)
  (if (null? kmerslist)
      (when (equal? (take acc *k-1*)(take-right acc *k-1*))
	    (hash-set! *res* (drop-right acc *k-1*) #t))
      (let ((next (delete-duplicates(filter (lambda(kmer)(kmer-connect? acc kmer))
					   kmerslist))))
	(for-each (lambda(kmer)(solve-grep0 (delete-once kmer kmerslist)(append acc (take-right kmer 1))))
		  next))))


