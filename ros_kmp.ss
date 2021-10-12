#lang racket
;; rosalind
;; Speeding Up Motif Finding
;; [KMP] 2021/10/09
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *kmp_out* "data\\kmp_out.txt")
(define *my-hash* #f)

(define (ros_kmp . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_kmp.txt"
		    (format "data\\rs_kmp~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )

    (set! res (fail-array (car dnas)))
    
    (call-with-output-file *kmp_out*
      (lambda(out)
	(for-each (lambda(x)
		    (display (format "~a " x) out))
		  res))
      #:exists 'truncate/replace)
    #t
    ))

(define (fail-array str)
  (set! *my-hash* (make-hash))
  (fail-array0 (string->list str))
  (map (lambda(n)(hash-ref *my-hash* n 0))(iota (string-length str)))
  )

(define (list-match-len0 lis1 lis2 acc)  
  (if (or (empty? lis1)(empty? lis2))
      acc
      (if (equal? (car lis1)(car lis2))
	  (list-match-len0 (cdr lis1) (cdr lis2) (+ 1 acc))
	  acc)))


(define (list-match-len lis1 lis2)
  (list-match-len0 lis1 lis2 0))

(define (hash-set-cont pos n)
  (for-each (lambda(i)(when (not (hash-ref *my-hash* (+ pos i -1) #f))
			    (hash-set! *my-hash* (+ pos i -1) i)))
	    (iota n 1)))
			    
(define (fail-array0 list)
  (fail-array1 (cdr list) list 1))

(define (fail-array1 list org pos)
  (if (empty? list)
      #t
      (let ((match (list-match-len list org)))
	(when (> match 0)
	      (hash-set-cont pos match))
	(fail-array1 (cdr list) org (+ 1 pos)))))

;;--*--
(define kmp1 "ABCXYZABC")
(define kmp2 "CAGCATGGTATCACAGCAGAG")
      
	
