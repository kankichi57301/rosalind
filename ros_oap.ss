#lang racket
;; rosalind
;; overlap Alignment
;; [OAP] 2021/03/13 AC
;; 2021/10/22 AC
(require srfi/13)
(require srfi/19)

(require "readfileA.ss")
(require (only-in "align-overlapA.ss" overlap-align mismatch-score))

(define *oap-out*  "data\\oap_out.txt")
(define *dnas* '())
(define *time* #f)

(define (ros_oap . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_oap.txt"
		    (format "data\\rs_oap~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 (res '())
	 )
    (set! *dnas* dnas)
    (set! *time* (current-time))
    (set! res (apply overlap-align dnas))
    (call-with-output-file *oap-out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a" x) out))
		  res))
      #:exists 'truncate/replace)
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))
    res
  ))
;;;--*-- 
(define (check_oap)
  (let ((aligned (read-file* *oap-out*)))
    (list (car aligned)
	  (+
	   (apply mismatch-score (cdr aligned))
	   (count-prefix-gap (cadr  aligned))
	   (count-suffix-gap (cadr  aligned))
	   (count-prefix-gap (caddr aligned))
	   (count-suffix-gap (caddr aligned))
	   )
	  (string= (car *dnas*) (string-delete #\- (cadr  aligned)))
	  (string= (cadr *dnas*)(string-delete #\- (caddr aligned)))
	   
     )))

(define (count-prefix-gap str)
  (if (equal? "-" (string-take str 1))
      (+ 1 (count-prefix-gap (string-drop str 1)))
      0 ))

(define (count-suffix-gap str)
  (if (equal? "-" (string-take-right str 1))
      (+ 1 (count-suffix-gap (string-drop-right str 1)))
      0 ))

