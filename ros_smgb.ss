#lang racket
;; rosalind
;; Semiglobal Alignment
;; [SMGB] 2021/03/12 AC
;; 2021/10/22 AC
(require srfi/13)
(require srfi/19)

(require "readfileA.ss")
(require "align-semiglobalA.ss")

(define *smgb-out*  "data\\rs_smgb_out.txt")
(define *dnas* '())
(define *time* #f)

(define (ros_smgb . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_smgb.txt"
		    (format "data\\rs_smgb~a.txt" (car n)))))
	 (dnas (edit-fasta data)))
    (set! *dnas* dnas)
    (set! *time* (current-time))
    (call-with-output-file *smgb-out*
      (lambda(out)
	(for-each (lambda(x)(displayln (format "~a" x) out))
		  (apply semiglobal-align dnas)))
      #:exists 'truncate/replace)
    (displayln (format "elapsed =~a sec" (time-second (time-difference (current-time) *time*))))

  ))

;;;--*-- tbc 
(define (check_smgb)
  (let ((aligned (read-file* *smgb-out*)))
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

