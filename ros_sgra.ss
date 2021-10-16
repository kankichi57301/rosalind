#lang racket
;; rosalind
;; Using the Spectrum Graph to Infer Peptides
;;(SGRA)
;; 2021/01/07 AC!
;; 2021/10/15 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(require "monoisotopicA.ss")

(define *data* '())
(define *result* '())

(define (ros_sgra . n)
  (let* ((data (map string->number (read-file*
				    (if (null? n)
					"data\\rosalind_sgra.txt"
					(format "data\\rs_sgra~a.txt" (car n)))))))
    (set! *result* '())
    data
    
    (set! *data* data)
    (solve_sgra data '())
    
    (apply string-append (map symbol->string (max-item *result* length)))
    
   ))


;;WMSPG
(define (solve_sgra numlist acc)

  ;;(displayln acc)
  
  (if (null? numlist)
      (set! *result* (cons (reverse acc) *result*))
      (let* ((fst (car numlist))
	     (candlist (filter (lambda(x)(weight->amino (- (list-ref numlist x) fst)))
			       (range 1 (length numlist)))))
	;;(list fst candlist)
    
	(if (null? candlist)
	    (set! *result* (cons (reverse acc) *result*))
	    (for-each (lambda(x)(solve_sgra (drop numlist x)(cons (weight->amino (- (list-ref numlist x) fst)) acc)))
		      candlist)))))
    

    





