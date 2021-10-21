#lang racket
;; rosalind
;; Inferring Genotype from a Pedigree
;; [MEND]
;; 2021/01/28 AC
;; 2021/10/21 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibB.ss")


(define (ros_mend . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_mend.txt"
		    (format "data\\rs_mend~a.txt" (car n))))))
    (for-each (lambda(x)(display (format "~a " x)))
	      (evaluate-newick-pedigreee (newick->sexpr (car data))))
  ))

(define (calc-mend triple1 triple2)
  (let ((p11 (car   triple1))
	(p01 (cadr  triple1))
	(p00 (caddr triple1))
	(q11 (car   triple2))
	(q01 (cadr  triple2))
	(q00 (caddr triple2)))
    (list (+ (* p11 q11)
	     (* 0.5 p11 q01)
	     (* 0.5 p01 q11)
	     (* 0.25 p01 q01))
	  (+ (* p00 q11)
	     (* p11 q00)
	     (* 0.5 p11 q01)
	     (* 0.5 p01 q11)
	     (* 0.5 p00 q01)
	     (* 0.5 p01 q00)
	     (* 0.5 p01 q01))
	  (+ (* p00 q00)
	     (* 0.5 p00 q01)
	     (* 0.5 p01 q00)
	     (* 0.25 p01 q01))
    )))

(define (evaluate-newick-pedigreee0 expr)
  ;;(displayln (format "arg=~a" expr))
  (if (symbol? expr)
      (case expr
	[(AA)    '(1.0 0 0)]
	[(Aa aA) '(0 1.0 0)]
	[(aa)    '(0 0 1.0)]
	)
      (if (every number? expr)
	  expr
	  (calc-mend (evaluate-newick-pedigreee0 (car expr))
		     (evaluate-newick-pedigreee0 (cadr expr))))))

;; top level only
(define (evaluate-newick-pedigreee expr)
  (if (= 3 (length expr))
      (evaluate-newick-pedigreee0 (list (evaluate-newick-pedigreee0 (take expr 2))(cadr expr)))
      (evaluate-newick-pedigreee0 expr)
      ))
