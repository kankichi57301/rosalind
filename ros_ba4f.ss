;; rosalind
;; Compute the Score of a Cyclic Peptide Against a Spectrum
;; [BA4F] 2021/08/02 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib2.ss")
(include "monoisotopic.ss")
(define *ba4f_out* "ba4f_out.txt")

(define (ros_ba4f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4f.txt"
		    (format "rs_ba4f~a.txt" (car n)))))
	 (peptide (car data))
	 (spect (map string->number (string-tokenize (cadr data))))
	 
	 )
   
    (solve-ba4f peptide spect)
		
    #|
    (call-with-output-file *ba4f_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

(define (solve-ba4f peptide spect)
  (let ((therical-spect (circular-weight-int peptide)))
    ;(displayln therical-spect)
    ;(displayln spect)
    (displayln (spectrum-score therical-spect spect))))
;;
;; 
;;
(define (spectrum-score spect1 spect2)
  (let ((intersect (set-intersect spect1 spect2))
	(gr-spect1 (group-by identity spect1))
	(gr-spect2 (group-by identity spect2))
	)
    (apply + (map (lambda(x)(min(get-multiplicity gr-spect1 x)
				(get-multiplicity gr-spect2 x)))
		  intersect))))
;;
;;name:get-multiplicity
;;����
;;grouped group-by�֐��ŕ��ނ�������
;;'((1 1 1)(2 2 2 2)(3 3)(4)...)
;;
				 
(define (get-multiplicity grouped item)
  (length (find-first (lambda(gr)(equal? (car gr) item))
		      grouped
		      )))

(define mult-test '((1 1 1)(2 2 2 2)(3 3)(4)(5 5)))
