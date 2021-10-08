;; rosalind
;; Implement DecodingIdealSpectrum
;; [BA11B] 2021/07/25 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(include "roslib2.ss")
(include "monoisotopic.ss")
(define *ba11b_out* "ba11b_out.txt")

(define (ros_ba11b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11b.txt"
		    (format "rs_ba11b~a.txt" (car n)))))
	 (spect (map string->number (string-tokenize (car data))))
	 )
    (solve-ba11b spect)
    #|
    (call-with-output-file *ba11b_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
))


(define (solve-ba11b nlist)
  (apply string
	 (map weight-int->amino
	      (solve-ba11b0 (cdr nlist) (list (car nlist)) '() (/ (+ 1 (length nlist)) 2)))))


(define (solve-ba11b0 nlist left right len)
  ;(displayln (format "arg=~s L=~a R=~a len=~a" nlist left right len))
  (let* ((next (car nlist))
	 (left-all (apply + left))
	 (right-all(apply + right)))

    (if (= 1 len)
	(append (reverse left)  right)
	(let ((d-left (- next left-all)))
	  ;(displayln (format "d-l=~a" d-left))
	  (or
	   (if (member d-left all-amino-weights)
	       (solve-ba11b0 (cdr nlist)(cons d-left left) right (- len 1))
	       #f
	       )
	   (let ((d-right (- next right-all)))
	     ;(displayln (format "d-r=~a" d-left))
	     (if (member d-right all-amino-weights)
		 (solve-ba11b0 (cdr nlist) left (cons d-right right) (- len 1))
		 #f)))))))
      
     
			 

;;--*--
(define w1 (all-prefix-suffix-weight-int "ACDEF"))
;;
