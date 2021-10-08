;; rosalind
;; Find a Shortest Transformation of One Genome into Another by 2-Breaks
;; [BA6B] 2021/07/24 AC
;(require srfi/1)
(require srfi/13)

(include "readfile.ss")
(define *ba6b_out* "ba6b_out1.txt")


(define (ros_ba6b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba6b.txt"
		    (format "rs_ba6b~a.txt" (car n)))))
	 (expr (call-with-input-string (car data)
				       (lambda(in)(read in))))
	 (res 0)
       )

    (bp expr)
    #|
    (call-with-output-file *ba6b_out*
      (lambda(out)
	(solve-ba6a expr out))
      #:exists 'truncate/replace)
    |#
   
    
    ))

	 
				
;;(define td '(+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14))

(define (bp nlist)
  (bp1 nlist 0))

(define (bp1 nlist prev)
  (if (null? nlist)
      0
      (let ((next  (car nlist)))
	(+ (if (=  1 (-  next prev))
	       0 1)
	   (bp1 (cdr nlist) next)))))
		    
