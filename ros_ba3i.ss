;; rosalind
;; Find a k-Universal Circular String 
;; [BA3I] 2021/07/29 AC

(include "ros_ba3f.ss")

(define *ba3i_out* "ba3i_out.txt")


(define (ros_ba3i . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3i.txt"
		    (format "rs_ba3i~a.txt" (car n)))))
	 )
    
    
    (call-with-output-file *ba3i_out*
      (lambda(out)
	(display (solve-ba3i (string->number (car data))) out))
		  
      #:exists 'truncate/replace)
    #t
))

(define (all-binary n)
  (map (lambda(x)(apply string x))
       (cartesian-expt '(#\0 #\1) n)))

(define (make-edge binstr)
  (let ((a0 (format "0~a" binstr))
	(a1 (format "1~a" binstr))
	(b0 (format "~a0" binstr))
	(b1 (format "~a1" binstr)))
    `((,a0 ,b0)
      (,a0 ,b1)
      (,a1 ,b0)
      (,a1 ,b1))))

(define (make-all-edges n)
  (append-map make-edge (all-binary n)))

(define (solve-ba3i n)
  (make-loop
   (find-cycle (make-all-edges (- n 2)))
   n))
  

(define (make-loop eu-cycle n)
  (apply string-append (cons (car eu-cycle)
		       (map (lambda(s)(string-take-right s 1))
			    (drop-right (cdr eu-cycle) (- n 1))))))
