;; rosalind
;; Compute the Number of Peptides of Given Total Mass
;; [BA4D] 2021/07/21 AC
(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(include "roslib.ss")
(define myhash #f)
(define *ba4d_out* "ba4d_out.txt")

(define (ros_ba4d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4d.txt"
		    (format "rs_ba4d~a.txt" (car n)))))
	 (res 0)
	 )
    
    (set! res (solve-ba4d (string->number (car data))))
    
    (call-with-output-file *ba4d_out*
      (lambda(out)
	(display (format "~a " res) out))
		  
      #:exists 'truncate/replace)
    
    res
))

(define (solve-ba4d n)
  (set! myhash (make-hash))
  (hash-set! myhash n 1)
  (for-each (lambda(i)
		  (let (( c (hash-ref myhash i 0)))
		    (when (> c 0)
			  (for-each (lambda(x)(inc-hash2! myhash (- i x) c))
				    all-amino-weights))))
	    (iota (+ 1 n) n -1))
  (hash-ref myhash 0))
	    

  

