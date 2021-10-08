;; rosalind
;; Find All Approximate Occurrences of a Pattern in a String
;; [BA1H] 2021/07/08 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "roslib.ss")
(define *ba1h_out* "ba1h_out.txt")
(define res '())

(define (ros_ba1h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1h.txt"
		    (format "rs_ba1h~a.txt" (car n)))))
	 (pat (car data))
	 (str (cadr data))
	 (d (string->number(caddr data)))
	 (slen (string-length str))
	 (len (string-length pat))
	 )
    (set! res '())

    
    (define (solve0 pat str d  pos slen)
      (if (< slen len)
	  #f
	  (begin
	    (when (<= (hamming-distance pat (string-take str len)) d)
		  (set! res (cons pos res)))
	    (solve0 pat (string-drop str 1) d (+ 1 pos)(- slen 1)))))
    	    
 
    (define (solve pat str d)
      (solve0 pat str d 0 slen))
      
    (solve pat str d)


    (call-with-output-file *ba1h_out*
      (lambda(out)
	(for-each (lambda(n)
		    (display (format "~a " n) out))
		  (reverse res)))
      #:exists 'truncate/replace)
    #t
))

	 
				

