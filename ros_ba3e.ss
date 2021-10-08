;; rosalind
;; Construct the De Bruijn Graph of a Collection of k-mers
;; [BA3E] 2021/07/18 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(define *ba3e_out* "ba3e_out.txt")
(define myhash '())

(define (ros_ba3e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3e.txt"
		    (format "rs_ba3e~a.txt" (car n)))))
	 (res '())
	 )
    
    (set! myhash (make-hash))
    (solve-ba3e  data)
    (set! res (map bruijn-format (output)))
 
    
    (call-with-output-file *ba3e_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln (format "~a" kmer) out))
		  res))
      #:exists 'truncate/replace)
    
    res
    
))

(define (solve-ba3e strs)
  (for-each (lambda(str)
	      (hash-append! myhash (string-drop-right str 1)(string-drop  str 1)))
	    strs))
	      
(define (hash-append! hash key item)
  (hash-set! hash key (append (hash-ref hash key '()) (list item))))

(define (output)
  (sort (hash-map myhash list)
	(lambda(x y)(string< (car x)(car y)))))

(define (bruijn-format pair)
  (format "~a -> ~a"
	  (car pair)
	  (string-join (cadr pair) ",")))


		       
		       
