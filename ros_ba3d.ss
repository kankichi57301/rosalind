;; rosalind
;; Construct the De Bruijn Graph of a String
;; [BA3D] 2021/07/18 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include roslib.ss")
(define *ba3d_out* "ba3d_out.txt")
(define myhash '())

(define (ros_ba3d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3d.txt"
		    (format "rs_ba3d~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (res '())
	)
    (set! myhash (make-hash))
    (solve-ba3d  (cadr data) k)
    (set! res (map bruijn-format (output)))
    
    (call-with-output-file *ba3d_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (displayln (format "~a" kmer) out))
		  res))
      #:exists 'truncate/replace)
    
    res
))

(define (solve-ba3d str k)
  (if (< (string-length str) k)
      #t
      (begin 
	(hash-append! myhash (string-take str (- k 1))(string-take (string-drop str 1)(- k 1)))
	(solve-ba3d (string-drop str 1) k))))


(define (output)
  (sort (hash-map myhash list)
	(lambda(x y)(string< (car x)(car y)))))

(define (bruijn-format pair)
  (format "~a -> ~a"
	  (car pair)
	  (string-join (cadr pair) ",")))


		       
		       
