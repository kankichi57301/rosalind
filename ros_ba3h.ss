;; rosalind
;; Reconstruct a String from its k-mer Composition
;; [BA3J] 2021/07/30 AC
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "ros_ba3g.ss")
(define *ba3h_out* "ba3h_out.txt")

(define (ros_ba3h . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba3h.txt"
		    (format "rs_ba3h~a.txt" (car n)))))
	 (k (string->number (car data)))
	 (edges (cdr data))
	 (adj-list (map str2edge edges))
	 (res '())
	 )
    (set! res (con-all-kmer (apply find-path (cons adj-list (calc-start-end adj-list)))))
    
    
    (call-with-output-file *ba3h_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
    #t
))

(define (str2edge str)
  (list (string-drop-right str 1)
	(string-drop str 1)))


(define (con-all-kmer strlist)
  (apply string-append (cons (car strlist)
		       (map (lambda(x)(string-take-right x 1))(cdr strlist)))))
