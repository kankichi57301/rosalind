;; rosalind
;; Construct the Graph of a Spectrum
;; [BA11A] 2021/07/04 AC 
;(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(define *ba11a_out* "ba11a_out.txt")

(define amino (map (lambda(pair)
		     
		     (cons (string-ref (symbol->string (car pair)) 0) ;; symbol->char
			   (list(inexact->exact (floor(cadr pair))))))
		   monoiso-mass))
(define r-amino (map reverse amino))

(define (ros_ba11a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11a.txt"
		    (format "rs_ba11a~a.txt" (car n)))))
	 (ndata (cons 0 (map string->number (string-tokenize (car data)))))
	 (res
	  (map (lambda(x)(format "~a->~a:~a" (car x)(cadr x)(cadr (assoc (- (cadr x)(car x)) r-amino))))
	       (filter (lambda(x)(and (< (car x)(cadr x))
				      (assoc (- (cadr x)(car x)) r-amino)))
		       (cartesian-product ndata ndata)))))
	 
    
    (call-with-output-file *ba11a_out*
      (lambda(out)
	 (for-each (lambda(line)
		     (displayln line out))
		   res))
      #:exists 'truncate/replace)
    res
))

;;(define (rassoc-amino
				

