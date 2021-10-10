#lang racket
;;rosalind
;;
;; Locating Restriction Sites
;; [REVP]
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")

    
(define (make-cart-prod len )
  (filter (lambda(x)(<= (+ (car x)(cadr x)) len))
	  (cartesian-product (iota len)
			     (iota 5 4 2))))


(define (find-rev-palin dna)
  (map (lambda(pair)(cons (+ 1 (car pair))(cdr pair)))
       (filter 
	(lambda(x)
	  (reverse-palin?
	   (substring dna (car x)(+(car x)(cadr x)))))
	(make-cart-prod (string-length dna)))))
	  

;; rosalind
;; 
;;
(require srfi/13)

(define *revp_out* "data\\revp_out.txt")

(define (ros_revp . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_revp.txt"
		    (format "data\\rs_revp~a.txt" (car n)))))
	 (dnas (edit-fasta data))
	 
	 (res '())
	 )

    (set! res (find-rev-palin (car dnas)))

    (define (disp-pair pair out)
      (displayln (format "~a ~a" (car pair)(cadr pair)) out))
    
    (call-with-output-file *revp_out*
      (lambda(out)
	(for-each (lambda(x)
		    (disp-pair x out))
		  res))
      #:exists 'truncate/replace)
    
    res
    
))
