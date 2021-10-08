;;#lang racket
;; rosalind
;;
;;  Convert a Peptide Vector into a Peptide
;; [BA11D] 2012/07/05 AC
(require srfi/1)
(require srfi/13)
(require srfi/48)

(include "readfile.ss")
(include "roslib.ss")
(include "monoisotopic.ss")
(define *ba11d_out* "ba11d_out.txt")
(define *result* #f)

(define *data* '())
(define amino-mass (append '((X 4)(Z 5)) monoiso-mass))
(define amino (map (lambda(pair)
		     
		     (cons (string-ref (symbol->string (car pair)) 0)
			   (list(inexact->exact (floor(cadr pair))))))
		   amino-mass))
(define r-amino (map reverse amino))


(define (ros_ba11d . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11d.txt"
		    (format "rs_ba11d~a.txt" (car n)))))
	 (result (apply string
			(map (lambda(str)(cadr (assoc (+ 1 (string-length str)) r-amino)))
			     (string-tokenize
			      (regexp-replace* #rx"1"
					       (regexp-replace* #rx" " (car data) "")
					       " "))))))
			    
    (call-with-output-file *ba11d_out*
      (lambda(out)
	(display result out))
      #:exists 'truncate/replace)
    result
    ))

