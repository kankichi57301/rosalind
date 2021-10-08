;; rosalind
;; Implement DecodingIdealSpectrum
;; [BA11J] 2021/07/
;(require srfi/1)
(require srfi/13)


(include "readfile.ss")
(include "roslib.ss")
(include "roslib2.ss")
(include "monoisotopic.ss")

(define *ba11j_out* "ba11j_out.txt")

(define mass-table (map (lambda(x)(list (inexact->exact (floor (cadr x)))(symbol->string (car x))))
			monoiso-mass))

(define (ros_ba11j . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11j.txt"
		    (format "rs_ba11j~a.txt" (car n)))))	
	 	 (spectrum (map string->number (string-tokenize (car data))))

	 )
    (solve-ba11j spectrum)
    #|
    (call-with-output-file *ba11j_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#

    
    ))

(define (list->mass strlist)
  (if (null? strlist)
      0
      (apply + (map (lambda(y)(car (find-first (lambda(c)(string=? y (cadr c))) mass-table)))
		    strlist))))


(define (solve-ba11j spect)
  (solve0 spect '() '() 0)
  )

(define (solve0 spect left right flag)
  (if (null? spect)
      (if (= 1 flag)
	  left
	  right)
      (begin
	(displayln (format "l=~a r=~a w=~a" left right (car spect)))
	(let* ((wd1 (- (car spect)(list->mass left)))
	       (it (find-first (lambda(x)(= (car x) wd1)) mass-table)))
	  (displayln (format "wd1=~a" wd1))
	  (if it
	      (solve0 (cdr spect)(cons (cadr it) left) right 1)
	      (let* ((wd2 (- (car spect)(list->mass right)))
		     (it2 (find-first (lambda(x)(= (car x) wd2)) mass-table)))
		(displayln (format "wd2=~a" wd2))
		(if it2
		    (solve0 (cdr spect) left (cons (cadr it2) right) 2)
		    #f)))
	  
))))
	
;;
;;--*-- test
;;
(define (make-spect nlist)
  (sort
   (append (cdr (prefix-sum nlist))
	   (prefix-sum (reverse nlist)))
   <))


(define (prefix-sum nlist)
  (prefix-sum0 (cdr nlist)(list (car nlist)))) 


(define (prefix-sum0 nlist acc)
  (if (null? nlist)
      acc
      (prefix-sum0 (cdr nlist)(cons (+ (car nlist)(car acc)) acc))))



(define (str->spect str)
  (make-spect
   (map (lambda(x)(car (find-first (lambda(s)(string=? (string x) (cadr s))) mass-table)))
	(string->list str))))
