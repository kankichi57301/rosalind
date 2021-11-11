(module rosalind racket/base
	(provide (all-defined-out))
	(require srfi/1)
	(require srfi/13)
	;;filename:bwt.ss
;;Burrows-Wheeler Transform
;;

(define (all-rot str)
  (map (lambda(n)(rot-str str n))(iota (string-length str))))

(define (rot-str str n)
  (string-append (string-drop str n)(string-take str n))) 

(define (bwt str)
  (apply string-append
	 (map (lambda(s)(string-take-right s 1))
	      (sort (all-rot str) string<?))))

(define (str-paste strlist1 strlist2)
	  (map string-append strlist1 strlist2))

(define (rev-bwt bwt)
  (let ((bwtlist (map string (string->list bwt))))
    (car
     (filter (lambda(s)(string=? (string-take-right s 1) "$"))
	     (rev-bwt0 bwtlist bwtlist (- (string-length bwt) 1))))))

(define (rev-bwt0 from bwt n)
  ;(displayln (format "rev-bwt arg=~a" from))
  (if (= n 0)
      from
      (rev-bwt0 (str-paste bwt (sort from string<?)) bwt (- n 1))))

(define (bwt* str)
  (let ((strs (sort (all-rot str) string<?)))
    (for-each (lambda(x)(displayln (format "~a\x1b[42m~a\x1b[0m"
					   (string-drop-right x 1)
					   (string-take-right x 1))))
	      strs)
    (apply string-append
	   (map (lambda(s)(string-take-right s 1))
		strs))))
)
