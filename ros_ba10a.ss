;; rosalind
;; Compute the Probability of a Hidden Path 
;; [BA10A]
(require srfi/13)
(include "readfile.ss")

(define *ros_ba10a* "rosalind_ba10a.txt")
(define *ros_ba10a0* "rs100.txt")
(define *ros_ba10a2* "rs1002.txt")

(define state '())
(define nstat 0)
(define fl-pat #rx"[0-9]*[.][0-9]+")
(define matrix '())


(define (ros_ba10a)
  (let* ((data (read-file "_ba10a"))
	 (seq (string->list (car data)))
	 )
    

    (set! state (map (lambda(x)(string-ref x 0))
		     (regexp-match* #rx"[A-Za-z]" (third data))))
    (set! nstat (length state))
    (set! matrix (map (lambda(x)(map string->number (regexp-match* fl-pat x)))(drop data 5)))
		
    
    (/ (prob-path seq) 2)
    ))

(define (prob-path seq)
  (if (= 1 (length seq))
      1.0
      (* (list-ref (list-ref matrix (index-of state (car seq)))
		   (index-of state (cadr seq)))
	 (prob-path (cdr seq)))))
