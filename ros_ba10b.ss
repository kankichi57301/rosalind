;; rosalind
;; 
;; Compute the Probability of an Outcome Given a Hidden Path 
;; [BA10B]
(require srfi/1)
(require srfi/13)
(include "readfile.ss")

(define *ros_ba10b* "rosalind_ba10b.txt")
(define *ros_ba10b0* "rs62.txt")
(define *ros_ba10b1* "rs621.txt")


(define fl-pat #rx"[0-9]*[.][0-9]+")


(define (ros_ba10b)
  (let* ((data (read-file "_ba10b"))
         (seq1 (string->list (car data)))
	 (seq2 (string->list (fifth data)))
         (probA
	  (map string->number
	       (regexp-match* fl-pat (list-ref data 9))))
	 (probB
	  (map string->number
	       (regexp-match* fl-pat (list-ref data 10))))
	 )
    (apply * 
	   (map (lambda(x y) (prob-2 x y probA probB))
		seq1 seq2))
    
  ))


(define (prob-2 arg1 arg2 probA probB)
  (list-ref
   (if (equal? arg2 #\A)
       probA
       probB)
   (case arg1
     [(#\x) 0]
     [(#\y) 1]
     [(#\z) 2])))
