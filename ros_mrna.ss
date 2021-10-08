#lang racket
;; rosalind
;; Inferring mRNA from Protein 
;; [MRNA] 2021/10/07
;(require srfi/1)
(require srfi/13)

(require "roslibA.ss")

(define aminos
'(
  #\F #\L #\I #\V 
  #\F #\L #\I #\V 
  #\L #\L #\I #\V 
  #\L #\L #\M #\V 
  #\S #\P #\T #\A 
  #\S #\P #\T #\A 
  #\S #\P #\T #\A 
  #\S #\P #\T #\A 
  #\Y #\H #\N #\D 
  #\Y #\H #\N #\D 
  #\@ #\Q #\K #\E 
  #\@ #\Q #\K #\E 
  #\C #\R #\S #\G 
  #\C #\R #\S #\G 
  #\@ #\R #\R #\G 
  #\W #\R #\R #\G ))

(define hindo (make-hash))

(for-each (lambda(a)(inc-hash! hindo a)) aminos)

(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *mrna_out* "mrna_out.txt")

(define (ros_mrna . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_mrna.txt"
		    (format "rs_mrna~a.txt" (car n)))))
	 )
    (modulo
     (* 3 (apply * (map(lambda(x)(hash-ref hindo x))(string->list (car data))))))
    #|
    (call-with-output-file *mrna_out*
      (lambda(out)
	(display (format res out)))
		  
      #:exists 'truncate/replace)
    |#
    
))



