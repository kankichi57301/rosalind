#lang racket
;; rosalind
;; Generate the Last-to-First Mapping of a String
;; [BA9K] 2021/09/14 AC
;; 2021/12/30 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "bwtA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")

(define *ba9k_out* "data\\ba9k_out.txt")

(define dna-alist '((#\$ 0)
		    (#\A 1)
		    (#\C 2)
		    (#\G 3)
		    (#\T 4)))

(define (ros_ba9k . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba9k.txt"
		    (format "data\\rs_ba9k~a.txt" (car n)))))
	 (str (car data))
	 (strlist (string->list (car data)))
	 (nlist (map (lambda(x)(cadr (assoc x dna-alist))) strlist))
	 (i   (string->number (cadr data)))
	 (poslist (find-pos nlist i))
	 )
;;(displayln (format "i=~a" i))
;;(displayln (format "pos=~a" poslist))
    (find-nth-pos (sort nlist <)(car poslist)(cadr poslist))
    #|
    (call-with-output-file *ba9k_out*
      (lambda(out)
	(displayln (rev-bwt (car data)) out))
      #:exists 'truncate/replace)
    |#
    
    ))




