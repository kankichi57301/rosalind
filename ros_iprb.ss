#lang racket
;; rosalind
;; Mendel's First Law
;; [IPRB] 2021/10/09 @kankichi57301
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *iprb_out* "data\\iprb_out.txt")

(define (ros_iprb . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_iprb.txt"
		    (format "data\\rs_iprb~a.txt" (car n)))))
	 (line (map string->number (string-tokenize (car data))))
	 (k (car line))
	 (m (cadr line))
	 (n (caddr line))
	 (res (solve-iprb k m n))
	 )
    
    
    (call-with-output-file *iprb_out*
      (lambda(out)
		    (display res out))
      #:exists 'truncate/replace)
    ))
    


(define (solve-iprb k m n)
  (let* ((t (+ k m n))
	 (den (* 0.5 t (- t 1)))
	 (num 0)
	 )
    ;(displayln (format "[~a]{~a]" den t))
    (for-each(lambda(i)
	       (set! num (+ num (- t i 1))))
	     (iota k))
    ;(displayln (format "num=~a den=~a [~a] [~a]" num den k t))
    (set! num (+ num (* 0.5 m (- m 1) 0.75)))
    ;(displayln (format "2)[~a]" (* 0.5 m (- m 1) 0.75)))
    (set! num (+ num (* 0.5 m n)))
    ;(displayln (format "3)[~a]" (* 0.5 m n)))
    (/ num den)))
