#lang racket
;; rosalind
;; Comparing Spectra with the Spectral Convolution
;; [CONV]
;; 2012/10/14 AC
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *conv_out* "data\\conv_out.txt")

(define (ros_conv . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_conv.txt"
		    (format "data\\rs_conv~a.txt" (car n)))))
	 
	 (s1  (map string->number (regexp-match* fl-pat (car  data))))
	 (s2  (map string->number (regexp-match* fl-pat (cadr data))))
	 (res
	  (max-item
	   (devide-group 
	    (mink-minus s1 s2)
	    (lambda(x y)(<= (abs (- x y)) 0.01)))
	   length)))
	     
    (displayln (length res))
    (displayln (format "~s" (/ (round (* (car res) 100000)) 100000)))

    (call-with-output-file *conv_out*
      (lambda(out)
	(displayln (length res) out)
	(displayln (format "~s" (/ (round (* (car res) 100000)) 100000)) out))
            #:exists 'truncate/replace)
    ))




(define (mink-minus s1 s2)
  (map (lambda(x)(abs (- (car x)(cadr x))))
       (cartesian-product s1 s2)))
