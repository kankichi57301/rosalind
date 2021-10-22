#lang racket
;; rosalind
;; 
;; [FOUN] 
;;
;;2021/04/16 @kankichi57301
;;2021/04/17 AC

(require srfi/1)
(require  math/number-theory)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ros_foun_out* "data\\foun_out.txt")


(define (ros_foun . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_foun.txt"
		    (format "data\\rs_foun~a.txt" (car n)))))
	 (fst (map string->number (string-tokenize (car  data))))
	 (snd (map string->number (string-tokenize (cadr data))))
	 (N (car fst))
	 (m (cadr fst))
	 (A snd)
	 (Ans
	  (map (lambda(j)
		 (map (lambda(i)(calc_foun N i j))
		      A))
	       (iota m 1))))
    (displayln Ans)
    (call-with-output-file *ros_foun_out*
      (lambda(out)
	(for-each (lambda(line)(displaylist line out)) Ans))
      #:exists 'replace)
))

(define (displaylist line out)
  (for-each (lambda(dat)(display (format "~a " dat)out)) line)
  (display "\n" out))


(define (prob1 N p k g)
  (let ((2N (* N 2))
	(q (- 1 p))
       )
    (if (= 1 g)
	(* 1.0
	 (binomial 2N k)
	 (expt q  (- 2N k))
	 (expt p  k))
	(apply + (map (lambda(i)
			(* (prob1 N p i (- g 1))
			   (binomial 2N k)
			   (expt (- 1 (/ i 2N)) (- 2N k))
			   (expt (/ i 2N) k)))
		      (iota 2N)
		 )
	))))
	   
;; NŒÂ‚Ì‚Q”{‘Ì‚Ì‚¤‚¿ö«ˆâ“`‚ªr_aŒÂi0<=r_a<=2Nj‚ ‚é‚Æ‚«‚Ìg¢‘ãŒã‚Ì’†‚Å‘S‚­“–ŠYö«ˆâ“`q‚ğ‚Á‚Ä‚¢‚È‚¢Šm—¦
;;‚Ìí—p‘Î”
(define (calc_foun N r_a g)
  (log10 (prob1 N (/ r_a (* 2 N)) 0 g)))
