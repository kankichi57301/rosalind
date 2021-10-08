#lang racket
;; rosalind
;; Find a Highest-Scoring Peptide in a Proteome against a Spectrum 
;; [BA11E] 2021/08/08
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "monoisotopicA.ss")
(require "roslibA.ss")
(define *spect* '())
(define *ba11e_out* "ba11e_out.txt")

(define (ros_ba11f . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11f.txt"
		    (format "rs_ba11f~a.txt" (car n)))))
	 (spect (map string->number (string-tokenize (car data))))
	 (prot (cadr data))
	 (protein (string->list prot))
	 (protlen (string-length prot))
	 (spclen (length spect))
	 (wk 0)
	 )
    (set! *spect* spect)
    

    ;(displayln spect)
    (displayln protein)

    #t
    #|
    (call-with-output-file *ba11f_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))

;;
;;--*-- rs_ba11f1.txt ‚Æ“¯ˆê
(define sp2 '(0 0 0 4 -2 -3 -1 -7 6 5 3 2 1 9 3 -8 0 3 1 2 1 8))
;;(peptide-score* sp2 '(#\X #\X)  #t)
;;(amino-weight #\C)

;; spect‚Ì’·‚³‚ð’´‚¦‚½‚ç‘Å‚¿Ø‚è
;; protlist‚Íchar‚Ìlist
;;(define (all-prefix-score peaks spectvect)

(define (pept->wt str)
  (map (lambda(a)(cons a (amino-weight a #t))) (string->list str)))
#|
(define (all-prefix-score0 peaks spectvect acc)
  (if (null? peaks)
      '()
|#
