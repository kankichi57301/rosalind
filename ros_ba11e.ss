;; rosalind
;; => C langx
;; [BA11E] 20**/**/**
(require srfi/1)
(require srfi/13)
(include "readfile.ss")
(include "monoisotopic.ss")
(define *nlist* '())
(define *ba11e_out* "ba11e_out.txt")
(define *dp* #f)
(define *from* #f)

(define (ros_ba11e . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba11e.txt"
		    (format "rs_ba11e~a.txt" (car n)))))
	 (nlist (map string->number (string-tokenize (car data)))) 
	 )
    (set! *nlist* nlist)
    (length nlist)
    (set! *dp* (make-hash))
    (set! *from* (make-hash))
    
    #|
    (call-with-output-file *ba11e_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))
      #:exists 'truncate/replace)
    |#
    
))
#|
(define (solve-ba11e nlist)
  (hash-set!  *dp* 0 0)
  (for-each (lambda(x)(
|#


;;--*-- test --*--
;;"GGPGGPGGAGG"
(define (spect-score nlist peptide)
  (spect-score0 (cons 0 nlist) (string->list peptide) 0))

(define (spect-score0 nlist peptidelist acc)
  (displayln (format "pept=~a acc=~a nlist=~a" peptidelist acc nlist))
  (if (null? peptidelist)
      acc
      (let ((len (cdr (assoc (car peptidelist) monoiso-mass-int-dummy))))
	(displayln (format "len=~a" len))
	(if (<= (length nlist) len)
	    acc
	    (let ((next (drop nlist len)))
	      (spect-score0 next (cdr peptidelist)(+ acc (car nlist))))))))

;;
(define spect01 '(0 0 0 4 -2 -3 -1 -7 6 5 3 2 1 9 3 -8 0 3 1 2 1 0))
(define pept01 "XZZXX")
;; (spect-score spect01 pept01 )
