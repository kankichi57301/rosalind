#lang racket
;;rosalind
;;Finding a Protein Motif
;;[MPRT]


(require net/url)
(require srfi/13)
(require srfi/1)
(require "readfileA.ss")

(define (make-uniprot prot-name)
  (format
    "https://www.uniprot.org/uniprot/~a.fasta"
   prot-name))

(define (make-uniprot2 prot-name)
  (format
    "https://www.uniprot.org/uniprot/~a.txt"
   prot-name))

(define (read-whole in)
  (reverse (read-whole0 in '())))

(define (read-whole0 in acc)  
  (let ((dat (read-line in)))
    (if (eof-object? dat)
	acc
	(read-whole0 in (cons dat acc)))))
  
(define (read-uniprot name)

    (call/input-url (string->url (make-uniprot name))
		    (lambda(x) (get-pure-port x #:redirections 1))
                    (lambda (in)
		      (read-whole in)
		    )))

(define (load-prot protname)
  (let ((content (read-uniprot protname)))
    (if (empty? content)
	""
	(apply string-append (cdr content)))))

(define (find-motif protain regexp len)
  (if (< (string-length protain) len)
      '()
      (map add1
	   (filter (lambda(x)
		     (regexp-match regexp
				   (substring protain x (+ x len))))
		   (iota (- (string-length protain) len -1))))))

(define (solve-mprt fasta-file out)
  (let* ((prot (load-prot fasta-file))
         (numlist (find-motif prot #rx"N[^P][ST][^P]" 4)))
    (when  (not (null? numlist))
	   (begin
	     (displayln fasta-file out)
	     (map (lambda(x) (display (format "~a " x) out)) numlist)
	     (display "\n" out)
	     ))))

(define *mprt_out* "data\\mprt_out.txt")

(define (ros_mprt . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_mprt.txt"
		    (format "data\\rs_mprt~a.txt" (car n)))))
	 )
    (call-with-output-file *mprt_out*
      (lambda(out)
	(for-each (lambda(x)(solve-mprt x out))data))
      #:exists 'truncate/replace)
    #t
))


