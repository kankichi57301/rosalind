;; rosalind
;; Trim a Peptide Leaderboard 
;; [BA4L] 2021/08/03 AC
;(require srfi/1)
(require srfi/13)
(include "monoisotopic.ss")
(include "readfile.ss")
(include "roslib2.ss")
(define *ba4l_out* "ba4l_out.txt")

(define (ros_ba4l . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba4l.txt"
		    (format "rs_ba4l~a.txt" (car n)))))
	 (peptides (string-tokenize (car data)))
	 (spect (map string->number (string-tokenize (cadr data))))
	 (N (string->number (caddr data)))
	 )

    
    (call-with-output-file *ba4l_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		      (solve-ba4l peptides spect N)))
      #:exists 'truncate/replace)
    #t
))

(define (solve-ba4l peptides spect n)
  (let* ((scores
	 (map (lambda(x)(list x (linear-score (theorical-weight-int x) spect)))
	      peptides))
	 (nth-score (list-ref (sort (map cadr scores) >)(- n 1)))
	)
    (map car (filter (lambda(pair)(>= (cadr pair) nth-score)) scores))
    ))
