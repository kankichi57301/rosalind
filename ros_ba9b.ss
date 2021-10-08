#lang racket
;; rosalind
;; Implement TrieMatching 
;; [BA9B] 2021/09/12 AC
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "trie.ss")
;;(require "roslibA.ss")
;;(require "roslibB.ss")
(define *ba9b_out* "ba9b_out.txt")
(define my-trie '())

(define (ros_ba9b . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba9b.txt"
		    (format "rs_ba9b~a.txt" (car n)))))
	 (str (car data))
	 (search (cdr data))
	 (trie '())
	 (res '())
	 )
    

    
    (set! trie (make-trie (map string->list search)))
    (set! my-trie trie)
    ;(displayln trie)
    (set! res (prefix-match-trie-all trie (string->list str)))
    res
    
    
    (call-with-output-file *ba9b_out*
      (lambda(out)
	(for-each (lambda(kmer)
		    (display (format "~a " kmer) out))
		  res))      #:exists 'truncate/replace)
    
    
))

(define (prefix-match-trie-all trie charlist)
  (reverse
   (prefix-match-trie-all-0 trie charlist 0 '())))

(define (prefix-match-trie-all-0 trie charlist pos acc)
  (if (null? charlist)
      acc
      (if (prefix-match-trie trie charlist)
	  (prefix-match-trie-all-0 trie (cdr charlist)  (+ 1 pos) (cons pos acc))
	  (prefix-match-trie-all-0 trie (cdr charlist)  (+ 1 pos) acc))))


