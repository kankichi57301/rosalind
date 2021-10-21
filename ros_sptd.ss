#lang racket
;; rosalind
;; split distance
;; [SPTD] 2021/04/04 AC
;; 2021/10/21 AC
(require srfi/1)
(require srfi/13)
(require srfi/19)
(require mzlib/string)
;(require racket/set)
(require "readfileA.ss")
(require "roslibA.ss")

(define *myhash1* #f)
(define *myhash2* #f)
(define *tree1* #f)

(define (ros_sptd . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_sptd.txt"
		    (format "data\\rs_sptd~a.txt" (car n)))))
	 (tree1 (newick->sexp (cadr data )))
	 (tree2 (newick->sexp (caddr data)))
	 (split1 '())
	 (split2 '())
	 (all-taxa (read-from-string (format "(~a)" (car data))))
	 (ntaxa (length all-taxa))
	 (ntree1 (conv2num tree1 all-taxa))
	 (ntree2 (conv2num tree2 all-taxa))
    	 )
    (set! *myhash1* (make-hash))
    (set! *myhash2* (make-hash))
    (all-nontrivial-split ntree1 *myhash1* (iota ntaxa 1))
    (all-nontrivial-split ntree2 *myhash2* (iota ntaxa 1))
    
    (set! split1 (hash-keys *myhash1*))
    (set! split2 (hash-keys *myhash2*))
    
    ;(displayln tree1 )
    ;(displayln tree2 )

    (displayln
     (+
      (length
       (filter (lambda(x)(not (member x split2 split-eq?))) split1))
      (length
       (filter (lambda(x)(not (member x split1 split-eq?))) split2))
      ))
    
))



(define (split-eq? bintree1 bintree2)
  (equal? bintree1 bintree2)) 
	 
	 
;; --*-- top level only --*--
(define (all-nontrivial-split tree hash all)
  ;(displayln (format ":arg=~a" tree))
  (for-each (lambda(x)(all-inner2 x hash all)) tree))

;;‘S‚Ä‚Ìnontrivial-split‚ğhash‚É“o˜^‚·‚éB
(define (all-inner2 tree hash all)
  (when (list? tree)
	;(displayln (format "arg=~a" tree))
	(let* ((split (sort (flatten tree) <))
	       (len (length split)))
	  (when (> len 1)
		;(displayln (format "arg=~a" split))
		(hash-set! hash
			   ;; split‚Ì•ĞŠ„‚ê‚ª‘S’·‚Ì”¼•ª‚ğ’´‚¦‚½‚çcS‚Ì‚Ù‚¤‚ğ“o˜^‚·‚é
			   ;; ‚¿‚å‚¤‚Ç”¼•ª‚Ì’·‚³‚Å‚P‚ğŠÜ‚Ü‚È‚¢‚Æ‚«‚àcS‚Ì‚Ù‚¤‚ğ“o˜^‚·‚é			   
			   (if (or (> len (/ (length all) 2))
				   (and (= len (/ (length all) 2))
					(not (= 1 (car split)))))
			       (set-complement split all)
			       split)
		 #t)
	(all-inner2 (car tree)  hash all)
	(all-inner2 (cadr tree) hash all)))))

(define (conv2num tree taxalist)
  (if (null? tree)
      '()
      (if (symbol? tree)
	  (+ 1 (index-of taxalist tree))
	  (cons (conv2num (car tree) taxalist)
		(conv2num (cdr tree) taxalist)))))




