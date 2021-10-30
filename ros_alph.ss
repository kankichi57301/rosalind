#lang racket
;; rosalind
;; Alignment-Based Phylogeny
;; [ALPH] 2021/10/30 AC 
(require srfi/1)
(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
;;(require "roslibB.ss")

(define *tree* '())
(define *leaves-alist* '())
(define *alph_out* "data\\alph_out.txt")
(define *result* #f)
(define *mytree* '())
(define *dnalen* 0)

(define (ros_alph . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_alph.txt"
		    (format "data\\rs_alph~a.txt" (car n)))))
	 (tree (newick->sexp (format "(~a)" (car data))))
	 (head (filter (lambda(x)(header? x))(cdr data)))
	 (dnas (edit-fasta (cdr data)))
	 (dnalen (string-length (car dnas)))
	 (leaves-alist (zip (map (lambda(s)(string->symbol (string-drop s 1)))head) dnas))
	 (res "")
	 )
    (set! *dnalen* dnalen)
    (set! *tree* (all-reverse tree))     ;; newickをパーレンで囲んでカンマを除去してS式として読める形にしてから全レベルでreverseしたもの
    (set! *leaves-alist* leaves-alist)   ;;　葉
    (displayln (format "total nodes=~a" (length (flatten tree))))
    (displayln (format "leaves=~a"      (length leaves-alist)))
    (displayln (format "dna len=~a"      dnalen))
    
    ;(displayln *tree*)
    ;(displayln leaves-alist)

    (set! res
     (call-with-output-string 
      (lambda(out)
	(exec_alph out))
    ))
    
    (call-with-output-file *alph_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)

    ;(display res)
    #t
))

(define (all-reverse bintree)
  (if (symbol? bintree)
      bintree
      (reverse (map all-reverse bintree))))
;;
;; 4つ組 (当該サブツリーの根をAにしたときのサブツリー全体のハミング距離の和
;;                       C
;;                       G
;;                       T
;;      )
;; 子ノード２つの４つ組から親ノードの４つ組を求める
;; 2021/10/29 GAP も加えて５つ組
(define (calc-tree-hd hdlist1 hdlist2)
  (let ((ret 0)
	(hd  0)
	(jmax -1)
	(kmax -1)
	(retval '())
	)
    (set! retval
	  (map (lambda(i)
		 (set! ret 999999)
		 (for ([j '(0 1 2 3 4)][lefthd hdlist1])
		      (for ([k '(0 1 2 3 4)][righthd hdlist2])
			   (set! hd (+ lefthd righthd (if(= i j) 0 1)(if (= i k) 0 1)))
			   (when (< hd ret)
				 (begin
				   (set! ret hd)
				   (set! jmax j)
				   (set! kmax k)))))
		 (list ret jmax kmax)
	       )
	       (iota 5)))
    retval))
;; tracke back なし版
(define (t-hd hdlist1* hdlist2*)
  (let ((hdlist1 (if (symbol? hdlist1*)
		     (symbol->hd4 hdlist1*)
		     hdlist1*))
	(hdlist2 (if (symbol? hdlist2*)
		     (symbol->hd4 hdlist2*)
		     hdlist2*))
	(ret 0)
	(hd  0)
	(retval '())
	)
    (set! retval
	  (map (lambda(i)
		 (set! ret 999999)
		 (for ([j '(0 1 2 3 4)][lefthd hdlist1])
		      (for ([k '(0 1 2 3 4)][righthd hdlist2])
			   (set! hd (+ lefthd righthd (if(= i j) 0 1)(if (= i k) 0 1)))
			   (when (< hd ret)
				 (set! ret hd))))
		 ret
	       )
	       (iota 5)))
    retval))

(define (leaf->hd4 char)
  (case char
    [(#\A ) '(0 1 1 1 1)]
    [(#\C ) '(1 0 1 1 1)]
    [(#\G ) '(1 1 0 1 1)]
    [(#\T ) '(1 1 1 0 1)]
    [(#\- ) '(1 1 1 1 0)]
    ))

(define (symbol->hd4 char)
  (case char
    [(A ) '(0 1 1 1 1)]
    [(C ) '(1 0 1 1 1)]
    [(G ) '(1 1 0 1 1)]
    [(T ) '(1 1 1 0 1)]
    [(- ) '(1 1 1 1 0)]
    ))

(define bintree-node-class%
  (class object%
	 (field
	  (name "")
	  (dna "")
	  (child '())
	  (from  '())
	  )
	 
	 (define/public (test01)
	   (displayln (format "~a[~a]" name dna))
	   )
	 (define/public (hd-min-char n)
	   (let ((it 
		  (if (null? child)     ;; leaf node
		      (begin
			;;(displayln (format "dna =~a[~a]" dna (string-ref dna n)))
			(leaf->hd4 (string-ref dna n))
		      )
		      (let ((it2
			     (calc-tree-hd (send (car  child) hd-min-char n)
					   (send (cadr child) hd-min-char n))))
			(set! from (map cdr it2))
			(map car it2)))))
	     it))
	 (define/public (init-inner-dna n) ;; "---"
	   (when (not (null? child))
	       (begin
		 (set! dna (make-string n #\-))
		 (send (car  child) init-inner-dna n)
		 (send (cadr child) init-inner-dna n))))
	 ;;
	 ;; 1st arg : dna position
	 ;; 2nd arg : nuc 0 A ,1 C ,2 G ,3 T 4 - ,root:#f
	 ;;
	 (define/public (alph0 pos nuc)
	   ;;(displayln (format "send alph=~a" name))
	   (let ((this-nuc nuc)
		 (minhdsum 0)
		 )
	     (when (not nuc)                           ;; root
		   (let ((root-enki-inf (hd-min-char pos)))   ;;side effect で子nodeの処理も行う
		     (set! this-nuc (min-index-of root-enki-inf identity))
		     (set! minhdsum (apply min root-enki-inf))))
	       ;; except root
	     (when (not (null? child))     ;; not leaf
		   (begin
		     ;;(displayln (format "spec=~a[~a] = ~a" name pos (list-ref all-enki this-nuc))) 
		     (string-set! dna pos (list-ref all-enki5 this-nuc))	;;* 2021/10/30     
		     (send (car  child) alph0 pos (car  (list-ref from this-nuc)))
		     (send (cadr child) alph0 pos (cadr (list-ref from this-nuc)))
		     ))
	     minhdsum
	     )
	   )
	 ;; alph0の後で呼ぶ
	 (define/public (disp-inner-node port)  
	   (when (not (null? child))
		 (displayln (format ">~a~%~a" name dna) port)
		 (send (car  child) disp-inner-node port)
		 (send (cadr child) disp-inner-node port)))
	 ;;検算用上記の後で呼ぶ
	 (define/public (check-hamming)
	   (if (null? child)
	       0
	       (+ (apply + (map (lambda(x)(send x check-hamming)) child))
		  (hamming-distance dna (get-field dna (car  child)))
		  (hamming-distance dna (get-field dna (cadr child))))))
			 
			 
;;;--*-- end of methods --*--
	 (super-new)
	 ))

(define (species->dna sp-name leaves-alist)
  (let ((it (assoc sp-name leaves-alist)))
    (if it
	(cadr it)
	#f)))
;;
;; rev-newick
;; newickをパーレンで囲んでカンマを除去してS式として読める形にしてから全レベルでreverseしたもの
;; leaves-alist
;; 葉の名前とdnaの対のリスト
;;
	
(define (make-root  rev-newick)
  (make-node (car rev-newick)(cadr rev-newick)))

(define (make-inner rev-newick)
  (case (length rev-newick)
    [(4)
     (if (and (list? (cadr   rev-newick))
	      (list? (cadddr rev-newick)))
	 (list
	  (make-node (car   rev-newick)(cadr   rev-newick))
	  (make-node (caddr rev-newick)(fourth rev-newick)))
	 "Tree Format Error 4")]
  
    [(3)
     (if (list? (cadr rev-newick))
	 (list
	   (make-node (car rev-newick)(cadr rev-newick))
	   (make-node (caddr rev-newick) '()))
	 (if (list? (caddr rev-newick))
	     (list
	       (make-node (car rev-newick) '())
	       (make-node (cadr rev-newick)(caddr rev-newick)))
	     "Tree Format Error 3"
	     ))]
    [(2)
     (if (and (symbol? (car  rev-newick))
	      (symbol? (cadr rev-newick)))
	 (list
	   (make-node (car  rev-newick) '())
	   (make-node (cadr rev-newick) '()))
	 "Tree Format Error 2")]))
		

    
(define (make-node sp-name child-list)
  ;(displayln (format "make-node: ~a" sp-name))
  (let ((my-instance (new bintree-node-class%))
	(sp-dna (species->dna sp-name *leaves-alist*))
	)
    ;(when sp-dna
    ;      (displayln (format "dna= ~a" sp-dna)))
    (set-field! name my-instance sp-name)
    (set-field! dna  my-instance sp-dna)
    
    (set-field! child my-instance
		(if (null? child-list)
		    '()
		    (make-inner child-list)))
    my-instance))

;;--*--
(define (exec_alph port)
  (let ((sumhd 0))
    (set! *mytree* (make-root *tree*))
    (send *mytree* init-inner-dna *dnalen*)
    (set! sumhd (apply + (map (lambda(i)(send *mytree* alph0 i #f))(iota *dnalen*))))
    (displayln sumhd port) 
    (send *mytree* disp-inner-node port)))

	  
;;--*-- test 1
(define (disp-bin-descendant binlist)
  (send (bin-descendant0 *mytree* binlist) test01))

(define (bin-descendant binlist)
  (bin-descendant0 *mytree* binlist))
		   
(define (bin-descendant0  mytree  binlist)
  (if (null? binlist)
      mytree
      (bin-descendant0 (list-ref (get-field child mytree)(car binlist)) (cdr binlist))))

;;--*-- test 2 --*--
;; (send mytree hd-min-char 0)
;; (get-field from (bin-descendant '(1 0 )))
;; (get-field dna (bin-descendant '(1 0 )))

