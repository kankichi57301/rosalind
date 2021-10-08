#lang racket
;; rosalind
;; Compute the Number of Times a Pattern Appears in a Text
;; [BA1A] 2021/07/06 AC 
#|
How to execute
>racket
>>(enter! "ros_ba1a.ss")
>>(ros_ba1a 1)
引数の1は　"rs_ba1a1.txt"を対象に操作することを指定する
(ros_ba3a)と引数を省略すると"rosalind_ba1a.txt"(本番用ダウロードファイル)が対象になる
結果は変数*ba1a_out*の値"ba1a_out.txt"で示すファイルに出力される。
|#

(require srfi/13)
(require "readfileA.ss")
(define *ba1a_out* "ba1a_out.txt")

(define (ros_ba1a . n)
  (let* ((data (read-file*
		(if (null? n)
		    "rosalind_ba1a.txt"
		    (format "rs_ba1a~a.txt" (car n)))))
	 (x (string->list(car data)))
	 (y (string->list(cadr data)))
	 (xlen (length x))
	 (ylen (length y))
       )

    
    (define (list-count all part alllen partlen)
      (if (< alllen partlen)
	  0
	  (+ (if (equal? (take all partlen) part)
		 1 0)
	     (list-count (cdr all) part (- alllen 1) partlen))))

    
    (list-count x y xlen ylen)
    
    (call-with-output-file *ba1a_out*
      (lambda(out)
	(display data out))
      #:exists 'truncate/replace)
    
))

	 
				

