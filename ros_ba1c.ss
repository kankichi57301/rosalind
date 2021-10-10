#lang racket
;; rosalind
;; Find the Reverse Complement of a String
;; [BA1C] 2021/07/08  AC
#|
How to execute
>racket
>>(enter! "ros_ba1c.ss")
>>(ros_ba1c 1)
������1�́@"data\\rs_ba1c1.txt"��Ώۂɑ��삷�邱�Ƃ��w�肷��
(ros_ba1c)�ƈ������ȗ������"data\\rosalind_ba1c.txt"(�{�ԗp�_�E���[�h�t�@�C��)���ΏۂɂȂ�
���ʂ͕ϐ�*ba1c_out*�̒l"data\\ba1c_out.txt"�Ŏ����t�@�C���ɏo�͂����B
|#

(require srfi/13)
(require "readfileA.ss")
(require "roslibA.ss")
(define *ba1c_out* "data\\ba1c_out.txt")

(define (ros_ba1c . n)
  (let* ((data (read-file*
		(if (null? n)
		    "data\\rosalind_ba1c.txt"
		    (format "data\\rs_ba1c~a.txt" (car n)))))
	 (res (make-reverse-complement (car data)))
       )
    (call-with-output-file *ba1c_out*
      (lambda(out)
	(display res out))
      #:exists 'truncate/replace)
   res 
))

	 
				

