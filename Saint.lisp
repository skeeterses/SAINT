(ql:quickload "trivia")
(use-package :trivia)

(defun elf (func)
  (match func
	 ((guard (list a b)
		 (and (constant? a) (dx? b)))
	  (list a (char b 1)))
	 ((guard (list (list 'exp a) b)
		 (dx? b))
		 (list 'exp a))))
;  (match func	 	 
;	 ((list 'exp a) (list 'exp a))
;	 ((list 'sin a) (list '- 'cos a))
;	 ((list 'cos a) (list 'sin a))
	
	  
(defun dx? (elem)
  (if (stringp elem)
    (and (= (length elem) 2)
	 (char= (char elem 0) #\d)
	 (alpha-char-p (char elem 1)))
    nil))

(defun constant? (elem)
  (cond ((numberp elem) elem)
	((characterp elem)
	 (if (upper-case-p elem)
	   elem
	   nil))
	 (t nil)))
