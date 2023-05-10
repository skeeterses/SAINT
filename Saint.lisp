(ql:quickload "trivia")
(use-package :trivia)

(defun elf (func)
  (match func
	 ((guard (list a b)
		 (and (constant? a) (dx? b)))
	  (list a (char b 1)))
	 ((guard (list (list 'exp a) b)
		 (and (variable? a) (dx? b)))
		 (list 'exp a))
	 ((guard (list (list 'sin a) b)
		 (and (variable? a) (dx? b)))
	  (list '- 'cos a))
	 ((guard (list (list 'expt a b) c)
		 (and (constant? a) (variable? b) (dx? c)))
	  (list (list 'expt a b) '/ (list 'log b)))
	 ((guard (list (list 'log a b) c)
		 (and (constant? b) (variable? a) (dx? c)))
	  (list (list a (list 'log a b)) '- (list a '/ (list 'log b))))
	 ((guard (list (list 'cos a) b)
		 (and (variable? a) (dx? b)))
	  (list 'sin a))))


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

(defun variable? (elem)
  (cond ((characterp elem)
	 (if (lower-case-p elem)
	   elem
	   nil))
	(t nil)))
