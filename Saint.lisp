(ql:quickload "trivia")
(use-package :trivia)

(defun elf (func)
  (match func
	 ((list 'exp a) (list 'exp a))
	 ((list 'sin a) (list '- 'cos a))
	 ((list 'cos a) (list 'sin a))))
	  

