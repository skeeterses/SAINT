#lang racket
; The 26 elementary forms (elf) from Jame's Slagle's 1961 Saint calculus integration
; program.
; The syntax of a saint integration function is
; (integrate '(function))
; Some of the math functions will have to be written out in order to do any numeric
; computations.

(define elf (lambda (function)
              ; Test the case of 'integral ((/ dv v)) --> lnv --Case t
              (if (not (dv? (last function)))
                  (let ([mainFunction (car function)])
                    (if (list? mainFunction)
                        (let ([dv (cadr mainFunction)] [v (caddr mainFunction)])
                          (if (and (equal? '/ (car mainFunction)) (dv? dv) (equal? v (string-ref dv 1)))
                              (list 'log v)
                              #f))
                        #f))                          
              ; Test the trivial case of integral dv = v             
                  (let ([dv (last function)])
                    (if (dv? dv)
                        (let ([variable (string-ref dv 1)] [funLength (length function)])
                          (cond
                            ; Trivial case of integral dv = v
                            [(= funLength 1) (string-ref dv 1)]
                            [else
                             (elfMatch (list (take function (- funLength 1)) variable))]))
                        #f)))))

;                             (match (take function (- funLength 1))
;                               ['(#\x) (list '/ (list 'expt #\x '2) '2)]  ; Rule u-i) vdv -> 1/2 * v^2;
;                               [(list 'exp #\x) (list 'exp #\x)]
;                               [_ #f]
;                               )]))
;                        #f)))))

(define elfMatch (lambda (function)
  (match function
    [(list (list a) a) (list '/ (list 'expt a 2) 2)]  ; Rule u-i) vdv -> 1/2 * v^2;
    [(list (list (list 'exp a)) a) (list 'exp a)]     ; Rule b) (e^x dx --> e^x
    [(list (list (list 'expt (? number? b) a)) a) (list '/ (list 'expt b a) (list 'log a))] ; Rule c above: c^v dv = c^v / ln c 
    [(list (list (list 'expt (? char? b) a)) a) (list '/ (list 'expt b a) (list 'log a))]
    [(list (list (list 'log a)) a) (list '- (list a (list 'log a)) a)]  ; Rule d: ln v --> vlnv - v
    [(list (list (list 'log (? number? b) a)) a) (list '- (list a (list 'log b a)) (list '/ a (list 'log b)))] ;Rule e
    [(list (list (list 'log (? char? b) a)) a) (list '- (list a (list 'log b a)) (list '/ a (list 'log b)))]
    [(list (list (list 'sin a)) a) (list '- 'cos a)]  ; Rule f
    [(list (list (list 'cos a)) a) (list 'sin a)]     ; Rule g
    [(list (list (list 'tan a)) a) (list 'log (list 'sec a))]  ; Rule h
    [(list (list (list 'cot a)) a) (list 'log (list 'sin a))]  ; Rule i
    [(list (list (list 'sec a)) a) '()]   ; Rule j
    [(list (list (list 'csc a)) a) '()]   ; Rule k
    [(list (list (list 'asin a)) a) '()]  ; Rule L
    [(list (list (list 'acos a)) a) '()]  ; Rule m
    [(list (list (list 'atan a)) a) '()]  ; Rule n
    [(list (list (list 'acot a)) a) '()]  ; Rule o
    [(list (list (list 'asec a)) a) '()]  ; Rule p
    [(list (list (list 'acsc a)) a) '()]  ; Rule q
    [(list (list (list 'expt (list 'sec a) 2)) a) '()]  ; Rule r
    [(list (list (list 'expt (list 'csc a) 2)) a) '()]  ; Rule s
    [_ #f])))




; Algorithmic like transformations below.
; The decomposition transform will be a little tricky.  In the LISP like languages, (- f(x)g(x)) usually means
; f - g, but in this program, f(x)g(x) means f(x) * g(x).  To get around that, I'm going to use '+ for all additions
; and subtractions and have subtraction represented as the addition of negative numbers; (+ (-A) B C (-D)).
(define algTransform (lambda (function)
                       (cond
                         [(number? (car function))                              
                          (list (car function) (integrate (cdr function)))] ; Case a: Factor constant
                         [(equal? '- (car function))
                          (list '- (integrate (cdr function)))]  ; Case b: Negate
                         [else #f])))

(define integrate (lambda (function)
                    (let ([result1 (elf function)])
                      (if result1
                          result1
                          (algTransform function)))))
                          
                                                         

; Check the format to see if a string is 2 letters and 
; d folowed by lowercase letter.
; dv is true but dV is false and daa is false
(define dv? (lambda (dv)
              (if (string? dv)
                  (if (= (string-length dv) 2)
                      (if (and (char=? (string-ref dv 0) #\d)
                               (char-lower-case? (string-ref dv 1)))
                          #t
                          #f)
                      #f)
                  #f)))


;(uSubstitution #\u '(sin x) "du" '(cos x) '( (sin x) (cos x)))
; ---> '(#\u "du")
;> (uSubstitution #\u '(sin x) "du" '(cos x) '( (expt (sin x) 2) (cos x)))
;--->'((expt #\u 2) "du")
;(uSubstitution #\u '(cos x) "du" '(- (sin x)) '( (/ (- (sin x)) (cos x))))
; ---> '((/ "du" #\u))
(define uSubstitution (lambda (u fx du dx function)
                        (if (null? function)
                            '()
                          (let ([head (car function)])
                            (cond [(equal? fx head)
                                   (append (list u) (uSubstitution u fx du dx (cdr function)))]
                                  [(equal? dx head)
                                   (append (list du) (uSubstitution u fx du dx (cdr function)))]
                                  [(list? head)
                                   (append (list (uSubstitution u fx du dx head))
                                           (uSubstitution u fx du dx (cdr function)))]                                  
                                  [else (append (list head) (uSubstitution u fx du dx (cdr function)))])))))
                          
                        
                        


              
              
