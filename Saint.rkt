#lang racket
; The 26 elementary forms (elf) from Jame's Slagle's 1961 Saint calculus integration
; program.
; The syntax of a saint integration function is
; (integrate '(function))
; Some of the math functions will have to be written out in order to do any numeric
; computations.

(define elf (lambda (function)
              ; Test the trivial case of integral dv = v             
              (let ([funLength (length function)] [dv (last function)])
                (if (dv? dv)
                    (let ([variable (string-ref dv 1)])
                      (cond
                        ; Trivial case of integral dv = v
                        [(= funLength 1) (string-ref dv 1)]
                        [(= funLength 2)
                           (let ([mainFunction (car function)])
                           (cond [(number? mainFunction) (list mainFunction variable)]
                                 [(and (char? mainFunction)
                                     (char-upper-case? mainFunction)) (list mainFunction variable)]   ; Rule a) Cdv -> Cv.
                                 [(equal? mainFunction variable)
                                  (list '/ (list 'expt variable '2) '2)]   ; Rule u-i) vdv -> 1/2 * v^2
                                 [(list? mainFunction)
                                  (cond
                                    [(and (equal? 'exp (car mainFunction))
                                          (equal? variable (cadr mainFunction)))
                                     mainFunction]  ; Rule b) (e^x dx --> e^x
                                    [(and (equal? 'expt (car mainFunction))
                                          (equal? variable (caddr mainFunction))
                                          (or (number? (cadr mainFunction))
                                              (char? (cadr mainFunction))))
                                     (list '/ (list 'expt (cadr mainFunction) variable) (list 'log (cadr mainFunction)))]
                                      ; Rule c above: c^v dv = c^v / ln c
                                    [(and (equal? 'log (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list '- (list variable (list 'log variable)) variable)]  ; Rule d: ln v --> vlnv - v
                                    [(and (equal? 'log (car mainFunction))
                                          (equal? variable (caddr mainFunction))
                                          (or (number? (cadr mainFunction))
                                              (char? (cadr mainFunction))))
                                     (list '- (list variable 'log (cadr mainFunction) variable)
                                              (list '/ variable (list 'log (cadr mainFunction))))] ; Rule e
                                    [(and (equal? 'sin (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list '- 'cos variable)] ; Rule f
                                    [(and (equal? 'cos (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list 'sin variable)]  ; Rule g
                                    [(and (equal? 'tan (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list 'log (list 'sec variable))] ; Rule h
                                    [(and (equal? 'cot (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list 'log (list 'sin variable))] ; Rule i
                                    [(and (equal? 'sec (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list 'log (list '+ (list 'sec variable) (list 'tan variable)))] ; Rule j
                                    [(and (equal? 'csc (car mainFunction)) (equal? variable (cadr mainFunction)))
                                     (list 'log (list '- (list 'csc variable) (list 'cot variable)))] ; Rule k
                                    [else #f])]
                                 [else #f]))]
                        [else #f]))
                    #f)))) ; The function doesn't have "dx" at the end.
                                 
                                 
                                                  
;                  (if (dv? (last function))
;                      (let ([variable (string-ref (last function) 1)])
;                        ; integral Cdv = Cv
;                        (if (= 2 (length function))
;                            (cond
;                              [(number? (car function)) (list (car function) variable)]
;                              [(and (char? (car function))
;                                    (char-upper-case? (car function))) (list (car function) variable)])
;                            '()))
;                      #f))))
                        
                        

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


; The first part of uSubstitution is complete.
; uSubstitution of u=sinx for ( sinx cosx dx) results in (u cosx dx)
; The next step is to replace (cosx dx) with du.
(define uSubstitution (lambda (u fx function)
                        (if (null? function)
                            '()
                          (let ([head (car function)])
                          (if (equal? fx head)
                              (append (list u) (uSubstitution u fx (cdr function)))
                              (if (list? head)
                                  (append (list (uSubstitution u fx head))
                                          (uSubstitution u fx (cdr function)))
                                  (append (list head) (uSubstitution u fx (cdr function)))))))))

; The next part of the uSubstitution
; If uSubstitution u=sin x results in (u cos dx), the du part has to equal cosx dx,
; and applying duSubstitute should give u du.
; If du is not present, the function returns #f and thus the uSubstitution did not work.
; (duSubstitute "du" '( (cos x) "dx") (uSubstitution #\u '(sin x) '( (sin x) (cos x) "dx")))
;  --> '(#\u "du")
; (duSubstitute "du" '( (cos x) "dx") (uSubstitution #\u '(sin x) '( (expt (sin x) 2) (cos x) "dx")))
;  ---> '((expt #\u 2) "du")
;
(define duSubstitute (lambda (du dx function)
                       (if (null? function)
                           #f
                           (if (equal? dx function)
                               (list du)
                               (append (list (car function))
                                       (duSubstitute du dx (cdr function)))))))

                        
                        


              
              
