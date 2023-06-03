#lang racket
(require rackunit)

; verifica se um parâmetro é um átomo
(define (atom? x)
  (and (not (null? x))
      (not (pair? x))))

; teste da função atom?
(check-equal? (atom? 'a) #t)
(check-equal? (atom? '(a b c)) #f)

; verificas e não é um átomo
(define (not-atom? x)
  (not (atom? x)))

; teste da função not-atom?
(check-equal? (not-atom? 'a) #f)
(check-equal? (not-atom? '(a b c)) #t)

; verifica se são todos átomos
(define (all-atom? list)
  (cond
    [(null? list) #t] ; caso base
    [(not-atom? (first list)) #f] ; resolve uma parte
    [else (all-atom? (rest list))])) ; resolve o resto

; teste da função all-atom?
(check-equal? (all-atom? '(a b c)) #t)
(check-equal? (all-atom? '(a b c (d))) #f)

; numa lista de átomo, verificar se um valor é membro
(define (member? list value)
  (cond
    [(null? list) #f]
    [(eqv? value (first list)) list]
    [else (member? (rest list) value)]))

; teste da função member?
(check-equal? (member? '(a b c) 'c) '(c))
(check-equal? (member? '(a b c) 'd) #f)

; numa lista de números, somar todos os números
(define (soma-num list)
  (cond
    [(null? list) 0]
    [else (+ (first list) (soma-num (rest list)))]))

; soma números em uma lista dobrando pela direita
(check-equal? (soma-num '(1 2 3 4)) 10)
(check-equal? (soma-num '()) 0)

(define (soma-num2 list)
  (if (null? list)
     0
     (+ (first list) (soma-num2 (rest list)))
     ))

; soma números em uma lista dobrando pela direita
(define (mul-num list)
  (if (null? list)
     1
     (* (first list) (mul-num (rest list)))
     ))

; soma números em uma lista dobrando pela esquerda
(define (soma-num3 list)
  (sn_aux list 0))

(define (sn_aux list acc)
  (if (null? list)
     acc
     (sn_aux (rest list) (+ (first list) acc))))

; inverte uma lista (dobra pela esquerda)
(define (invert list)
  (inv-aux list '()))

(define (inv-aux list acc)
  (if (null? list)
     acc
     (inv-aux (rest list) (cons (first list) acc))))

; remove a primeira ocorrência de um membro da lista
(define (remove-member m list)
  (cond
    [(null? list) '()]
    [(eqv? (first list) m) (rest list)]
    [else
     (cons (first list) (remove-member m (rest list)))]
    ))

; remove todas as ocorrências de um membro da lista
(define (remove-all-member m list)
  (cond
    [(null? list) '()]
    [(eqv? (first list) m) (remove-all-member m (rest list))]
    [else
     (cons (first list) (remove-all-member m (rest list)))]
    ))

; RECURSION!!!
; (println "Verifica se todos os membros da lista '(a b c) são átomos")
; (all-atom? '(a b c))
; (println "Verifica se todos os membros da lista '(a b c (d)) são átomos")
; (all-atom? '(a b c (d)))
; (println "Verifica se 'c é um membro da lista '(a b c d)")
; (member? '(a b c d) 'c)
; (println "Somar os elementos da lista '(1 2 3 4) pela direita")
; (soma-num '(1 2 3 4))
; ;(soma-num2 '(1 2 3 4))
; (println "Somar os elementos da lista '(1 2 3 4) pela esquerda")
; (soma-num3 '(1 2 3 4))
; (println "Multiplicar os elementos da lista '(1 2 3 4)")
; (mul-num '(1 2 3 4))
; (println "Inverter a lista '(1 2 3 4)")
; (invert '(1 2 3 4))
; (println "Remove a primeira ocorrência do elemento 2 da lista '(1 2 3 2 4)")
; (remove-member 2 '(1 2 3 2 4))
; (println "Remove a todas as ocorrências do elemento 2 da lista '(1 2 3 2 4)")
; (remove-all-member 2 '(1 2 3 2 4))
