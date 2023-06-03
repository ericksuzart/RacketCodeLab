#lang racket
(require "../aula2-tarde.rkt")
(require rackunit)

; remove-membro (e, l)
; remove a primeira ocorrência do elemento 'e' na lista 'l'
; atom? list -> list?
(define (remove-membro e l)
  (cond
    [(empty? l) empty]
    [(eqv? e (first l)) (rest l)]
    [else (cons (first l) (remove-membro e (rest l)))]))

; Teste remove-membro:
(check-equal? (remove-membro 1 empty) empty)
(check-equal? (remove-membro 1 (list 1)) empty)
(check-equal? (remove-membro 1 (list 1 2)) (list 2))
(check-equal? (remove-membro 1 (list 1 2 1)) (list 2 1))
(check-equal? (remove-membro 2 (list 1 2 1 3)) (list 1 1 3))

; remove-todos (e, l)
; remove todas as ocorrências do elemento 'e' na lista 'l'
; atom? list -> list?
(define (remove-todos e l)
  (cond
    [(empty? l) empty]
    [(eqv? e (first l)) (remove-todos e (rest l))]
    [else (cons (first l) (remove-todos e (rest l)))]))

; Teste remove-todos:
(check-equal? (remove-todos 1 empty) empty)
(check-equal? (remove-todos 1 (list 1)) empty)
(check-equal? (remove-todos 1 (list 1 2)) (list 2))
(check-equal? (remove-todos 1 (list 1 2 1)) (list 2))

; dobra (l)
; dobra os elementos da lista 'l'
; number? list? -> list?
(define (dobra l)
  (cond
    [(empty? l) empty]
    [else (cons (* 2 (first l)) (dobra (rest l)))]))

; Teste dobra:
(check-equal? (dobra empty) empty)
(check-equal? (dobra (list 1)) (list 2))
(check-equal? (dobra (list 1 2)) (list 2 4))

; substitui-todos (e, n, l)
; substitui todas as ocorrências do elemento 'e' pelo elemento 'n' na lista 'l'
; atom? atom? list? -> list?
(define (substitui-todos e n l)
  (cond
    [(empty? l) empty]
    [(eqv? e (first l)) (cons n (substitui-todos e n (rest l)))]
    [else (cons (first l) (substitui-todos e n (rest l)))]))

; Teste substitui-todos:
(check-equal? (substitui-todos 1 2 empty) empty)
(check-equal? (substitui-todos 1 2 (list 1)) (list 2))
(check-equal? (substitui-todos 1 2 (list 1 2)) (list 2 2))

; membro*? e lg
; verifica se o elemento 'e' pertence à lista genérica 'lg'
; atom? list? -> boolean?
(define (membro*? e lg)
  (cond
    [(empty? lg) false]
    [(list? (first lg))
     (or (membro*? e (first lg))
         (membro*? e (rest lg)))]
    [(eqv? e (first lg)) true]
    [else (membro*? e (rest lg))]))

; Teste membro?:
(check-equal? (membro*? 1 empty) false)
(check-equal? (membro*? 1 '(1)) true)
(check-equal? (membro*? 1 '(1 2)) true)
(check-equal? (membro*? 1 '((2) 1)) true)
(check-equal? (membro*? 1 '((2) (1))) true)
(check-equal? (membro*? 1 '((2) ((1) 2))) true)

; remove-membro* (e, lg)
; remove todas as ocorrências do elemento 'e' na lista genérica 'lg'
; atom? list? -> list?
(define (remove-membro* e lg)
  (cond
    [(empty? lg) empty]
    [(list? (first lg))
     (cons (remove-membro* e (first lg))
           (remove-membro* e (rest lg)))]
    [(eqv? e (first lg)) (rest lg)]
    [else (cons (first lg) (remove-membro* e (rest lg)))]))

; Teste remove-membro*:
(check-equal? (remove-membro* 1 empty) empty)
(check-equal? (remove-membro* 1 '(1)) empty)
(check-equal? (remove-membro* 1 '(1 2)) '(2))
(check-equal? (remove-membro* 1 '((2) 1)) '((2)))
(check-equal? (remove-membro* 2 '((2) 1)) '(() 1))
(check-equal? (remove-membro* 1 '((2) (1))) '((2) ()))
(check-equal? (remove-membro* 1 '((2) ((1) 2))) '((2) (() 2)))

; remove-um-membro* (e, lg)
; remove a primeira ocorrência do elemento 'e' na lista genérica 'lg'
; atom? list? -> list?
(define (remove-um-membro* e lg)
  (let-values ([(b lr) (remove-um-membro-rec e lg) ])
    lr))

; remove-um-membro-rec (e, lg) 
; auxiliar para a função remove-um-membro* removendo a primeira ocorrência do
; elemento 'e' na lista genérica 'lg' retornando um booleano e a lista
; resultante
; atom? list? -> boolean? list?
(define (remove-um-membro-rec e lg)
  (cond
    [(empty? lg) (values false empty)]
    [(list? (first lg))
     (let-values ([(b1 lr1) (remove-um-membro-rec e (first lg))])
       (if b1
           (values true (cons lr1 (rest lg)))
           (let-values ([(b2 lr2) (remove-um-membro-rec e (rest lg))])
             (values b2 (cons lr1 lr2)))
           ))]
    [(eqv? e (first lg))
     (values true (rest lg))
     ]
    [else (let-values ([(b2 lr2) (remove-um-membro-rec e (rest lg))])
            (values b2 (cons (first lg) lr2)))
          ]
    )
  )

; Teste remove-um-membro*:
(check-equal? (remove-um-membro* 1 empty) empty)
(check-equal? (remove-um-membro* 1 '(1)) empty)
(check-equal? (remove-um-membro* 1 '(1 2)) '(2))
(check-equal? (remove-um-membro* 1 '((2) 1)) '((2)))
(check-equal? (remove-um-membro* 2 '((2) 1)) '(() 1))
(check-equal? (remove-um-membro* 1 '((2) (1))) '((2) ()))
(check-equal? (remove-um-membro* 1 '((2) ((1) 2))) '((2) (() 2)))


; insere-direita* a e lg
; insere o elemento 'e' à direita de todas as ocorrências do elemento 'a' na lista genérica 'lg'
; atom? atom? list? -> list?
(define (insere-direita* a e lg)
  (cond
    [(empty? lg) empty]
    [(list? (first lg))
     (cons (insere-direita* a e (first lg))
           (insere-direita* a e (rest lg)))]
    [(eqv? a (first lg))
     (cons a (cons e (insere-direita* a e (rest lg))))]
    [else (cons (first lg) (insere-direita* a e (rest lg)))]))

; Teste insere-direita*:
(check-equal? (insere-direita* 1 2 empty) empty)
(check-equal? (insere-direita* 1 2 '(1)) '(1 2))
(check-equal? (insere-direita* 1 3 '(1 2)) '(1 3 2))
(check-equal? (insere-direita* 1 3 '((2) 1)) '((2) 1 3))
(check-equal? (insere-direita* 2 3 '((2) 1)) '((2 3) 1))
(check-equal? (insere-direita* 3 5 '((2) 1)) '((2) 1))

; inverso* lg
; inverte todos os elementos da lista genérica 'lg'
; list? -> list?
(define (inverso* lg)
  (inverte-aux* lg empty))

; inverte-aux* lg acc
; auxiliar de inverso*
; list? list? -> list?
(define (inverte-aux* lg acc)
  (cond
    [(empty? lg) acc]
    [(list? (first lg))
     (inverte-aux* (rest lg) (cons (inverso* (first lg)) acc))]
    [else (inverte-aux* (rest lg) (cons (first lg) acc))]))

; Teste inverso*:
(check-equal? (inverso* empty) empty)
(check-equal? (inverso* '(1)) '(1))
(check-equal? (inverso* '(1 2)) '(2 1))
(check-equal? (inverso* '((2) 1)) '(1 (2)))
(check-equal? (inverso* '((2) (1))) '((1) (2)))
(check-equal? (inverso* '((2) ((1) 2))) '((2 (1)) (2)))
(check-equal? (inverso* '((2) ((1) 2) 3)) '(3 (2 (1)) (2)))


(letrec [(f (lambda (l)
              (if (null? l)
                  0
                  (+ (first l) (f (rest l)))
                  )
              )
            )]
  (check-equal? (f (list 1 2 3)) 6))


; inverte2 lg
; inverte todos os elementos da lista genérica 'lg'
; list? -> list?
(define (inverte2 l)
  (inv-aux l empty))

(define (inv-aux lg acc)
  (cond
    [(empty? lg) acc]
    [(list? (first lg))
     (inv-aux (rest lg) (cons (inverte2 (first lg)) acc))]
    [else (inv-aux (rest lg) (cons (first lg) acc))]))

; inverte lista 
; inverte a lista usando parametro
; list? -> list?
(define (inverte l [acc empty])
  (cond
    [(empty? l) acc]
    [else (inverte (rest l) (cons (first l) acc))]))

; Teste inverte:
(check-equal? (inverte empty) empty)
(check-equal? (inverte '(1)) '(1))
(check-equal? (inverte '(1 2)) '(2 1))
(check-equal? (inverte '((2) 1)) '(1 (2)))
(check-equal? (inverte '((2) (1))) '((1) (2)))

; minha-soma lista variadica
; minha-soma todos os elementos da lista
; list? -> number?
(define (minha-soma . l)
  (minha-soma-rec l 0))

(define (minha-soma-rec l [acc 0])
  (cond
    [(empty? l) acc]
    [else 
     (+ acc (first l) (minha-soma-rec (rest l) acc))]))
; Teste minha-soma:
(check-equal? (minha-soma) 0)
(check-equal? (minha-soma 1) 1)
(check-equal? (minha-soma 1 2) 3)
(check-equal? (minha-soma 1 2 3) 6)
(check-equal? (minha-soma 1 2 3 4) 10)

; minha-soma2 lista variadica
; minha-soma2 todos os elementos da lista usando parametro sem função auxiliar
; list? -> number?
(define (minha-soma2 . l)
  (cond
    [(empty? l) 0]
    [else (+ (first l) (apply minha-soma2 (rest l)))]))

; Teste minha-soma2:
(check-equal? (minha-soma2) 0)
(check-equal? (minha-soma2 1) 1)
(check-equal? (minha-soma2 1 2) 3)
(check-equal? (minha-soma2 1 2 3) 6)
(check-equal? (minha-soma2 1 2 3 4) 10)

; escreva uma função para criar uma lista intercalada de tamanho n com os
; elementos e_1 .. e_n
; exemplo: (intercala-n 5 'a 'b) => '(a b a b a)

; intercala n ... -> list?
; numero? any/c ... -> list?
(define (intercala-n n . l)
  (cond
    [(< n 1) empty]
    [else 
     (cons (first l)
           (apply intercala-n
                  (cons (- n 1) (insere-fim (first l) (rest l)))))]))

(define (insere-fim a lg)
  (cond
    [(empty? lg) (list a)]
    [(list? (first lg))
     (cons (insere-fim a (first lg)) (rest lg))]
    [else (cons (first lg) (insere-fim a (rest lg)))]))

; Teste insere-fim
(check-equal? (insere-fim 1 empty) '(1))
(check-equal? (insere-fim 1 '(1)) '(1 1))
(check-equal? (insere-fim 1 '(1 2)) '(1 2 1))

; Teste intercala-n
(check-equal? (intercala-n 0) empty)
(check-equal? (intercala-n 1 'a) '(a))
(check-equal? (intercala-n 1 'a) '(a))
(check-equal? (intercala-n 2 'a 'b) '(a b))
(check-equal? (intercala-n 3 'a 'b 'c) '(a b c))
(check-equal? (intercala-n 3 'a 'b 'c 'd) '(a b c))
(check-equal? (intercala-n 5 'a 'b 'c) '(a b c a b))
(check-equal? (intercala-n 5 'a 'b 'c 'd) '(a b c d a))
(check-equal? (intercala-n 5 'a 'b 'c 'd 'e) '(a b c d e))

; generic-remove 
(define (generic-remove v l #:proc [proc eqv?] #:key [key identity])
  (cond
    [(empty? l) empty]
    [(proc (key (first l)) v)
     (generic-remove v (rest l) #:proc proc #:key key)]
    [else (cons (first l)
                (generic-remove v (rest l) #:proc proc #:key key))]))

; Teste generic-remove
(check-equal? (generic-remove 1 empty) empty)
(check-equal? (generic-remove 1 '(1)) empty)
(check-equal? (generic-remove 1 '(1 2)) '(2))
(check-equal? (generic-remove 1 '(1 2 1)) '(2))
(check-equal? (generic-remove 1 '(1 2 1 3)) '(2 3))
(check-equal? (generic-remove 2 '(1 2) #:proc <=) '())
(check-equal? (generic-remove 2 '(1 2 3) #:proc <=) '(3))
(check-equal? (generic-remove 40 '((manoel 31)(maria 41)(jose 41)(joana 18)) #:proc < #:key second) '((maria 41)(jose 41)))
