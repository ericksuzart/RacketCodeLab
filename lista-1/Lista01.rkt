#lang racket
(require rackunit)


; Questão 1
; Escreva as cláusulas para concatenar duas listas em racket:
; (concatenar1 l1 l2)
; Por exemplo:
; > (concatenar1 '(a b c) '(d e f g h))
; Deve retornar:
; (a b c d e f g h)
(define (concatenar1 l1 l2)
  (if (empty? l1)
     l2
     (cons (first l1) (concatenar1 (rest l1) l2))))

(check-equal? (concatenar1 '(a b c) '(d e f g h)) '(a b c d e f g h))


; Questão 2
; Escreva as cláusulas para concatenar duas listas, sendo que a segunda lista
; vem na frente.
; (concatenarInv l1 l2)
; Por exemplo:
; > (concatenarInv '(a b c) '(d e f g h))
; Deve retornar:
; (d e f g h a b c)
(define (concatenarInv l1 l2)
  (if (empty? l2)
     l1
     (cons (first l2) (concatenarInv l1 (rest l2)))))

(check-equal? (concatenarInv '(a b c) '(d e f g h)) '(d e f g h a b c))


; Questão 3
; Escreva uma função para concatenar uma lista de listas sem utilizar nenhuma
; das funções anteriores.
; (concatenar2 ll)
; Por exemplo:
; > (concatenar2 '((a b) (c) (d e f g)))
; Deve retornar:
; (a b c d e f g)
(define (concatenar2 ll)
  (if (empty? ll)
     empty
     (append (first ll) (concatenar2 (rest ll)))))

(check-equal? (concatenar2 '((a b) (c) (d e f g))) '(a b c d e f g))


; Questão 4
; Escreva uma função variádica para concatenar N listas.
; (concatenar3 l ...)
; Por exemplo:
; > (concatenar3 '(a b) '(c) '(d e f g))
; Deve retornar:
; (a b c d e f g)
(define (concatenar3 . l)
  (if (empty? l)
     empty
     (append (first l) (apply concatenar3 (rest l)))))

(check-equal? (concatenar3 '(a b) '(c) '(d e f g)) '(a b c d e f g))


; Questão 5
; Escreva as cláusulas para juntar duas listas, intercalando seus elementos.
; (juntar l1 l2)
; Por exemplo:
; > (juntar '(a b c) '(d e f g h))
; Deve retornar:
; (a d b e c f g h)
(define (juntar l1 l2)
  (if (empty? l1)
     l2
     (cons (first l1) (juntar l2 (rest l1)))))

(check-equal? (juntar '(a b c) '(d e f g h)) '(a d b e c f g h))


; Questão 6
; Escreva uma função para criar uma lista intercalada de tamanho N, de dois
; elementos e1 e e2.
; (intercala n e1 e2)
; Por exemplo:
; > (intercala 5 'x 'y)
; Deve retornar:
; (x y x y x)
(define (intercala n e1 e2)
  (cond
    [(< n 1) empty]
    [(= n 1) (list e1)]
    [(= n 2) (list e1 e2)]
    [else (cons e1 (cons e2 (intercala (- n 2) e1 e2)))]))

(check-equal? (intercala 5 'x 'y) '(x y x y x))


; Questão 7
; Escreva uma função para criar uma lista intercalada de tamanho N, de M
; elementos e_M .
; (intercala2 n e ... )
; Por exemplo:
; > (intercala 5 'x 'y 'z)
; Deve retornar:
; (x y z x y)
(define (intercala2 n . e)
  (cond
    [(< n 1) empty]
    [else
     (cons (first e)
          (apply intercala2 (- n 1) (append (rest e) (list (first e))))
          )]))

(check-equal? (intercala2 5 'x 'y 'z) '(x y z x y))
(check-equal? (intercala2 5 'x 'y 'z 'w) '(x y z w x))


; Questão 8
; Escreva a função parear que recebe um elemento E e uma lista L, e produz a
; lista de pares cujo primeiro elemento é E e o segundo elemento é um membro de
; L.
; (parear e l)
; Por exemplo:
; > (parear 'x '(a b c))
; Deve retornar:
; ((x a) (x b) (x c))
(define (parear e l)
  (if (empty? l)
     empty
     (cons (list e (first l)) (parear e (rest l)))))

(check-equal? (parear 'x '(a b c)) '((x a) (x b) (x c)))


; Questão 9
; Escreva a função pares que recebe uma lista L e produz a lista de todos os
; pares de elementos de L.
; (pares l)
; Por exemplo:
; > (pares '(a b c d))
; Deve retornar:
; ((a b) (a c) (a d) (b c) (b d) (c d))
(define (pares l)
  (if (empty? l)
     empty
     (append (parear (first l) (rest l)) (pares (rest l)))))

(check-equal? (pares '(a b c d)) '((a b) (a c) (a d) (b c) (b d) (c d)))


; Questão 10
; Escreva a função que recebe uma lista e testa se ela tem elementos repetidos
; (pode ser interpretada como um conjunto).
; (conjunto? L)
; Por exemplo:
; > (conjunto? '(a b c d))
; Deve retornar:
; #t
; > (conjunto? '(a b c d b))
; Deve retornar:
; #f
(define (conjunto? l)
  (cond
    [(empty? l) #t]
    [(member (first l) (rest l)) #f]
    [else (conjunto? (rest l))]))

(check-equal? (conjunto? '(a b c d)) #t)
(check-equal? (conjunto? '(a b c d b)) #f)


; Questão 11
; Escreva a função que recebe duas listas e testa se a primeira é prefixo da
; segunda.
; (prefixo? l1 l2)
; Por exemplo:
; > (prefixo? '(a b c) '(a b c d e f g))
; Deve retornar:
; #t
; > (prefixo? '(a b c) '(a b f g))
; Deve retornar:
; #f
(define (prefixo? l1 l2)
  (cond
    [(empty? l1) #t]
    [(empty? l2) #f]
    [(equal? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else #f]))

(check-equal? (prefixo? '(a b c) '(a b c d e f g)) #t)
(check-equal? (prefixo? '(a b c) '(a b f g)) #f)


; Questão 12
; Escreva a função que recebe duas listas e testa se a primeira é subsequência
; da segunda.
; (subsequência? l1 l2)
; Por exemplo.
; > (subsequência? '(a b c) '(d z a b c f g))
; Deve retornar:
; #t
; > (subsequência? '(a b c) '(d z a b f c g))
; Deve retornar:
; #f
(define (subsequência? l1 l2)
  (cond
    [(empty? l1) #t]
    [(empty? l2) #f]
    [(equal? (first l1) (first l2)) (prefixo? (rest l1) (rest l2))]
    [else (subsequência? l1 (rest l2))]))

(check-equal? (subsequência? '(a b c) '(d z a b c f g)) #t)
(check-equal? (subsequência? '(a b c) '(d z a b f c g)) #f)


; Questão 13
; Sem usar o comando equal?, escreva a função que recebe duas listas genérica e
; testa se elas são iguais.
; (iguais-lg? lg1 lg2)
; Por exemplo.
; > (iguais-lg? '(a (b c)) '(a (b c)))
; Deve retornar:
; #t
; > (iguais-lg? '(a b c) '(d z a b f c g))
; Deve retornar:
; #f
(define (iguais-lg? lg1 lg2)
  (cond [(and (null? lg1) (null? lg2)) #t]
        [(or (null? lg1) (null? lg2)) #f]
        [(and (symbol? lg1) (symbol? lg2))
              (string=? (symbol->string lg1) (symbol->string lg2))]
        [else (and (iguais-lg? (car lg1) (car lg2))
                  (iguais-lg? (cdr lg1) (cdr lg2)))]
       ))

(check-equal? (iguais-lg? '() '()) #t)
(check-equal? (iguais-lg? '(a) '()) #f)
(check-equal? (iguais-lg? '(a b c) '(a b c)) #t)
(check-equal? (iguais-lg? '(a b c) '(a b c d)) #f)
(check-equal? (iguais-lg? '(a (b c)) '(a (b c))) #t)


; Questão 14
; Escreva a função substitui-lg que substitui todas as ocorrências do átomo old
; por um átomo new em uma estrutura de lista genérica lg.
; (substitui-lg 'c 'manoel '(a (b c)))
; Deve retornar:
; '(a (b manoel))
(define (substitui-lg velho lg novo)
  (cond
    [(empty? lg) empty]
    [(list? (first lg))
     (cons (substitui-lg velho (first lg) novo)
           (substitui-lg velho (rest lg) novo))]
    [(eqv? velho (first lg)) (cons novo (rest lg))]
    [else (cons (first lg) (substitui-lg velho (rest lg) novo))]))

(check-equal? (substitui-lg 'c '(a (b c)) 'manoel) '(a (b manoel)))
(check-equal? (substitui-lg 'c '(a (b (c))) 'manoel) '(a (b (manoel))))


; Questão 15
; Escreva a função aplanar que recebe uma lista genérica LG e produz a lista
; plana L com todos os elementos de LG.
; Por exemplo:
; > (aplanar '(a (b c (d)) (((e)))))
; Deve retornar:
; '(a b c d e)
(define (aplanar lg)
  (cond
    [(empty? lg) empty]
    [(list? (first lg))
     (append (aplanar (first lg)) (aplanar (rest lg)))]
    [else (cons (first lg) (aplanar (rest lg)))]
    ))

(check-equal? (aplanar '(a (b c (d)) (((e))))) '(a b c d e))
