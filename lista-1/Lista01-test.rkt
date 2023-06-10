#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "Lista01.rkt")

(define teste-da-lista
  (test-suite
   "Testes Lista 01"

   (test-case "Teste Questão 1: concatenar1"
             (check-equal? (concatenar1 '(a b c) '(d e f g h)) '(a b c d e f g h)))

   (test-case "Teste Questão 2: concatenarInv"
             (check-equal? (concatenarInv '(a b c) '(d e f g h)) '(d e f g h a b c)))

   (test-case "Teste Questão 3: concatenar2"
             (check-equal? (concatenar2 '((a b) (c) (d e f g))) '(a b c d e f g)))

   (test-case "Teste Questão 4: concatenar3"
             (check-equal? (concatenar3 '(a b) '(c) '(d e f g)) '(a b c d e f g) "Not equal"))

   (test-case "Teste Questão 5: juntar"
             (check-equal? (juntar '(a b c) '(d e f g h)) '(a d b e c f g h)))

   (test-case "Teste Questão 6: intercala"
             (check-equal? (intercala 5 'x 'y) '(x y x y x)))

   (test-case "Teste Questão 7: intercala2"
             (check-equal? (intercala2 5 'x 'y 'z) '(x y z x y))
             (check-equal? (intercala2 5 'x 'y 'z 'w) '(x y z w x)))

   (test-case "Teste Questão 8: parear"
             (check-equal? (parear 'x '(a b c)) '((x a) (x b) (x c))))

   (test-case "Teste Questão 9: pares"
             (check-equal? (pares '(a b c d)) '((a b) (a c) (a d) (b c) (b d) (c d))))

   (test-case "Teste Questão 10: conjunto?"
             (check-equal? (conjunto? '(a b c d)) #t)
             (check-equal? (conjunto? '(a b c d b)) #f))

   (test-case "Teste Questão 11: prefixo?"
             (check-equal? (prefixo? '(a b c) '(a b c d e f g)) #t)
             (check-equal? (prefixo? '(a b c) '(a b f g)) #f))

   (test-case "Teste Questão 12: subsequência?"
             (check-equal? (subsequência? '(a b c) '(d z a b c f g)) #t)
             (check-equal? (subsequência? '(a b c) '(d z a b f c g)) #f))

   (test-case "Teste Questão 13: iguais-lg?"
             (check-equal? (iguais-lg? '() '()) #t)
             (check-equal? (iguais-lg? '(a) '()) #f)
             (check-equal? (iguais-lg? '(a b c) '(a b c)) #t)
             (check-equal? (iguais-lg? '(a b c) '(a b c d)) #f)
             (check-equal? (iguais-lg? '(a (b c)) '(a (b c))) #t)
             (check-equal? (iguais-lg? '(a b c) '(d z a b f c g)) #f))

   (test-case "Teste Questão 14: substitui-lg"
             (check-equal? (substitui-lg 'c '(a (b c)) 'manoel) '(a (b manoel)))
             (check-equal? (substitui-lg 'c '(a (b (c))) 'manoel) '(a (b (manoel)))))

   (test-case "Teste Questão 15: aplanar"
             (check-equal? (aplanar '(a (b c (d)) (((e))))) '(a b c d e)))
   )
  )

(run-tests teste-da-lista)
