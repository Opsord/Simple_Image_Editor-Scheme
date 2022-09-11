#lang racket

;------------------------------------------------------------------------------------------------------------------------------------------
;Multiplicar 2 numeros
(define multiplicar (lambda (A B);   Basicamente la estructura es -> (define [nombreFuncion] (lambda ([arumentos]) 
                      (* A B)));                                                 ([operacion de argumentos])
;------------------------------------------------------------------------------------------------------------------------------------------
;Multiplicar 2 numeros pero curriicado
(define multiplicarCurr (lambda (A); Solo se entrega un argumento a la funcion
                          (lambda (B) (* A B)))); y la operacion a realizar es otra funcion
;------------------------------------------------------------------------------------------------------------------------------------------
;Aplicacion de recurcion N°1 - Factorial - Recurcion lineal, ya que deja estados pendientes
(define factorial (lambda (N)
                    ;se define el caso base
                    (if (= N 0); -> condicion de borde
                        1      ; -> solucion conocida
                        (* N (factorial (- N 1)))))) ; -> se aplica la funcion original con modificacion recursiva
;------------------------------------------------------------------------------------------------------------------------------------------
;Factorial - Recurcion de cola
(define factorialCola (lambda (N resultado); Se le entrega un N y una solucion conocida, es decir, "resutado" = 1
                        (if (= N 0);   -> condicion de borde
                            resultado; -> caso base/solucion conocida
                            (factorialCola (- N 1) (* N resultado))))); -> se va acumulando el resultado parcial sin dejarlo pendiente
;------------------------------------------------------------------------------------------------------------------------------------------
;Factorial - Recurcion de cola - Encapsulada
(define factorialCapsula (lambda (N); Se define una funcion que acapare toda la funcion que se encapsula a la cual
                           ;se le entrea solo 1 elemento
                           (define factorialCola (lambda (N resultado)
                                                   (if (= N 0)
                                                       resultado
                                                       (factorialCola (- N 1) (* N resultado)))))
                           ;Finalmente se hace que se evalue la funcion implementada con la solucion conocida por el programador
                           ;pero no por el usuario, creando el concepto de "caja negra"
                           (factorialCola N 1))); -> Evaluacion de funcion con solucion conocida
;------------------------------------------------------------------------------------------------------------------------------------------
;Agregar un elemento al final de una lista
(define (agregarFinal elemento lista)
  (reverse (cons elemento (reverse lista))))
;------------------------------------------------------------------------------------------------------------------------------------------
;Calcular el largo de una lista - Recursion natural
;(define (largoListaRN lista)
;  (if (null? (cdr lista)
             
  

;------------------------------------------------------------------------------------------------------------------------------------------
;Calcular el largo de una lista - Recursion de cola
(define (largoListaRC lista)
  (define (funcionInterior lista acumulador)
    (if (null? (cdr lista))
        acumulador
        (funcionInterior (cdr lista) (+ 1 acumulador))))
  (funcionInterior lista 1))
;------------------------------------------------------------------------------------------------------------------------------------------
