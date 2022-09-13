#lang racket

;TDA pixel



;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene un pixel (posX posY R G B profundidad)
;Se definen los 3 tipos de pixeles que hay
;-----------------------------------------------------------------------------------
;Constructores

;Constructor general de un pixel

;Decidir si especializar u ocupar uno general
;Verificar si "depth" forma parte de los argumentos
(define (pixel tipo posX posY lst-args depth)
  (list tipo posX posY lst-args depth))

;-----------------------------------------------------------------------------------
;Constructores individuales (desechado)

;Constructor del tipo Bit
;(define (pixbit-d posX posY bool depth)
;  (list posX posY bool depth))

;Constructor del tipo RGB
;R G B tiene que ser entre 0 y 255 (implementar en pertenencia)
;(define (pixrgb-d posX posY R G B depth)
;  (list posX posY R G B depth))

;Constructor del tipo Hexadecimal
;(define (pixhex-d posX posY HEX depth)
;  (list posX posY HEX depth))

;-----------------------------------------------------------------------------------
;Pertenencia
;(pertnencia basica, falta completar)

(define (pixbit? pixel)
  (cond [(eq? "pixbit-d" (car pixel))#t]
        ;Tiene posicion
        ;Contenido del pixel es 0 o 1
        ;depth es un numero
        [else #f]))

(define (pixRGB? pixel)
  (cond [(eq? "pixrgb-d" (car pixel))#t]
        ;Tiene posicion
        ;R entre 0 y 255
        ;G entre 0 y 255
        ;B entre 0 y 255
        ;depth es un numero
        [else #f]))

(define (pixHEX? pixel)
  (cond [(eq? "pixhex-d" (car pixel))#t]
        ;Tiene posicion
        ;Contenido en hexadecimal
        ;depth es un  numero
        [else #f]))

;-----------------------------------------------------------------------------------
;Selector

;Obtener posicion horizontal
(define (getPos-hor pixel)
  (car (cdr pixel)))

;Obtener posicion vertical
(define (getPos-Vert pixel)
  (car (cddr pixel)))

;Obtener coordenada posicion
(define (getCord pixel)
  (cons (car (cdr pixel)) (car (cddr pixel))))


;Decidir si depth es parte del contenido del pixel

;Obtener contenido del pixel

;Obtener profundidad del pixel

;-----------------------------------------------------------------------------------
;Modificador

;-----------------------------------------------------------------------------------

;Otras funciones