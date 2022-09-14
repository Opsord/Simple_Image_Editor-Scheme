#lang racket

;TDA pixel



;-----------------------------------------------------------------------------------
;Definicion de elementos que tendra el TDA
;Elementos que tiene un pixel (posX posY R G B profundidad)
;Se definen los 3 tipos de pixeles que hay
;-----------------------------------------------------------------------------------
;Constructores individuales

;Constructor del tipo Bit
(define (pixbit-d posX posY bool depth)
  (list "pixbit-d" posX posY bool depth))

;Constructor del tipo RGB
(define (pixrgb-d posX posY R G B depth)
  (list "pixrgb-d" posX posY R G B depth))

;Constructor del tipo Hexadecimal
(define (pixhex-d posX posY HEX depth)
  (list "pixhex-d" posX posY HEX depth))

;-----------------------------------------------------------------------------------
;Pixel de prueba

(define pixbit-prueba(pixbit-d 1 1 0 10))
;-----------------------------------------------------------------------------------
;Pertenencia


(define (pixbit? pixel)
  (and
    (and [eq? "pixbit-d" (car pixel)]
         [and (integer? (cadr pixel)) (integer? (caddr pixel))]
         )
    (and [or (= (cadddr pixel) 0) (= (cadddr pixel) 1)]
         [integer? (car(reverse pixel))]
         )
    ))

        ;[eq? "pixbit-d" (car pixel)]                              ;-> Verifica el tipo
        ;[and (integer? (cadr pixel)) (integer? (caddr pixel))]    ;-> Tiene posicion
        ;[or (= (cadddr pixel) 0) (= (cadddr pixel) 1)]            ;-> Contenido del pixel es 0 o 1

(define (pixrgb? pixel)
  (cond [(eq? "pixrgb-d" (car pixel))#t]
        ;[and (integer? (cadr pixel)) (integer? (caddr pixel))]    ;-> Tiene posicion
        ;[and (>= (cadddr pixel) 0) (<= (cadddr pixel) 255))]      ;-> R entre 0 y 255
        ;[and (>= (caddddr pixel) 0) (<= (caddddr pixel) 255))]    ;-> G entre 0 y 255
        ;[and (>= (cadddddr pixel) 0) (<= (cadddddr pixel) 255))]  ;-> B entre 0 y 255
        ;[integer? (car(reverse(pixel)))]                          ;-> depth es un numero
        [else #f]))

(define (pixhex? pixel)
  (cond [(eq? "pixhex-d" (car pixel))#t]
        ;[and (integer? (cadr pixel)) (integer? (caddr pixel))]    ;-> Tiene posicion
        ;Contenido en hexadecimal
        ;[integer? (car(reverse(pixel)))]                          ;-> depth es un numero
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