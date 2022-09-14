#lang racket

;TDA pixel
;-----------------------------------------------------------------------------------
;Exportacion de funciones

;Constructores
(provide pixbit-d)
(provide pixrgb-d)
(provide pixhex-d)
;Validadores
(provide pixbit?)
(provide pixrgb?)
(provide pixhex?)

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
;Pertenencia


;Verificador de pertenencia al tipo pixBit
(define (pixbit? pixel)
  (and
    (and [eq? "pixbit-d" (car pixel)]                             ;-> Verifica el tipo
         [and (integer? (cadr pixel)) (integer? (caddr pixel))]   ;-> Verifica posicion
         )
    (and [or (= (cadddr pixel) 0) (= (cadddr pixel) 1)]           ;-> Contenido del pixel es 0 o 1
         [integer? (car(reverse pixel))]                          ;-> Verifica la profundidad
         )
    ))


;Verificador de pertenencia al tipo pixRGB
(define (pixrgb? pixel)
  (and (and 
        (and [eq? "pixrgb-d" (car pixel)]                                             ;-> Verifica el tipo
             [and (integer? (cadr pixel)) (integer? (caddr pixel))]                   ;-> Tiene posicion
             )
        (and [and (>= (cadddr pixel) 0) (<= (cadddr pixel) 255)]                      ;-> R entre 0 y 255
             [and (>= (cadddr (cdr pixel)) 0) (<= (cadddr (cdr pixel)) 255)]          ;-> G entre 0 y 255
             )
        )
       (and [and (>= (cadddr (cdr(cdr pixel))) 0) (<= (cadddr (cdr(cdr pixel))) 255)] ;-> B entre 0 y 255
            [integer? (car(reverse pixel))]                                           ;-> Verifica la profundidad
            )
       ))



;Verificador de pertenencia al tipo pixHEX
(define (pixhex? pixel)
  (and
   (and [eq? "pixhex-d" (car pixel)]                               ;-> Verifica el tipo
        [and (integer? (cadr pixel)) (integer? (caddr pixel))]     ;-> Tiene posicion
        )
   (and [string? (cadddr pixel)]                                   ;-> Contenido en hexadecimal
        [integer? (car(reverse pixel))]                           ;-> depth es un numero
        )
   ))

;-----------------------------------------------------------------------------------
;Pixeles de prueba

(define pixbit-prueba(pixbit-d 1 1 0 10))
(define pixrgb-prueba(pixrgb-d 1 1 0 100 255 10))
(define pixhex-prueba(pixhex-d 1 0 "#FF00(codigo random)" 10))

;Verificaciones de prueba
;(pixbit? pixbit-prueba)
;(pixrgb? pixrgb-prueba)
;(pixhex? pixhex-prueba)

;-----------------------------------------------------------------------------------
;Selector
;-----------------------------------------------------------------------------------
;Modificador
;-----------------------------------------------------------------------------------
;Otras funciones
;-----------------------------------------------------------------------------------