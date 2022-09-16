#lang racket

;TDA imagen

;Importacion de TDA pixel
(require "TDA-Pixel.rkt")

;-----------------------------------------------------------------------------------
;Constructor diamico de imagen

;; Descripción: Constructor dinamico de una imagen
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: numero X numero X n-pieles
;; Rec: imagen
(define (image largo ancho . pixeles) pixeles)
;El ". pixel" hace que la lsita sea dinamica

;-----------------------------------------------------------------------------------

;Definicion de "imagen-prueba" como una imagen de prueba
(define imagen-prueba(image 2 2
                           (pixrgb-d 0 0 0 0 0 0)
                           (pixrgb-d 0 1 10 10 10 10)
                           (pixrgb-d 0 2 20 20 20 20)
                           (pixrgb-d 0 3 30 30 30 30)
                           (pixrgb-d 0 4 40 40 40 40)
                           (pixrgb-d 1 0 0 0 0 0)
                           (pixrgb-d 1 1 10 10 10 10)
                           (pixrgb-d 1 2 20 20 20 20)
                           (pixrgb-d 1 3 30 30 30 30)
                           (pixrgb-d 1 4 40 40 40 40)))
;-----------------------------------------------------------------------------------
;Pertenencia

;bitmap?
(define (bitmap? image)
  (define (bitmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (bitmap-inner (cdr image) (and (pixbit? (car image)) acumulador))))
  (bitmap-inner image #t))

;pixmap?
(define (pixmap? image)
  (define (pixmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (pixmap-inner (cdr image) (and (pixrgb? (car image)) acumulador))))
  (pixmap-inner image #t))

;hexmap?
(define (hexmap? image)
  (define (hexmap-inner image acumulador)
    (if (null? (cdr image))
        acumulador
        (hexmap-inner (cdr image) (and (pixhex? (car image)) acumulador))))
  (hexmap-inner image #t))

;-----------------------------------------------------------------------------------
;Selector

;contar-filas
(define (contarH image)
  (define (cont-inner image contador)
    (if (null? (cdr image))
        contador
        (if (eq? (cadr(car image)) (cadr(cadr image)))
            (cont-inner (cdr image) contador)
            (cont-inner (cdr image) (+ 1 contador)))))
  (cont-inner image 1))

;contar-columnas
(define (contarV image)
  (define (cont-inner image acumulador)
    (if (null? (cdr image))
        (+ 1 acumulador)
        (if (= (caddr(car image)) (caddr(cadr image)))
            (cont-inner (cdr image) acumulador)
            (if (> (caddr(car image)) acumulador)
                (cont-inner (cdr image) (caddr (car image)))
                (cont-inner (cdr image) acumulador)))))
  (cont-inner image 0))
;-----------------------------------------------------------------------------------
;Modificador

;flipH
(define (flipH image)
  ;Constructor encapsulado de un pixBIT con el posY invertido
  (define(ConstrPixBIT image totalColumnas)
    (pixbit-d
     [cadr(car image)]                              ;-> posX
     [- (- totalColumnas 1) (caddr(car image))]     ;-> posY
     [car(cdddr(car image))]                        ;-> bool
     [car(reverse(car image))]))                    ;-> depth
  
  ;Constructor encapsulado de un pixRGB con el posY invertido
  (define(ConstrPixRGB image totalColumnas)
    (pixrgb-d
     [cadr(car image)]                              ;-> posX
     [- (- totalColumnas 1) (caddr(car image))]     ;-> posY
     [car(cdddr(car image))]                        ;-> R
     [cadr(cdddr(car image))]                       ;-> G
     [caddr(cdddr(car image))]                      ;-> B
     [car(reverse(car image))]))                    ;-> depth

  ;Constructor encapsulado de un pixHEX con el posY invertido
  (define(ConstrPixHEX image totalColumnas)
    (pixhex-d
     [cadr(car image)]                              ;-> posX
     [- (- totalColumnas 1) (caddr(car image))]     ;-> posY
     [car(cdddr(car image))]                        ;-> hexadecimal
     [car(reverse(car image))]))                    ;-> depth
  
  ;Funcion interna encapsulada
  (define (flipH-inner image newImage auxColumnas)
    (if (null? (cdr image))
        (reverse newImage)
        (cond [(bitmap? image) (flipH-inner (cdr image) (cons (ConstrPixBIT (cdr image) auxColumnas) newImage) auxColumnas)]
              [(pixmap? image) (flipH-inner (cdr image) (cons (ConstrPixRGB (cdr image) auxColumnas) newImage) auxColumnas)]
              [(hexmap? image) (flipH-inner (cdr image) (cons (ConstrPixHEX (cdr image) auxColumnas) newImage) auxColumnas)] )))
  
  ;Llamado con solucion conocida
  (cond [(bitmap? image) (flipH-inner image (list(ConstrPixBIT image (contarV image))) (contarV image))]
        [(pixmap? image) (flipH-inner image (list(ConstrPixRGB image (contarV image))) (contarV image))]
        [(hexmap? image) (flipH-inner image (list(ConstrPixHEX image (contarV image))) (contarV image))]))


;flipV
(define (flipV image)
  ;Constructor encapsulado de un pixBIT con el posY invertido
  (define(ConstrPixBIT image totalFilas)
    (pixbit-d
     [- (- totalFilas 1) (cadr(car image))]               ;-> posX
     [caddr(car image)]                             ;-> posY
     [car(cdddr(car image))]                        ;-> bool
     [car(reverse(car image))]))                    ;-> depth
  
  ;Constructor encapsulado de un pixRGB con el posY invertido
  (define(ConstrPixRGB image totalFilas)
    (pixrgb-d
     [- (- totalFilas 1) (cadr(car image))]               ;-> posX
     [caddr(car image)]                             ;-> posY
     [car(cdddr(car image))]                        ;-> R
     [cadr(cdddr(car image))]                       ;-> G
     [caddr(cdddr(car image))]                      ;-> B
     [car(reverse(car image))]))                    ;-> depth

  ;Constructor encapsulado de un pixHEX con el posY invertido
  (define(ConstrPixHEX image totalFilas)
    (pixhex-d
     [- (- totalFilas 1) (cadr(car image))]               ;-> posX
     [caddr(car image)]                             ;-> posY
     [car(cdddr(car image))]                        ;-> hexadecimal
     [car(reverse(car image))]))                    ;-> depth
  
  ;Funcion interna encapsulada
  (define (flipV-inner image newImage auxFilas)
    (if (null? (cdr image))
        (reverse newImage)
        (cond [(bitmap? image) (flipV-inner (cdr image) (cons (ConstrPixBIT (cdr image) auxFilas) newImage) auxFilas)]
              [(pixmap? image) (flipV-inner (cdr image) (cons (ConstrPixRGB (cdr image) auxFilas) newImage) auxFilas)]
              [(hexmap? image) (flipV-inner (cdr image) (cons (ConstrPixHEX (cdr image) auxFilas) newImage) auxFilas)] )))
  
  ;Llamado con solucion conocida
  (cond [(bitmap? image) (flipV-inner image (list(ConstrPixBIT image (contarH image))) (contarH image))]
        [(pixmap? image) (flipV-inner image (list(ConstrPixRGB image (contarH image))) (contarH image))]
        [(hexmap? image) (flipV-inner image (list(ConstrPixHEX image (contarH image))) (contarH image))]))


;-----------------------------------------------------------------------------------
;Otras funciones
;-----------------------------------------------------------------------------------