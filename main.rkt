#lang racket

;Archivo donde se aplican todos los ejemplos y sript de pruebas

;Importacion de los TDA pixel
(require "TDA-pixbit-d.rkt")
(require "TDA-pixrgb-d.rkt")
(require "TDA-pixhex-d.rkt")
;Importacion de TDA imagen
(require "TDA-Imagen.rkt")

;Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)))

;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))

(define img3 (imgRGB->imgHex img1))

;Imagen de creacion propia para pruebas
(define imagen-prueba(image 3 5
                           (pixrgb-d 0 0 250 251 252 0)
                           (pixrgb-d 0 1 255 160 12 10)
                           (pixrgb-d 0 2 20 20 20 20)
                           (pixrgb-d 0 3 30 30 30 30)
                           (pixrgb-d 0 4 40 40 40 40)
                           (pixrgb-d 1 0 0 0 0 0)
                           (pixrgb-d 1 1 10 10 10 10)
                           (pixrgb-d 1 2 20 20 20 20)
                           (pixrgb-d 1 3 30 30 30 30)
                           (pixrgb-d 1 4 40 40 40 40)
                           (pixrgb-d 2 0 0 0 0 0)
                           (pixrgb-d 2 1 10 10 10 10)
                           (pixrgb-d 2 2 20 20 20 20)
                           (pixrgb-d 2 3 90 90 90 90)
                           (pixrgb-d 2 4 80 80 80 30)))

;Imagen de creacion propia 2 para pruebas
(define imagen-prueba2(image 1 5
                           (pixrgb-d 0 0 250 251 252 0)
                           (pixrgb-d 0 1 10 10 10 10)
                           (pixrgb-d 0 2 20 20 20 20)
                           (pixrgb-d 0 3 30 30 30 30)
                           (pixrgb-d 0 4 40 40 40 40)))

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t
