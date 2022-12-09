#lang racket

;TDA imagen

;Importacion de los TDA pixel
(require "ADT-PixBIT.rkt")
(require "ADT-PixRGB.rkt")
(require "ADT-PixHEX.rkt")
;Eportacion para rchivo main.rkt
(provide(all-defined-out))

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Constructor diamico de imagen

;; Descripción: Constructor dinamico de una imagen
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: numero X numero X n-pieles
;; Rec: imagen
(define (image largo ancho . pixeles)
  (if (= (* largo ancho) (length pixeles))
      pixeles
      (display "Dimensiones no corresponden con pixeles entregados")))
;El ". pixel" hace que la lsita sea dinamica

;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Pertenencia

;; Descripción: Verifica que todos los pixeles de una imagen sean del tipo BIT
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: imagen
;; Rec: booleano
;bitmap?
(define (bitmap? image)
  (andmap pixbit? image))

;; Descripción: Verifica que todos los pixeles de una imagen sean del tipo RGB
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: imagen
;; Rec: booleano
;pixmap?
(define (pixmap? image)
  (andmap pixrgb? image))

;; Descripción: Verifica que todos los pixeles de una imagen sean del tipo hexadecimal
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: imagen
;; Rec: booleano
(define (hexmap? image)
  (andmap pixhex? image))

;; Descripción: Comprueba si existe el string "compressed" en una imagen
;;              dado que en las imagenes comprimidas se encuentra presente
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: imagen
;; Rec: booleano
;compressed?
(define (compressed? image)
  (if (member "compressed" image)
      #t
      #f))
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Selector

;; Descripción: Se usa para obtener el contenido de un pixel, sin contar el parametro
;;              de profundidad
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: NA
;; Dom: pixel
;; Rec: numero X numero X numero // numero // Hexadecimal
(define(get-cont pixel)
  (reverse(cdr(reverse(cdddr pixel)))))

;; Descripción: Cuenta el numero de filas que tiene una imagen
;;              utilizando un acomulador
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: entero
(define (contarH image)
  (define (cont-inner image contador)
    (if (null? (cdr image))
        contador
        (if (eq? (cadr(car image)) (cadr(cadr image)))
            (cont-inner (cdr image) contador)
            (cont-inner (cdr image) (+ 1 contador)))))
  (cont-inner image 1))

;; Descripción: Cuenta el numero de columnas que tiene una imagen
;;              utilizando un acomulador
;; Tipo de algoritmo/estrategia: NA
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: entero
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
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Modificador

;; Descripción: Da vuelta la imagen de forma horizontal
;; Tipo de algoritmo/estrategia: Se crean pixees nuevos de forma recursiva con la posicion Y invertida
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: imagen
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
        [(hexmap? image) (flipH-inner image (list(ConstrPixHEX image (contarV image))) (contarV image))] ))


;; Descripción: Da vuelta la imagen de forma vertical
;; Tipo de algoritmo/estrategia: Se crean pixees nuevos de forma recursiva con la posicion X invertida
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: imagen
(define (flipV image)
  ;Constructor encapsulado de un pixBIT con el posX invertido
  (define(ConstrPixBIT image totalFilas)
    (pixbit-d
     [- (- totalFilas 1) (cadr(car image))]         ;-> posX
     [caddr(car image)]                             ;-> posY
     [car(cdddr(car image))]                        ;-> bool
     [car(reverse(car image))]))                    ;-> depth
  ;Constructor encapsulado de un pixRGB con el posX invertido
  (define(ConstrPixRGB image totalFilas)
    (pixrgb-d
     [- (- totalFilas 1) (cadr(car image))]         ;-> posX
     [caddr(car image)]                             ;-> posY
     [car(cdddr(car image))]                        ;-> R
     [cadr(cdddr(car image))]                       ;-> G
     [caddr(cdddr(car image))]                      ;-> B
     [car(reverse(car image))]))                    ;-> depth
  ;Constructor encapsulado de un pixHEX con el posX invertido
  (define(ConstrPixHEX image totalFilas)
    (pixhex-d
     [- (- totalFilas 1) (cadr(car image))]         ;-> posX
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
  ;Llamado con solucion conocida (para cada caso)
  (cond [(bitmap? image) (flipV-inner image (list(ConstrPixBIT image (contarH image))) (contarH image))]
        [(pixmap? image) (flipV-inner image (list(ConstrPixRGB image (contarH image))) (contarH image))]
        [(hexmap? image) (flipV-inner image (list(ConstrPixHEX image (contarH image))) (contarH image))] ))


;; Descripción: Genera una imagen nueva con los pixeles que cumplan las condiciones de borde
;;              revisandolos de forma recursiva
;; Tipo de algoritmo/estrategia: Se crea una imagen nueva de forma recursiva
;; Tipo de recursión: Cola
;; Dom: imagen X numero X numero X numero X numero
;; Rec: imagen
(define(crop image x1 y1 x2 y2)
  ;Funcion interna encapsulada
  (define(crop-inner image minX maxX minY maxY newImage)
    (if (null? (cdr image))
        (if (and (and (<= minX (cadr (car image))) (>= maxX (cadr (car image))))
                 (and (<= minY (caddr (car image))) (>= maxY (caddr (car image)))))
            (cons (car image) newImage)
            newImage)
        (crop-inner (cdr image) minX maxX minY maxY (if (and (and (<= minX (cadr(car image))) (>= maxX (cadr(car image))))
                                                             (and (<= minY (caddr(car image))) (>= maxY (caddr(car image)))))
                                                        (cons (car image) newImage)
                                                        newImage))))
  ;Llamado con solucion conocida
  (cond
    [(and (>= x1 x2) (>= y1 y2)) (crop-inner image x2 x1 y2 y1 null)]
    [(and (>= x1 x2) (<= y1 y2)) (crop-inner image x2 x1 y1 y2 null)]
    [(and (<= x1 x2) (>= y1 y2)) (crop-inner image x1 x2 y2 y1 null)]
    [(and (<= x1 x2) (<= y1 y2)) (crop-inner image x1 x2 y1 y2 null)]))


;; Descripción: Cambia el tipo de una imagen de RGB a hexadecimal
;; Tipo de algoritmo/estrategia: Se pasa cada pixel de la imagen original por un conversor para
;;                               generar una imagen nueva de forma recursiva
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: imagen
(define(imgRGB->imgHex image)
  ;Parte entera
  (define(valor-int numero)
    (quotient numero 16))
  ;Parte decimal
  (define(valor-dec numero)
    (* (- (/ numero 16) (valor-int numero)) 16))
  ;Generador de hexadecimal
  (define(genHex numero)
    (string (string-ref "0123456789ABCDEF" (valor-int numero)) (string-ref "0123456789ABCDEF" (valor-dec numero))))
  ;Conversor de pixel invididual
  (define (pixRGB->pixHEX pixel)
    (pixhex-d (cadr pixel)
              (caddr pixel)
              (string-append (genHex (car(cdddr pixel))) (genHex (cadr(cdddr pixel))) (genHex (caddr(cdddr pixel))))
              (car (reverse pixel))))  
  ;Constructor de la funcion
  (map pixRGB->pixHEX image))


;; Descripción: Cuenta la frecuencia con la que se repite el contenido de un pixel
;; Tipo de algoritmo/estrategia: Se aislan los contneidos de los pixeles de una imagen
;;                               para luego contar sus apariciones en la misma.
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: contenidoPixel X numero
(define (histogram image)
  ;Sintatizador de pixeles de una imagen
  (define(unificador-pix image lista-pix)
    (if (null? (cdr image))
        (if (null? (car image))
            lista-pix
            (if (member (get-cont(car image)) lista-pix)
                lista-pix
                (cons (get-cont(car image)) lista-pix)))
        (if (member (get-cont(car image)) lista-pix)
            (unificador-pix (cdr image) lista-pix)
            (unificador-pix (cdr image) (cons (get-cont(car image)) lista-pix)))))
  ;Contador de ocurrencias de 1 pixel en una imagen
  (define(contador-pixel image cont-pixel contador)
    (if (null? (cdr image))
        (if (null? (car image))
            (cons cont-pixel contador)
            (if (equal? (get-cont(car image)) cont-pixel)
                (cons cont-pixel (+ 1 contador))
                (cons cont-pixel contador)))
        (if (equal? (get-cont(car image)) cont-pixel)
            (contador-pixel (cdr image) cont-pixel (+ 1 contador))
            (contador-pixel (cdr image) cont-pixel contador))))
  ;Funcion interna del histograma
  (define (hist-inner image pix-unificados histograma)
    (if (null? (cdr pix-unificados))
        (if (null? (car pix-unificados))
            histograma
            (cons (contador-pixel image (car pix-unificados) 0) histograma))
        (hist-inner image (cdr pix-unificados) (cons (contador-pixel image (car pix-unificados) 0) histograma))))
  ;Llamado a funcion con solucion conocidas
  (hist-inner image (unificador-pix image null) null))


;; Descripción: Rota una imagen en 90° mediante el metodo cartesiano
;; Tipo de algoritmo/estrategia: Primero se invierten las coordenadas de cada pixel y se le cambia el signo a
;;                               la coordenada Y, luego se traslada verticalmente el equivalente a la coordenada
;;                               que este mas abajo (con menor Y)
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: imagen
(define (rotate90 image)
  ;Constructor encapsulado de un pixBIT rotado cartesianamente
  (define(const-bit pixel)
    (pixbit-d
     [caddr pixel]                                  ;-> posX
     [*(cadr pixel) -1]                             ;-> posY
     [cadr (reverse pixel)]                         ;-> bool
     [car (reverse pixel)]))                        ;-> depth
  ;Constructor encapsulado de un pixRGB rotado cartesianamente
  (define(const-pix pixel)
    (pixrgb-d
     [caddr pixel]                                  ;-> posX
     [*(cadr pixel) -1]                             ;-> posY
     [cadddr pixel]                                 ;-> R
     [caddr (reverse pixel)]                        ;-> G
     [cadr (reverse pixel)]                         ;-> B
     [car (reverse pixel)]))                        ;-> depth
  ;Constructor encapsulado de un pixHEX rotado cartesianamente
  (define(const-hex pixel)
    (pixhex-d
     [caddr pixel]                                  ;-> posX
     [*(cadr pixel) -1]                             ;-> posY
     [cadr (reverse pixel)]                         ;-> hexadecimal
     [car (reverse pixel)]))                        ;-> depth 
  ;Encontrar el menor valor despues de convertir
  (define (menor-valor image contador)
    (if (null? (cdr image))
        (if (null? (car image))
            contador
            (if (< (caddr(car image)) contador)
                (caddr(car image))
                contador))
        (menor-valor (cdr image) (if (< (caddr(car image)) contador)
                                     (caddr(car image)) contador))))
  ;Constructor 2 pixbit para imagen rotada
  (define(const2-bit pixel traslador)
    (pixbit-d
     [caddr pixel]                                  ;-> posX
     [+ (cadr pixel) traslador]                     ;-> posY
     [cadr (reverse pixel)]                         ;-> bool
     [car (reverse pixel)]))                        ;-> depth
  ;Constructor 2 pixrgb para imagen rotada
  (define(const2-pix pixel traslador)
    (pixrgb-d
     [cadr pixel]                                   ;-> posX
     [+ (caddr pixel) traslador]                    ;-> posY
     [cadddr pixel]                                 ;-> R
     [caddr (reverse pixel)]                        ;-> G
     [cadr (reverse pixel)]                         ;-> B
     [car (reverse pixel)]))                        ;-> depth
  ;Constructor 2 pixhex para imagen rotada
  (define(const2-hex pixel traslador)
    (pixhex-d
     [caddr pixel]                                  ;-> posX
     [+ (cadr pixel) traslador]                     ;-> posY
     [cadr (reverse pixel)]                         ;-> hexadecimal
     [car (reverse pixel)]))                        ;-> depth
  ;Funcion interna 
  (define (rotate90-inner image traslador newImage)
    (if (null? (cdr image))
        (if (null? (car image))
            newImage
            (cond
              [(bitmap? image) (cons (const2-bit (car image) traslador) newImage)]
              [(pixmap? image) (cons (const2-pix (car image) traslador) newImage)]
              [(hexmap? image) (cons (const2-hex (car image) traslador) newImage)]))
        (rotate90-inner (cdr image) traslador (cond
                                                [(bitmap? image) (cons (const2-bit (car image) traslador) newImage)]
                                                [(pixmap? image) (cons (const2-pix (car image) traslador) newImage)]
                                                [(hexmap? image) (cons (const2-hex (car image) traslador) newImage)]))))
  ;Llamada con solucion conocida
  (rotate90-inner (cond
                    [(bitmap? image) (map const-bit image)]
                    [(pixmap? image) (map const-pix image)]
                    [(hexmap? image) (map const-hex image)]) (* (menor-valor (cond
                                                                               [(bitmap? image) (map const-bit image)]
                                                                               [(pixmap? image) (map const-pix image)]
                                                                               [(hexmap? image) (map const-hex image)]) 0) -1) null))


;----------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Otras funciones
;----------------------------------------------------------------------------------------------------------------------------------------------------------------------


;; Descripción: Se comprime una imagen
;; Tipo de algoritmo/estrategia: Se encuentra el pixel con mas frecuencia en terminos de contenido
;;                               para luego generar una imagen nueva sin los pixeles con el mismo contenido
;;                               pero agregando informacion extra para su posterior descompresion
;; Tipo de recursión: Cola
;; Dom: imagen
;; Rec: imagenComprimida
(define (compress image)
  ;Funcion para obtener el pixel mas repetido en la imagen
  (define(get-max histograma max)
    (if (null? (cdr histograma))
        (if (> (cdr(car histograma)) (cdr max))
            histograma
            max)
        (get-max (cdr histograma) (if (> (cdr (car histograma)) (cdr max))
                                      (car histograma)
                                      max))))
  ;Funcion interna de compress para BIT y HEX
  (define (compress-BIT-HEX image pixelAbundante compressedImage)
    (if (null? (cdr image))
        (if (and (eq? (car(cdddr(car image))) (car(car pixelAbundante))))
            (reverse compressedImage)
            (cons image compressedImage))
        (compress-BIT-HEX (cdr image) pixelAbundante (if (and (eq? (car(cdddr(car image))) (car(car pixelAbundante))))
                                                         compressedImage
                                                         (cons (car image) compressedImage)))))
  ;Funcion interna de compress para RGB
  (define (compress-RGB image pixelAbundante compressedImage)
    (if (null? (cdr image))
        (if (and
             (and (eq? (car(cdddr(car image))) (car(car pixelAbundante)))
                  (= (cadr(cdddr(car image))) (cadr(car pixelAbundante))))
             (= (caddr(cdddr(car image))) (caddr(car pixelAbundante))))
            (reverse compressedImage)
            (cons image compressedImage))
        (compress-RGB (cdr image) pixelAbundante (if (and
                                                      (and
                                                       (= (car(cdddr(car image))) (car(car pixelAbundante)))
                                                       (= (cadr(cdddr(car image))) (cadr(car pixelAbundante))))
                                                      (= (caddr(cdddr(car image))) (caddr(car pixelAbundante))))
                                                     compressedImage
                                                     (cons (car image) compressedImage)))))
  ;Llamado a funcion con solucion conocida
  (cond [(pixmap? image) (compress-RGB image (get-max (histogram image) (cons 0 0)) (list "compressed" (length image) (get-max (histogram image) (cons 0 0))))]
        [else (compress-BIT-HEX image (get-max (histogram image) (cons 0 0)) (list "compressed" (length image) (get-max (histogram image) (cons 0 0))))]))
