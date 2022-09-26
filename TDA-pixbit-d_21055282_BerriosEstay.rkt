#lang racket

; --------- Tipo de Funcion: de pertenecia ------------

; Verifica si la entrada es un entero y si es mayor o igual a 0. Dom: x (int). Rec: Booleano

(define int>0?
  (lambda (x)
       (if (and (integer? x)
            (>= x 0)) true false)))

; --------- Tipo de Funcion: de pertenecia ------------

; Verifica si la entrada es igual a 0 o 1. Dom: bitt (int). Rec: Booleano

(define bit?
  (lambda (bit)
       (if (and (integer? bit)
            (or (= bit 0)(= bit 1))) true false)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene la coordenada y de un pixel. Dom: pixel. Rec: int

(define gety (lambda (pix)
                    (list-ref pix 0)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene la coordenada x de un pixel. Dom: pixel. Rec: int

(define getx (lambda (pix)
                    (list-ref pix 1)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el bit de un pixbit-d. Dom: pixelbit (pixbit-d). Rec: int

(define getbit (lambda (pixbit)
                    (list-ref pixbit 2)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el canal D de un pixel Dom: pixel(pixbit-rgb-hex-d). Rec: int
(define getD (lambda (pixel)
               (list-ref (reverse pixel) 0)))

; --------- Tipo de Funcion: Constructor ------------ ::::::::::::::::::::::::::::::::::::::::::::::::::::: PIXBIT-D :::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: TDA-pixbit-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 4to int>0?, y 3ro bit?. si es asi, crea un pixbit-d
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): x (int) X y (int) X bit (int) X depth (int)
Recorrido(Retorno): pixbit-d 
|#

(define pixbit-d
  (lambda (y x bit depth)
    (if (and (int>0? x)
             (int>0? y)
             (bit? bit)
             (int>0? depth))
         (list y x bit depth)
         null)))

;---------------------------------------------------------------------------------------------------------------------------------------------

; --------- Tipo de Funcion: de pertenecia ------------

; Verifica si el elemento de entrada es un pixbit-d. Dom: pixel (pixbit-d). Rec: Booleano


(define pixbits? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))                       
                       (if (and (equal? (length lista) 4) (bit? (getbit lista)))
                           true false)
                       false )))

; Finalmente se exportan todas las funciones de este archivo para afuera de este, con el fin de ejecutar correctamente el archivo MAIN.rkt

(provide (all-defined-out))

