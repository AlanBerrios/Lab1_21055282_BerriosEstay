#lang racket

(require "TDA-pixbit-d_21055282_BerriosEstay.rkt")

; --------- Tipo de Funcion: de pertenecia ------------

#| Documentacion: rgb?
Descripcion: Verifica si la entrada es un entero mayor o igual a 0 y menor o igual a 255
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): C (int) 
Recorrido(Retorno): Booleano
|#

(define rgb?
  (lambda (C)
       (if (and (integer? C)
            (>= C 0)
            (<= C 255)) true false)))

; --------- Tipo de Funcion: Constructor ------------ ::::::::::::::::::::::::::::::::::::::::::::::: PIXRGB-D ::::::::::::::::::::::::::::::::::::::

#| Documentacion: TDA-pixrgb-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 6to int>0?, y 3ro, 4to y 5to rgb?. si es asi, crea un pixrgb-d
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): x (int) X y (int) X r (int) X g (int) X b (int) X depth (int)
Recorrido(Retorno): pixrgb-d '(x y r g b depth)
|#

(define pixrgb-d
  (lambda (y x r g b depth)
    (if (and (int>0? x)
             (int>0? y)
             (rgb? r)
             (rgb? g)
             (rgb? b)
             (int>0? depth))
         (list y x r g b depth)
         null)))

;---------------------------------------------------------------------------------------------------------------------------------------------

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el canal R de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getR (lambda (pixelrgb)
               (list-ref pixelrgb 2)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el canal G de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getG (lambda (pixelrgb)
               (list-ref pixelrgb 3)))

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el canal B de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getB (lambda (pixelrgb)
               (list-ref pixelrgb 4)))

; --------- Tipo de TDA: modificador ------------
;Funcion modificador del canal R. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setR (lambda (R)
               (if (= R 255)
                    R
                    (+ R 1))))

; --------- Tipo de Funcion: Modificador ------------
;Funcion modificador del canal G. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setG (lambda (G)
               (if (= G 255)
                    G
                    (+ G 1))))

; --------- Tipo de Funcion: Modificador ------------
;Funcion modificador del canal B. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setB (lambda (B)
               (if (= B 255)
                    B
                    (+ B 1))))

; --------- Tipo de Funcion: Modificador ------------
;Funcion modificador del canal D. Dom: D (int). Rec: int

(define setD (lambda (D)
                    (+ D 1)))

; --------- Tipo de Funcion: de pertenecia ------------

#| Documentacion: pixrgbs?
Descripcion: Verifica si el elemento de entrada es un pixrgb-d
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (pixrgb-d)
Recorrido(Retorno): Booleano
|#

(define pixrgbs? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))                     
                       (if (and (equal? (length lista) 6)
                                (rgb? (getR lista))
                                (rgb? (getG lista))
                                (rgb? (getB lista)))                                                                      
                           true false)
                       false )))

(provide (all-defined-out)); Finalmente se exportan todas las funciones de este archivo para afuera de este, con el fin de ejecutar correctamente el archivo MAIN.rkt

(provide (all-defined-out))