#lang racket

(require "TDA-pixbit-d.rkt")

; --------- Tipo de Funcion: de pertenecia ------------

#| Documentacion: hex?
Descripcion: Verifica si la entrada es un string
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): hexx (string) 
Recorrido(Retorno): Booleano
|#

(define hex?
  (lambda (hexx)
       (if (string? hexx) true false)))

; --------- Tipo de Funcion: Constructor ------- ::::::::::::::::::::::::::::::::::::::::::::: PIXHEX-D ::::::::::::::::::::::::::::::::::::::::

#| Documentacion: TDA-pixhex-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 4to int>0?, y 3ro hex?. si es asi, crea un pixhex-d
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): x (int) X y (int) X hex (string) X depth (int)
Recorrido(Retorno): pixhex-d '(x y hex depth)
|#

(define pixhex-d
  (lambda (y x hex depth)
    (if (and (int>0? x)
             (int>0? y)
             (hex? hex)
             (int>0? depth))
         (list y x hex depth)
         null)))
;---------------------------------------------------------------------------------------------------------------------------------------------

; --------- Tipo de Funcion: Selector ------------
;Funcion que obtiene el color hex de un pixhex-d. Dom: pixelhex (pixhex-d). Rec: int
(define gethex (lambda (pixelhex)
               (list-ref pixelhex 2)))

; --------- Tipo de Funcion: de pertenecia ------------

#| Documentacion: pixhexs?
Descripcion: Verifica si el elemento de entrada es un pixhex-d
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (pixhex-d)
Recorrido(Retorno): Booleano
|#

(define pixhexs? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))
                       (if (and (equal? (length lista) 4) (hex? (gethex lista)))
                           true false)
                       false)))

; Finalmente se exportan todas las funciones de este archivo para afuera de este, con el fin de ejecutar correctamente el archivo MAIN.rkt

(provide (all-defined-out))