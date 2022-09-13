#lang racket
(require "TDA-Image.rkt")

; TDA de pertenencia

#| Documentacion:  TDA-hexmap?
Descripcion: Verifica si los elementos de la lista de entrada corresponden a pixhex
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define hexmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixhexs? (list-ref lista 2))
                          true
                          false)
                      false)))

(provide (all-defined-out))