#lang racket
(require "TDA-Image.rkt")

; TDA de pertenencia

#| Documentacion:  TDA-pixmap?
Descripcion: Verifica si los elementos de la lista de entrada corresponden a pixrgb
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define pixmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixrgbs? (list-ref lista 2))
                          true
                          false)
                      false)))

(provide (all-defined-out))