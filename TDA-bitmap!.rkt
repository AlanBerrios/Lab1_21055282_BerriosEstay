#lang racket
(require "TDA-Image.rkt")

; TDA de pertenencia

#| Documentacion:  TDA-bitmap?
Descripcion: Verifica si los elementos de la lista de entrada corresponden a pixbit
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#
  
(define bitmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixbits? (list-ref lista 2))
                          true
                          false)
                      false)))

(provide (all-defined-out))