#lang racket

(require "TDA-Image.rkt")

#| Documentacion:  TDA-compressed?
Descripcion: Verifica si la imagen de entrada esta comprimida
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define compressed? (lambda (lista)
                      (if (and (list? lista)(not(null? lista)))
                          (if (> (*(list-ref lista 0)(list-ref lista 1)) (length (list-ref lista 2)))
                              true
                              false)
                          null)))

(provide (all-defined-out))