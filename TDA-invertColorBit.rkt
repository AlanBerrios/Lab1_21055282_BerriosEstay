#lang racket

(require "TDA-Image.rkt")

#| Documentacion: TDA-invertColorBit
Descripcion: Funcion que invierte los bits de cada pixel de una imagen bitmap
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagenbit (image) 
Recorrido(Retorno): image 
|#

(define invertColorBit (lambda (imagenbit)
                         (if (bitmap? imagenbit)
                             (list (list-ref imagenbit 0)(list-ref imagenbit 1)
                                   (map (lambda (p) (list (list-ref p 0)(list-ref p 1)(abs(- (list-ref p 2) 1))(list-ref p 3))) (list-ref imagenbit 2)))
                             null))

(provide (all-defined-out))