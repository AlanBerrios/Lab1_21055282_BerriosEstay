#lang racket

(require "TDA-Image.rkt")

#| Documentacion: TDA-edit
Descripcion: Funcion que permite aplicar funciones especiales a las imágenes. 
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): funcion (procedure) X (imagen (image)
Recorrido(Retorno): image
|#

(define edit (lambda (funcion imagen)
               ;Funcion que verifica si la imagen es compatible con la funcion. Dom: funcion (procedure) X imagen (image)
               (define compatible? (lambda (funcion imagen)
                                     (if (not(boolean? (funcion imagen)))
                                         true
                                         false)))
               
               ; Si la funcion es compatible, aplica la funcion.
               (if (compatible? funcion imagen)               
                   (funcion imagen)               
                   null)))

(provide (all-defined-out))