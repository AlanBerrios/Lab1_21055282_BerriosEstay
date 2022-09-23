#lang racket

(require "TDA-Image.rkt")

#| Documentacion: TDA-invertColorRGB
Descripcion: Funcion que invierte los colores de cada pixel de una imagen pixmap
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagenRGB (image) 
Recorrido(Retorno): image 
|#

(define invertColorRGB (lambda (imagenrgb)
      (if (pixmap? imagenrgb)
          (list (list-ref imagenrgb 0)(list-ref imagenrgb 1)
                (map (lambda (p) (list (list-ref p 0)(list-ref p 1)(- 255 (list-ref p 2))(abs(- (list-ref p 3) 255))(abs(- (list-ref p 4) 255))(list-ref p 5)))(list-ref imagenrgb 2)))
          null)))

(provide (all-defined-out))