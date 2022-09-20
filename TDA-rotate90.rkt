#lang racket

(require "TDA-Image.rkt")
(require "TDA-flipV.rkt")

; require - image, flipV

#| Documentacion: TDA-rotate90
Descripcion: Funcion que gira en 90 grados la imagen hacia la derecha.
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image 
|#

(define rotate90 (lambda (imagen)
                   ;Funcion que toma las dos coordenadas de un pixel y las invierte. Dom: pixel (pix bit|rgb|hex -d). Rec: pixel 
                   (define darvuelta (lambda (pixel)                    
                      (append (list (list-ref pixel 1) (list-ref pixel 0)) (cddr pixel))))
                   ; Aqui se aplica la construccion de la imagen rotada en 90 grados. llamando a la funcion flipV primero, para luego aplicar la funcion darvuelta.
                   (list (list-ref imagen 0) (list-ref imagen 1) (map (lambda (p) (darvuelta p)) (list-ref (flipV imagen) 2)))))

(provide (all-defined-out))