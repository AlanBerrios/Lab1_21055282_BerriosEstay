#lang racket

(require "TDA-Image.rkt")

; TDA modificador

#| Documentacion:  flopH
Descripcion: Funcion que permite modificar el valor de la posicion x del pixel recibido
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): ancho(int) X  pix(pixbit-d|pixrgb-d|pixhex-d) 
Recorrido(Retorno): Booleano
|#

(define flopH (lambda (ancho pix)
                (append (list (list-ref pix 0) (abs (- (list-ref pix 1) (- ancho 1)))) (cddr pix))))
                         
#| Documentacion:  TDA-flipH
Descripcion: Funcion que modifica la posicion de los pixeles de la imagen en la coordenada x
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): lista (image) flipeada
|#

(define flipH (lambda (lista)
                (if (and (list? lista)(not(null? lista)))
                    (list (list-ref lista 0)(list-ref lista 1)(map (lambda (p) (flopH (list-ref lista 1) p)) (list-ref lista 2)))
                    null)))

(provide (all-defined-out))