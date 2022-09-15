#lang racket

(require "TDA-Image.rkt")

; TDA modificador

#| Documentacion: flopH
Descripcion: Funcion que permite modificar el valor de la posicion x del pixel, se encuentra definida dentro de flipH
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): ancho(int) X  pix(pixbit-d|pixrgb-d|pixhex-d) 
Recorrido(Retorno): Booleano
|#
                         
#| Documentacion:  TDA-flipH
Descripcion: Funcion que modifica la posicion de los pixeles de la imagen en la coordenada x
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola (mediante la funcion mymaprecur)
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): lista (image) flipeada
|#

#| Documentacion: mymaprecur
Descripcion: Version recursiva de la funcion map, se encuentra definida dentro del flipH
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola
Dominio(Argumento de entrada): procedimiento (funcion) X lista (list) 
Recorrido(Retorno): lista (list)
|#

(define flipH (lambda (lista)
                
                (define mymaprecur (lambda (procedimiento lista)
                                     (if (null? lista)
                                         null
                                         (cons (procedimiento (car lista)) (mymaprecur procedimiento (cdr lista))))))
                
                (define flopH (lambda (ancho pix)
                                (append (list (list-ref pix 0) (abs (- (list-ref pix 1) (- ancho 1)))) (cddr pix))))
                
                (if (and (list? lista)(not(null? lista)))
                    (list (list-ref lista 0)(list-ref lista 1)(mymaprecur (lambda (p) (flopH (list-ref lista 0) p)) (list-ref lista 2)))
                    null)))

(provide (all-defined-out))