#lang racket

(require "TDA-Image.rkt")

; TDA modificador

#| Documentacion: flopV
Descripcion: Funcion que permite modificar el valor de la posicion y del pixel, se encuentra definida dentro de flipV
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): alto(int) X  pix(pixbit-d|pixrgb-d|pixhex-d) 
Recorrido(Retorno): Booleano
|#
                         
#| Documentacion:  TDA-flipV
Descripcion: Funcion que modifica la posicion de los pixeles de la imagen en la coordenada y
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola (mediante la funcion mymaprecur)
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): lista (image) flipeada
|#

#| Documentacion: mymaprecur
Descripcion: Version recursiva de la funcion map, se encuentra definida dentro del flipV
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola
Dominio(Argumento de entrada): procedimiento (funcion) X lista (list) 
Recorrido(Retorno): lista (list)
|#

(define flipV (lambda (lista)
                
                (define mymaprecur (lambda (procedimiento lista)
                                     (if (null? lista)
                                         null
                                         (cons (procedimiento (car lista)) (mymaprecur procedimiento (cdr lista))))))
                
                (define flopV (lambda (alto pix)
                                (append (list (abs (- (list-ref pix 0) (- alto 1))) (list-ref pix 1)) (cddr pix))))
                
                (if (and (list? lista)(not(null? lista)))
                    (list (list-ref lista 0)(list-ref lista 1)(mymaprecur (lambda (p) (flopV (list-ref lista 1) p)) (list-ref lista 2)))
                    null)))

(provide (all-defined-out))