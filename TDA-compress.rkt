#lang racket

(require "TDA-Image.rkt")
(require "TDA-bitmap.rkt")
(require "TDA-pixmap.rkt")
(require "TDA-hexmap.rkt")
(require "TDA-compressed.rkt")
(require "TDA-histogram.rkt")


#| Documentacion: TDA-compress
Descripcion: Funcion que comprime una imágen eliminando aquellos pixeles con el color más frecuente
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image

Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.

|#

(define compress (lambda (imagen)
                   ;Funcion que obtiene las repeticiones de cada color en una imagen. Dom: imagen (image). Rec: list
                   (define obtenernumpix (lambda (imagen)
                                           (map (lambda (p) (sub1(add1 (list-ref p 1)))) (histogram imagen))))
                   ;Funcion que voltea cada elemento de un histograma ((color)vecesrepetidas) -> (vecesrepetidas(color)). Dom: histogramimagen (histogram). Rec: list
                   (define darvuelta (lambda (histogramimagen)
                                       (map (lambda (q) (reverse q)) histogramimagen)))
                   ;Funcion que obtiene el color mas repetido de la imagen. Dom: imagen (image). Rec: list
                   (define obtenermasrepetido (lambda (imagen)
                                                (list-ref (reverse (assoc (car (sort (obtenernumpix imagen) >)) (darvuelta(histogram imagen))))0)))
                   ;Funcion que elimina los pixeles con el color mas frecuente de una imagen. Dom: masrepetido (list "color") X imagen (image). Rec: list
                   (define removepix (lambda (masrepetido imagen)
                    
                                       (if (or (bitmap? imagen) (hexmap? imagen))

                                           (filter (lambda (p) (not (equal? masrepetido (list-ref p 2)))) (list-ref imagen 2))

                                           (if (pixmap? imagen)

                                               (filter (lambda (p) (not (equal? masrepetido (list (list-ref p 2)(list-ref p 3)(list-ref p 4)) ))) (list-ref imagen 2))
                                               null ))))
                       ; Aqui se llama a algunas funciones para devolver imagen comprimida.
                       (list (list-ref imagen 0)(list-ref imagen 1)(removepix (obtenermasrepetido imagen)imagen))))

(provide (all-defined-out))