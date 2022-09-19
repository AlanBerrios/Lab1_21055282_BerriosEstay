#lang racket

(require "TDA-Image.rkt")

#| Documentacion: TDA-imgRGB->imgHex
Descripcion: Funcion transforma una imagen con sus colores en rgb a una imagen con sus colores en hexadecimal
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image hex

Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.

|#

(define imgRGB->imgHex (lambda (imagen)
                        
                      ;Funcion que compara el numero entrante con la tabla hexadecimal y lo entrega respecto a esta. Dom: RGB (int), Rec: string   
                      (define tablahexadecimal (lambda (RGB)
                          (if (<= RGB 9)
                              (number->string RGB)
                              (if (and (>= RGB 10) (<= RGB 15))
                                  (cond [(equal? RGB 10) "A"]
                                  [(equal? RGB 11) "B"]
                                  [(equal? RGB 12) "C"]
                                  [(equal? RGB 13) "D"]
                                  [(equal? RGB 14) "E"]
                                  [(equal? RGB 15) "F"])
                                  null))))
                       ;Funcion que recibe un numero y realiza su transformacion a hexadecimal. Tipo de recursion: De cola. Dom: RGB(int) X RGB1(string). Rec: string
                      (define rgbahex (lambda (RGB RGB1)
                           (if (<= RGB 15)
                               (if (= (string-length RGB1) 0)
                                   (string-append (number->string 0)(tablahexadecimal RGB) RGB1)
                                   (if ( = (string-length RGB1) 1)
                                       (string-append (tablahexadecimal RGB) RGB1)
                                       null))
                                (rgbahex (quotient RGB 16)(string-append (tablahexadecimal(abs(-(*(quotient RGB 16) 16) RGB)))RGB1)))))
                      ;Funcion que une los 3 numeros de colores de un pixrgb-d, los transforma a hex y los junta en un solo string. Dom: R(int) X G(int) X B(int). Rec: string
                      (define unirhexs (lambda (R G B)
                          (string-append(string-append(string-append "#" R) G) B)))
                      ;Funcion que devuelve el pixrgb-d en formato pixhex-d. Dom: pixel(pixrgb-d). Rec: pixhex-d
                      (define cambio2 (lambda (pixel)
                         (list(list-ref pixel 0)(list-ref pixel 1)(unirhexs(rgbahex(list-ref pixel 2)"")(rgbahex (list-ref pixel 3)"")(rgbahex (list-ref pixel 4)""))(list-ref pixel 5)
                              )))
                      ;Funcion que aplica el cambio a hexadecimal a todos los pixeles de una imagen rgb. Dom: listapixeles(list). Rec: list
                      (define cambio1 (lambda (listapixeles)
                        (map (lambda (p) (cambio2 p)) listapixeles)))
                      ;Aqui se realiza ya el cambio a la imagen en si. Devolviendo la imagen en formato hex.  
                      (list (list-ref imagen 0) (list-ref imagen 1) (cambio1 (list-ref imagen 2)))))

(provide (all-defined-out))