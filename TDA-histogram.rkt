#lang racket

(require "TDA-Image.rkt")
(require "TDA-bitmap.rkt")
(require "TDA-pixmap.rkt")
(require "TDA-hexmap.rkt")
(require "TDA-crop.rkt")

#| Documentacion: TDA-histogram
Descripcion: Retorna un histograma de frecuencias a partir de los colores en cada una de las imágenes.
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): lista de colores y su cantidad (list)

Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.

|#

(define histogram (lambda (imagen)
                    ; Funcion que cuenta la cantidad de bits que hay en la imagen. Tipo de recursion: De cola. Dom: pixeles(list). Rec: lista de cada bit y su cantidad (list)
                    (define contarbits (lambda (pixeles)
                                         ;Funcion que obtiene todos los bits de los pixeles. Dom: pixeles(list). Rec: bits (list)
                                         (define obtener01s (lambda (pixeles)
                                                              (map (lambda (p) (sub1 p)) (map (lambda (q) (add1 (list-ref q 2))) pixeles))))
                                         ;Funcion que cuenta la cantidad de 0s y 1s. Tipo de recursion: De cola. Dom: lista(list) X cont0 (int 0) X cont1 (int 0) 
                                         (define contar (lambda (lista cont0 cont1)                 
                                                          (if (null? lista)
                                                              (list (list 0 cont0) (list 1 cont1))                       
                                                              (if (= (car lista) 0)
                                                                  (contar (cdr lista) (+ cont0 1) cont1)                    
                                                                  (if (= (car lista) 1)
                                                                      (contar (cdr lista) cont0 (+ cont1 1))                                                  
                                                                      null)))))
                                         ; Aqui se manda a llamar a contar los numeros de bits
                                         (contar (obtener01s pixeles) 0 0)))
                    
                    ;Funcion que devuelve cada color de la imagen y cuantas veces se repite el color. Tipo de recursion: De cola. Dom: pixeles (list). Rec: colores y cantidad (list)
                    (define contarrgbs (lambda (pixeles)
                                         ;Funcion que devuelve una lista con todos los colores de cada pixel. Dom: pixeles (list). Rec: lista de colores (list)
                                         (define obtenergbs (lambda (pixeles)
                                                              (map (lambda (q) (list (sub1(add1 (list-ref q 2))) (sub1(add1 (list-ref q 3))) (sub1(add1 (list-ref q 4))) )) pixeles)))
                                    ; Funcion que cuenta cuantas veces se repite un color en la imagen. Tipo de recursion: De cola. Dom: pixel (pixrgb-d) X pixeles (list) X cont(int 0)
                                         (define contargb (lambda (pixel pixeles cont)                   
                                                            (if (null? pixeles)
                                                                (list pixel cont)                       
                                                                (if (equal? pixel (car pixeles))
                                                                    (contargb pixel (cdr pixeles) (+ cont 1))
                                                                    (contargb pixel (cdr pixeles) cont)) )))
                                     ; Aqui juntan las funciones anteriores y devuelve la lista de los colores y su cantidad en la imagen
                                         (map (lambda (p) (contargb p (obtenergbs pixeles) 0)) (delrepe (obtenergbs pixeles)))))
                    
                   ;Funcion que devuelve cada color de la imagen y cuantas veces se repite el color. Tipo de recursion: De cola. Dom: pixeles (list). Rec: colores hex y cantidad (list)
                    (define contarhexs (lambda (pixeles)
                                         ;Funcion que devuelve una lista con todos los colores hex de cada pixel. Dom: pixeles (list). Rec: lista de colores hex (list)
                                         (define obtenerhexs (lambda (pixeles)
                                                               (map (lambda (q) (list->string(reverse(cdr(reverse(string->list(string-append (list-ref q 2) "a"))))))) pixeles)))
                             ; Funcion que cuenta cuantas veces se repite un color hex en la imagen. Tipo de recursion: De cola. Dom: pixel (pixhex-d) X pixeles (list) X cont(int 0)
                                         (define contahex (lambda (pixel pixeles cont)                   
                                                            (if (null? pixeles)
                                                                (list pixel cont)                       
                                                                (if (equal? pixel (car pixeles))
                                                                    (contahex pixel (cdr pixeles) (+ cont 1))
                                                                    (contahex pixel (cdr pixeles) cont)) )))
                                  ; Aqui juntan las funciones anteriores y devuelve la lista de los colores hex y su cantidad en la imagen
                                         (map (lambda (p) (contahex p (obtenerhexs pixeles) 0)) (delrepe (obtenerhexs pixeles)))))
                 ; Aqui se verifica que tipo de map es la imagen, dependiendo de esto, realiza una operacion u otra.
                    (cond
                      [(bitmap? imagen) (contarbits (list-ref imagen 2))]
                      [(pixmap? imagen) (contarrgbs (list-ref imagen 2))]
                      [(hexmap? imagen) (contarhexs (list-ref imagen 2))]
                      )))

(provide (all-defined-out))