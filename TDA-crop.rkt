#lang racket

(require "TDA-Image.rkt")


#| Documentacion: get-minimo
Descripcion: Funcion recibe una lista y devuelve el valor minimo de esta
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola
Dominio(Argumento de entrada): lista (list)
Recorrido(Retorno): numero (int)
|#

(define (get-minimo lista)
  (cond[(null? (cdr lista)) (list-ref lista 0)]
       [(< (list-ref lista 0) (get-minimo (cdr lista))) (list-ref lista 0)]
       [else (get-minimo (cdr lista))]))

#| Documentacion: delrepe
Descripcion: Funcion elimina los elementos repetidos de una lista
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola
Dominio(Argumento de entrada): lista (list)
Recorrido(Retorno): lista (list)
|#

(define (delrepe lista)
  (cond [(null? lista) lista]
        [(member (list-ref lista 0) (cdr lista)) (delrepe (cdr lista))]
        [else (append (list (list-ref lista 0)) (delrepe (cdr lista)))]))

#| Documentacion: TDA-crop
Descripcion: Funcion que recorta la imagen mediante 2 coordenadas, reescala los parametros de ancho, alto de la imagen y las coordenadas x e y de cada pixel
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
Recorrido(Retorno): image "recortada y re-escalada"

Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.

|#

(define crop (lambda (imagen x1 y1 x2 y2)
               ; Funcion que recibe un pixel y compara si ese pixel esta dentro del recorte dado
               ; Dominio: pixel (pix |bit|rgb|hex| -d) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
               ; Recorrido: pixel  
               (define seleccionapixel (lambda (pixel x1 y1 x2 y2)                                                                               
                                         (if (and (list? pixel)(not(null? pixel)))
                                             (if (and (and (>= (list-ref pixel 1) x1)(>= (list-ref pixel 0) y1)) (and (<= (list-ref pixel 1) x2)(<= (list-ref pixel 0) y2)))
                                                 pixel
                                                 #f)
                                             null)))
               ; Funcion que recibe una imagen y las coordenadas a recortar
               ; Dominio: imagen(image) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
               ; Recorrido: image "recortada"
               (define cropear (lambda (imagen x1 y1 x2 y2)
                                 (if (and (list? imagen)(not(null? imagen)))
                                     (list (list-ref imagen 0) (list-ref imagen 1) (filter list? (map (lambda (p) (seleccionapixel p x1 y1 x2 y2)) (list-ref imagen 2))))
                                     null)))
               ; Funcion que recibe una imagen recortada y devuelve todas las coordenadas y de sus pixeles
               ; Dominio: lista (image)
               ; Recorrido: lista de y's (list)
               (define getall-y (lambda (lista)
                                  (map (lambda (q) (sub1 q)) (map (lambda (p) (add1 (list-ref p 0))) (list-ref lista 2)))))
               ; Funcion que recibe una imagen recortada y devuelve todas las coordenadas x de sus pixeles
               ; Dominio: lista (image)
               ; Recorrido: lista de x's (list)
               (define getall-x (lambda (lista)
                                  (map (lambda (q) (sub1 q)) (map (lambda (p) (add1 (list-ref p 1))) (list-ref lista 2)))))
               ; Funcion que recibe los pixeles de una imagen recortada y la menor coordenada x e y de sus pixeles, y devuelve la lista de pixeles corregida
               ; Dominio: imagencrop (list)
               ; Recorrido: lista de pixeles corregidos (list)
               (define pixelescorregidos (lambda (imagencrop menorx menory)
                                           
                                           ;funcion que reescala la coordenada y de un pixel. Dom: pix (pixel) X menory (int). Rec: pixel
                                           (define cambiary (lambda (pix menory) 
                                                              (append (list (- (list-ref pix 0) menory) (list-ref pix 1)) (cddr pix))))
                                           
                                           ;funcion que reescala la coordenada x de un pixel. Dom: pix (pixel) X menory (int). Rec: pixel
                                           (define cambiarx (lambda (pix menorx) 
                                                              (append (list (list-ref pix 0) (- (list-ref pix 1) menorx)) (cddr pix))))
                                           
                                           ;funcion que reescala la coordenada x de los pixeles. Dom: listapixeles (list) X menorx (int). Rec: pixeles (list)
                                           (define xcorregido (lambda (listapixeles menorx) 
                                                                (map (lambda (p) (cambiarx p menorx)) listapixeles)))
                                           
                                           ;funcion que reescala la coordenada y de los pixeles. Dom: listapixeles (list) X menory (int). Rec: pixeles (list)
                                           (define ycorregido (lambda (listapixeles menory) 
                                                                (map (lambda (p) (cambiary p menory)) listapixeles)))
                                           
                                           ;funcion que reescala las coordenadas de los pixeles. Dom: listapixeles (list) X menorx (int) X menory (int) Rec: pixeles (list)
                                           (define devolver (lambda (listapixeles menorx menory)                                             
                                                              (ycorregido (xcorregido listapixeles menorx) menory)))
                                           
                                           (devolver imagencrop menorx menory)))
               
               ; Funcion que recibe la imagen recortada y reescala el ancho, alto y las coordenadas de los pixeles
               ; Dominio: imagencropeada (iamge) X menorx (int) X menory (int)
               ; Recorrido: imagen corregida (image)
               (define devolver-crop (lambda (imagencropeada menorx menory listaxs listays)
                                       (list (length listaxs)
                                             (length listays)
                                             (pixelescorregidos (list-ref imagencropeada 2) menorx menory))))
               
               ; Aqui se juntan las funciones anteriores y se ejecuta finalmente la funcion crop          
               (devolver-crop (cropear imagen x1 y1 x2 y2) (get-minimo (getall-x (cropear imagen x1 y1 x2 y2))) (get-minimo (getall-y (cropear imagen x1 y1 x2 y2)))
                              (delrepe (getall-x (cropear imagen x1 y1 x2 y2))) (delrepe (getall-y (cropear imagen x1 y1 x2 y2))))))


(provide (all-defined-out))