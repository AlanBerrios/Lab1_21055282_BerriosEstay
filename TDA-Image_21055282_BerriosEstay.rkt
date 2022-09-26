#lang racket

; Se requieren los siguientes archivos para poder construir el TDA-Image.
(require "TDA-pixbit-d_21055282_BerriosEstay.rkt")
(require "TDA-pixrgb-d_21055282_BerriosEstay.rkt")
(require "TDA-pixhex-d_21055282_BerriosEstay.rkt")

; ------------------------------------------------------------------------------------------------------------------------------------
; ------------------------------------------- ACLARACIONES COMENTARIOS ---------------------------------------------------------------
; ------------------------------------------------------------------------------------------------------------------------------------

; Respecto a los comentarios, las funciones que tengan una linea de este tipo encima:

; ---------------- Tipo de Funcion: _____ ----------------- :::::::::::::::::: NOMBRE :::::::::::::::::::

; Hacen referencia a que es una funcion "principal" del codigo. Osea que son funciones que piden en el enunciado.

; Por otro lado las funciones que NO tienen ":::::: NOMBRE ::::::"
; Son funciones propias, hechas para facilitar las funciones principales.


; ------------ Necesarios para image --------------

; --------- Tipo de Funcion: de pertenecia ------------

; Verifica si los elementos de la lista de entrada son de un mismo tipo de pixel (pixbit-rgb-hex-d). Dom: Lista de n-variables (pixbit|rgb|hex-d). Rec: Booleano

(define homologo? (lambda (lista)
  (if (or (andmap pixbits? lista)
          (andmap pixrgbs? lista)
          (andmap pixhexs? lista))
      true false)))

; --------- Tipo de Funcion: de pertenecia ------------

;Verifica si las coordenadas x e y de cada pixel estan dentro del alto y ancho de la imagen. Dom: ancho (int) X alto (int) X lista (pixbit|rgb|hex-d). Rec: Booleano

(define dentrodelarea? (lambda (ancho alto lista)
                         (if (and (andmap (lambda (x) (> ancho (list-ref x 1))) lista)
                                  (andmap (lambda (y) (> alto (list-ref y 0))) lista))
                             true false)))

; --------- Tipo de TDA: Constructor --------------------------------------------------------- :::::::::::::::::::::::::: IMAGE :::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: TDA-image
Descripcion: verifica que los dos primeros elementos cumplen con int>0? y que el tercer elemento, la lista de n-variables, cumpla con homologo?,
si se cumplen, crea una representacion de image en froma de lista
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): ancho (int) X alto (int) X pixeles (pixbit|rgb|hex-d)
Recorrido(Retorno): image
|#

(define image (lambda (ancho alto . pixeles)
    (if (and (int>0? ancho) 
             (int>0? alto)
             (<= (length pixeles)(* ancho alto))
             (homologo? pixeles)
             (dentrodelarea? ancho alto pixeles)
             )        
        (list ancho alto pixeles)
        null )))

;-------------------------------------------------------------------------------------

; --------- Tipo de Funcion: selector ------------

;Funcion que recibe el ancho de una imagen. Dom: imagen (image). Rec: int

(define getancho (lambda (imagen)
                   (list-ref imagen 0)))

; --------- Tipo de Funcion: selector ------------

;Funcion que recibe el alto de una imagen. Dom: imagen (image). Rec: int

(define getalto (lambda (imagen)
                    (list-ref imagen 1)))

; --------- Tipo de Funcion: selector ------------ 

;Funcion que recibe los pixeles de una imagen. Dom: imagen (image). Rec: list

(define getpixeles (lambda (imagen)
                           (list-ref imagen 2)))

; --------- Tipo de Funcion: de pertenecia ------- :::::::::::::::::::::::::::::::::::: BITMAP? :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion:  bitmap?
Descripcion: Verifica si los pixeles de una imagen corresponden a pixbit
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#
  
(define bitmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixbits? (getpixeles lista))
                          true
                          false)
                      false)))

; --------- Tipo de Funcion: de pertenecia ------- ::::::::::::::::::::::::::::::::::: PIXMAP? :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion:  pixmap?
Descripcion: Verifica si los pixeles de una imagen corresponden a pixrgb
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define pixmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixrgbs? (getpixeles lista))
                          true
                          false)
                      false)))

; --------- Tipo de Funcion: de pertenecia ---------- :::::::::::::::::::::::::::::::::: HEXMAP? :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: hexmap?
Descripcion: Verifica si los pixeles de una imagen corresponden a pixhex
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define hexmap? (lambda (lista)
                  (if (and (list? lista)(not(null? lista)))
                      (if (andmap pixhexs? (getpixeles lista))
                          true
                          false)
                      false)))

; --------- Tipo de Funcion: de pertenecia ------- :::::::::::::::::::::::::::::::::::::::: COMPRESSED? ::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: compressed?
Descripcion: Verifica si la imagen de entrada esta comprimida
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): lista (image) 
Recorrido(Retorno): Booleano
|#

(define compressed? (lambda (lista)
                      (if (and (list? lista)(not(null? lista)))                          
                          (if (not(null? (getpixeles lista)))                              
                              (if (> (*(getancho lista)(getalto lista)) (length (getpixeles lista)))
                                  true
                                  false)                              
                              (if (> (*(getancho lista)(getalto lista)) (- (length (getpixeles lista)) 1))
                                  true
                                  null))                              
                          null)))

; ------- Necesarios para flipH-V ------------------

; ----------------- Tipo de Funcion: modificador -----------------
; Funcion que ordena de menor a mayor la coordenada x de una lista de pixeles. Dom: pixeles (list). Rec: list
(define ordenarx (lambda (pixeles)
                   (sort pixeles #:key cadr <)))

; ----------------- Tipo de Funcion: modificador -----------------
; Funcion que ordena de menor a mayor la coordenada y de una lista de pixeles. Dom: pixeles (list). Rec: list
(define ordenary (lambda (pixeles)
                   (sort pixeles #:key car <)))

; ----------------- Tipo de Funcion: modificador -----------------
; Funcion que ordena de menor a mayor la coordenada x e y de una imagen. Dom: imagen (image). Rec: image
(define ordenarpixeles (lambda (imagen)
                        (list (getancho imagen) (getalto imagen) (ordenary (ordenarx (getpixeles imagen))))))


                    
; --------- Tipo de Funcion: Otros ------------                    
;Version recursiva de la funcion map. Tipo de recursion: De cola. Dom: procedimiento (funcion) X lista (list) . Rec: lista (list)

(define mymaprecur (lambda (procedimiento lista)
                                     (if (null? lista)
                                         null
                                         (cons (procedimiento (car lista)) (mymaprecur procedimiento (cdr lista))))))
                                               ; Recursion de cola. No se dejan procedimientos sin realizar en el camino

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: FLIPH ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: flipH
Descripcion: Funcion que modifica la posicion de los pixeles de la imagen en la coordenada x
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola (mediante la funcion mymaprecur)
Dominio(Argumento de entrada): imagen (image) 
Recorrido(Retorno): image
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#

(define flipH (lambda (imagen)
                
  ;Funcion que permite modificar el valor de la posicion x del pixel. Dom: ancho(int) X  pix(pixbit|rgb|hex-d). Rec: Bool
                (define flopH (lambda (ancho pix)
                                (append (list (gety pix) (abs (- (getx pix) (- ancho 1)))) (cddr pix))))
                
                (if (and (list? imagen)(not(null? imagen)))
                    (ordenarpixeles(list (getancho imagen)(getalto imagen)(mymaprecur (lambda (p) (flopH (getancho imagen) p)) (getpixeles imagen))))
                    null)))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: FLIPV :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: flipV
Descripcion: Funcion que modifica la posicion de los pixeles de la imagen en la coordenada y
Tipo de algoritmo/estrategia: No aplica
Tipo de recursion: De cola (mediante la funcion mymaprecur)
Dominio(Argumento de entrada): iamgen (image) 
Recorrido(Retorno): image
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#

(define flipV (lambda (imagen)
                
  ;Funcion que permite modificar el valor de la posicion x del pixel. Dom: ancho(int) X  pix(pixbit|rgb|hex-d). Rec: Bool
                (define flopV (lambda (alto pix)
                                (append (list (abs (- (gety pix) (- alto 1))) (getx pix)) (cddr pix))))
                
                (if (and (list? imagen)(not(null? imagen)))
                    (ordenarpixeles(list (getancho imagen)(getalto imagen)(mymaprecur (lambda (p) (flopV (getalto imagen) p)) (getpixeles imagen))))
                    null)))


;----------- Necesarios para el crop y otros --------------------
; --------- Tipo de Funcion: Otros ------------
; Funcion recibe una lista y devuelve el valor minimo de esta. Tipo de recursion: De cola. Dom: lista (list). Rec: int

(define get-minimo (lambda (lista)
  (cond[(null? (cdr lista)) (list-ref lista 0)]
       [(< (list-ref lista 0) (get-minimo (cdr lista))) (list-ref lista 0)] ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
       [else (get-minimo (cdr lista))]))) ; Recursion

; --------- Tipo de Funcion: Otros ------------
; Funcion elimina los elementos repetidos de una lista. Tipo de recursion: De cola. Dom: lista (list). Rec: list

(define delrepe (lambda (lista)
  (cond [(null? lista) lista]
        [(member (list-ref lista 0) (cdr lista)) (delrepe (cdr lista))] ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
        [else (append (list (list-ref lista 0)) (delrepe (cdr lista)))]))) ; Recursion

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: CROP ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: crop
Descripcion: Funcion que recorta recorta una imagen a partir de un cuadrante, redimensionando la imagen y sus pixeles
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image) X x1 (int) X y1 (int) X x2 (int) X y2 (int)
Recorrido(Retorno): image 
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#

(define crop (lambda (imagen x1 y1 x2 y2)
               
           ; Funcion que recibe un pixel y compara si ese pixel esta dentro del cuadrante. Dom: pixel (pix|bit|rgb|hex|-d) X x1 (int) X y1 (int) X x2 (int) X y2 (int). Rec: pixel  
               (define seleccionapixel (lambda (pixel x1 y1 x2 y2)                                                                               
                                         (if (and (list? pixel)(not(null? pixel)))
                                             (if (and (and (>= (getx pixel) x1)(>= (gety pixel) y1)) (and (<= (getx pixel) x2)(<= (gety pixel) y2)))
                                                 pixel
                                                 #f)
                                             null)))
               
               ; Funcion que recibe una imagen y las coordenadas a recortar, "recorta" la imagen. Dom: imagen(image) X x1 (int) X y1 (int) X x2 (int) X y2 (int). Rec: image
               (define cropear (lambda (imagen x1 y1 x2 y2)
                                 (if (and (list? imagen)(not(null? imagen)))
                                     (list (getancho imagen) (getalto imagen) (filter list? (map (lambda (p) (seleccionapixel p x1 y1 x2 y2)) (getpixeles imagen))))
                                     null)))
               
               ; Funcion que recibe una imagen y devuelve todas las coordenadas y de sus pixeles. Dom: lista (image). Recorrido: list
               (define getall-y (lambda (lista)
                                  (map (lambda (q) (sub1 q)) (map (lambda (p) (add1 (gety p))) (getpixeles lista)))))
               
               ; Funcion que recibe una imagen y devuelve todas las coordenadas x de sus pixeles. Dom: lista (image). Rec: list
               (define getall-x (lambda (lista)
                                  (map (lambda (q) (sub1 q)) (map (lambda (p) (add1 (getx p))) (getpixeles lista)))))
               
               ; Funcion que devuelve la lista de pixeles corregida. Dom: imagencrop (list) X menorx (int) X menory (int). Rec: list
               (define pixelescorregidos (lambda (pixelesimagencrop menorx menory)
                                           
                                           ;funcion que reescala la coordenada y de un pixel. Dom: pix (pixel) X menory (int). Rec: pixel
                                           (define cambiary (lambda (pix menory) 
                                                              (append (list (- (gety pix) menory) (getx pix)) (cddr pix))))
                                           
                                           ;funcion que reescala la coordenada x de un pixel. Dom: pix (pixel) X menory (int). Rec: pixel
                                           (define cambiarx (lambda (pix menorx) 
                                                              (append (list (gety pix) (- (getx pix) menorx)) (cddr pix))))
                                           
                                           ;funcion que reescala la coordenada x de los pixeles. Dom: listapixeles (list) X menorx (int). Rec: pixeles (list)
                                           (define xcorregido (lambda (listapixeles menorx) 
                                                                (map (lambda (p) (cambiarx p menorx)) listapixeles)))
                                           
                                           ;funcion que reescala la coordenada y de los pixeles. Dom: listapixeles (list) X menory (int). Rec: pixeles (list)
                                           (define ycorregido (lambda (listapixeles menory) 
                                                                (map (lambda (p) (cambiary p menory)) listapixeles)))
                                           
                                           ;funcion que reescala las coordenadas de los pixeles. Dom: listapixeles (list) X menorx (int) X menory (int) Rec: pixeles (list)
                                           (define devolver (lambda (listapixeles menorx menory)                                             
                                                              (ycorregido (xcorregido listapixeles menorx) menory)))
                                           
                                           (devolver pixelesimagencrop menorx menory)))
               
               ; Funcion que devuelve la imagen cropeada con los pixeles corregidos. Dom: imagencropeada (iamge) X menorx (int) X menory (int). Rec: image
               (define devolver-crop (lambda (imagencropeada menorx menory listaxs listays)
                                       (list (length listaxs)
                                             (length listays)
                                             (pixelescorregidos (getpixeles imagencropeada) menorx menory))))
               
               ; Aqui se juntan las funciones anteriores y se ejecuta finalmente la funcion crop
           (if (not(null? (getpixeles imagen)))
               (devolver-crop (cropear imagen x1 y1 x2 y2) (get-minimo (getall-x (cropear imagen x1 y1 x2 y2))) (get-minimo (getall-y (cropear imagen x1 y1 x2 y2)))
                              (delrepe (getall-x (cropear imagen x1 y1 x2 y2))) (delrepe (getall-y (cropear imagen x1 y1 x2 y2))))
               null)))


;---- Necesarios para el imgRGB->imgHex -----

; --------- Tipo de Funcion: Otros ------------                       
;Funcion que compara el numero entrante con la tabla hexadecimal y lo entrega respecto a esta. Dom: RGB (int), Rec: string   
(define tablahexadeciamal (lambda (RGB)
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

; --------- Tipo de Funcion: Otros ------------  
;Funcion que recibe un numero y realiza su transformacion a hexadecimal. Tipo de recursion: De cola. Dom: RGB(int) X RGB1(string). Rec: string
(define rgbahex (lambda (RGB RGB1)
                  (if (<= RGB 15)
                      (if (= (string-length RGB1) 0)
                          (string-append (number->string 0)(tablahexadeciamal RGB) RGB1)
                          (if ( = (string-length RGB1) 1)
                              (string-append (tablahexadeciamal RGB) RGB1)
                              null))
                      (rgbahex (quotient RGB 16)(string-append (tablahexadeciamal(abs(-(*(quotient RGB 16) 16) RGB)))RGB1)))))
                      ; Recursion de cola. No se dejan procedimientos sin realizar en el camino

; --------- Tipo de Funcion: Otros ------------    
;Funcion que une los 3 numeros de colores de un pixrgb-d, los transforma a hex y los junta en un solo string. Dom: R(int) X G(int) X B(int). Rec: string
(define unirhexs (lambda (R G B)
                   (string-append(string-append(string-append "#" R) G) B)))

; --------- Tipo de Funcion: Otros ------------    
;Funcion que devuelve el pixrgb-d en formato pixhex-d. Dom: pixel(pixrgb-d). Rec: pixhex-d
(define pixelrgbahex (lambda (pixel)
                  (list (gety pixel) (getx pixel) (unirhexs(rgbahex(getR pixel)"")(rgbahex (getG pixel)"")(rgbahex (getB pixel)""))(getD pixel))))

; --------- Tipo de Funcion: Otros ------------  
;Funcion que aplica el cambio a hexadecimal a todos los pixeles de una imagen rgb. Dom: listapixeles(list). Rec: list
(define pixelesrgbahex (lambda (listapixeles)
                  (map (lambda (p) (pixelrgbahex p)) listapixeles)))

; --------- Tipo de Funcion: Modificador ------------ :::::::::::::::::::::::::::::::::: IMAGERGB->IMAGEHEX ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: imgRGB->imgHex
Descripcion: Funcion que transforma una imagen con sus colores en rgb a una imagen con sus colores en hexadecimal
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#


(define imgRGB->imgHex (lambda (imagen)                         
                      ;Aqui se realiza ya el cambio a la imagen en si. Devolviendo la imagen en formato hex.  
                      (list (getancho imagen) (getalto imagen) (pixelesrgbahex (getpixeles imagen)))))

; --------- Tipo de funcion: Constructor ------------ :::::::::::::::::::::::::::::::::: HISTOGRAM :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: histogram
Descripcion: Retorna un histograma de frecuencias a partir de los colores en cada una de las imágenes.
Tipo de algoritmo/estrategia: Fuerza bruta (debido a que realiza una accion u otra dependiendo del tipo de imagen)
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): histogram
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#

(define histogram (lambda (imagen)
                    ; Funcion que cuenta la cantidad de bits que hay en la imagen. Tipo de recursion: De cola mediante la funcion contar
                    ; Dom: pixeles(list). Rec: lista de cada bit y su cantidad (list)
                    (define contarbits (lambda (pixeles)
                                         ;Funcion que obtiene todos los bits de los pixeles. Dom: pixeles(list). Rec: bits (list)
                                         (define obtener01s (lambda (pixeles)
                                                              (map (lambda (p) (sub1 p)) (map (lambda (q) (add1 (getbit q))) pixeles))))
                                         ;Funcion que cuenta la cantidad de 0s y 1s. Tipo de recursion: De cola. Dom: lista(list) X cont0 (int 0) X cont1 (int 0). Rec: list
                                         (define contar (lambda (lista cont0 cont1)                 
                                                          (if (null? lista)
                                                              (list (list 0 cont0) (list 1 cont1))                       
                                                              (if (= (car lista) 0)
                                                                  (contar (cdr lista) (+ cont0 1) cont1) ; Recursion de cola. No se dejan procedimientos sin realizar en el camino              
                                                                  (if (= (car lista) 1)
                                                                      (contar (cdr lista) cont0 (+ cont1 1))  ;Recursion                                                
                                                                      null)))))
                                         ; Aqui se manda a llamar a contar los numeros de bits
                                         (contar (obtener01s pixeles) 0 0)))
                    
                  ;Funcion que devuelve cada color de la imagen y cuantas veces se repite el color. Tipo de recursion: De cola. Dom: pixeles (list). Rec: colores y cantidad (list)
                    (define contarrgbs (lambda (pixeles)
                                         ;Funcion que devuelve una lista con todos los colores de cada pixel. Dom: pixeles (list). Rec: lista de colores (list)
                                         (define obtenergbs (lambda (pixeles)
                                                              (map (lambda (q) (list (sub1(add1 (getR q))) (sub1(add1 (getG q))) (sub1(add1 (getB q))) )) pixeles)))
                  ; Funcion que cuenta cuantas veces se repite un color en la imagen. Tipo de recursion: De cola, mismas razones que contarbits.
                                                                                            ; Dom: pixel (pixrgb-d) X pixeles (list) X cont(int 0). Rec: list
                                         (define contargb (lambda (pixel pixeles cont)                   
                                                            (if (null? pixeles)
                                                                (list pixel cont)                       
                                                                (if (equal? pixel (car pixeles))
                                                                    (contargb pixel (cdr pixeles) (+ cont 1))
                                                                    (contargb pixel (cdr pixeles) cont)) )))
                                     ; Aqui se juntan las funciones anteriores y devuelve la lista de los colores y su cantidad en la imagen
                                         (map (lambda (p) (contargb p (obtenergbs pixeles) 0)) (delrepe (obtenergbs pixeles))))) ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
                    
                 ;Funcion que devuelve cada color de la imagen y cuantas veces se repite el color. Tipo de recursion: De cola. Dom: pixeles (list). Rec: colores hex y cantidad (list)
                    (define contarhexs (lambda (pixeles)
                                         ;Funcion que devuelve una lista con todos los colores hex de cada pixel. Dom: pixeles (list). Rec: lista de colores hex (list)
                                         (define obtenerhexs (lambda (pixeles)
                                                               (map (lambda (q) (list->string(reverse(cdr(reverse(string->list(string-append (gethex q) "a"))))))) pixeles)))
                 ; Funcion que cuenta cuantas veces se repite un color hex en la imagen. Tipo de recursion: De cola, mismas razones que contarbit y contargb.
                                                                                           ; Dom: pixel (pixhex-d) X pixeles (list) X cont(int 0). Rec: list
                                         (define contahex (lambda (pixel pixeles cont)                   
                                                            (if (null? pixeles)
                                                                (list pixel cont)                       
                                                                (if (equal? pixel (car pixeles))
                                                                    (contahex pixel (cdr pixeles) (+ cont 1))
                                                                    (contahex pixel (cdr pixeles) cont)) )))
                                  ; Aqui juntan las funciones anteriores y devuelve la lista de los colores hex y su cantidad en la imagen
                                         (map (lambda (p) (contahex p (obtenerhexs pixeles) 0)) (delrepe (obtenerhexs pixeles))))) ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
                 ; Aqui se verifica que tipo de map es la imagen, dependiendo de esto, realiza una operacion u otra.
                    (cond
                      [(bitmap? imagen) (contarbits (getpixeles imagen))]
                      [(pixmap? imagen) (contarrgbs (getpixeles imagen))]
                      [(hexmap? imagen) (contarhexs (getpixeles imagen))]
                      )))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: ROTATE90 :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: rotate90
Descripcion: Funcion que gira en 90 grados la imagen hacia la derecha.
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image
Funciones anonimas: Cada funcion anononima tendra un comentario arriba para la descripcion, dominio y retorno.
|#

(define rotate90 (lambda (imagen)
                   ;Funcion que toma las dos coordenadas de un pixel y las invierte. Dom: pixel (pix bit|rgb|hex -d). Rec: pixel 
                   (define darvuelta (lambda (pixel)                    
                      (append (list (getx pixel) (gety pixel)) (cddr pixel))))
                   ; Aqui se aplica la construccion de la imagen rotada en 90 grados. llamando a la funcion flipV primero, para luego aplicar la funcion darvuelta.
                   (ordenarpixeles(list (getancho imagen) (getalto imagen) (map (lambda (p) (darvuelta p)) (getpixeles (flipV imagen)))))))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: COMPRESS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: compress
Descripcion: Funcion que comprime una imagen eliminando aquellos pixeles con el color más frecuente.
Tipo de algoritmo/estrategia: Fuerza bruta, debido a que en la funcion removepix, se realiza un proceso u otro dependiendo del tipo de pixeles
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
                                           (filter (lambda (p) (not (equal? masrepetido (list-ref p 2)))) (getpixeles imagen))
                                           (if (pixmap? imagen)
                                               (filter (lambda (p) (not (equal? masrepetido (list (getR p)(getG p)(getB p)) ))) (getpixeles imagen))
                                               null ))))
                       ; Aqui se llama a algunas funciones para devolver imagen comprimida.
                       (list (getancho imagen)(getalto imagen)(removepix (obtenermasrepetido imagen)imagen))))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: INVERTCOLORBIT ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#| Documentacion: invertColorBit
Descripcion: Funcion que invierte los bits de cada pixel de una imagen bitmap
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagenbit (image) 
Recorrido(Retorno): image 
|#

(define invertColorBit (lambda (imagenbit)
                         (if (bitmap? imagenbit)
                             (list (getancho imagenbit)(getalto imagenbit)
                                   (map (lambda (p) (list (gety p) (getx p) (abs(- (getbit p) 1)) (getD p) )) (getpixeles imagenbit)))
                             null)))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: INVERTCOLORRGB :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: invertColorRGB
Descripcion: Funcion que invierte los colores de cada pixel de una imagen pixmap
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagenRGB (image) 
Recorrido(Retorno): image 
|#

(define invertColorRGB (lambda (imagenrgb)
      (if (pixmap? imagenrgb)
          (list (getancho imagenrgb)(getalto imagenrgb)
                (map (lambda (p) (list (gety p)(getx p)(abs (- (getR p) 255))(abs(- (getG p) 255))(abs(- (getB p) 255))(getD p)))(getpixeles imagenrgb)))
          null)))


; --------- Tipo de Funcion: modificador ------------
;Funcion que incorpora la funcion selector dentro de la funcion modificador. Dom: funcion1 (procedure) X funcion2 (procedure). Rec: procedure
(define incCh (lambda (funcion1 funcion2)
                (compose funcion2 funcion1)))

; --------- Tipo de Funcion: modificador ------------ :::::::::::::::::::::::::::::::::: ADJUSTCHANNEL :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
               
#| Documentacion: adjustChannel
Descripcion: Función que permite ajustar cualquier canal de una imagen con pixeles pixrgb-d.
Tipo de algoritmo/estrategia: No aplica
Dominio: f1 (funcion selectora del canal) X f2 (función modificadora del canal) X f3 (operación a realizar sobre el canal). X Recibe de forma currificada la imagen.
Recorrido(Retorno): image tipo pixrgb-d 
|#


(define adjustChannel (lambda (f1 f2 f3)
                        (lambda (imagen)
                          
        ; Funcion que aplica las funciones dependiendo del canal escogido. Dom: funcion1 X funcion2 X funcion3 X pixelrgb(pixrgb-d)                                            
        (define aplicarfunciones (lambda (funcion1 funcion2 funcion3 pixelrgb)
            ;Funcion que une las funciones y lo ejecuta en el pixel. Dom: funcion3 X funcion1 X funcion2 .X pixelrgb (pixrgb-d)              
            (define ajustarcanal ((funcion3 funcion1 funcion2) pixelrgb))

                          (if (equal? funcion1 getR)
                              (list (gety pixelrgb)(getx pixelrgb) ajustarcanal (getG pixelrgb)(getB pixelrgb)(getD pixelrgb))

                              (if (equal? funcion1 getG)
                                  (list (gety pixelrgb)(getx pixelrgb)(getR pixelrgb) ajustarcanal (getB pixelrgb)(getD pixelrgb))

                                  (if (equal? funcion1 getB)
                                      (list (gety pixelrgb)(getx pixelrgb)(getR pixelrgb)(getG pixelrgb) ajustarcanal (getD pixelrgb))

                                      (if (equal? funcion1 getD)
                                          (list (gety pixelrgb)(getx pixelrgb)(getR pixelrgb)(getG pixelrgb)(getB pixelrgb)  ajustarcanal)
                                          null))))))

         (if (pixmap? imagen)
             ; Aqui finalmente se llaman a las funciones y se apluican devolviendo la imagen modificada.    
             (list (getancho imagen)(getalto imagen)(map (lambda(p) (aplicarfunciones f1 f2 f3 p)) (getpixeles imagen)))  
             null))))

; --------- Tipo de Funcion: Modificador ------------ :::::::::::::::::::::::::::::::::: EDIT :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: edit
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

;-------- Necesarios para el image->string ---------------

; --------- Tipo de Funcion: Otros ------------
;Funcion que pasa el color del pixel a string. Dom: pixel (pixbit-rgb-hex-d). Rec: string
(define colorastring (lambda (pixel)
                       (if (pixbits? pixel)
                           (string-append (number->string (getbit pixel)) " ") 
                           (if (pixrgbs? pixel)
                               (string-append (gethex (pixelrgbahex pixel)) " ")
                               (if (pixhexs? pixel)
                                   (string-append (gethex pixel) " ")
                               null)))))

                      
; --------- Tipo de Funcion: Otros ------------
; Funcion que incorpora saltos de linea en el string. Tipo de recursion: De cola.
; Dom: x (int) X y (int) X lista (list) (el dominio son los componentes de una imagen por separado). Rec: list
(define setSaltosdelinea (lambda (x y lista)                                  
                    (if (null? (cdr lista))                        
                        (cons (colorastring (car lista)) null)                                                  
                        (if (= (getx (car lista)) (- x 1))                                                
                            (append (list (colorastring (car lista)) "\n") (setSaltosdelinea x y (cdr lista))); Recursion de cola. No se dejan procedimientos sin realizar en el camino

                            (append (list (colorastring (car lista))) (setSaltosdelinea x y (cdr lista)))))))  ; Recursion

; --------- Tipo de Funcion: Otros ------------
; Funcion que une los pixeles en un solo string. Dom: listastrings (list). Rec: string
(define unirpixs (lambda (listastrings)
             
                  ;Funcion que une en un solo string todos los strings de una lista. Tipo de recursion: De cola. Dom: listastrings (list). Rec: string
                   (define unirpixs2 (lambda (listastrings)

                                       (if (null? (cdr listastrings))

                                           (car listastrings)
                                       
                                           (string-append (car listastrings) (unirpixs2 (cdr listastrings))) ) )) ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
                   (unirpixs2 listastrings)))

; --------- Tipo de Funcion: Otros ------------
; Funcion que transforma una imagen bitmap a string en un formato el cual es la representacion visual de una imagen. Dom: imagen. Rec: string
(define pixbit->string (lambda (imagen)
                           (unirpixs (setSaltosdelinea (getancho imagen) (getalto imagen) (getpixeles imagen)))))

; --------- Tipo de Funcion: Otros ------------
; Funcion que transforma una imagen hexmap a string en un formato el cual es la representacion visual de una imagen. Dom: imagen. Rec: string
(define pixhex->string (lambda (imagen)
                           (unirpixs (setSaltosdelinea (getancho imagen) (getalto imagen) (getpixeles imagen)))))

; --------- Tipo de Funcion: Otros ------------
; Funcion que transforma una imagen pixmap a string en un formato el cual es la representacion visual de una imagen. Dom: imagen. Rec: string
(define pixrgb->string (lambda (imagen)
                         (unirpixs (setSaltosdelinea (getancho imagen) (getalto imagen) (getpixeles imagen)))))


; --------- Tipo de Funcion: Otros ------------ :::::::::::::::::::::::::::::::::: IMAGE->STRING :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: image->string
Descripcion: Función que transforma una imagen a una representación string. 
Tipo de algoritmo/estrategia: Fuerza bruta debido a que realiza un proceso u otro dependiendo del tipo de pixeles
Dominio(Argumento de entrada): imagen (image) X funcion (procedure)
Recorrido(Retorno): string
|#

(define image->string (lambda (imagen funcion)
                        (if (null? (list-ref imagen 2))
                            null
                          (if (and (bitmap? imagen) (equal? pixbit->string funcion))
                              (funcion imagen)
                              (if (and(hexmap? imagen) (equal? pixhex->string funcion))
                                  (funcion imagen)
                                  (if (and(pixmap? imagen) (equal? pixrgb->string funcion))
                                      (funcion imagen)
                                      null))))))


; --------- Necesarios para depthLayers -----------

; --------- Tipo de Funcion: Otros ------------
; Funcion que obtiene todas las profundades de una imagen. Dom: imagen (image). Rec: list
(define obtenerprofundidades (lambda (imagen)
                               (delrepe (map (lambda (p) (car(reverse p))) (getpixeles imagen)))))

; --------- Tipo de Funcion: Otros ------------
; Funcion que devuelve un pixel si este correspone a cierta profundidad. Dom: prof (int) X pixel (pixbit-rgb-hex-d). Rec: pixel
(define obtenerpixelesprofundidad (lambda (prof pixel)                                
                                    (if (= prof (car(reverse pixel)))
                                        pixel
                                        null)))

; --------- Tipo de Funcion: Otros ------------
; Funcion que obtiene una lista con los pixeles de una profundidad. Dom: listapixeles (list) X listaprof (list) X cont (int). Rec: list
(define obtenerlistapixelesprof (lambda (listapixeles listaprof cont)

   (if (= cont (- (length listaprof) 1))
                                      
       (cons (map (lambda (p) (obtenerpixelesprofundidad (list-ref listaprof cont) p)) listapixeles) null)
       
       (cons (map (lambda (p) (obtenerpixelesprofundidad (list-ref listaprof cont) p)) listapixeles) (obtenerlistapixelesprof listapixeles listaprof (+ cont 1))))))

; --------- Tipo de Funcion: Otros ------------
; Funcion que devuelve una imagen por una respectiva profundidad. Dom: imagen (image). Rec: list
(define devolverimagenesporprof (lambda (imagen)
                           (obtenerlistapixelesprof (getpixeles imagen) (obtenerprofundidades imagen) 0)))

; --------- Tipo de Funcion: Otros ------------
; Funcion que añade el alto y ancho a la lista de pixeles. Dom: imagen (image) X pixeles (list). Rec: image
(define añadiraltoancho (lambda (imagen pixeles)
                   (append (cons (getancho imagen) null) (cons (getalto imagen) null) (cons pixeles null))))

; --------- Tipo de Funcion: Otros ------------
; Funcion que añade el alto y ancho a todas las imagenes de una lista de imagenes. Dom: image. Rec: list
(define añadiraltoanchoimagenes (lambda (imagen)
                           (map (lambda (p) (añadiraltoancho imagen p)) (devolverimagenesporprof imagen))))

; --------- Tipo de Funcion: modificador ------------
; Funcion que rellena una imagen con pixeles blancos tipo pixbit-d. Dom: image. Rec: list
(define rellenarpixblancosbit (lambda (imagen)
                             ; Funcion que obtiene la profunidad de una imagen. Dom: imagen (image). Rec: int
                             (define obtenerprofundidad (lambda (imagen)                                                   
                                                   (car(reverse(car(reverse(filter (lambda (p) (not(null? p))) (getpixeles imagen))))))))
                             ; Define la profunidad 
                             (define profundidad (obtenerprofundidad imagen))
                             ; Define listapixeles
                             (define listapixeles (getpixeles imagen))
                             ; Define largo
                             (define largo (length listapixeles))
                         ; Funcion que rellena la imagen con pixeles blancos. Tipo de recursion: De cola.
                         ; Dom: imagen (image) X listapixeles (list) X profundidad (int) X largo (int) X cont (int). Rec: list
                             (define rellenar (lambda (imagen listapixeles profundidad largo cont)
                               (if (= cont largo)
                                listapixeles
                                (if (null? (car listapixeles))
                                   (append (cons (append '(0 0 1) (cons profundidad null)) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1)) )
                                                           ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
                                 (append (cons (car listapixeles) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1)) ) ) ) )) ; Recursion

                            (rellenar imagen listapixeles profundidad largo 0)))

; --------- Tipo de Funcion: modificador ------------
; Funcion que rellena una imagen con pixeles blancos tipo pixrgb-d. Dom: image. Rec: list
(define rellenarpixblancospix (lambda (imagen)
                            ; Funcion que obtiene la profunidad de una imagen. Dom: imagen (image). Rec: int
                             (define obtenerprofundidad (lambda (imagen)                                                   
                                                   (car(reverse(car(reverse(filter (lambda (p) (not(null? p))) (list-ref imagen 2))))))))

                             ; Define la profunidad 
                             (define profundidad (obtenerprofundidad imagen))
                             ; Define listapixeles
                             (define listapixeles (getpixeles imagen))
                             ; Define largo
                             (define largo (length listapixeles))
                                
                         ; Funcion que rellena la imagen con pixeles blancos. Tipo de recursion: De cola.
                         ; Dom: imagen (image) X listapixeles (list) X profundidad (int) X largo (int) X cont (int). Rec: list
                             (define rellenar (lambda (imagen listapixeles profundidad largo cont)
                               (if (= cont largo)
                                listapixeles               
                                (if (null? (car listapixeles)) 
                                (append (cons (append '(0 0 255 255 255) (cons profundidad null)) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1))) ;Recursion
                                 (append (cons (car listapixeles) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1)) ) ) ) )) ;Recursion

                            (rellenar imagen listapixeles profundidad largo 0)))

; --------- Tipo de Funcion: modficador ------------
; Funcion que rellena una imagen con pixeles blancos tipo pixhex-d. Dom: image. Rec: list
(define rellenarpixblancoshex (lambda (imagen)
                           ; Funcion que obtiene la profunidad de una imagen. Dom: imagen (image). Rec: int
                             (define obtenerprofundidad (lambda (imagen)                                                 
                                                   (car(reverse(car(reverse(filter (lambda (p) (not(null? p))) (list-ref imagen 2))))))))

                             ; Define la profunidad 
                             (define profundidad (obtenerprofundidad imagen))
                             ; Define listapixeles
                             (define listapixeles (getpixeles imagen))
                             ; Define largo
                             (define largo (length listapixeles))
                                
                         ; Funcion que rellena la imagen con pixeles blancos. Tipo de recursion: De cola.
                         ; Dom: imagen (image) X listapixeles (list) X profundidad (int) X largo (int) X cont (int). Rec: list
                             (define rellenar (lambda (imagen listapixeles profundidad largo cont)
                               (if (= cont largo)
                                listapixeles             
                                (if (null? (car listapixeles)) 
                                 (append (cons (append '(0 0 "#FFFFFF") (cons profundidad null)) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1)) ) ;Recursion
                                 (append (cons (car listapixeles) null) (rellenar imagen (cdr listapixeles) profundidad largo (+ cont 1)) ) ) ) )) ;Recursion

                            (rellenar imagen listapixeles profundidad largo 0)))

; --------- Tipo de Funcion: Otros ------------
;Funcion que devuelve una imagen bitmap con los pixeles blancos rellenados. Dom: imagen (image). Rec: image
(define devolverimagenconpixblancosbit (lambda (imagen)
                                         (list (getancho imagen) (getalto imagen) (rellenarpixblancosbit imagen))))

; --------- Tipo de Funcion: Otros ------------
;Funcion que devuelve una imagen pixmap con los pixeles blancos rellenados. Dom: imagen (image). Rec: image
(define devolverimagenconpixblancospix (lambda (imagen)
                                      (list (getancho imagen) (getalto imagen) (rellenarpixblancospix imagen))))

; --------- Tipo de Funcion: Otros ------------
;Funcion que devuelve una imagen hexmap con los pixeles blancos rellenados. Dom: imagen (image). Rec: image
(define devolverimagenconpixblancoshex (lambda (imagen)
                                      (list (getancho imagen) (getalto imagen) (rellenarpixblancoshex imagen))))

; --------- Tipo de Funcion: modificador ------------
; Funcion que corrige los pixeles blancos con sus coordenadas erroneas de una imagen. Dom: imagen (image). Rec: image
(define corregirpixeles (lambda (imagen)
          ; Define Y maximo
          (define maxY (-(getalto imagen)1))
          ; Define X maximo               
          (define maxX (-(getancho imagen)1))
          ; Define lista de pixeles             
          (define listapixeles (getpixeles imagen))
                          
          ;Funcion que corrige las coordenadas y,x de una imagen.Tipo de recursion: De cola. Dom: listapixeles (list) X conty (int) X contx (int). Rec: imagen
          (define corregir (lambda (listapixeles conty contx)
           (if (not(null? (cdr listapixeles)))                       
            (if (and (= conty maxY) (= contx maxX))
                (cons (append (cons conty null) (cons contx null) (cddr (list-ref listapixeles 0))) null)
                (if (= contx maxX)
                  (append (cons (append (cons conty null)(cons contx null)(rest(rest(list-ref listapixeles 0)))) null)(corregir (cdr listapixeles) (+ conty 1) 0)) 
                    (if (= conty maxY)                                                                   ; Recursion de cola. No se dejan procedimientos sin realizar en el camino
                    (append(cons(append (cons conty null)(cons contx null)(rest(rest(list-ref listapixeles 0)))) null) (corregir (cdr listapixeles) conty (+ contx 1))) ;Recursion                             
                    (append(cons(append (cons conty null)(cons contx null)(rest(rest(list-ref listapixeles 0)))) null) (corregir (cdr listapixeles) conty (+ contx 1))) ;Recursion
                        )))                        
            (cons (append (cons conty null) (cons contx null) (rest(rest (list-ref listapixeles 0)))) null)
            )))
                     
                          (corregir listapixeles 0 0)))

; --------- Tipo de Funcion: Otros ------------
;Funcion que corrige todas las imagenes de una lista de imagen dependiendo del tipo de imagen. Dom: imagen. Rec: list
(define corregirtodos (lambda (imagen)
                        (cond
                          [(bitmap? imagen)(map (lambda (i) (corregirpixeles (devolverimagenconpixblancosbit i))) (añadiraltoanchoimagenes imagen))]
                          [(pixmap? imagen)(map (lambda (i) (corregirpixeles (devolverimagenconpixblancospix i))) (añadiraltoanchoimagenes imagen))]
                          [(hexmap? imagen)(map (lambda (i) (corregirpixeles (devolverimagenconpixblancoshex i))) (añadiraltoanchoimagenes imagen))]
                          )))

; --------- Tipo de Funcion: Otros ------------ :::::::::::::::::::::::::::::::::: DEPTHLAYERS :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#| Documentacion: depthLayers
Descripcion: Función que permite separar una imágen en capas en base a la profundidad en que se sitúan los pixeles.
Además, en las imágenes resultantes se sustituyen los píxeles que se encuentran en otro nivel de profundidad por píxeles blancos
Tipo de algoritmo/estrategia: Fuerza bruta, debido a que en la funcion corregirtodos, realiza un proceso u otro dependiendo del tipo de pixeles
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): list
|#

(define depthLayers (lambda (imagen)
                      (map (lambda (i) (añadiraltoancho imagen i)) (corregirtodos imagen))))

; --------- Tipo de funcion: Modificador -------- :::::::::::::::::::::::::: DECOMPRESS :::::::::::::::::::::::::::::::::::

; Esta funcion no se realizo, pero se definirá para que en el archivo MAIN.rkt deje correr todas las funciones, de esa forma no efectuarian errores,
; pero los ejemplos que se aplique decompress estarian erroneos.

; La siguiente documentacion es lo que se esperaba de esta funcion.

#| Documentacion: decompress
Descripcion: Función que permite descomprimir una imágen comprimida.
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): imagen (image)
Recorrido(Retorno): image
|#

(define decompress (lambda (imagen)
                     imagen))

; ------------------------------------------------ FIN -----------------------------------------------------------------------------------

; Finalmente se exportan todas las funciones de este archivo para afuera de este, con el fin de ejecutar correctamente el archivo MAIN.rkt

(provide (all-defined-out))







