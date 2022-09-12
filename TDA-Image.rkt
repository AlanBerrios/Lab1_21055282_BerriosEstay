#lang racket

;Lso siguientes tdas son necesarios para crear el TDA image
  
;TDA de pertenecia

#| Documentacion:  TDA-int>0?
Descripcion: Verifica si la entrada es un entero y si es mayor o igual a 0
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): xx (int) 
Recorrido(Retorno): Booleano
|#

(define int>0?
  (lambda (xx)
       (if (and (integer? xx)
            (>= xx 0)) true false)))

#| Documentacion: TDA-bit?
Descripcion: Verifica si la entrada es igual a 0 o 1
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): bitt (int) 
Recorrido(Retorno): Booleano
|#
  
(define bit?
  (lambda (bitt)
       (if (and (integer? bitt)
            (or (= bitt 0)(= bitt 1))) true false)))

#| Documentacion: TDA-rgb?
Descripcion: Verifica si la entrada es un entero mayor o igual a 0 y menor o igual a 255
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): C (int) 
Recorrido(Retorno): Booleano
|#

(define rgb?
  (lambda (C)
       (if (and (integer? C)
            (>= C 0)
            (<= C 255)) true false)))

#| Documentacion: TDA-hex?
Descripcion: Verifica si la entrada es un string
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): hexx (string) 
Recorrido(Retorno): Booleano
|#

(define hex?
  (lambda (hexx)
       (if (string? hexx) true false)))

; TDA Constructores

#| Documentacion: TDA-pixbit-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 4to int>0?, y 3ro bit?. si es asi, crea un pixbit-d
Tipo de algoritmo/estrategia: No aplica/Desconozco.
Dominio(Argumento de entrada): x (int) X y (int) X bit (int) X depth (int)
Recorrido(Retorno): pixbit-d '(x y bit depth)
|#

(define pixbit-d
  (lambda (x y bit depth)
    (if (and (int>0? x)
             (int>0? y)
             (bit? bit)
             (int>0? depth))
         (list x y bit depth)
         null)))

#| Documentacion: TDA-pixrgb-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 6to int>0?, y 3ro, 4to y 5to rgb?. si es asi, crea un pixrgb-d
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): x (int) X y (int) X r (int) X g (int) X b (int) X depth (int)
Recorrido(Retorno): pixrgb-d '(x y r g b depth)
|#

(define pixrgb-d
  (lambda (x y r g b depth)
    (if (and (int>0? x)
             (int>0? y)
             (rgb? r)
             (rgb? g)
             (rgb? b)
             (int>0? depth))
         (list x y r g b depth)
         null)))

#| Documentacion: TDA-pixhex-d
Descripcion: Verifica si los elementos de entrada cumplen con: 1ero, 2do y 4to int>0?, y 3ro hex?. si es asi, crea un pixhex-d
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): x (int) X y (int) X hex (string) X depth (int)
Recorrido(Retorno): pixhex-d '(x y hex depth)
|#

(define pixhex-d
  (lambda (x y hex depth)
    (if (and (int>0? x)
             (int>0? y)
             (hex? hex)
             (int>0? depth))
         (list x y hex depth)
         null)))

#| Documentacion: TDA-pixbits?
Descripcion: Verifica si el elemento de entrada es un pixbit-d
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): lista (pixbit-d)
Recorrido(Retorno): Booleano
|#

(define pixbits? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))                       
                       (if (and (equal? (length lista) 4) (bit? (list-ref lista 2)))
                           true false)
                       false )))

#| Documentacion: TDA-pixrgbs?
Descripcion: Verifica si el elemento de entrada es un pixrgb-d
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): lista (pixrgb-d)
Recorrido(Retorno): Booleano
|#

(define pixrgbs? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))                     
                       (if (and (equal? (length lista) 6)
                                (rgb? (list-ref lista 2))
                                (rgb? (list-ref lista 3))
                                (rgb? (list-ref lista 4)))                                                                      
                           true false)
                       false )))

#| Documentacion: TDA-pixhexs?
Descripcion: Verifica si el elemento de entrada es un pixhex-d
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): lista (pixhex-d)
Recorrido(Retorno): Booleano
|#

(define pixhexs? (lambda (lista)
                   (if (and (list? lista)(not (null? list?)))
                       (if (and (equal? (length lista) 4) (hex? (list-ref lista 2)))
                           true false)
                       false)))

;TDA de pertenencia

#| Documentacion: TDA-homologo?
Descripcion: Verifica si los elementos de la lista de entrada son de un mismo tipo de pixel (pixbit-d,pixrgb-d o pixhex-d)
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): Lista de n-variables (pixbit-d | pixrgb-d | pixhex-d)
Recorrido(Retorno): Booleano
|#

(define homologo? (lambda (lista)
  (if (or (andmap pixbits? lista)
          (andmap pixrgbs? lista)
          (andmap pixhexs? lista))
      true false)))

;TDA constructor
; TDA Principal

#| Documentacion: TDA-image
Descripcion: verifica que los dos primeros elementos cumplen con int>0? y que el tercer elemento, la lista de n-variables, cumpla con homologo?
Tipo de algoritmo/estrategia: No aplica/Desconozco
Dominio(Argumento de entrada): width (int) X height (int) X pixeles (de n-variables) (pixbit-d | pixrgb-d | pixhex-d)
Recorrido(Retorno): Booleano
|#

(define image (lambda (width heigth . pixeles)
    (if (and (int>0? width) 
             (int>0? heigth)
             (<= (length pixeles)(* width heigth))
             (homologo? pixeles))
        (list width heigth pixeles)
        null )))

(provide (all-defined-out))