#lang racket

(require "TDA-Image.rkt")
(require "TDA-pixmap.rkt")
(require "TDA-edit.rkt")

;Funcion que obtiene el canal R de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getR (lambda (pixelrgb)
               (list-ref pixelrgb 2)))

;Funcion que obtiene el canal G de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getG (lambda (pixelrgb)
               (list-ref pixelrgb 3)))

;Funcion que obtiene el canal B de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getB (lambda (pixelrgb)
               (list-ref pixelrgb 4)))

;Funcion que obtiene el canal D de un pixrgb-d. Dom: pixelrgb (pixrgb-d). Rec: int
(define getD (lambda (pixelrgb)
               (list-ref pixelrgb 5)))

;Funcion modificador del canal R. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setR (lambda (R)
               (if (= R 255)
                    R
                    (+ R 1))))

;Funcion modificador del canal G. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setG (lambda (G)
               (if (= G 255)
                    G
                    (+ G 1))))

;Funcion modificador del canal B. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setB (lambda (B)
               (if (= B 255)
                    B
                    (+ B 1))))

;Funcion modificador del canal D. Dom: Dom: pixelrgb (pixrgb-d). Rec: int
(define setD (lambda (D)
                    (+ D 1)))

;Funcion que incorpora la funcion selector dentro de la funcion modificador. 
(define incCh (lambda (funcion1 funcion2)
                (compose funcion2 funcion1)))

#| Documentacion: TDA-adjustChannel
Descripcion: Función que permite ajustar cualquier canal de una imagen con pixeles pixrgb-d.
Tipo de algoritmo/estrategia: No aplica
Dominio(Argumento de entrada): f1 (funcion selectora del canal) X f2 (función modificadora del canal) X f3 (operación a realizar sobre el canal)
. X Recibe de forma currificada la imagen.
Recorrido(Retorno): image tipo pixrgb-d 
|#


(define adjustChannel (lambda (f1 f2 f3)
                        (lambda (imagen)
                          
        ; Funcion que aplica las funciones dependiendo del canal escogido. Dom: funcion1 X funcion2 X funcion3 X pixelrgb(pixrgb-d)                                            
        (define aplicarfunciones (lambda (funcion1 funcion2 funcion3 pixelrgb)
            ;Funcion que une las funciones y lo ejecuta en el pixel. Dom: funcion3 X funcion1 X funcion2 .X pixelrgb (pixrgb-d)              
            (define ajustarcanal ((funcion3 funcion1 funcion2) pixelrgb))

                          (if (equal? funcion1 getR)
                              (list (list-ref pixelrgb 0)(list-ref pixelrgb 1) ajustarcanal (list-ref pixelrgb 3)(list-ref pixelrgb 4)(list-ref pixelrgb 5))

                              (if (equal? funcion1 getG)
                                  (list (list-ref pixelrgb 0)(list-ref pixelrgb 1)(list-ref pixelrgb 2) ajustarcanal (list-ref pixelrgb 4)(list-ref pixelrgb 5))

                                  (if (equal? funcion1 getB)
                                      (list (list-ref pixelrgb 0)(list-ref pixelrgb 1)(list-ref pixelrgb 2) (list-ref pixelrgb 3) ajustarcanal (list-ref pixelrgb 5))

                                      (if (equal? funcion1 getD)
                                          (list (list-ref pixelrgb 0)(list-ref pixelrgb 1)(list-ref pixelrgb 2) (list-ref pixelrgb 3) (list-ref pixelrgb 4)  ajustarcanal)
                                          null))))))

         (if (pixmap? imagen)
             ; Aqui finalmente se llaman a las funciones y se apluican devolviendo la imagen modificada.    
             (list (list-ref imagen 0)(list-ref imagen 1)(map (lambda(p) (aplicarfunciones f1 f2 f3 p)) (list-ref imagen 2)))  
             null))))

(provide (all-defined-out))