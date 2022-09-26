#lang racket

(require "TDA-pixbit-d_21055282_BerriosEstay.rkt")
(require "TDA-pixrgb-d_21055282_BerriosEstay.rkt")
(require "TDA-pixhex-d_21055282_BerriosEstay.rkt")
(require "TDA-image_21055282_BerriosEstay.rkt")


;Pruebas del documento del laboratorio

(display "\n")
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255 1)))
img1                                                 ;Resultado esperado: '(2 2 ((0 0 255 0 0 10) (0 1 0 255 0 20) (1 0 0 0 255 10) (1 1 255 255 255 1)))
(display "\n")

(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))
img2                                                  ;Resultado esperado: '(2 2 ((0 0 0 10) (0 1 1 20) (1 0 1 10) (1 1 0 255)))
(display "\n")

(define img3 (imgRGB->imgHex img1))
img3                                          ;Resultado esperado: '(2 2 ((0 0 "#FF0000" 10) (0 1 "#00FF00" 20) (1 0 "#0000FF" 10) (1 1 "#FFFFFF" 1)))
(display "\n")
(display "--------------------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(display (image->string img1 pixrgb->string))               ;Resultado esperado: #FF0000 #00FF00 \n #0000FF #FFFFFF 
(display "\n")
(display "\n")
(display (image->string img2 pixbit->string))               ;Resultado esperado: 0 1 \n 1 0
(display "\n")
(display "\n")
(display "----------------------- bitmap? ---------------------------------------------------------------------")
(display "\n")
(display "\n")

(bitmap? img1)         ;Resultado esperado: #f
(bitmap? img2)         ;Resultado esperado: #t
(bitmap? img3)         ;Resultado esperado: #f
(display "\n")
(display "-------------------------- pixmap? ------------------------------------------------------------------")
(display "\n")
(display "\n")

(pixmap? img1)          ;Resultado esperado: #t
(pixmap? img2)           ;Resultado esperado: #f
(pixmap? img3)           ;Resultado esperado: #f
(display "\n")
(display "--------------------------- hexmap? ----------------------------------------------------------------")
(display "\n")
(display "\n")

(hexmap? img1)          ;Resultado esperado: #f
(hexmap? img2)          ;Resultado esperado: #f
(hexmap? img3)          ;Resultado esperado: #t
(display "\n")
(display "---------------------------- compressed? ----------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? img1)          ;Resultado esperado: #f
(compressed? img2)          ;Resultado esperado: #f
(compressed? img3)          ;Resultado esperado: #f
(display "\n")
(display "------------------------------ flipH --------------------------------------------------------------")
(display "\n")
(display "\n")

(flipH img1)        
(flipH img2)         
(flipH img3)        
(display "\n")
(display "------------------------------ flipV --------------------------------------------------------------")
(display "\n")
(display "\n")

(flipV img1)        
(flipV img2)          
(flipV img3)         
(display "\n")
(display "------------------------- crop -------------------------------------------------------------------")
(display "\n")
(display "\n")

(define img4 (crop img1 0 0 0 0))          ;Resultado esperado: una imágen con un pixel
img4
(define img5 (crop img2 0 0 0 1))          ;Resultado esperado: una imágen con dos pixeles
img5
(define img6 (crop img1 0 1 1 1))          ;Resultado esperado: una imágen con dos pixeles
img6
(define img7 (crop img2 0 0 1 1))          ;Resultado esperado: la misma imagen
img7
(display "\n")
(display "------------------------------ histogram --------------------------------------------------------------")
(display "\n")
(display "\n")

(histogram img1)              ;Resultado esperado: deberia devolver que cada color tiene un pixel
(histogram img2)              ;Resultado esperado: deberia devolver 2 bits 0 y 2 bits 1
(histogram img3)              ;Resultado esperado: deberia devolver que cada color tiene un pixel
(histogram img4)              ;Resultado esperado: debería retornar un solo color con 1 repeticion
(histogram img5)              ;Resultado esperado: debería retornar un dos colores con 1 repeticion
(histogram img6)              ;Resultado esperado: debería retornar un dos colores con 1 repeticion
(histogram img7)              ;Resultado esperado: deberia devolver 2 bits 0 y 2 bits 1
(display "\n")
(display "-------------------------------- rotate90 ------------------------------------------------------------")
(display "\n")
(display "\n")

(define img18 (rotate90 img1))
img18                          
(define img19 (rotate90 img2))
img19                          
(define img20 (rotate90 img3))
img20                          
(define img21 (rotate90 img4))
img21                          
(define img22 (rotate90 img5)) 
img22                          
(define img23 (rotate90 img6))
img23                          
(define img24 (rotate90 img7))
img24                          
(display "\n")
(display "----------------------------------- compress ---------------------------------------------------------")
(display "\n")
(display "\n")
    
(define img8 (compress img1))    
img8                          ;Resultado esperado: imagen de 3 pixeles, se elimina el 255 0 0
(define img9 (compress img2))
img9                          ;Resultado esperado: imagen de 2 pixeles, se elimina el 0
(define img10 (compress img3))
img10                          ;Resultado esperado: imagen de 3 pixeles
(define img11 (compress img4))
img11                          ;Resultado esperado: imagen sin pixeles
(define img12 (compress img5))
img12                          ;Resultado esperado: imagen de 1 pixel
(define img13 (compress img6))
img13                          ;Resultado esperado: imagen de 1 pixel
(define img14 (compress img7))
img14                          ;Resultado esperado: imagen de 2 pixeles
(display "\n")
(display "-------------------------------- compressed? ------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? img8)                           ;Resultado esperado: #t
(compressed? img9)                           ;Resultado esperado: #t
(compressed? img10)                           ;Resultado esperado: #t
(compressed? img11)                           ;Resultado esperado: #t
(compressed? img12)                           ;Resultado esperado: #t
(compressed? img13)                           ;Resultado esperado: #t
(compressed? img14)                           ;Resultado esperado: #t
(display "\n")
(display "-------------------- invertColorbit - invertColorRGB ---------------------------------------------------------")
(display "\n")
(display "\n")

(define img15 (edit invertColorBit img2))   
img15                                    ;Resultado esperado: img2 con sus bits invertidos
(define img16 (edit invertColorRGB img1))
img16                                    ;Resultado esperado: img1 con sus canales RGB invertidos
(display "\n")
(display "---------------------- adjustChannel ----------------------------------------------------------------------")
(display "\n")
(display "\n")

(define img33 (edit (adjustChannel getR setR incCh) img1))
img33                                    ;Resultado esperado: img1 con su canal R aumentado en 1
(define img34 (edit (adjustChannel getG setG incCh) img1))
img34                                    ;Resultado esperado: img1 con su canal G aumentado en 1
(define img35 (edit (adjustChannel getB setB incCh) img1))
img35                                    ;Resultado esperado:  img1 con su canal B aumentado en 1
(display "\n")
(display "----------------------------- image->string ---------------------------------------------------------------")
(display "\n")
(display "-------------------  imagenes no comprimidas ---------------------------------------------------------------")
(display "\n")
(display "\n")
(display (image->string img1 pixrgb->string))     ;Resultado esperado: #FF0000 #00FF00 \n #0000FF #FFFFFF 
(display "\n")
(display "\n")
(display (image->string img2 pixbit->string))     ;Resultado esperado: 0 1 \n 1 0
(display "\n")
(display "\n")
(display (image->string img3 pixhex->string))     ;Resultado esperado: #FF0000 #00FF00 \n #0000FF #FFFFFF
(display "\n")
(display "\n")
(display (image->string img4 pixrgb->string))     ;Resultado esperado: #FF0000
(display "\n")
(display "\n")
(display (image->string img5 pixbit->string))    ;Resultado esperado: 0 \n 1
(display "\n")
(display "\n")
(display (image->string img6 pixbit->string))    ;Resultado esperado: vacio
(display "\n")
(display "\n")
(display (image->string img7 pixbit->string))    ;Resultado esperado: 0 1 \n 1 0
(display "\n")
(display "\n")
(display "------------------- imagenes comprimidas -------------------------------------------------------------------------")
(display "\n")
(display "\n")

(display (image->string img8 pixrgb->string))    ;Resultado esperado:  #00FF00 \n #0000FF #FFFFFF 
(display "\n")
(display "\n")
(display (image->string img9 pixbit->string))     ;Resultado esperado: 1 \n 1
(display "\n")
(display "\n")
(display (image->string img10 pixhex->string))    ;Resultado esperado: #00FF00 \n #0000FF #FFFFFF
(display "\n")
(display "\n")
(display (image->string img11 pixrgb->string))    ;Resultado esperado: vacio
(display "\n")
(display "\n")
(display  (image->string img12 pixbit->string))   ;Resultado esperado: 1
(display "\n")
(display "\n")
(display (image->string img13 pixrgb->string))    ;Resultado esperado: #FFFFFF
(display "\n")
(display "\n")
(display (image->string img14 pixbit->string))    ;Resultado esperado: 1 \n 1
(display "\n")
(display "\n")
(display "---------------------------- imagenes no comprimidas --------------------------------------------------")
(display "\n")
(display "\n")


(display (image->string img33 pixrgb->string)) ;Aqui deberia ser (display (image->string img15 pixrgb->string)) pero img15 es pixbit. No se corrigio en el docs
(display "\n")                                 ;Resultado esperado: #FF0000 #01FF00 \n #0100FF #FFFFFF 
(display "\n")
(display (image->string img34 pixrgb->string)) ;Aqui deberia ser (display (image->string img16 pixrgb->string)) pero img16 es pixbit. No se corrigio en el docs
(display "\n")                                 ;Resultado esperado: #FF0100 #00FF00 \n #0001FF #FFFFFF 
(display "\n")
(display (image->string img35 pixrgb->string)) ;Aqui deberia ser (display (image->string img17 pixrgb->string)) pero img17 no esta definida. No se corrigio en el docs
(display "\n")                                 ;Resultado esperado: #FF0001 #00FF01 \n #0000FF #FFFFFF
(display "\n")
(display (image->string img18 pixrgb->string)) ;Resultado esperado: #0000FF #FF0000 \n #FFFFFF #00FF00
(display "\n")
(display "\n")
(display (image->string img19 pixbit->string)) ;Resultado esperado: 1 0 \n 0 1
(display "\n")
(display "\n")
(display (image->string img20 pixhex->string)) ;Resultado esperado: #0000FF #FF0000 \n #FFFFFF #00FF00
(display "\n")
(display "\n")
(display (image->string img21 pixrgb->string)) ;Resultado esperado: #FF0000
(display "\n")
(display "\n")
(display (image->string img22 pixbit->string)) ;Resultado esperado: 1 \n 0
(display "\n")
(display "\n")
(display (image->string img23 pixrgb->string)) ;Resultado esperado: #0000FF #FFFFFF
(display "\n")
(display "\n")
(display (image->string img24 pixbit->string)) ;Resultado esperado: 1 0 \n 0 1
(display "\n")
(display "\n")
(display "-------------------------- depthLayers -------------------------------------------------------")
(display "\n")
(display "\n")

(depthLayers img1) ;Resultado esperado: lista de 3 imagenes 2x2 de profunidades 20, 10 y 1
(display "\n")
(display "\n")
(depthLayers img2) ;Resultado esperado: lista de 3 imagenes 2x2 de profunidades 20, 10 y 255
(display "\n")
(display "\n")
(depthLayers img3) ;Resultado esperado: lista de 3 imagenes 2x2 de profundidades 20, 10 y 1
(display "\n")
(display "\n")
(depthLayers img4) ;Resultado esperado: lista de 1 imagen 2x2 de profundidad 10
(display "\n")
(display "\n")
(depthLayers img5) ;Resultado esperado: lista de 1 imagen 1x1 de profundidad 10
(display "\n")
(display "\n")
(depthLayers img6) ;Resultado esperado: lista de 1  imagenes 1x2 de profunidad 10
(display "\n")
(display "\n")
(depthLayers img7) ;Resultado esperado: lista de 1  imagenes 1x2 de profunidades 20, 10 y 255
(display "\n")
(display "\n")
(display "---------------------------- decompress --------------------------------------------------------")
(display "\n")
(display "\n")
(define img25 (decompress img8)) ;Resultado esperado: img1
img25
(define img26 (decompress img9)) ;Resultado esperado: img2
img26
(define img27 (decompress img10)) ;Resultado esperado: img3
img27
(define img28 (decompress img11)) ;Resultado esperado: img4
img28
(define img29 (decompress img12)) ;Resultado esperado: img5
img29
(define img30 (decompress img13)) ;Resultado esperado: img6
img30
(define img31 (decompress img14)) ;Resultado esperado: img7
img31
(display "\n")
(display "\n")
(display "--------------------------------------------------------------------------------------------")
(display "\n")
(display "\n")
;las siguientes comparaciones deberían arrojar #t
(equal? img25 img1) 
(equal? img26 img2)
(equal? img27 img3)
(equal? img28 img4)
(equal? img29 img5)
(equal? img30 img6)
(equal? img31 img7)
(display "\n")
(display "\n")
(display "--------------------------------------------------------------------------------------------")
(display "\n")
(display "\n")
;las siguientes comparaciones deberían arrojar #f
(equal? img25 img2)
(equal? img26 img1)
(display "\n")
(display "\n")
(display "--------------------------- FIN SCRIPT DE PRUEBAS DEL ENUNCIADO -----------------------------------------------------------------")
(display "\n")
(display "\n")
(display "--------------------------- SCRIPT DE PRUEBAS: EJEMPLOS PROPIOS -----------------------------------------------------------------")
(display "\n")
(display "\n")
(display "------------------------------------ IMAGENES DE EJEMPLO -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(define imagen2x2bit (image 2 2 (pixbit-d 0 0 0 10)(pixbit-d 0 1 1 11)
                             (pixbit-d 1 0 1 12)(pixbit-d 1 1 0 13)))
imagen2x2bit ;Resultado esperado: imagen 2x2 bitmap
(display "\n")
(display "\n")

(define imagen3x3bit (image 3 3 (pixbit-d 0 0 1 11)(pixbit-d 0 1 1 11)(pixbit-d 0 2 0 13)
                             (pixbit-d 1 0 0 13)(pixbit-d 1 1 0 13)(pixbit-d 1 2 1 16)
                             (pixbit-d 2 0 1 16)(pixbit-d 2 1 1 16)(pixbit-d 2 2 0 11)))
imagen3x3bit ;Resultado esperado: imagen 3x3 bitmap
(display "\n")
(display "\n")

(define imagen4x4bit (image 4 4 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 11)(pixbit-d 0 2 1 11)(pixbit-d 0 3 1 10)
                             (pixbit-d 1 0 0 11)(pixbit-d 1 1 1 10)(pixbit-d 1 2 0 10)(pixbit-d 1 3 0 11)
                             (pixbit-d 2 0 1 10)(pixbit-d 2 1 0 10)(pixbit-d 2 2 0 11)(pixbit-d 2 3 1 10)
                             (pixbit-d 3 0 0 11)(pixbit-d 3 1 1 11)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 11)))
imagen4x4bit ;Resultado esperado: imagen 4x4 bitmap
(display "\n")
(display "\n")

(define imagen2x2rgb (image 2 2 (pixrgb-d 0 0 244 1 17 10)(pixrgb-d 0 1 242 23 22 11)
                                (pixrgb-d 1 0 250 9 10 12)(pixrgb-d 1 1 89 0 10 13)))
imagen2x2rgb ;Resultado esperado: imagen 2x2 pixmap
(display "\n")
(display "\n")

(define imagen3x3rgb (image 3 3 (pixrgb-d 0 0 244 1 17 11) (pixrgb-d 0 1 242 23 22 11)(pixrgb-d 0 2 242 23 22 16)
                                (pixrgb-d 1 0 250 9 10 13) (pixrgb-d 1 1 244 1 17 13) (pixrgb-d 1 2 89 0 10 16)
                                (pixrgb-d 2 0 242 23 22 16)(pixrgb-d 2 1 89 0 10 11)  (pixrgb-d 2 2 244 1 17 13)))
imagen3x3rgb ;Resultado esperado: imagen 3x3 pixmap
(display "\n")
(display "\n")

(define imagen4x4rgb (image 4 4 (pixrgb-d 0 0 255 255 255 10)(pixrgb-d 0 1 29 90 19 11)(pixrgb-d 0 2 1 1 1 11)  (pixrgb-d 0 3 2 2 2 10)
                                (pixrgb-d 1 0 4 5 6 11)      (pixrgb-d 1 1 7 8 9 10)   (pixrgb-d 1 2 90 1 90 10)(pixrgb-d 1 3 77 77 77 11)
                                (pixrgb-d 2 0 7 7 7 10)      (pixrgb-d 2 1 8 8 8 10)   (pixrgb-d 2 2 6 6 6 11)  (pixrgb-d 2 3 40 40 40 10)
                                (pixrgb-d 3 0 255 255 255 11)(pixrgb-d 3 1 0 0 7 11)   (pixrgb-d 3 2 1 1 2 10)  (pixrgb-d 3 3 1 5 5 11)))
imagen4x4rgb ;Resultado esperado: imagen 4x4 pixmap
(display "\n")
(display "\n")

(define imagen2x2hex (imgRGB->imgHex imagen2x2rgb))
imagen2x2hex ;Resultado esperado: imagen2x2rgb con sus colores en hexadecimal, imagen 2x2 hexmap
(display "\n")
(display "\n")

(define imagen3x3hex (imgRGB->imgHex imagen3x3rgb))
imagen3x3hex ;Resultado esperado:imagen3x3rgb con sus colores en hexadecimal, imagen 3x3 hexmap
(display "\n")
(display "\n")

(display "------------------------------------ bitmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")
(bitmap? imagen2x2bit) ;Resultado esperado: deberia ser true
(bitmap? imagen3x3bit) ;Resultado esperado: deberia ser true
(bitmap? imagen3x3rgb) ;Resultado esperado: deberia ser false
(display "\n")
(display "\n")

(display "\n")
(display "\n")

(display "------------------------------------ pixmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(pixmap? imagen2x2bit) ;Resultado esperado: deberia ser false
(pixmap? imagen2x2rgb) ;Resultado esperado: deberia ser true
(pixmap? imagen3x3hex) ;Resultado esperado: deberia ser false
(display "\n")
(display "\n")

(display "------------------------------------ hexmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(hexmap? imagen4x4bit) ;Resultado esperado: deberia ser false
(hexmap? imagen3x3rgb) ;Resultado esperado: deberia ser false
(hexmap? imagen2x2hex) ;Resultado esperado: deberia ser true
(display "\n")
(display "\n")

(display "------------------------------------ compressed? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? (compress imagen2x2bit)) ;Resultado esperado: deberia ser true
(compressed? imagen2x2rgb)            ;Resultado esperado: deberia ser false
(compressed? (compress imagen3x3hex)) ;Resultado esperado: deberia ser true
(display "\n")
(display "\n")

(display "------------------------------------ flipH -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(flipH imagen4x4bit) ;Resultado esperado: imagen4x4bit volteada horizontalmente
(display "\n")
(display "\n")
(flipH imagen3x3hex) ;Resultado esperado: imagen3x3hex volteada horizontalmente
(display "\n")
(display "\n")
(flipH imagen2x2rgb) ;Resultado esperado: imagen2x2rgb volteada horizontalmente
(display "\n")
(display "\n")

(display "------------------------------------ flipV -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(flipV imagen2x2bit) ;Resultado esperado: imagen2x2bit volteada verticalmente
(display "\n")
(display "\n")
(flipV imagen3x3hex) ;Resultado esperado: imagen3x3hex volteada verticalmente
(display "\n")
(display "\n")
(flipV imagen3x3rgb) ;Resultado esperado: imagen3x3rgb volteada verticalmente
(display "\n")
(display "\n")

(display "------------------------------------ crop -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(crop imagen4x4bit 1 1 2 2) ;Resultado esperado: imagen de 2x2 
(display "\n")
(display "\n")
(crop imagen2x2rgb 0 1 1 1) ;Resultado esperado: imagen de 2x1 (una fila)
(display "\n")
(display "\n")
(crop imagen3x3hex 1 0 1 2) ;Resultado esperado: imagen de 1x3 (una columna)
(display "\n")
(display "\n")

(display "------------------------------------ imgRGB->imgHex -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(imgRGB->imgHex imagen2x2rgb) ;Resultado esperado: imagen2x2rgb con sus colores transformados a hexadecimal
(display "\n")
(display "\n")
(imgRGB->imgHex imagen3x3rgb) ;Resultado esperado: imagen3x3rgb con sus colores transformados a hexadecimal
(display "\n")
(display "\n")
(define imagen4x4hex (imgRGB->imgHex imagen4x4rgb)) 
imagen4x4hex                  ;Resultado esperado: imagen4x4rgb con sus colores transformados a hexadecimal
(display "\n")
(display "\n")
(display "------------------------------------ histogram -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(histogram imagen4x4rgb) ;Resultado esperado: 1 color de cada uno, menos (255 255 255) que hay 2 bits de ese color.
(display "\n")
(display "\n")
(histogram imagen2x2hex) ;Resultado esperado: 1 color de cada uno
(display "\n")
(display "\n")
(histogram imagen4x4bit) ;Resultado esperado: seis bits 0, diez bits 1
(display "\n")
(display "\n")
(display "------------------------------------ rotate90 -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(rotate90 imagen2x2bit) ;Resultado esperado: '(2 2 ((0 0 1 12) (0 1 0 10) (1 0 0 13) (1 1 1 11)))
(display "\n")
(display "\n")
(rotate90 imagen3x3bit) ;Resultado esperado: '(3 3 ((0 0 1 16) (0 1 0 13) (0 2 1 11) (1 0 1 16) (1 1 0 13) (1 2 1 11) (2 0 0 11) (2 1 1 16) (2 2 0 13)))
(display "\n")
(display "\n")
(rotate90 imagen2x2hex) ;Resultado esperado: '(2 2 ((0 0 "#FA090A" 12) (0 1 "#F40111" 10) (1 0 "#59000A" 13) (1 1 "#F21716" 11)))
(display "\n")
(display "\n")
(display "------------------------------------ compress -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(compress imagen2x2bit) ;Resultado esperado: imagen2x2bit con 2 pixeles, como hay la misma cantidad de 0s y 1s, se elimino arbitrariamente el 0.
(display "\n")
(display "\n")
(compress imagen3x3rgb) ;Resultado esperado: imagen3x3rgb con 6 pixeles, se elimina la combinacion de colores (242 23 22)
(display "\n")
(display "\n")
(compress imagen4x4hex) ;Resultado esperado: imagen4x4hex con 14 pixeles, se elimina el color #FFFFFF
(display "\n")
(display "\n")
(display "------------------------------------ edit -----------------------------------------------------------------------------------")
(display "\n")
(display "------------------------------------ invertColorBit -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit invertColorBit imagen2x2bit) ;Resultado esperado: imagen4x4bit con sus bits invertidos (0->1) (1->0)
(display "\n")
(display "\n")
(edit invertColorBit imagen3x3bit) ;Resultado esperado: imagen4x4bit con sus bits invertidos (0->1) (1->0)
(display "\n")
(display "\n")
(edit invertColorBit imagen4x4bit) ;Resultado esperado: imagen4xbit con sus bits invertidos (0->1) (1->0)
(display "\n")
(display "\n")

(display "------------------------------------ invertColorRGB -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit invertColorRGB imagen2x2rgb) ;Resultado esperado: imagen2x2rgb con sus colores invertidos.                            
(display "\n")
(display "\n")
(edit invertColorRGB imagen3x3rgb) ;Resultado esperado: imagen3x3rgb con sus colores invertidos
(display "\n")
(display "\n")
(edit invertColorRGB imagen4x4rgb) ;Resultado esperado: imagen4x4rgb con sus colores invertidos.
(display "\n")
(display "\n")

(display "------------------------------------ adjustChannel -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit (adjustChannel getR setR incCh) imagen2x2rgb) ;Resultado esperado: imagen2x2rgb con su canal R aumentado en 1: 244->245, 242->243, 250->251, 89->90.  
(display "\n")
(display "\n")
(edit (adjustChannel getG setG incCh) imagen3x3rgb) ;Resultado esperado: imagen3x3rgb con su canal G aumentado en 1: 1->2, 23->24, 23->24, 9->10, 1->2, 0->1, 23->24, 0->1, 1->2.  
(display "\n")
(display "\n")
(edit (adjustChannel getB setB incCh) imagen2x2rgb) ;Resultado esperado: imagen2x2rgb con su canal B aumentado en 1: 17->18, 22->23, 10->11, 10->11.
(display "\n")
(display "\n")
(edit (adjustChannel getD setD incCh) imagen3x3rgb) ;Resultado esperado: imagen3x3rgb con su canal D aumentado en 1: 11->12,11->12,16->17,13->14,13->14,16->17,16->17,11->12,13->14.
(display "\n")
(display "\n")

(display "------------------------------------ imagen->string -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(display (image->string (compress (invertColorBit imagen2x2bit)) pixbit->string)) ;Resultado esperado: "1 1 "
(display "\n")
(display "\n")
(display (image->string imagen3x3rgb pixrgb->string)) ;Resultado esperado: "#F40111 #F21716 #F21716 \n#FA090A #F40111 #59000A \n#F21716 #59000A #F40111 "
(display "\n")
(display "\n")
(display (image->string (flipH (rotate90 imagen3x3bit)) pixbit->string)) ;Resultado esperado: "1 0 1 \n1 0 1 \n0 1 0"
(display "\n") 
(display "\n")
(display (image->string (crop imagen3x3hex 1 0 1 2) pixhex->string)) ;Resultado esperado: "#F21716  \n#F40111 \n#59000A "
(display "\n") 
(display "\n")

(display "------------------------------------ depthLayers -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(depthLayers imagen2x2bit) ;Resultado esperado: 4 imagenes de 2x2, de profundidades 10, 11, 12 y 13.
(display "\n")
(display "\n")
(display "\n")
(depthLayers imagen4x4hex) ;Resultado esperado: 2 imagenes de 4x4, de profunidades 10 y 11. 
(display "\n")
(display "\n")
(display "\n")
(depthLayers imagen3x3rgb) ;Resultado esperado: 3 imagenes de 3x3, de profundidades 16, 11 y 13. 
(display "\n")
(display "\n")
(display "\n")

(display "------------------------------------ FIN SCRIPT PRUEBAS PROPIO-----------------------------------------------------------------------------------")
(display "\n")
(display "\n")



