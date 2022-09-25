#lang racket

(require "TDA-pixbit-d.rkt")
(require "TDA-pixrgb-d.rkt")
(require "TDA-pixhex-d.rkt")
(require "TDA-image.rkt")


;Pruebas del documento del laboratorio

(display "\n")
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255 1)))
img1
(display "\n")

(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)))
img2 
(display "\n")

(define img3 (imgRGB->imgHex img1))
img3
(display "\n")
(display "--------------------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(display (image->string img1 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img2 pixbit->string))
(display "\n")
(display "\n")
(display "----------------------- bitmap? ---------------------------------------------------------------------")
(display "\n")
(display "\n")

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f
(display "\n")
(display "-------------------------- pixmap? ------------------------------------------------------------------")
(display "\n")
(display "\n")

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f
(display "\n")
(display "--------------------------- hexmap? ----------------------------------------------------------------")
(display "\n")
(display "\n")

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t
(display "\n")
(display "---------------------------- compressed? ----------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f
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

(define img4 (crop img1 0 0 0 0))
img4
(define img5 (crop img2 0 0 0 1))
img5
(define img6 (crop img1 0 1 1 1))
img6
(define img7 (crop img2 0 0 1 1))
img7
(display "\n")
(display "------------------------------ histogram --------------------------------------------------------------")
(display "\n")
(display "\n")

(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)
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
(define img22 (rotate90 img5)) ;---
img22
(define img23 (rotate90 img6)) ;---
img23
(define img24 (rotate90 img7))
img24
(display "\n")
(display "----------------------------------- compress ---------------------------------------------------------")
(display "\n")
(display "\n")
    
(define img8 (compress img1))
img8
(define img9 (compress img2))
img9
(define img10 (compress img3))
img10
(define img11 (compress img4))
img11
(define img12 (compress img5))
img12
(define img13 (compress img6))
img13
(define img14 (compress img7))
img14
(display "\n")
(display "-------------------------------- compressed? ------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t
(display "\n")
(display "-------------------- invertColorbit- invertColorRGB ---------------------------------------------------------")
(display "\n")
(display "\n")

(define img15 (edit invertColorBit img2))
img15
(define img16 (edit invertColorRGB img1))
img16
(display "\n")
(display "---------------------- adjustChannel ----------------------------------------------------------------------")
(display "\n")
(display "\n")

(define img33 (edit (adjustChannel getR setR incCh) img1))
img33
(define img34 (edit (adjustChannel getG setG incCh) img1))
img34
(define img35 (edit (adjustChannel getB setB incCh) img1))
img35
(display "\n")
(display "----------------------------- image->string ---------------------------------------------------------------")
(display "\n")
(display "-------------------  imagenes no comprimidas ---------------------------------------------------------------")
(display "\n")
(display "\n")
(display (image->string img1 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img2 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img3 pixhex->string))
(display "\n")
(display "\n")
(display (image->string img4 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img5 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img6 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img7 pixbit->string))
(display "\n")
(display "\n")
(display "------------------- imagenes comprimidas -------------------------------------------------------------------------")
(display "\n")
(display "\n")

(display (image->string img8 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img9 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img10 pixhex->string))
(display "\n")
(display "\n")
(display (image->string img11 pixrgb->string))
(display "\n")
(display "\n")
(display  (image->string img12 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img13 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img14 pixbit->string))
(display "\n")
(display "\n")
(display "---------------------------- imagenes no comprimidas --------------------------------------------------")
(display "\n")
(display "\n")


(display (image->string img33 pixrgb->string)) ;Aqui deberia ser (display (image->string img15 pixrgb->string)) pero img15 es pixbit
(display "\n")
(display "\n")
(display (image->string img34 pixrgb->string)) ;Aqui deberia ser (display (image->string img16 pixrgb->string)) pero img16 es pixbit
(display "\n")
(display "\n")
(display (image->string img35 pixrgb->string)) ;Aqui deberia ser (display (image->string img17 pixrgb->string)) pero img17 no esta definida
(display "\n")
(display "\n")
(display (image->string img18 pixrgb->string)) 
(display "\n")
(display "\n")
(display (image->string img19 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img20 pixhex->string))
(display "\n")
(display "\n")
(display (image->string img21 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img22 pixbit->string))
(display "\n")
(display "\n")
(display (image->string img23 pixrgb->string))
(display "\n")
(display "\n")
(display (image->string img24 pixbit->string))
(display "\n")
(display "\n")
(display "-------------------------- depthLayers -------------------------------------------------------")
(display "\n")
(display "\n")

(depthLayers img1)
(display "\n")
(display "\n")
(depthLayers img2)
(display "\n")
(display "\n")
(depthLayers img3)
(display "\n")
(display "\n")
(depthLayers img4)
(display "\n")
(display "\n")
(depthLayers img5)
(display "\n")
(display "\n")
(depthLayers img6)
(display "\n")
(display "\n")
(depthLayers img7)
(display "\n")
(display "\n")
(display "---------------------------- decompress --------------------------------------------------------")
(display "\n")
(display "\n")
(define img25 (decompress img8))
img25
(define img26 (decompress img9))
img26
(define img27 (decompress img10))
img27
(define img28 (decompress img11))
img28
(define img29 (decompress img12))
img29
(define img30 (decompress img13))
img30
(define img31 (decompress img14))
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
imagen2x2bit
(display "\n")
(display "\n")

(define imagen3x3bit (image 3 3 (pixbit-d 0 0 1 11)(pixbit-d 0 1 1 11)(pixbit-d 0 2 0 13)
                             (pixbit-d 1 0 0 13)(pixbit-d 1 1 0 13)(pixbit-d 1 2 1 16)
                             (pixbit-d 2 0 1 16)(pixbit-d 2 1 1 16)(pixbit-d 2 2 0 11)))
imagen3x3bit
(display "\n")
(display "\n")

(define imagen4x4bit (image 4 4 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 11)(pixbit-d 0 2 1 11)(pixbit-d 0 3 1 10)
                             (pixbit-d 1 0 0 11)(pixbit-d 1 1 1 10)(pixbit-d 1 2 0 10)(pixbit-d 1 3 0 11)
                             (pixbit-d 2 0 1 10)(pixbit-d 2 1 0 10)(pixbit-d 2 2 0 11)(pixbit-d 2 3 1 10)
                             (pixbit-d 3 0 0 11)(pixbit-d 3 1 1 11)(pixbit-d 3 2 1 10)(pixbit-d 3 3 1 11)))
imagen4x4bit
(display "\n")
(display "\n")

(define imagen2x2rgb (image 2 2 (pixrgb-d 0 0 244 1 17 10)(pixrgb-d 0 1 242 23 22 11)
                                (pixrgb-d 1 0 250 9 10 12)(pixrgb-d 1 1 89 0 10 13)))
imagen2x2rgb
(display "\n")
(display "\n")

(define imagen3x3rgb (image 3 3 (pixrgb-d 0 0 244 1 17 11) (pixrgb-d 0 1 242 23 22 11)(pixrgb-d 0 2 242 23 22 16)
                                (pixrgb-d 1 0 250 9 10 13) (pixrgb-d 1 1 244 1 17 13) (pixrgb-d 1 2 89 0 10 16)
                                (pixrgb-d 2 0 242 23 22 16)(pixrgb-d 2 1 89 0 10 11)  (pixrgb-d 2 2 244 1 17 13)))
imagen3x3rgb
(display "\n")
(display "\n")

(define imagen4x4rgb (image 4 4 (pixrgb-d 0 0 255 255 255 10)(pixrgb-d 0 1 29 90 19 11)(pixrgb-d 0 2 1 1 1 11)  (pixrgb-d 0 3 2 2 2 10)
                                (pixrgb-d 1 0 4 5 6 11)      (pixrgb-d 1 1 7 8 9 10)   (pixrgb-d 1 2 90 1 90 10)(pixrgb-d 1 3 77 77 77 11)
                                (pixrgb-d 2 0 7 7 7 10)      (pixrgb-d 2 1 8 8 8 10)   (pixrgb-d 2 2 6 6 6 11)  (pixrgb-d 2 3 40 40 40 10)
                                (pixrgb-d 3 0 255 255 255 11)(pixrgb-d 3 1 0 0 7 11)   (pixrgb-d 3 2 1 1 2 10)  (pixrgb-d 3 3 1 5 5 11)))
imagen4x4rgb
(display "\n")
(display "\n")

(define imagen2x2hex (imgRGB->imgHex imagen2x2rgb))
imagen2x2hex
(display "\n")
(display "\n")

(define imagen3x3hex (imgRGB->imgHex imagen3x3rgb))
imagen3x3hex
(display "\n")
(display "\n")

(display "------------------------------------ bitmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")
(bitmap? imagen2x2bit) ; deberia ser true
(bitmap? imagen3x3bit) ; deberia ser true
(bitmap? imagen3x3rgb) ; deberia ser false
(display "\n")
(display "\n")

(display "\n")
(display "\n")

(display "------------------------------------ pixmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(pixmap? imagen2x2bit) ; deberia ser false
(pixmap? imagen2x2rgb) ; deberia ser true
(pixmap? imagen3x3hex) ; deberia ser false
(display "\n")
(display "\n")

(display "------------------------------------ hexmap? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(hexmap? imagen4x4bit) ; deberia ser false
(hexmap? imagen3x3rgb) ; deberia ser false
(hexmap? imagen2x2hex) ; deberia ser true
(display "\n")
(display "\n")

(display "------------------------------------ compressed? -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(compressed? (compress imagen2x2bit)) ; deberia ser true
(compressed? imagen2x2rgb) ; deberia ser false
(compressed? (compress imagen3x3hex)) ; deberia ser true
(display "\n")
(display "\n")

(display "------------------------------------ flipH -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(flipH imagen4x4bit)
(display "\n")
(display "\n")
(flipH imagen3x3hex)
(display "\n")
(display "\n")
(flipH imagen2x2rgb)
(display "\n")
(display "\n")

(display "------------------------------------ flipV -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(flipV imagen2x2bit)
(display "\n")
(display "\n")
(flipV imagen3x3hex)
(display "\n")
(display "\n")
(flipV imagen3x3rgb)
(display "\n")
(display "\n")

(display "------------------------------------ crop -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(crop imagen4x4bit 1 1 2 2)
(display "\n")
(display "\n")
(crop imagen2x2rgb 0 1 1 1)
(display "\n")
(display "\n")
(crop imagen3x3hex 1 0 1 2)
(display "\n")
(display "\n")

(display "------------------------------------ imgRGB->imgHex -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(imgRGB->imgHex imagen2x2rgb)
(display "\n")
(display "\n")
(imgRGB->imgHex imagen3x3rgb)
(display "\n")
(display "\n")
(define imagen4x4hex (imgRGB->imgHex imagen4x4rgb))
imagen4x4hex
(display "\n")
(display "\n")
(display "------------------------------------ histogram -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(histogram imagen4x4rgb)
(display "\n")
(display "\n")
(histogram imagen2x2hex)
(display "\n")
(display "\n")
(histogram imagen4x4bit)
(display "\n")
(display "\n")
(display "------------------------------------ rotate90 -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(rotate90 imagen2x2bit)
(display "\n")
(display "\n")
(rotate90 imagen3x3bit)
(display "\n")
(display "\n")
(rotate90 imagen2x2hex)
(display "\n")
(display "\n")
(display "------------------------------------ compress -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(compress imagen2x2bit)
(display "\n")
(display "\n")
(compress imagen3x3rgb)
(display "\n")
(display "\n")
(compress imagen4x4hex)
(display "\n")
(display "\n")
(display "------------------------------------ edit -----------------------------------------------------------------------------------")
(display "\n")
(display "------------------------------------ invertColorBit -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit invertColorBit imagen2x2bit)
(display "\n")
(display "\n")
(edit invertColorBit imagen3x3bit)
(display "\n")
(display "\n")
(edit invertColorBit imagen4x4bit)
(display "\n")
(display "\n")

(display "------------------------------------ invertColorRGB -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit invertColorRGB imagen2x2rgb)
(display "\n")
(display "\n")
(edit invertColorRGB imagen3x3rgb)
(display "\n")
(display "\n")
(edit invertColorRGB imagen4x4rgb)
(display "\n")
(display "\n")

(display "------------------------------------ adjustChannel -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(edit (adjustChannel getR setR incCh) imagen2x2rgb)
(display "\n")
(display "\n")
(edit (adjustChannel getG setG incCh) imagen3x3rgb)
(display "\n")
(display "\n")
(edit (adjustChannel getB setB incCh) imagen2x2rgb)
(display "\n")
(display "\n")
(edit (adjustChannel getD setD incCh) imagen3x3rgb)
(display "\n")
(display "\n")

(display "------------------------------------ imagen->string -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(image->string (compress (invertColorBit imagen2x2bit)) pixbit->string)
(display "\n")
(display "\n")
(image->string imagen3x3rgb pixrgb->string)
(display "\n")
(display "\n")
(image->string (flipH (rotate90 imagen3x3bit)) pixbit->string)
(display "\n")
(display "\n")

(display "------------------------------------ depthLayers -----------------------------------------------------------------------------------")
(display "\n")
(display "\n")

(depthLayers imagen2x2bit)
(display "\n")
(display "\n")
(display "\n")
(depthLayers imagen4x4hex)
(display "\n")
(display "\n")
(display "\n")
(depthLayers imagen3x3rgb)
(display "\n")
(display "\n")
(display "\n")

(display "------------------------------------ FIN SCRIPT PRUEBAS PROPIO-----------------------------------------------------------------------------------")
(display "\n")
(display "\n")



