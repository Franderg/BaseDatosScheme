#lang scheme

;############  Funciones que escribe en archivos ############################
(define (escritura2 dato archivo)
  (call-with-output-file archivo dato #:exists 'append))

(define (escritura dato archivo)
  (call-with-output-file archivo #:exists 'append
    (lambda (output-port)
      (display dato output-port))))
;############################################################################

(define leer-archivo
  (lambda (ruta)
    (let ((p (open-input-file ruta)))
      (let f ((x (read-line p)))
        (cond ((eof-object? x) (begin (close-input-port p) '()))
              (else (cons x (f (read-line p)))))))))

;##################################################################

;Primer comando ct (crear tabla)
(define (ct tabla datos)
  (cond ((file-exists? tabla) #f);verificamos que la tabla no exista
        (else 
         (cond ((list? datos );si vamos a ingresar algun dato
                (cond ((null? datos) #f);verificamos si se debe agregar algo a la tabla
                      (else (ct_auxiliar datos tabla))));vamos a una funcion auxiliar recursiva
               (else #f))))
  (escritura2 newline tabla));\n al final de los datos para mantener orden

(define (ct_auxiliar datos tabla) ;funcion recursiva donde se escribe en la tabla la info
  (cond ((null? datos)#f);revisamos si no es null
        (else 
         (escritura (car datos) tabla ) ;escribimos en la tabla los datos
         (escritura ":" tabla ) ;como separador se va a usar los dos puntos :
         (ct_auxiliar (cdr datos) tabla))));segimos con el resto de los datos

;##################################################################


(define ins
  (lambda ()
    (display "ins")))

(define (endof ls apd)
  (cond 
    [(char=? #\) (string-ref (car ls) ( - (string-length(car ls)) 1) )) (append apd (string-trim (car ls) ")" )) ]
    [else (append apd (endof (cdr ls) apd))]
   )
)
(define (lastof ls)
  ( cond
       [(char=? #\) (string-ref (car ls) ( - (string-length(car ls)) 1) )) (cdr ls)]
       [else (lastof (cdr ls))]
  )
)
(define sel
  (lambda ()
    (display "sel")))

(define act
  (lambda ()
    (display "act")))

(define (alt_list ls jmp)
  (cond
    [(integer? (/ (length ls) 2) ) (cond
                                     [(= (length ls) 2) (append jmp (car ls))]
                                     [else (append jmp (alt_list (cdr(cdr ls )) jmp ))]
                                   )]
    [else (cond
            [(= (length ls) 1) (append jmp (car ls))]
            [else (append jmp (alt_list (cdr(cdr ls )) jmp ))]
          )]
  )
)

(define boir
  (lambda ()
    (display "boir")))

(define ir
  (lambda ()
    (display "ir")))

(define bo
  (lambda ()
    (display "bo")))
;##################################################################

;Se define una consola, consiste en un read-line.
(define (base-datos)
  (let ((entrada (read-line))) 
    (cond ((equal? entrada "exit") "Gracias por utilizar nuestro programa");condición de parada
          (else (cond ((string? entrada)(main entrada ))(base-datos))))));si no es la condición de parada entra al main
                                                                      ;con la entrada
(define (main ls)
  (let ((comando (car(string-split ls))))
    (cond ((string=?  comando  "ct")
         (ct (car(cdr(string-split ls))) (cdr(cdr(string-split ls)))));verificacion del comando ct
          
          [(string=? comando "ins")
           (cond [(char=? #\( (string-ref (car(cdr(cdr( string-split ls )))) 0 ))
                (ins (car(cdr( string-split ls ))))]
               [else ins( (car(cdr( string-split ls )))null ;columnas
                          (cdr(cdr( string-split ls ))))])]
        
        [(string=? ( car(string-split ls ) ) "sel")
         (cond [(char=? #\( (string-ref (car(cdr(cdr( string-split ls )))) 0 ))
                (sel (car(cdr( string-split ls ))))]
               [else sel( (car(cdr( string-split ls ))) null ;columnas
                                                        (cdr(cdr( string-split ls ))))])]
        [(string=? ( car(string-split ls ) ) "act") 
         (act (car(cdr( string-split ls ))) ;table name
              (car(cdr(cdr( string-split ls )))) ;id
              (append (list (car(cdr(cdr(cdr (string-split ls))))))
                      (list (alt_list (cdr(cdr(cdr(string-split ls))))'() )) ) ;col names
              (append (list (car(cdr(cdr(cdr(cdr(string-split ls)))))))
                      (list (alt_list (cdr(cdr(cdr(cdr(string-split ls))))) '() )) ) ;new values
                                                     )]
        [(string=? ( car(string-split ls) ) "boir") (boir (cdr (string-split ls)))]
        [(string=? ( car(string-split ls) ) "ir")   (ir   (cdr (string-split ls)))]
        [(string=? ( car(string-split ls) ) "bo")   (bo   (cdr (string-split ls)))]
        [(string=? ( car(string-split ls) ) "info")   (display "Base de Datos en Scheme, creado por Maikol y Frander")(newline)
                                                      (display "Los comandos que puede usar son: ct, ins, sel, act, bo, ir, boir")
                                                      (newline)]
      )
  (base-datos)
))