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

;Comando ins estud 2012001 julio 5554444
(define (ins tabla pColList pValList)
  (cond ((and (and (and (and (file-exists? tabla);verificamos que existe la tabla
                             (list? pColList)) (list? tabla))
               (verificadorDeID tabla (car pColList)))
          (existsID? (cdr(file->lines tabla))(car pValList)))
     (ins-aux tabla (lineFormat (file->lines tabla)) pColList pValList 0)
     (escritura2 tabla))))

;Auxiliar function for ins. Makes the recursive part.
(define (ins-aux pTabla pTablaList pColList pValList pFlag)
  (cond
    ((empty? pTablaList))
    ((and (empty? pColList) (= 0 pFlag)))
    ((and (empty? pColList) (= 1 pFlag))
     (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
     (write-to-file '_ pTabla #:mode'text #:exists'append )
     (ins-aux pTabla (cdr pTablaList) pColList pValList 1))
    ((equal? (car pTablaList)(car pColList))
     (cond
       ((empty? pValList)
        (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
        (write-to-file '_ pTabla #:mode'text #:exists'append )
        (ins-aux pTabla (cdr pTablaList) (cdr pColList) pValList 1))
       (else
        (write-to-file (car pValList) pTabla #:mode'text #:exists'append)
        (write-to-file '_ pTabla #:mode'text #:exists'append )
        (ins-aux pTabla (cdr pTablaList) (cdr pColList) (cdr pValList)1))))
    ((eq? (equal? (car pTablaList)(car pColList)) #f )
     (write-to-file '"(nil)" pTabla #:mode'text #:exists'append)
     (write-to-file '_ pTabla #:mode'text #:exists'append )
     (ins-aux pTabla (cdr pTablaList) pColList pValList 1))))

;Checks if the unique ID name exists.
(define (verificadorDeID pTabla pID)
  (equal? pID (car(lineFormat (file->lines pTabla)))))

;Checks existant ID.
;receives a list and searches for a existant IDvalue in the first column.
(define (existsID? list pIDVal)
  (cond
    ((empty? list) #t) 
    ((empty? (lineFormat list)) #t)
    ((equal? pIDVal (car(lineFormat list))) #f)
    (else (existsID? (cdr list) pIDVal))
    ) 
  )

(define (lineFormat list)(string-split (string-replace(string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))

;##################################3
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
          (else (cond ((string? entrada)(verifica entrada ))(base-datos))))));si no es la condición de parada entra al main
                                                                             ;con la entrada

;Se define una funcion que verifica los comandos
(define (verifica entrada)
  (let ((comando (car(string-split entrada))));creamos la variable comando con el primer elemento leido del read-line
    (cond ((string=?  comando  "ct");le pasamos dos parametros, el primero el nombre de la tabla y lo siguiente son las columnas
         (ct (car(cdr(string-split entrada))) (cdr(cdr(string-split entrada)))))
          
          ((string=? comando "ins");este comando tiene dos formatos. Para determinar cuando se usa cada uno se verifica
           ;guardamos en una variable el simbolo siguiente a la llave
           (let ((verif(car(cdr(cdr( string-split entrada ))))))
             (cond ((char=? #\( (string-ref verif 0 ))
                (ins (car(cdr( string-split entrada )))))
                 (else ins( (car(cdr( string-split entrada )))null ;columnas
                          (cdr(cdr( string-split entrada ))))))))
        
        [(string=? ( car(string-split entrada ) ) "sel")
         (cond [(char=? #\( (string-ref (car(cdr(cdr( string-split entrada )))) 0 ))
                (sel (car(cdr( string-split entrada ))))]
               [else sel( (car(cdr( string-split entrada ))) null ;columnas
                                                        (cdr(cdr( string-split entrada ))))])]
        [(string=? ( car(string-split entrada ) ) "act") 
         (act (car(cdr( string-split entrada ))) ;table name
              (car(cdr(cdr( string-split entrada )))) ;id
              (append (list (car(cdr(cdr(cdr (string-split entrada))))))
                      (list (alt_list (cdr(cdr(cdr(string-split entrada))))'() )) ) ;col names
              (append (list (car(cdr(cdr(cdr(cdr(string-split entrada)))))))
                      (list (alt_list (cdr(cdr(cdr(cdr(string-split entrada))))) '() )) ) ;new values
                                                     )]
        [(string=? ( car(string-split entrada) ) "boir") (boir (cdr (string-split entrada)))]
        [(string=? ( car(string-split entrada) ) "ir")   (ir   (cdr (string-split entrada)))]
        [(string=? ( car(string-split entrada) ) "bo")   (bo   (cdr (string-split entrada)))]
        [(string=? ( car(string-split entrada) ) "info")   (display "Base de Datos en Scheme, creado por Maikol y Frander")(newline)
                                                      (display "Los comandos que puede usar son: ct, ins, sel, act, bo, ir, boir")
                                                      (newline)]
      )
  (base-datos)
))