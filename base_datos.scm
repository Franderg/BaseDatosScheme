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
                      (else (auxiliar datos tabla))));vamos a una funcion auxiliar recursiva
               (else #f))))
  (escritura2 newline tabla));\n al final de los datos para mantener orden

(define (auxiliar datos tabla) ;funcion recursiva donde se escribe en la tabla la info
  (cond ((null? datos)#f);revisamos si no es null
        (else 
         (escritura (car datos) tabla ) ;escribimos en la tabla los datos
         (escritura ":" tabla ) ;como separador se va a usar los dos puntos :
         (auxiliar (cdr datos) tabla))));segimos con el resto de los datos

;##################################################################

;Comando ins estud 2012001 julio 5554444
(define (ins tabla valores columna)
  (cond ((and 
          (and
           (and
           (and ;debe cumplir que exista la tabla y la columna sea una tupla
            (file-exists? tabla);verificamos que existe la tabla
            (list? columna)
            (display columna))
           (list? valores))
           (verificadorDeID tabla (car columna)))
          (existID (cdr(file->lines tabla))(car valores)))
     (ins-aux tabla (lineFormat (file->lines tabla)) columna valores 0)
     (escritura2 tabla))))

(define (ins1 tabla valores)
  (let ((existe(file-exists? tabla)))
    (cond ((equal? existe #t);verificamos que la tabla exista
           (auxiliar valores tabla));se utiliza la misma funcion ct_auxiliar
          (else (display "No se encuentra la tabla")(newline)))))


;Auxiliar function for ins. Makes the recursive part.
(define (ins-aux tabla pTablaList pColList pValList pFlag)
  (cond
    ((empty? pTablaList))
    ((and (empty? pColList) (= 0 pFlag)))
    ((and (empty? pColList) (= 1 pFlag))
     (escritura "(nulo)" tabla )
     (escritura ":" tabla )
     (ins-aux tabla (cdr pTablaList) pColList pValList 1))
    ((equal? (car pTablaList)(car pColList))
     (cond
       ((empty? pValList)
        (escritura "(nulo)" tabla )
        (escritura ":" tabla )
        (ins-aux tabla (cdr pTablaList) (cdr pColList) pValList 1))
       (else
        (escritura (car pValList) tabla)
        (escritura ":" tabla )
        (ins-aux tabla (cdr pTablaList) (cdr pColList) (cdr pValList)1))))
    ((eq? (equal? (car pTablaList)(car pColList)) #f )
     (escritura "(nulo)" tabla )
     (escritura ":" tabla )
     (ins-aux tabla (cdr pTablaList) pColList pValList 1))))

;Checks if the unique ID name exists.
(define (verificadorDeID tabla pID)
  (equal? pID (car(lineFormat (file->lines tabla)))))

(define (lineFormat list)
  (string-split 
   (string-replace
    (string-replace (string-replace(car list)"\"" "_") "___" "_")"__""_") "_"))

;##################################
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
  ))
  
(define (tokenizer cadena token)
  (let repetir ([palabra ""] [lista (string->list cadena)] [resultado '()]) 
    (cond
      ((and (eq? lista '()) (eq? palabra "")) resultado)
      ((and (eq? lista '()) (not (eq? palabra ""))) (repetir "" lista (append resultado (list palabra))))
      ((eq? (car lista) token) (repetir "" (cdr lista) (append resultado (list palabra)))) 
      (else (repetir (string-append palabra (string(car lista))) (cdr lista) resultado) )
      )))


(define (existID identificador Listtabla)
  (cond
    ((eq? Listtabla '())#f)
    (else
     (let ((carnet (car(tokenizer (car Listtabla) #\:))))
       (cond ((string=? identificador carnet) #t)
            (else
             (existID identificador (cdr Listtabla))))))))

(define sel (lambda (x y z) ( x y z ))
  )

;################################################################################################
;comando act actualizar
(define (act tabla carnet columnas valoresNuevos)
  (cond 
    ((eq? (existID carnet (leer-archivo tabla)) #t)
     (bo tabla carnet);se borra de la tabla
     (ins1 tabla (append(string-split(carnet)) valoresNuevos)))));se inserta con los nuevos valores

;################################################################################################


(define (alt_list datos salida)
  (cond;primera condicion, queremos saber cuantos elementos se van a modificar
    ((integer? (/ (length datos) 2) );vrificamos que la entrada sea par
     (cond ((= (length datos) 2) (append salida (car datos)))
       (else (append salida (alt_list (cdr(cdr datos )) salida )))))
    (else 
     (cond
       ((= (length datos) 1) (append salida (car datos)))
       (else (append salida (alt_list (cdr(cdr datos )) salida )))))))

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
  (let ((comando (car(string-split entrada))));creamos la variable comando con el primer elemento leido del read-line y se verifica 
    (cond 
      ((string=? comando "ct");verifica el comando ct
           ;le pasamos dos parametros, el primero el nombre de la tabla y lo siguiente son las columnas
           (ct (car(cdr(string-split entrada))) (cdr(cdr(string-split entrada)))))
      
      ((string=? comando "ins");este comando tiene dos formatos. Para determinar cuando se usa cada uno se verifica
           ;guardamos en una variable el simbolo siguiente a la llave
           (let ((verif(car(cdr(cdr( string-split entrada ))))));variable verif donde se guarda el tercer elemento de la entrada
             (cond ((char=? #\( (string-ref verif 0 ));verificamos el tipo de formato de ins
                    (ins (car(cdr( string-split entrada )))
                         (append(list ( string-trim (car(cdr(cdr( string-split entrada )))) "(" )
                              (endof(cdr(cdr(cdr( string-split entrada )))) '() )));cambiar por eliminar element
                         (lastof(cdr(cdr( string-split entrada )))))); en caso de ser el que tiene tupla entra a este
                   ;sino hace el ins1, donde recibe 2 argumentos
                   (else
                    (ins1 (car(cdr( string-split entrada )))(cdr(cdr( string-split entrada ))))))))
      
      ((string=? comando "sel")
         (let ((verif(car(cdr(cdr( string-split entrada ))))));variable verif donde se guarda el tercer elemento de la entrada
           (cond ((char=? #\( (string-ref verif 0 ));verificamos el parentesis para la correcta sintaxis del comando sel
                  (sel (car(cdr( string-split entrada )))
                       (append( list ( string-trim (car(cdr(cdr( string-split entrada )))) "(" )
                              (endof(cdr(cdr(cdr( string-split entrada )))) '() ))))
                  (lastof(cdr(cdr( string-split entrada )))))
           (else (sel( (car(cdr( string-split entrada )))) null (cdr(cdr( string-split entrada ))))))))
      
      ((string=? comando "act");se verifica el comando act
       (let (( tabla (car(cdr( string-split entrada )))));llave o tabla 
         (let (( carnet (car(cdr(cdr( string-split entrada))))));ID
           (let (( n (car(cdr(cdr(cdr (string-split entrada)))))));
             (let (( columnas (cdr(cdr(cdr(string-split entrada))))));columnas que se modifican
               (let (( name (car(cdr(cdr(cdr(cdr(string-split entrada))))))))
                 (let ((nuevo (cdr(cdr(cdr(cdr(string-split entrada)))))));nuevo valor que se actualiza
                   ;se le pasan 4 parametros, la tabla el carnet y los otros pueden ser datos a actualizar
                   (act tabla carnet (append (list n)(list (alt_list columnas'()))) (append (list name)(list (alt_list nuevo '())))))))))))
      
      
        [(string=? comando "boir") (boir (cdr (string-split entrada)))]
        [(string=? comando "ir")   (ir   (cdr (string-split entrada)))]
        [(string=? comando "bo")   (bo   (cdr (string-split entrada)))]
        [(string=? comando "man")    (display "Base de Datos en Scheme, creado por Maikol y Frander")
                                                           (newline)
                                                           (display "Los comandos que puede usar son: ct, ins, sel, act, bo, ir, boir y exit para salir")
                                                           (newline)
                                                           (display "Para mas info puede revisar el manual de usuario en la documentación")
                                                           (newline)])
    (base-datos)))