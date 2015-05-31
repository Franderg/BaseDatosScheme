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
  (let ((existe(file-exists? tabla)))
    (cond ((eq? existe #t);verificamos que la tabla no exista
          (display "La tabla ya existe")(newline))
          (else
           (cond ((list? datos );si vamos a ingresar algun dato
                (cond ((null? datos) #f);verificamos si se debe agregar algo a la tabla
                      (else (auxiliar datos tabla))));vamos a una funcion auxiliar recursiva
               (else #f))
  (escritura2 newline tabla)))));\n al final de los datos para mantener orden

(define (auxiliar datos tabla) ;funcion recursiva donde se escribe en la tabla la info
  (cond ((null? datos)#f);revisamos si no es null
        (else 
         (escritura (car datos) tabla ) ;escribimos en la tabla los datos
         (escritura ":" tabla ) ;como separador se va a usar los dos puntos :
         (auxiliar (cdr datos) tabla))));segimos con el resto de los datos


;comando act actualizar
(define (act tabla carnet columnas valoresNuevos);funcion que actualiza los datos
  (cond 
    ((eq? (existID carnet (leer-archivo tabla)) #t);si existe el ID a actualizar, en caso de false no se hace nada.
     (bo tabla carnet);se borra de la tabla
     (ins1 tabla (append(string-split(carnet)) valoresNuevos)))
    (else(display "No se encuentra el ID"))));se inserta con los nuevos valores

;##################################################################

;Comando ins estud 2012001 julio 5554444

(define (ins1 tabla valores)
  (let ((existe(file-exists? tabla)))
    (cond ((equal? existe #t);verificamos que la tabla exista
           (cond ((eq? (existID (car valores) (leer-archivo tabla)) #f)
                  (auxiliar valores tabla)
                  (escritura2 newline tabla));se utiliza la misma funcion auxiliar
                 (else (display "el ID ya se encuentra") (newline))))
          (else (display "No se encuentra la tabla")(newline)))))

  
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


;################################################################################################


(define (cambios datos salida);esta funcion nos indica cuales datos se van a modificar en el comando act
  (let ((largo (length datos)));lardo de los datos, esto para saber cuantos cambios se van a realizar 
    (cond;primera condicion, queremos saber cuantos elementos se van a modificar
      ((integer? (/ (length datos) 2) );verificamos que la entrada sea par
       (cond ((= largo 2) (append salida (car datos)));se hace append de car
             (else ;si no se cumple
              (append salida (cambios (cdr(cdr datos )) salida )))))
      (else ;si solo se trada de 1 cambio o mas de 2
       (cond((= largo 1) (append salida (car datos)));si se llega al cambio a realizar
            (else (append salida (cambios (cdr(cdr datos )) salida ))))))));sino se hace de forma recursiva el cdr de los datos

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
                    (ins1 (car(cdr( string-split entrada ))) (cdr(cdr( string-split entrada ))))); en caso de ser el que tiene tupla entra a este
                   ;sino hace el ins1, donde recibe 2 argumentos
                   (else
                    (ins1 (car(cdr( string-split entrada ))) (cdr(cdr( string-split entrada ))))))))
      
      
      ((string=? comando "act");se verifica el comando act
       (let (( tabla (car(cdr( string-split entrada )))));llave o tabla 
         (let (( carnet (car(cdr(cdr( string-split entrada))))));ID
           (let (( n (car(cdr(cdr(cdr (string-split entrada)))))));
             (let (( columnas (cdr(cdr(cdr(string-split entrada))))));columnas que se modifican
               (let (( name (car(cdr(cdr(cdr(cdr(string-split entrada))))))))
                 (let ((nuevo (cdr(cdr(cdr(cdr(string-split entrada)))))));nuevo valor que se actualiza
                   ;se le pasan 4 parametros, la tabla el carnet y los otros pueden ser datos a actualizar
                   (act tabla carnet (append (list n)(list (cambios columnas'()))) (append (list name)(list (cambios nuevo '())))))))))))
      
      
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