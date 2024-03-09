;;; Andres Nicolas Uranga Limon

;;; ResoluciÃ³n deductiva de un Kakuro
;;; Departamento de Ciencias de la ComputaciÃ³n e Inteligencia Artificial 
;;; Universidad de Sevilla
;;;============================================================================


;;;============================================================================
;;; RepresentaciÃ³n del Kakuro
;;;============================================================================

;;;   Utilizaremos la siguiente plantilla para representar las celdas del
;;; Kakuro. Cada celda tiene los siguientes campos:
;;; - id: Identificador Ãºnico de la celda
;;; - fila: NÃºmero de fila en la que se encuentra la celda
;;; - columna: NÃºmero de columna en la que se encuentra la celda
;;; - rango: Rango de valores que se pueden colocar en la celda. Inicialmente
;;;   el rango son todos los valores numÃ©ricos de 1 a 9.

(deftemplate celda
  (slot id)
  (slot fila)
  (slot columna)
  (multislot rango
             (default (create$ 1 2 3 4 5 6 7 8 9))))

;;;   De esta forma, una celda tendrÃ¡ un valor asignado si y solo si dicho
;;; valor es el Ãºnico elemento del rango.

;;;   La siguiente variable global sirve enumerar las restricciones del puzle.

(defglobal ?*restricciones* = 0)

;;;   La siguiente funciÃ³n sirve para asignar de forma automÃ¡tica y Ãºnica
;;; identificadores a las restricciones del puzle. 

(deffunction idRestriccion ()
  (bind ?*restricciones* (+ ?*restricciones* 1))
  ?*restricciones*)

;;;   Utilizaremos la siguiente plantilla para almacenar las restricciones del
;;; puzle. Cada restricciÃ³n tiene los siguientes campos:
;;; - id: Identificador Ãºnico de la restricciÃ³n
;;; - valor: Valor de la restricciÃ³n
;;; - casillas: Identificadores de las casillas que se ven afectadas por la
;;;   restricciÃ³n

(deftemplate restriccion
  (slot id
        (default-dynamic (idRestriccion)))
  (slot valor)
  (multislot casillas))

;;;============================================================================
;;; Estrategias de resoluciÃ³n
;;;============================================================================

;;;   El objetivo del ejercicio consiste en implementar un conjunto de reglas
;;; CLIPS que resuelvan un Kakuro de forma deductiva, es decir, deduciendo el
;;; valor de una casilla a partir de reglas que analicen los posibles valores
;;; de las casillas relacionadas.

;;; Si el valor de la restriccion en 16 solo se puede sumar como 7+9 luego el 
;;; rango de las casillas a las que afecta la restriccion será 7 y 9

;;; A continuación, se definen una función que dado un numero n nos da la
;;; lista con las posibles maneras de sumar n con k sumandos distintos entre 
;;; 1 y 9. Esta función nos sevira para reducir el rango de valores de las 
;;; casillas usando las restricciones. Si una restriccion afecta  a 3 casillas
;;; usando generar-sumandos3 de valor que indique la restriccion obtendremos
;;; el rango de esas 3 casillas.


;;;============================================================================
(deffunction generar-sumandos2 (?n)
   	(bind $?sumandos (create$)) ; Inicializamos la lista de sumandos
   	(bind ?i 1)
   	(while (<= ?i 9)
      		(bind ?j 1)
      			(while (<= ?j 9)
         				(if (and (= (+ ?i ?j) ?n)          
			; Comprobamos que la suma se igual a n
                 	(not (= ?i ?j)))         ; i distinto de j
                 then
            	 ;Si se cumplen las condiones añadimos i y j a las lista de sumandos
            	(bind $?sumandos (create$ ?i ?j $?sumandos))
         	)
         (bind ?j (+ ?j 1))
      )
      (bind ?i (+ ?i 1))
   )
   $?sumandos
)

;;; Esta regla toma las restricciones de dos casillas y  modifica el rango
;;; de las casillas a las que afecta


(defrule restriccion-2c
	?h <-(restriccion (id ?) (valor ?v) (casillas ?i ?j))
	?h1<-(celda (id ?i) (rango $?C&:(> (length$ $?C) 1)))
	?h2<-(celda (id ?j) (rango $?D&:(> (length$ $?D) 1)))
	=>
	;(retract ?h)
	(bind ?rango (generar-sumandos2 ?v))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango))))

;;;============================================================================

(deffunction generar-sumandos3 (?n)
   (bind $?sumandos (create$)) ; Inicializamos la lista de sumandos
   (bind ?i 1)
   (while (<= ?i 9)
      (bind ?j 1)
      (while (<= ?j 9)
         (bind ?k 1)
         (while (<= ?k 9)
            (if (and (= (+ ?i ?j ?k) ?n)       ; Suma igual n
                     (not (= ?i ?j))             ;  i!=j!=k
                     (not (= ?i ?k))             ; 
                     (not (= ?j ?k)))             ;  
               then
               (bind $?sumandos (create$ ?i ?j ?k $?sumandos))
            )
            (bind ?k (+ ?k 1))
         )
         (bind ?j (+ ?j 1))
      )
      (bind ?i (+ ?i 1))
   )
   $?sumandos
)

;;; Esta regla toma las restricciones de tres casillas  y modifica el rango
;;; de las casillas a las que afecta

(defrule restriccion-3c
	(declare (salience -5))
	?h<-(restriccion (id ?) (valor ?v) (casillas ?i ?j ?k))
	?h1<-(celda (id ?i) (rango $?C&:(> (length$ $?C) 1)))
	?h2<-(celda (id ?j) (rango $?D&:(> (length$ $?D) 1)))
	?h3<-(celda (id ?k) (rango $?E&:(> (length$ $?E) 1)))
	=>
	;(retract ?h)
	(bind ?rango (generar-sumandos3 ?v))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango))))


;;;============================================================================

(deffunction generar-sumandos4 (?n)
   (bind $?sumandos (create$)) ; Inicializamos la lista de sumandos
   (bind ?i 1)
   (while (<= ?i 9)
      (bind ?j 1)
      (while (<= ?j 9)
         (bind ?k 1)
         (while (<= ?k 9)
            (bind ?m 1)
            (while (<= ?m 9)
               (if (and (= (+ ?i ?j ?k ?m) ?n)       ; Suma igual a n
                        (not (= ?i ?j))             ;  i!=j!=k!=m
                        (not (= ?i ?k))             ; 
                        (not (= ?i ?m))             ; 
                        (not (= ?j ?k))             ; 
                        (not (= ?j ?m))             ; 
                        (not (= ?k ?m)))             ;
                  then
                  (bind $?sumandos (create$ ?i ?j ?k ?m $?sumandos))
               )
               (bind ?m (+ ?m 1))
            )
            (bind ?k (+ ?k 1))
         )
         (bind ?j (+ ?j 1))
      )
      (bind ?i (+ ?i 1))
   )
   $?sumandos
)


;;; Esta regla toma las restricciones de cuatro casillas  y modifica el rango
;;; de las casillas a las que afecta


(defrule restriccion-4c
	?h<-(restriccion (id ?) (valor ?v) (casillas ?i ?j ?k ?l))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	=>
	(bind ?rango (generar-sumandos4 ?v))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango))))
;
	
;;;============================================================================

(deffunction generar-sumandos5 (?z)
   (bind $?sumandos (create$)) ; Inicializamos la lista de sumandos
   (bind ?i 1)
   (while (<= ?i 9)
      (bind ?j 1)
      (while (<= ?j 9)
         (bind ?k 1)
         (while (<= ?k 9)
            (bind ?m 1)
            (while (<= ?m 9)
               (bind ?n 1)
               (while (<= ?n 9)
                  (if (and (= (+ ?i ?j ?k ?m ?n) ?z)          ; Suma igual a n
                           (not (= ?i ?j))             ; 
                           (not (= ?i ?k))             ; 
                           (not (= ?i ?m))             ; 
                           (not (= ?i ?n))             ; 
                           (not (= ?j ?k))             ; 
                           (not (= ?j ?m))             ; 
                           (not (= ?j ?n))             ;
                           (not (= ?k ?m))             ;
                           (not (= ?k ?n))             ;
                           (not (= ?m ?n)))             ; 
                     then
                     (bind $?sumandos (create$ ?i ?j ?k ?m ?n $?sumandos))
                  )
                  (bind ?n (+ ?n 1))
               )
               (bind ?m (+ ?m 1))
            )
            (bind ?k (+ ?k 1))
         )
         (bind ?j (+ ?j 1))
      )
      (bind ?i (+ ?i 1))
   )
   $?sumandos
)


;;; Esta regla toma las restricciones de cinco casillas  y modifica el rango
;;; de las casillas a las que afecta


(defrule restriccion-5c
	?h<-(restriccion (id ?) (valor ?v) (casillas ?i ?j ?k ?l ?m))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	?h5<-(celda (id ?m) (rango $?G))
	=>
	(bind ?rango (generar-sumandos5 ?v))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango)))
	(modify ?h5 (rango (intersection$ $?G ?rango))))

;======================================================================================


;;; Hay números que solo puenden ser sumados por un numero de sumandos de
;;; una forma específica. Por ejemplo 10=1+2+3+4. La regla 
;;; restriccion-i-j restringe el rango de una casilla que está implicada
;;; en una restricción de tamaño i a los únicos números que pueden sumar
;;; el valor de la restriccion, j.(Intersecándolos con el rango por si ya 
;;; se habían descartado anteriormente alguno de esos valores) 

;;; Las siguentes reglas no son necesarias pues están contenidas en las
;;; anteriores. 

;======================================================================================
;(defrule restriccion-4-10
;	?h<-(restriccion (id ?) (valor 10) (casillas ?i ?j ?k ?l))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	=>
;	(bind ?rango (create$ 1 2 3 4))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango))))
;
;(defrule restriccion-4-11
;	?h<-(restriccion (id ?) (valor 11) (casillas ?i ?j ?k ?l))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	=>
;	(bind ?rango (create$ 1 2 3 5))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango))))
;	
;
;(defrule restriccion-4-29
;	?h<-(restriccion (id ?) (valor 29) (casillas ?i ?j ?k ?l))
;	?h1<-(celda (id ?i) (rango $?C&:(> (length$ $?C) 1)))
;	?h2<-(celda (id ?j) (rango $?D&:(> (length$ $?D) 1)))
;	?h3<-(celda (id ?k) (rango $?E&:(> (length$ $?E) 1)))
;	?h4<-(celda (id ?l) (rango $?F&:(> (length$ $?F) 1)))
;	=>
;	(bind ?rango (create$ 5 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango))))
;	
;
;(defrule restriccion-4-30
;	?h<-(restriccion (id ?) (valor 30) (casillas ?i ?j ?k ?l))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	=>
;	(bind ?rango (create$ 6 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango))))
;
;(defrule restriccion-5-15
;	?h<-(restriccion (id ?) (valor 15) (casillas ?i ?j ?k ?l ?m))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	=>
;	(bind ?rango (create$ 1 2 3 4 5))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango))))
;
;(defrule restriccion-5-16
;	?h<-(restriccion (id ?) (valor 16) (casillas ?i ?j ?k ?l ?m))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	=>
;	(bind ?rango (create$ 1 2 3 4 6))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango))))
;
;(defrule restriccion-5-34
;	?h<-(restriccion (id ?) (valor 34) (casillas ?i ?j ?k ?l ?m))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	=>
;	(bind ?rango (create$ 4 6 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango))))
;
;(defrule restriccion-5-35
;	?h<-(restriccion (id ?) (valor 35) (casillas ?i ?j ?k ?l ?m))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	=>
;	(bind ?rango (create$ 5 6 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango))))

;======================================================================================

;;; Las reglas de 6 nos aportan nuevas soluciones.

(defrule restriccion-6-21
	?h<-(restriccion (id ?) (valor 21) (casillas ?i ?j ?k ?l ?m ?n))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	?h5<-(celda (id ?m) (rango $?G))
	?h6<-(celda (id ?n) (rango $?H))
	=>
	(bind ?rango (create$ 1 2 3 4 5 6))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango)))
	(modify ?h5 (rango (intersection$ $?G ?rango)))
	(modify ?h6 (rango (intersection$ $?H ?rango))))

(defrule restriccion-6-22
	?h<-(restriccion (id ?) (valor 22) (casillas ?i ?j ?k ?l ?m ?n))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	?h5<-(celda (id ?m) (rango $?G))
	?h6<-(celda (id ?n) (rango $?H))
	=>
	(bind ?rango (create$ 1 2 3 4 5 7))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango)))
	(modify ?h5 (rango (intersection$ $?G ?rango)))
	(modify ?h6 (rango (intersection$ $?H ?rango))))

(defrule restriccion-6-38
	?h<-(restriccion (id ?) (valor 38) (casillas ?i ?j ?k ?l ?m ?n))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	?h5<-(celda (id ?m) (rango $?G))
	?h6<-(celda (id ?n) (rango $?H))
	=>
	(bind ?rango (create$ 3 5 6 7 8 9))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango)))
	(modify ?h5 (rango (intersection$ $?G ?rango)))
	(modify ?h6 (rango (intersection$ $?H ?rango))))

(defrule restriccion-6-39
	?h<-(restriccion (id ?) (valor 39) (casillas ?i ?j ?k ?l ?m ?n))
	?h1<-(celda (id ?i) (rango $?C))
	?h2<-(celda (id ?j) (rango $?D))
	?h3<-(celda (id ?k) (rango $?E))
	?h4<-(celda (id ?l) (rango $?F))
	?h5<-(celda (id ?m) (rango $?G))
	?h6<-(celda (id ?n) (rango $?H))
	=>
	(bind ?rango (create$ 4 5 6 7 8 9))
	(modify ?h1 (rango (intersection$ $?C ?rango)))
	(modify ?h2 (rango (intersection$ $?D ?rango)))
	(modify ?h3 (rango (intersection$ $?E ?rango)))
	(modify ?h4 (rango (intersection$ $?F ?rango)))
	(modify ?h5 (rango (intersection$ $?G ?rango)))
	(modify ?h6 (rango (intersection$ $?H ?rango))))

;======================================================================================
;; Las reglas de 7 no aportan informacion 

;(defrule restriccion-7-28
;	?h<-(restriccion (id ?) (valor 28) (casillas ?i ?j ?k ?l ?m ?n ?p))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	?h6<-(celda (id ?n) (rango $?H))
;	?h7<-(celda (id ?p) (rango $?K))
;	=>
;	(bind ?rango (create$ 1 2 3 4 5 6 7))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango)))
;	(modify ?h6 (rango (intersection$ $?H ?rango)))	
;	(modify ?h7 (rango (intersection$ $?K ?rango))))	
;
;(defrule restriccion-7-29
;	?h<-(restriccion (id ?) (valor 29) (casillas ?i ?j ?k ?l ?m ?n ?p))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	?h6<-(celda (id ?n) (rango $?H))
;	?h7<-(celda (id ?p) (rango $?K))
;	=>
;	(bind ?rango (create$ 1 2 3 4 5 6 8))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango)))
;	(modify ?h6 (rango (intersection$ $?H ?rango)))
;	(modify ?h7 (rango (intersection$ $?K ?rango))))
;
;(defrule restriccion-7-41
;	?h<-(restriccion (id ?) (valor 41) (casillas ?i ?j ?k ?l ?m ?n ?p))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	?h6<-(celda (id ?n) (rango $?H))
;	?h7<-(celda (id ?p) (rango $?K))
;	=>
;	(bind ?rango (create$  2 4 5 6 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango)))
;	(modify ?h6 (rango (intersection$ $?H ?rango)))
;	(modify ?h7 (rango (intersection$ $?K ?rango))))
;
;(defrule restriccion-7-42
;	?h<-(restriccion (id ?) (valor 41) (casillas ?i ?j ?k ?l ?m ?n ?p))
;	?h1<-(celda (id ?i) (rango $?C))
;	?h2<-(celda (id ?j) (rango $?D))
;	?h3<-(celda (id ?k) (rango $?E))
;	?h4<-(celda (id ?l) (rango $?F))
;	?h5<-(celda (id ?m) (rango $?G))
;	?h6<-(celda (id ?n) (rango $?H))
;	?h7<-(celda (id ?p) (rango $?K))
;	=>
;	(bind ?rango (create$  3 4 5 6 7 8 9))
;	(modify ?h1 (rango (intersection$ $?C ?rango)))
;	(modify ?h2 (rango (intersection$ $?D ?rango)))
;	(modify ?h3 (rango (intersection$ $?E ?rango)))
;	(modify ?h4 (rango (intersection$ $?F ?rango)))
;	(modify ?h5 (rango (intersection$ $?G ?rango)))
;	(modify ?h6 (rango (intersection$ $?H ?rango)))
;	(modify ?h7 (rango (intersection$ $?K ?rango))))
;



;==================================================================================== 

;;; Veamos a continuacion si podemos implementar alguna estrategia.

;;; Si una restriccion afecta a dos casillas de una cuales ya conocemos su valor, el 
;;; valor de la otra casilla será la resta del valor de la restriccion y ese valor

;;; Si no conozco el valor de la segunda casilla

(defrule dos-una-conocida
	(restriccion (id ?) (valor ?v) (casillas ?i ?j))
	(celda (id ?i) (rango ?k))
	?h2<-(celda (id ?j) (rango $?D&: (> (length$ $?D) 1))); comprueba que la celda
                                                              ; no tenga valor asignado
	(test (member$ (- ?v ?k) $?D))	
	=>
	(modify ?h2 (rango (- ?v ?k))))

;;; Si no conozco el valor de la primera casilla

(defrule dos-una-conocida2
	
	(restriccion (id ?) (valor ?v) (casillas ?j ?i))
	(celda (id ?i) (rango ?k))
	?h2<-(celda (id ?j) (rango $?D&: (> (length$ $?D) 1)))
	(test (member$ (- ?v ?k) $?D))
	=>
	(modify ?h2 (rango (- ?v ?k))))


;=====================================================================================

;;; A continuación, vamos a crear una regla que nos dice que si sabemos un valor 
;;; de una casilla que afecta una restricción, las demas casillas a las que afecta 
;;; deberán eliminar ese valor de su rango

(defrule reduce-rango
	(restriccion (id ?a) (valor ?v1) (casillas $?w1 ?i $?w2))
	(celda (id ?i) (rango ?k))
        ?h<-(celda (id ?j&:(or (member$ ?j $?w1) (member$ ?j $?w2)))  
	(rango $?w ?k $?v)) ; 
	=>
	(modify ?h (rango $?w $?v)))


;======================================================================================


;===========================================================================================

;;; Con estas reglas bloque se prentende añadir nuevas restricciones. Contemplemos la siguiente
;;; situacion

;;;				ab
;;; 		    ====cd====

;;; Si conozcon a+b, a+c y b+d, entonces se que c+d=(a+c)+(b+d)-(a+b). DE ahí surgen dos nuvas
;;; restricciones una que afecta a c+d y otra al resto de la fila, con el valor original - (c+d).


;(defrule bloque
;	(declare (salience -1))
;	(restriccion (valor ?v1) (casillas ?a ?b))
;	(restriccion (valor ?v2) (casillas ?a ?c))
;	(restriccion (valor ?v3) (casillas ?b ?d))
;	?h<-(restriccion (valor ?v4) (casillas $?y1 ?c ?d $?y2))
;	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
;	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
;	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
;	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
;	(test (or (!= 0 (length$ $?y1)) (!= 0 (length$ $?y2))))
;	=>
;	(bind ?valor (- (+ ?v2 ?v3) ?v1))
;	(assert (restriccion (valor ?valor) (casillas ?c ?d)))
;	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2))))

;(defrule bloque
;	(declare (salience -1))
;	(restriccion (valor ?v1) (casillas ?a ?b))
;	(restriccion (valor ?v2) (casillas ?a ?c&:(!= ?b ?c)))
;	(restriccion (valor ?v3) (casillas ?b ?d))
;	?h<-(restriccion (valor ?v4) (casillas $?y1 ?c $?y2 ?d $?y3))
;	(not(restriccion (casillas ?c ?d)))
;	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
;	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
;	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
;	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
;	(test (or (!= 0 (length$ $?y1)) (!= 0 (length$ $?y2)) (!= 0 (length$ $?y3))))
;	=>
;	(bind ?valor (- (+ ?v2 ?v3) ?v1))
;	(assert (restriccion (valor ?valor) (casillas ?c ?d)))
;	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2 $?y3))))
;
;;; Con las dos reglas anteriores no consigo ningún nuevo kakuro y si las defino a la
;;; vez que bloque2 entra en bucle

;;; La regla de abajo regla ataca la siguiente situación
;;;                     |
;;;				ab
;;; 		        cd 
;;;                     |

(defrule bloque2
	(declare (salience -10))
	(restriccion (valor ?v1) (casillas ?a ?b))
	(restriccion (valor ?v2) (casillas ?c ?d))
	(restriccion (valor ?v3) (casillas ?b ?d))
	;(not(restriccion (casillas ?a ?c)))
	?h<-(restriccion (valor ?v4) (casillas $?y1 ?a ?c $?y2))
	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
	(test (or (!= 0 (length$ $?y1)) (!= 0 (length$ $?y2))))
	=>
	(bind ?valor (- (+ ?v1 ?v2) ?v3))
	(assert (restriccion (valor ?valor) (casillas ?a ?c)))
	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2))))



;;; Con bloque2 conseguimos un nuevo puzzle.


;;;============================================================================

;;; En el kakuro 42, nos encontramos frente a una situación similar
;;;

;;;		    ab=====
;;; 		    cd
;;;                 ef

;;; Teniendo en cuenta las sumas verticales y horizontales, se deduce que a+b=6
;;; De esta forma el 3 debría de desaparecer del rango de a y b. 

(defrule bloque32
	;(declare (salience -10))
	(restriccion (valor ?v1) (casillas ?a ?c ?e))
	(restriccion (valor ?v2) (casillas ?c ?d))
	(restriccion (valor ?v3) (casillas ?e ?f))
	(restriccion (valor ?v5) (casillas ?b ?d ?f))
	?h<-(restriccion (valor ?v4) (casillas $?y1 ?a ?b $?y2))
	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
	?h5 <- (celda (id ?e) (fila ?j2&:(= ?j2 (+ 2 ?j))) (columna ?i))
	?h6 <- (celda (id ?f)  (fila ?j2) (columna ?i1))
	(test (or (!= 0 (length$ ?y1)) (!= 0 (length$ ?y2))))
	=>
	(bind ?valor (- (+ ?v1 ?v5) (+ ?v2 ?v3)))
	(assert (restriccion (valor ?valor) (casillas ?a ?b)))
	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2))))


(defrule bloque322
	;(declare (salience -10))
	(restriccion (valor ?v1) (casillas ?a ?c ?e))
	(restriccion (valor ?v2) (casillas ?c ?d))
	(restriccion (valor ?v3) (casillas ?a ?b))
	(restriccion (valor ?v5) (casillas ?b ?d ?f))
	?h<-(restriccion (valor ?v4) (casillas $?y1 ?e ?f $?y2))
	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
	?h5 <- (celda (id ?e) (fila ?j2&:(= ?j2 (+ 2 ?j))) (columna ?i))
	?h6 <- (celda (id ?f)  (fila ?j2) (columna ?i1))
	(test (or (!= 0 (length$ ?y1)) (!= 0 (length$ ?y2))))
	=>
	(bind ?valor (- (+ ?v1 ?v5) (+ ?v2 ?v3)))
	(assert (restriccion (valor ?valor) (casillas ?e ?f)))
	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2))))

(defrule bloque323
	;(declare (salience -10))
	(restriccion (valor ?v1) (casillas ?a ?c ?e))
	(restriccion (valor ?v2) (casillas ?e ?f))
	(restriccion (valor ?v3) (casillas ?a ?b))
	(restriccion (valor ?v5) (casillas ?b ?d ?f))
	?h<-(restriccion (valor ?v4) (casillas $?y1 ?c ?d $?y2))
	?h1 <- (celda (id ?a)  (fila ?j) (columna ?i))
	?h2 <- (celda (id ?b) (fila ?j) (columna ?i1&:(= ?i1 (+ ?i 1))))
	?h3 <- (celda (id ?c) (fila ?j1&:(= ?j1 (+ 1 ?j))) (columna ?i))
	?h4 <- (celda (id ?d)  (fila ?j1) (columna ?i1))
	?h5 <- (celda (id ?e) (fila ?j2&:(= ?j2 (+ 2 ?j))) (columna ?i))
	?h6 <- (celda (id ?f)  (fila ?j2) (columna ?i1))
	(test (or (!= 0 (length$ ?y1)) (!= 0 (length$ ?y2))))
	=>
	(bind ?valor (- (+ ?v1 ?v5) (+ ?v2 ?v3)))
	(assert (restriccion (valor ?valor) (casillas ?c ?d)))
	(assert (restriccion (valor (- ?v4 ?valor)) (casillas  $?y1 $?y2))))

;;;============================================================================


;;;============================================================================

;;;  En la situacion que aparece abajo:
;;;            a b c d
;;;              e f g
;;; Podemos saber el valor de a si tengo las restricciones horizontales y verticales

(defrule falta-1
	(restriccion (id ?) (valor ?v1) (casillas ?a ?b ?c ?d))
	(restriccion (id ?) (valor ?v2) (casillas ?e ?f ?g))
	(restriccion (id ?) (valor ?v3) (casillas ?b ?e))
	(restriccion (id ?) (valor ?v4) (casillas ?c ?f))
	(restriccion (id ?) (valor ?v5) (casillas ?d ?g))
	?h<-(celda (id ?a) (rango $?R))
	(test (> (length$ $?R) 1))
	=>
	(modify ?h (rango (- (+ ?v1 ?v2) (+ ?v3 ?v4 ?v5)))))

(defrule falta-12
	(restriccion (id ?) (valor ?v1) (casillas ?a ?b ?c))
	(restriccion (id ?) (valor ?v2) (casillas ?d ?e ?f ?g))
	(restriccion (id ?) (valor ?v3) (casillas ?a ?e))
	(restriccion (id ?) (valor ?v4) (casillas ?b ?f))
	(restriccion (id ?) (valor ?v5) (casillas ?c ?g))
	?h<-(celda (id ?d))
	=>
	(modify ?h (rango (- (+ ?v1 ?v2) (+ ?v3 ?v4 ?v5)))))


;==================================================================================

;;; Si tengo dos casillas que deben sumar k y hay algún número en el rango de unas
;;; de estas casillas de manera que en la otra no está el número que necesito para
;;; sumar k, entonces este número puede ser eliminado del rango.


(defrule elimina-complemento
	(restriccion (id ?a) (valor ?v1) (casillas ?i ?j))
	?h <-(celda (id ?i) (rango $?w1 ?x $?w2))
	(celda (id ?j) (rango $?w3&:(not (member$ (- ?v1 ?x) $?w3))))
	=>
	(modify ?h (rango $?w1 $?w2)))

(defrule elimina-complemento2
	(restriccion (id ?a) (valor ?v1) (casillas ?j ?i))
	?h <-(celda (id ?i) (rango $?w1 ?x $?w2))
	(celda (id ?j) (rango $?w3&:(not (member$ (- ?v1 ?x) $?w3))))
	=>
	(modify ?h (rango $?w1 $?w2)))


;================================================================================

;;; Una vez no queden más reglas que aplicar y haya números colocados, conviene
;;; modificar las restricciones de tal forma que la restricción pase a ser una 
;;; restricción que afecte solo a las celdas que no tienen valor asignado y
;;; con valor de restriccion la diferencia entre en valor original y el 
;;; valor colocado.
 
(defrule modifica-restriccion
	(declare (salience -10))
	?h1 <- (restriccion (id ?a) (valor ?v1) (casillas $?w1 ?i $?w2))
	?h2 <- (celda (id ?i) (rango ?k))
	;(not (restriccion  (casillas $?w1 $?w2)))
	(test (or (> (length$ $?w1) 0)  ( > (length$ $?w2) 0) ))
	=>
	(modify ?h1 (valor (- ?v1 ?k)) (casillas $?w1 $?w2)))


;;;============================================================================
;;; Reglas para imprimir el resultado
;;;============================================================================

;;;   Las siguientes reglas permiten visualizar el estado del puzle, una vez
;;; aplicadas todas las reglas que implementan las estrategias de resoluciÃ³n.
;;; La prioridad de estas reglas es -10 para que sean las Ãºltimas en
;;; aplicarse.

;;;   Para las casillas del tablero con un Ãºnico valor en su rango se indica
;;; dicho valor, para las casillas del tablero en las que no se haya podido
;;; deducir el valor se indica el sÃ­mbolo '?'. El resto de las casillas hasta
;;; completar la cuadrÃ­cula 9x9 se dejan en blanco.

(defrule imprime-solucion
  (declare (salience -10))
  =>
  (printout t "+---------+" crlf "|")
  (assert (imprime 1 1)))

(defrule imprime-celda-1
  (declare (salience -10))
  ?h <- (imprime ?i ?j&:(<= (* ?i ?j) 81))
  (celda (fila ?i) (columna ?j) (rango $?v))
  =>
  (retract ?h)
  (if (= (length$ $?v) 1)
      then (printout t (nth$ 1 $?v))
    else (printout t "?"))
  (if (= ?j 9)
      then (printout t "|" crlf))
  (if (and (= ?i 9) (= ?j 9))
      then (printout t "+---------+" crlf))
  (if (and (= ?j 9) (not (= ?i 9)))
      then (printout t "|")
           (assert (imprime (+ ?i 1) 1))
    else (assert (imprime ?i (+ ?j 1)))))

(defrule imprime-celda-2
  (declare (salience -10))
  ?h <- (imprime ?i ?j&:(<= (* ?i ?j) 81))
  (not (celda (fila ?i) (columna ?j) (rango $?v)))
  =>
  (retract ?h)
  (printout t " ")
  (if (= ?j 9)
      then (printout t "|" crlf))
  (if (and (= ?i 9) (= ?j 9))
      then (printout t "+---------+" crlf))
  (if (and (= ?j 9) (not (= ?i 9)))
      then (printout t "|")
           (assert (imprime (+ ?i 1) 1))
    else (assert (imprime ?i (+ ?j 1)))))

;;;============================================================================
;;; Funcionalidad para leer los puzles del fichero de ejemplos
;;;============================================================================

;;;   Esta funciÃ³n crea una lista de identificadores de casillas en horizontal.

(deffunction crea-casillas-horizontal (?f ?c ?n)
  (bind ?datos (create$))
  (loop-for-count
   (?i 0 (- ?n 1))
   (bind ?datos (insert$ ?datos ?n (eval (str-cat ?f (+ ?c ?i))))))
  ?datos)

;;;   Esta funciÃ³n construye los hechos que representan las restricciones de
;;; una lÃ­nea horizontal del Kakuro.

(deffunction procesa-restricciones-fila (?f $?linea)
  (bind ?i 1)
  (bind ?d (nth$ ?i $?linea))
  (while (< ?i 9)
    (bind ?v 0)
    (while (and (<= ?i 9) (not (numberp ?d)))
      (bind ?i (+ ?i 1))
      (bind ?d (nth$ ?i $?linea)))
    (bind ?c ?i)
    (while (and (<= ?i 9) (numberp ?d))
      (bind ?i (+ ?i 1))
      (bind ?v (+ ?v ?d))
      (bind ?d (nth$ ?i $?linea)))
    (if (< 0 (- ?i ?c))
        then (assert (restriccion
                      (valor ?v)
                      (casillas
                       (crea-casillas-horizontal ?f ?c (- ?i ?c)))))))
  TRUE)

;;;   Esta funciÃ³n crea una lista de identificadores de casillas en vertical.

(deffunction crea-casillas-vertical (?f ?c ?n)
  (bind ?datos (create$))
  (loop-for-count
   (?i 0 (- ?n 1))
   (bind ?datos (insert$ ?datos ?n (eval (str-cat (+ ?f ?i) ?c)))))
  ?datos)

;;;   Esta funciÃ³n construye los hechos que representan las restricciones de
;;; una lÃ­nea horizontal del Kakuro.

(deffunction procesa-restricciones-columna (?c $?linea)
  (bind ?i 1)
  (bind ?d (nth$ ?i $?linea))
  (while (< ?i 9)
    (bind ?v 0)
    (while (and (<= ?i 9) (not (numberp ?d)))
      (bind ?i (+ ?i 1))
      (bind ?d (nth$ ?i $?linea)))
    (bind ?f ?i)
    (while (and (<= ?i 9) (numberp ?d))
      (bind ?i (+ ?i 1))
      (bind ?v (+ ?v ?d))
      (bind ?d (nth$ ?i $?linea)))
    (if (< 0 (- ?i ?f))
        then (assert (restriccion
                      (valor ?v)
                      (casillas
                       (crea-casillas-vertical ?f ?c (- ?i ?f)))))))
  TRUE)

;;;   Esta funciÃ³n construye todos los hechos que representan las restricciones
;;; de un Kakuro dado en el formato utilizado en el fichero de ejemplos.

(deffunction procesa-restricciones-ejemplo (?datos)
  (loop-for-count
   (?i 1 9)
   (bind $?linea (create$))
   (loop-for-count
    (?j 2 10)
    (bind ?linea (insert$ ?linea 10
                          (eval (sub-string (+ (* 10 ?i) ?j)
                                            (+ (* 10 ?i) ?j) ?datos)))))
   (procesa-restricciones-fila ?i ?linea))
  (loop-for-count
   (?j 2 10)
   (bind $?linea (create$))
   (loop-for-count
    (?i 1 9)
    (bind ?linea (insert$ ?linea 10
                          (eval (sub-string (+ (* 10 ?i) ?j)
                                            (+ (* 10 ?i) ?j) ?datos)))))
   (procesa-restricciones-columna (- ?j 1) ?linea))
  TRUE)

;;;   Esta funciÃ³n localiza el Kakuro que se quiere resolver en el fichero de
;;; ejemplos.

(deffunction lee-kakuro (?n)
  (open "ejemplos.txt" data "r")
  (loop-for-count (?i 1 (- ?n 1))
                  (readline data))
  (bind ?datos (readline data))
  (procesa-restricciones-ejemplo ?datos)
  (close data))

;;;   Esta regla pregunta al usuario que nÃºmero de Kakuro del fichero de
;;; ejemplos se quiere resolver y genera los hechos que representan las
;;; restricciones asociadas.

(defrule kakuro-numero
  (declare (salience 100))
  =>
  (printout t "Qué problema quieres resolver (1-50)? : ")
  (lee-kakuro (read)))

;;;   Esta regla construye las celdas que aparecen en alguna de las
;;; restricciones del Kakuro que se quiere resolver.

(defrule crea-celdas-restricciones
  (declare (salience 100))
  (restriccion (casillas $? ?id $?))
  (not (celda (id ?id)))
  =>
  (assert (celda (id ?id) (fila (div ?id 10)) (columna (mod ?id 10)))))

;;;============================================================================
