
;; Definimos variables 

(defvar *tablero* nil)
(defvar *variables* nil)
(defvar *operadores* nil)

;; Definimos estructuras

(defstruct (ficha (:constructor crea-ficha)
		  (:conc-name get-))
  nombre
  matriz)

(defstruct (operador (:constructor crea-operador)
		     (:conc-name ))
  ficha
  x
  y)

(defstruct (nodo (:constructor crea-nodo)
		 (:conc-name ))
  estado
  camino
  heuristica
  operadores)

;; Funciones auxiliares sobre matrices
(defun rota (m)
  (let ((x (first (array-dimensions m)))
	(y (second (array-dimensions m)))
	(matriz))
    (setf matriz (make-array (list y x) :initial-element 0))
    
    (loop for i from 0 below x do
	 (loop for j from 0 below y do
	      (setf (aref matriz j i) (aref m i (- y (+ j 1))))))
    (pinta-matriz matriz)
    matriz))

(defun pinta-matriz (n)
  (let ((x (first (array-dimensions n)))
	(y (second (array-dimensions n))))
    (loop for i from 0 below x do
	 (loop for j from 0 below y do
	      (if (= j 0)
		  (format t "~&~a" (aref n i j))
		  (format t "~a" (aref n i j)))))
    (format t "~%---~%")
))

(defun get-submatriz (matrix puntox puntoy longx longy)
  (let ((matriz))
    ; Uso de las funciones a y b
    (setf matriz (a matrix puntoy longy))
    (setf matriz (b matriz puntox longx))
    
    matriz))
    
;; Define los limites de las matrices. A en la coordenada y y b en la coordenada x.
(defun a (m y1 y2)
  (let ((x (first (array-dimensions m)))
	(y (second (array-dimensions m)))
	(matriz)
	(k 0))
     
    (setf matriz (make-array (list x y2) :initial-element 0))
    
    (loop for i from 0 below x do
	 (and (setf k 0)
	 (loop for j from 0 below y do
	      (if(and (or (> j y1) (= j y1)) (< j (+ y1 y2)))
		 (and (setf (aref matriz i k) (aref m i j)) (setf k (+ k 1)))))))
    
    matriz
))
		 
(defun b (m x1 x2)
  (let ((x (first (array-dimensions m)))
	(y (second (array-dimensions m)))
	(matriz)
	(k 0))
     
    (setf matriz (make-array (list x2 y) :initial-element 0))
    
    (loop for i from 0 below x do
	 
	 (loop for j from 0 below y do
	      (if(and (or (> i x1) (= i x1)) (< i (+ x1 x2)))
		 (and (setf (aref matriz k j) (aref m i j))(if (= j (- y 1))
							       (setf k (+ k 1)))))))
    
    matriz
))

;; Funciones auxiliares sobre listas




;;; Definicion de variables y operadores

; Crea el tablero, 6 x 10, relleno de 0.
(defun crea-tablero()
  (setf *tablero* (make-array '(4 5) :initial-element 0)))

(defun nuevas-variables()
   
(setf *variables* (list
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(1 0 0 0 0 0 0 0 0 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((0 1 1)(1 1 0)(0 1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 1 0 0 0 0 0 0 0 0 0 0)) :matriz (make-array '(5 1) :initial-element 1))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 1 0 0 0 0 0 0 0 0 0)) :matriz (make-array '(4 2) :initial-contents '((1 0)(1 0)(1 0)(1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 1 0 0 0 0 0 0 0 0)) :matriz (make-array '(4 2) :initial-contents '((0 1)(1 1)(1 0)(1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 1 0 0 0 0 0 0 0)) :matriz (make-array '(3 2) :initial-contents '((0 1)(1 1)(1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 1 0 0 0 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((1 1 1)(0 1 0)(0 1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 1 0 0 0 0 0)) :matriz (make-array '(2 3) :initial-contents '((1 0 1)(1 1 1))))1
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 1 0 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((1 0 0)(1 0 0)(1 1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 1 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((1 0 0)(1 1 0)(0 1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 0 1 0 0)) :matriz (make-array '(3 3) :initial-contents '((0 1 0)(1 1 1)(0 1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 0 0 1 0)) :matriz (make-array '(4 2) :initial-contents '((0 1)(1 1)(0 1)(0 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 0 0 0 1)) :matriz (make-array '(3 3) :initial-contents '((1 1 0)(0 1 0)(0 1 1))))
		     
		     
		     ;;volteados
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(1 0 0 0 0 0 0 0 0 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((1 1 0)(0 1 1)(0 1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 1 0 0 0 0 0 0 0 0 0)) :matriz (make-array '(4 2) :initial-contents '((0 1)(0 1)(0 1)(1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 1 0 0 0 0 0 0 0 0)) :matriz (make-array '(4 2) :initial-contents '((1 0)(1 1)(0 1)(0 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 1 0 0 0 0 0 0 0)) :matriz (make-array '(3 2) :initial-contents '((1 0)(1 1)(1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 1 0 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((0 0 1)(0 0 1)(1 1 1))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 1 0 0 0)) :matriz (make-array '(3 3) :initial-contents '((0 0 1)(0 1 1)(1 1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 0 0 1 0)) :matriz (make-array '(4 2) :initial-contents '((1 0)(1 1)(1 0)(1 0))))
		     (crea-ficha :nombre (make-array '(1 12) :initial-elements '(0 0 0 0 0 0 0 0 0 0 0 1)) :matriz (make-array '(3 3) :initial-contents '((0 1 1)(0 1 0)(1 1 0))))
		     
)))

; Rota las variables y las introduce rotadas como nuevas variables

(defun continua-variables ()
  (let ((x *variables*))
    (loop for i in x do
	 (setf *variables* (append
			    *variables*
			    (list (crea-ficha
				   :nombre (get-nombre i)
				   :matriz (rota (get-matriz i))))))
	 (setf *variables* (append
			    *variables*
			    (list (crea-ficha
				   :nombre (get-nombre i)
				   :matriz (rota (rota (get-matriz i)))))))
	 (setf *variables* (append
			    *variables*
			    (list (crea-ficha
				   :nombre (get-nombre i)
				   :matriz (rota (rota (rota (get-matriz i))))))))
	 )))
; Elimina las variables repetidas

(defun elimina-dobles ()
  (let ((lista ()))
    (loop for i in *variables* do
	 (if (not (contiene2 lista i))
	     (setf lista (append lista (list i)))))
    (setf *variables* lista)
    lista
    ))


(defun matrices-iguales (a b)
  (let ((res 1))
    (if (and (= (first (array-dimensions a)) (first (array-dimensions b)))
	     (= (second (array-dimensions a)) (second (array-dimensions b))))
	(loop for i from 0 below (first (array-dimensions a)) do
	     (loop for j from 0 below (second (array-dimensions a)) do
		  (if (not (= (aref a i j)(aref b i j)))
		      (setf res 0))))
	(setf res 0))
    (if (= res 0)
	nil
	T))
)

(defun contiene(lista termino)
  (let ((res 0))
     
  (loop for i in lista when (eq i termino) do
       (setf res 1))
  (if (= res 0)
      nil
      T)))

(defun contiene2(lista termino)
  (let ((res 0))
     
  (loop for i in lista do
       (if (and (eq (get-nombre i) (get-nombre termino)) 
		(matrices-iguales (get-matriz i) (get-matriz termino)))
	   (setf res 1)))

  (if (= res 1)
      T
      nil)
))



;; Crea los operadores
(defun crea-operadores()
  (setf *operadores* ())
  (let ((a)(b))
    (loop for i in *variables* do
	 (setf a (- (first (array-dimensions *tablero*)) (first (array-dimensions (get-matriz i)))))
	 (setf b (- (second (array-dimensions *tablero*)) (second (array-dimensions (get-matriz i)))))
	 (loop for k from 0 to a do
	      (loop for j from 0 to b do
		   (setf *operadores* (append *operadores*
					      (list (crea-operador
						     :ficha i
						     :x k
						     :y j
						     )))))))))

;;; Metodos del algoritmo

;; Se-puede-aplicar. Comprueba que la submatriz de la pieza entre en la matriz del tablero (no se superponga con otra pieza)
; h = matriz de la pieza en cuestion
; a, b = coordenadas de la pieza
; matriz = tablero
(defun se-puede-aplicar (h a b matriz)
  (let ((x 0)
	(m))

    (setf m (get-submatriz matriz a b
			   (first (array-dimensions h))
			   (second (array-dimensions h))
			   ))
    (loop for i from 0 below (first (array-dimensions m)) do
	 (loop for j from 0 below (second (array-dimensions m)) do
	      (if (and (= (aref m i j) (aref h i j)) (= (aref m i j) 1))
		  (setf x 1))))
    (if (= x 1)
	NIL
	T)))

;; Aplica un operador a un estado.
(defun aplica (operador estado)
  (let ((res))
    (if (se-puede-aplicar (get-matriz (ficha operador))
			  (x operador)
			  (y operador)
			  estado)
	(setf res (apli estado (get-matriz (ficha operador)) (x operador) (y operador))))
    res))

;; Aplica la matriz h a la matriz m (comunmente conocida como la matriz chopped)
(defun apli (m h x y)
  (let ((res))
    (setf res (copia-estado m))
    (loop for i from 0 below (first (array-dimensions m)) do
	 (loop for j from 0 below (second (array-dimensions m)) do
	      (if (and
		   (= 0 (aref res i j))
		   ; Comprueba que no se salga de la matriz al recorrerla.
		   (and (and (or (> j y) (= j y))(< j (+ y (second (array-dimensions h)))))(and (or (> i x)(= i x)) (< i (+ x (first (array-dimensions h)))))))
		  (setf (aref res i j) (aref h (- i x)(- j y))))))
    res))

(defun elimina-duplicados (lista-nodos abiertos cerrados)
  (loop for nodo in lista-nodos
       when (and (not (esta nodo abiertos))
		 (not (esta nodo cerrados)))
       collect nodo))

(defun esta (nodo lista-nodos)
  (let ((estado (estado nodo)))
    (loop for n in lista-nodos
	 thereis (equalp estado (estado n)))))

;; Devuelve el sucedor de un nodo, al aplicarle el operador.
(defun sucesor (nodo operador)
  (let* ((estado-sucesor (aplica operador (estado nodo))))
    (when estado-sucesor
      (crea-nodo
       :estado estado-sucesor
       :camino (cons operador (camino nodo))
       :heuristica (pondera estado-sucesor)
       :operadores (elimina-operadores nodo operador)))))

(defun elimina-operadores(nodo operador)
  (let ((res))
    (loop for i in (operadores nodo) do
	 (if (not ( eq (get-nombre (ficha operador)) (get-nombre (ficha i))))
	     (setf res (append res (list i)))))
    res)

)

(defun prueba ()
  (let ((res)(nodo))
    (setf nodo (crea-nodo
		:estado (make-array '(6 10) :initial-element 0)
		:operadores *operadores*))
    (setf res (elimina-operadores nodo (first *operadores*)))
	res)	
)



;; TODO - Borrar. Se llama a esta funcion en (apli)
(defun copia-estado (estado)
  (let ( (nuevo-estado (make-array (list (first (array-dimensions estado))
					 (second (array-dimensions estado))))))
    (loop for i from 0 below (first (array-dimensions estado)) do
	 (loop for j from 0 below (second (array-dimensions estado)) do
	      (setf (aref nuevo-estado i j) (aref estado i j))))
    nuevo-estado))

;; Comprueba que toda la matriz m esta llena de 1.
(defun es-estado-final (m)
  (let ((x (first (array-dimensions m)))
	(y (second (array-dimensions m)))
	(k T))
    (loop for i from 0 below x do
	 (loop for j from 0 below y do
	      (if (= 0 (aref m i j))
		  (setf k NIL))))
    k))



;; Define los sucesores de un nodo para todos los operadores.

(defun sucesores (nodo)
  (let ((siguiente)
	(lista))
    ;AQUI SE MIRAN LOS NOMBRES DE LAS FICHAS EN LOS CAMINOS DE CADA NODO
    (loop for i in (camino nodo) do
	 (setf lista (append lista (list (get-nombre (ficha i))))))
    ;SE MIRA QUE NO SE META NINGUN OPERADOR CON UN NOMBRE IGUAL EN EL CAMINO DEL NODO
    (loop for operador in (operadores nodo) do
	 (setf siguiente (sucesor nodo operador))
	 when (and siguiente (not (contiene lista (get-nombre (ficha operador)))))
	 collect siguiente)))

(defun nuevos-sucesores (nodo abiertos cerrados)
  (elimina-duplicados (sucesores nodo) abiertos cerrados))

(defun busqueda-en-profundidad ()
  (let ((abiertos (list (crea-nodo                                 ;1.1
                         :estado *tablero*
                         :camino ()
			 :heuristica 0
			 :operadores *operadores*
			 )))
        (cerrados ())                                              ;1.2
        actual                                                     ;1.3
        nuevos-sucesores)                                          ;1.4
    (loop until (null abiertos) do
	  ;(setf abiertos (sort abiertos #' ordena))
          
	  (setf abiertos (limpia abiertos))
	  ;(format t "~a~%" (heuristica actual))
	  (setf actual (first abiertos))                           ;2.1
	  ;(loop for i in abiertos do (format t "~a" (heuristica i)))
	  ;(format t "~a --- " (length abiertos))
          ;(format t "~a~%" (heuristica actual))
          (setf abiertos (rest abiertos))                          ;2.2
	  ;(pinta-matriz (estado actual))
          (push actual cerrados)
	  ;2.3
	  ;(format t " --------- ")
          (cond ((es-estado-final (estado actual))                 ;2.4
                 (return actual))                                  ;2.4.1
                (t (setf nuevos-sucesores                          ;2.4.2.1
                         (nuevos-sucesores actual abiertos cerrados))
                   (setf abiertos                                  ;2.4.2.2
                         (append nuevos-sucesores abiertos))))
	  

    )))

(defun limpia (lista)
  (let ((res ()))
    (if (not (= 0 (length lista)))
	(loop for i in lista do
	     ;(format t "~a" (length lista))
	     (if  (not (= 100 (heuristica i)))
		 (setf res (append res (list i))))))
    res))
	     

				 
(defun ordena(a b)
  (< (heuristica a)(heuristica b))
     
)

(defun pondera (matriz)
  (let ((x 0)(res 0)(estado))
    (setf estado (copia-estado matriz))
    (setf *aux* estado)
    (loop for i from 0 below (first (array-dimensions matriz)) do
	 (loop for j from 0 below (second (array-dimensions matriz)) do
	      (if (= 0 (aref *aux* i j))
		  (and (setf x (cuenta i j))
		   (if (not (= 0 (mod x 5)))
			    (setf res 100)
				 )))))
    res)
)

(defun copia-matriz (matriz)
  (let ((res)(x)(y))
    (setf x (first (array-dimensions matriz)))
    (setf y (second (array-dimensions matriz)))
    (setf res (make-array (list (x y)) :initial-element 0))
res)
)

(defun cuenta(i j)
    (let ((res 0)(alto)(largo))
      (setf alto (first (array-dimensions *aux*)))
      (setf largo (second (array-dimensions *aux*)))
      (if (= 0 (aref *aux* i j))
	  (and (and (setf (aref *aux* i j) 2)
		    (setf res 1))
	       (cond 
		    ;;ESQUINAS
		    ((and (= i 0)(= j 0))
		      (and (setf res (+ res (cuenta i (+ j 1))))
			   (setf res (+ res (cuenta (+ i 1) j)))))
		     
		     ((and (= i 0)(= j (- largo 1)))
		      (and (setf res (+ res (cuenta (+ i 1) j)))
			   (setf res (+ res (cuenta i (- j 1))))))

		     ((and (= i (- alto 1))(= j 0))
		      (and (setf res (+ res (cuenta (- i 1) j)))
			   (setf res (+ res (cuenta i (+ j 1))))))

		     ((and (= i (- alto 1))(= j (- largo 1)))
		      (and (setf res (+ res (cuenta (- i 1) j)))
			   (setf res (+ res (cuenta i (- j 1))))))

		     ;;ARISTAS
		     ((and (= i 0)
			   (and (> j 0)(< j (- largo 1))))
		      (and (and (setf res (+ res (cuenta i (- j 1))))
				(setf res (+ res (cuenta i (+ j 1))))
				(setf res (+ res (cuenta (+ i 1) j))))))

		     ((and (= i (- alto 1))
			   (and (> j 0)(< j (- largo 1))))
		      (and (and (setf res (+ res (cuenta i (- j 1))))
				(setf res (+ res (cuenta i (+ j 1))))
				(setf res (+ res (cuenta (- i 1) j))))))

		     ((and (= j 0)
			   (and (> i 0)(< i (- largo 1))))
		      (and (and (setf res (+ res (cuenta (- i 1) j)))
				(setf res (+ res (cuenta (+ i 1) j)))
				(setf res (+ res (cuenta i (+ j 1)))))))		     
		     
		     ((and (= j (- largo 1))
			   (and (> i 0)(< i (- largo 1))))
		      (and (and (setf res (+ res (cuenta (- i 1) j)))
				(setf res (+ res (cuenta (+ i 1) j)))
				(setf res (+ res (cuenta i (- j 1)))))))

		     ;;CENTRALES
		     ((and (and (> i 0)(< i (- alto 1)))
		     	   (and (> j 0)(< j (- largo 1))))
		      (and (and (setf res (+ res (cuenta (- i 1) j)))
				(setf res (+ res (cuenta (+ i 1) j)))
				(setf res (+ res (cuenta i (+ j 1))))
				(setf res (+ res (cuenta i (- j 1)))))))		   

)))

      res)
)

(defun funcion ()

  (setf *variables* (list 
		     (crea-ficha :nombre 'a :matriz (make-array '(2 3) :initial-contents '((1 1 1)(1 1 0))))
		     (crea-ficha :nombre 'b :matriz (make-array '(3 3) :initial-contents '((0 0 1)(1 1 1)(1 0 0))))
		     (crea-ficha :nombre 'c :matriz (make-array '(3 3) :initial-contents '((0 0 1)(0 0 1)(1 1 1))))
		     (crea-ficha :nombre 'd :matriz (make-array '(4 2) :initial-contents '((1 1)(0 1)(0 1)(0 1))))
		     ;(crea-ficha :nombre 'e :matriz (make-array '(2 3) :initial-contents '((1 0 0)(1 1 1))))

)))

;;COSAS NUEVAS

