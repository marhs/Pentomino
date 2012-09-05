
 (defstruct (ficha (:constructor crea-ficha)
		  (:conc-name get-))
  nombre
  matriz)

(defstruct (operador(:constructor crea-operador)
		    (:conc-name ))
  ficha
  x
  y)

(defstruct (nodo(:constructor crea-nodo)
		(:conc-name ))
  estado
  camino
  heuristica)

(defun asd ()
  (let ((x)(y)(z))
    (setf x (crea-nodo :estado (make-array '(3 3) :initial-contents '((0 0 1)(1 0 1)(0 0 1)))))
    (setf y (crea-nodo :estado (make-array '(3 3) :initial-contents '((0 0 0)(1 0 1)(1 1 1)))))
    (setf z (crea-nodo :estado (make-array '(3 3) :initial-contents '((0 0 0)(1 1 0)(0 0 0)))))
    (ordena x y)
))

(defun prueba ()
  (let ((res))
    (setf res (pondera (make-array '(4 4) :initial-contents '((0 0 1 1)(1 0 0 0)(0 1 1 0)(0 0 1 1)))))
    res)
)

(defun ordena(a b)
  (if (> (heuristica a)(heuristica b))
      a
      b)
)

(defun pondera (matriz)
  (let ((x 0)(res 0))
    (setf *aux* matriz)
    (loop for i from 0 below (first (array-dimensions matriz)) do
	 (loop for j from 0 below (second (array-dimensions matriz)) do
	      (if (= 0 (aref *aux* i j))
		  (and (setf x (cuenta i j))
		   (if (and (> x 0)(< x 5))
			    (setf res 100)
				 )))))
    res)
)

(defun ordena (a b)
  (if (
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





(defun pinta(matriz)
  (let ((alto)(ancho))
    (setf alto (second (array-dimensions matriz)))
    (setf ancho (first (array-dimensions matriz)))
    (loop for i from 0 below ancho do
	 (loop for j from 0 below alto do
	      (if (= j (- ancho 1))
		  (format t "~a~%" (aref matriz i j))
		  (format t "~a" (aref matriz i j)))))
    (format t "~%~%"))
				      
)



(defstruct (str (:constructor make-str)
		(:conc-name nombre-))
nom
y
)

(defstruct (ficha (:constructor crea-ficha)
		  (:conc-name get-))
  nombre
  matriz)

