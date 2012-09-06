(defun crea-variables()
(setf *variables* (list
		     (crea-ficha :nombre 'f :matriz (make-array '(3 3) :initial-contents '((0 1 1)(1 1 0)(0 1 0))))
		     (crea-ficha :nombre 'i :matriz (make-array '(5 1) :initial-element 1))
		     (crea-ficha :nombre 'l :matriz (make-array '(4 2) :initial-contents '((1 0)(1 0)(1 0)(1 1))))
		     (crea-ficha :nombre 'n :matriz (make-array '(4 2) :initial-contents '((0 1)(1 1)(1 0)(1 0))))
		     (crea-ficha :nombre 'p :matriz (make-array '(3 2) :initial-contents '((0 1)(1 1)(1 1))))
		     (crea-ficha :nombre 't :matriz (make-array '(3 3) :initial-contents '((1 1 1)(0 1 0)(0 1 0))))
		     (crea-ficha :nombre 'u :matriz (make-array '(2 3) :initial-contents '((1 0 1)(1 1 1))))
		     (crea-ficha :nombre 'v :matriz (make-array '(3 3) :initial-contents '((1 0 0)(1 0 0)(1 1 1))))
		     (crea-ficha :nombre 'w :matriz (make-array '(3 3) :initial-contents '((1 0 0)(1 1 0)(0 1 1))))
		     (crea-ficha :nombre 'x :matriz (make-array '(3 3) :initial-contents '((0 1 0)(1 1 1)(0 1 0))))
		     (crea-ficha :nombre 'y :matriz (make-array '(4 2) :initial-contents '((0 1)(1 1)(0 1)(0 1))))
		     (crea-ficha :nombre 'z :matriz (make-array '(3 3) :initial-contents '((1 1 0)(0 1 0)(0 1 1))))
		     
		     
		     ;;volteados
		     (crea-ficha :nombre 'f :matriz (make-array '(3 3) :initial-contents '((1 1 0)(0 1 1)(0 1 0))))
		     (crea-ficha :nombre 'l :matriz (make-array '(4 2) :initial-contents '((0 1)(0 1)(0 1)(1 1))))
		     (crea-ficha :nombre 'n :matriz (make-array '(4 2) :initial-contents '((1 0)(1 1)(0 1)(0 1))))
		     (crea-ficha :nombre 'p :matriz (make-array '(3 2) :initial-contents '((1 0)(1 1)(1 1))))
		     (crea-ficha :nombre 'v :matriz (make-array '(3 3) :initial-contents '((0 0 1)(0 0 1)(1 1 1))))
		     (crea-ficha :nombre 'w :matriz (make-array '(3 3) :initial-contents '((0 0 1)(0 1 1)(1 1 0))))
		     (crea-ficha :nombre 'y :matriz (make-array '(4 2) :initial-contents '((1 0)(1 1)(1 0)(1 0))))
		     (crea-ficha :nombre 'z :matriz (make-array '(3 3) :initial-contents '((0 1 1)(0 1 0)(1 1 0))))
		     
)))