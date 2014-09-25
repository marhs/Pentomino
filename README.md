# Pentomino solver

Marco Herrero <me@marhs.de>

José María Martínez <warning.mexa@gmail.com>

Algoritmo para la resolución de pentóminos en Lisp. 
Versión funcional no optimizada, funciona sólo con pentóminos de pequeño tamaño.

## RESOLUCIÓN DE PROBLEMAS DE PENTOMINÓS

 Alumnos:
 	José María Martínez López
 	Marco Herrero Serna


 El trabajo trata sobre resolución del puzzle de los pentominós. Lo resolvemos planteando el problema como un espacio de estados, y aplicando diferentes estrategias para encontrar un camino hacia el estado final. 

 Los diferentes archivos son:

- **base.lisp**: Resuelve el problema buscando por los diferentes algoritmos, sin ninguna mejora.
 	
- **elimina-operadores.lisp**: Cada estado solo comprueba los operadores que no han sido probados ya, eliminando todos los anteriores y la variable global *operadores*. Así, no hay que recorrer todos los operadores con cada iteracion, comprobando solamente los restantes de cada nodo.
 	
- **ponderado.lisp**: Aplica una heuristica al problema. Se comprueba si alguno de los estados tiene caminos en su matriz de longitud que no sea multiplo de 5, dandoles para ello un valor heuristico de 100. Ello conllevaria que en estos estados hubiera huecos que no se pudieran llenar con ninguna ficha. Luego, se ordena la lista de 'abiertos' con el objetivo de coger los estados mejores. 
 	
- **elimina-imposibles.lisp**: Elimina del conjunto de estados aquellos que tienen un valor heuristico de 100, con lo que se reduce el numero de estados que llegan a 'abiertos'
 	
- **unificado.lisp**: Utiliza todas las mejoras descritas anteriormente (ponderado, elimina-imposibles y elimina-operadores).


##### EJECUCION:

 		Todos los archivos se ejecutan de la misma forma. Se dispone de una funcion 'prepara' que ejecuta los siguientes pasos independientemente del archivo en cuestion:

 			1 - (crea-tablero)
 			2 - (crea-variables)
 			3 - (continua-variables)
 			4 - (elimina-dobles)
 			5 - (crea-operadores)

 		Después de ejecutar (prepara) se ejecutará el algoritmo deseado, ya sea

 			(busqueda-en-profundidad)
 			(busqueda-en-anchura)
 			(busqueda-en-profundidad-acotada :cota n)
 			(busqueda-en-profundidad-iterativa :cota-inicial n)
