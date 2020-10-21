
/*
Dado un dato y un árbol binario de búsqueda, localizar el nodo en el árbol que contiene
el dato y retornar el subárbol que tiene a este nodo como raíz.
[[[[], 1, []], 3, [[], 4, []]], 5, [[], 9, [[], 11, []]]]
            5
      3          9 
  1       4           11
*/
preorden([]).

preorden([AI, Nodo, AD]):-
  writeln(Nodo),
  preorden(AI),
  preorden(AD).


buscar(Dato, [AI, Nodo, AD]):-
  =:=(Dato, Nodo), writeln('Se encontro :0'), !.

buscar(Dato, [AI, Nodo, AD]):-
  <(Dato, Nodo), buscar(Dato, AI), !.


buscar(Dato, [AI, Nodo, AD]):-
  >(Dato, Nodo), buscar(Dato, AD), !.


%ej 5
contarNodos([], 0):- !.

contarNodos([AI, _ , AD], Contador):-
    contarNodos(AI, ContadorIz),
    contarNodos(AD, ContadorDer),
    is(Contador, +(1, +(ContadorIz, ContadorDer))).



comprobar([AI, Nodo, AD], [AI, Nodo, AD]):- !.
























retornar_subarbol(Dato, [_, Raiz, SubArbolDer], SubArbol):-
    >(Dato, Raiz),
    retornar_subarbol(Dato, SubArbolDer, SubArbol),!.

retornar_subarbol(Dato, [SubArbolIzq, Raiz, _], SubArbol):-
    <(Dato, Raiz),
    retornar_subarbol(Dato, SubArbolIzq, SubArbol),!.

retornar_subarbol(Dato, [SubArbolIzq, Dato, SubArbolDer], [SubArbolIzq, Dato, SubArbolDer]):-!.



/*
Un árbol binario se dice que es equilibrado si el número de niveles de sus subárboles
izquierdo y derecho no difieren en más de una unidad. Haga un programa en Prolog que
evalúe si un árbol es equilibrado.
*/

equilibrado(Arbol):-
    diferencia_alturas(Arbol, _).

diferencia_alturas([], 0):-!.

diferencia_alturas([SubArbolIzq, _, SubArbolDer], Altura):-
    diferencia_alturas(SubArbolIzq, AlturaIzq),
    diferencia_alturas(SubArbolDer, AlturaDer),
    is(Altura, +(1, max(AlturaIzq, AlturaDer))),
    =<(abs(-(AlturaIzq, AlturaDer)),1).

/* Dada una lista de números enteros, haga un programa que coloque todos los elementos
de la lista en un árbol binario de búsqueda. */

% [5,4,16,3,8,20]

lista_a_arbol([Cabeza|Cola], Arbol):-
  insertar(Cola, [[],Cabeza,[]], Arbol).

insertar([Cabeza|Cola], ArbolAux, Arbol):-
  insertar_elemento(ArbolAux, Cabeza, NuevoArbol),
  insertar(Cola, NuevoArbol, Arbol),!.

insertar([], ArbolAux, ArbolAux).

insertar_elemento([ArbolIzquierdo, Nodo, ArbolDerecho], Elemento, NuevoArbol):-
  <(Elemento, Nodo),
  insertar_elemento(ArbolIzquierdo, Elemento, ArbolAux),
  =(NuevoArbol, [ArbolAux, Nodo, ArbolDerecho]),!.

insertar_elemento([ArbolIzquierdo, Nodo, ArbolDerecho], Elemento, NuevoArbol):-
  >(Elemento, Nodo),
  insertar_elemento(ArbolDerecho, Elemento, ArbolAux),
  =(NuevoArbol, [ArbolIzquierdo, Nodo, ArbolAux]),!.

insertar_elemento([], Elemento, [ [], Elemento, [] ]).

/*
Elabore un programa en Prolog que cuente la cantidad de nodos de un árbol.
*/
% [[[[],44,[]],25,[[],10,[[],-9,[]]]],7,[[[],1,[]],2,[[[],0,[]],80,[[],21,[]]]]]

recorrer_y_contar([SubArbolIzq, _, SubArbolDer], Respuesta):-
    recorrer_y_contar(SubArbolIzq, ContadorIzq),
    recorrer_y_contar(SubArbolDer, ContadorDer),
    is(Respuesta, +(1, +(ContadorIzq, ContadorDer))),!.
    
recorrer_y_contar([], 0):-!.


/*
Elabore un programa en Prolog que invierta un árbol binario, es decir, todos los hijos
izquierdos pasan a ser hijos derechos, y viceversa.
*/

invertirArbol([SubArbolIzq, Nodo, SubArbolDer], [SubArbolDerechoInv, Nodo, SubArbolIzqInv]):-
    invertirArbol(SubArbolIzq, SubArbolIzqInv),
    invertirArbol(SubArbolDer, SubArbolDerechoInv).

invertirArbol([], []):- !.

/*Ejercicio 7, Dos arboles son iguales? */


sonIguales([AI, Nodo, AD], [AI, Nodo, AD]).