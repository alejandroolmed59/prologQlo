%1r ejer

contarNodos([], 0):- !.

contarNodos([AI, Nodo , AD], Contador):-
    contarNodos(AI, ContadorIz),
    contarNodos(AD, ContadorDer),
    is(Contador, +(Nodo, +(ContadorIz, ContadorDer))).




hilera(Colonia):-

    =(Colonia , [_,_,_,_]),
    asignarHilera([OrdenGarcia, garcia], Colonia),
    asignarHilera([OrdenHernandez, hernandez], Colonia),
    (
        succ(OrdenGarcia, OrdenHernandez);
        succ(OrdenHernandez, OrdenGarcia) 
    ),
    =\=(OrdenHernandez, 1),
    asignarHilera([OrdenFuentes, fuentes], Colonia),
    asignarHilera([OrdenMorales, morales], Colonia),
    not(succ(OrdenFuentes, OrdenMorales)),
    not(succ(OrdenMorales, OrdenFuentes)),
    (
        succ(OrdenFuentes, OrdenHernandez);
        succ(OrdenHernandez, OrdenFuentes)
    ).
    


asignarHilera([1, Y], [[1, Y], _ , _ , _]).
asignarHilera([2, Y], [ _ ,[2, Y], _ , _]).
asignarHilera([3, Y], [ _ , _ ,[3,Y], _ ]).
asignarHilera([4, Y], [ _ , _ , _ , [4,Y]]).