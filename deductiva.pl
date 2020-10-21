% 1)En una carrera, Tato llegó a la meta inmediatamente después de Milo,
% Nino no llegó en primer lugar, José llegó antes que Tato y Nino llegó
% después de Milo. ¿En qué lugares llegaron estos corredores?

cuatro_corredores(Lugares):-
    =(Lugares,[_, _, _, _]),
    lugar([T, tato], Lugares),
    lugar([M, milo], Lugares),
    succ(M, T),
    lugar([N, nino], Lugares),
    =\=(N, 1),
    lugar([J, jose], Lugares),
    <(J, T),
    >(N, M).

lugar([1, Nombre], [[1, Nombre],_,_,_]).
lugar([2, Nombre], [_,[2, Nombre],_,_]).
lugar([3, Nombre], [_,_,[3, Nombre],_]).
lugar([4, Nombre], [_,_,_,[4, Nombre]]).


/*
Don Chilo, doña Canuta, don Mario, la niña Lencha y doña Marina viven en un edificio
de cinco plantas. 
Todos ellos viven en distintos pisos. 
Don Chilo no vive en el quinto piso 
y doña Canuta no vive en el primero.
Don Mario no vive en el piso superior ni en el
inferior, 
y no está en un piso adyacente al de doña Marina ni al de doña Canuta. 
La niña Lencha vive en algún piso por encima de doña Canuta. ¿Quién vive en cada piso?
*/

quienViveEnCadaPiso(Edificio):-
	Edificio = [_, _, _, _, _],
  
  asignarpiso([chilo, PisoChilo], Edificio),
  =\=(PisoChilo, 5),
  
  asignarpiso([canuta, PisoCanuta], Edificio),
  =\=(PisoCanuta, 1),

   asignarpiso([mario, PisoMario], Edificio),
   =\=(PisoMario, 5),
   =\=(PisoMario, 1),  
    
   asignarpiso([marina, PisoMarina], Edificio),
    % <(PisoMario, -(PisoMarina,1)), 
     
    not(succ(PisoMario, PisoMarina)),
	not(succ(PisoMarina, PisoMario)),
    
	not(succ(PisoMario, PisoCanuta)), 
	not(succ(PisoCanuta, PisoMario)), 
	
	asignarpiso([lencha, PisoLencha], Edificio),
	>(PisoLencha, PisoCanuta).

asignarpiso([X, Y], [[X, Y], _, _, _, _]):- is(Y, 1).
asignarpiso([X, Y], [_, [X, Y], _, _, _]):- is(Y, 2).
asignarpiso([X, Y], [_, _, [X, Y], _, _]):- is(Y, 3).
asignarpiso([X, Y], [_, _, _, [X, Y], _]):- is(Y, 4).
asignarpiso([X, Y], [_, _, _, _, [X, Y]]):- is(Y, 5).

/*
Tres señoras de apellidos Blanco, Rubio y Castaño, se conocen en una reunión. Poco
después de hacerse las presentaciones, una de ellas hace notar: "Es muy curioso que
nuestros apellidos sean Blanco Rubio y Castaño, y que nos hayamos reunido aquí tres
personas con eso mismos colores de cabello". "Sí que lo es -dijo la dama que de pelo
rubio-, pero habrá observado que nadie tiene el color de pelo que corresponde a su
apellido." "¡Es verdad!" -exclamó la señora de apellido Blanco. Si la última dama no
tiene el pelo castaño, ¿de qué color es el cabello de las tres señoras? */

cualPelo(Pelo):-
    =(Pelo, [_,_,_]),
    pelo([sBlanca, ColorB], Pelo),
    \==(ColorB, blanco),
    \==(ColorB, castannio),

    pelo([sRubio, ColorR],Pelo),
    \==(ColorR, rubio),
    \==(ColorR, ColorB),

    pelo([sCastannio, ColorC],Pelo),
    \==(ColorC, ColorR),
    \==(ColorC, ColorB),
    \==(ColorC, castannio), !.


pelo([X, blanco], [[X, blanco],_,_]).
pelo([X, rubio], [_,[X, rubio],_]).
pelo([X, castannio], [_,_,[X, castannio]]).


/*
Cinco amigos estaban desesperados por ir a celebrar el fin de exámenes al Tunco. 
Acordaron que, después del último examen, saldrían volando a la playa, sin esperar a
nadie que se tardara. 
El día del último examen, el primero en terminarlo fue Chico,
Paco salió después de Pancho, 
Chepe no fue al paseo 
y José salió inmediatamente antes de
Paco. 
En qué orden terminaron el examen?
*/

ordenExamen(Orden):-
    =(Orden, [_,_,_,_,_]),

    =(OrdenChico, 1),
    examen_finalizado([chico, OrdenChico], Orden),
    
    examen_finalizado([paco, OrdenPaco], Orden),
    examen_finalizado([pancho, OrdenPancho], Orden),
    
    >(OrdenPaco, OrdenPancho),
    
    examen_finalizado([chepe, OrdenChepe], Orden),
    =(OrdenChepe, 5),

    examen_finalizado([jose, OrdenJose], Orden),
    
    succ(OrdenJose, OrdenPaco),!.
    
examen_finalizado([X,1], [[X,1],_,_,_,_]).
examen_finalizado([X,2], [_,[X,2],_,_,_]).
examen_finalizado([X,3], [_,_,[X,3],_,_]).
examen_finalizado([X,4], [_,_,_,[X,4],_]).
examen_finalizado([X,5], [_,_,_,_,[X,5]]).

% Angel, Boris, Cesar y Diego salieron a tomarse un trago y se
% sentaron en la barra de un restaurante.

% Angel se sento un puesto a la derecha del que bebio tequila.
% Los que bebieron vino y cerveza no se sentaron a la par.
% El que bebio champaña estaba en uno de los extremos de la barra.
% Diego y quien bebio vino se sentaron en asientos contiguos.
% Cesar se sento inmediatamente a la derecha de Angel.
% Boris no bebio cerveza.

% Indique las ubicaciones en que se sentaron los cuatro amigos y
% las bebidas que tomo cada uno.

%      Bebida1   Bebida2   Bebida3   Bebida4
%      --------------------------------------
%      --------------------------------------
%      Amigo1    Amigo2    Amigo3    Amigo4

que_bebieron(Barra):-
	=(Barra, [[_, _, _], [_, _, _],[_, _, _],[_, _, _]]),

	puesto([PosAngel, angel, _], Barra),
	puesto([PosTequila, _, tequila], Barra),
	succ(PosTequila, PosAngel),

	puesto([PosVino, _, vino], Barra),
	puesto([PosCerveza, _, cerveza], Barra),
	(not(succ(PosCerveza, PosVino)) , not(succ(PosVino, PosCerveza))),

	(puesto([1, _, champana], Barra); puesto([4, _, champana], Barra)),

	puesto([PosDiego, diego, _], Barra),
	(succ(PosVino, PosDiego) ; succ(PosDiego, PosVino)),

	puesto([PosCesar, cesar, _], Barra),
	succ(PosAngel, PosCesar),

	puesto([PosBoris, boris, _], Barra),
	=\=(PosBoris, PosCerveza),!.

puesto([1, A, B], [[1, A, B], [_, _, _],[_, _, _],[_, _, _]]).
puesto([2, A, B], [[_, _, _], [2, A, B],[_, _, _],[_, _, _]]).
puesto([3, A, B], [[_, _, _], [_, _, _],[3, A, B],[_, _, _]]).
puesto([4, A, B], [[_, _, _], [_, _, _],[_, _, _],[4, A, B]]).

/*En un estanque hay siete piedras que sobresalen del
agua. Hay tres ranas verdes subidas, cada una, sobre las tres
piedras del extremo izquierdo y tres ranas cafes subidas sobre las
tres piedras del extremo derecho.
Si cada piedra solo puede ser ocupada por una
rana a la vez, ¿cómo deben hacer las seis las ranas para trasladarse, las verdes
hacia la derecha y las café hacia ala izquierda?*/

/*Restricciones:
* Cada rana puede avanzar dando un salto sencillo o un salto doble.
El salto sencillo consiste en saltar hacia la piedra que
tiene adelante, si está vacía. El salto doble consiste en saltar hacia la segunda piedra
que tiene adelante, si está vacía.
* Las ranas verdes solo pueden avanzar hacia la derecha sin retroceder
* Las ranas cafes solo pueden avanzar a la izquierda sin retroceder.
* Cualquier rana puede avanzar a la siguiente piedra si esta desocupada o
saltar sobre otra rana si la segunda piedra adelante de su posicion esta desocupada.
% v representa una rana de color verde.
% c representa una rana de color cafe.
% d representa la piedra disponible.
*/
juego_ranas(Estado_Inicial):-
	Estado_Inicial = [v,v,v,d,c,c,c],
	juego_ranas1(Estado_Inicial, [Estado_Inicial]).

juego_ranas1([c,c,c,d,v,v,v], Estados_Juego):-
	write('FIN DEL JUEGO. Las ranas llegaron a su destino.'), nl,
	write(Estados_Juego),nl,nl.

juego_ranas1(Estado_Actual, Estados_Juego):-
	buscar_piedra_desocupada(Estado_Actual, Estados_Juego).

buscar_piedra_desocupada(Estado_Actual, Estados_Juego):-
	mover_rana_verde(Estado_Actual, Nuevo_Estado),
	guardar_Estado(Estados_Juego, Estados_Juego2, Nuevo_Estado),
	juego_ranas1(Nuevo_Estado, Estados_Juego2).

buscar_piedra_desocupada(Estado_Actual, Estados_Juego):-
	mover_rana_cafe(Estado_Actual, Nuevo_Estado),
	guardar_Estado(Estados_Juego, Estados_Juego2, Nuevo_Estado),
	juego_ranas1(Nuevo_Estado, Estados_Juego2).

guardar_Estado(Estados_Juego, Estados_Juego2, Nuevo_Estado):-
	revisar(Estados_Juego, Nuevo_Estado),
	=(Estados_Juego2, [Nuevo_Estado|Estados_Juego]).

revisar([], _).
revisar([EstadoAnt|EstadosAnt], Nuevo_Estado):-
	Nuevo_Estado \== EstadoAnt,
	revisar(EstadosAnt, Nuevo_Estado).

%Las ranas verdes pueden saltar hacia la siguiente piedra de
%la derecha, segun las siguientes posibilidades:
mover_rana_verde([v,d,A,B,C,D,E], [d,v,A,B,C,D,E]).
mover_rana_verde([A,v,d,B,C,D,E], [A,d,v,B,C,D,E]).
mover_rana_verde([A,B,v,d,C,D,E], [A,B,d,v,C,D,E]).
mover_rana_verde([A,B,C,v,d,D,E], [A,B,C,d,v,D,E]).
mover_rana_verde([A,B,C,D,v,d,E], [A,B,C,D,d,v,E]).
mover_rana_verde([A,B,C,D,E,v,d], [A,B,C,D,E,d,v]).

%Las ranas verdes pueden saltar dos posiciones a la derecha,
%segun las siguientes posibilidades:
mover_rana_verde([v,A,d,B,C,D,E], [d,A,v,B,C,D,E]).
mover_rana_verde([A,v,B,d,C,D,E], [A,d,B,v,C,D,E]).
mover_rana_verde([A,B,v,C,d,D,E], [A,B,d,C,v,D,E]).
mover_rana_verde([A,B,C,v,D,d,E], [A,B,C,d,D,v,E]).
mover_rana_verde([A,B,C,D,v,E,d], [A,B,C,D,d,E,v]).

%Las ranas cafe pueden saltar hacia la siguiente piedra de
%la izquierda, segun las siguientes posibilidades:
mover_rana_cafe([A,B,C,D,E,d,c], [A,B,C,D,E,c,d]).
mover_rana_cafe([A,B,C,D,d,c,E], [A,B,C,D,c,d,E]).
mover_rana_cafe([A,B,C,d,c,D,E], [A,B,C,c,d,D,E]).
mover_rana_cafe([A,B,d,c,C,D,E], [A,B,c,d,C,D,E]).
mover_rana_cafe([A,d,c,B,C,D,E], [A,c,d,B,C,D,E]).
mover_rana_cafe([d,c,A,B,C,D,E], [c,d,A,B,C,D,E]).

%Las ranas cafe pueden saltar dos posiciones a la izquierda,
%segun las siguientes posibilidades:
mover_rana_cafe([A,B,C,D,d,E,c], [A,B,C,D,c,E,d]).
mover_rana_cafe([A,B,C,d,D,c,E], [A,B,C,c,D,d,E]).
mover_rana_cafe([A,B,d,C,c,D,E], [A,B,c,C,d,D,E]).
mover_rana_cafe([A,d,B,c,C,D,E], [A,c,B,d,C,D,E]).
mover_rana_cafe([d,A,c,B,C,D,E], [c,A,d,B,C,D,E]).