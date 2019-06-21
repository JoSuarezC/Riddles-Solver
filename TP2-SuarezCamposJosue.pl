% Autor: Josu� Su�rez Campos
% Fecha: 16/5/2018

%========= Relaciones que definen el mecanismo gen�rico de resoluci�n =========

% La relaci�n test_puzzle/2 define un acertijo (puzzle/4) como un cu�druple que consiste de:
%   a) Structure: una estructura con los datos a determinar.
%   b) Clues: una lista de pistas que van instanciando los elementos de la estructura.
%   c) Queries: una lista de consultas a la estructura para responder a las preguntas.
%   d) Solution: una lista con las respuestas del acertijo.

test_puzzle(Name,puzzle(Structure,Clues,Queries,Solution)):-
   structure(Name,Structure),
   clues(Name,Structure,Clues),
   queries(Name,Structure,Queries,Solution).


% La relaci�n solve_puzzle/2 es el mecanismo por el cual se resuelve el acertijo.
% Simplemente se toma un acertijo con sus cuatro componentes. Va instanciando
% la estructura al ir resolviendo las pistas y las consultas.

solve_puzzle(puzzle(_, Clues,Queries,Solution),Solution):-    %_ = Structures
   solve(Clues),solve(Queries).


% La relaci�n solve/1 va tomando uno por uno los elementos de las listas
% ya sea de pistas o de consultas y los resuelve.
% N�tese c�mo solve toma el car de la lista de entrada y en la parte derecha
% usa ese car como meta a resolver.

solve([Clue|Clues]):-Clue,solve(Clues).
solve([]).


% Para mayor claridad se incluy� una relaci�n que imprime en
% diferentes l�neas las entradas de una lista.
% Se usar� para mostrar Structure.
mostrar([]).
mostrar([C|Cs]) :- writeln(C),mostrar(Cs).


% Esta es la relaci�n a llamar para resolver un acertijo.
% Ejemplo de uso:
%    ?- resolver(viajes, Struct, Sol).
%    viaje(4,peck,protestas)
%    viaje(5,maddy,olimpiadas)
%    viaje(6,linda,eleccion)
%    viaje(7,tam,boda)
%    Struct = [viaje(4, peck, protestas), viaje(5, maddy, olimpiadas),
%              viaje(6, linda, eleccion), viaje(7, tam, boda)],
%    Sol = [['Las olimpiadas fueron cubiertas por ', maddy]]
%
% La l�nea "Struct = ..." se formateo para mayor legibilidad.

% resolver/3 recibe el nombre del acertijo y produce la soluci�n;
% se incluye Structure entre los t�rminos de resolver para que se pueda ver.
% Si no se incluye, Prolog lo usar� internamente pero no lo mostrar�.
% Eso har�a m�s dif�cil la depuraci�n.

resolver(Acertijo, Structure, Solucion) :-
         test_puzzle(Acertijo,Puzzle),    % Primero define el cu�druple usando el nombre.
         Puzzle=puzzle(Structure,_,_,_),  % Se extrae la estructura del cu�druple para poder ver y depurar.
         solve_puzzle(Puzzle,Solucion),   % Aplica las pistas y consultas y obtiene la soluci�n.
         mostrar(Structure).              % Muestra estructura en forma legible.


%========= Relaciones espec�ficas para un acertijo dado =========

% structure crea para un acertijo dado una lista con los elementos
% a determinar. Como no se ha resuelto a�n todas las partes de los
% elementos est�n sin instanciar:
%       viaje(Dia,Periodista,Evento)

% A veces es posible instanciar una parte; en este caso es la posici�n ocupada.
% structure(NombreAcertijo, ListaConElementosADeterminar)

% orden(Precio,Cliente,SuperAlimento,Fruta).

structure(ordenes,[orden(6,_,_,_),
                  orden(7,_,_,_),
                  orden(8,_,_,_),
                  orden(9,_,_,_),
                  orden(10,_,_,_)]).

% historia(viajero,crucero,destino,anho).

structure(historias,[historia(_,_,_,1983),
                  historia(_,_,_,1984),
                  historia(_,_,_,1985),
                  historia(_,_,_,1986),
                  historia(_,_,_,1987),
                  historia(_,_,_,1988),
                  historia(_,_,_,1989)
                  ]).





% Implementa las pistas dadas.
% Cada pista es un elemento de una lista.
% Cada una de ellas es una meta a resolver que va
% instanciando los elementos de la estructura.

clues(
      ordenes,   % identifica las pistas como del acertijo "viajes"
      Ordenes,   % la estructura del acertijo va atando todo

   [ pista1(Ordenes),  % Resuelve cada pista y va instanciando elementos de Ordenes
     pista2(Ordenes),
     pista3(Ordenes),
     pista4(Ordenes),
     pista5(Ordenes),
     pista6(Ordenes),
     pista7(Ordenes),
     pista8(Ordenes),
     pista9(Ordenes),
     pista10(Ordenes)
   ]).
   

clues(
      historias,   % identifica las pistas como del acertijo "viajes"
      Historias,   % la estructura del acertijo va atando todo

   [ pista1_C2(Historias),  % Resuelve pista y va instanciando elementos de Historias
     pista2_C2(Historias),
     pista3_C2(Historias),
     pista4_C2(Historias),
     pista5_C2(Historias),
     pista6_C2(Historias),
     pista7_C2(Historias),
     pista8_C2(Historias),
     pista9_C2(Historias),
     pista10_C2(Historias),
     pista11_C2(Historias),
     pista12_C2(Historias),
     pista13_C2(Historias),
     pista14_C2(Historias),
     pista15_C2(Historias),
     pista16_C2(Historias)

   ]).


% Esta relaci�n hace consultas a la estructura com�n y luego
% prepara las respuestas del enunciado.
queries(
        ordenes, % identifica las pistas como del acertijo "ordenes"
        Ordenes, % la estructura del acertijo va atando todo

        % Preguntas a la estructura
        %orden(Precio,Cliente,SuperAlimento,Fruta).
        [
          % Preguna %11.     �Qui�n pidi� mandarina?
          member(orden(_,QuienPidioMandarina,_,mandarina),Ordenes)
        ],

        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
          ['Las mandarinas fueron pedidas por ', QuienPidioMandarina]
        ]).

queries(
        historias, % identifica las pistas como del acertijo "historias"
        Historias, % la estructura del acertijo va atando todo

        % Preguntas a la estructura
        % historia(viajero,crucero,destino,anho)
        [
          % Caso 2 no tiene pregunta
          member(historia(_,_,_,1983),Historias)
        ],

        % Respuestas pedidas. Usa los valores determinados en la lista anterior.
        [
          ['']
        ]).
% 
% Implementaci�n de las pistas.
% Es mejor que cada pista sea un predicado aparte porque si se unen se debe
% evitar que haya variables comunes entre pistas. Toda conexi�n debe hacerse
% por medio de la estructura.



%Pistas Caso 1

%orden(Precio,Cliente,SuperAlimento,Fruta).

%1.      El cliente que pag� $6 no pidi� ar�ndanos.

pista1(E):-precio(6,O1), select(O1,E,E2),
           fruta(arandanos,O2), member(O2,E2).

%2.      El cliente que orden� semilla de lino pag� m�s que la persona que orden� pasto de trigo.

pista2(E):-superAlimento(semillaDeLino,O1), precio(P1,O1),
           select(O1,E,E2),
           superAlimento(pastoDeTrigo,O2), precio(P2,O2),
           select(O2,E2,_),
           P1 > P2.

%3.      Isabel pidi� semillas de chia.

pista3(E):-cliente(isabel,O1),superAlimento(semillasDeChia,O1),member(O1,E).


%4.      El cliente que solicit� gengibre es Paulette o es la persona que pag� $10.

pista4(E):-cliente(paulette,O1), superAlimento(gengibre,O1), select(O1,E,E2),
           precio(10,O2), member(O2,E2).

pista4(E):-superAlimento(gengibre,O1), precio(10,O1), select(O1,E,E2),
           cliente(paulette,O2), member(O2,E2).

%5.      Paulette, el cliente que pidi� ar�ndanos y la persona que pidi� naranjas,
%        son tres personas distintas.

pista5(E):-fruta(arandanos,O1), select(O1,E,E2),
           fruta(naranjas,O2), select(O2,E2,E3),
           cliente(paulette,O3), member(O3,E3).


%6.      El cliente que pidi� naranjas pag� 1 d�lar m�s que la persona que pidi�
         %bananos.

pista6(E):-fruta(naranjas,O1), precio(P1,O1),
           select(O1,E,E2),
           fruta(bananos,O2), precio(P2,O2),
           select(O2,E2,_),
           P1 is (P2 + 1).

%7.      Otis, o pag� $6 o pag� $10.

pista7(E):-cliente(otis,O1), precio(6,O1), member(O1,E).

pista7(E):-cliente(otis,O1), precio(10,O1), member(O1,E).

%8.      La persona que pidi� quinoa pag� $3 m�s que Mercedes.

pista8(E):-superAlimento(quinoa,O1), precio(P1,O1),
           select(O1,E,E2),
           cliente(mercedes,O2), precio(P2,O2),
           select(O2,E2,_),
           P1 is (P2 + 3).

%9.      Sobre Paulette y la persona que orden� frambuesas: una pidi� pasto de trigo
         %y la otra persona pag� $8.

pista9(E):-cliente(paulette,O1), superAlimento(pastoDeTrigo,O1),
           select(O1,E,E2),
           fruta(frambuesas,O2), precio(8,O2),
           select(O2,E2,_).
           
pista9(E):-cliente(paulette,O3), precio(8,O3),
           select(O3,E,E2),
           fruta(frambuesas,O4), superAlimento(pastoDeTrigo,O4),
           select(O4,E2,_).

%10.     Isabel pag� 3 d�lares menos que Amelia.

pista10(E):-cliente(isabel,O1), precio(P1,O1),
            select(O1,E,E2),
            cliente(amelia,O2), precio(P2,O2),
            select(O2,E2,_),
            P1 is (P2 - 3).

precio(P,orden(P,_,_,_)).
cliente(C,orden(_,C,_,_)).
superAlimento(A,orden(_,_,A,_)).
fruta(F,orden(_,_,_,F)).


%Pistas Caso 2

% historia(viajero,crucero,destino,anho)

%1.      Eugene no viaj� en el crucero Azure Seas.

pista1_C2(E):-  crucero(azureSeas,C1),
                select(C1,E,E2),
                viajero(eugene,V2),
                select(V2,E2,_).

%2.      La persona que fue a Trinidad zarp� 1 a�o antes que Lee.

pista2_C2(E):-  destino(trinidad,H1), anho(A1,H1),
                select(H1,E,E2),
                viajero(lee,H2), anho(A2,H2),
                select(H2,E2,_),
                A1 is (A2 - 1).

%3.      La persona que se embarc� en el crucero Silver Shores es Francis o
         %es quien viaj� en 1984.

pista3_C2(E):-  crucero(silverShores,C1),viajero(francis,C1),
                select(C1,E,E2),
                anho(1984,A2),
                select(A2,E2,_).

pista3_C2(E):-  anho(1984,A1),crucero(silverShores,A1),
                select(A1,E,E2),
                viajero(francis,C2),
                select(C2,E2,_).

%4.      Los siete viajeros son: la persona que fue a Saint Lucia, Greg,
         %la persona que se embarc� en el crucero Neptunia, la persona que viaj� en 1987,
         %la persona que tom� el crucero Trinity, la persona que se embarc� en el crucero
         %Baroness y la persona que tom� un crucero en 1986.

pista4_C2(E):-  destino(saintLucia,D1),
                select(D1,E,E2),
                viajero(greg,V2),
                select(V2,E2,E3),
                crucero(neptunia,C3),
                select(C3,E3,E4),
                anho(1987,A4),
                select(A4,E4,E5),
                crucero(trinity,C5),
                select(C5,E5,E6),
                crucero(baroness,C6),
                select(C6,E6,E7),
                anho(1986,A7),
                select(A7,E7,_).

%5.      Sobre los que tomaron el crucero Farralon y el crucero Caprica, uno es Greg y el otro fue a Martinique.

pista5_C2(E):-  crucero(farralon,C1),viajero(greg,C1),
                select(C1,E,E2),
                crucero(caprica,C2),destino(martinique,C2),
                select(C2,E2,_).

pista5_C2(E):-  crucero(farralon,C1),destino(martinique,C1),
                select(C1,E,E2),
                crucero(caprica,C2),viajero(greg,C2),
                select(C2,E2,_).

%6.      La persona que fue a Puerto Rico viaj� 1 a�o despu�s de la persona que tom� el crucero Silver Shores.

pista6_C2(E):-  destino(puertoRico,D1),anho(A1,D1),
                select(D1,E,E2),
                crucero(silverShores,C2),anho(A2,C2),
                select(C2,E2,_),
                A1 is (A2 + 1).

%7.      Kathy no viaj� en el crucero Azure Seas.

pista7_C2(E):-  crucero(azureSeas,C1),
                select(C1,E,E2),
                viajero(kathy,V2),
                select(V2,E2,_).

%8.      Natasha viaj� ya sea en el crucero Baroness o en el crucero de 1985.

pista8_C2(E):-  viajero(natasha,V1),crucero(baroness,V1),
                select(V1,E,E2),
                anho(1985,A2),
                select(A2,E2,_).

pista8_C2(E):-  viajero(natasha,V1),anho(1985,V1),
                select(V1,E,E2),
                crucero(baroness,C2),
                select(C2,E2,_).

%9.      La persona que fue a Martinique est� entre Eugene y la persona que tom� el crucero Caprica.

% pista9_C2(E):- viajero(eugene,H1),anho(EugeneA,H1),
%                select(H1,E,E2),
%                crucero(caprica,H2),anho(CapricaA,H2),
%                select(H2,E2,E3),
%                destino(martinique,H3),anho(MartiniqueA,H3),
%                select(H3,E3,_),
%                MartiniqueA > EugeneA, CapricaA > MartiniqueA.
%                

pista9_C2(E):-  destino(martinique,D1),viajero(eugene,D1),
                select(D1,E,E2),
                crucero(caprica,C2),
                select(C2,E2,_).

pista9_C2(E):-  destino(martinique,D1),crucero(caprica,D1),
                select(D1,E,E2),
                viajero(eugene,V2),
                select(V2,E2,_).

%10.     La persona que tom� el crucero de 1987 no fue la misma que viaj� en el crucero Caprica.

pista10_C2(E):- anho(1987,A1),
                select(A1,E,E2),
                crucero(caprica,C2),
                select(C2,E2,_).

%11.     Sobre Francis y la persona que fue a Trinidad: uno estuvo en el crucero de 1983 y el otro tom� el crucero Neptunia.

pista11_C2(E):- viajero(francis,V1),anho(1983,V1),
                select(V1,E,E2),
                destino(trinidad,D2),crucero(neptunia,D2),
                select(D2,E2,_).

pista11_C2(E):- viajero(francis,V1),crucero(neptunia,V1),
                select(V1,E,E2),
                destino(trinidad,D2),anho(1983,D2),
                select(D2,E2,_).

%12.     Bradley, o fue a Jamaica o m�s bien tom� el crucero de 1987.

pista12_C2(E):- viajero(brandley,V1),destino(jamaica,V1),
                select(V1,E,E2),
                anho(1987,A2),
                select(A2,E2,_).

pista12_C2(E):- viajero(brandley,V1),anho(1987,V1),
                select(V1,E,E2),
                destino(jamaica,D2),
                select(D2,E2,_).

%13.     La persona que fue a Grenada viaj� 2 a�os despu�s que Kathy.

pista13_C2(E):- destino(granada,D1),anho(A1,D1),
                select(D1,E,E2),
                viajero(kathy,V2),anho(A2,V2),
                select(V2,E2,_),
                A1 is (A2 + 2).

%14.     La persona que tom� el crucero Neptunia lo hizo 1 year a�o despu�s de que qui�n tom� el crucero Silver Shores.

pista14_C2(E):- crucero(neptunia,C1),anho(A1,C1),
                select(C1,E,E2),
                crucero(silverShores,C2),anho(A2,C2),
                select(C2,E2,_),
                A1 is (A2 + 1).

%15.     La persona que viaj� en el crucero Trinity zarp� 1 a�o despu�s de quien tom� el crucero Baroness.

pista15_C2(E):- crucero(trinity,C1),anho(A1,C1),
                select(C1,E,E2),
                crucero(baroness,C2),anho(A2,C2),
                select(C2,E2,_),
                A1 is (A2 + 1).

%16.     Uno de los viajeros fue a Barbados.

pista16_C2(E):- destino(barbados,B1),
                member(B1,E).



% historia(viajero,crucero,destino,anho)
viajero(V,historia(V,_,_,_)).
crucero(C,historia(_,C,_,_)).
destino(D,historia(_,_,D,_)).
anho(A,historia(_,_,_,A)).

