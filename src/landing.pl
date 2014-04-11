:- use_module(library(lists)).
:- use_module(library(random)).

p1(100). % penalizacao por tempo superior ou inferior a limites
p2(100). % penalizacao por aterrar com subprosicao na mesma pista

faval_1plane([Time-_Runway], N, V) :-
        flight(N,Tmin,Tpref,Tmax,Cbefore,Cafter,D),
        p1(P1),
        (Time =< Tmax+D -> C1 is 0; C1 is P1*(Tmax+D-Time)),
        (Time >= Tmin -> C2 is C1; C2 is P1*(Tmax+D-Time)+C1),
        (C2 > 0 -> V is C2; (Time < Tpref -> V is Cbefore*(Tpref-Time); V is Cafter*(Time-Tpref))).

faval_overlap([T1-R],N1,[T2-R],N2,Penal) :- % para a mesma pista
        flight(N1,_Tmin0,_Tpref0,_Tmax0,_Cbefore0,_Cafter0,D1),
        T1end is T1 + D1,
        flight(N2,_Tmin,_Tpref,_Tmax,_Cbefore,_Cafter,D2),
        T2end is T2 + D2,
        p2(P2),
        (T1 < T2 -> (T1end > T2 -> Penal is P2; Penal is 0); (T2end > T1 -> Penal is P2; Penal is 0)),
       % format('T1 ~d, N1 ~d, D1 ~d T2 ~d N2 ~d D2 ~d Penal ~d',[T1,N1,D1,T2,N2,D2,Penal]),nl,
        !.
faval_overlap([_T1-_R1],_N1,[_T2-_R2],_N2,0). % em pistas separadas o custo e 0

faval(List,Value):-
        findall(Penal,(nth1(N1,List,F1),nth1(N2,List,F2),N1 < N2, faval_overlap(F1,N1,F2,N2,Penal)),Lpenal),
        sumlist(Lpenal,Vtmp),
        findall(Cost,(nth1(N,List,F),faval_1plane(F,N,Cost)),Lcost),
        sumlist(Lcost,Vtmp2),
        Value is Vtmp + Vtmp2.

build_flights(Lout) :-
        numRunways(NumRunways),
        NR1 is NumRunways+1,
        findall([Tpref-Runway],(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D),
               validate(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)),random(1,NR1,Runway)),Lout).

validate(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)) :-
        (Tmin>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref<Tmin -> write('Flight '), write(Id), write(' not valid'), break;true),
        (D=<0 -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Cafter<0 -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Cbefore<0 -> write('Flight '), write(Id), write(' not valid'), break;true).

showFlights(flight(Id,_,_,_,_,_,_),[Hs-Hm]) :- 
                                                                                              write('Flight No '), write(Id),
                                                                                              write(' -> landing time: '),
                                                                                              write(Hs),
                                                                                              write('; runway '),
                                                                                              write(Hm),
                                                                                              write('; cost '),
                                                                                              faval_1plane([Hs-Hm],Id,Hc),
                                                                                              write(Hc),
                                                                                              write('.'),nl.

landing :- write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        build_flights(Initial),
        write(Initial),nl,
        faval(Initial,Value),
        write(Value).
        