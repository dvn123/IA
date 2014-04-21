:-use_module(library(lists)).
:-use_module(library(random)).

numRunways(2).
temperatureDecrease(1).

%flight(id,tmin,tpref,tmax,cbefore,cafter,tafter)
flight(1,30,70,90,2,3,4).
flight(2,50,60,70,10,3,4).
flight(3,50,60,70,1,10,10).
flight(4,10,20,30,50,50,20).

initial_state([0-0, 4-0, 2-1, 0-1]).

:- use_module(library(lists)).
:- use_module(library(random)).

p1(100). % penalizacao por tempo superior ou inferior a limites
p2(100). % penalizacao por aterrar com subprosicao na mesma pista

faval_1plane([Time-_Runway], N, V) :-
        flight(N,Tmin,Tpref,Tmax,Cbefore,Cafter,_),
        p1(P1),
        (Time =< Tmax -> C1 is 0; C1 is P1*(Tmax-Time)),
        (Time >= Tmin -> C2 is C1; C2 is P1*(Tmax-Time)+C1),
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

add_cost(In,Cost,N,Out) :- add_cost(In,Cost,N,1,Out).
add_cost([],_,_,_,[]).
add_cost([H|T],Cost,N,N,[Ho|T]) :-
        Ho is H + Cost,!.
add_cost([H|T],Cost,ElemNum,N,[H|To]) :- N1 is N+1, add_cost(T,Cost,ElemNum,N1,To).

start_cost_list(0,[]) :- !.
start_cost_list(N,[H|T]) :- H is 0, N1 is N - 1, start_cost_list(N1,T).

accumulate_costs(Costs,Nflights,Out) :- start_cost_list(Nflights,In), accumulate_costs_aux(Costs,In,Out).
accumulate_costs_aux([],In,In).
accumulate_costs_aux([N1-N2-Penal|T],In,Out) :- add_cost(In,Penal,N1,O1), add_cost(O1,Penal,N2,O2), accumulate_costs_aux(T,O2,Out).

sum_2lists([],[],[]).
sum_2lists([H1|T1],[H2|T2],[H3|T3]) :- H3 is H1 + H2, sum_2lists(T1,T2,T3).

faval(List,Value) :- faval(List,Value,_).
faval(List,Value,CostPerFlight):-
        findall(N1-N2-Penal,(nth1(N1,List,F1),nth1(N2,List,F2),N1 < N2, faval_overlap(F1,N1,F2,N2,Penal)),Lpenal),
        length(List,Length),
        accumulate_costs(Lpenal,Length,Ltmp),
        findall(Cost,(nth1(N,List,F),faval_1plane(F,N,Cost)),Lcost),
        sum_2lists(Ltmp,Lcost,CostPerFlight),
        sumlist(CostPerFlight,Value).

build_flights(Lout) :-
        numRunways(NumRunways),
        NR1 is NumRunways+1,
        findall([Tpref-Runway],(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D),
               validate(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)),random(1,NR1,Runway)),Lout).

build_flights_random(Lout) :-
        numRunways(NumRunways),
        NR1 is NumRunways+1,
        findall([T-Runway],(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D),
               validate(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)),
               TM1 is Tmax + 1,
               random(Tmin,TM1,T),random(1,NR1,Runway)),Lout).

validate(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)) :-
        (Tmin>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref<Tmin -> write('Flight '), write(Id), write(' not valid'), break;true),
        (D=<0 -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Cafter<0 -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Cbefore<0 -> write('Flight '), write(Id), write(' not valid'), break;true).

showFlight(Id,Cost,[Hs-Hm]) :-
        write('Flight No '), write(Id),
        write(' -> landing time: '),
        write(Hs),
        write('; runway '),
        write(Hm),
        write('; cost '),
        write(Cost),
        write('.'),nl.

showFlights([],[],_).
        showFlights([H|T],[Hc|Tc],N) :- showFlight(N,Hc,H), N1 is N+1, showFlights(T,Tc,N1).

landing :- write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        build_flights(Initial),
        write(Initial),nl,
        faval(Initial,Value,Costs),
        showFlights(Initial,Costs,1),nl,
        write(Value).

neighbour(S,Sv, Neighbours, _) :-
        numRunways(NR),
        length(S, Length),
        random(X), %inc is positive or negative
        (X>0.5 -> Inc is 1; Inc is -1),
        random(0, Length, X1), %element to change
        Ele is round(X1),
        %write(element), write(Ele), nl,
        nth0(Ele,S,[OldTime-OldRunway]),
        random(X2), %change runway or time
        (X2>0.5 -> Runway is (1 + mod(OldRunway + Inc, NR)), Runway \= OldRunway, select([OldTime-OldRunway], S, [OldTime-Runway], Sv)
        ; Time is OldTime+Inc, Time > 0, Time \= OldTime, select([OldTime-OldRunway], S, [Time-OldRunway], Sv)),
        \+(member(Sv,Neighbours)).

neighbour(_S,Sv, Neighbours, 20) :-
        length(Neighbours, Length),
        Length1 is Length - 1,
        random(0,Length1,X),
        nth0(X,Neighbours, Sv).

neighbour(S,Sv, Neighbours, N):-
        N1 is N +1,
        neighbour(S,Sv, Neighbours, N1).        

probability(E, Enew, K, P) :-
        (Enew < E -> P is 1; P is K/10000).

simulated_annealing(_, Lf, _, Best, 0, _) :-
        Lf = Best.

simulated_annealing(_, Lf, Temperature, Best, BestScore, _) :-
        Temperature < 0,
        write('BestScore-'), write(BestScore), nl,
        Lf = Best.

simulated_annealing(L, Lf, Temperature, Best, BestScore, Visited) :-
        %write('Current State '), write(L), nl, write('Visited '), write(Visited), nl,
        neighbour(L,L1, Visited, 0),
        faval(L, Score),
        %write('Score '), write(Score), nl, write('Best Score '), write(BestScore), nl,
        probability(BestScore,Score,Temperature, P),
        %write('Probability '), write(P), nl,
        %write('Temperature '), write(Temperature), nl,
        random(X),
        temperatureDecrease(T),
        NewTemp is Temperature-T,
        (P > X -> (Score < BestScore -> simulated_annealing(L1, Lf, NewTemp, L1, Score, [L1| Visited]); simulated_annealing(L1, Lf, NewTemp, Best, BestScore, [L1| Visited]))
                  ; simulated_annealing(L, Lf, NewTemp, Best, BestScore, [L1| Visited])).

simulated_annealing(L, Lf) :-
        simulated_annealing(L, Lf, 10000, L, 9999, []).