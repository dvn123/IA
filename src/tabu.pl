niterations(1000).
tabu_tenure(50).
max_tabu_list_size(10).
factor(0). % valor maximo para obter funcao adaptacao



numRunways(2).

%flight(id,tmin,tpref,tmax,cbefore,cafter,tafter)
flight(1,30,70,90,2,3,4).
flight(2,50,60,70,10,3,4).
flight(3,50,60,70,1,10,10).
flight(4,10,20,30,50,50,20).





:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).

p1(700). % penalizacao por tempo superior ou inferior a limites
p2(700). % penalizacao por aterrar com subprosicao na mesma pista

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

join_eval([],[],[]).
join_eval([H1|T1],[H2|T2],[H3|T3]) :- factor(0), !, H3 is H1 + H2, join_eval(T1,T2,T3).
join_eval([H1|T1],[H2|T2],[H3|T3]) :- factor(V), H3 is V - (H1 + H2), join_eval(T1,T2,T3).

faval(List,Value) :- faval(List,Value,_).
faval(List,Value,CostPerFlight):-
        findall(N1-N2-Penal,(nth1(N1,List,F1),nth1(N2,List,F2),N1 < N2, faval_overlap(F1,N1,F2,N2,Penal)),Lpenal),
        length(List,Length),
        accumulate_costs(Lpenal,Length,Ltmp),
        findall(Cost,(nth1(N,List,F),faval_1plane(F,N,Cost)),Lcost),
        join_eval(Ltmp,Lcost,CostPerFlight),
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
        factor(F),
        (F =:= 0 -> C1 is Cost;C1 is F - Cost),
        write(C1),
        write('.'),nl.

showFlights([],[],_).
showFlights([H|T],[Hc|Tc],N) :- showFlight(N,Hc,H), N1 is N+1, showFlights(T,Tc,N1).



neighbour(S,Sv, Neighbours, N) :-
        N < 10,
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

neighbour(S,Sv, Neighbours, N):-
        N < 10,
        N1 is N +1,
        neighbour(S,Sv, Neighbours, N1).        

isTabu(L, TabuList) :-
        member(L, TabuList).

tabu_search_aux(L, Lf, TabuList, Visited) :-
        tabu_search_aux(L, [], Lf, TabuList, Visited).

tabu_search_aux(L, Aux, Lf, TabuList, Visited) :-
        neighbour(L,L1, Visited, 0),
        (isTabu(L1, TabuList) -> tabu_search_aux(L, Aux, Lf, TabuList, [L1, Visited]); tabu_search_aux(L, [L1|Aux], Lf, TabuList, [L1, Visited])).

tabu_search_aux(_, Aux, Lf, _, _) :-
        Lf = Aux.

choose_best([], Aux, Aux, _BestScore).

choose_best([X|L], Aux, Best, BestScore) :-
        %write('Choose_best2'), nl,
        %write('X - '), write(X), nl,
        %write([X|L]), nl,
        faval(X, Score),
        %write('Score - '), write(Score), nl,
        %write('BestScore - '), write(BestScore), nl,
        (Score < BestScore -> choose_best(L, X, Best, Score); choose_best(L, Aux, Best, BestScore)).

choose_best([X|L], Best) :-
        %write('Choose_best'), nl,
        choose_best([X|L], X, Best, 9999).

remove_expired([],Aux, NewTabuList) :-
        NewTabuList = Aux.
        
remove_expired([L-E], Aux, NewTabuList) :-
        tabu_tenure(Y),
        (E < Y -> remove_expired([], [[L-E]|Aux], NewTabuList); remove_expired([], Aux, NewTabuList)).

remove_expired([[L-E]|TabuList], Aux, NewTabuList) :-
        tabu_tenure(Y),
        (E < Y -> remove_expired(TabuList, [[L-E]|Aux], NewTabuList); remove_expired(TabuList, Aux, NewTabuList)).

remove_expired(X, X) :-
        length(X, Y),
        Y = 0.
   
remove_expired(TabuList, NewTabuList) :-
        length(TabuList, X),
        \+(X = 0),
        remove_expired(TabuList, [], NewTabuList).    

update([], []).
update([L3-T], [L3-T1]) :- 
        T1 is T + 1.

update([[L3-T]|L], [[L3-T1]|L2]) :- 
        T1 is T + 1,
        update(L, L2).

update_tabu_list([], L1, L) :-
        L1 = [[L-0]].

update_tabu_list(TabuList, NewTabuList, L) :-
        update(TabuList, L3),
        remove_expired(L3, L2),
        %write('L2 - '), write(L2), nl,
        length(L2, Len),
        Len > 1, !,
        %write(Len), nl, write(L2),
        nth1(Len, L2, Last),
        %write(L2), nl, write([Last]), nl,
        append(L1, [Last], L2),
        %write('L1 - '), write(L1), nl, write('Last - '), write(Last), nl,
        max_tabu_list_size(Y2),
        length([L1|Last], N),
        %write([L-0]), nl, write(L2), nl,
        (N > Y2 -> append([[L-0]], L1, NewTabuList); append([[L-0]], L2, NewTabuList)).

update_tabu_list(TabuList, NewTabuList, L) :-
        %write('2'), nl, 
        !, update(TabuList, L3), !,
        remove_expired(L3, L1), !,
        %write('List - '), write([[L]-0]), nl, write([L1]), nl,
        append([[L-0]], L1, NewTabuList).

        
%In its simplest form, a tabu list is a short-term set of the solutions that have been visited in the recent past (less than n iterations ago, where n is the number of previous solutions to be stored - is also called the tabu tenure).
%More commonly, a tabu list consists of solutions that have changed by the process of moving from one solution to another. 
%It is convenient, for ease of description, to understand a solution to be coded and represented by such attributes.

tabu_search(_, Lf, _, Best, 0, _) :-
        Lf = Best.

tabu_search(_, Lf, 0, Best, _BestScore, _) :-
        %write('BestScore - '), write(BestScore),
        Lf = Best.

tabu_search(L, Lf, Iterations, Best, BestScore, TabuList) :-
        write('Iterations - '), write(Iterations), nl, write('Current - '), write(L), nl, write('TabuList - '), write(TabuList), nl, write('BestScore - '), write(BestScore), nl,
        tabu_search_aux(L, Lout, TabuList, []), !,
        %write('Lout - '), write(Lout), nl,
        write('2 - '), nl,
        %write(Lout), nl, !,
        (Lout = [] -> build_flights_random(CandidateBest); choose_best(Lout, CandidateBest)),!,
        write('3 - '), nl,
        faval(CandidateBest, CandidateBestScore), !,
        write('4 - '), write('CandidateBestScore - '), write(CandidateBestScore), nl,
        Iterations1 is Iterations - 1, !,
        write('5 - '), nl, nl, !,
        %trace, notrace,
        (CandidateBestScore < BestScore ->  update_tabu_list(TabuList, NewTabuList, CandidateBest), !, tabu_search(CandidateBest, Lf, Iterations1, CandidateBest, CandidateBestScore, NewTabuList); update_tabu_list(TabuList, NewTabuList, CandidateBest), !, tabu_search(Best, Lf, Iterations1, Best, BestScore, NewTabuList)).

tabu_search(L, Lf) :-
        niterations(X),
        tabu_search(L, Lf, X, L, 9999, []).

landing :- ['landing.pl'],nl,nl,write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        build_flights(Lin),
        tabu_search(Lin,Lout),
        faval(Lout,_,Costs),
        showFlights(Lout,Costs,1),nl.