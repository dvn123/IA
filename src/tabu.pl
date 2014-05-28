niterations(20000).
tabu_tenure(1000).
max_tabu_list_size(1000).
factor(0). % valor maximo para obter funcao adaptacao

neighbour(S,Sv, Neighbours) :-
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

isTabuAux(L, [[L-_]]).

isTabuAux(L, [[L-_]|_]).    

isTabuAux(L, [_|L1]) :-
        isTabuAux(L, L1).  
 
isTabu(L, TabuList) :-
        isTabuAux(L, TabuList).

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

tabu_search(_, Lf, _, Best, 0, _) :-
        Lf = Best.

tabu_search(_, Lf, 0, Best, _BestScore, _) :-
        %write('BestScore - '), write(BestScore),
        Lf = Best.

tabu_search(L, Lf, Iterations, Best, BestScore, TabuList) :-
        %write('Iterations - '), write(Iterations), nl, write('Current - '), write(L), nl, write('TabuList - '), write(TabuList), nl, write('BestScore - '), write(BestScore), nl,
        %tabu_search_aux(L, Lout, TabuList, []), !,
        findall(X, (neighbour(L, X, []), \+(isTabu(X, TabuList))), Z), !,
        %write(Z), nl,
        %write('Lout - '), write(Lout), nl,
        %write('2 - '), nl,
        %write(Lout), nl, !,
        (Z = [] -> build_flights_random(CandidateBest); choose_best(Z, CandidateBest)),!,
        %write('3 - '), nl,
        faval(CandidateBest, CandidateBestScore), !,
        %write('4 - '), write('CandidateBestScore - '), write(CandidateBestScore), nl,
        Iterations1 is Iterations - 1, !,
        %write('5 - '), nl, nl, !,
        %trace, notrace,
        (CandidateBestScore < BestScore ->  update_tabu_list(TabuList, NewTabuList, CandidateBest), !, tabu_search(CandidateBest, Lf, Iterations1, CandidateBest, CandidateBestScore, NewTabuList); update_tabu_list(TabuList, NewTabuList, CandidateBest), !, tabu_search(CandidateBest, Lf, Iterations1, Best, BestScore, NewTabuList)).

tabu_search(L, Lf) :-
        niterations(X),
        tabu_search(L, Lf, X, L, 9999, []).

landing :- ['landing.pl'],nl,nl,write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        build_flights(Lin),
        statistics(total_runtime, [T0|_]),   
        tabu_search(Lin,Lout),
        statistics(total_runtime, [T1|_]),
        T is T1-T0,
        format('Time -  ~3d seconds.~n', [T]),
        faval(Lout, X),
        write('BestScore - '), write(X), nl,
        faval(Lout,_,Costs),
        showFlights(Lout,Costs,1),nl.