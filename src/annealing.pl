temperatureDecrease(1).
factor(0). % valor maximo para obter funcao adaptacao

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


landing :- ['landing.pl'],nl,nl,write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        build_flights(Lin),
        simulated_annealing(Lin,Lout),
        faval(Lout,_,Costs),
        showFlights(Lout,Costs,1),nl.