factor(10000). % valor maximo para obter funcao adaptacao


landing :- ['landing.pl'],nl,nl,write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
        write('Size of population'), nl, read(Size), nl,
        write('Number of generations'),nl, read(NumGen), nl,
        write('Crossover probability'),nl, read(CO), nl,
        genetic(Size,CO,NumGen,_,Best),
        faval(Best,_,Costs),tell('result.txt'),
        showFlights(Best,Costs,1),told,showFlights(Best,Costs,1),nl.


%% -------------------
genetic(Nelems,ProbCross,Ngens,CostBest,Best) :- build_pop(Nelems,Lout,C1), geneticIteration(Lout,C1,ProbCross,1,Ngens,Costs,BestSet),
              max_member(CostBest,Costs),
              nth1(Pos,Costs,CostBest),
              nth1(Pos,BestSet,Best).
        
        geneticIteration(Last,Costs,_,Ngens,Ngens,Costs,Last):- !.
        geneticIteration(Lout,C1,ProbCross,I,Ngens,Clast,Last) :-
                scale(C1,Cscale), replace(Cscale,Lout,Lnew),crossOver(ProbCross,Lnew,Out),
                findall(C,(member(L,Out),faval(L,C,_)),Costs),
                I1 is I + 1,
                geneticIteration(Out,Costs,ProbCross,I1,Ngens,Clast,Last).

build_pop(N,Lout,Cout) :- findall(L,(repeat(N),build_flights_random(L)),Lout),findall(C,(member(L,Lout),faval(L,C,_)),Cout).
scale(Cin,Cout) :- sumlist(Cin,V), findall(C,(member(Celem,Cin), C is Celem/ V),Cout).
rand_elems(N,Rand) :- findall(R,(repeat(N),random(R)),Rand).
accumulate_percent([],[],_).
accumulate_percent([Hin|Tin],[Hout|Tout],A) :- Hout is A + Hin, accumulate_percent(Tin,Tout,Hout).
select_rand_elems([],[],_,_).
select_rand_elems([Hr|Tr],[H|T],Prob,N) :- Hr < Prob,!, H is N, N1 is N+1, select_rand_elems(Tr,T,Prob,N1).
select_rand_elems([_|Tr],L,Prob,N) :- N1 is N+1, select_rand_elems(Tr,L,Prob,N1).
find_elem(R,N,Nout,[H|T]) :- H < R, N1 is N+1, !, find_elem(R,N1,Nout,T).
find_elem(_,N,N,_).
replace(Lcost,Lelem,Lout) :- accumulate_percent(Lcost,L1,0),% write(L1),
        length(L1,N), rand_elems(N,Rand), %write(Rand), 
        findall(A,(member(R,Rand),find_elem(R,1,Nelem,L1),nth1(Nelem,Lelem,A)),Lout).

setElem(In,I,New,Out) :- setElem(In,Out,New,1,I).
setElem([_|Ti],[New|Ti],New,N,N).
setElem([Hi|Ti],[Hi|T],New,I,N) :- I1 is I+1, setElem(Ti,T,New,I1,N).

cross(Lelems,[P1,P2|T],Out) :- !, nth1(P1,Lelems,E1), nth1(P2,Lelems,E2), length(E1, Esize),random(1,Esize,CrossPoint),
        %write(CrossPoint),
        Suf is Esize - CrossPoint,
        prefix_length(E1, E11, CrossPoint),
        prefix_length(E2, E21, CrossPoint),
        suffix_length(E1, E12, Suf),
        suffix_length(E2, E22, Suf),
        append(E11,E22,E1new),
        append(E21,E12,E2new),
        setElem(Lelems,P1,E1new,O1),
        setElem(O1,P2,E2new,O3),
        cross(O3,T,Out).
cross(O,_,O).

crossOver(Prob,Lin,Out) :- length(Lin,N), rand_elems(N,Rand), select_rand_elems(Rand,Lpairs,Prob,1), cross(Lin,Lpairs,Out).
