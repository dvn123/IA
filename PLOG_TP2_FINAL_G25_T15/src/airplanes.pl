:- use_module(library(clpfd)).

buildTasks(Lf,Ls,Le,Lc,Lm,Lt) :- length(Lf,Nflights), length(Ls,Nflights), length(Le,Nflights),length(Lc,Nflights),
                                          length(Lm,Nflights), numRunways(Nmac), domain(Ls,0,sup),
                                          domain(Le,0,sup), domain(Lm,1,Nmac), domain(Lc,0,sup),
                                          iterateflights(Lf,Ls,Le,Lm,Lc,[],Lt).

iterateflights([],_,_,_,_,Lt,Lt).                         
iterateflights([flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,D)|Tf],[Hs|Ts],[He|Te],[Hm|Tm],[Hc|Tc],Ltemp, Lt) :-
        Tendmin is Tmin + D, Tendmax is Tmax + D,
        
        (Tmin>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref>Tmax -> write('Flight '), write(Id), write(' not valid'), break;true),
        (Tpref<Tmin -> write('Flight '), write(Id), write(' not valid'), break;true),
        (D=<0 -> write('Flight '), write(Id), write('not valid'), break;true),
        (Cafter<0 -> write('Flight '), write(Id), write('not valid'), break;true),
        (Cbefore<0 -> write('Flight '), write(Id), write('not valid'), break;true),

        domain([Hs],Tmin,Tmax), domain([He],Tendmin,Tendmax),
        Hs #< Tpref #<=> B, Hc #= Cbefore*(Tpref-Hs)*B + Cafter*(Hs-Tpref)*(1-B),
        append(Ltemp,[task(Hs,D,He,1,Hm)],Out),
        iterateflights(Tf,Ts,Te,Tm,Tc,Out,Lt).         

buildmachines(N,N,Temp,Lout) :- append(Temp,[machine(N,1)],Lout),!.
buildmachines(I,N,Temp,Lout) :- append(Temp,[machine(I,1)],Out), I1 is I+1, buildmachines(I1,N,Out,Lout).

schedule(MilisecondsTimeOut,Lf,Ls,Le,Lc,Lm,SumCost,FlagTimeOout) :-
        findall(flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,Tafter),flight(Id,Tmin,Tpref,Tmax,Cbefore,Cafter,Tafter),Lf),
        numRunways(Nmac),
        buildmachines(1,Nmac,[],Machines),
        buildTasks(Lf,Ls,Le,Lc,Lm,Lt),
        domain([SumCost],0,sup),
        sum(Lc,#=,SumCost),
        cumulatives(Lt,Machines,[bound(upper)]),
        append(Ls,Lm,V1),
        append(V1,[SumCost],Vars),
        (MilisecondsTimeOut > 0 -> Options = [bisect,down,minimize(SumCost),time_out(MilisecondsTimeOut,FlagTimeOout)];
         Options = [bisect,down,minimize(SumCost)]),
        labeling(Options, Vars).

showFlights([],[],[],[]).
showFlights([flight(Id,_,_,_,_,_,_)|Tf],[Hs|Ts],[Hc|Tc],[Hm|Tm]) :- 
                                                                                              write('Flight No '), write(Id),
                                                                                              write(' -> landing time: '),
                                                                                              write(Hs),
                                                                                              write('; runway '),
                                                                                              write(Hm),
                                                                                              write('; cost '),
                                                                                              write(Hc),
                                                                                              write('.'),nl,
                                                                                              showFlights(Tf,Ts,Tc,Tm).

readSeconds(Out) :- write('Seconds to timeout (0 for no timeout)'),nl,read(NumSecs), isNumber(NumSecs,Out).
isNumber(X,X) :- number(X), X >= 0, !.
isNumber(_,Z) :- write('Invalid'), read(Y), !, isNumber(Y, Z).
																							  
landing :- write('Landing System. Input name of file:'),nl,read(FileName),[FileName],
		   readSeconds(NumSecs), MilisecondsTimeOut is NumSecs*1000,
           write('Starting...'),nl,
           statistics(runtime, [T0|_]),
           schedule(MilisecondsTimeOut,Lf,Ls,_,Lc,Lm,SumCost,FlagTimeOout),
           statistics(runtime, [T1|_]),
           (NumSecs > 0 -> write('Timeout: '), write(FlagTimeOout), nl;true),
           length(Lf,N), numRunways(Nmac), write(N), write(' flight(s) scheduled to land on '), write(Nmac), write(' runway(s).'), nl,
           nl,
           showFlights(Lf,Ls,Lc,Lm),nl, write('Total Cost: '), write(SumCost),write('.'),nl,nl,nl,
           write('Statistics:'),nl,
           fd_statistics,nl,
           T is T1 - T0,
           format('task took ~3d sec.~n', [T]).
