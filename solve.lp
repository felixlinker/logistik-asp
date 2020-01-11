
#const truck_capacity=20.
capacity_opt(0..truck_info).

max_steps=15.
steps(1..max_steps).

% predicate truck_info(truck, good, quantity, place, step).

% trucks are empty each morning and are parked at first warehouse (=place nr. factory_num+1).
truck_info(T, G, 0, factory_num + 1, 1) :- truck(T), good(G).

% capacity constraint for trucks
:- truck_capacity < #sum { Q*V : capacity_opt(Q) good(G), vol(G, V), truck_info(T, G, Q, P, S) } , truck(T), place(P), step(S).

% position of trucks at each step well-defined
:- truck_info(T, G, Q, P, S), truck_info(T, G', Q', P', S), truck(T), good(G), good(G'), capacity_opt(Q), capacity_opt(Q'), place(P), place(P'), step(S), P != P'.

% quantity of certain good in trucks at each step well-defined
:- truck_info(T, G, Q, P, S), truck_info(T, G, Q', P', S), truck(T), good(G), capacity_opt(Q), capacity_opt(Q'), place(P), place(P'), step(S), Q != Q'.

% predicate drive(truck, to, step) (************hilfe garantiert nicht richtig geraten)
{ drive(T, P, S) : place(P) } 1 :- truck(T), step(S).
% don't drive around in circles; otherwise useless extensions since cost=0
:- truck_info(T, G, Q, P, S), drives(T, P, S), truck(T), good(G), capacity_opt(Q), place(P), step(S).

% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S), not drive(T, P', 1).
% clean schedule: need time to load and unload, see below (distance between S and S' should be at least 3)
:- drive(T, P, S), drive(T, P', S'), (S-S')*(S'-S)>-6, truck(T), place(P), place(P'), step(S), step(S').
% clean schedule: additional predicate after(T, S, S') to help ensure that we drive every third step
after(T, S, S') :- drive(T, P, S), drive(T, P', S'), S<S'.
% clean schedule: if we keep driving, we must drive after precisely 3 steps
:- drive(T, P, S), after(T, S, S'), not drive(T, P, S+3).

% after driving, truck arrives
truck_info(T, G, Q, P, S+1) :- truck_info(T, G, Q, P', S), drives(T, P, S), truck(T), good(G), capacity_opt(Q), place(P), place(P'), step(S).

% predicate unload(truck, good, quantity, place, step).
% after arriving, truck unloads
1 { unload(T, G, Q, P, S+1) : capacity_opt(Q) } 1 :- truck(T), good(G), place(P), step(S), drive(T, P, S).
% do not unload too much
:- unload(T, G, Q, P, S), truck_info(T, G, Q', P, S), Q > Q', truck(T), good(G), capacity_opt(Q), capacity_opt(Q'), place(P), step(S).
% after unloading, update truck info
truck_info(T, G, Q', P, S+2) :- unload(T, G, Q, P, S+1).

% predicate load(truck, good, quantity, place, step).
1 { load(T, G, Q, P, S+1) : capacity_opt(Q) } 1 :- truck(T), good(G), place(P), step(S), unload(T, G, Q, P, S).
% cannot load too much since capacity constrained above

% total expenses E from all the driving(*************wie schreib ich die zu minimierende variable E auf?)
:- E != #sum { C : cost(X, Y, C), truck_info(T, 1, Q, X, S), drive(T, Y, S), truck(T), capacity_opt(Q), place(X), place(Y), step(S)}

% sum of transported goods equals demand and consumption
% weekly nachschub und waren abtransport
