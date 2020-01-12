truck(1..6).

#const truck_capacity=20.
capacity_opt(0..truck_capacity).

#const max_steps=15.
step(1..max_steps).

% predicate truck_info(truck, good, quantity, place, step).

% trucks are empty each morning and are parked at first factory (=place nr. 1).
truck_info(T, G, 0, 1, 1) :- truck(T), good(G).

% % Trucks start at some random factory
% 1 { truck_info(T, G, 0, F, 1) } 1 :- truck(T), good(G), factory(F).
% % additional predicates last_drivestep(T, S) and notlast_drivestep(T, S) for obtaining truck_info at last step (!=max_steps)
% notlast_drivestep(T, S) :- step(S), step(S'), after(T, S, S').

% last_drivestep(T, S) :- not notlast_drivestep(T, S), drive(T, P, S).
% % ... and must end at the same factory
% :- truck_info(T, G, 0, F, 1), truck_info(T, G, 0, F', S+2), last_drivestep(T, S), F != F'.

% capacity constraint for trucks
:- truck_capacity < #sum { Q*V : vol(G, V), truck_info(T, G, Q, P, S) }, truck(T), place(P), step(S).

% position of trucks at each step well-defined
:- truck_info(T, G, Q, P, S), truck_info(T, G', Q', P', S), P != P'.

% quantity of certain good in trucks at each step well-defined
:- truck_info(T, G, Q, P, S), truck_info(T, G, Q', P', S), Q != Q'.

% predicate drive(truck, to, step) (************hilfe garantiert nicht richtig geraten)
{ drive(T, P, S) : place(P) } 1 :- truck(T), step(S).
% don't drive around in circles; otherwise useless extensions since cost=0
:- truck_info(T, G, Q, P, S), drive(T, P, S).

% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S), not drive(T, P', 1), place(P').
% clean schedule: need time to load and unload, see below (distance between S and S' should be at least 3)
:- drive(T, P, S), drive(T, P', S'), (S-S')*(S'-S)>-6.
% clean schedule: additional predicate after(T, S, S') to help ensure that we drive every third step
after(T, S, S') :- drive(T, P, S), drive(T, P', S'), S<S'.
% clean schedule: if we keep driving, we must drive after precisely 3 steps
:- drive(T, P, S), after(T, S, S'), not drive(T, P, S+3).

% after driving, truck arrives
truck_info(T, G, Q, P, S+1) :- truck_info(T, G, Q, P', S), drive(T, P, S).

% predicate unload(truck, good, quantity, place, step).
% after arriving, truck unloads
1 { unload(T, G, Q, P, S+1) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S).
% do not unload too much
:- unload(T, G, Q, P, S), truck_info(T, G, Q', P, S), Q > Q'.
% after unloading, update truck info
truck_info(T, G, Q', P, S+2) :- unload(T, G, Q, P, S+1), capacity_opt(Q').

% predicate load(truck, good, quantity, place, step).
1 { load(T, G, Q, P, S+1) : capacity_opt(Q) } 1 :- unload(T, G, Q, P, S).
% cannot load too much since capacity constrained above

% total expenses from all the driving
#minimize { C : cost(X, Y, C), truck_info(T, 1, Q, X, S), drive(T, Y, S) }.

% sum of transported goods equals demand and consumption
% weekly nachschub und waren abtransport
