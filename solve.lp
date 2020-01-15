#const trucks=6.
truck(1..trucks).

#const truck_capacity=20.
capacity_opt(0..truck_capacity).

#const max_steps=15.

#const days=5.
day(1..days).

% predicate truck_loaded(truck, good, quantity, step, day).

% trucks are empty at the beginning of the week and are parked at first factory (=place nr. 1).
truck_loaded(T, G, 0, 1, D) :- truck(T), good(G), day(D).
truck_at(T, 1, 1, D) :- truck(T), day(D).

% ... and must end at the same factory
:- truck_at(T, P, S, D), not drive(T, _, S, D), P != 1.

% capacity constraint for trucks
:- truck_capacity < #sum { (Q+QL)*V : vol(G, V), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D) }, truck_at(T, P, S, D).

% position of trucks at each step well-defined
:- truck_at(T, P, S, D), truck_at(T, P', S, D), P != P'.

% quantity of certain good in trucks at each step well-defined
:- truck_loaded(T, G, Q, S, D), truck_loaded(T, G, Q', S, D), Q != Q'.

% predicate drive(truck, to, step)
% Drive but don't drive around in circles
{ drive(T, P', S, D) : place(P'), P' != P } 1 :- truck_at(T, P, S, D), S <= max_steps.

% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S, D), not drive(T, _, 1, D).

% after driving, truck arrives
truck_at(T, P, S+1, D) :- truck_at(T, P', S, D), drive(T, P, S, D).

% predicate unload(truck, good, quantity, step, day).
% When arriving, truck unloads
1 { unload(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not unload too much
:- unload(T, G, QU, S, D), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D), QU > Q + QL.

% predicate load(truck, good, quantity, step).
% Before departing, truck loads
1 { load(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% cannot load too much since capacity constrained above

% after (un)loading, update truck info
truck_loaded(T, G, Q+QL-QU, S+1, D) :- unload(T, G, QU, S, D), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D).

% total expenses from all the driving
%#minimize { C : cost(X, Y, C), truck_loaded(T, 1, Q, X, S), drive(T, Y, S) }.

% transport the correct amount
% drop off whatever products warehouses want
:- Q != #sum{ Q',T,S,D: unload(T, G, Q', S, D), drive(T, P, S, D) }, demand(P, G, Q).%no need to multiply sum with days!!
% pick up whatever resources warehouses offer
:- Q != #sum{ Q',T,S,D: load(T, G, Q', S, D), truck_at(T, P, S, D) }, supply(P, G, 0, Q).%no need to multiply sum with days!!
% pick up whatever products factories make
:- Q != #sum{ Q',T,S: load(T, G, Q', S, D), truck_at(T, P, S, D) }, produces(P, G, Q), day(D).
% drop off whatever resources factories need
:- Q != #sum{ Q',T,S: unload(T, G, Q', S, D), drive(T, P, S, D) }, consumes(P, G, Q), day(D).

% do not load what you should deliver---additional predicates needed!:
:- load(T, G, Q, S, D), truck_at(T, P, S, D), warehouse(P), product(G), Q>0.
:- load(T, G, Q, S, D), truck_at(T, P, S, D), factory(P), resource(G), Q>0.
% do not drop what you should pick up:
:- unload(T, G, Q, S, D), drive(T, P, S, D), factory(P), product(G), Q>0.
:- unload(T, G, Q, S, D), drive(T, P, S, D), warehouse(P), resource(G), Q>0.

% weekly nachschub und waren abtransport
