#const trucks=6.
truck(1..trucks).

#const truck_capacity=20.
capacity_opt(0..truck_capacity).

#const max_steps=15.

#const days=5.
day(1..days).

% predicate truck_loaded(truck, good, quantity, step, day).
% predicate truck_at(truck, place, step, day).

% Trucks are empty at the beginning of each day.
truck_loaded(T, G, 0, 1, D) :- truck(T), good(G), day(D).
% Trucks are parked at some warehouse at the beginning of each day...
1 { truck_at(T, W, 1, D) : warehouse(W) } 1 :- truck(T), day(D).
% ... and must end at the same warehouse where it started.
:- truck_at(T, P_start, 1, D), truck_at(T, P_end, S, D), not drive(T, _, S, D), P_start != P_end.

% predicate drive(truck, to, step, day).
#show drive/4.

% Drive but don't drive around in circles
{ drive(T, P', S, D) : place(P'), P' != P } 1 :- truck_at(T, P, S, D), S <= max_steps.
% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S, D), not drive(T, _, 1, D).
% after driving, truck arrives
truck_at(T, P, S+1, D) :- truck_at(T, P', S, D), drive(T, P, S, D).

% predicate unload(truck, good, quantity, step, day).
#show unload/5.

% When arriving, truck unloads
1 { unload(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not unload too much
:- unload(T, G, QU, S, D), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D), QU > Q + QL.

% do not drop more than is needed:
:- unload(T, G, Q, S, D), demand(P, G, Q'), Q'<Q, warehouse(P), drive(T, P, S, D).
:- unload(T, G, Q, S, D), consumes(P, G, Q'), Q'*days<Q, factory(P), drive(T, P, S, D).
% do not drop what you should pick up:
:- unload(T, G, Q, S, D), factory(P), product(G), Q>0, drive(T, P, S, D).
:- unload(T, G, Q, S, D), warehouse(P), resource(G), Q>0, drive(T, P, S, D).
% drop off whatever products warehouses want
:- Q != #sum{ Q',T,S,D: unload(T, G, Q', S, D), drive(T, P, S, D) }, demand(P, G, Q).%no need to multiply sum with days!!
% drop off whatever resources factories need
:- Q != #sum{ Q',T,S: unload(T, G, Q', S, D), drive(T, P, S, D) }, consumes(P, G, Q), day(D).

% predicate load(truck, good, quantity, step, day).
#show load/5.
% Before departing, truck loads
1 { load(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not load too much
:- truck_capacity < #sum { (Q+QL)*V : vol(G, V), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D) }, truck_at(T, P, S, D).

% do not load more than is available:
:- load(T, G, Q, S, D), supply(P, G, 0, Q'), Q'<Q, warehouse(P), truck_at(T, P, S, D).
:- load(T, G, Q, S, D), produces(P, G, Q'), Q'*days<Q, factory(P), truck_at(T, P, S, D).
% do not load what you should deliver:
:- load(T, G, Q, S, D), warehouse(P), product(G), Q>0, truck_at(T, P, S, D).
:- load(T, G, Q, S, D), factory(P), resource(G), Q>0, truck_at(T, P, S, D).
% pick up whatever resources warehouses offer
:- Q != #sum{ Q',T,S,D: load(T, G, Q', S, D), truck_at(T, P, S, D) }, supply(P, G, 0, Q).%no need to multiply sum with days!!
% pick up whatever products factories make
:- Q != #sum{ Q',T,S: load(T, G, Q', S, D), truck_at(T, P, S, D) }, produces(P, G, Q), day(D).

% after (un)loading, update truck info
truck_loaded(T, G, Q+QL-QU, S+1, D) :- unload(T, G, QU, S, D), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D).

% position of trucks at each step well-defined
:- truck_at(T, P, S, D), truck_at(T, P', S, D), P != P'.
% quantity of certain good in trucks at each step well-defined
:- truck_loaded(T, G, Q, S, D), truck_loaded(T, G, Q', S, D), Q != Q'.

% total expenses from all the driving
#minimize { C,T,S,D,P,P' : cost(P, P', C), truck_at(T, P, S, D), drive(T, P', S, D) }.
