#const trucks=6.
truck(1..trucks).

#const truck_capacity=20.
capacity_opt(0..truck_capacity).

#const max_steps=15.
step(1..max_steps).

#const days=5.
day(1..days).

% predicate truck_info(truck, good, quantity, place, step, day).

% trucks are empty each morning and are parked at first factory (=place nr. 1).
truck_info(T, G, 0, 1, 1, D) :- truck(T), good(G), day(D).

% additional predicates last_drivestep(T, S, D) and notlast_drivestep(T, S, D) for obtaining truck_info at last step (!=max_steps)
notlast_drivestep(T, S, D) :- after(T, S, S', D).
last_drivestep(T, S, D) :- not notlast_drivestep(T, S, D), drive(T, P, S, D).
% ... and must end at the same factory
:- truck_info(T, G, Q, P, S+2, D), last_drivestep(T, S, D), P!=1.

% capacity constraint for trucks
:- truck_capacity < #sum { Q*V : vol(G, V), truck_info(T, G, Q, P, S, D) }, truck(T), place(P), step(S), day(D).

% position of trucks at each step well-defined
:- truck_info(T, G, Q, P, S, D), truck_info(T, G', Q', P', S, D), P != P'.

% quantity of certain good in trucks at each step well-defined
:- truck_info(T, G, Q, P, S, D), truck_info(T, G, Q', P', S, D), Q != Q'.

% predicate drive(truck, to, step)
{ drive(T, P, S, D) : place(P) } 1 :- truck(T), step(S), day(D).
% don't drive around in circles; otherwise useless extensions since cost=0
:- truck_info(T, G, Q, P, S, D), drive(T, P, S, D).

% clean schedule: did we drive in step 1?
drives1(T, D) :- drive(T, P, 1, D).
% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S, D), not drives1(T, D).
% clean schedule: need time to load and unload, see below (distance between S and S' should be at least 3)
:- drive(T, P, S, D), drive(T, P', S', D), S>S', S-S'<3.
% clean schedule: additional predicate after(T, S, S', D) to help ensure that we drive every third step
after(T, S, S', D) :- drive(T, P, S, D), drive(T, P', S', D), S<S'.
% clean schedule: if we keep driving, we must drive after precisely 3 steps
drives_3_steps_later(T, S, D) :- drive(T, P, S, D), drive(T, P', S+3, D).
:- drive(T, P, S, D), after(T, S, S', D), not drives_3_steps_later(T, S, D).
%maybe instead:    :- after(T, S, S', D), not drives_3_steps_later(T, S, D).

% after driving, truck arrives
truck_info(T, G, Q, P, S+1, D) :- truck_info(T, G, Q, P', S, D), drive(T, P, S, D).

% predicate unload(truck, good, quantity, step, day).
% after arriving, truck unloads
1 { unload(T, G, Q, S+1, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not unload too much
:- unload(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), Q > Q'.
% after unloading, update truck info
truck_info(T, G, Q'-Q, P, S+1, D) :- unload(T, G, Q, S, D), truck_info(T, G, Q', P, S, D).

% predicate load(truck, good, quantity, step).
1 { load(T, G, Q, S+1, D) : capacity_opt(Q) } 1 :- unload(T, G, Q', S, D).
% cannot load too much since capacity constrained above
% after loading, update truck info
truck_info(T, G, Q'+Q, P, S+1, D) :- load(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), step(S+1).

% total expenses from all the driving
%#minimize { C : cost(X, Y, C), truck_info(T, 1, Q, X, S), drive(T, Y, S) }.

% transport the correct amount
% drop off whatever products warehouses want
:- Q != #sum{ Q',T,S,D: unload(T, G, Q', S, D), truck_info(T, G, Q'', P, S, D) }, demand(P, G, Q).%no need to multiply sum with days!!
% pick up whatever resources warehouses offer
:- Q != #sum{ Q',T,S,D: load(T, G, Q', S, D), truck_info(T, G, Q'', P, S, D) }, supply(P, G, 0, Q).%no need to multiply sum with days!!
% pick up whatever products factories make
:- Q != #sum{ Q',T,S: load(T, G, Q', S, D), truck_info(T, G, Q'', P, S, D) }, produces(P, G, Q), day(D).
% drop off whatever resources factories need
:- Q != #sum{ Q',T,S: unload(T, G, Q', S, D), truck_info(T, G, Q'', P, S, D) }, consumes(P, G, Q), day(D).

% do not load what you should deliver---additional predicates needed!:
:- load(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), warehouse(P), product(G), Q>0.
:- load(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), factory(P), resource(G), Q>0.
% do not drop what you should pick up:
:- unload(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), factory(P), product(G), Q>0.
:- unload(T, G, Q, S, D), truck_info(T, G, Q', P, S, D), warehouse(P), resource(G), Q>0.

% weekly nachschub und waren abtransport
