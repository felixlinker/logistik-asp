%%% INPUT %%%
% How many trucks are available?
#const trucks=6.
truck(1..trucks).
% What is the capacity of each truck?
#const truck_capacity=20.
capacity_opt(0..truck_capacity).
% How many deliveries can a truck make at max per day?
#const max_steps=15.
% How many days does a week have?
#const days=5.
day(1..days).

%%% ALL PREDICATES %%%
% predicate truck_loaded(T, G, Q, S, D).
% truck_loaded encodes what has been loaded into truck T after load and unload
% in step S-1 at day D per good G as its quantity Q. It does not include what
% has been loaded into the truck in step S.

% predicate truck_at(T, P, S, D).
% truck_at encodes the position P of the truck T after driving in step-1, i.e.
% at the beginning of step S at day D.

% predicate drive(T, P, S, D).
#show drive/4.
% drive encodes where truck T drives to (P) in step S at day D, i.e. at the end
% of step S, T ends in P.

% predicate unload(T, G, Q, S, D).
#show unload/5.
% unload encodes how much (Q) of good G is unloaded from truck T at the position
% the truck has driven to (given by drive) in step S at day D.

% predicate load(truck, good, quantity, step, day).
#show load/5.
% load encodes how much (Q) of good G is loaded into truck T at the position the
% truck has started at (given by truck_at) in step S at day D.

%%% EXAMPLE %%%
% The places at which a truck might load/unload may seem coutnerintuitive.
% Consider the following example for a fixed truck T and day D:
%
% * Truck starts at warehouse W and loads Q_R of resource R.
% | ... truck drives to factory F ...
% * Truck arrives at factory F, unloads the resources and loads Q_P of product P.
% | ... truck drives back to warehouse W ...
% * Truck unloads the products.
%
% This would sequence of actions would be implemented in two steps in this ASP.
% At each step, a truck may decide to drive somewhere. If it does, it has to
% load all goods (may load nothing by loading an amount of zero) where it starts
% (given by truck_at) and has to unload all goods where it ends (may unload
% nothing by unloading an amount of zero) where it ends (given by drive). The
% steps would look like this:
% First step load(T, R, Q_R, 1, D), drive(T, W, 1, D), unload(T, R, Q_R, 1, D)
% Second step: load(T, P, Q_P, 2, D), drive(T, F, 2, D), unload(T, P, Q_P, 1, D)
%
% The hard thing to get here is that truck_at encodes the starting location for
% each of these steps. If you want to find out where a truck unloaded something,
% you have to look at the place in drive.

%%% INITIAL STATE %%%
% Trucks are empty at the beginning of each day.
truck_loaded(T, G, 0, 1, D) :- truck(T), good(G), day(D).
% Trucks are parked at some warehouse at the beginning of the week...
1 { truck_at(T, W, 1, 1) : warehouse(W) } 1 :- truck(T).
% ... and must end at the same warehouse where it started.
:- truck_at(T, P_start, 1, 1), truck_at(T, P_end, S, days), not drive(T, _, S, days), P_start != P_end.
% Trucks start their day where they ended the last one.
truck_at(T, P, 1, D + 1) :- truck_at(T, P, S, D), not drive(T, _, S, D), D < days.

%%% DRIVING RULES %%%
% Drive but don't drive around in circles
{ drive(T, P', S, D) : place(P'), P' != P } 1 :- truck_at(T, P, S, D), S <= max_steps.
% clean schedule: if truck is used, drive in step 1 already
:- drive(T, P, S, D), not drive(T, _, 1, D).
% after driving, truck arrives
truck_at(T, P, S+1, D) :- truck_at(T, P', S, D), drive(T, P, S, D).

%%% UNLOADING RULES $$$
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

%%% LOADING RULES %%%
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

%%% GENERAL CONSISTENCY %%%
% position of trucks at each step well-defined
:- truck_at(T, P, S, D), truck_at(T, P', S, D), P != P'.
% quantity of certain good in trucks at each step well-defined
:- truck_loaded(T, G, Q, S, D), truck_loaded(T, G, Q', S, D), Q != Q'.

%%% OPTIMIZATION %%%
% total expenses from all the driving
#minimize { C,T,S,D,P,P' : cost(P, P', C), truck_at(T, P, S, D), drive(T, P', S, D) }.
