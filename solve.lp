%%% INPUT %%%
% How many trucks are available?
%    ***fixed number of equal trucks (given in task)
#const trucks=6.
truck(1..trucks).
% What is the capacity of each truck?
%    ***reasonable condition to avoid unrealistic solutions (own extension)
#const truck_capacity=5.
capacity_opt(0..truck_capacity).
% How many deliveries can a truck make at max per day?
%    ***reasonable condition to avoid unrealistic solutions (own extension)
#const max_steps=tu_per_day.

%%% IMPORTANT %%%
% It is assumed that the instance program defines the two constants days giving
% the number of days a week has and tu_per_day giving the number of time units
% a day has.

%%% ALL PREDICATES %%%
% predicate truck_loaded(T, G, Q, S, D).
% truck_loaded encodes what has been loaded into truck T after load and unload
% in step S-1 at day D per good G as its quantity Q. It does not include what
% has been loaded into the truck in step S.

% predicate truck_at(T, P, S, D).
#show truck_at/4.
% truck_at encodes the position P of the truck T after driving in step S-1, i.e.
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
% This sequence of actions would be implemented in two steps in this ASP.
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
% Trucks are empty at the beginning of the week.
truck_loaded(T, G, 0, 1, 1) :- truck(T), good(G).
% Trucks are parked at some warehouse at the beginning of the week...
1 { truck_at(T, W, 1, 1) : warehouse(W) } 1 :- truck(T).
% ... and must end at the same warehouse where they started.
:- truck_at(T, P_start, 1, 1), truck_at(T, P_end, S, days), not drive(T, _, S, days), P_start != P_end.
% Trucks start their day where they ended the last one.
truck_at(T, P, 1, D + 1) :- truck_at(T, P, S, D), not drive(T, _, S, D), D < days.
% Trucks keep their load over night
truck_loaded(T, G, Q, 1, D + 1) :- truck_loaded(T, G, Q, S, D), not drive(T, _, S, D), D < days.

%%% DRIVING RULES %%%
% Drive but don't drive around in circles
%    ***avoids multiple equivalent solutions
{ drive(T, P', S, D) : place(P'), P' != P } 1 :- truck_at(T, P, S, D), S <= max_steps.
% clean schedule: if truck is used, drive in step 1 already
%    ***avoids multiple equivalent solutions
:- drive(T, P, S, D), not drive(T, _, 1, D).
% after driving, truck arrives
truck_at(T, P, S+1, D) :- truck_at(T, P', S, D), drive(T, P, S, D).
% A truck must not drive longer than a day is
%    ***reasonable condition to avoid unrealistic solutions (own extension)
:- tu_per_day < #sum { C,S : truck_at(T, P, S, D), drive(T, P', S, D), cost(P, P', C) }, truck(T), day(D).

%%% UNLOADING RULES $$$
% When arriving, truck unloads
1 { unload(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not unload too much
:- unload(T, G, QU, S, D), truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D), QU > Q + QL.

% do not drop what you should pick up:
:- unload(T, G, Q, S, D), 0 < Q, demand(P, G, 0), warehouse(P), drive(T, P, S, D).
:- unload(T, G, Q, S, D), 0 < Q, warehouse(P), drive(T, P, S, D), resource(G).
:- unload(T, G, Q, S, D), 0 < Q, consumes(P, G, 0), factory(P), drive(T, P, S, D).
:- unload(T, G, Q, S, D), 0 < Q, factory(P), drive(T, P, S, D), product(G).
% do not drop off more than is needed (in the course of one week):
% drop off whatever products warehouses want
:- Q != #sum{ Q',T,S,D: unload(T, G, Q', S, D), drive(T, P, S, D) }, demand(P, G, Q).%no need to multiply sum with days!!
% each day, do not drop off less than the factories needed so far
:- Q*D > #sum{ Q',T,S,D': unload(T, G, Q', S, D'), drive(T, P, S, D'), D'<=D }, consumes(P, G, Q), day(D).
%   ***allow for the trucks to unload goods in advance in order to drive more efficiently (own extension)

%%% LOADING RULES %%%
% Before departing, truck loads
1 { load(T, G, Q, S, D) : capacity_opt(Q) } 1 :- good(G), drive(T, P, S, D).
% do not load too much
:- truck_capacity < #sum { Q+QL,G : truck_loaded(T, G, Q, S, D), load(T, G, QL, S, D) }, truck_at(T, P, S, D).
%   ***take into consideration how different goods take up different amount of space per unit (own extension)

% do not load what you should deliver:
:- load(T, G, Q, S, D), 0 < Q, supply(P, G, 0), warehouse(P), truck_at(T, P, S, D).
:- load(T, G, Q, S, D), 0 < Q, warehouse(P), truck_at(T, P, S, D), product(G).
:- load(T, G, Q, S, D), 0 < Q, produces(P, G, 0), factory(P), truck_at(T, P, S, D).
:- load(T, G, Q, S, D), 0 < Q, factory(P), truck_at(T, P, S, D), resource(G).
% do not load more than is available (each week):
% pick up whatever resources warehouses offer
:- Q != #sum{ Q',T,S,D: load(T, G, Q', S, D), truck_at(T, P, S, D) }, supply(P, G, Q).%no need to multiply sum with days!!
% each day, do not pick up more than the factories have made so far
:- Q*D < #sum{ Q',T,S,D': load(T, G, Q', S, D'), truck_at(T, P, S, D'), D'<=D }, produces(P, G, Q), day(D).
%   ***allow for the trucks to load goods collectively later in order to drive more efficiently (own extension)

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
