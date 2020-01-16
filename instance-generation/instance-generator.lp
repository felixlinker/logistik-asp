% Helper
taut.

#const days=5.
day(1..days).
#show day/1.

#const tu_per_day=8.

#const factory_num=2.
#const warehouse_num=3.
factory(1..factory_num).
warehouse(factory_num + 1..factory_num + warehouse_num).
#show factory/1.
#show warehouse/1.

#const resource_num=5.
#const product_num=2.
resource(1..resource_num).
product(resource_num + 1..resource_num + product_num).
#show resource/1.
#show product/1.

place(X) :- factory(X).
place(X) :- warehouse(X).
good(X) :- resource(X).
good(X) :- product(X).
#show place/1.
#show good/1.

#const min_cost=1.
#const max_cost_frac=2.
:- max_cost_frac < 2.
cost_opt(min_cost..tu_per_day/max_cost_frac).
cost_opt(0).

% cost(from, to, time).
1 { cost(X, Y, C) : cost_opt(C) } 1 :- place(X), place(Y).
#show cost/3.

%%% Cost is a metric %%%
% Cost is symmetric
:- cost(X, Y, C), cost(Y, X, C'), C != C'.
% No cost when not traveling
:- cost(X, X, C), C != 0.
% Only no cost when not traveling
:- cost(X, Y, 0), X != Y.
% Triangle inequality
:- cost(X, Z, C_XZ), cost(X, Y, C_XY), cost(Y, Z, C_YZ), C_XZ > C_XY + C_YZ.

#const min_consumes=1.
#const max_consumes=20.
consumes_opt(min_consumes..max_consumes).
consumes_opt(0).

% consumes(factory, resource, quantity).
1 { consumes(F, R, Q) : consumes_opt(Q) } 1 :- factory(F), resource(R).
#show consumes/3.

% Every factory consumes something
1 <= #sum { Q,R : taut : consumes(F, R, Q) } :- factory(F).
% Every resource is consumed
1 <= #sum { Q,F : taut : consumes(F, R, Q) } :- resource(R).

#const min_produces=1.
#const max_produces=20.
produces_opt(min_produces..max_produces).
produces_opt(0).

% produces(factory, product, quantity).
1 { produces(F, P, Q) : produces_opt(Q) } 1 :- factory(F), product(P).
#show produces/3.

% Every factory produces something
1 <= #sum { Q,P : taut : produces(F, P, Q) } :- factory(F).
% Every product is produced
1 <= #sum { Q,F : taut : produces(F, P, Q) } :- product(P).

%%% Calculate supply needed per resource %%%
supply_needed(R, days * Q_ALL) :- resource(R), Q_ALL = #sum { Q,F : consumes(F, R, Q) }.
supply_opt(0).
supply_opt(Q) :- Q = #max { Q' : supply_needed(_, Q') }.
supply_opt(Q) :- supply_opt(Q + 1), Q > 0.

% supply(warehouse, resource, day, quantity).
1 { supply(W, R, 0, Q) : supply_opt(Q) } 1 :- warehouse(W), resource(R).
#show supply/4.

% Every warehouse supplies something
1 <= #sum { Q,R : taut : supply(W, R, _, Q) } :- warehouse(W).
% Supply per resource matches consumption
S = #sum { Q,W : taut : supply(W, R, _, Q) } :- supply_needed(R, S).

%%% Calculate demand per resource %%%
production(P, days * Q_ALL) :- product(P), Q_ALL = #sum { Q,F : produces(F, P, Q) }.
demand_opt(0).
demand_opt(Q) :- Q = #max { Q' : production(_, Q') }.
demand_opt(Q) :- demand_opt(Q + 1), Q > 0.

% demand(warehouse, product, quantity).
1 { demand(W, P, Q) : demand_opt(Q) } 1 :- warehouse(W), product(P).
#show demand/3.

% Every warehouse sells something
1 <= #sum { Q,P : taut : demand(W, P, Q) } :- warehouse(W).
% Demand per resource matches production
PR = #sum { Q,W : taut : demand(W, P, Q) } :- production(P, PR).

%%% Additional changes %%%
#const min_vol=1.
#const max_vol=3.
vol_opt(min_vol..max_vol).

% vol(good, value).
1 { vol(G, V) : vol_opt(V) } 1 :- good(G).
#show vol/2.


