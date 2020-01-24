#const consumes_sum=50.
#const produces_sum=30.

% Have an "interesting" amount of consumption and production
:- consumes_sum != #sum { Q,F,R : consumes(F, R, Q) }.
:- produces_sum != #sum { Q,F,R : produces(F, R, Q) }.

% Each possible cost is given at least once
c(C) :- cost(P1, P2, C).
:- not c(2).
:- not c(3).
:- not c(4).
