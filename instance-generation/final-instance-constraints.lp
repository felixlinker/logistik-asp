#const consumes_sum=50.
#const produces_sum=50.

:- consumes_sum != #sum { Q,F,R : consumes(F, R, Q) }.
:- produces_sum != #sum { Q,F,R : produces(F, R, Q) }.
