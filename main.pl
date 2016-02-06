sideHelper([],L) :- L == 6.
sideHelper([H|T], L) :- (H = 1; H = 0), M is L + 1, side(T,M).
side(S) :- sideHelper(S,0).

piece([A,[T,R,B,L]]) :- string(A), side(T), side(R), side(B), side(L).

% termination rule
rotate(A,0,B) :- !, A = B.
% to rotate by one, just append head to the end of tail
rotate([H|T],N,B) :- append(T,[H],R), M is N - 1,  rotate(R,M,B).

% reverse using accumulator
reverseHelp([],L,L).
reverseHelp([H|T],L,Acc) :- reverseHelp(T,L,[H|Acc]).
reverse(A,B) :- reverseHelp(A,B,[]).

xor(A,B) :- (A = 1;A = 0), (B = 1;B = 0), \+ (A = B).

xorlist([],[]).
xorlist([HA|TA],[HB|TB]) :- xor(HA,HB), xorlist(TA,TB).

range(Min,_,Min).
range(Min,Max,Val) :- NewMin is Min + 1, NewMin < Max, range(NewMin,Max,Val).
