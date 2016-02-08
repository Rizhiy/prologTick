sideHelper([],L) :- L == 6.
sideHelper([H|T], L) :- (H = 1; H = 0), M is L + 1, side(T,M).
side(S) :- sideHelper(S,0).

piece([A,[T,R,B,L]]) :- string(A), side(T), side(R), side(B), side(L).

% termination rule
rotate(A,0,B) :- !, A = B.
% to rotate by one, just append head to the end of tail
rotate([H|T],N,B) :- append(T,[H],R), M is N - 1,  rotate(R,M,B).
% tests
rotate([1,2,3,4],2,[3,4,1,2]).
rotate([2,2,1,4,5,7],4,[5,7,2,2,1,4]).

% reverse using accumulator
reverseHelp([],L,L).
reverseHelp([H|T],L,Acc) :- reverseHelp(T,L,[H|Acc]).
reverse(A,B) :- reverseHelp(A,B,[]).

xor(A,B) :- (A = 1;A = 0), (B = 1;B = 0), !, \+ (A = B).

xorlist([],[]).
xorlist([0],[0]). % although not true, included to allow corners (check at of the last element).
xorlist([HA|TA],[HB|TB]) :- xor(HA,HB), xorlist(TA,TB).

range(Min,_,Min).
range(Min,Max,Val) :- NewMin is Min + 1, NewMin < Max, range(NewMin,Max,Val).


rotatePiece([PN,PS],N,[PN,RS]) :- rotate(PS,N,RS).

% reverse the lists while moving them to correct position in the list
flipped([PN, [PT,PR,PB,PL]], [PN,[FPT,FPR,FPB,FPL]]) :- reverse(PT,FPT), reverse(PR,FPL), reverse(PB,FPB), reverse(PL,FPR).

orientation(P,0,P).
orientation(P,Or,R) :- Or < 0, flipped(P,IP), NewOr is Or * -1, !, orientation(IP,NewOr,R).
orientation(P,Or,R) :- Or > 0, !, rotatePiece(P,Or,R).

compatible_corner([_,[[P1C|_]|_]], 0, [_,[[P2C|_]|_]], 0, [_,[[P3C|_]|_]], 0) :- !,
  ((P1C = 1, P2C = 0, P3C = 0);(P1C = 0, P2C = 1, P3C = 0);(P1C = 0, P2C = 0, P3C = 1)).
compatible_corner(P1,Side1,P2,Side2,P3,Side3) :- \+ (Side1 < 0), \+ (Side2 < 0), \+ (Side3 < 0),
  orientation(P1,Side1,RotatedP1), orientation(P2, Side2, RotatedP2), orientation(P3,Side3, RotatedP3),
  compatible_corner(RotatedP1,0,RotatedP2,0,RotatedP3,0).

% first make sure that the piece is orientated as such that the sides we want to compare are on the top, if not then rotate accordingly
% to take care of corners I handle first and last elements of the lists separatly.
compatible([_,[[P1S1|P1S]|_]],0,[_,[P2|_]],0) :- !, reverse(P2,[P2S1|P2SR]), xorlist([P1S1],[P2S1]), xorlist(P1S,P2SR).
compatible(P1,Side1,P2,Side2) :- \+ (Side1 < 0), \+ (Side2 < 0), orientation(P2,Side2,RotatedP2),
  orientation(P1,Side1,RotatedP1), compatible(RotatedP1,0,RotatedP2,0).

puzzle([P0|Pieces],[[P0,O0],[P1,O1],[P2,O2],[P3,O3],[P4,O4],[P5,O5]]) :-
  % generate order of pieces to try
  permutation(Pieces,[P1,P2,P3,P4,P5]),
  % generate rotations to try
  O0 = 0, range(-4,4,O1), range(-4,4,O2), range(-4,4,O3), range(-4,4,O4), range(-4,4,O5),
  % rotate pieces
  orientation(P0,O0,OP0), orientation(P1,O1,OP1), orientation(P2,O2,OP2),
  orientation(P3,O3,OP3), orientation(P4,O4,OP4), orientation(P5,O5,OP5),
  % rules for matching sides
  compatible(OP0,2,OP1,0), compatible(OP0,3,OP2,0), compatible(OP1,3,OP2,1), compatible(OP0,1,OP3,0),
  compatible(OP1,1,OP3,3), compatible(OP1,2,OP4,0), compatible(OP2,2,OP4,3), compatible(OP3,2,OP4,3),
  compatible(OP4,2,OP5,0), compatible(OP2,3,OP5,3), compatible(OP0,0,OP5,2), compatible(OP3,1,OP5,1),
  % rules for matching corners
  compatible_corner(OP0,3,OP1,0,OP2,1), compatible_corner(OP0,2,OP1,1,OP3,0), compatible_corner(OP2,2,OP1,3,OP4,0),
  compatible_corner(OP3,3,OP1,2,OP4,1), compatible_corner(OP5,0,OP4,3,OP2,3), compatible_corner(OP5,1,OP4,2,OP3,2),
  compatible_corner(OP5,2,OP0,1,OP3,1), compatible_corner(OP5,3,OP0,0,OP2,0),
  % print out the results
  format("~w at ~w~n", [P0, O0]),
  format("~w at ~w~n", [P1, O1]),
  format("~w at ~w~n", [P2, O2]),
  format("~w at ~w~n", [P3, O3]),
  format("~w at ~w~n", [P4, O4]),
  format("~w at ~w~n", [P5, O5]).
