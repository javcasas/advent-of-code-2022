:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).


height('S', 0).
height('E', MAX) :-
  char_code('a', A),
  char_code('z', Z),
  MAX is Z - A.
height(C, H) :-
  char_code('a', A),
  char_code('z', Z),
  char_code(C, V),
  V >= A,
  V =< Z,
  H is V - A.

start_point_row(R, X) :- nth0(X, R, 'S').
start_point([R|_], Y, p(X, Y)) :- start_point_row(R, X).
start_point([_|Mx], CurY, S) :-
  CurY1 is CurY + 1,
  start_point(Mx, CurY1, S).
start_point(M, p(X, Y)) :- start_point(M, 0, p(X, Y)).

end_point_row(R, X) :- nth0(X, R, 'E').
end_point([R|_], Y, p(X, Y)) :- end_point_row(R, X).
end_point([_|Mx], CurY, S) :-
  CurY1 is CurY + 1,
  end_point(Mx, CurY1, S).
end_point(M, p(X, Y)) :- end_point(M, 0, p(X, Y)).

line([]) --> "\n".
line([X|Xs]) --> [X], { X \= '\n' }, line(Xs).
lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

map_to_heightmap([], []).
map_to_heightmap([X|Xs], [R|Rs]) :-
  maplist(height, X, R),
  map_to_heightmap(Xs, Rs).

can_move(H1, H2, true) :-
  H2 =< H1 + 1.
can_move(H1, H2, false) :-
  H2 > H1 + 1.

:- dynamic(height_map/3).

fill_height_map_row([], _, _).
fill_height_map_row([E|Es], X, Y) :-
  assertz(height_map(X, Y, E)),
  X1 is X + 1,
  fill_height_map_row(Es, X1, Y).
fill_height_map([], _).
fill_height_map([R|Rs], Y) :-
  fill_height_map_row(R, 0, Y),
  Y1 is Y + 1,
  fill_height_map(Rs, Y1).
fill_height_map(HM) :- retractall(height_map), fill_height_map(HM, 0).

:- dynamic(distance_map/3).

move(p(X, Y), p(X1, Y1), D, []) :-
  \+ height_map(X1, Y1, H1),
  !.
move(p(X, Y), p(X1, Y1), D, []) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  can_move(H, H1, false),
  !.
move(p(X, Y), p(X1, Y1), D, E) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  can_move(H, H1, true),
  distance_map(X1, Y1, _),
  E = [],
  !.
move(p(X, Y), p(X1, Y1), D, E) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  D1 is D + 1,
  write(distance_map(X1, Y1, D1)),
  write('\n'),
  assertz(distance_map(X1, Y1, D1)),
  E=[p(X1, Y1, D1)].
    
    

% distance_map(HeightMap, PendingNodes, NewPendingNodes) :-
fill_distance_map_([]).
fill_distance_map_([p(X, Y, D)|Ps]) :-
  X1 is X - 1,
  X2 is X + 1,
  Y1 is Y - 1,
  Y2 is Y + 1,
  move(p(X, Y), p(X1, Y), D, E1),
  move(p(X, Y), p(X2, Y), D, E2),
  move(p(X, Y), p(X, Y1), D, E3),
  move(p(X, Y), p(X, Y2), D, E4),
  append(E1, E2, E12),
  append(E12, E3, E123),
  append(E123, E4, E1234),
  append(Ps, E1234, Pss),
  fill_distance_map_(Pss).

fill_distance_map(E) :-
  retractall(distance_map(X, Y, Z)),
  fill_distance_map_(E).
  
  

solution1(S) :-
  phrase_from_file(lines(L), '12.input'),
  start_point(L, p(Sx, Sy)),
  end_point(L, p(Ex, Ey)),
  map_to_heightmap(L, M),
  fill_height_map(M),
  fill_distance_map([p(Sx, Sy, 0)]),
  distance_map(Ex, Ey, S).

:- dynamic(distance_map_2/3).

move2(p(X, Y), p(X1, Y1), D, []) :-
  \+ height_map(X, Y, _),
  !.
move2(p(X, Y), p(X1, Y1), D, []) :-
  \+ height_map(X1, Y1, _),
  !.
move2(p(X, Y), p(X1, Y1), D, []) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  can_move(H, H1, false),
  !.
move2(p(X, Y), p(X1, Y1), D, E) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  can_move(H1, H, true),
  distance_map_2(X, Y, _),
  E = [],
  !.
move2(p(X, Y), p(X1, Y1), D, E) :-
  height_map(X, Y, H),
  height_map(X1, Y1, H1),
  D1 is D + 1,
  write(distance_map_2(X, Y, D1)),
  write('\n'),
  assertz(distance_map_2(X, Y, D1)),
  E=[p(X, Y, D1)].
    
    

% distance_map(HeightMap, PendingNodes, NewPendingNodes) :-
fill_distance_map_2([]).
fill_distance_map_2([p(X, Y, D)|Ps]) :-
  X1 is X - 1,
  X2 is X + 1,
  Y1 is Y - 1,
  Y2 is Y + 1,
  move2(p(X1, Y), p(X, Y), D, E1),
  move2(p(X2, Y), p(X, Y), D, E2),
  move2(p(X, Y1), p(X, Y), D, E3),
  move2(p(X, Y2), p(X, Y), D, E4),
  append(E1, E2, E12),
  append(E12, E3, E123),
  append(E123, E4, E1234),
  append(Ps, E1234, Pss),
  fill_distance_map_2(Pss).

fill_distance_map2(E) :-
  retractall(distance_map_2(X, Y, Z)),
  fill_distance_map_2(E).

solution2(S) :-
  phrase_from_file(lines(L), '12.input'),
  end_point(L, p(Ex, Ey)),
  map_to_heightmap(L, M),
  fill_height_map(M),
  findall(p(X, Y, 0), height_map(X, Y, 0), Initials),
  fill_distance_map(Initials),
  distance_map(Ex, Ey, S).
