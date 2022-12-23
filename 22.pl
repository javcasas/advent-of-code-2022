:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).

digit('0') --> "0".
digit('1') --> "1".
digit('2') --> "2".
digit('3') --> "3".
digit('4') --> "4".
digit('5') --> "5".
digit('6') --> "6".
digit('7') --> "7".
digit('8') --> "8".
digit('9') --> "9".
digits_([X]) --> digit(X).
digits_([X1|Xs]) --> digit(X1), digits_(Xs).
digits(X) --> digits_(Xs), {number_chars(X, Xs)}.
digits(X) --> "-", digits_(Xs), {number_chars(X1, Xs), X is -X1}.

tile(open) --> ".".
tile(wall) --> "#".
tile(empty) --> " ".

tile_line([X]) --> tile(X), "\n".
tile_line([X|Xs]) --> tile(X), tile_line(Xs).
parse_map([X]) --> tile_line(X).
parse_map([X|Xs]) --> tile_line(X), parse_map(Xs).

forward(X) --> digits(X).
turn(left) --> "L".
turn(right) --> "R".

parse_operations([forward(X)]) --> forward(X), "\n".
parse_operations([forward(X), turn(Y)|Xs]) --> forward(X), turn(Y), parse_operations(Xs).

parse_input(input(Map, Operations)) --> parse_map(Map), "\n", parse_operations(Operations), "\n".

:- dynamic(map/3).
fill_map_row(_, _, []).
fill_map_row(RowIndex, ColIndex, [Tile|Tiles]) :-
  if_(
    Tile = empty,
    true,
    assertz(map(ColIndex, RowIndex, Tile))
  ),
  ColIndex1 is ColIndex + 1,
  fill_map_row(RowIndex, ColIndex1, Tiles).
fill_map(_, []).
fill_map(RowIndex, [Row|Rows]) :-
  fill_map_row(RowIndex, 0, Row),
  RowIndex1 is RowIndex + 1,
  fill_map(RowIndex1, Rows).
fill_map(Map) :-
  retractall(map(_, _, _)),
  fill_map(0, Map).

direction_number(right, 0).
direction_number(down, 1).
direction_number(left, 2).
direction_number(up, 3).
turn_left(right, up).
turn_left(down, right).
turn_left(left, down).
turn_left(up, left).
turn_right(O, O1) :- turn_left(O1, O).
direction_to_coord_inc(right, 1, 0).
direction_to_coord_inc(down, 0, 1).
direction_to_coord_inc(left, -1, 0).
direction_to_coord_inc(up, 0, -1).

wraparound_move(X, Y, D, X2, Y2) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  map(X2, Y2, open).

wraparound_move(X, Y, D, X, Y) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  map(X2, Y2, wall).

wraparound_move(X, Y, D, X1, Y1) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  \+ map(X2, Y2, _),
  wraparound(X, Y, D, X3, Y3),
  map(X3, Y3, Tile),
  if_(
    Tile = open,
    (X1 = X3, Y1 = Y3),
    (X1 = X, Y1 = Y)
  ).

wraparound(X, Y, left, X1, Y) :-
  findall(X2, map(X2, Y, _), Xs),
  list_max(Xs, X1).
wraparound(X, Y, right, X1, Y) :-
  findall(X2, map(X2, Y, _), Xs),
  list_min(Xs, X1).
wraparound(X, Y, down, X, Y1) :-
  findall(Y2, map(X, Y2, _), Ys),
  list_min(Ys, Y1).
wraparound(X, Y, up, X, Y1) :-
  findall(Y2, map(X, Y2, _), Ys),
  list_max(Ys, Y1).

operation(state(X, Y, D), forward(0), state(X, Y, D)).
operation(state(X, Y, D), forward(F), state(X1, Y1, D)) :-
  F > 0,
  F1 is F - 1,
  wraparound_move(X, Y, D, X2, Y2),
  if_(
    (X2 = X, Y2 = Y),
    (X1 = X2, Y1 = Y2),
    operation(state(X2, Y2, D), forward(F1), state(X1, Y1, D))
  ).
operation(state(X, Y, D), turn(left), state(X, Y, D1)) :- turn_left(D, D1).
operation(state(X, Y, D), turn(right), state(X, Y, D1)) :- turn_right(D, D1).

operations(S, [], S).
operations(S, [O|Os], S1) :-
  write(.),
  operation(S, O, S2),
  operations(S2, Os, S1).

opening_tile(X, Y) :-
  findall(Y1, map(_, Y1, _), Ys),
  list_min(Ys, Y),
  findall(X1, map(X1, Y, _), Xs),
  list_min(Xs, X).

initial_state(state(X, Y, right)) :-
  opening_tile(X, Y).

password(state(X, Y, D), P) :-
  S1 is (Y + 1) * 1000,
  S2 is (X + 1) * 4,
  direction_number(D, S3),
  P is S1 + S2 + S3.

solution1(Sol1) :-
  phrase_from_file(parse_input(I), '22.input'),
  input(Map, Operations) = I,
  fill_map(Map),
  initial_state(S),
  operations(S, Operations, S1),
  password(S1, Sol1).

wraparound_cube_move(state(X, Y, D), state(X, Y, D)) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  map(X2, Y2, wall).

wraparound_cube_move(state(X, Y, D), state(X2, Y2, D)) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  map(X2, Y2, open).

connected(edge(2, 1, down), edge(2, 1, left), normal).
connected(edge(3, 0, right), edge(2, 2, left), reverse).
connected(edge(1, 3, down), edge(1, 3, right), normal).
connected(edge(0, 4, down), edge(2, 0, up), normal).
connected(edge(0, 3, left), edge(1, 0, up), normal).
connected(edge(0, 2, left), edge(1, 0, left), reverse).
connected(edge(1, 1, left), edge(0, 2, up), normal).

edge_length(edge(Ex, Ey, down), X, Y, Dist) :-
  Y #= Ey * 50,
  MinX #= Ex * 50,
  MaxX #= (Ex + 1) * 50 - 1,
  X #>= MinX,
  X #=< MaxX,
  Dist #= X - MinX.

edge_length(edge(Ex, Ey, up), X, Y, Dist) :-
  Y #= (Ey * 50) - 1,
  MinX #= Ex * 50,
  MaxX #= (Ex + 1) * 50 - 1,
  X #>= MinX,
  X #=< MaxX,
  Dist #= X - MinX.

edge_length(edge(Ex, Ey, right), X, Y, Dist) :-
  X #= Ex * 50,
  MinY #= Ey * 50,
  MaxY #= (Ey + 1) * 50 - 1,
  Y #>= MinY,
  Y #=< MaxY,
  Dist #= Y - MinY.

edge_length(edge(Ex, Ey, left), X, Y, Dist) :-
  X #= Ex * 50 - 1,
  MinY #= Ey * 50,
  MaxY #= (Ey + 1) * 50 - 1,
  Y #>= MinY,
  Y #=< MaxY,
  Dist #= Y - MinY.

connected_(state(X1, Y1, D1), state(X2, Y2, D2)) :-
  connected(edge(Ex, Ey, D1), edge(E2x, E2y, D2), Rev),
  edge_length(edge(Ex, Ey, D1), X1, Y1, Dist),
  if_(
    Rev = normal,
    Dist1 = Dist,
    Dist1 #= - Dist
  ),
  edge_length(edge(E2x, E2y, D2), X2, Y2, Dist1).


%connected_(state(X1, Y1, D1), state(X2, Y2, D2)) :-
%  connected(edge(Ex, Ey, D1), edge(E2x, E2y, D2)),


  %wraparound_cube

wraparound_cube1(state(X, 50, down), state(99, Y2, left)) :-
  % edge below 2
  X #> 100,
  X #< 150,
  Y2 #= X - 100 + 50.

wraparound_cube1(state(X, 150, down), state(99, Y2, left)) :-
  % edge below 4
  X #> 50,
  X #< 100,
  Y2 #= 150 + (X - 50).

wraparound_cube1(state(150, Y, right), state(99, Y2, left)) :-
  % edge right of 2
  Y #> 0,
  Y #< 50,
  Y2 #= 150 - Y.

wraparound_cube(S, S1) :- wraparound_cube1(S, S1).
wraparound_cube(state(X, Y, D), state(X1, Y1, D1)) :-
  turn_180(D, D_2),
  turn_180(D1, D1_2),
  wraparound_cube1(state(X1, Y1, D1_2), state(X, Y, D_2)).

turn_180(D, D1) :-
  turn_left(D, D2),
  turn_left(D2, D1).

wraparound_cube_move(state(X, Y, D), state(X2, Y2, D2)) :-
  direction_to_coord_inc(D, Dx, Dy),
  X2 is X + Dx,
  Y2 is Y + Dy,
  \+ map(X2, Y2, open).

  %  X in [101, 150], Y = 50, D = down -> X = 100, Y = 50 + (X - 100), D -> left
  %X = 100, Y = [50, 100], D = right -> X = [101, 150], Y = 50, D = up

  %X = 100, Y = [101, 150], D = right -> X = 150, Y = [50, 0], D = left
  %X = 150, Y = [50, 0], D = right -> X = 100, Y = [101, 150], D = left

operation2(state(X, Y, D), forward(0), state(X, Y, D)).
operation2(state(X, Y, D), forward(F), state(X1, Y1, D)) :-
  F > 0,
  F1 is F - 1,
  wraparound_move(X, Y, D, X2, Y2),
  if_(
    (X2 = X, Y2 = Y),
    (X1 = X2, Y1 = Y2),
    operation2(state(X2, Y2, D), forward(F1), state(X1, Y1, D))
  ).
operation2(state(X, Y, D), turn(left), state(X, Y, D1)) :- turn_left(D, D1).
operation2(state(X, Y, D), turn(right), state(X, Y, D1)) :- turn_right(D, D1).

operations2(S, [], S).
operations2(S, [O|Os], S1) :-
  write(.),
  operation2(S, O, S2),
  operations2(S2, Os, S1).


solution2(Sol2) :-
  phrase_from_file(parse_input(I), '22.input'),
  input(Map, Operations) = I,
  fill_map(Map),
  initial_state(S),
  operations2(S, Operations, S1),
  password(S1, Sol1).
