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

cube_coord(X, Y, Z) --> digits(X), ",", digits(Y), ",", digits(Z), "\n".
cube_coords([]) --> "\n".
cube_coords([cube(X,Y,Z)|Xs]) --> cube_coord(X,Y,Z), cube_coords(Xs).

:- dynamic(cube_wall/4).
assert_cube_walls_([]).
assert_cube_walls_([cube(X, Y, Z)|Xs]) :-
  X1 is X + 1,
  Y1 is Y + 1,
  Z1 is Z + 1,
  assertz(cube_wall(x, X, Y ,Z)),
  assertz(cube_wall(y, X, Y, Z)),
  assertz(cube_wall(z, X, Y, Z)),
  assertz(cube_wall(x, X1, Y, Z)),
  assertz(cube_wall(y, X, Y1, Z)),
  assertz(cube_wall(z, X, Y, Z1)),
  assert_cube_walls_(Xs).

assert_cube_walls(Cubes) :-
  retractall(cube_wall(_, _, _, _)),
  assert_cube_walls_(Cubes).

single_cube_wall(cube_wall(A1, B1, C1, D1), T) :-
  findall(cube_wall(A1, B1, C1, D1), cube_wall(A1, B1, C1, D1), Walls),
  length(Walls, WL),
  if_(
    WL = 1,
    T = true,
    T = false
  ).

  

solution1(Sol1) :-
  phrase_from_file(cube_coords(X), '18.input'),
  assert_cube_walls(X),
  findall(cube_wall(T, A, B, C), cube_wall(T, A, B, C), R),
  sort(R, R1),
  tfilter(single_cube_wall, R1, R2),
  length(R2, Sol1).

bounding_box(cube(X, Y, Z), box(X1, X2, Y1, Y2, Z1, Z2)) :-
  X1 is X - 1,
  X2 is X + 1,
  Y1 is Y - 1,
  Y2 is Y + 1,
  Z1 is Z - 1,
  Z2 is Z + 1.

bounding_box([T], Box) :- bounding_box(T, Box).

bounding_box([C | Cubes], box(X1, X2, Y1, Y2, Z1, Z2)) :-
  bounding_box(Cubes, box(X11, X21, Y11, Y21, Z11, Z21)),
  bounding_box(C, box(X12, X22, Y12, Y22, Z12, Z22)), 
  list_min([X11, X12], X1),
  list_max([X21, X22], X2),
  list_min([Y11, Y12], Y1),
  list_max([Y21, Y22], Y2),
  list_min([Z11, Z12], Z1),
  list_max([Z21, Z22], Z2).

:- dynamic(water/3).

in_box(box(X1, X2, Y1, Y2, Z1, Z2), water(X, Y, Z), T) :-
  X >= X1,
  X =< X2,
  Y >= Y1,
  Y =< Y2,
  Z >= Z1,
  Z =< Z2,
  T = true,
  !.
in_box(_, _, false).

:- dynamic(water_wall/4).

relative_water_x(Box, water(X, Y, Z), Waters, Walls) :-
  X1 is X - 1,
  X2 is X + 1,
  (
    (cube_wall(x, X, Y, Z), Waters1 = [], Walls1 = [water_wall(x, X, Y, Z)], !);
    (Waters1 = [water(X1, Y, Z)], Walls1 = [])
  ),
  (
    (cube_wall(x, X2, Y, Z), Waters2 = [], Walls2 = [water_wall(x, X2, Y, Z)], !);
    (Waters2 = [water(X2, Y, Z)], Walls2 = [])
  ),
  append([Waters1, Waters2], Waters3),
  append([Walls1, Walls2], Walls),
  tfilter(in_box(Box), Waters3, Waters).

relative_water_y(Box, water(X, Y, Z), Waters, Walls) :-
  Y1 is Y - 1,
  Y2 is Y + 1,
  (
    (cube_wall(y, X, Y, Z), Waters1 = [], Walls1 = [water_wall(y, X, Y, Z)], !);
    (Waters1 = [water(X, Y1, Z)], Walls1 = [])
  ),
  (
    (cube_wall(y, X, Y2, Z), Waters2 = [], Walls2 = [water_wall(y, X, Y2, Z)],  !);
    (Waters2 = [water(X, Y2, Z)], Walls2 = [])
  ),
  append([Waters1, Waters2], Waters3),
  append([Walls1, Walls2], Walls),
  tfilter(in_box(Box), Waters3, Waters).

relative_water_z(Box, water(X, Y, Z), Waters, Walls) :-
  Z1 is Z - 1,
  Z2 is Z + 1,
  (
    (cube_wall(z, X, Y, Z), Waters1 = [], Walls1 = [water_wall(z, X, Y, Z)],  !);
    (Waters1 = [water(X, Y, Z1)], Walls1 = [])
  ),
  (
    (cube_wall(z, X, Y, Z2), Waters2 = [], Walls2 = [water_wall(z, X, Y, Z2)],  !);
    (Waters2 = [water(X, Y, Z2)], Walls2 = [])
  ),
  append([Waters1, Waters2], Waters3),
  append([Walls1, Walls2], Walls),
  tfilter(in_box(Box), Waters3, Waters).
  
relative_water(Box, W, Waters, Walls) :-
  relative_water_x(Box, W, WX, WallX),
  relative_water_y(Box, W, WY, WallY),
  relative_water_z(Box, W, WZ, WallZ),
  append([WX, WY, WZ], Waters),
  append([WallX, WallY, WallZ], Walls).

assertz_all([]).
assertz_all([X|Xs]) :- ground(X), assertz(X), !, assertz_all(Xs).

assert_water(_, []).
assert_water(Box, [water(X, Y, Z)|Ws]) :-
  water(X, Y, Z),
  !,
  assert_water(Box, Ws).
assert_water(Box, [water(X, Y, Z)|Ws]) :-
  \+ water(X, Y, Z),
  !,
  write('adding '),
  write(water(X, Y, Z)),
  write('\n'),
  assertz(water(X, Y, Z)),
  relative_water(Box, water(X, Y, Z), NewWaters, NewWalls),
  assertz_all(NewWalls),
  append(Ws, NewWaters, Ws2),
  assert_water(Box, Ws2).

cube_wall1(x-1, X, Y, Z) :- cube_wall(x, X, Y, Z).
cube_wall1(x+1, X, Y, Z) :- cube_wall(x, X1, Y, Z), X1 is X + 1.
cube_wall1(y-1, X, Y, Z) :- cube_wall(y, X, Y, Z).
cube_wall1(y+1, X, Y, Z) :- cube_wall(y, X, Y1, Z), Y1 is Y + 1.
cube_wall1(z-1, X, Y, Z) :- cube_wall(z, X, Y, Z).
cube_wall1(z+1, X, Y, Z) :- cube_wall(z, X, Y, Z1), Z1 is Z + 1.

outer_wall(cube_wall(A, X, Y, Z), T) :-
  cube_wall(A, X, Y, Z),
  X1 is X - 1,
  X2 is X + 1,
  Y1 is Y - 1,
  Y2 is Y + 1,
  Z1 is Z - 1,
  Z2 is Z + 1,
  if_(
    (A = x, (water(X1, Y, Z); water(X2, Y, Z))),
    T = true,
    if_(
      (A = y, (water(X, Y1, Z); water(X, Y2, Z))),
      T = true,
      if_(
        (A = z, (water(X, Y, Z1); water(X, Y, Z2))),
        T = true,
        false
      )
    )
  ).

solution2(Sol1) :-
  phrase_from_file(cube_coords(X), '18.input'),
  assert_cube_walls(X),
  bounding_box(X, B),
  assert_water(B, [water(0,0,0)]),
  findall(water_wall(T1, X1, Y1, Z1), water_wall(T1, X1, Y1, Z1), Sols), sort(Sols, Sols1), length(Sols1, Sol1).
