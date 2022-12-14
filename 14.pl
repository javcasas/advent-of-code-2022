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

coords(c(X, Y)) --> digits(X), ",", digits(Y).
arrow --> " -> ".

line([X]) --> coords(X), "\n".
line([X|Xs]) --> coords(X), arrow, line(Xs).

lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

:- dynamic(rock/2).

fill_rocks_line(c(X, Y), []):-
  assertz(rock(X, Y)).
fill_rocks_line(c(X, Y), [c(X, Y)|Cs]) :-
  fill_rocks_line(c(X, Y), Cs).
fill_rocks_line(c(X1, Y), [c(X2, Y)|Xs]) :-
  X1 > X2,
  assertz(rock(X1, Y)),
  X3 is X1 - 1,
  fill_rocks_line(c(X3, Y), [c(X2, Y)|Xs]).
fill_rocks_line(c(X1, Y), [c(X2, Y)|Xs]) :-
  X1 < X2,
  assertz(rock(X1, Y)),
  X3 is X1 + 1,
  fill_rocks_line(c(X3, Y), [c(X2, Y)|Xs]).
fill_rocks_line(c(X, Y1), [c(X, Y2)|Xs]) :-
  Y1 > Y2,
  assertz(rock(X, Y1)),
  Y3 is Y1 - 1,
  fill_rocks_line(c(X, Y3), [c(X, Y2)|Xs]).
fill_rocks_line(c(X, Y1), [c(X, Y2)|Xs]) :-
  Y1 < Y2,
  assertz(rock(X, Y1)),
  Y3 is Y1 + 1,
  fill_rocks_line(c(X, Y3), [c(X, Y2)|Xs]).

fill_rocks_line([X|Xs]) :- fill_rocks_line(X, Xs).

fill_rocks_([]).
fill_rocks_([X|Xs]) :-
  fill_rocks_line(X),
  fill_rocks_(Xs).

fill_rocks(Lines) :-
  retractall(rock(_, _)),
  fill_rocks_(Lines).

:- dynamic(sand/2).

max_rock_depth(D) :-
  findall(Y, rock(_, Y), S),
  sort(S, S1),
  reverse(S1, [D|_]).

drop_sand(_, Y, MaxDepth, lost) :-
  Y > MaxDepth.

drop_sand(X, Y, MaxDepth, R) :-
  Y =< MaxDepth,
  Y1 is Y + 1,
  \+ rock(X, Y1),
  \+ sand(X, Y1),
  %write(down), write("\n"),
  !,
  drop_sand(X, Y1, MaxDepth, R).

drop_sand(X, Y, MaxDepth, R) :-
  Y =< MaxDepth,
  Y1 is Y + 1,
  XL is X - 1,
  \+ rock(XL, Y1),
  \+ sand(XL, Y1),
  %write(left), write("\n"),
  !,
  drop_sand(XL, Y1, MaxDepth, R).

drop_sand(X, Y, MaxDepth, R) :-
  Y =< MaxDepth,
  Y1 is Y + 1,
  XR is X + 1,
  \+ rock(XR, Y1),
  \+ sand(XR, Y1),
  %write(right), write("\n"),
  !,
  drop_sand(XR, Y1, MaxDepth, R).

drop_sand(X, Y, MaxDepth, stuck) :-
  Y =< MaxDepth,
  assertz(sand(X, Y)).

drop_sand_(MaxDepth) :-
  sand(500, 0),
  !.
drop_sand_(MaxDepth) :-
  write(s),
  drop_sand(500, 0, MaxDepth, S),
  if_(
    S = stuck,
    drop_sand_(MaxDepth),
    true
  ).

drop_sand :-
  retractall(sand(_, _)),
  max_rock_depth(MaxDepth),
  drop_sand_(MaxDepth).

solution1(SandUnits) :-
  phrase_from_file(lines(L), '14.input'),
  fill_rocks(L),
  drop_sand,
  findall([X, Y], sand(X, Y), Sand),
  length(Sand, SandUnits).

solution2(MaxY, SandUnits) :-
  phrase_from_file(lines(L), '14.input'),
  fill_rocks(L),
  findall(Y, rock(X, Y), Ys),
  sort(Ys, SYs), reverse(SYs, [MaxY|_]),
  BedRockY is MaxY + 2,
  fill_rocks([[c(-1000, BedRockY), c(2000, BedRockY)]|L]),
  drop_sand,
  findall([X, Y], sand(X, Y), Sand),
  length(Sand, SandUnits).
