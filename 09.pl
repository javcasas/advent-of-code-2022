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

direction(up) --> "U".
direction(down) --> "D".
direction(left) --> "L".
direction(right) --> "R".

line(move(D, X)) --> direction(D), " ", digits(X), "\n".

lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

coord_differences(t(HX, HY), t(TX, TY), d(Dx, Dy)) :-
  Dx is HX - TX,
  Dy is HY - TY.

clamp(Min, X, Max, X) :-
  X > Min,
  X < Max.
clamp(Min, X, _, Min) :-
  X =< Min.
clamp(_, X, Max, Max) :-
  X >= Max.

move(d(X, Y), d(0, 0)) :-
  X =< 1,
  X >= -1,
  Y =< 1,
  Y >= -1.
move(d(Dx, Dy), d(1, My)) :-
  Dx > 1,
  clamp(-1, Dy, 1, My).
move(d(Dx, Dy), d(-1, My)) :-
  Dx < -1,
  clamp(-1, Dy, 1, My).
move(d(Dx, Dy), d(Mx, 1)) :-
  Dy > 1,
  clamp(-1, Dx, 1, Mx).
move(d(Dx, Dy), d(Mx, -1)) :-
  Dy < -1,
  clamp(-1, Dx, 1, Mx).

move_tail(t(Tx, Ty), d(Dx, Dy), t(NTx, NTy)) :- NTx is Tx + Dx, NTy is Ty + Dy.
  
transition(t(Hx, Hy), left, t(NHx, Hy)) :- NHx is Hx - 1.
transition(t(Hx, Hy), right, t(NHx, Hy)) :- NHx is Hx + 1.
transition(t(Hx, Hy), up, t(Hx, NHy)) :- NHy is Hy - 1.
transition(t(Hx, Hy), down, t(Hx, NHy)) :- NHy is Hy + 1.

full_transition(H, T, D, NH, NT) :-
  transition(H, D, NH),
  drag_tail(NH, T, NT).

transitions(H, T, [], H, T, [T]).
transitions(H, T, [move(Dir, 0)|Moves], RH, RT, V) :- transitions(H, T, Moves, RH, RT, V).
transitions(H, T, [move(Dir, Amount)|Moves], RH, RT, [T|Vs]) :- 
  Amount > 0,
  full_transition(H, T, Dir, NH, NT),
  Amount1 is Amount - 1,
  transitions(NH, NT, [move(Dir, Amount1)|Moves], RH, RT, Vs).

drag_tail(NH, T, NT) :-
  coord_differences(NH, T, Diff),
  move(Diff, Move),
  move_tail(T, Move, NT).

full_transition_10(H, [T1, T2, T3, T4, T5, T6, T7, T8, T9], D, NH, [NT1, NT2, NT3, NT4, NT5, NT6, NT7, NT8, NT9]) :-
  transition(H, D, NH),
  drag_tail(NH, T1, NT1),
  drag_tail(NT1, T2, NT2),
  drag_tail(NT2, T3, NT3),
  drag_tail(NT3, T4, NT4),
  drag_tail(NT4, T5, NT5),
  drag_tail(NT5, T6, NT6),
  drag_tail(NT6, T7, NT7),
  drag_tail(NT7, T8, NT8),
  drag_tail(NT8, T9, NT9).

full_transition_n_(H, [], [], H).
full_transition_n_(H, [T|Ts], [NT|NTs], V) :-
  drag_tail(H, T, NT),
  full_transition_n_(NT, Ts, NTs, V).
full_transition_n([H|R], D, [NH|NR], V) :-
  transition(H, D, NH),
  full_transition_n_(NH, R, NR, V).

last([X], X).
last([_|Xs], R) :- last(Xs, R).

transitions_n(T, [], T, [L]) :- last(T, L).
transitions_n(T, [move(Dir, 0)|Moves], RT, V) :- transitions_n(T, Moves, RT, V).
transitions_n(T, [move(Dir, Amount)|Moves], RT, [L|Vs]) :- 
  Amount > 0,
  full_transition_n(T, Dir, NT, L),
  Amount1 is Amount - 1,
  transitions_n(NT, [move(Dir, Amount1)|Moves], RT, Vs).


solution1(Sol1, H, T) :-
  phrase_from_file(lines(L), '09.input'),
  transitions_n([t(0, 0), t(0, 0)], L, T, Visited),
  list_to_ord_set(Visited, UniqueVisited),
  length(UniqueVisited, Sol1).

initial_tails([t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0)]).
solution2(Sol2, H, T) :-
  phrase_from_file(lines(L), '09.input'),
  Tails = [t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0), t(0, 0)],
  transitions_n(Tails, L, T, Visited),
  list_to_ord_set(Visited, UniqueVisited),
  length(UniqueVisited, Sol2).
