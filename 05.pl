:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).

isDigit(D) :-
  char_code('0', Zero),
  char_code('9', Nine),
  char_code(D, Dc),
  Dc >= Zero,
  Dc =< Nine.

char_to_number(D, N) :-
  char_code('0', Zero),
  char_code(D, Number),
  N is Number - Zero.

revdigits([]) --> [].
revdigits([D|Ds]) --> [D], {isDigit(D)}, revdigits(Ds).

revdigits_to_number(D, N) :-
  reverse(D, D2),
  digits_to_number(D2, N).

digits_to_number([], 0).
digits_to_number([D|Ds], R) :-
  digits_to_number(Ds, Rs),
  char_to_number(D, Dn),
  R is Rs * 10 + Dn.

number(N) --> revdigits(D), {revdigits_to_number(D, N)}.


cell(empty) --> "   ".
cell(letter(X)) --> "[", [X], "]".

stack([X]) --> cell(X), "\n".
stack([X|Xs]) --> cell(X), " ", stack(Xs).

numberCell(X) --> " ", [X], " ".
numberRow([X]) --> numberCell(X), "\n".
numberRow([X|Xs]) --> numberCell(X), " ", numberRow(Xs).

stacks([], Y) --> numberRow(Y).
stacks([X|Xs], Y) --> stack(X), stacks(Xs, Y).

operation(move(Amount, Start, End)) --> "move ", number(Amount), " from ", number(Start), " to ", number(End).

operations([]) --> "\n".
operations([X|Xs]) --> operation(X), "\n", operations(Xs).

parser(p(X, Y,Z)) --> stacks(X, Y), "\n", operations(Z).

clean_stack([], []).
clean_stack([X|Ss], R) :-
  if_(
    X = empty,
    clean_stack(Ss, R),
    R = [X|Ss]
  ).

take(move(Amount, 1, _), [S|Ss], Taken, [Left|Ss]) :-
  length(Taken, Amount),
  append(Taken, Left, S).
take(move(Amount, X, A), [S|Ss], Taken, [S|Rest]) :-
  X1 is X - 1,
  take(move(Amount, X1, A), Ss, Taken, Rest).

put(move(_, _, 1), Taken, [S|Ss], [Added|Ss]) :-
  append(Taken, S, Added).
put(move(Amount, A, X), Taken, [S|Ss], [S|Rest]) :-
  X1 is X - 1,
  put(move(Amount, A, X1), Taken, Ss, Rest).

apply_operation(O, S, S1) :-
  take(O, S, Taken, S2),
  reverse(Taken, Taken2),
  put(O, Taken2, S2, S1).

apply_operations([], S, S).
apply_operations([O|Os], S, S1) :-
  apply_operation(O, S, S2),
  apply_operations(Os, S2, S1).

apply_operation_part2(O, S, S1) :-
  take(O, S, Taken, S2),
  put(O, Taken, S2, S1).

apply_operations_part2([], S, S).
apply_operations_part2([O|Os], S, S1) :-
  apply_operation_part2(O, S, S2),
  apply_operations_part2(Os, S2, S1).

tops([], []).
tops([[letter(X)|S]|Ss], [X|Xs]) :-
  tops(Ss, Xs).

solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(parser(p(Rows, _, Operations)), '05.input'),
  transpose(Rows, DirtyStacks),
  maplist(clean_stack, DirtyStacks, Stacks),
  apply_operations(Operations, Stacks, OperatedStacks),
  tops(OperatedStacks, Sol1),
  apply_operations_part2(Operations, Stacks, OperatedStacks2),
  tops(OperatedStacks2, Sol2).
