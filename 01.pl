:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

digit(0) --> "0".
digit(1) --> "1".
digit(2) --> "2".
digit(3) --> "3".
digit(4) --> "4".
digit(5) --> "5".
digit(6) --> "6".
digit(7) --> "7".
digit(8) --> "8".
digit(9) --> "9".

digits([X]) --> digit(X), "\n".
digits(X) --> digit(H), digits(Rs), {append([H], Rs, X)}.

number(X) --> digits(D), {parsedNumber(D, X)}.

parsedNumber([], 0).
parsedNumber(Ds, X) :-
  append(H, [D], Ds),
  parsedNumber(H, Xs),
  X is D + Xs * 10.

bunch_of_numbers([]) --> "\n".
bunch_of_numbers([X|R]) -->
  number(X), bunch_of_numbers(R).

bunch_of_bunches([]) --> [].
bunch_of_bunches([X|R]) -->
  bunch_of_numbers(X),
  bunch_of_bunches(R).

% line(X, Chars) --> bunch_of_bunches(X).

max([X], X).
max([X|Xs], R) :-
  max(Xs, Rxs),
  max_inner(X, Rxs, R).

max_inner(A, B, A) :- A >= B.
max_inner(A, B, B) :- B > A.

max_3(X, A, B, C) :-
  sort(X, Sorted),
  reverse(Sorted, [A, B, C|_]).

sum([], 0).
sum([X|Xs], R) :-
  sum(Xs, Rs),
  R is X + Rs.

flatten_1_level([], []).
flatten_1_level([X|Xs], [R|Rs]) :-
  sum(X, R),
  flatten_1_level(Xs, Rs).

solution(P1, P2) :-
  phrase_from_file(bunch_of_bunches(L), '01.input'),
  flatten_1_level(L, Sums),
  max_3(Sums, A, B, C),
  sum([A, B, C], Sol),
  P1 = top_sum(A),
  P2 = top3_sum(Sol).
