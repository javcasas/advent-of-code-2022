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

number(N) --> revdigits(D), {revdigits_to_number(D, N)}.

revdigits_to_number(D, N) :-
  reverse(D, D2),
  digits_to_number(D2, N).

digits_to_number([], 0).
digits_to_number([D|Ds], R) :-
  digits_to_number(Ds, Rs),
  char_to_number(D, Dn),
  R is Rs * 10 + Dn.

range(A, B, C, D) --> number(A), "-", number(B), ",", number(C), "-", number(D), "\n".

ranges([]) --> "\n".
ranges([r(A, B, C, D)|Rs]) --> range(A, B, C, D), ranges(Rs).

between(A, B, C) :-
  A >= B,
  A =< C.

include_range(r(A, B, C, D), true) :-
  between(A, C, D),
  between(B, C, D).
include_range(r(A, B, C, D), true) :-
  between(C, A, B),
  between(D, A, B).
include_range(X, false) :-
  \+include_range(X, true).

overlap_range(r(A, _, C, D), true) :-
  between(A, C, D).
overlap_range(r(_, B, C, D), true) :-
  between(B, C, D).
overlap_range(r(A, B, C, _), true) :-
  between(C, A, B).
overlap_range(r(A, B, _, D), true) :-
  between(D, A, B).
overlap_range(X, false) :-
  \+overlap_range(X, true).

solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(ranges(R), '04.input'),
  tfilter(include_range, R, O),
  length(O, Sol1),
  tfilter(overlap_range, R, O2),
  length(O2, Sol2).
