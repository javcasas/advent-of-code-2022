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

include_ranges([], []).
include_ranges([R|Rs], O) :-
  include_ranges(Rs, Os),
  (
    include_range(R),
    O = [R|Os],
    !;
    O = Os
  ).
    
between(A, B, C) :-
  A >= B,
  A =< C.

include_range(r(A, B, C, D)) :-
  between(A, C, D),
  between(B, C, D).
include_range(r(A, B, C, D)) :-
  between(C, A, B),
  between(D, A, B).

overlap_ranges([], []).
overlap_ranges([R|Rs], O) :-
  overlap_ranges(Rs, Os),
  (
    overlap_range(R),
    O = [R|Os],
    !;
    O = Os
  ).

overlap_range(r(A, B, C, D)) :-
  between(A, C, D).
overlap_range(r(A, B, C, D)) :-
  between(B, C, D).
overlap_range(r(A, B, C, D)) :-
  between(C, A, B).
overlap_range(r(A, B, C, D)) :-
  between(D, A, B).

solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(ranges(R), '04.input'),
  include_ranges(R, O),
  length(O, Sol1),
  overlap_ranges(R, O2),
  length(O2, Sol2).

