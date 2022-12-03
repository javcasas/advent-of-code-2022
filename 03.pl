:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

letter(L) :-
  char_code('A', A),
  char_code('Z', Z),
  char_code(L, Lc),
  Lc >= A,
  Lc =< Z.
letter(L) :-
  char_code('a', A),
  char_code('z', Z),
  char_code(L, Lc),
  Lc >= A,
  Lc =< Z.

rucksack([]) --> "\n".
rucksack([E|Es]) --> [E], { letter(E) }, rucksack(Es).

rucksack(A, B) --> rucksack(E), {append(A, B, E), length(A, T), length(B, T)}.
rucksacks([]) --> "\n".
rucksacks([r(A, B)|Rs]) --> rucksack(A, B), rucksacks(Rs).

common_item(r(A, B), C) :-
  list_to_ord_set(A, SA),
  list_to_ord_set(B, SB),
  ord_intersect(SA, SB, C).

priority(L, P) :-
  atom(L),
  priority([L], P).
priority([L], P) :-
  char_code('a', A),
  char_code('z', Z),
  char_code(L, Lc),
  Lc >= A,
  Lc =< Z,
  P is Lc - A + 1.
priority([L], P) :-
  char_code('A', A),
  char_code('Z', Z),
  char_code(L, Lc),
  Lc >= A,
  Lc =< Z,
  P is Lc - A + 27.

elf_groups([], []).
elf_groups([A,B,C|R], [[A,B,C]|Rx]) :- elf_groups(R, Rx).

badge([r(A1, A2), r(B1, B2), r(C1, C2)], Badge) :-
  append(A1, A2, A),
  append(B1, B2, B),
  append(C1, C2, C),
  list_to_ord_set(A, SA),
  list_to_ord_set(B, SB),
  list_to_ord_set(C, SC),
  ord_intersect(SA, SB, Badge1),
  ord_intersect(SA, SC, Badge2),
  ord_intersect(Badge1, Badge2, Badge).

solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(rucksacks(R), '03.input'),
  maplist(common_item, R, C),
  maplist(priority, C, Priorities),
  sum_list(Priorities, Sol1),
  elf_groups(R, G),
  maplist(badge, G, Badges),
  append(Badges, Badges2),
  maplist(priority, Badges2, Priorities2),
  sum_list(Priorities2, Sol2).
