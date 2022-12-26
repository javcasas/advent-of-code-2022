:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).
:- use_module(library(files)).

% File parser
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

file_entry(e(X)) --> digits(X), "\n".
file_entries([]) --> "\n".
file_entries([X|Xs]) --> file_entry(X), file_entries(Xs).

% Zipper operations
% A list zipper is a list optimized for selecting (o(N)) and updating elements (o(1)) efficiently
% zipper(ReversedHead, CurrentElement, Tail) represents a list like [reverse ReversedHead | [CurrentElement] | Tail]
list_to_zipper([X|Xs], zipper([], X, Xs)).

move_view(zipper(P, X, [Xs1|Xs]), right, zipper([X|P], Xs1, Xs)).
move_view(zipper(P, X, []), right, Z1) :-
  reverse([X|P], L2),
  list_to_zipper(L2, Z1).
move_view(zipper([P|Ps], X, Xs), left, zipper(Ps, P, [X|Xs])).
move_view(zipper([], X, Xs), left, Z) :-
  reverse([X|Xs], [X1|L]),
  Z = zipper(L, X1, []).

move_view(Z, left(0), Z).
move_view(Z, right(0), Z).
move_view(Z, left(X), Z1) :-
  X > 0,
  X1 is X - 1,
  move_view(Z, left, Z2),
  move_view(Z2, left(X1), Z1).
move_view(Z, right(X), Z1) :-
  X > 0,
  X1 is X - 1,
  move_view(Z, right, Z2),
  move_view(Z2, right(X1), Z1).

replace_item(zipper(A, _, C), B1, zipper(A, B1, C)).

get_item(zipper(_, I, _), I).

drag_item(zipper(A, B, [C|C1]), right, zipper([C|A], B, C1)).
drag_item(zipper(A, B, []), right, Z) :-
  reverse(A, AR),
  Z1 = zipper([], B, AR),
  drag_item(Z1, right, Z).
drag_item(zipper([A], B, C), left, zipper(CR, B, [])) :-
  reverse([A|C], CR), !.
drag_item(zipper([A|As], B, C), left, zipper(As, B, [A|C])) :- !.
drag_item(zipper([], B, C), left, Z) :-
  reverse(C, CR),
  Z1 = zipper(CR, B, []),
  drag_item(Z1, left, Z).
drag_item(Z, left(0), Z).
drag_item(Z, left(X), Z1) :-
  X > 0,
  X1 is X - 1,
  drag_item(Z, left, Z2),
  drag_item(Z2, left(X1), Z1).
drag_item(Z, right(0), Z).
drag_item(Z, right(X), Z1) :-
  X > 0,
  X1 is X - 1,
  drag_item(Z, right, Z2),
  drag_item(Z2, right(X1), Z1).

go_to_start(Z, Z1) :-
  zipper_to_list(Z, L),
  list_to_zipper(L, Z1).

at_end(zipper(_, _, []), true).
at_end(zipper(_, _, [_|_]), false).

zipper_to_list(zipper(P, X, Xs), L) :-
  reverse(P, P1),
  append([P1, [X], Xs], L).

% Solution for part 1
% Encrypted (unprocessed) items are marked as e(Element)
% Decrypted (processed) items are marked as d(Element)

process(Z, Z) :- at_end(Z, true), get_item(Z, d(_)), !.
process(Z, Z1) :-
  at_end(Z, false),
  \+ get_item(Z, e(_)),
  !,
  move_view(Z, right, Z2),
  process(Z2, Z1).
process(Z, Z4) :-
  get_item(Z, e(V)),
  !,
  Steps is abs(V),
  (
    (V >= 0, Direction = right(Steps));
    (V < 0, Direction = left(Steps))
  ),
  replace_item(Z, d(V), Z1),
  drag_item(Z1, Direction, Z2),
  go_to_start(Z2, Z3),
  write(V), nl,
  process(Z3, Z4).
  
find_zero_(Z, Z) :- get_item(Z, d(0)).
find_zero_(Z, Z1) :- \+ get_item(Z, d(0)), move_view(Z, right, Z2), find_zero_(Z2, Z1).
  

find_zero(Z, Z1) :-
  go_to_start(Z, Z2),
  find_zero_(Z2, Z1).

solution1(Sol1) :-
  phrase_from_file(file_entries(X), '20.input'),
  list_to_zipper(X, Z),
  process(Z, Z1),
  find_zero(Z1, Z2),
  move_view(Z2, right(1000), Z3),
  get_item(Z3, d(I1000)),
  move_view(Z3, right(1000), Z4),
  get_item(Z4, d(I2000)),
  move_view(Z4, right(1000), Z5),
  get_item(Z5, d(I3000)),
  Sol1 is I1000 + I2000 + I3000.

% Testsuite for the zipper operations
assert(Name, X, X) :- !, write(Name), write(' '), write('PASS'), nl.
assert(Name, X, Y) :- X \= Y, !, write(Name), write(' '), write('FAIL'), write(' '), write(X), write(' '), write(Y), nl.

test :-
  list_to_zipper([1, 2, -3, 3, -2, 0, 4], Z1),
  drag_item(Z1, right(1), Z3),
  zipper_to_list(Z3, L),
  assert('1', L, [2, 1, -3, 3, -2, 0, 4]).

test :-
  list_to_zipper([2, 1, -3, 3, -2, 0, 4], Z1),
  drag_item(Z1, right(2), Z3),
  zipper_to_list(Z3, L),
  assert('2', L, [1, -3, 2, 3, -2, 0, 4]).

test :-
  list_to_zipper([1, -3, 2, 3, -2, 0, 4], Z1),
  move_view(Z1, right(1), Z2),
  drag_item(Z2, left(3), Z3),
  zipper_to_list(Z3, L),
  assert('-3', L, [1, 2, 3, -2, -3, 0, 4]).

test :-
  list_to_zipper([1, 2, 3, -2, -3, 0, 4], Z1),
  move_view(Z1, right(2), Z2),
  drag_item(Z2, right(3), Z3),
  zipper_to_list(Z3, L),
  assert('3', L, [1, 2, -2, -3, 0, 3, 4]).

test :-
  list_to_zipper([1, 2, -2, -3, 0, 3, 4], Z1),
  move_view(Z1, right(2), Z2),
  drag_item(Z2, left(2), Z3),
  zipper_to_list(Z3, L),
  assert('-2', L, [1, 2, -3, 0, 3, 4, -2]).

test :-
  list_to_zipper([1, 2, -3, 0, 3, 4, -2], Z1),
  move_view(Z1, right(4), Z2),
  drag_item(Z2, left(0), Z3),
  zipper_to_list(Z3, L),
  assert('0', L, [1, 2, -3, 0, 3, 4, -2]).

test :-
  list_to_zipper([1, 2, -3, 0, 3, 4, -2], Z1),
  move_view(Z1, right(5), Z2),
  drag_item(Z2, right(4), Z3),
  zipper_to_list(Z3, L),
  assert('4', L, [1, 2, -3, 4, 0, 3, -2]).

runtests :-
  findall(_, test, _).

% Part 2 solution
% First we preprocess all the elements converting them to e(Position, Value)
% so that then we can search for the element with position P incrementally, moving it around
add_position(MaxIndex, [], [], MaxIndex).
add_position(P, [e(V)|Es], [e(P, V1)|Es1], MaxIndex) :-
  P1 is P + 1,
  V1 is V * 811589153,
  add_position(P1, Es, Es1, MaxIndex).
add_position(X, X1, MaxIndex) :- add_position(0, X, X1, MaxIndex).

find_element_by_index(Index, Z, Z) :-
  get_item(Z, e(Index, _)).
find_element_by_index(Index, Z, Z1) :-
  \+ get_item(Z, e(Index, _)),
  at_end(Z, false),
  move_view(Z, right, Z2),
  find_element_by_index(Index, Z2, Z1).

% For each index, search for the element in the list and move it to the new position
process2(Index, MaxIndex, Z, Z) :- Index >= MaxIndex.
process2(Index, MaxIndex, Z, Z1) :-
  % Quick log process
  ((0 #= Index mod 200) -> (write(p(Index, MaxIndex)), nl); true),
  Index < MaxIndex,
  go_to_start(Z, Z2),
  find_element_by_index(Index, Z2, Z3),
  get_item(Z3, e(Index, Value)),
  Steps is abs(Value) mod (MaxIndex - 1),
  (
    (Value >= 0, Direction = right(Steps));
    (Value < 0, Direction = left(Steps))
  ),
  drag_item(Z3, Direction, Z4),
  Index1 is Index + 1,
  !,
  process2(Index1, MaxIndex, Z4, Z1).

find_zero_2(Z, Z) :- get_item(Z, e(_, 0)).
find_zero_2(Z, Z1) :- \+ get_item(Z, e(_, 0)), move_view(Z, right, Z2), find_zero_2(Z2, Z1).

find_zero2(Z, Z1) :-
  go_to_start(Z, Z2),
  find_zero_2(Z2, Z1).

process2_ten_times(10, _, Z, Z).
% Stuff to store and retrieve progress, allowing stopping and restarting the program
process2_ten_times(X, MaxIndex, Z, Z1) :-
  write(process(X)), nl,
  open("20.progress", write, FD), write(FD, progress(X, MaxIndex, Z)), write(FD, '.'), close(FD),
  X < 10,
  process2(0, MaxIndex, Z, Z2),
  X1 is X + 1,
  process2_ten_times(X1, MaxIndex, Z2, Z1).

process2_ten_times(MaxIndex, _, Z1) :-
  file_exists("20.progress"),
  open("20.progress", read, FD), read_term(FD, progress(X, MaxIndex, Z), []), close(FD),
  write(continuing_from(X)),
  process2_ten_times(X, MaxIndex, Z, Z1).

process2_ten_times(MaxIndex, Z, Z1) :-
  process2_ten_times(0, MaxIndex, Z, Z1).

solution2(Sol2) :-
  phrase_from_file(file_entries(X), '20.input'),
  add_position(X, X1, MaxIndex),
  list_to_zipper(X1, Z),
  process2_ten_times(MaxIndex, Z, Z1),
  find_zero2(Z1, Z2),
  move_view(Z2, right(1000), Z3),
  get_item(Z3, e(_, I1000)),
  move_view(Z3, right(1000), Z4),
  get_item(Z4, e(_, I2000)),
  move_view(Z4, right(1000), Z5),
  get_item(Z5, e(_, I3000)),
  write(i(I1000, I2000, I3000)),
  Sol2 is I1000 + I2000 + I3000.
