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

monkey_index(X) --> "Monkey ", digits(X), ":\n".

starting_items_([X]) --> digits(X).
starting_items_([X|Xs]) --> digits(X), ", ", starting_items_(Xs).
starting_items(X) --> "  Starting items: ", starting_items_(X), "\n".

operand_(old) --> "old".
operand_(X) --> digits(X).
operation_(A + B) --> operand_(A), " + ", operand_(B).
operation_(A * B) --> operand_(A), " * ", operand_(B).
operation(X) --> "  Operation: new = ", operation_(X), "\n".

iftrue(X) --> "    If true: throw to monkey ", digits(X).
iffalse(X) --> "    If false: throw to monkey ", digits(X).
test(t(D, T, F)) --> "  Test: divisible by ", digits(D), "\n", iftrue(T), "\n", iffalse(F), "\n".

monkey(monkey(Index, StartItems, Operation, Test, 0)) -->
  monkey_index(Index),
  starting_items(StartItems),
  operation(Operation),
  test(Test),
  "\n".

monkeys([]) --> [].
monkeys([X|Xs]) --> monkey(X), monkeys(Xs).

item_operation(Item, old, Item).
item_operation(_, X, X) :- integer(X).
item_operation(Item, A+B, X) :-
  item_operation(Item, A, X1),
  item_operation(Item, B, X2),
  X is X1 + X2.
item_operation(Item, A*B, X) :-
  item_operation(Item, A, X1),
  item_operation(Item, B, X2),
  X is X1 * X2.

test_operation(t(Divisor, True, False), Item, Thrown) :-
  Divisible is Item mod Divisor,
  if_(
    Divisible = 0,
    Thrown = True,
    Thrown = False
  ).

turn_monkey_item(monkey(Index, [Item|Items], Operation, Test, Inspects), monkey(Index, Items, Operation, Test, Inspects1), Throw) :-
  Inspects1 is Inspects + 1,
  item_operation(Item, Operation, Item1),
  Item2 is floor(Item1 / 3),
  test_operation(Test, Item2, Thrown),
  Throw = throw_to(Item2, Thrown).

turn_monkey(monkey(Index, [], Operation, Test, X), monkey(Index, [], Operation, Test, X), []) :- !.
turn_monkey(M, M2, [T|Ts]) :-
  turn_monkey_item(M, M1, T),
  turn_monkey(M1, M2, Ts).

receive_item([], _, []).
receive_item(
  [monkey(Index, Items, Operation, Test, Activity)|Ms],
  throw_to(Item, Index),
  [monkey(Index, Items1, Operation, Test, Activity)|Ms]
) :- append(Items, [Item], Items1), !.
receive_item([M|Ms], T, [M|Ms1]) :- receive_item(Ms, T, Ms1).

receive_items(Monkeys, [], Monkeys).
receive_items(Monkeys, [T|Ts], Monkeys2) :-
  receive_item(Monkeys, T, Monkeys1),
  receive_items(Monkeys1, Ts, Monkeys2).
  

turn([M|Monkeys], Monkeys1) :-
  turn_monkey(M, M1, Throws),
  receive_items(Monkeys, Throws, Monkeys2),
  append(Monkeys2, [M1], Monkeys1).

round(Monkeys, Monkeys, 0) :- !.
round(Monkeys, Monkeys1, Turns) :-
  Turns1 is Turns - 1,
  turn(Monkeys, Monkeys2),
  round(Monkeys2, Monkeys1, Turns1).

round(Monkeys, Monkeys1) :-
  length(Monkeys, Turns),
  round(Monkeys, Monkeys1, Turns).

rounds(0, Monkeys, Monkeys) :- !.
rounds(X, Monkeys, Monkeys1) :-
  round(Monkeys, Monkeys2),
  X1 is X - 1,
  rounds(X1, Monkeys2, Monkeys1).

monkey_activity(monkey(_,_,_,_,Activity), Activity).

solution1(Sol1) :-
  phrase_from_file(monkeys(X), '11.input'),
  rounds(20, X, X1),
  maplist(monkey_activity, X1, X2),
  sort(X2, X3), reverse(X3, [A1, A2|_]),
  Sol1 is A1 * A2.

turn_monkey_item2(Modulus, monkey(Index, [Item|Items], Operation, Test, Inspects), monkey(Index, Items, Operation, Test, Inspects1), Throw) :-
  Inspects1 is Inspects + 1,
  item_operation(Item, Operation, Item1),
  Item2 is Item1 mod Modulus,
  test_operation(Test, Item2, Thrown),
  Throw = throw_to(Item2, Thrown),
  !.

turn_monkey2(Modulus, monkey(Index, [], Operation, Test, X), monkey(Index, [], Operation, Test, X), []) :- !.
turn_monkey2(Modulus, M, M2, [T|Ts]) :-
  turn_monkey_item2(Modulus, M, M1, T),
  turn_monkey2(Modulus, M1, M2, Ts),
  !.

receive_item2([], _, []).
receive_item2(
  [monkey(Index, Items, Operation, Test, Activity)|Ms],
  throw_to(Item, Index),
  [monkey(Index, Items1, Operation, Test, Activity)|Ms]
) :- append(Items, [Item], Items1), !.
receive_item2([M|Ms], T, [M|Ms1]) :- receive_item2(Ms, T, Ms1), !.

receive_items2(Monkeys, [], Monkeys).
receive_items2(Monkeys, [T|Ts], Monkeys2) :-
  receive_item2(Monkeys, T, Monkeys1),
  receive_items2(Monkeys1, Ts, Monkeys2),
  !.
  

turn2(Modulus, [M|Monkeys], Monkeys1) :-
  turn_monkey2(Modulus, M, M1, Throws),
  receive_items2(Monkeys, Throws, Monkeys2),
  append(Monkeys2, [M1], Monkeys1),
  !.

round2(_, Monkeys, Monkeys, 0) :- !.
round2(Modulus, Monkeys, Monkeys1, Turns) :-
  Turns1 is Turns - 1,
  turn2(Modulus, Monkeys, Monkeys2),
  round2(Modulus, Monkeys2, Monkeys1, Turns1),
  !.

round2(Modulus, Monkeys, Monkeys1) :-
  length(Monkeys, Turns),
  round2(Modulus, Monkeys, Monkeys1, Turns),
  !.

rounds2(Modulus, 0, Monkeys, Monkeys) :- !.
rounds2(Modulus, X, Monkeys, Monkeys1) :-
  write(X), write("\n"),
  round2(Modulus, Monkeys, Monkeys2),
  X1 is X - 1,
  rounds2(Modulus, X1, Monkeys2, Monkeys1),
  !.

last([X], X).
last([_|Xs], X) :- last(Xs, X).

mod_monkey(monkey(_, _, _, t(X, _, _), _), X).

mul_mods([], 1).
mul_mods([X|Xs], R) :-
  mul_mods(Xs, Rs),
  R is X * Rs.

solution2(Divider, Sol2) :-
  phrase_from_file(monkeys(X), '11.input'),
  maplist(mod_monkey, X, Mods),
  mul_mods(Mods, Divider),
  rounds2(Divider, 10000, X, X1),
  maplist(monkey_activity, X1, X2),
  sort(X2, X3), reverse(X3, [A1, A2|_]),
  Sol2 is A1 * A2.
