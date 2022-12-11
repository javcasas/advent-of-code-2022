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

line(addx(X)) --> "addx ", digits(X), "\n".
line(noop) --> "noop\n".

lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

%execute(Instructions, XRegister, Result)
execute([], X, [X]).
execute([noop|Ins], X, [X|Xs]) :-
  execute(Ins, X, Xs).
execute([addx(Add)|Ins], X, [X, X|Xs]) :-
  X1 is X + Add,
  execute(Ins, X1, Xs).

execute(Ins, Xs) :- execute(Ins, 1, Xs).

  
signal_point_multiplier(X, 0) :-
  X > 220.
signal_point_multiplier(X, Y) :-
  X =< 220,
  Zero is (X - 20) mod 40,
  if_(
    Zero = 0,
    Y = X,
    Y = 0
  ).

signal_strength(_, [], 0).
signal_strength(Pos, [X|Xs], S) :-
  signal_point_multiplier(Pos, Mul),
  Pos1 is Pos + 1,
  signal_strength(Pos1, Xs, S1),
  S is S1 + X * Mul.
  
signal_strength(Xs, S) :-
  signal_strength(1, Xs, S).

pixel_(-1, '#').
pixel_(0, '#').
pixel_(1, '#').
pixel_(X, '.') :-
  X \= -1,
  X \= 0,
  X \= 1.

pixel(X, Pos, P) :-
  D is Pos - X,
  if_(
    (D = -1; D=0; D=1),
    P = ('#'),
    P = ('.')
  ).

pixels([], _, []).
pixels([X|Xs], Pos, [R|Rs]) :-
  pixel(X, Pos, R),
  Pos1 is (Pos + 1) mod 40,
  pixels(Xs, Pos1, Rs).

write_pixels(N, [X|Xs]) :-
  N1 is N + 1,
  write(X),
  Newline is N1 mod 40,
  if_(
    Newline = 0,
    write("\n"),
    true
  ),
  write_pixels(N1, Xs).

write_pixels(X) :- write_pixels(0, X).

solution1(S) :-
  phrase_from_file(lines(L), '10.input'),
  execute(L, Xs),
  signal_strength(Xs, S).

solution2(S) :-
  phrase_from_file(lines(L), '10.input'),
  execute(L, Xs),
  pixels(Xs, 0, S).
