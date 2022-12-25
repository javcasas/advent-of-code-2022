:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(clpz)).

snafu_digit(2) --> "2".
snafu_digit(1) --> "1".
snafu_digit(0) --> "0".
snafu_digit(-1) --> "-".
snafu_digit(-2) --> "=".

snafu_digits([]) --> [].
snafu_digits([D|Ds]) --> snafu_digit(D), snafu_digits(Ds).

snafu_file([]) --> "\n".
snafu_file([X|Xs]) --> snafu_digits(X), "\n", snafu_file(Xs).

snafu_value([], 0).
snafu_value(Digits, Value) :-
  append(Prefix, [Digit], Digits),
  Digit #>= -2,
  Digit #=< 2,
  Value #= Digit + 5 * PrefixValue,
  snafu_value(Prefix, PrefixValue).

solution1(X) :-
  phrase_from_file(snafu_file(D), '25.input'),
  maplist(snafu_value, D, Values),
  sum_list(Values, Total),
  snafu_value(X1, Total),
  phrase(snafu_digits(X1), X).
