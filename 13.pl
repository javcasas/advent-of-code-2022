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

entry(X) --> digits(X).
entry(X) --> list(X).

list_([]) --> [].
list_([X]) --> entry(X).
list_([X|Xs]) --> entry(X), ",", list_(Xs).
list(Xs) --> "[", list_(Xs), "]".

line(X) --> entry(X), "\n".

pair(p(X, Y)) --> line(X), line(Y).

pairs([]) --> [].
pairs([X|Xs]) --> pair(X), "\n", pairs(Xs).

lt(A, B, true) :- A < B.
lt(A, B, false) :- A >= B.
gt(A, B, T) :- lt(B, A, T).

compare_(L, R, Result) :- 
  integer(L),
  integer(R),
  if_(
    lt(L, R),
    Result = lt,
    if_(
      gt(L, R),
      Result = gt,
      Result = eq
    )
  ).

compare_([], [_|_], lt).
compare_([], [], eq).
compare_([_|_], [], gt).
compare_([L|Ls], [R|Rs], Result) :- 
  compare_(L, R, Result1),
  if_(
    Result1 = eq,
    compare_(Ls, Rs, Result),
    Result = Result1
  ).

compare_(L, R, Result) :-
  islist(L),
  integer(R),
  compare_(L, [R], Result).

compare_(L, R, Result) :-
  integer(L),
  islist(R),
  compare_([L], R, Result).

islist([]).
islist([X|Xs]).

compare_pair(p(L, R), Res) :- compare_(L, R, Res).

sum_comparisons(_, [], 0).
sum_comparisons(P, [X|Xs], R) :-
  P1 is P + 1,
  sum_comparisons(P1, Xs, R1),
  if_(
    X = lt,
    R is R1 + P,
    R is R1
  ).

sum_comparisons(X, R) :- sum_comparisons(1, X, R).

% Quicksort here because I can't find a sortBy algorithm

packet_lt(L, R, T) :-
  compare_(L, R, Res),
  if_(
    Res = lt,
    T = true,
    T = false
  ).
  
sort_packets([], []).
sort_packets([X], [X]).
sort_packets([Pivot|Xs], R) :-
  tpartition(packet_lt(Pivot), Xs, Smallers, Biggers),
  sort_packets(Smallers, SortedSmallers),
  sort_packets(Biggers, SortedBiggers),
  append(SortedSmallers, [Pivot], R1),
  append(R1, SortedBiggers, R).

pairs_to_list([], []).
pairs_to_list([p(X, Y)|Ps], [X,Y|Rs]) :-
  pairs_to_list(Ps, Rs).
  
solution1(Comparisons, Result) :-
  phrase_from_file(pairs(L), '13.input'),
  maplist(compare_pair, L, Comparisons),
  sum_comparisons(Comparisons, Result).

decoder_key(_, [], []).
decoder_key(C, [P|Ps], K) :-
  C1 is C + 1,
  compare_(P, [[2]], Is2),
  compare_(P, [[6]], Is6),
  decoder_key(C1, Ps, K1),
  if_(
    Is2 = eq,
    K = [C|K1],
    if_(
      Is6 = eq,
      K = [C|K1],
      K = K1
    )
  ).
decoder_key(L, K) :- decoder_key(1, L, K).

solution2(K) :-
  phrase_from_file(pairs(L), '13.input'),
  pairs_to_list(L, L2),
  append(L2, [[[2]]], L3),
  append(L3, [[[6]]], L4),
  sort_packets(L4, L5),
  reverse(L5, L6),
  decoder_key(L6, [K1, K2]),
  K is K1 * K2.
