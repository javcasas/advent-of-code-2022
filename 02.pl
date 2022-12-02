:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).

result(rock, scissors, lose).
result(scissors, paper, lose).
result(paper, rock, lose).
result(scissors, rock, win).
result(paper, scissors, win).
result(rock, paper, win).
result(X, X, draw).

player1(rock) --> "A".
player1(paper) --> "B".
player1(scissors) --> "C".

player2(rock) --> "X".
player2(paper) --> "Y".
player2(scissors) --> "Z".

round(P1, P2, R) --> player1(P1), " ", player2(P2), "\n", {result(P1, P2, R)}.

rounds([]) --> "\n".
rounds([r(A, B, C)|Xs]) --> round(A, B, C), rounds(Xs).

result_(lose) --> "X".
result_(draw) --> "Y".
result_(win) --> "Z".

round2(P1, P2, R) --> player1(P1), " ", result_(R), "\n", {result(P1, P2, R)}.

rounds2([]) --> "\n".
rounds2([r(A, B, C)|Xs]) --> round2(A, B, C), rounds2(Xs).


score2(rock, 1).
score2(paper, 2).
score2(scissors, 3).

winscore(lose, 0).
winscore(draw, 3).
winscore(win, 6).

score(r(_, Y, Z), S) :-
  score2(Y, S1),
  winscore(Z, S3),
  S is S1 + S3.

totalScore([], 0).
totalScore([X|Xs], R) :-
  score(X, S),
  totalScore(Xs, Ss),
  R is S + Ss.

solution(part1(S), part2(S2)) :-
  phrase_from_file(rounds(R), '02.input'),
  totalScore(R, S),
  phrase_from_file(rounds2(R2), '02.input'),
  totalScore(R2, S2).
