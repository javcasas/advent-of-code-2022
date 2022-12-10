:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).

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

line([]) --> "\n".
line([X|Xs]) --> digit(X), line(Xs).

lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

tree_lookup(Grid, lookup(Grid, TopFirst, RightFirst, BottomFirst)) :-
  transpose(Grid, TopFirst),
  maplist(reverse, TopFirst, BottomFirst),
  maplist(reverse, Grid, RightFirst).

all_smaller(_, 0, _).
all_smaller([RowElem|Row], XCoord, TreeSize) :-
  RowElem < TreeSize,
  NextXCoord is XCoord - 1,
  all_smaller(Row, NextXCoord, TreeSize).

is_visible_left(lookup(Left, _, _, _), coords(X, Y)) :-
  nth0(Y, Left, Row),
  nth0(X, Row, TreeSize),
  all_smaller(Row, X, TreeSize).

is_visible_right(lookup(_, _, Right, _), coords(X, Y)) :-
  nth0(Y, Right, Row),
  nth0(X, Row, TreeSize),
  all_smaller(Row, X, TreeSize).

is_visible_top(lookup(_, Top, _, _), coords(X, Y)) :-
  nth0(Y, Top, Row),
  nth0(X, Row, TreeSize),
  all_smaller(Row, X, TreeSize).
  
is_visible_bottom(lookup(_, _, _, Bottom), coords(X, Y)) :-
  nth0(Y, Bottom, Row),
  nth0(X, Row, TreeSize),
  all_smaller(Row, X, TreeSize).

  
is_visible(Lookup, Coords) :- is_visible_left(Lookup, Coords). 
is_visible(Lookup, Coords) :- is_visible_right(Lookup, Coords). 
is_visible(Lookup, Coords) :- is_visible_top(Lookup, Coords). 
is_visible(Lookup, Coords) :- is_visible_bottom(Lookup, Coords). 

ge(X, Y, true) :- X >= Y.
ge(X, Y, false) :- X < Y.

row_visible([], _, [], 0).
row_visible([R|Rs], Height, Rest, Pos) :-
  if_(
    ge(R, Height),
    (Rs = Rest, Pos = 0),
    (row_visible(Rs, Height, Rest, Pos1), Pos is Pos1 + 1)
  ).

row_visibility_lookup(Row, 9, [visibility(9, Pos)]) :-
  row_visible(Row, 9, _, Pos).
row_visibility_lookup(Row, X, [visibility(X, Pos)|Vs]) :-
  X < 9,
  row_visible(Row, X, _, Pos),
  X1 is X + 1,
  row_visibility_lookup(Row, X1, Vs).

row_visibility_lookup(Row, Visibility) :- row_visibility_lookup(Row, 0, Visibility).

is_tree_visible_([visibility(Height, MinX)|_], Xcoord, Height) :- Xcoord =< MinX.
is_tree_visible_([visibility(HeightT, _)|Vs], Xcoord, Height) :-
  HeightT < Height,
  is_tree_visible_(Vs, Xcoord, Height).
  
is_tree_visible(Row, XCoord) :-
  nth0(XCoord, Row, Height),
  row_visibility(Row, RV),
  is_tree_visible_(RV, XCoord, Height).

row_visibility(_, _, [], []).
row_visibility(Lookup, Position, [X|Rest], [V|Vs]) :-
  is_tree_visible_(Lookup, Position, X),
  V = true,
  Pos1 is Position + 1,
  row_visibility(Lookup, Pos1, Rest, Vs).
row_visibility(Lookup, Position, [X|Rest], [V|Vs]) :-
  \+is_tree_visible_(Lookup, Position, X),
  V = false,
  Pos1 is Position + 1,
  row_visibility(Lookup, Pos1, Rest, Vs).

row_visibility(Row, Visibility) :-
  row_visibility_lookup(Row, L),
  row_visibility(L, 0, Row, Visibility).
    
blit4_row([], [], [], [], []).
blit4_row([A|As], [B|Bs], [C|Cs], [D|Ds], [R|Rs]) :-
  if_(
    (A = true; B = true; C = true; D = true),
    R = true,
    R = false
  ),
  blit4_row(As, Bs, Cs, Ds, Rs).

blit4([], [], [], [], []).
blit4([A|As], [B|Bs], [C|Cs], [D|Ds], [R|Rs]) :-
  blit4_row(A, B, C, D, R),
  blit4(As, Bs, Cs, Ds, Rs).

count_visible_row([], 0).
count_visible_row([X|Xs], Result) :-
  if_(
    X = true,
    R = 1,
    R = 0
  ),
  count_visible_row(Xs, Rs),
  Result is R + Rs.

count_visible([], 0).
count_visible([R|Rs], Result) :-
  count_visible_row(R, Result1),
  count_visible(Rs, Results),
  Result is Result1 + Results.

tree_score([], _, 0).
tree_score([R|Rest], Tree, Score1) :-
  R < Tree,
  tree_score(Rest, Tree, Score),
  Score1 is Score + 1.

tree_score([R|Rest], Tree, 1) :-
  R >= Tree.

row_scores(Row, Tree, [], [0]).
row_scores(Before, Tree, After, [S|Ss]) :-
  tree_score(Before, Tree, Score1),
  tree_score(After, Tree, Score2),
  S is Score1 * Score2,
  [NewTree|NewAfter] = After,
  row_scores([Tree|Before], NewTree, NewAfter, Ss).
row_scores([R|Row], Scores) :- row_scores([], R, Row, Scores).

%row_scores(Before, Tree, After


solution1(Sol1) :-
  phrase_from_file(lines(L), '08.input'),

  maplist(row_visibility, L, Left),

  transpose(L, T),
  maplist(row_visibility, T, TopT),
  transpose(TopT, Top),

  maplist(reverse, L, R),
  maplist(row_visibility, R, RightR),
  maplist(reverse, RightR, Right),

  maplist(reverse, T, B),
  maplist(row_visibility, B, BottomRT),
  maplist(reverse, BottomRT, BottomT),
  transpose(BottomT, Bottom),
  
  blit4(Top, Left, Right, Bottom, Result),
  Sol1 = Result.

blitmul_row([], [], []).
blitmul_row([A|As], [B|Bs], [R|Rs]) :-
  R is A * B,
  blitmul_row(As, Bs, Rs).

blitmul([], [], []).
blitmul([A|As], [B|Bs], [R|Rs]) :-
  blitmul_row(A, B, R),
  blitmul(As, Bs, Rs).

max_matrix(M, Max) :-
  maplist(list_max, M, Maxes),
  list_max(Maxes, Max).

solution2(Scores, Max) :-
  phrase_from_file(lines(L), '08.input'),

  maplist(row_scores, L, RowScores),

  transpose(L, T),
  maplist(row_scores, T, ColScoresT),
  transpose(ColScoresT, ColScores),

  blitmul(RowScores, ColScores, Scores),
  max_matrix(Scores, Max).
