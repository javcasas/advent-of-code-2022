:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).

jet_(left) --> "<".
jet_(right) --> ">".
jet_pattern([]) --> "\n".
jet_pattern([X|Xs]) --> jet_(X), jet_pattern(Xs).

rock(0, [block(0,0), block(1, 0), block(2,0), block(3,0)]).
rock(1, [block(1,0), block(0, 1), block(1,1), block(2, 1), block(1, 2)]).
rock(2, [block(0,0), block(1, 0), block(2, 0), block(2,1), block(2,2)]).
rock(3, [block(0,0), block(0, 1), block(0,2), block(0,3)]).
rock(4, [block(0,0), block(0, 1), block(1,0), block(1,1)]).

rocks(X) :-
  findall(rock(B), rock(_, B), X).

:- dynamic(stopped_rock/2).
:- dynamic(top_y/1).

start_simulation :-
  retractall(stopped_rock(_, _)),
  retractall(top_y(_)),
  assertz(top_y(-1)).

move_blocks([], _, _, []).
move_blocks([block(X, Y)|Bs], Dx, Dy, [block(X1, Y1)|Rs]) :-
  X1 is X + Dx,
  Y1 is Y + Dy,
  move_blocks(Bs, Dx, Dy, Rs).

spawn_rock([R|Rs], Rs1, rock(Blocks1)) :-
  append(Rs, [R], Rs1),
  rock(Blocks) = R,
  top_y(TopY),
  Y is TopY + 4,
  move_blocks(Blocks, 2, Y, Blocks1).

no_stopped_rock_overlap([], true).
no_stopped_rock_overlap([block(X, Y)|_], false) :-
  X < 0, !.
no_stopped_rock_overlap([block(X, Y)|_], false) :-
  X > 6, !.
no_stopped_rock_overlap([block(X, Y)|_], false) :-
  Y < 0, !.
no_stopped_rock_overlap([block(X, Y)|Blocks], false) :-
  stopped_rock(X, Y),
  !.
no_stopped_rock_overlap([block(X, Y)|Blocks], T) :-
  \+ stopped_rock(X, Y),
  !,
  no_stopped_rock_overlap(Blocks, T).

jet_push(Blocks, left, Blocks1) :-
  move_blocks(Blocks, -1, 0, Blocks2),
  if_(
    no_stopped_rock_overlap(Blocks2),
    Blocks2 = Blocks1,
    Blocks1 = Blocks
  ).

jet_push(Blocks, right, Blocks1) :-
  move_blocks(Blocks, 1, 0, Blocks2),
  if_(
    no_stopped_rock_overlap(Blocks2),
    Blocks2 = Blocks1,
    Blocks1 = Blocks
  ).

assert_top_y(Y) :-
  top_y(CurrentY),
  CurrentY < Y,
  retractall(top_y(_)),
  assertz(top_y(Y)),
  !.

assert_top_y(_) :- !.

assert_stopped_rocks([]).
assert_stopped_rocks([block(X, Y)|Bs]) :-
  assertz(stopped_rock(X, Y)),
  assert_top_y(Y),
  assert_stopped_rocks(Bs).

down_push(Blocks, Result) :-
  move_blocks(Blocks, 0, -1, Blocks2),
  if_(
    no_stopped_rock_overlap(Blocks2),
    Result = Blocks2,
    (Result = stuck, assert_stopped_rocks(Blocks))
  ).
  

simulate_rock(rock(Blocks), [P|JetPatterns], JetPatterns1) :-
  append(JetPatterns, [P], JetPatterns2),
  jet_push(Blocks, P, Blocks1),
  down_push(Blocks1, Result),
  if_(
    Result = stuck,
    JetPatterns1 = JetPatterns2,
    simulate_rock(rock(Result), JetPatterns2, JetPatterns1)
  ).
  

step(Rocks, JetPatterns, Rocks1, JetPatterns1) :-
  spawn_rock(Rocks, Rocks1, Rock),
  simulate_rock(Rock, JetPatterns, JetPatterns1).

find_y(X, Y1) :-
  findall(Y, stopped_rock(X, Y), Res),
  list_max(Res, Y1).

retract_list([]).
retract_list([X|Xs]):-retract(X), retract_list(Xs).

assert_all([]).
assert_all([X|Xs]) :- assertz(X), assert_all(Xs).

clear_under(Y) :-
  findall(stopped_rock(X, Y1), (stopped_rock(X, Y1), Y1 < Y), ToDelete),
  retract_list(ToDelete),
  findall(stopped_rock(X, Y1), stopped_rock(X, Y1), ToSave),
  length(ToDelete, N),
  length(ToSave, N1),
  write(deleted(N, N1)), nl,
  retractall(stopped_rock(_)),
  assert_all(ToSave).

clear_rocks :-
  find_y(0, Y0),
  find_y(1, Y1),
  find_y(2, Y2),
  find_y(3, Y3),
  find_y(4, Y4),
  find_y(5, Y5),
  find_y(6, Y6),
  list_min([Y0, Y1, Y2, Y3, Y4, Y5, Y6], Y),
  clear_under(Y).

steps(0, Rocks, JetPatterns, Rocks, JetPatterns).
steps(N, Rocks, JetPatterns, Rocks1, JetPatterns1) :-
  N > 0,
  write(N), write('\n'),
  ShallClear is N mod 200,
  if_(
    ShallClear = 0,
    clear_rocks,
    true
  ),
  step(Rocks, JetPatterns, Rocks2, JetPatterns2),
  N1 is N - 1,
  !,
  steps(N1, Rocks2, JetPatterns2, Rocks1, JetPatterns1).

draw_rock(X, Y) :-
  stopped_rock(X, Y),
  write('#'),
  !.
draw_rock(X, Y) :-
  \+ stopped_rock(X, Y),
  write('.'),
  !.
draw_rocks(-1).
draw_rocks(Y) :-
  Y >= 0,
  draw_rock(0, Y),
  draw_rock(1, Y),
  draw_rock(2, Y),
  draw_rock(3, Y),
  draw_rock(4, Y),
  draw_rock(5, Y),
  draw_rock(6, Y),
  write('\n'),
  Y1 is Y - 1,
  !,
  draw_rocks(Y1).

draw_rocks :-
  top_y(Y),
  draw_rocks(Y).

solution1(Sol1) :-
  phrase_from_file(jet_pattern(X), '17.input'),
  start_simulation,
  rocks(R),
  steps(2022, R, X, _, _),
  top_y(Y),
  Sol1 is Y + 1. 

draw_top_rocks(Y, Y1) :-
  Y >= Y1,
  draw_rock(0, Y),
  draw_rock(1, Y),
  draw_rock(2, Y),
  draw_rock(3, Y),
  draw_rock(4, Y),
  draw_rock(5, Y),
  draw_rock(6, Y),
  write('\n'),
  Y1 is Y - 1,
  !,
  draw_rocks(Y1).

draw_top_rocks :-
  top_y(Y),
  Y1 is Y - 5,
  draw_top_rocks(Y, Y1).

simulate_rock2(rock(Blocks), JPI, JPI1) :-
  jet_pattern_index(JPI, P),
  jet_push(Blocks, P, Blocks1),
  down_push(Blocks1, Result),
  max_jet_pattern_index(MJPI),
  JPI2 is (JPI + 1) mod MJPI,
  if_(
    Result = stuck,
    JPI1 = JPI2,
    simulate_rock2(rock(Result), JPI2, JPI1)
  ).

spawn_rock2(rock(Blocks), rock(Blocks1)) :-
  top_y(TopY),
  Y is TopY + 4,
  move_blocks(Blocks, 2, Y, Blocks1).

step2(Blocks, JPI, JPI1) :-
  spawn_rock2(rock(Blocks), Rock1),
  simulate_rock2(Rock1, JPI, JPI1).

write_cjpi(CJPI, CRI, N) :-
  CJPI < 5,
  top_y(Y),
  write(cjpi(jet_index(CJPI), rock_index(CRI), rock(N), y(Y))), nl.
write_cjpi(_, _, _).

steps2(N, N1, CurrentJetPattern, CurrentJetPattern) :- N > N1.
steps2(N, N1, CurrentJetPatternIndex, CurrentJetPatternIndex1) :-
  CurrentRockIndex is N mod 5,
  rock(CurrentRockIndex, Blocks),
  step2(Blocks, CurrentJetPatternIndex, CurrentJetPatternIndex2),
  write_cjpi(CurrentJetPatternIndex, CurrentRockIndex, N), 
  ShallClear is (N+1) mod 500,
  if_(
    ShallClear = 0,
    clear_rocks,
    true
  ),
  N2 is N + 1,
  !,
  steps2(N2, N1, CurrentJetPatternIndex2, CurrentJetPatternIndex1).

:- dynamic(jet_pattern_index/2).
:- dynamic(max_jet_pattern_index/1).

assert_jet_pattern(N, []) :-
  retractall(max_jet_pattern_index(_)),
  assertz(max_jet_pattern_index(N)).

assert_jet_pattern(N, [X|Xs]) :-
  assertz(jet_pattern_index(N, X)),
  N1 is N + 1,
  assert_jet_pattern(N1, Xs).

assert_jet_pattern(Xs) :-
  retractall(jet_pattern_index(_, _)),
  assert_jet_pattern(0, Xs).

start_simulation2(JetPattern) :-
  retractall(stopped_rock(_, _)),
  retractall(top_y(_)),
  assertz(top_y(-1)),
  assert_jet_pattern(JetPattern).

solution2(Sol2) :-
  phrase_from_file(jet_pattern(X), '17.input'),
  rocks(R),
  length(X, NPatterns),
  length(R, NRocks),
  %Sol2 is NPatterns * NRocks,
  %write(debug(NPatterns, NRocks)),
  start_simulation2(X),
  DroppedRocks is 10000,
  DroppedRocks1 is DroppedRocks - 1,
  steps2(0, DroppedRocks1, 0, _),
  top_y(Y),
  Sol2 is Y + 1. 
  
solution2_1(Sol2) :-
  TotalRocks = 1000000000000,
  TR1 is TotalRocks - 1724,
  RepeatCycles is TR1 // 1725,
  RemainingAfter is TR1 mod 1725,

  BaseHeight = 2612,
  CycleHeight is RepeatCycles * 2630,

  phrase_from_file(jet_pattern(X), '17.input'),
  start_simulation2(X),
  RemainingAfter4 is RemainingAfter + 4 + 1,
  steps2(4, RemainingAfter4, 0, _),
  top_y(Y),
  Sol2 is BaseHeight + CycleHeight + Y + 1.
  

% jet_index(0),rock_index(0),rock(0)
% jet_index(4),rock_index(1),rock(1), y=3
% jet_index(0),rock_index(4),rock(1724), y=2612
% jet_index(0),rock_index(4),rock(3449), y=5242
% jet_index(0),rock_index(4),rock(5174), y=7872
% jet_index(0),rock_index(4),rock(6899)

% tower height = top_y + 1
% pattern:
%  start: rock 0 - rock 1723 - y=2612
%  repeat: rock 1724 - rock 3449 - pattern length: 1725, y increment=2630
