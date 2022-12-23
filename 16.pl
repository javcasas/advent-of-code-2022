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

letter(X) --> [X], { char_code(X, C), char_code('A', A), char_code('Z', Z), C >= A, C =< Z }.
valve_name([X, Y]) --> letter(X), letter(Y).

valves([X]) --> valve_name(X).
valves([X|Xs]) --> valve_name(X), ", ", valves(Xs).

tunnel_message --> "; tunnels lead to valves ".
tunnel_message --> "; tunnel leads to valve ".

line(valve(Name, Flow, Tunnels)) -->
  "Valve ", valve_name(Name), " has flow rate=", digits(Flow), tunnel_message, valves(Tunnels), "\n", !.

lines([]) --> "\n".
lines([X|Xs]) --> line(X), lines(Xs).

total_flow_released_([], 0).
total_flow_released_([opened_valve(_, F)|OV], TotalFlow) :-
  total_flow_released_(OV, F1),
  TotalFlow is F + F1.

total_flow_released(state(_, _, _, OV), F) :- total_flow_released_(OV, F).


max_potential_extra_flow_(T, _, 0) :- T >= 30.
max_potential_extra_flow_(_, [], 0).
max_potential_extra_flow_(T, [closed_valve(_, F)|CVs], EF) :-
  ExtraFlow is (30 - T) * F,
  T1 is T + 2,
  max_potential_extra_flow_(T1, CVs, EF1),
  EF is EF1 + ExtraFlow,
  !.

max_potential_extra_flow(state(T, _, CV, _), EF) :- max_potential_extra_flow_(T, CV, EF).

step(_, state(_, Position, [], OpenedValves), done, state(30, Position, [], OpenedValves)).
step(_, state(Time, Position, ClosedValves, OpenedValves), open(Valve), state(Time1, Valve, ClosedValves1, OpenedValves1)) :-
  select(closed_valve(Valve, Flow), ClosedValves, ClosedValves1),
  OpenedValves1 = [opened_valve(Position, TotalFlowReleased)|OpenedValves],
  distance(Position, Valve, Dist),
  Time1 is Time + Dist + 1,
  Time1 =< 30,
  TotalFlowReleased is (30 - Time1 + 1) * Flow.
  
steps_(_, _, InitialState, [], InitialState) :- InitialState = state(30, _, _, _).
steps_(best_solution(S, BestScore), Paths, InitialState, [Action|Actions], FinalState) :-
  InitialState = state(Time, _, _, _),
  Time < 30,
  step(Paths, InitialState, Action, NextState),
  max_potential_extra_flow(NextState, MaxExtraScore),
  total_flow_released(NextState, CurrentScore),
  MaxExtraScore + CurrentScore > BestScore,
  steps_(best_solution(S, BestScore), Paths, NextState, Actions, FinalState).

sort_closed_valves(CV, CV1) :-
  findall(cv(F, P), select(closed_valve(P, F), CV, _), CV2),
  sort(CV2, CV3), reverse(CV3, CV4),
  findall(closed_valve(P, F), select(cv(F, P), CV4, _), CV1).

steps1(BestScore, Paths, InitialState, Actions, FinalState) :-
  steps_(best_solution([done], BestScore), Paths, InitialState, Actions1, FinalState1),
  !,
  (
    (
      Actions = Actions1,
      FinalState = FinalState1
    );
    (
      total_flow_released(FinalState1, BestScore1),
      steps1(BestScore1, Paths, InitialState, Actions2, FinalState2),
      Actions = Actions2,
      FinalState = FinalState2
    )
  ).

steps(Valves, Actions, FinalState) :-
  findall(
    closed_valve(P, F),
    (select(valve(P, F, _), Valves, _), F > 0),
    ClosedValves1
  ),
  sort_closed_valves(ClosedValves1, ClosedValves),
  findall(
    path(P, Ps),
    (select(valve(P, _, Ps), Valves, _)),
    Paths
  ),
  InitialState = state(1, "AA", ClosedValves, []),
  steps1(0, Paths, InitialState, Actions, FinalState).

:- dynamic(distance/3).

subtract(L, [], L).
subtract(L, [X|Xs], L1) :-
  select(X, L, L2),
  subtract(L2, Xs, L1).
subtract(L, [X|Xs], L1) :-
  \+ select(X, L, _),
  subtract(L, Xs, L1).


all_reached(Valves, Origin, Reached) :-
  findall(Name, distance(Origin, Name, _), Reached).

reached(Valves, Name, Reached) :-
  select(valve(Name, _, Reached), Valves, _).

new_reaches(Valves, Reached, NewReached) :-
  maplist(reached(Valves), Reached, NewReached1),
  maplist(list_to_ord_set, NewReached1, NewReached2),
  ord_union(NewReached2, NewReached).

remove_existing(Origin, [], []).
remove_existing(Origin, [Dest|Dests], Dests1) :-
  distance(Origin, Dest, _),
  !,
  remove_existing(Origin, Dests, Dests1).
remove_existing(Origin, [Dest|Dests], [Dest|Dests1]) :-
  \+ distance(Origin, Dest, _),
  !,
  remove_existing(Origin, Dests, Dests1).

fill_elements(Origin, [], _).
fill_elements(Origin, [Origin|Dests], Distance) :-
  fill_elements(Origin, Dests, Distance),
  !.
fill_elements(Origin, [Dest|Dests], Distance) :-
  \+ distance(Origin, Dest, _),
  !,
  assertz(distance(Origin, Dest, Distance)),
  fill_elements(Origin, Dests, Distance).
fill_elements(Origin, [Dest|Dests], Distance) :-
  distance(Origin, Dest, _),
  !.

fill_distances(Valves, Origin, Distance, []):- !.
fill_distances(Valves, Origin, Distance, Pending) :-
  new_reaches(Valves, Pending, Reaches),
  remove_existing(Origin, Reaches, NewReaches),
  ord_del_element(NewReaches, Origin, NewReaches2),
  fill_elements(Origin, NewReaches2, Distance),
  Distance1 is Distance + 1,
  fill_distances(Valves, Origin, Distance1, NewReaches2).

fill_all_distances(Valves, []).
fill_all_distances(Valves, [E|Es]) :-
  fill_distances(Valves, E, 1, [E]),
  fill_all_distances(Valves, Es).

fill_all_distances(Valves) :-
  findall(Name, select(valve(Name, _, _), Valves, _), Names),
  fill_all_distances(Valves, Names).
  

solution1(A, S, F) :-
  phrase_from_file(lines(X), '16.input'),
  retractall(distance(_, _, _)),
  fill_all_distances(X),
  steps(X, A, S),
  total_flow_released(S, F).

steps2(Valves, Actions, FinalState) :-
  findall(
    closed_valve(P, F),
    (select(valve(P, F, _), Valves, _), F > 0),
    ClosedValves1
  ),
  sort_closed_valves(ClosedValves1, ClosedValves),
  findall(
    path(P, Ps),
    (select(valve(P, _, Ps), Valves, _)),
    Paths
  ),
  InitialState = state(5, "AA", ClosedValves, []),
  steps1(0, Paths, InitialState, Actions, FinalState).

solution2(A, S, F) :-
  phrase_from_file(lines(X), '16.input'),
  retractall(distance(_, _, _)),
  fill_all_distances(X),
  steps2(X, A, S),
  total_flow_released(S, F).
