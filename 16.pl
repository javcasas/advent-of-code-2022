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


%max_potential_extra_flow_(30, _, 0).
%max_potential_extra_flow_(X, _, 0).

%max_potential_extra_flow(state(T, _, CV, _), EF) :- max_potential_extra_flow_(T, CV, EF).

%step(Valves, Position, Action, NewPosition).
step(state(Time, Position, [], OpenedValves), done, state(30, Position, [], OpenedValves)).
step(state(Time, Position, ClosedValves, OpenedValves), open(Position), state(Time1, Position, ClosedValves1, OpenedValves1)) :-
  select(closed_valve(Position), ClosedValves, ClosedValves1),
  valve(Position, Flow, _),
  Flow > 0,
  OpenedValves1 = [opened_valve(Position, TotalFlowReleased)|OpenedValves],
  TotalFlowReleased is (30 - Time) * Flow,
  Time1 is Time + 1.

step(state(Time, Position, ClosedValves, OpenedValves), move_to(Destination), state(Time1, Destination, ClosedValves, OpenedValves)) :-
  valve(Position, _, Destinations),
  select(Destination, Destinations, _),
  Time1 is Time + 1.

%steps(Start, InitialState, Actions, FinalState)
steps_(InitialState, [], InitialState) :- InitialState = state(30, _, _, _).
steps_(InitialState, [Action|Actions], FinalState) :-
  InitialState = state(Time, _, _, _),
  Time < 30,
  step(InitialState, Action, NextState),
  steps_(NextState, Actions, FinalState).

openable_valve(closed_valve(P), T) :-
  valve(P, F, _),
  if_(
    F = 0,
    T = false,
    T = true
  ).

steps(Actions, FinalState) :-
  InitialState = state(1, "AA", ClosedValves1, []),
  findall(closed_valve(P), valve(P, _, _), ClosedValves),
  tfilter(openable_valve, ClosedValves, ClosedValves1),
  steps_(InitialState, Actions, FinalState).

:- dynamic(valve/3).
fillvalves_([]).
fillvalves_([X|Xs]) :-
  assertz(X),
  fillvalves_(Xs).
fillvalves(X) :-
  retractall(valve(_, _, _)),
  fillvalves_(X).

solution1(A, F) :-
  phrase_from_file(lines(X), '16.sample.input'),
  fillvalves(X),
  steps(A, F).
