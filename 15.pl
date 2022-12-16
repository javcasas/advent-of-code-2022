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

parse_sensor(s(X, Y, X1, Y1)) -->
  "Sensor at x=", digits(X), ", y=", digits(Y), ": ",
  "closest beacon is at x=", digits(X1), ", y=", digits(Y1), "\n".

sensors([]) --> "\n".
sensors([X|Xs]) --> parse_sensor(X), sensors(Xs).

sensor([s(X, Y, _, _)], X, Y).
sensor([s(X, Y, _, _)|_], X, Y).
sensor([_|Sx], X, Y) :- sensor(Sx, X, Y).

beacon([s(_, _, X, Y)], X, Y).
beacon([s(_, _, X, Y)|_], X, Y).
beacon([_|Sx], X, Y) :- beacon(Sx, X, Y).

covers(s(X, Y, BX, BY), CX, CY) :-
  Distance #= abs(BX - X) + abs(BY - Y),
  Distance #>= abs(CX - X) + abs(CY - Y).

%any_covers([S], CX, CY) :-
%  covers(S, CX, CY).
any_covers([S|_], CX, CY) :-
  covers(S, CX, CY).
any_covers([_|Ss], CX, CY) :-
  any_covers(Ss, CX, CY).

any_covers2([s(X, Y, BX, BY)], CX, CY, Expr) :-
  Expr = ((abs(BX - X) + abs(BY - Y)) #>= (abs(CX - X) + abs(CY - Y))),
  !.
any_covers2([s(X, Y, BX, BY)|Xs], CX, CY, Expr2) :-
  Distance = (abs(BX - X) + abs(BY - Y)),
  Expr = (Distance #>= (abs(CX - X) + abs(CY - Y))),
  any_covers2(Xs, CX, CY, Expr1),
  Expr = (Expr1 #\/ Expr2).

no_sensor_no_beacon2([], CX, CY, 1 #= 2).
no_sensor_no_beacon2([s(X, Y, BX, BY)|Xs], CX, CY, Expr) :-
  no_sensor_no_beacon2(Xs, CX, CY, Expr1),
  NoSensor = ((CX #\= X) #\/ (CY #\= Y)),
  NoBeacon = ((CX #\= BX) #\/ (CY #\= BY)),
  Expr = (NoSensor #/\ NoBeacon #/\ Expr1).

covered(Sensors, X, Y, Sols) :-
  any_covers(Sensors, X, Y),
  write([X, Y]),
  findall([X, Y], label([X, Y]), Sols).

concatall([], []).
concatall([X|Xs], S) :-
  concatall(Xs, Rest),
  append(X, Rest, S).

covered_coords(Sensors, X, Y, S4) :-
  findall(Sols, covered(Sensors, X, Y, Sols), S1),
  concatall(S1, S2),
  sort(S2, S3),
  no_sensor_no_beacon(Sensors, S3, S4).

no_sensor_no_beacon(Sensors, [], []).
no_sensor_no_beacon(Sensors, [[X,Y]|Xs], Xs) :-
  sensor(Sensors, X, Y),
  !.
no_sensor_no_beacon(Sensors, [[X,Y]|Xs], Xs) :-
  beacon(Sensors, X, Y),
  !.
no_sensor_no_beacon(Sensors, [[X,Y]|Xs], [[X, Y]|Xs1]) :-
  no_sensor_no_beacon(Sensors, Xs, Xs1).

line_covered([], _, []).
line_covered([s(X, Y, BX, BY)|Xs], CY, Expr) :-
  Distance is abs(X - BX) + abs(Y - BY),
  %Distance =< abs(X - CX) + abs(Y - CY)
  %Distance - ABS(Y - CY) =< abs(X - CX)
  %Distance - ABS(Y - CY) =< X - CX
  %Distance - ABS(Y - CY) =< CX - X,
  MaxDx is Distance - abs(Y - CY),
  MaxDx >= 0,
  StartX is X - MaxDx,
  EndX is X + MaxDx,
  Expr = [[StartX, EndX]|Expr1],
  line_covered(Xs, CY, Expr1),
  !.
line_covered([s(X, Y, BX, BY)|Xs], CY, Expr) :-
  Distance is abs(X - BX) + abs(Y - BY),
  %Distance =< abs(X - CX) + abs(Y - CY)
  %Distance - ABS(Y - CY) =< abs(X - CX)
  %Distance - ABS(Y - CY) =< X - CX
  %Distance - ABS(Y - CY) =< CX - X,
  MaxDx is Distance - abs(Y - CY),
  MaxDx < 0,
  line_covered(Xs, CY, Expr),
  !.

union_ranges_([], []).
union_ranges_([X], [X]).
union_ranges_([[X1, X2], [X1, X2]|Xs], R) :-
  % Identical ranges
  union_ranges_([[X1, X2]|Xs], R),
  !.

union_ranges_([[X1, X2], [X1, X4]|Xs], R) :-
  % Start with same value, end with different value
  union_ranges_([[X1, X4]|Xs], R),
  !.

union_ranges_([[X1, X2], [X3, X4]|Xs], R) :-
  X1 =< X3,
  X2 >= X4,
  union_ranges_([[X1, X2]|Xs], R),
  !.
union_ranges_([[X1, X2], [X3, X4]|Xs], R) :-
  X22 is X2 + 1,
  X22 >= X3,
  union_ranges_([[X1, X4]|Xs], R),
  !.
union_ranges_([[X1, X2], [X3, X4]|Xs], [[X1, X2]|R]) :-
  union_ranges_([[X3, X4]|Xs], R),
  !.
union_ranges(X, R) :-
  sort(X, X1),
  union_ranges_(X1, R).

remove_point(X, [], []).
remove_point(X, [[X1, X2]|Rs], Result) :-
  % Point is before range. Ignore.
  X < X1,
  remove_point(X, Rs, Result1),
  Result = [[X1, X2]|Result1].
remove_point(X, [[X, X2]|Rs], Result) :-
  % Point is at start of range
  remove_point(X, Rs, Result1),
  X1 is X + 1,
  Result = [[X1, X2]|Result1].
remove_point(X, [[X1, X2]|Rs], Result) :-
  % Point is in the middle of range
  X > X1,
  X < X2,
  remove_point(X, Rs, Result1),
  X12 is X - 1,
  X13 is X + 1,
  Result = [[X1, X12],[X13, X2]|Result1].
remove_point(X, [[X1, X]|Rs], Result) :-
  % Point is at end of range
  remove_point(X, Rs, Result1),
  X2 is X - 1,
  Result = [[X1, X2]|Result1].
remove_point(X, [[X1, X2]|Rs], Result) :-
  % Point is after range. Ignore.
  X > X2,
  remove_point(X, Rs, Result1),
  Result = [[X1, X2]|Result1].

remove_sensor(s(X, Y, _, _), Y, Ranges, Ranges1) :-
  remove_point(X, Ranges, Ranges1).
remove_sensor(s(X, Y, _, _), Y1, Ranges, Ranges) :-
  Y \= Y1.
remove_beacon(s(_, _, X, Y), Y, Ranges, Ranges1) :-
  remove_point(X, Ranges, Ranges1).
remove_beacon(s(_, _, X, Y), Y1, Ranges, Ranges) :-
  Y \= Y1.

remove_sensors_beacons([], Y, Ranges, Ranges).
remove_sensors_beacons([S|Xs], CY, Ranges, Ranges1) :-
  remove_sensor(S, CY, Ranges, Ranges2),
  remove_beacon(S, CY, Ranges2, Ranges3),
  remove_sensors_beacons(Xs, CY, Ranges3, Ranges1).

count_range([], 0).
count_range([[X1, X2]|Xs], R) :-
  X is X2 - X1 + 1,
  count_range(Xs, R1),
  R is X + R1.
  
solution1(Sol1) :-
  phrase_from_file(sensors(L), '15.input'),
  line_covered(L, 2000000, E),
  sort(E, E1),
  union_ranges(E1, E2),
  remove_sensors_beacons(L, 2000000, E2, E3),
  count_range(E3, Sol1).

calc_line(Sensors, Y, Ranges) :-
  line_covered(Sensors, Y, E1),
  sort(E1, E2),
  union_ranges(E2, Ranges).

is_hole(Sensors, Y, Ranges, T) :-
  calc_line(Sensors, Y, Ranges),
  if_(
    [R1,R2] = Ranges,
    T = true,
    T = false
  ).
  

find_hole(Sensors, Y, not_found) :- Y > 4000000, !.
find_hole(Sensors, Y, Res) :-
  Y1 is Y mod 1000,
  Percent is Y * 100 / 4000000,
  if_(
    Y1 = 0,
    (write(Y), write('/'), write(4000000), write(' '), write(Percent), write('\n')),
    true
  ),
  Y2 is Y + 1,
  if_(
    is_hole(Sensors, Y, Ranges),
    Res = [Y, Ranges],
    find_hole(Sensors, Y2, Res)
  ).


  

solution2(Y, Sol2) :-
  phrase_from_file(sensors(L), '15.input'),
  find_hole(L, Y, Res),
  Sol2 = Res.

border(s(X, Y, BX, BY), CX, CY) :-
  Distance #= abs(BX - X) + abs(BY - Y),
  Distance #= abs(CX - X) + abs(CY - Y).
border1(s(X, Y, BX, BY), CX, CY) :-
  Distance #= abs(BX - X) + abs(BY - Y),
  Distance #= abs(CX - X) + abs(CY - Y) - 1.

outside(s(X, Y, BX, BY), CX, CY) :-
  Distance #= abs(BX - X) + abs(BY - Y),
  Distance #< abs(CX - X) + abs(CY - Y).

outsidemany([], _, _).
outsidemany([X|Xs], CX, CY) :-
  outside(X, CX, CY),
  outsidemany(Xs, CX, CY).

%find_hole2([], not_found).
find_hole2(Xs, R) :-
  select(X1, Xs, Xs1),
  select(X2, Xs1, Xs2),
  select(X3, Xs2, Xs3),
  CX #>= 0,
  CX #=< 4000000,
  CY #>= 0,
  CY #=< 4000000,
  border1(X1, CX, CY),
  border1(X2, CX, CY),
  border1(X3, CX, CY),
  %outsidemany([R1], CX, CY),
  %write(1),
  label([CX, CY]),
  %write(2),
  R = [CX, CY].


solution22(Sol2) :-
  phrase_from_file(sensors(L), '15.input'),
  find_hole2(L, Sol2).
