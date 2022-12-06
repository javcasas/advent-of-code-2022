:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).

string_different(S) :-
  maplist(char_code, S, Sc),
  all_different(Sc).
  
string_different(A, B, C, D) :-
  char_code(A, Ac),
  char_code(B, Bc),
  char_code(C, Cc),
  char_code(D, Dc),
  all_different([Ac, Bc, Cc, Dc]).
  
header(0) --> [].
header(L) --> [_], header(L2), {L is L2 + 1}.
start_marker(P) --> header(P), {length(S, 4)}, S, {string_different(S)}, seq(_).
start_packet(P) --> header(P), {length(S, 14)}, S, {string_different(S)}, seq(_).

start_packet4(S, L, X) :-
  length(S, Sl),
  MaxPrefixLength is Sl - L,
  between(0, MaxPrefixLength, PrefixLength),
  SuffixLength is Sl - PrefixLength - L,
  length(Prefix, PrefixLength),
  length(Suffix, SuffixLength),
  length(Packet, L),
  append(Prefix, Packet, PrefixPacket),
  append(PrefixPacket, Suffix, S),
  string_different(Packet),
  X #= PrefixLength + L.

start_packet3(S, L, X) :-
  length(S, Sl),
  PrefixLength #>= 0,
  SuffixLength #< Sl,
  length(Prefix, PrefixLength),
  length(Suffix, SuffixLength),
  length(Packet, L),
  append(Prefix, Packet, PrefixPacket),
  append(PrefixPacket, Suffix, S),
  string_different(Packet),
  X #= PrefixLength + L.

start_packet2(S, L, P) :-
  length(S2, L),
  append(S1, S2, S12),
  append(S12, S3, S),
  string_different(S2),
  length(S1, Prefix),
  P is Prefix + L.
  
solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(seq(X), '06.input'),
  start_packet4(X, 4, Sol1),
  start_packet4(X, 14, Sol2).
