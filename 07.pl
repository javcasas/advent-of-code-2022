:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(clpz)).
:- use_module(library(between)).

line([]) --> "\n".
line([X|Xs]) --> [X], {X \= '\n'}, line(Xs).

lineToCommand("ls", ls).
lineToCommand(CdCommand, Command) :-
  append("cd ", Dir, CdCommand),
  if_(
    Dir = "/",
    Command = cd_root,
    if_(
      Dir = "..",
      Command = cd_parent,
      Command = cd(Dir)
    )
  ).

command(X) --> "$ ", line(X1), {lineToCommand(X1, X)}.

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

commandResponse(dir(Name)) --> "dir ", line(Name).
commandResponse(file(Name, Size)) --> digits(Size), " ", line(Name).

responses([]) --> [].
responses([X|Xs]) --> commandResponse(X), responses(Xs).

parse([]) --> "\n".
parse([c(Req, Res)|Xs]) --> command(Req), responses(Res), parse(Xs).

commands_dirs([], _, []).
commands_dirs([c(cd_root, [])|Rc], _, Rt) :-
  commands_dirs(Rc, ["/"], Rt).

commands_dirs([c(cd_parent, [])|Rc], CWD, Rt) :-
  append(CWD2, [_], CWD),
  commands_dirs(Rc, CWD2, Rt).

commands_dirs([c(cd(Dir), [])|Rc], CWD, Rt) :-
  append(CWD, [Dir], CWD2),
  commands_dirs(Rc, CWD2, Rt).

commands_dirs([c(ls, Content)|Rc], CWD, [R|Rt]) :-
  R = dir(CWD, Content),
  commands_dirs(Rc, CWD, Rt).

% find_entry(Lookup, Path, Entry)
find_entry([dir(Y, Content)|L], X, FoundContent) :-
  if_(
    X = Y,
    FoundContent = Content,
    find_entry(L, X, FoundContent)
  ).

recursive_file_size(_, _, file(_, Size), Size).
recursive_file_size(Lookup, Prefix, dir(Name), TotalSize) :-
  append(Prefix, [Name], FullPath),
  recursive_dir_size(Lookup, FullPath, dir_size(FullPath, TotalSize)).

% dir_size(commands, dir, size)
recursive_dir_size(Lookup, X, dir_size(X, Size)) :-
  find_entry(Lookup, X, Content),
  maplist(recursive_file_size(Lookup, X), Content, Sizes),
  sum_list(Sizes, Size).

nonrecursive_file_size(file(_, Size), Size).
nonrecursive_file_size(dir(_), 0).

% dir_size(commands, dir, size)
nonrecursive_dir_size(Lookup, X, dir_size(X, Size)) :-
  find_entry(Lookup, X, Content),
  maplist(nonrecursive_file_size, Content, Sizes),
  sum_list(Sizes, Size).

dir(dir(X, _), X).

smalldirs([], []).
smalldirs([X|Xs], Smalls) :-
  X = dir_size(_, Size),
  Size =< 100000,
  smalldirs(Xs, Rest),
  Smalls = [X | Rest].
smalldirs([X|Xs], Smalls) :-
  X = dir_size(_, Size),
  Size > 100000,
  smalldirs(Xs, Smalls).

size(dir_size(_, Size), Size).

small_dirs_sum(Dirs, Sum) :-
  maplist(dir, Dirs, DirNames),
  maplist(recursive_dir_size(Dirs), DirNames, DirSizes),
  smalldirs(DirSizes, SmallDirs),
  maplist(size, SmallDirs, Sizes),
  sum_list(Sizes, Sum).

acceptable_size(MinSize, dir_size(_, Size), true) :-
  Size >= MinSize.
acceptable_size(MinSize, dir_size(_, Size), false) :-
  Size < MinSize.

deletable_dirs([], _, []).
deletable_dirs([Dir|Dirs], MinSize, [Dir|DeletableDirs]) :-
  acceptable_size(MinSize, Dir, true),
  deletable_dirs(Dirs, MinSize, DeletableDirs).
deletable_dirs([Dir|Dirs], MinSize, DeletableDirs) :-
  acceptable_size(MinSize, Dir, false),
  deletable_dirs(Dirs, MinSize, DeletableDirs).

size_dir(dir_size(A, B), size_dir(B, A)).
  

deletable_dir(Dirs, MinSize, Result) :-
  maplist(dir, Dirs, DirNames),
  maplist(recursive_dir_size(Dirs), DirNames, DirSizes),
  deletable_dirs(DirSizes, MinSize, Deletable),
  maplist(size_dir, Deletable, DeletableSortable),
  sort(DeletableSortable, Sorted),
  [Result|_] = Sorted.

total_space_used(Dirs, Total) :-
  maplist(dir, Dirs, DirNames),
  maplist(nonrecursive_dir_size(Dirs), DirNames, DirSizes2),
  maplist(size, DirSizes2, AllSizes),
  sum_list(AllSizes, Total).

solution(part1(Sol1), part2(Sol2)) :-
  phrase_from_file(parse(Commands), '07.input'),
  commands_dirs(Commands, _, Dirs),
  total_space_used(Dirs, Used),
  Required is 30000000 - (70000000 - Used),
  deletable_dir(Dirs, Required, DeletableDir),
  size_dir(Sol2, _) = DeletableDir.
