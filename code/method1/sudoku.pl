:- use_module(library(clpfd)).
:- use_module(library(random)).

sudoku(Rows) :-
    length(Rows, 9),
    maplist(same_length(Rows), Rows),    % Ensure 9x9 grid
    append(Rows, Vars),                  % Flatten the board
    Vars ins 1..9,                       % Each cell must be 1..9

    % Rows must have all different values
    maplist(all_distinct, Rows),

    % Columns must have all different values
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),

    % 3x3 blocks must have all different values
    Rows = [A,B,C,D,E,F,G,H,I],
    blocks(A,B,C),
    blocks(D,E,F),
    blocks(G,H,I),

    label(Vars).  % Try assigning values (solving)

unique_solution(Puzzle) :-
    findall(Solution, sudoku(Solution), Solutions),
    length(Solutions, 1).


gensudoku_puzzle(Full, Puzzle) :-
    gensudoku(Full),              % Generate full valid board
    hide_some(Full, Puzzle).

% Generate a valid full Sudoku solution
gensudoku(Board) :-
    length(Board, 9),
    maplist(same_length(Board), Board),
    append(Board, Vars),
    Vars ins 1..9,

    maplist(all_distinct, Board),         % Rows
    transpose(Board, Columns),
    maplist(all_distinct, Columns),       % Columns

    Board = [A,B,C,D,E,F,G,H,I],
    blocks(A, B, C),
    blocks(D, E, F),
    blocks(G, H, I),

    label(Vars).

% 3x3 subgrid constraints
blocks([], [], []).
blocks([A1,A2,A3|T1], [B1,B2,B3|T2], [C1,C2,C3|T3]) :-
    all_distinct([A1,A2,A3,B1,B2,B3,C1,C2,C3]),
    blocks(T1, T2, T3).

% Randomly blank out some cells (about 40) from the full board
hide_some(Full, Puzzle) :-
    maplist(maplist(maybe_blank), Full, Puzzle).

maybe_blank(Value, Cell) :-
    random(0.0, 1.0, R),
    ( R < 0.5 -> Cell = Value ; Cell = _ ).

print_sudoku(Board) :-
    print_rows(Board, 0).

print_rows([], _).
print_rows([Row | Rest], Index) :-
    (Index mod 3 =:= 0 -> writeln('+-------+-------+-------+') ; true),
    print_row(Row, 0), nl,
    Index1 is Index + 1,
    print_rows(Rest, Index1),
    (Index1 =:= 9 -> writeln('+-------+-------+-------+') ; true).

print_row([], _).
print_row([Cell | Rest], Index) :-
    (Index mod 3 =:= 0 -> write('| ') ; true),
    print_cell(Cell),
    write(' '),
    Index1 is Index + 1,
    print_row(Rest, Index1).

print_cell(Cell) :-
    ( var(Cell) -> write('.') ; write(Cell) ).


