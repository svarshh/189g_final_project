:- consult("geobase.pl").

/* Geobase Example Facts and Predicates */
riverformat(River, FormatRiver) :- river(River, _, _), format(string(FormatRiver), "the ~w river", [River]).

myriver(River) :- river(RiverName, _, _), riverformat(RiverName, River).
mystate(State) :- state(State, _, _, _, _, _, _, _, _, _).

population(State, Population) :- state(State, _, _, Population, _, _, _, _, _, _).
abbrev(State, Abbrev) :- state(State, Abbrev,_, _, _, _, _, _, _, _).
simple_border(State, States) :- border(State, _, BorderStates), member(States, BorderStates).

neighbors(State, Neighbors) :-
    border(State, _, Neighbors).

% notsimple_border(State, States, NegState):
% States is the list of states that border State but do NOT border NegState.
notsimple_border(State, NegState, States) :-
    neighbors(State, StateNeighbors),
    neighbors(NegState, NegNeighbors),
    subtract(StateNeighbors, NegNeighbors, States).

% Helper to get border list from a state
borders_of(State, Borders) :-
    border(State, _, Borders).

% Helper to intersect a list of lists
intersect_all([L], L).
intersect_all([L1, L2 | Rest], Result) :-
    intersection(L1, L2, Temp),
    intersect_all([Temp | Rest], Result).

% complex_logic_border(+BorderStates, +NegState, -States)
complex_logic_border(BorderStates, NegState, States) :-
    % Get the borders of each state in BorderStates
    findall(Bs, (member(S, BorderStates), borders_of(S, Bs)), BorderLists),
    intersect_all(BorderLists, Common),
    borders_of(NegState, NegBorders),
    subtract(Common, NegBorders, States).

riverflow(FormatRiver, States) :-
    riverformat(River, FormatRiver),
    river(River, _, StatesList), member(States, StatesList).

stateriverflow(State, FormatRiver) :-
    setof(FormatRiver,
        River^StatesList^(
            river(River, _, StatesList),
            member(State, StatesList),
            riverformat(River, FormatRiver)
        ),
        FormatRivers),
    member(FormatRiver, FormatRivers).

statemajorcity(State, MajorCity) :-
    state(State, _, _, _, _, _,City1, City2, City3, City4), MajorCities = [City1, City2, City3, City4], member(MajorCity, MajorCities).


statecapital(State, Capital) :-
    state(State, _, Capital, _, _, _, _, _, _, _).