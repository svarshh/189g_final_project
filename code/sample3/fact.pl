:- consult("geobase.pl").

/* Toy Example Facts and Predicates */
/* Base Type Instances */
special_person(nidhi).
special_person(rajesh).
special_person(divyansh).
special_person(mahipal).
special_person(asha).
special_person(divisha).
special_person(bob).

food(pizza).
food(burger).
food(roti).
food(dosa).

course(ecs36a).
course(ecs36b).
course(ecs36c).
course(ecs50).

sport(basketball).

country("India").

/* Parent Relations */
parent(bob, alice).

parent(divyansh, nidhi).
parent(divyansh, rajesh).
parent(divisha, mahipal).
parent(divisha, asha).


/* Like Relations */
like(rajesh, dosa).
like(nidhi, roti).
like(divyansh, pizza).

like(mahipal, dosa).
like(asha, roti).
like(divisha, pizza).

car_like(divyansh, ferrari).

play(divyansh, basketball).
play(divyansh, badminton).
play(divyansh, "pickle ball").
play(divisha, badminton).
play(mahipal, badminton).
play(rajesh, "pickle ball").

play2(basketball, divyansh).
play2(basketball, divisha).

like2(divyansh, mythri).
like2(rajesh, nidhi).
like2(rajesh, divyansh).
like2(nidhi, rajesh).

course_after(ecs36b, ecs36a).
course_after(ecs36c, ecs36b).
course_after(ecs36c, ecs50).

course_before(ecs36a, ecs36b).
course_before(ecs36b, ecs36c).
course_before(ecs50, ecs36c).

birthday(divyansh, "July 9th").
birthday(nidhi, "July 9th").

capital("India", "New Delhi").

/* Geobase Example Facts and Predicates */
myriver(River) :- river(River, _, _).
mystate(State) :- state(State, _, _, _, _, _, _, _, _, _).

population(State, Population) :- state(State, _, _, Population, _, _, _, _, _, _).
abbrev(State, Abbrev) :- state(State, Abbrev,_, _, _, _, _, _, _, _).
myborder(State, States) :- border(State, _, BorderStates), member(States, BorderStates).

neighbors(State, Neighbors) :-
    border(State, _, Neighbors).

% notmyborder(State, States, NegState):
% States is the list of states that border State but do NOT border NegState.
notmyborder(State, NegState, States) :-
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

% notmyborder2(+BorderStates, +NegState, -States)
notmyborder2(BorderStates, NegState, States) :-
    % Get the borders of each state in BorderStates
    findall(Bs, (member(S, BorderStates), borders_of(S, Bs)), BorderLists),
    intersect_all(BorderLists, Common),
    borders_of(NegState, NegBorders),
    subtract(Common, NegBorders, States).


riverflow(River, States) :-
    river(River, _, StatesList), member(States, StatesList).
