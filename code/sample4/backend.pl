:- consult("builtins.pl").
:- consult("annotations.pl").
:- use_module(library(random)).
:- use_module(library(lists)).

/* AND OR chaining given a list of strings.  Connector = or | and */

:- dynamic seen_subset/1.

phrase_list(List, Output, Connector) :-
    phrase(chain(List, Connector), Tokens),
    atomic_list_concat(Tokens, ' ', Output), !.

chain([], _) --> [].
chain([X], _) --> [X].
chain([X, Y], Connector) --> [X, Connector, Y].
chain([X | Rest], Connector) -->
    [X, ','],
    chain(Rest, Connector), !.



combination(0, _, []) :- !.
combination(K, [H|T], [H|Comb]) :-
    K > 0,
    K1 is K - 1,
    combination(K1, T, Comb).
combination(K, [_|T], Comb) :-
    K > 0,
    combination(K, T, Comb).
    
/* Random sample N subjects */
sample(Subj, N, Sample) :-
    length(Subj, Len),
    (Len < N -> Sample = Subj ; 
      random_permutation(Subj, Shuffled),
      length(Sample, N),
      append(Sample, _, Shuffled)
    ).

/*Subsets of a list*/
subset([], []).
subset([H|T], [H|Subset]) :- subset(T, Subset).
subset([_|T], Subset) :- subset(T, Subset).

/*Subject collector*/
collect_subject(Tense, Verb, Subjects) :-
    setof(Subj, List^(verb_relation(Tense, Verb, Subj, List)), Sbj),
    combination(3, Sbj, Sample),
    findall(SortedSubset,
            (
                subset(Sample, Subset), Subset \= [], length(Subset, L), L > 2,
                sort(Subset, SortedSubset)  /* sort subsets to avoid permutation duplicates */
            ),
            AllSubsets),
    list_to_set(AllSubsets, Subjects).

collect_PN(Relation, Tense, PNs):-

    setof(Subj, Object^(possessive_relation(Tense, Relation, Subj, Object)), Sbj),
    combination(3, Sbj, Sample),
    findall(SortedSubset,
            (
                subset(Sample, Subset), Subset \= [], length(Subset, L), L > 2,
                sort(Subset, SortedSubset) 
            ),
            AllSubsets),
    list_to_set(AllSubsets, PNs), 
    !.


/*Union all lists in a list*/
union_all([], []).
union_all([L|Ls], Union) :-
    union_all(Ls, U1),
    append(L, U1, All),
    sort(All, Union).

/*Intersect all lists in a list*/
intersection_all([], []).
intersection_all([L], L) :- !.
intersection_all([L1, L2 | Ls], Result) :-
    intersection(L1, L2, L12),
    intersection_all([L12 | Ls], Result).

collect_object(Subjects, ObjectsList) :-
    findall(Object, 
        ( member(Subject, Subjects),
          verb_relation(_, _, Subject, Object)
        ),
        ObjectsList).


/*Present simple passive, place question*/
verb_question(Q, A) :-
    (Place = 'place'),
    /* Collect all the tense|verb combos */
    setof([T, V], S^O^verb_relation(T, V, S, O), Pairs), 
    member([Tense, Verb], Pairs),
    /* Collect Subjects and verbs based on the current tense|verb combo */ 
    collect_subject(Tense, Verb, Subjects),
    member(Subj, Subjects),
    /* Sort to standardize subsets for seen check */
    sort(Subj, SortedSubj),
    (   seen_subset(SortedSubj)
    ->  fail  /* already generated this subset question, skip */
    ;   assertz(seen_subset(SortedSubj))
    ),
    collect_object(Subj, Objects),
    

    (

    (
        /* OR all*/
    /* Whether subjects and objects are singular | plural | none */
    union_all(Objects, Obj),

    phrase_list(Subj, Subject_chain, 'or'),
    phrase_list(Obj, Object_list, 'and'),


    length(Subj, L), (L = 1 -> Collective_subj = singular ; L = 0 -> Collective_subj = none ; Collective_subj = plural),
    length(Obj, L2), (L2 = 1 -> Collective_obj = singular ; L2 = 0 -> Collective_obj = none ; Collective_obj = plural),

    match(Collective_obj, Obj, Wh),
    auxiliary(Tense, 'third', Collective_subj, Aux), 
    

    atomic_list_concat(["Q: ",Wh, " ", Aux, " ", Subject_chain, " ", Verb,"?"], Q),
    ((Collective_obj = none, atomic_list_concat(["A: None"], A)); (Collective_obj \= none, atomic_list_concat(["A: ", Object_list, "."], A)))
    )
    ;


    (
        /* AND all*/
    intersection_all(Objects, Obj),

    phrase_list(Subj, Subject_chain, 'and'),
    phrase_list(Obj, Object_list, 'and'),

    length(Subj, L), (L = 1 -> Collective_subj = singular ; L = 0 -> Collective_subj = none ; Collective_subj = plural),
    length(Obj, L2), (L2 = 1 -> Collective_obj = singular ; L2 = 0 -> Collective_obj = none ; Collective_obj = plural),

    match(Collective_obj, Obj, Wh),
    auxiliary(Tense, 'third', Collective_subj, Aux), 


    atomic_list_concat(["Q: ", Wh, " ", Aux, " ", Subject_chain, " ", Verb," in common?"], Q),
    ((Collective_obj = none, atomic_list_concat(["A: None"], A)); (Collective_obj \= none, atomic_list_concat(["A: ", Object_list, "."], A)))
    )).

possessive_question(Q, A) :-

    Place = 'place',

    plural_possessive_relation(Tense, Relation, _, _), 
    
    plural_possessive(Single_relation, Relation), 

    collect_PN(Single_relation, Tense, PNs),

    member(PN, PNs),

    plural_possessive_relation(Tense, Relation, PN, Objects),

    collective(Relation, Collective), 
    match(Collective, [Objects], Wh),
    Aux = is,

    phrase_list(PN, PN_chain, 'and'),

    atomic_list_concat([" Q: ", Wh, " ", Aux, " " , PN_chain, "'s ", Relation, "? "], Q),
    atomic_list_concat([" A: ", PN_chain, "'s ", Relation, " " , Aux, " ", Objects, ". "], A).



print_n_questions(Limit) :-
    findnsols(Limit, verb_question(Q, A), verb_question(Q, A), Questions),
    print_questions(Questions, 1).

print_questions([], _).
print_questions([verb_question(Q, A)|Rest], N) :-
    format("~w~n", [ Q]),
    format("~w~n~n", [A]),
    N1 is N + 1,
    print_questions(Rest, N1).



