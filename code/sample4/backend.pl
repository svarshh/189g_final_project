:- consult("builtins.pl").
:- consult("annotations.pl").
:- use_module(library(random)).
:- use_module(library(lists)).

/* AND OR chaining given a list of strings.  Connector = or | and */

phrase_list(List, Output, Connector) :-
    phrase(chain(List, Connector), Tokens),
    atomic_list_concat(Tokens, ' ', Output), !.

chain([], Connector) --> [].
chain([X], Connector) --> [X].
chain([X, Y], Connector) --> [X, Connector, Y].
chain([X | Rest], Connector) -->
    [X, ','],
    chain(Rest, Connector), !.

/* Random sample N subjects */
sample(Subj, N, Sample) :-
    length(Subj, Len),
    (Len < N -> Sample = Subj ; 
      random_permutation(Subj, Shuffled),
      length(Sample, N),
      append(Sample, _, Shuffled)
    ).

/*Subsets of a list*/

subset([H|T], [H|Subset]) :- subset(T, Subset).
subset([_|T], Subset) :- subset(T, Subset).
subset([H|_], [H]).

/*Subject collector*/
collect_subject(Tense, Verb, Subjects) :-
    once((
        setof(Subj, List^(verb_relation(Tense, Verb, Subj, List)), Sbj),
        sample(Sbj, 3, Sample),
        findall(S, subset(Sample, S), Subjects)
    )).

/*Union all lists in a list*/

union_all([], []).
union_all([L|Ls], Union) :-
    union_all(Ls, U1),
    append(L, U1, All),
    sort(All, Union).

/*Intersect all lists in a list*/
intersection_all([], []).
intersection_all([L], L).
intersection_all([L1, L2 | Ls], Result) :-
    intersection(L1, L2, L12),
    intersection_all([L12 | Ls], Result).

collect_object(Subjects, Objects) :-
    setof(Y, Y^X^(member(X, Subjects), verb_relation(_, _, X, Y)), Objects).


/*Present simple passive, place question*/

question(Q, A) :-
                    is_type_of(Place, 'place'),
                    wh('place', W),

                    setof([T, V], S^O^verb_relation(T, V, S, O), Pairs), !,
                    member([Tense, Verb], Pairs),

                    collect_subject(Tense, Verb, Subjects), 
                    member(Subj, Subjects),

                    collect_object(Subj, Objects),
                    union_all(Objects, Obj),

                    /* Whether subjects and objects are singular | plural | none */
                    length(Subj, L), (L = 1, Collective_subj = 'singular'; L = 0, Collective_subj = 'none'; Collective_subj = 'plural'),
                    length(Obj, L2), (L2 = 1, Collective_obj = 'singular'; L2 = 0, Collective_obj = 'none'; Collective_obj = 'plural'),

                    phrase_list(Subj, Subject_chain, 'and'),
                    phrase_list(Obj, Object_list, 'and'),

                    intersection_all(Objects, Obj_intersection),
                    phrase_list(Obj_intersection, Obj_intersection_list, 'and'),
                    

(atomic_list_concat([" What all states do " , Subject_chain, " neighbor in total?" ], Q),
atomic_list_concat([ Object_list, "."], A);

atomic_list_concat([" What all states do " , Subject_chain, " in common?" ], Q),
atomic_list_concat([ Obj_intersection_list, "."], A)).

print_n_questions(Limit) :-
    findnsols(Limit, question(Q, A), question(Q, A), Questions),
    print_questions(Questions, 1).

print_questions([], _).
print_questions([question(Q, A)|Rest], N) :-
    format("~w) Q: ~w~n", [N, Q]),
    format("   A: ~w~n~n", [A]),
    N1 is N + 1,
    print_questions(Rest, N1).