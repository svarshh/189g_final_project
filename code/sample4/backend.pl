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
    setof(Subj,
          List^(verb_relation(Tense, Verb, Subj, List)),
          Sbj), 
    sample(Sbj, 3, Sample),

    findall(S, subset(Sample, S), Subjects).

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
                    collect_subject(X, 'neighbors', Subjects), ! ,
                    member(Subj, Subjects),

                    collect_object(Subj, Objects),

                    union_all(Objects, Obj),

                    
                    length(Subj, L), (L = 1, Collective = 'singular'; L = 0, Collective = 'none'; Collective = 'plural'),
                    phrase_list(Subj, Subject_chain, 'and'),
                    phrase_list(Obj, Object_list, 'and'),

                    intersection_all(Objects, Obj_intersection),
                    phrase_list(Obj_intersection, Obj_intersection_list, 'and'),
                    

(atomic_list_concat([" Q: ", " What all states do " , Subject_chain, " neighbor in total?" ], Q),
atomic_list_concat([" A: ", Object_list, "."], A);

atomic_list_concat([" Q: ", " What all states do " , Subject_chain, " in common?" ], Q),
atomic_list_concat([" A: ", Obj_intersection_list, "."], A)).