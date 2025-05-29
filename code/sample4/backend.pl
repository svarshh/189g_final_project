/* AND OR chaining given a list of strings.  Connector = or | and */

phrase_list(List, Output, Connector) :-
    phrase(chain(List, Connector), Tokens),
    atomic_list_concat(Tokens, ' ', Output).

chain([], Connector) --> [].
chain([X], Connector) --> [X].
chain([X, Y], Connector) --> [X, Connector, Y].
chain([X | Rest], Connector) -->
    [X, ','],
    chain(Rest, Connector), !.





