% inheritance structure

% inherit(noun, root).
% inherit(verb, root).
% inherit(pronoun, root).
% inherit(proper_noun, noun).
% inherit(person, proper_noun).
% inhreit(things, noun).
% inherit(food, things).


% facts and rules

% Facts, unary type relations

type(person, alice).
type(person, bob).
type(person, claire).

type(food, pizza).
type(food, sushi).
type(verb, like).

% binary verb relations (subj obj pairs)

verb_relation(like, alice, pizza).
verb_relation(like, bob, pizza).
verb_relation(like, bob, sushi).
verb_relation(like, claire, sushi).
verb_relation(like, claire, bob).

% Rule
% B is friend of A
possessive_relation(friend, A, B) :- verb_relation(like, A, F), verb_relation(like, B, F), type(food, F), A \= B.

% builtin natural language rules
who(X, Y) :- type(person, X), verb_relation(_, Y, X).
what(X, Y) :- type(food, X), verb_relation(_, Y, X).