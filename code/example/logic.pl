% Facts
person(alice).
person(bob).
person(claire).

food(pizza).
food(sushi).

like(alice, pizza).
like(bob, pizza).
like(bob, sushi).
like(claire, sushi).

% Rule
friend_of(A, B, F) :-
    like(A, F),
    like(B, F),
    A \= B.
