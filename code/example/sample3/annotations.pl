generate_predicates([like, like2, play]).

type(special_person, person).
type(food, object).
type(special_food, food).
type(course, object).

arg(parent, [special_person, special_person]).
arg(like, [special_person, special_food]).
arg(like2, [special_person, person]).
arg(play, [special_person, object]).
arg(course_after, [course, course]).

verb(like, ["like", "likes"]).
verb(like2, ["like", "likes"]).
verb(play, ["play", "plays"]).
verb(course_after, ["come after", "comes after"]).

verb(parent, ["parent", "parents"]).

predicate_bucket(like, cannonical).
predicate_bucket(like2, cannonical).
predicate_bucket(play, cannonical).
predicate_bucket(course_after, cannonical).

predicate_bucket(parent, possessive).