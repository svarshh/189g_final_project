generate_predicates([capital, like, car_like, like2, play, play2, parent, course_after, course_before]).

type(special_person, person).
type(food, object).
type(special_food, food).
type(course, object).
type(sport, object).
type(country, location).

arg(like, [special_person, special_food]).
arg(car_like, [special_person, object]).
arg(like2, [special_person, person]).
arg(play, [special_person, object]).
arg(play2, [sport, special_person]).
arg(course_after, [course, course]).
arg(course_before, [course, course]).

arg(parent, [special_person, special_person]).
arg(birthday, [special_person, date]).
arg(capital, [country, object]).

verb(like, ["like", "likes"]).
verb(car_like, ["like", "likes"]).
verb(like2, ["like", "likes"]).
verb(play, ["play", "plays"]).
verb(play2, ["plays", "plays"]).
verb(course_after, ["come after", "comes after"]).
verb(course_before, ["come before", "comes before"]).

verb(parent, ["parent", "parents"]).
verb(birthday, ["birthday", "birthday"]).
verb(capital, ["capital", "capital"]).

modifier(like, ["food", "foods"]).
modifier(car_like, ["car", "cars"]).
modifier(play, ["sport", "sports"]).

predicate_bucket(like, cannonical).
predicate_bucket(like2, cannonical).
predicate_bucket(play, cannonical).
predicate_bucket(play2, cannonical).
predicate_bucket(course_after, cannonical).
predicate_bucket(car_like, cannonical).

predicate_bucket(parent, possessive).
predicate_bucket(birthday, possessive).
predicate_bucket(capital, possessive).