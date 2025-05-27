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