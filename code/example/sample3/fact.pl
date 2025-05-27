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

play(divyansh, basketball).
play(divisha, badminton).
play(mahipal, badminton).
play(rajesh, "pickle ball").

like2(divyansh, mythri).
like2(rajesh, nidhi).
like2(nidhi, rajesh).

course_after(ecs36b, ecs36a).
course_after(ecs36c, ecs36b).
course_after(ecs50, ecs36b).