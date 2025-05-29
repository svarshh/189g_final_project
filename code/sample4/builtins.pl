/* WH Words */
wh('person', 'who').
wh('noun', 'what').
wh('place', 'where').
wh('time', 'when').
wh('noun', 'which').

/* Inheritance Structure */

inherits(common_noun, noun).
inherits(concrete_noun, common_noun).
inherits(countable_noun, concrete_noun).
inherits(uncountable_noun, concrete_noun).
inherits(abstract_noun, common_noun).
inherits(countable_abstract, abstract_noun).
inherits(uncountable_abstract, abstract_noun).

inherits(proper_noun, noun).
inherits(person_name, proper_noun).
inherits(place_name, proper_noun).
inherits(organization_name, proper_noun).
inherits(specific_title_event, proper_noun).

inherits(collective_noun, noun).
inherits(compound_noun, noun).
inherits(gerund, noun).
inherits(possessive_noun, noun).

inherits(number, concrete_noun).


/* Check base type of an instance*/

is_type_of(Instance, SuperType) :-
    type(Type, Instance),
    inherits_or_same(Type, SuperType).

inherits_or_same(Type, Type).
inherits_or_same(SubType, SuperType) :-
    inherits(SubType, Parent),
    inherits_or_same(Parent, SuperType).


