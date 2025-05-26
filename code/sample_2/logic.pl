
% ------------Builtins--------------
% lex
lex(wh, 'who').
lex(wh, 'what').
lex(wh, 'where').
lex(wh, 'when').
lex(wh, 'which').
lex(wh, 'how').

lex(modal, 'can').
lex(modal, 'could').
lex(modal, 'will').
lex(modal, 'would').
lex(modal, 'shall').
lex(modal, 'should').
lex(modal, 'may').
lex(modal, 'might').
lex(modal, 'must').
lex(modal, 'ought_to').

lex(pronoun, 'i').
lex(pronoun, 'you').
lex(pronoun, 'he').
lex(pronoun, 'she').
lex(pronoun, 'it').
lex(pronoun, 'we').
lex(pronoun, 'they').

lex(subject, 'bob').
lex(subject, 'alice').

lex(object, 'pizza').
lex(object, 'food').
lex(object, 'sushi').


% -------------- Inheritance Structure ---------------

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

% ----------------------- auxiliary section ----------------------------

% choose_auxiliary(+Tense, +Person, +Number, -Aux)

% Simple aspect (do-support)
auxiliary(present_simple, third, singular, 'does').
auxiliary(present_simple, _, plural, 'do').
auxiliary(present_simple, first, singular, 'do').
auxiliary(present_simple, second, singular, 'do').
auxiliary(past_simple, _, _, 'did').

% Progressive aspect (be)
auxiliary(present_progressive, first, singular, 'am').
auxiliary(present_progressive, third, singular, 'is').
auxiliary(present_progressive, _, plural, 'are').
auxiliary(past_progressive, third, singular, 'was').
auxiliary(past_progressive, first, singular,  'was').
auxiliary(past_progressive, _, plural,  'were').
auxiliary(past_progressive, second, singular, 'were').

% Perfect aspect (have)
auxiliary(present_perfect, third, singular, 'has').
auxiliary(present_perfect, _, _, 'have').
auxiliary(past_perfect, _, _, 'had').

% Choose the tense of the question verb based on the auxillary used.

% Simple aspect
aux_tense_map(do, base).
aux_tense_map(does, base).
aux_tense_map(did, base).

% Progressive aspect
aux_tense_map(am, present_progressive).
aux_tense_map(is, present_progressive).
aux_tense_map(are, present_progressive).
aux_tense_map(was, past_progressive).
aux_tense_map(were, past_progressive).

% Perfect aspect
aux_tense_map(has, past_participle).
aux_tense_map(have, past_participle).
aux_tense_map(had, past_participle).



% check whether an object is inherited from a super class.  example: check whether bob is a noun.


is_type_of(Instance, SuperType) :-
    type(Type, Instance),
    inherits_or_same(Type, SuperType).

inherits_or_same(Type, Type).
inherits_or_same(SubType, SuperType) :-
    inherits(SubType, Parent),
    inherits_or_same(Parent, SuperType).



% ----------------------------------------------USER-DEFINED---------------------------------------------------------- 

% INHERITANCE

inherits(person, proper_noun).
inherits(place, proper_noun).
inherits(grade, number).
inherits(class, countable_noun).
inherits(prerequisite, abstract_noun).



% TYPE ASSIGNMENTS

type(person, 'Bob').
type(grade, 'A+').
type(grade, 'A').
type(grade, 'B').

type(class, '36a').
type(class, '36b').
type(class, '36c').
type(class, '20').
type(class, '32a').

type(verb, 'take').
type(verb, 'like').

type(person, 'Bob').
type(person, 'Alice').

type(place, 'San Francisco').

type(countable_noun, 'medicine').

% PERSON POV

pov(first, 'user').
pov(third, 'Bob').
pov(third, 'Alice').

% TELLS WHETHER THERE IS A FINITE CHOICE OR NOT

choice(person).
choice(class).
choice(place).

% SINGULAR/PLURAL SPECIFICATION FOR NOUNS

collective(singular, 'Bob').
collective(singular, 'Alice').
collective(singular, 'San Francisco').
collective(plural, prerequisite).

% TENSE DEFINITION FOR ANY VERB

tense('take', _{
    base: take,
    present_simple: takes,
    past_simple: took,
    present_progressive: taking,
    past_progressive: was_taking 
}).

tense('like', _{
    base: like,
    present_simple: likes,
    past_simple: liked,
    present_progressive: liking,
    past_progressive: was_liking  
}).

tense('go', _{
    base: go,
    present_simple: goes,
    past_simple: 'went to',
    present_progressive: going,
    past_progressive: was_going  
}).



% VERB RELATIONS

verb_relation(present_simple, 'like', '36a', 'Bob').
verb_relation(present_simple, 'like', '36a', 'Alice').
verb_relation(past_simple, 'take', 'medicine', 'Bob').
verb_relation(past_simple, 'like', 'Alice', 'Bob').
verb_relation(past_simple, 'go', 'San Francisco', 'Bob').


% CONDITIONAL VERB RELATIONS

conditional_verb_relation('take', X, Y) :- type(class, Y), type(person, X), 
                                        property(Z, prerequisite, Y), 
                                        past_verb_relation('took', X, Z).

% scheduling binary relations

property("20", prerequisites, [[]]).
property("36a", prerequisites, [["32a"],["placement"]]).
property("36b", prerequisites, [["36a"]]).
property("36c", prerequisites, [["36b", "20"]]).



% POSSESSIVE RELATIONS (tense, relation, owner, owned)
possessive_relation(Tense, friend, A, B) :- verb_relation(Tense, 'like', C, A), verb_relation(Tense, 'like', C, B), 
                                            type(class, C), type(person, A), type(person, B), A \= B.


% ------------------------BUILTI-IN------------------------------
lex(subject, X) :- verb_relation(_, _, _, X).

match(X, Y, Z) :- is_type_of(X, 'person'), Y = 'who', Z = Y;
                is_type_of(X, 'noun'), Y = 'which', type(Z1, X), choice(Z1), atomic_list_concat([Y, " ", Z1], Z);
                is_type_of(X, 'noun'), type(Z1, X),  \+ choice(Z1), Y = 'what', Z=Y;
                is_type_of(X, 'place'), Y = 'where', Z=Y.

match_possessive_tense(X, Y, Z) :-  Y = 'present_simple', possessive_relation(Y, X, _, _), Z = 'is'; 
                                    Y = 'present_progressive', possessive_relation(Y, X, _, _), Z = 'is';
                                    Y = 'past_simple', possessive_relation(Y, X, _, _), Z = 'was'; 
                                    Y = 'past_progressive', possessive_relation(Y, X, _, _), Z = 'was'.

             

% QUESTION ANSWER CONSTRAINTS.  BUILT-IN
% wh + (OBJ) + aux + subj + verb 
% wh + aux/verb + possessive_noun + noun?
% how + modifier + aux + subj + (verb )?
question(Q, A) :-  
                lex(wh, Wh), 
                verb_relation(Tense, Verb, Obj, Subj), 
                match(Obj, Wh, W), 
                pov(Pov, Subj), 
                collective(Collective, Subj), 
                auxiliary(Tense, Pov, Collective, Aux), 
                aux_tense_map(Aux, Question_tense), 
                tense(Verb, V), 
                get_dict(Question_tense, V, Verb_final),
                get_dict(Tense, V, Ans_verb), 

                atomic_list_concat([" Q: ", W, " ", Aux, " " , Subj, " ", Verb_final, "? "], Q),
                atomic_list_concat([" A: ", Subj, " ", Ans_verb, " ", Obj,"."], A);

                lex(wh, Wh), 
                possessive_relation(Tense, Relation, PN, Noun), 
                match(Noun, Wh, W),
                match_possessive_tense(Relation, Tense, Aux), 

                atomic_list_concat([" Q: ", W, " ", Aux, " " , PN, "'s ", Relation, "? "], Q),
                atomic_list_concat([" A: ", PN, "'s ", Relation, " " , Aux, " ", Noun, ". "], A).



prompt :-
    setof((Q, A), question(Q, A), Prompts), writeln(Prompts).
    








