

/*lex*/
lex(wh, 'who').
lex(wh, 'what').
lex(wh, 'where').
lex(wh, 'when').
lex(wh, 'which').
lex(wh, 'how').

/*Base type checker*/

object_inherits(Object, SuperType) :-
    type(Type, Object),
    inherits_or_same(Type, SuperType).



inherits_or_same(Type, Type).
inherits_or_same(Type, SuperType) :-
    inherits(Type, Parent),
    inherits_or_same(Parent, SuperType).

    
/* Match wh with tense and object */
match(Collectivity, Objects, Z ) :- 
                Objects = [X | _],
                lex(wh, Y),
                (
                object_inherits(X, 'person'), Y = 'who', Z = Y;
                object_inherits(X, 'noun'), Y = 'which', type(Z1, X), categorical(Z1), (Collectivity \= 'singular',  plural_form(Z1, Z2); Z2=Z1), atomic_list_concat([Y, " ", Z2], Z);
                object_inherits(X, 'noun'), type(Z1, X),  \+ categorical(Z1), Y = 'what', Z=Y
                ).

/*match the possessive tense with the aux*/

match_possessive_tense(X, Y, Z) :-  Y = 'present_simple', possessive_relation(Y, X, _, _), Z = 'is'; 
                                    Y = 'present_progressive', possessive_relation(Y, X, _, _), Z = 'is';
                                    Y = 'past_simple', possessive_relation(Y, X, _, _), Z = 'was'; 
                                    Y = 'past_progressive', possessive_relation(Y, X, _, _), Z = 'was'.


/* ----------------------- auxiliary section ----------------------------*/

/* choose_auxiliary(+Tense, +Person, +Number, -Aux)*/

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

% Passive voice (be-support)
auxiliary(present_simple_passive, third, singular, 'is').
auxiliary(present_simple_passive, _, plural, 'are').
auxiliary(present_simple_passive, first, singular, 'am').
auxiliary(present_simple_passive, second, singular, 'are').


/* Choose the tense of the question verb based on the auxillary used. */

% Simple aspect (do-support)
aux_tense_map(do, base).
aux_tense_map(does, base).
aux_tense_map(did, base).

% Progressive aspect (be-support + present participle)
aux_tense_map(am, present_progressive).
aux_tense_map(is, present_progressive).
aux_tense_map(are, present_progressive).
aux_tense_map(was, past_progressive).
aux_tense_map(were, past_progressive).

% Perfect aspect (have-support + past participle)
aux_tense_map(has, past_participle).
aux_tense_map(have, past_participle).
aux_tense_map(had, past_participle).

% Passive voice (be-support + past participle)
aux_tense_map(is, past_participle).
aux_tense_map(are, past_participle).
aux_tense_map(am, past_participle).
aux_tense_map(was, past_participle).
aux_tense_map(were, past_participle).






