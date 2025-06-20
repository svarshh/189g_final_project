:- consult("fact.pl").
:- consult("annotations.pl").
:- initialization(main).

/* Resolve Type Predicate */
resolve_type(person, person).
resolve_type(location, location).
resolve_type(date, date).
resolve_type(number, number).
resolve_type(object, object).

resolve_type(InputType, OutputType) :- 
    type(InputType, NextLevelType), resolve_type(NextLevelType, OutputType).

/* Resolve Predicate Type Predicates */
resolve_base_type_of_list([], []).

resolve_base_type_of_list(Types, Result) :-
    [H | T] = Types, resolve_base_type_of_list(T, RestOfBase), 
    resolve_type(H, BaseOfHead), append([BaseOfHead], RestOfBase, Result).

resolve_predicate_type_info(Predicate, Types, BaseTypes, VerbInfo) :-
    arg(Predicate, Types), resolve_base_type_of_list(Types, BaseTypes),
    verb(Predicate, VerbInfo).

/* Wh-word mapping facts */
wh_word(person, "Who").
wh_word(location, "Where").
wh_word(date, "When").
wh_word(number, "How many").
wh_word(object, "What").

/* Aux Verb mapping predicate */
aux_word(_, AuxWord) :- AuxWord = "does".

possessive_aux_word(Subjects, Aux) :-
    length(Subjects, SubjectsLen),
    ( SubjectsLen =:= 1 -> Aux = "is";
      Aux = "are"
    ).

/* Verb predicates */
get_singular_verb(VerbInfo, SingularVerb) :- [SingularVerb | _] = VerbInfo.
get_plural_verb(VerbInfo, PluralVerb) :- [_ | [PluralVerb | _]] = VerbInfo.

get_singular_modifier(Modifier, SingularModifier) :- [SingularModifier | _] = Modifier.
get_plural_modifier(Modifier, PluralModifier) :- [_ | [PluralModifier | _]] = Modifier.

oxford_comma_list([One], One) :- !.
oxford_comma_list([One, Two], Joined) :-
    !,
    string_concat(One, " and ", Temp),
    string_concat(Temp, Two, Joined).
oxford_comma_list([First, Second | Rest], Joined) :-
    append(Middle, [Last], [Second | Rest]),
    atomic_list_concat([First | Middle], ", ", CommaPart),
    string_concat(CommaPart, ", and ", Temp),
    string_concat(Temp, Last, Joined),
    !.

pick_n(0, _, []) :- !.
pick_n(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    pick_n(N1, T, Rest).
pick_n(N, [_|T], Rest) :-
    N > 0,
    pick_n(N, T, Rest).

generate_subtype_qa(cannonical, Predicate, Question, Answer) :- 
    /* Fetch the Type Info for Predicate */
    resolve_predicate_type_info(Predicate, Types, BaseTypes, VerbInfo),

    /* Extract Types for Fixed Argument and Variable Argument */
    [SubjectType | [_ | _]] = Types, 
    [SubjectBaseType | [VariableArgBaseType | _]] = BaseTypes,

    /* Find the wh-word */
    wh_word(VariableArgBaseType, WhWord),

    /* Find the aux word */
    aux_word(SubjectBaseType, AuxVerb),

    /* Find the verb */
    get_singular_verb(VerbInfo, Verb),
    get_plural_verb(VerbInfo, PluralVerb),


    /* Bind FixedArg to a concrete value */
    SubjectTerm =.. [SubjectType | [SubjectVal]], call(SubjectTerm),

    /* Query the database for the information */
    QueryTerm =.. [Predicate | [SubjectVal, VariableArgVal]], findall(VariableArgVal, call(QueryTerm), UnsortVariableArgVals),

    sort(UnsortVariableArgVals, VariableArgVals),
    
    length(VariableArgVals, ArgsValLen), ArgsValLen >= 1,

    oxford_comma_list(VariableArgVals, CommaVariableArgVal),
    
    ( modifier(Predicate, Modifier) -> get_singular_modifier(Modifier, SingularModifier), get_plural_modifier(Modifier, PluralModifier); SingularModifier = "", PluralModifier = ""), 

    ( ArgsValLen =:= 1 -> EffectiveModifier = SingularModifier; EffectiveModifier = PluralModifier),

    /* Construct the question string */
    (
        EffectiveModifier == "" -> format(string(Question), "~w ~w ~w ~w?", [WhWord, AuxVerb, SubjectVal, Verb])
        ;
        format(string(Question), "~w ~w ~w ~w ~w?", [WhWord, EffectiveModifier, AuxVerb, SubjectVal, Verb])
    ),

    /* Construct the answer string */
    format(string(Answer), "~w ~w ~w.", [SubjectVal, PluralVerb, CommaVariableArgVal]).

generate_subtype_qa(cannonical_negative, Predicate, Question, Answer) :- 
    /* Fetch the Type Info for Predicate */
    resolve_predicate_type_info(Predicate, Types, BaseTypes, VerbInfo),

    /* Extract Types for Fixed Argument and Variable Argument */
    [SubjectType | [_ | _]] = Types, 
    [SubjectBaseType | [VariableArgBaseType | _]] = BaseTypes,
    nth1(3, Types, NegSubjectType),

    /* Find the wh-word */
    wh_word(VariableArgBaseType, WhWord),

    /* Find the aux word */
    aux_word(SubjectBaseType, AuxVerb),

    /* Find the verb */
    get_singular_verb(VerbInfo, Verb),
    get_plural_verb(VerbInfo, PluralVerb),

    /* Bind FixedArg to a concrete value */
    /* SubjectTerm =.. [SubjectType | [SubjectVal]], call(SubjectTerm),*/
    SubjectTerm =.. [SubjectType | [InterSubjectVal]],
    findall(InterSubjectVal, SubjectTerm, All),
    pick_n(3, All, SubjectVal),
    NegSubjectTerm =.. [NegSubjectType | [NegSubjectVal]], call(NegSubjectTerm),

    /* Query the database for the information */
    QueryTerm =.. [Predicate | [SubjectVal, NegSubjectVal, VariableArgVal]], findall(VariableArgVal, call(QueryTerm), UnsortVariableArgVals),

    sort(UnsortVariableArgVals, VariableArgVals),
    
    length(VariableArgVals, ArgsValLen), ArgsValLen >= 1,

    member(VariableArgVal, VariableArgVals),

    oxford_comma_list(VariableArgVal, CommaVariableArgVal),
    oxford_comma_list(SubjectVal, CommaSubjectVal),
    
    ( modifier(Predicate, Modifier) -> get_singular_modifier(Modifier, SingularModifier), get_plural_modifier(Modifier, PluralModifier); SingularModifier = "", PluralModifier = ""), 

    ( ArgsValLen =:= 1 -> EffectiveModifier = SingularModifier; EffectiveModifier = PluralModifier),

    /* Construct the question string */
    (
        EffectiveModifier == "" -> format(string(Question), "~w ~w ~w ~w?", [WhWord, AuxVerb, SubjectVal, Verb])
        ;
        format(string(Question), "~w ~w ~w ~w but not ~w?", [WhWord, PluralModifier, Verb, CommaSubjectVal, NegSubjectVal])
    ),

    /* Construct the answer string */
    format(string(Answer), "~w ~w ~w but not ~w.", [CommaVariableArgVal, PluralVerb, CommaSubjectVal, NegSubjectVal]).


generate_subtype_qa(possessive, Predicate, Question, Answer) :-
    /* Fetch the Type Info for Predicate */
    resolve_predicate_type_info(Predicate, Types, BaseTypes, VerbInfo),

    /* Extract Types for Fixed Argument and Variable Argument */
    [SubjectType | [_ | _]] = Types, 
    [_ | [VariableArgBaseType | _]] = BaseTypes,

    /* Find the wh-word */
    wh_word(VariableArgBaseType, WhWord),

    /* Find the verb */
    get_singular_verb(VerbInfo, Verb),
    get_plural_verb(VerbInfo, PluralVerb),

    /* Bind FixedArg to a concrete value */
    SubjectTerm =.. [SubjectType | [SubjectVal]], call(SubjectTerm),

    /* Query the database for the information */
    QueryTerm =.. [Predicate | [SubjectVal, VariableArgVal]], findall(VariableArgVal, call(QueryTerm), UnsortVariableArgVals),

    sort(UnsortVariableArgVals, VariableArgVals),

    /* Check how many answers there are */
    length(VariableArgVals, ArgsValLen), ArgsValLen >= 1,

    /* Choose the possessive AUX word from the number of answers */
    possessive_aux_word(VariableArgVals, AuxVerb), 

    /* Convert the list of values to oxford common list */
    oxford_comma_list(VariableArgVals, CommaVariableArgVals),

    /* Choose which form of verb to use */
    (ArgsValLen =:= 1 -> EffectiveVerb = Verb; EffectiveVerb = PluralVerb),
    
    /* Construct the question string */
    atom_chars(SubjectVal, AtomSubjectVal), (last(AtomSubjectVal, 's') -> format(string(AposSubjectVal), "~w'", [SubjectVal]); format(string(AposSubjectVal), "~w's", [SubjectVal])),
    format(string(Question), "~w ~w ~w ~w?", [WhWord, AuxVerb, AposSubjectVal, EffectiveVerb]),

    /* Construct the answer string */
    format(string(Answer), "~w ~w ~w ~w.", [AposSubjectVal, EffectiveVerb, AuxVerb, CommaVariableArgVals]).

generate_predicate_qa(Predicate, Question, Answer) :- 
    predicate_bucket(Predicate, SubType),
    generate_subtype_qa(SubType, Predicate, Question, Answer).

generate_predicate_qa_tuple(Predicate, QA) :-
    generate_predicate_qa(Predicate, Question, Answer),
    QA = [Question, Answer].

generate_qas([], []).
generate_qas(Predicates, QAList) :-
    [H | T] = Predicates, generate_qas(T, TailQAList),
    findall(QA, generate_predicate_qa_tuple(H, QA), HeadQAList),
    format(string(FileName), "~w.txt", [H]),
    open(FileName, write, Stream),
    print_questions(Stream, HeadQAList),
    close(Stream),
    append(HeadQAList, TailQAList, QAList).

print_questions(_, []).
print_questions(Stream, [[Question, Answer] | T]) :-
    format(Stream, "Q: ~w~n", [Question]),
    format(Stream, "A: ~w~n~n", [Answer]),
    print_questions(Stream, T).

generate_qa :-
    generate_predicates(Predicates),
    generate_qas(Predicates, QAList),
    length(QAList, QAListLength),
    open("prompts.txt", write, Stream),
    format(Stream, "Number of QA Pairs: ~w~n~n", [QAListLength]),
    print_questions(Stream, QAList),
    close(Stream).

main :- generate_qa.