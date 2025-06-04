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


/* User defined inheritance */

inherits(person, proper_noun).
inherits(place, proper_noun).
inherits(state, place).
inherits(population, number).
inherits(alphabet, noun).
inherits(mountain, place).

/* User given verbs and their tenses*/

tense('locate', _{
    base: locate,
    present_simple: locates,
    past_simple: located,
    present_progressive: locating,
    past_progressive: was_locating,
    present_simple_passive: is_located
}).

tense('neighbor', _{
    base: neighbor,
    present_simple: neighbors,
    past_simple: neighbored,
    present_progressive: neighboring,
    past_progressive: was_neighboring,
    present_simple_passive: is_neighbored
}).

/*plural forms of words*/
plural_form('state', 'states').

/* whether a word is categorical so we can use "which" */
categorical('state').

/* Types */

type(common_noun, state).
type(noun, population).
type(number, X) :- number(X).

type(state, 'Alabama').
type(state, 'Alaska').
type(state, 'Arizona').
type(state, 'Arkansas').
type(state, 'California').
type(state, 'Colorado').
type(state, 'Connecticut').
type(state, 'Delaware').
type(state, 'Florida').
type(state, 'Georgia').
type(state, 'Hawaii').
type(state, 'Idaho').
type(state, 'Illinois').
type(state, 'Indiana').
type(state, 'Iowa').
type(state, 'Kansas').
type(state, 'Kentucky').
type(state, 'Louisiana').
type(state, 'Maine').
type(state, 'Maryland').
type(state, 'Massachusetts').
type(state, 'Michigan').
type(state, 'Minnesota').
type(state, 'Mississippi').
type(state, 'Missouri').
type(state, 'Montana').
type(state, 'Nebraska').
type(state, 'Nevada').
type(state, 'New Hampshire').
type(state, 'New Jersey').
type(state, 'New Mexico').
type(state, 'New York').
type(state, 'North Carolina').
type(state, 'North Dakota').
type(state, 'Ohio').
type(state, 'Oklahoma').
type(state, 'Oregon').
type(state, 'Pennsylvania').
type(state, 'Rhode Island').
type(state, 'South Carolina').
type(state, 'South Dakota').
type(state, 'Tennessee').
type(state, 'Texas').
type(state, 'Utah').
type(state, 'Vermont').
type(state, 'Virginia').
type(state, 'Washington').
type(state, 'West Virginia').
type(state, 'Wisconsin').
type(state, 'Wyoming').


type(mountain, 'Denali').
type(mountain, 'Mount Whitney').
type(mountain, 'Mount Elbert').
type(mountain, 'Mount Rainier').
type(mountain, 'Mount Shasta').
type(mountain, 'Mount Hood').
type(mountain, 'Mount Mitchell').
type(mountain, 'Mount Saint Helens').
type(mountain, 'Mount Baker').
type(mountain, 'Grand Teton').
type(mountain, 'Pikes Peak').
type(mountain, 'Mount Katahdin').
type(mountain, 'Mount Washington').
type(mountain, 'Mount Adams').
type(mountain, 'Mount Marcy').
type(mountain, 'Mount Baldy').
type(mountain, 'Mount Princeton').
type(mountain, 'Mount Hood').
type(mountain, 'Gannett Peak').
type(mountain, 'Kings Peak').

verb_relation(present_simple_passive, 'located in', 'Denali', ['Alaska']).
verb_relation(present_simple_passive, 'located in', 'Mount Whitney', ['California']).
verb_relation(present_simple_passive, 'located in', 'Mount Shasta', ['California']).
verb_relation(present_simple_passive, 'located in', 'Mount Baldy', ['California']).
verb_relation(present_simple_passive, 'located in', 'Mount Elbert', ['Colorado']).
verb_relation(present_simple_passive, 'located in', 'Pikes Peak', ['Colorado']).
verb_relation(present_simple_passive, 'located in', 'Mount Princeton', ['Colorado']).
verb_relation(present_simple_passive, 'located in', 'San Juan Mountains', ['Colorado', 'New Mexico']).
verb_relation(present_simple_passive, 'located in', 'Mount Rainier', ['Washington']).
verb_relation(present_simple_passive, 'located in', 'Mount Saint Helens', ['Washington']).
verb_relation(present_simple_passive, 'located in', 'Mount Baker', ['Washington']).
verb_relation(present_simple_passive, 'located in', 'Mount Adams', ['Washington']).
verb_relation(present_simple_passive, 'located in', 'Mount Hood', ['Oregon']).
verb_relation(present_simple_passive, 'located in', 'Mount Mitchell', ['North Carolina']).
verb_relation(present_simple_passive, 'located in', 'Grand Teton', ['Wyoming']).
verb_relation(present_simple_passive, 'located in', 'Gannett Peak', ['Wyoming']).
verb_relation(present_simple_passive, 'located in', 'Mount Katahdin', ['Maine']).
verb_relation(present_simple_passive, 'located in', 'Mount Washington', ['New Hampshire']).
verb_relation(present_simple_passive, 'located in', 'Mount Marcy', ['New York']).
verb_relation(present_simple_passive, 'located in', 'Kings Peak', ['Utah']).


verb_relation(present_simple, neighbor, 'Alabama', ['Florida', 'Georgia', 'Mississippi', 'Tennessee']).
verb_relation(present_simple, neighbor, 'Alaska', []).
verb_relation(present_simple, neighbor, 'Arizona', ['California', 'Colorado', 'Nevada', 'New Mexico', 'Utah']).
verb_relation(present_simple, neighbor, 'Arkansas', ['Louisiana', 'Mississippi', 'Missouri', 'Oklahoma', 'Tennessee', 'Texas']).
verb_relation(present_simple, neighbor, 'California', ['Arizona', 'Nevada', 'Oregon']).
verb_relation(present_simple, neighbor, 'Colorado', ['Arizona', 'Kansas', 'Nebraska', 'New Mexico', 'Oklahoma', 'Utah', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Connecticut', ['Massachusetts', 'New York', 'Rhode Island']).
verb_relation(present_simple, neighbor, 'Delaware', ['Maryland', 'New Jersey', 'Pennsylvania']).
verb_relation(present_simple, neighbor, 'Florida', ['Alabama', 'Georgia']).
verb_relation(present_simple, neighbor, 'Georgia', ['Alabama', 'Florida', 'North Carolina', 'South Carolina', 'Tennessee']).
verb_relation(present_simple, neighbor, 'Hawaii', []).
verb_relation(present_simple, neighbor, 'Idaho', ['Montana', 'Nevada', 'Oregon', 'Utah', 'Washington', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Illinois', ['Indiana', 'Iowa', 'Kentucky', 'Missouri', 'Wisconsin']).
verb_relation(present_simple, neighbor, 'Indiana', ['Illinois', 'Kentucky', 'Michigan', 'Ohio']).
verb_relation(present_simple, neighbor, 'Iowa', ['Illinois', 'Minnesota', 'Missouri', 'Nebraska', 'South Dakota', 'Wisconsin']).
verb_relation(present_simple, neighbor, 'Kansas', ['Colorado', 'Missouri', 'Nebraska', 'Oklahoma']).
verb_relation(present_simple, neighbor, 'Kentucky', ['Illinois', 'Indiana', 'Missouri', 'Ohio', 'Tennessee', 'Virginia', 'West Virginia']).
verb_relation(present_simple, neighbor, 'Louisiana', ['Arkansas', 'Mississippi', 'Texas']).
verb_relation(present_simple, neighbor, 'Maine', ['New Hampshire']).
verb_relation(present_simple, neighbor, 'Maryland', ['Delaware', 'Pennsylvania', 'Virginia', 'West Virginia']).
verb_relation(present_simple, neighbor, 'Massachusetts', ['Connecticut', 'New Hampshire', 'New York', 'Rhode Island', 'Vermont']).
verb_relation(present_simple, neighbor, 'Michigan', ['Illinois', 'Indiana', 'Minnesota', 'Ohio', 'Wisconsin']).
verb_relation(present_simple, neighbor, 'Minnesota', ['Iowa', 'Michigan', 'North Dakota', 'South Dakota', 'Wisconsin']).
verb_relation(present_simple, neighbor, 'Mississippi', ['Alabama', 'Arkansas', 'Louisiana', 'Tennessee']).
verb_relation(present_simple, neighbor, 'Missouri', ['Arkansas', 'Illinois', 'Iowa', 'Kansas', 'Kentucky', 'Nebraska', 'Oklahoma', 'Tennessee']).
verb_relation(present_simple, neighbor, 'Montana', ['Idaho', 'North Dakota', 'South Dakota', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Nebraska', ['Colorado', 'Iowa', 'Kansas', 'Missouri', 'South Dakota', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Nevada', ['Arizona', 'California', 'Idaho', 'Oregon', 'Utah']).
verb_relation(present_simple, neighbor, 'New Hampshire', ['Maine', 'Massachusetts', 'Vermont']).
verb_relation(present_simple, neighbor, 'New Jersey', ['Delaware', 'New York', 'Pennsylvania']).
verb_relation(present_simple, neighbor, 'New Mexico', ['Arizona', 'Colorado', 'Oklahoma', 'Texas', 'Utah']).
verb_relation(present_simple, neighbor, 'New York', ['Connecticut', 'Massachusetts', 'New Jersey', 'Pennsylvania', 'Vermont']).
verb_relation(present_simple, neighbor, 'North Carolina', ['Georgia', 'South Carolina', 'Tennessee', 'Virginia']).
verb_relation(present_simple, neighbor, 'North Dakota', ['Minnesota', 'Montana', 'South Dakota']).
verb_relation(present_simple, neighbor, 'Ohio', ['Indiana', 'Kentucky', 'Michigan', 'Pennsylvania', 'West Virginia']).
verb_relation(present_simple, neighbor, 'Oklahoma', ['Arkansas', 'Colorado', 'Kansas', 'Missouri', 'New Mexico', 'Texas']).
verb_relation(present_simple, neighbor, 'Oregon', ['California', 'Idaho', 'Nevada', 'Washington']).
verb_relation(present_simple, neighbor, 'Pennsylvania', ['Delaware', 'Maryland', 'New Jersey', 'New York', 'Ohio', 'West Virginia']).
verb_relation(present_simple, neighbor, 'Rhode Island', ['Connecticut', 'Massachusetts']).
verb_relation(present_simple, neighbor, 'South Carolina', ['Georgia', 'North Carolina']).
verb_relation(present_simple, neighbor, 'South Dakota', ['Iowa', 'Minnesota', 'Montana', 'Nebraska', 'North Dakota', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Tennessee', ['Alabama', 'Arkansas', 'Georgia', 'Kentucky', 'Mississippi', 'Missouri', 'North Carolina', 'Virginia']).
verb_relation(present_simple, neighbor, 'Texas', ['Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma']).
verb_relation(present_simple, neighbor, 'Utah', ['Arizona', 'Colorado', 'Idaho', 'Nevada', 'New Mexico', 'Wyoming']).
verb_relation(present_simple, neighbor, 'Vermont', ['Massachusetts', 'New Hampshire', 'New York']).
verb_relation(present_simple, neighbor, 'Virginia', ['Kentucky', 'Maryland', 'North Carolina', 'Tennessee', 'West Virginia']).
verb_relation(present_simple, neighbor, 'Washington', ['Idaho', 'Oregon']).
verb_relation(present_simple, neighbor, 'West Virginia', ['Kentucky', 'Maryland', 'Ohio', 'Pennsylvania', 'Virginia']).
verb_relation(present_simple, neighbor, 'Wisconsin', ['Illinois', 'Iowa', 'Michigan', 'Minnesota']).
verb_relation(present_simple, neighbor, 'Wyoming', ['Colorado', 'Idaho', 'Montana', 'Nebraska', 'South Dakota', 'Utah']).


possessive_relation(present_simple, 'capital', 'Alabama', 'Montgomery').
possessive_relation(present_simple, 'capital', 'Alaska', 'Juneau').
possessive_relation(present_simple, 'capital', 'Arizona', 'Phoenix').
possessive_relation(present_simple, 'capital', 'Arkansas', 'Little Rock').
possessive_relation(present_simple, 'capital', 'California', 'Sacramento').
possessive_relation(present_simple, 'capital', 'Colorado', 'Denver').
possessive_relation(present_simple, 'capital', 'Connecticut', 'Hartford').
possessive_relation(present_simple, 'capital', 'Delaware', 'Dover').
possessive_relation(present_simple, 'capital', 'Florida', 'Tallahassee').
possessive_relation(present_simple, 'capital', 'Georgia', 'Atlanta').
possessive_relation(present_simple, 'capital', 'Hawaii', 'Honolulu').
possessive_relation(present_simple, 'capital', 'Idaho', 'Boise').
possessive_relation(present_simple, 'capital', 'Illinois', 'Springfield').
possessive_relation(present_simple, 'capital', 'Indiana', 'Indianapolis').
possessive_relation(present_simple, 'capital', 'Iowa', 'Des Moines').
possessive_relation(present_simple, 'capital', 'Kansas', 'Topeka').
possessive_relation(present_simple, 'capital', 'Kentucky', 'Frankfort').
possessive_relation(present_simple, 'capital', 'Louisiana', 'Baton Rouge').
possessive_relation(present_simple, 'capital', 'Maine', 'Augusta').
possessive_relation(present_simple, 'capital', 'Maryland', 'Annapolis').
possessive_relation(present_simple, 'capital', 'Massachusetts', 'Boston').
possessive_relation(present_simple, 'capital', 'Michigan', 'Lansing').
possessive_relation(present_simple, 'capital', 'Minnesota', 'Saint Paul').
possessive_relation(present_simple, 'capital', 'Mississippi', 'Jackson').
possessive_relation(present_simple, 'capital', 'Missouri', 'Jefferson City').
possessive_relation(present_simple, 'capital', 'Montana', 'Helena').
possessive_relation(present_simple, 'capital', 'Nebraska', 'Lincoln').
possessive_relation(present_simple, 'capital', 'Nevada', 'Carson City').
possessive_relation(present_simple, 'capital', 'New Hampshire', 'Concord').
possessive_relation(present_simple, 'capital', 'New Jersey', 'Trenton').
possessive_relation(present_simple, 'capital', 'New Mexico', 'Santa Fe').
possessive_relation(present_simple, 'capital', 'New York', 'Albany').
possessive_relation(present_simple, 'capital', 'North Carolina', 'Raleigh').
possessive_relation(present_simple, 'capital', 'North Dakota', 'Bismarck').
possessive_relation(present_simple, 'capital', 'Ohio', 'Columbus').
possessive_relation(present_simple, 'capital', 'Oklahoma', 'Oklahoma City').
possessive_relation(present_simple, 'capital', 'Oregon', 'Salem').
possessive_relation(present_simple, 'capital', 'Pennsylvania', 'Harrisburg').
possessive_relation(present_simple, 'capital', 'Rhode Island', 'Providence').
possessive_relation(present_simple, 'capital', 'South Carolina', 'Columbia').
possessive_relation(present_simple, 'capital', 'South Dakota', 'Pierre').
possessive_relation(present_simple, 'capital', 'Tennessee', 'Nashville').
possessive_relation(present_simple, 'capital', 'Texas', 'Austin').
possessive_relation(present_simple, 'capital', 'Utah', 'Salt Lake City').
possessive_relation(present_simple, 'capital', 'Vermont', 'Montpelier').
possessive_relation(present_simple, 'capital', 'Virginia', 'Richmond').
possessive_relation(present_simple, 'capital', 'Washington', 'Olympia').
possessive_relation(present_simple, 'capital', 'West Virginia', 'Charleston').
possessive_relation(present_simple, 'capital', 'Wisconsin', 'Madison').
possessive_relation(present_simple, 'capital', 'Wyoming', 'Cheyenne').

possessive_relation(present_simple, 'population', 'California', 39663800).
possessive_relation(present_simple, 'population', 'Texas', 31853800).
possessive_relation(present_simple, 'population', 'Florida', 23839600).
possessive_relation(present_simple, 'population', 'New York', 19997100).
possessive_relation(present_simple, 'population', 'Pennsylvania', 13139800).
possessive_relation(present_simple, 'population', 'Illinois', 12778100).
possessive_relation(present_simple, 'population', 'Ohio', 11942600).
possessive_relation(present_simple, 'population', 'Georgia', 11297300).
possessive_relation(present_simple, 'population', 'North Carolina', 11210900).
possessive_relation(present_simple, 'population', 'Michigan', 10197600).
possessive_relation(present_simple, 'population', 'New Jersey', 9622060).
possessive_relation(present_simple, 'population', 'Virginia', 8887700).
possessive_relation(present_simple, 'population', 'Washington', 8059040).
possessive_relation(present_simple, 'population', 'Arizona', 7691740).
possessive_relation(present_simple, 'population', 'Tennessee', 7307200).
possessive_relation(present_simple, 'population', 'Massachusetts', 7205770).
possessive_relation(present_simple, 'population', 'Indiana', 6968420).
possessive_relation(present_simple, 'population', 'Maryland', 6309380).
possessive_relation(present_simple, 'population', 'Missouri', 6282890).
possessive_relation(present_simple, 'population', 'Colorado', 6013650).
possessive_relation(present_simple, 'population', 'Wisconsin', 5991540).
possessive_relation(present_simple, 'population', 'Minnesota', 5833250).
possessive_relation(present_simple, 'population', 'South Carolina', 5569830).
possessive_relation(present_simple, 'population', 'Alabama', 5197720).
possessive_relation(present_simple, 'population', 'Kentucky', 4626150).
possessive_relation(present_simple, 'population', 'Louisiana', 4607410).
possessive_relation(present_simple, 'population', 'Oregon', 4291090).
possessive_relation(present_simple, 'population', 'Oklahoma', 4126900).
possessive_relation(present_simple, 'population', 'Connecticut', 3707120).
possessive_relation(present_simple, 'population', 'Utah', 3564000).
possessive_relation(present_simple, 'population', 'Nevada', 3320570).
possessive_relation(present_simple, 'population', 'Iowa', 3264560).
possessive_relation(present_simple, 'population', 'Arkansas', 3107240).
possessive_relation(present_simple, 'population', 'Kansas', 2989710).
possessive_relation(present_simple, 'population', 'Mississippi', 2942920).
possessive_relation(present_simple, 'population', 'New Mexico', 2139350).
possessive_relation(present_simple, 'population', 'Idaho', 2032120).
possessive_relation(present_simple, 'population', 'Nebraska', 2023070).
possessive_relation(present_simple, 'population', 'West Virginia', 1769460).
possessive_relation(present_simple, 'population', 'Hawaii', 1450900).
possessive_relation(present_simple, 'population', 'New Hampshire', 1415860).
possessive_relation(present_simple, 'population', 'Maine', 1410380).
possessive_relation(present_simple, 'population', 'Montana', 1143160).
possessive_relation(present_simple, 'population', 'Rhode Island', 1121190).
possessive_relation(present_simple, 'population', 'Delaware', 1067410).
possessive_relation(present_simple, 'population', 'South Dakota', 931033).
possessive_relation(present_simple, 'population', 'North Dakota', 804089).
possessive_relation(present_simple, 'population', 'Alaska', 743756).
possessive_relation(present_simple, 'population', 'Vermont', 648278).
possessive_relation(present_simple, 'population', 'Wyoming', 590169).



collective('population in total', singular).

plural_possessive('population', 'population in total').

plural_possessive_relation(present_simple, 'population in total', States, Population) :-
    sum_state_populations(States, Population).

state_population(State, Population) :-
    possessive_relation(present_simple, 'population', State, Population), !.
state_population(_, 0).

sum_state_populations([], 0).
sum_state_populations([State|Rest], Total) :-
    state_population(State, Pop),
    sum_state_populations(Rest, RestTotal),
    Total is Pop + RestTotal.

ranker(R, N, 'population', States) :- (R = 'TopN', top_n_states(N, States));(R = 'Rank', nth_most_populous_state(N, State)).

    all_state_populations(SortedPairs) :-
        findall(Pop-State,
                possessive_relation(present_simple, 'population', State, Pop),
                Pairs),
        sort(0, @>=, Pairs, SortedPairs).

    top_n_states(N, StateList) :-
        all_state_populations(SortedPairs),
        length(TopN, N),
        append(TopN, _, SortedPairs),
        findall(State, member(_-State, TopN), StateList).

    nth_most_populous_state(N, State) :-
        all_state_populations(SortedPairs),
        nth1(N, SortedPairs, _Pop-State).

