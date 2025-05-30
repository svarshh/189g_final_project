
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

/* Types */
type(place, 'Alabama').
type(place, 'Alaska').
type(place, 'Arizona').
type(place, 'Arkansas').
type(place, 'California').
type(place, 'Colorado').
type(place, 'Connecticut').
type(place, 'Delaware').
type(place, 'Florida').
type(place, 'Georgia').
type(place, 'Hawaii').
type(place, 'Idaho').
type(place, 'Illinois').
type(place, 'Indiana').
type(place, 'Iowa').
type(place, 'Kansas').
type(place, 'Kentucky').
type(place, 'Louisiana').
type(place, 'Maine').
type(place, 'Maryland').
type(place, 'Massachusetts').
type(place, 'Michigan').
type(place, 'Minnesota').
type(place, 'Mississippi').
type(place, 'Missouri').
type(place, 'Montana').
type(place, 'Nebraska').
type(place, 'Nevada').
type(place, 'New Hampshire').
type(place, 'New Jersey').
type(place, 'New Mexico').
type(place, 'New York').
type(place, 'North Carolina').
type(place, 'North Dakota').
type(place, 'Ohio').
type(place, 'Oklahoma').
type(place, 'Oregon').
type(place, 'Pennsylvania').
type(place, 'Rhode Island').
type(place, 'South Carolina').
type(place, 'South Dakota').
type(place, 'Tennessee').
type(place, 'Texas').
type(place, 'Utah').
type(place, 'Vermont').
type(place, 'Virginia').
type(place, 'Washington').
type(place, 'West Virginia').
type(place, 'Wisconsin').
type(place, 'Wyoming').

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

verb_relation(present_simple_passive, locate, 'Denali', ['Alaska']).
verb_relation(present_simple_passive, locate, 'Mount Whitney', ['California']).
verb_relation(present_simple_passive, locate, 'Mount Shasta', ['California']).
verb_relation(present_simple_passive, locate, 'Mount Baldy', ['California']).
verb_relation(present_simple_passive, locate, 'Mount Elbert', ['Colorado']).
verb_relation(present_simple_passive, locate, 'Pikes Peak', ['Colorado']).
verb_relation(present_simple_passive, locate, 'Mount Princeton', ['Colorado']).
verb_relation(present_simple_passive, locate, 'San Juan Mountains', ['Colorado', 'New Mexico']).
verb_relation(present_simple_passive, locate, 'Mount Rainier', ['Washington']).
verb_relation(present_simple_passive, locate, 'Mount Saint Helens', ['Washington']).
verb_relation(present_simple_passive, locate, 'Mount Baker', ['Washington']).
verb_relation(present_simple_passive, locate, 'Mount Adams', ['Washington']).
verb_relation(present_simple_passive, locate, 'Mount Hood', ['Oregon']).
verb_relation(present_simple_passive, locate, 'Mount Mitchell', ['North Carolina']).
verb_relation(present_simple_passive, locate, 'Grand Teton', ['Wyoming']).
verb_relation(present_simple_passive, locate, 'Gannett Peak', ['Wyoming']).
verb_relation(present_simple_passive, locate, 'Mount Katahdin', ['Maine']).
verb_relation(present_simple_passive, locate, 'Mount Washington', ['New Hampshire']).
verb_relation(present_simple_passive, locate, 'Mount Marcy', ['New York']).
verb_relation(present_simple_passive, locate, 'Kings Peak', ['Utah']).


verb_relation(present_simple, neighbors, 'Alabama', ['Florida', 'Georgia', 'Mississippi', 'Tennessee']).
verb_relation(present_simple, neighbors, 'Alaska', []).
verb_relation(present_simple, neighbors, 'Arizona', ['California', 'Colorado', 'Nevada', 'New Mexico', 'Utah']).
verb_relation(present_simple, neighbors, 'Arkansas', ['Louisiana', 'Mississippi', 'Missouri', 'Oklahoma', 'Tennessee', 'Texas']).
verb_relation(present_simple, neighbors, 'California', ['Arizona', 'Nevada', 'Oregon']).
verb_relation(present_simple, neighbors, 'Colorado', ['Arizona', 'Kansas', 'Nebraska', 'New Mexico', 'Oklahoma', 'Utah', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Connecticut', ['Massachusetts', 'New York', 'Rhode Island']).
verb_relation(present_simple, neighbors, 'Delaware', ['Maryland', 'New Jersey', 'Pennsylvania']).
verb_relation(present_simple, neighbors, 'Florida', ['Alabama', 'Georgia']).
verb_relation(present_simple, neighbors, 'Georgia', ['Alabama', 'Florida', 'North Carolina', 'South Carolina', 'Tennessee']).
verb_relation(present_simple, neighbors, 'Hawaii', []).
verb_relation(present_simple, neighbors, 'Idaho', ['Montana', 'Nevada', 'Oregon', 'Utah', 'Washington', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Illinois', ['Indiana', 'Iowa', 'Kentucky', 'Missouri', 'Wisconsin']).
verb_relation(present_simple, neighbors, 'Indiana', ['Illinois', 'Kentucky', 'Michigan', 'Ohio']).
verb_relation(present_simple, neighbors, 'Iowa', ['Illinois', 'Minnesota', 'Missouri', 'Nebraska', 'South Dakota', 'Wisconsin']).
verb_relation(present_simple, neighbors, 'Kansas', ['Colorado', 'Missouri', 'Nebraska', 'Oklahoma']).
verb_relation(present_simple, neighbors, 'Kentucky', ['Illinois', 'Indiana', 'Missouri', 'Ohio', 'Tennessee', 'Virginia', 'West Virginia']).
verb_relation(present_simple, neighbors, 'Louisiana', ['Arkansas', 'Mississippi', 'Texas']).
verb_relation(present_simple, neighbors, 'Maine', ['New Hampshire']).
verb_relation(present_simple, neighbors, 'Maryland', ['Delaware', 'Pennsylvania', 'Virginia', 'West Virginia']).
verb_relation(present_simple, neighbors, 'Massachusetts', ['Connecticut', 'New Hampshire', 'New York', 'Rhode Island', 'Vermont']).
verb_relation(present_simple, neighbors, 'Michigan', ['Illinois', 'Indiana', 'Minnesota', 'Ohio', 'Wisconsin']).
verb_relation(present_simple, neighbors, 'Minnesota', ['Iowa', 'Michigan', 'North Dakota', 'South Dakota', 'Wisconsin']).
verb_relation(present_simple, neighbors, 'Mississippi', ['Alabama', 'Arkansas', 'Louisiana', 'Tennessee']).
verb_relation(present_simple, neighbors, 'Missouri', ['Arkansas', 'Illinois', 'Iowa', 'Kansas', 'Kentucky', 'Nebraska', 'Oklahoma', 'Tennessee']).
verb_relation(present_simple, neighbors, 'Montana', ['Idaho', 'North Dakota', 'South Dakota', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Nebraska', ['Colorado', 'Iowa', 'Kansas', 'Missouri', 'South Dakota', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Nevada', ['Arizona', 'California', 'Idaho', 'Oregon', 'Utah']).
verb_relation(present_simple, neighbors, 'New Hampshire', ['Maine', 'Massachusetts', 'Vermont']).
verb_relation(present_simple, neighbors, 'New Jersey', ['Delaware', 'New York', 'Pennsylvania']).
verb_relation(present_simple, neighbors, 'New Mexico', ['Arizona', 'Colorado', 'Oklahoma', 'Texas', 'Utah']).
verb_relation(present_simple, neighbors, 'New York', ['Connecticut', 'Massachusetts', 'New Jersey', 'Pennsylvania', 'Vermont']).
verb_relation(present_simple, neighbors, 'North Carolina', ['Georgia', 'South Carolina', 'Tennessee', 'Virginia']).
verb_relation(present_simple, neighbors, 'North Dakota', ['Minnesota', 'Montana', 'South Dakota']).
verb_relation(present_simple, neighbors, 'Ohio', ['Indiana', 'Kentucky', 'Michigan', 'Pennsylvania', 'West Virginia']).
verb_relation(present_simple, neighbors, 'Oklahoma', ['Arkansas', 'Colorado', 'Kansas', 'Missouri', 'New Mexico', 'Texas']).
verb_relation(present_simple, neighbors, 'Oregon', ['California', 'Idaho', 'Nevada', 'Washington']).
verb_relation(present_simple, neighbors, 'Pennsylvania', ['Delaware', 'Maryland', 'New Jersey', 'New York', 'Ohio', 'West Virginia']).
verb_relation(present_simple, neighbors, 'Rhode Island', ['Connecticut', 'Massachusetts']).
verb_relation(present_simple, neighbors, 'South Carolina', ['Georgia', 'North Carolina']).
verb_relation(present_simple, neighbors, 'South Dakota', ['Iowa', 'Minnesota', 'Montana', 'Nebraska', 'North Dakota', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Tennessee', ['Alabama', 'Arkansas', 'Georgia', 'Kentucky', 'Mississippi', 'Missouri', 'North Carolina', 'Virginia']).
verb_relation(present_simple, neighbors, 'Texas', ['Arkansas', 'Louisiana', 'New Mexico', 'Oklahoma']).
verb_relation(present_simple, neighbors, 'Utah', ['Arizona', 'Colorado', 'Idaho', 'Nevada', 'New Mexico', 'Wyoming']).
verb_relation(present_simple, neighbors, 'Vermont', ['Massachusetts', 'New Hampshire', 'New York']).
verb_relation(present_simple, neighbors, 'Virginia', ['Kentucky', 'Maryland', 'North Carolina', 'Tennessee', 'West Virginia']).
verb_relation(present_simple, neighbors, 'Washington', ['Idaho', 'Oregon']).
verb_relation(present_simple, neighbors, 'West Virginia', ['Kentucky', 'Maryland', 'Ohio', 'Pennsylvania', 'Virginia']).
verb_relation(present_simple, neighbors, 'Wisconsin', ['Illinois', 'Iowa', 'Michigan', 'Minnesota']).
verb_relation(present_simple, neighbors, 'Wyoming', ['Colorado', 'Idaho', 'Montana', 'Nebraska', 'South Dakota', 'Utah']).


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

