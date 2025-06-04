generate_predicates([
    abbrev, simple_border, complex_logic_border, riverflow, statecapital, population, stateriverflow, statemajorcity
]).

/* Geobase Annotations */
type(myriver, object).
type(mystate, object).

arg(simple_border, [mystate, mystate]).
arg(riverflow, [myriver, mystate]).
arg(stateriverflow, [mystate, myriver]).
arg(statemajorcity, [mystate, object]).
arg(complex_logic_border, [mystate, mystate, mystate]).
arg(population, [mystate, object]).
arg(abbrev, [mystate, object]).
arg(statecapital, [mystate, object]).

verb(simple_border, ["border", "borders"]).
verb(riverflow, ["flow through", "flows through"]).
verb(stateriverflow, ["contain", "contains"]).
verb(statemajorcity, ["contain", "contains"]).
verb(complex_logic_border, ["border", "borders"]).
verb(population, ["population", "population"]).
verb(abbrev, ["abbreviation", "abbreviation"]).
verb(statecapital, ["capital", "capital"]).

modifier(simple_border, ["state", "states"]).
modifier(complex_logic_border, ["state", "states"]).
modifier(riverflow, ["state", "states"]).
modifier(stateriverflow, ["river", "rivers"]).
modifier(statemajorcity, ["major city", "major cities"]).

predicate_bucket(simple_border, cannonical).
predicate_bucket(riverflow, cannonical).
predicate_bucket(stateriverflow, cannonical).
predicate_bucket(statemajorcity, cannonical).
predicate_bucket(complex_logic_border, cannonical_negative).

predicate_bucket(population, possessive).
predicate_bucket(abbrev, possessive).
predicate_bucket(statecapital, possessive).