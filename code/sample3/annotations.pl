generate_predicates([
    population, abbrev, myborder, notmyborder2,
    riverflow
]).

/* Geobase Annotations */
type(myriver, object).
type(mystate, object).

arg(myborder, [mystate, mystate]).
arg(riverflow, [myriver, mystate]).
arg(notmyborder2, [mystate, mystate, mystate]).
arg(population, [mystate, object]).
arg(abbrev, [mystate, object]).

verb(myborder, ["border", "borders"]).
verb(riverflow, ["flow through", "flows through"]).
verb(notmyborder2, ["border", "borders"]).
verb(population, ["population", "population"]).
verb(abbrev, ["abbreviation", "abbreviation"]).

modifier(myborder, ["state", "states"]).
modifier(notmyborder2, ["state", "states"]).
modifier(riverflow, ["state", "states"]).

predicate_bucket(myborder, cannonical).
predicate_bucket(riverflow, cannonical).
predicate_bucket(notmyborder2, cannonical_negative).

predicate_bucket(population, possessive).
predicate_bucket(abbrev, possessive).