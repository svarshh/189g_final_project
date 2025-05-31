from pyswip import Prolog
from queue import deque

class noun:
    def __init__(self, name: str):
        self.name = name
    def __str__(self):
        return f"{self.name}"

class verb:
    def __init__(self, name: str, p_simple: str):
        self.name = name
        self.ps = p_simple
    def present_simple(self):
        return self.ps
    def __str__(self):
        return f"{self.name}"

class propernoun(noun):
    def __init__(self, name: str):
        super().__init__(name)  # Capitalize here
    def __str__(self):
        return f"{self.name.capitalize()}"

class person(propernoun):
    def __init__(self, name: str):
        super().__init__(name)

class time(noun):
    def __init__(self, name: str):
        super().__init__(name)
    # def __str__(self):
    #     return f"{self.name}"

class place(noun):
    def __init__(self, name: str):
        super().__init__(name)

class proper_place(propernoun):
    def __init__(self, name: str):
        super().__init__(name)

class things(noun):
    def __init__(self, name: str):
        super().__init__(name)


# can be used to decide which auxillaries to use.
def select_auxiliary(subject_person, subject_number, tense):
    if tense == "present_simple":
        if subject_person == 3 and subject_number == "singular":
            return "does"
        else:
            return "do"
    elif tense == "past_simple":
        return "did"
    elif tense == "present_continuous":
        if subject_person == 1 and subject_number == "singular":
            return "am"
        elif subject_person == 3 and subject_number == "singular":
            return "is"
        else:
            return "are"
    elif tense == "past_continuous":
        if subject_person in [1,3] and subject_number == "singular":
            return "was"
        else:
            return "were"
    elif tense == "future_simple":
        return "will"
    

gc = {
    "WH": ["who", "what"],
    "AUX": [verb("do","does")],
    "SUBJ": [person("alice"), person("bob"), person("claire") ],
    "VERB": [verb("like", "likes")],
    "OBJ": [things("pizza"), things("sushi")],
    "POSSESSIVE": [person("alice"), person("bob"), person("claire")],
    "PN": [noun("friend")]
}

# Questioning Facts
# wh + aux + subj + verb
prolog = Prolog()
prolog.consult("logic.pl")

for wh in gc["WH"]:
    for aux in gc["AUX"]:
        for subj in gc["SUBJ"]:
            for v in gc["VERB"]:
                obj = (deque(prolog.query(f"{wh}(X, {subj.name})")))
                if not obj:
                    break
                print(f"Q: {wh.capitalize()} {aux.present_simple()} {subj} {v}?")
                print(f"A: {subj} {v.present_simple()} {obj[0]['X']}.")
                print(" ")


# Yes/No Questions
# AUX + SUBJ + VERB + OBJ
for aux in gc["AUX"]:
    for subj in gc["SUBJ"]:
        for v in gc["VERB"]:
            for obj in gc["OBJ"]:
                ans = "No."
                if(deque(prolog.query(f"verb_relation({v}, {subj.name}, {obj.name})"))):
                    ans = "Yes."
                print(f"Q: {aux.present_simple().capitalize()} {subj} {v} {obj}?")
                print(f"A: {ans}")
                print(" ")

# SUBJ + AUX + Possessive + predicate_noun

# for wh in gc["WH"]:
#     for aux in gc["AUX"]:
#         for possessive in gc["POSSESSIVE"]:
#             for pn in gc["PN"]:
                
#                 subj = (deque(prolog.query(f"possessive_relation({pn}, {possessive}, X)")))
                    
#                 if not subj:
#                     break

#                 print(f"{wh} {aux} {possessive}'s {pn}")
                

                   
                









