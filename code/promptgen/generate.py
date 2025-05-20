from swiplserver import PrologMQI

def extract_relation_facts_from_annotations(prolog_file_path: str, annotations: dict):
    """
    Extract all relation facts from a Prolog file based on a parsed annotations dictionary.
    
    Parameters:
        prolog_file_path (str): Path to the .pl Prolog file.
        annotations (dict): Parsed annotations.json as a Python dictionary.
    
    Returns:
        dict[str, list[dict]]: A mapping from relation names to lists of variable bindings.
    """
    relation_facts = {}

    with PrologMQI() as mqi:
        with mqi.create_thread() as prolog:
            # Consult the Prolog file
            prolog.query(f"consult('{prolog_file_path}').")

            for relation in annotations.get("relations", []):
                rel_name = relation["name"]
                var_names = [arg["var"] for arg in relation["args"]]

                # Create a Prolog query
                var_str = ', '.join(var_names)
                query = f"{rel_name}({var_str})"

                # Execute the query
                results = list(prolog.query(query))
                relation_facts[rel_name] = results

    return relation_facts

def resolve_wh_word(var_type, type_hierarchy):
    """
    Recursively walk the type hierarchy to find the most specific WH-word.
    """
    wh_map = {
        "person": "who",
        "object": "what",
        "place": "where",
    }

    current = var_type
    visited = set()
    while current:
        if current in wh_map:
            return wh_map[current]
        visited.add(current)
        current = type_hierarchy.get(current)
        if current in visited:
            break  # prevent cycles
    return "what"  # default fallback

def generate_binary_questions(facts, annotations):
    """
    Generate Q/A pairs for binary predicates using type info from parsed annotations.
    """
    qa_pairs = []
    type_hierarchy = annotations.get("types", {})

    for rel in annotations.get("relations", []):
        if len(rel["args"]) != 2:
            continue  # Skip non-binary relations for now

        rel_name = rel["name"]
        verb_phrase = rel.get("verb_phrase", rel_name)

        var1, var2 = rel["args"]
        var1_name, type1 = var1["var"], var1["type"]
        var2_name, type2 = var2["var"], var2["type"]

        wh1 = resolve_wh_word(type1, type_hierarchy)
        wh2 = resolve_wh_word(type2, type_hierarchy)

        for binding in facts.get(rel_name, []):
            val1, val2 = binding[var1_name], binding[var2_name]

            if wh1 == "who":
                val1 = val1.capitalize()

            # Question: What does X like?
            question1 = f"{wh2.capitalize()} does {val1} {verb_phrase[0]}?"
            answer1 = f"{val1} {verb_phrase[1]} {val2}."
            qa_pairs.append((question1, answer1))

    return qa_pairs

def write_qa_pairs_to_file(qa_pairs, output_path):
    """
    Write question-answer pairs to a file.
    
    Each line will have the format:
    Q: <question>
    A: <answer>
    ---
    """
    with open(output_path, 'w', encoding='utf-8') as f:
        for question, answer in qa_pairs:
            f.write(f"Q: {question}\n")
            f.write(f"A: {answer}\n")
            f.write("---\n")


