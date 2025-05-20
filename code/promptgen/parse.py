import json

# Built-in WH mapping (can be extended)
BUILTIN_WH_MAP = {
    "person": "who",
    "object": "what",
    "place": "where"
}

# Built-in type hierarchy
BUILTIN_TYPE_HIERARCHY = {
    "person": "noun",
    "object": "noun",
    "place": "noun",
    "noun": None,
    "verb": None
}

def parse_annotations(json_path):
    with open(json_path, 'r') as f:
        data = json.load(f)

    # Merge user-defined types into hierarchy
    type_hierarchy = BUILTIN_TYPE_HIERARCHY.copy()
    for child, parent in data.get("types", {}).items():
        type_hierarchy[child] = parent

    # Inherit WH map from built-in only (extendable if needed)
    wh_map = BUILTIN_WH_MAP.copy()

    # Parse relations
    relations = []
    for rel in data.get("relations", []):
        relations.append({
            "name": rel["name"],
            "args": rel["args"],
            "meta_template": rel["meta_template"],
            "verb_phrase": rel["verb_phrase"]
        })

    return {
        "type_hierarchy": type_hierarchy,
        "wh_map": wh_map,
        "relations": relations
    }