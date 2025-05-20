import argparse
import json
import os

from parse import parse_annotations
from generate import extract_relation_facts_from_annotations, generate_binary_questions, write_qa_pairs_to_file

def main():

    parser = argparse.ArgumentParser(description="Parse Prolog and annotation files.")
    parser.add_argument("prolog_file", help="Path to the Prolog file")
    parser.add_argument("annotations_file", help="Path to the annotations JSON file")
    args = parser.parse_args()

    if not os.path.exists(args.annotations_file):
        print(f"Error: Annotations file '{args.annotations_file}' not found.")
        return

    if not os.path.exists(args.prolog_file):
        print(f"Warning: Prolog file '{args.prolog_file}' not found. Proceeding anyway.")

    parsed = parse_annotations(args.annotations_file)
    facts = extract_relation_facts_from_annotations(args.prolog_file, parsed)

    for rel, rel_facts in facts.items():
        print(f"\nRelation: {rel}")
        for fact in rel_facts:
            print(f"  {fact}")

    binary_questions = generate_binary_questions(facts, parsed)

    write_qa_pairs_to_file(binary_questions, "prompts.txt") 

if __name__ == "__main__":
    main()