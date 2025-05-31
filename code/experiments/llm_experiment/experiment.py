def ask_llm(system_prompt: str, prompt: str) -> str:
    import requests

    url = "http://localhost:11434/api/chat"

    payload = {
        "model": "llama2",
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": prompt}
        ],
        "stream": False
    }

    response = requests.post(url, json=payload)
    response.raise_for_status()

    return response.json()["message"]["content"]

def check_llm_answer(llm_answer: str, correct_answer: str) -> bool:
    return llm_answer.strip().lower() == correct_answer.strip().lower()

def parse_qa_file(file_path):
    qa_pairs = []
    with open(file_path, "r", encoding="utf-8") as f:
        lines = f.read().strip().split('\n')
    
    question = None
    answer = None
    
    for line in lines:
        line = line.strip()
        if line.startswith("Q:"):
            question = line[2:].strip()
        elif line.startswith("A:"):
            answer = line[2:].strip()
            if question is not None:
                qa_pairs.append((question, answer))
                question = None
                answer = None

    return qa_pairs

def write_to_csv(filename, headers, rows):
    import csv

    with open(filename, mode='w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(headers)  # write header
        writer.writerows(rows)    # write data rows

def run_experiment(data_file_name: str, text_data_base_path: str, table_print: bool, csv_file_name: str = None):
    import json
    import os
    import random
    from tabulate import tabulate

    with open(data_file_name, "r") as file:
        data = json.load(file)

    rows = []

    for test_case_category in data["data"]:
        category = test_case_category["category"]
        text_file_name = os.path.join(text_data_base_path, test_case_category["text_file"])
        system_prompt = test_case_category["system_prompt"]
        qa_pairs = parse_qa_file(text_file_name)

        total_gen_questions = len(qa_pairs)  

        if test_case_category.get("sample", None) is not None:
            qa_pairs = random.sample(qa_pairs, test_case_category["sample"])

        correct_question = 0
        total_question = 0

        for question, correct_answer in qa_pairs:
            total_question += 1

            llm_answer = ask_llm(system_prompt, question)
            is_correct = check_llm_answer(llm_answer, correct_answer)

            if is_correct:
                correct_question += 1
            
        percent_correct = (correct_question / total_question) * 100
        rows.append([
            category,
            f"{total_gen_questions}",
            f"{total_question}",
            f"{correct_question}",
            f"{percent_correct:.2f}%"
        ])
    
    headers = ["Category", "# Q's Generated", "# Q's Asked", "# Q's Correct", "% Passed"]
    if table_print:
        print(tabulate(rows, headers=headers, tablefmt="grid"))
    
    if csv_file_name is not None and csv_file_name != "":
        write_to_csv(csv_file_name, headers, rows)

run_experiment("data.json", "./text_data", True, "experiment.csv")