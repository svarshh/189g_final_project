def compute_bleu_score(reference: str, hypothesis: str):
    import sacrebleu

    references = [[reference]]
    hypothesis = [hypothesis]

    bleu_score = sacrebleu.corpus_bleu(hypothesis, references)
    return bleu_score.score

def compute_bert_score(reference: str, hypothesis: str):
    from bert_score import score
    import transformers

    transformers.logging.set_verbosity_error()

    references = [reference]
    hypothesis = [hypothesis]

    P, R, F1 = score(hypothesis, references, lang="en", verbose=False)

    return P.item(), R.item(), F1.item()

def write_to_csv(filename, headers, rows):
    import csv

    with open(filename, mode='w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(headers)  # write header
        writer.writerows(rows)    # write data rows

def run_experiment(data_file_name: str, table_print: bool = True, csv_file_name: str = None):
    import json
    from tabulate import tabulate

    with open(data_file_name, "r") as file:
        data = json.load(file)

    rows = []
    for question_category in data["data"]:
        category = question_category["category"]
        reference_question = question_category["reference_question"]
        system_gen_question = question_category["system_generated_question"]

        bleu_score = compute_bleu_score(reference_question, system_gen_question)

        precision, recall, F1 = compute_bert_score(reference_question, system_gen_question)

        rows.append([
            category,
            f"{bleu_score:.2f}",
            f"{precision:.4f}",
            f"{recall:.4f}",
            f"{F1:.4f}"
        ])

    headers = ["Category", "BLEU Score", "BERT Precision", "BERT Recall", "BERT F1"]
    if table_print:
        print(tabulate(rows, headers=headers, tablefmt="grid"))
    
    if csv_file_name is not None and csv_file_name != "":
        write_to_csv(csv_file_name, headers, rows)

run_experiment("data.json", table_print=True, csv_file_name="experiment.csv")