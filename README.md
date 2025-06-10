
# ⚙️ PromptGen Setup Instructions

## 🛠️ Required Software

To run this project, ensure you have the following installed:

| Software     | Version  | Installation Link                                                                            |
|--------------|----------|----------------------------------------------------------------------------------------------|
| Python       | 3.10     | [Download Python 3.10](https://www.python.org/downloads/release/python-3100/)               |
| SWI-Prolog   | 9.2.9    | [Download SWI-Prolog 9.2.9](https://www.swi-prolog.org/Download.html)                        |
| Ollama       | Latest   | [Download Ollama](https://ollama.com/download)                                              |

> ℹ️ **Tips:**  
> - Use `pyenv` or `conda` to manage Python versions.  
> - Confirm Prolog is installed by running `swipl --version`.  
> - Ollama is required to run the LLaMA 2 7B model locally on your Mac.

---

## 🧠 Download LLaMA 2 Model

After installing [Ollama](https://ollama.com), run the following command in your terminal:

```bash
ollama pull llama2
```

This will download the 7B parameter LLaMA 2 model locally for use in evaluation experiments.

---

## 📦 Python Environment Setup

1. Navigate to the experiment code folder:
   ```bash
   cd code/experiments
   ```

2. Create and activate a virtual environment:
   ```bash
   python3 -m venv venv
   source venv/bin/activate
   ```

3. Install required dependencies:
   ```bash
   pip install -r requirements.txt
   ```

---
## 🧾 Generating Prompt Datasets with PromptGen

To generate new prompt–answer datasets for the experiments, we provide two systems, each designed to support different methods of constraint specification.

---

### 🧠 System 1: Dataset Generation

1. Navigate to the method1 directory:
   ```bash
   cd ../../method1
   ```

2. Run the Prolog driver:
   ```bash
   swipl -s driver.pl
   ```

This will generate question–answer datasets from constraint annotations defined in `annotations.pl`.

---

### 🧠 System 2: Alternative Dataset Generation

1. Navigate to the method2 directory:
   ```bash
   cd ../method2
   ```

2. Open the Prolog interpreter:
   ```bash
   swipl
   ```

3. Load the backend and run the generator:
   ```prolog
   consult("backend.pl").
   generate.
   ```

This will produce additional QA datasets and save them to predefined output files.

---


## 🚀 Running Experiments

Starting off, assume you’re in the `code/experiments` directory and the virtual environment is activated.

---

### 🔹 Experiment 1: BLEU and BERT Score Evaluation


```bash
cd bleu_experiments
python experiment.py
```

Runs BLEU and BERT score evaluations comparing system-generated prompts against baseline prompts, prompt pairs in `data.json`.
Results saved in `experiment.csv`

---

### 🔹 Experiment 2: LLM Evaluation

This experiment evaluates the LLaMA 2 7B model on generated prompts. 

Generated prompts from previous runs are stored in `../llm_experiment/text_data`. Move any new generated prompts into this directory before running.

Update respective JSON files (with and without system prompts) based on the text filenames.

#### Part 1: Without System Prompt

```bash
cd ../llm_experiment
python experiment.py no_system_prompt
```

Evaluates LLaMA 2 without system-level prompt guidance.  
Results are saved in `experiment_no_sys_prompt.csv`.

#### Part 2: With System Prompt

```bash
python experiment.py system_prompt
```

Evaluates LLaMA 2 with system-level prompt guidance to structure the responses.  
Results are saved in `experiment_with_sys_prompt.csv`.

---


