# Systematic LLM Prompt Development for Human-Centric Interpretation of Linear Temporal Logic - Supplemental Material

## Purpose

This repository contains the supplemental information for the REJ'25 submission entitled **"Systematic LLM Prompt Development for Human-Centric Interpretation of Linear Temporal Logic"**. 
This work investigates how prompt factors and large language model (LLM) choice affect the quality of LLM explanations and to what extent these explanations can assist practitioners and novices in interpreting formal requirements expressed in Linear Temporal Logic (LTL).

## Description of Artifact

This repository includes study materials, datasets, prompts, and analysis scripts that allow users to verify the results of the associated paper (see paper for details).

### Abbreviation Key

Filenames use the following letters to identify which model and prompt factor they were generated from.

```
R - DeepSeek
I - Gemini
O - GPT-oss
L - Llama
M - Mistral
Q - Qwen
T - GPT-4o

a - LLM_Base
b-expert - expert role
b-hsteacher - teacher role
b-student - student role
c - guidelines
d - operators
e - example
f - style
g - LLM_Strict
h - LLM_Agg
o - LLM_Init
n - LLM_Exp
```

### Directory Structure  

```
README.md                                   # This file  
interrater_reliability/                     # Folder containing files related to correctness adjudication
│  ├── data/                                # Folder containing files for each llm+prompt condition with each rater's judgement of correctness and the consensus for each formula
│  ├── cohens_kappa.R                       # Code for calculating Cohen's kappa based on raters' adjudications
llm_explanations/
│  ├── data/                                # Folder containing folders of explanations for each llm+prompt condition
│  │  ├── Xx/                               # Folders containing text files for each explanation; folder names indicate the llm+prompt condition (see above Abbreviation Key) and formula number (see formulas.csv)
│  ├── prompting_script-gpt4o.py            # Script for generating LLM explanations with GPT-4o
│  ├── prompting_script.py                  # Script for generating LLM explanations with free models
metrics/                                    # Folder containing files related to readability, coherence, length, and correctness
│  ├── data/                                # Folder containing calculated scores in the form of [metric]-[comparison].csv
│  ├── calculate_metrics.ipynb              # Script for calculating FRE, DC, Coherence, and Length from .txt files; also makes boxplots
│  ├── correctness-stats.R                  # Script for running Fisher Exact Test on correctness based on rater consensus
│  ├── quartiles_and_p.R                    # Script for calculating Q1, median, Q3, and p-values with Wilcoxon signed rank test for readability, coherence, and length
formulas.csv                                # LTL formulas and their corresponding numerical IDs; formulas 1-77 are the 77F dataset
```

## Setup

### Statistics
To analyze results, download and install **R**. For ease of use, we recommend **RStudio**. The analyses in the paper were created using RStudio for OSX (Version: 2023.12.0+369) and R version 4.4.0 (2024-04-24).

Install the following R packages:  

```
dplyr
lawstat
BSDA
```

### Python

For Python-based scripts, install:  

```
time
pandas
openai
datetime
numpy
google-generativeai
mistralai
textstat
re
matplotlib
markdown_text_clean
transformers
torch
scipy
```

## License

This work is licensed under a Creative Commons Attribution-NonCommercial-No Derivative Works 4.0 International License.
