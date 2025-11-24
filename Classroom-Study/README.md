## Description of Artifact

This repository includes study materials, datasets, and analysis scripts that allow users to verify the results of the classroom study (see paper for details).

The repository contains:
- Classroom study materials: consent form, questions, training slides and handouts (see `materials folder`).  
- The statistical scripts and data to verify the results in the paper/thesis (see `statistics`).  

### Directory Structure  

```
README.md                                   # This file  
materials/                  # Folder containing classroom study materials  
│  ├── Consent-Form.pdf                     # Anonymous consent form used
│  ├── LTL-Intervention.pdf                 # Copy of survey including all questions and explanations
│  ├── LTL-Lecture-Final.pdf                # Slide deck used to explain LTL in classroom study
statistics/                                 # Folder containing analysis scripts and data  
│  ├── script-class-analysis.R              # Classroom study results analysis  
│  ├── class-study                          # Data files for classroom study 
│  │  ├── Codebook.xlsx                     # "code book" for the classroom study  
│  │  ├── qualitative-data.csv              # Raw qualitative responses  
│  │  ├── qualitative-data-adjudicated.csv  # Adjudicated qualitative responses  
│  │  ├── qualitative-data-anotated.xlsx    # Notes and interrater reliability data for qualitative responses
│  │  ├── quantitative-data.csv             # Classroom study results  
│  │  ├── raw-anonymous.csv                 # Raw anonymized study data  
│  │  ├── recommendations-data              # Data for recommendation questions
```

## Setup

### Statistics
To analyze results, download and install **R**. For ease of use, we recommend **RStudio**. The analyses in the paper were created using RStudio for OSX (Version: 2024.12.0+467) and R version 4.4.2 (2024-10-31).

Install the following R packages:  

- `conflicted`
- `devtools`
- `dplyr`
- `ggplot2`
- `ggpubr`
- `lawstat`
- `likert`
- `magrittr`
- `PairedData`
- `psych`
- `purrr`
- `readr` 
- `reshape2`
- `rlang`
- `tidyverse`
- `xtable`

## Usage

### Reviewing Data  

Review classroom study data: 

   - `classroom-study/qualitative-data.csv` (student responses)  
   - `classroom-study/qualitative-data-adjudicated.csv` (adjudicated responses)  
   - `classroom-study/Codebook.xlsx` (coding schema)  
   - `survey-materials/` (folder containing the slides and survey)

### Running Statistical Analysis Scripts  

We strongly recommend using RStudio. If you wish to use command line, use the following instructions:

  ```bash
  Rscript statistics/script-class-analysis.R
  ```

If run from the terminal (rather than in RStudio), plots will appear in `Rplots.pdf`. 

## License

This work is licensed under a Creative Commons Attribution-NonCommercial-No Derivative Works 4.0 International License.
