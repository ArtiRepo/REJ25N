library(dplyr)
library(lawstat)
library(BSDA)

setwd("./data/")

get_quartiles <- function(df) {
  q1 <- df %>%
    sapply(function(x) quantile(x, probs = 0.25))
  
  q2 <- df %>%
    apply(2, median)
  
  q3 <- df %>%
    sapply(function(x) quantile(x, probs = 0.75))
  
  data.frame(q1, q2, q3)
}

get_p <- function(df, standard) {
  df %>% 
    sapply(function(x) wilcox.test(standard, c(x), paired = TRUE))
}

symm_assumption <- function(df, standard) {
  df %>% 
    sapply(function(x) symmetry.test(c(standard) - c(x), option = c("MGG", "CM", "M")))
}

## PROMPT ####################################

## FRE

fre_prompt <- read.csv("fre-prompt.csv")
get_quartiles(fre_prompt)
symm_assumption(select(fre_prompt, -Ra), fre_prompt$Ra)
get_p(select(fre_prompt, -Ra), fre_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rb.expert))
SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rf))

## DC

dc_prompt <- read.csv("dc-prompt.csv")
get_quartiles(dc_prompt)
symm_assumption(select(dc_prompt, -Ra), dc_prompt$Ra)
get_p(select(dc_prompt, -Ra), dc_prompt$Ra)

## COHERENCE

coh_prompt <- read.csv("coh-prompt.csv")
get_quartiles(coh_prompt)
symm_assumption(select(coh_prompt, -Ra), coh_prompt$Ra)
get_p(select(coh_prompt, -Ra), coh_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.expert))
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.student))
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.hsteacher))

## LENGTH

len_prompt <- read.csv("len-prompt.csv")
get_quartiles(len_prompt)
symm_assumption(select(len_prompt, -Ra), len_prompt$Ra)
get_p(select(len_prompt, -Ra), len_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(len_prompt$Ra), c(len_prompt$Rb.hsteacher))

## LLM  ########################################

get_p <- function(df, col1, col2) {
  if (symmetry.test(c(df[[col1]]) - c(df[[col2]]), option = c("MGG", "CM", "M"))$p.value > 0.05) {
    p <- wilcox.test(df[[col1]], c(df[[col2]]), paired = TRUE)
  } else {
    p <- SIGN.test(df[[col1]], c(df[[col2]]))
  }
  p
}

## FRE

fre_llm <- read.csv("fre-llm.csv")
get_quartiles(fre_llm)

# get p-values for each model
get_p(fre_llm, "Ra", "Rh")
get_p(fre_llm, "Ia", "Ih")
get_p(fre_llm, "Oa", "Oh")
get_p(fre_llm, "La", "Lh")
get_p(fre_llm, "Ma", "Mh")
get_p(fre_llm, "Qa", "Qh")

## DC

dc_llm <- read.csv("dc-llm.csv")
get_quartiles(dc_llm)

# get p-values for each model
get_p(dc_llm, "Ra", "Rh")
get_p(dc_llm, "Ia", "Ih")
get_p(dc_llm, "Oa", "Oh")
get_p(dc_llm, "La", "Lh")
get_p(dc_llm, "Ma", "Mh")
get_p(dc_llm, "Qa", "Qh")

## COHERENCE

coh_llm <- read.csv("coh-llm.csv")
get_quartiles(coh_llm)

# get p-values for each model
get_p(coh_llm, "Ra", "Rh")
get_p(coh_llm, "Ia", "Ih")
get_p(coh_llm, "Oa", "Oh")
get_p(coh_llm, "La", "Lh")
get_p(coh_llm, "Ma", "Mh")
get_p(coh_llm, "Qa", "Qh")

## LENGTH

len_llm <- read.csv("len-llm.csv")
get_quartiles(len_llm)

# get p-values for each model
get_p(len_llm, "Ra", "Rh")
get_p(len_llm, "Ia", "Ih")
get_p(len_llm, "Oa", "Oh")
get_p(len_llm, "La", "Lh")
get_p(len_llm, "Ma", "Mh")
get_p(len_llm, "Qa", "Qh")

## FOR GPT-4o EXPLANATIONS #####################

# LEN

len_gpt4o <- read.csv("len_gpt4o.csv")
get_quartiles(len_gpt4o)

get_p(len_gpt4o, "o", "n")

# COH

coh_gpt4o <- read.csv("coh_gpt4o.csv") 
get_quartiles(len_gpt4o)

get_p(len_gpt4o, "o", "n")

# FRE

fre_gpt4o <- read.csv("fre_gpt4o.csv") 
get_quartiles(fre_gpt4o)

get_p(fre_gpt4o, "o", "n")

# DC

dc_gpt4o <- read.csv("dc_gpt4o.csv") 
get_quartiles(dc_gpt4o)

get_p(dc_gpt4o, "o", "n")

## FOR GPT-4o SUMMARIES #####################

# FRE

fre_gpt4o_summ <- read.csv("fre_gpt4o_summ.csv") 
get_quartiles(fre_gpt4o)

get_p(fre_gpt4o_summ, "o", "n")

# DC

dc_gpt4o_summ <- read.csv("dc_gpt4o_summ.csv") 
get_quartiles(dc_gpt4o_summ)

get_p(dc_gpt4o_summ, "o", "n")

# LEN

len_gpt4o_summ <- read.csv("len_gpt4o_summ.csv")
get_quartiles(len_gpt4o_summ)

get_p(len_gpt4o_summ, "o", "n")