library(dplyr)
library(lawstat)
library(BSDA)
library(rstatix)
library(tidyr)

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

get_z <- function(df, condition1, condition2) {
  df <- df %>%
    select(c(condition1, condition2)) # select data for the two comparison conditions
  df$ID <- 1:nrow(df) # add IDs
  df_long <- pivot_longer(df, cols=-ID, names_to="condition", values_to="score")
  df_long$condition <- factor(df_long$condition)
  df_long$ID <- factor(df_long$ID)
  z <- coin::wilcoxsign_test(formula = score ~ condition | ID, data = df_long) %>%
    coin::statistic(type="standardized") %>%
    as.numeric() # z score
  if (condition2 > condition1) { # this function assigns pre and post group based on alphabetical columns
    z <- -z
  }
  data.frame(condition1 = condition1, condition2 = condition2, z = z)
}

# procedure from https://pmc.ncbi.nlm.nih.gov/articles/PMC12701665/
get_eff_size <- function(df, condition1, condition2) {
  df <- df %>%
    select(c(condition1, condition2)) # select data for the two comparison conditions
  df$ID <- 1:nrow(df) # add IDs
  df_long <- pivot_longer(df, cols=-ID, names_to="condition", values_to="score")
  df_long$condition <- factor(df_long$condition)
  df_long$ID <- factor(df_long$ID)
  z <- coin::wilcoxsign_test(formula = score ~ condition | ID, data = df_long) %>%
    coin::statistic(type="standardized") %>%
    as.numeric() # z score
  r <- -z / sqrt(nrow(df_long)) # r = z / sqrt(N)
  size <- case_when(
    abs(r) >= 0.5 ~ "large",
    abs(r) >= 0.3~ "medium",
    abs(r) >= 0.1 ~ "small",
    TRUE ~ "none"
  )
  data.frame(condition1 = condition1, condition2 = condition2, r = r, eff_size = size)
}

z_for_sign_test <- function(x, n=77) {
  (x - (n/2)) / sqrt(n/4)
}

## PROMPT ####################################

## FRE

fre_prompt <- read.csv("fre-prompt.csv")
get_quartiles(fre_prompt)
symm_assumption(select(fre_prompt, -Ra), fre_prompt$Ra)
get_p(select(fre_prompt, -Ra), fre_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rb.expert))
z_for_sign_test(as.numeric(SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rb.expert))$statistic))
SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rf))
z_for_sign_test(as.numeric(SIGN.test(c(fre_prompt$Ra), c(fre_prompt$Rf))$statistic))

# get z
fre_prompt_z <- data.frame(
  condition1 = character(),
  condition2 = character(),
  z = numeric()
)
for (col in colnames(fre_prompt)[colnames(fre_prompt) != "ID" & colnames(fre_prompt) != "Ra"]) {
  fre_prompt_z <- rbind(fre_prompt_z, get_z(fre_prompt, "Ra", col))
}
fre_prompt_z

# get effect size
fre_prompt_eff_size <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric(),
  eff_size = character()
)
for (col in colnames(fre_prompt)[colnames(fre_prompt) != "ID" & colnames(fre_prompt) != "Ra"]) {
  fre_prompt_eff_size <- rbind(fre_prompt_eff_size, get_eff_size(fre_prompt, "Ra", col))
}
fre_prompt_eff_size

## DC

dc_prompt <- read.csv("dc-prompt.csv")
get_quartiles(dc_prompt)
symm_assumption(select(dc_prompt, -Ra), dc_prompt$Ra)
get_p(select(dc_prompt, -Ra), dc_prompt$Ra)

# get z
dc_prompt_z <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric()
)
for (col in colnames(dc_prompt)[colnames(dc_prompt) != "ID" & colnames(dc_prompt) != "Ra"]) {
  dc_prompt_z <- rbind(dc_prompt_z, get_z(dc_prompt, "Ra", col))
}
dc_prompt_z

# get effect size
dc_prompt_eff_size <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric(),
  eff_size = character()
)
for (col in colnames(dc_prompt)[colnames(dc_prompt) != "ID" & colnames(dc_prompt) != "Ra"]) {
  dc_prompt_eff_size <- rbind(dc_prompt_eff_size, get_eff_size(dc_prompt, "Ra", col))
}
dc_prompt_eff_size

## COHERENCE

coh_prompt <- read.csv("coh-prompt.csv")
get_quartiles(coh_prompt)
symm_assumption(select(coh_prompt, -Ra), coh_prompt$Ra)
get_p(select(coh_prompt, -Ra), coh_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.expert))
z_for_sign_test(as.numeric(SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.expert))$statistic))
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.student))
z_for_sign_test(as.numeric(SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.student))$statistic))
SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.hsteacher))
z_for_sign_test(as.numeric(SIGN.test(c(coh_prompt$Ra), c(coh_prompt$Rb.hsteacher))$statistic))

# get z
coh_prompt_z <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric()
)
for (col in colnames(coh_prompt)[colnames(coh_prompt) != "ID" & colnames(coh_prompt) != "Ra"]) {
  coh_prompt_z <- rbind(coh_prompt_z, get_z(coh_prompt, "Ra", col))
}
coh_prompt_z

# get effect size
coh_prompt_eff_size <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric(),
  eff_size = character()
)
for (col in colnames(coh_prompt)[colnames(coh_prompt) != "ID" & colnames(coh_prompt) != "Ra"]) {
  coh_prompt_eff_size <- rbind(coh_prompt_eff_size, get_eff_size(coh_prompt, "Ra", col))
}
coh_prompt_eff_size

## LENGTH

len_prompt <- read.csv("len-prompt.csv")
get_quartiles(len_prompt)
symm_assumption(select(len_prompt, -Ra), len_prompt$Ra)
get_p(select(len_prompt, -Ra), len_prompt$Ra)

# sign test for those that violate symmetry assumption
SIGN.test(c(len_prompt$Ra), c(len_prompt$Rb.hsteacher))
z_for_sign_test(as.numeric(SIGN.test(c(len_prompt$Ra), c(len_prompt$Rb.hsteacher))$statistic))

# get z
len_prompt_z <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric()
)
for (col in colnames(len_prompt)[colnames(len_prompt) != "ID" & colnames(len_prompt) != "Ra"]) {
  len_prompt_z <- rbind(len_prompt_z, get_z(len_prompt, "Ra", col))
}
len_prompt_z

# get effect size
len_prompt_eff_size <- data.frame(
  condition1 = character(),
  condition2 = character(),
  r = numeric(),
  eff_size = character()
)
for (col in colnames(len_prompt)[colnames(len_prompt) != "ID" & colnames(len_prompt) != "Ra"]) {
  len_prompt_eff_size <- rbind(len_prompt_eff_size, get_eff_size(len_prompt, "Ra", col))
}
len_prompt_eff_size

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

# z
fre_llm_z <- rbind(get_z(fre_llm, "Ra", "Rh"),
                          get_z(fre_llm, "Ia", "Ih"),
                          get_z(fre_llm, "Oa", "Oh"),
                          get_z(fre_llm, "La", "Lh"),
                          get_z(fre_llm, "Ma", "Mh"),
                          get_z(fre_llm, "Qa", "Qh"))
fre_llm_z

# get effect size
fre_llm_eff_size <- rbind(get_eff_size(fre_llm, "Ra", "Rh"),
                          get_eff_size(fre_llm, "Ia", "Ih"),
                          get_eff_size(fre_llm, "Oa", "Oh"),
                          get_eff_size(fre_llm, "La", "Lh"),
                          get_eff_size(fre_llm, "Ma", "Mh"),
                          get_eff_size(fre_llm, "Qa", "Qh"))
fre_llm_eff_size

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

# z
dc_llm_z <- rbind(get_z(dc_llm, "Ra", "Rh"),
                   get_z(dc_llm, "Ia", "Ih"),
                   get_z(dc_llm, "Oa", "Oh"),
                   get_z(dc_llm, "La", "Lh"),
                   get_z(dc_llm, "Ma", "Mh"),
                   get_z(dc_llm, "Qa", "Qh"))
dc_llm_z

# get effect size
dc_llm_eff_size <- rbind(get_eff_size(dc_llm, "Ra", "Rh"),
                          get_eff_size(dc_llm, "Ia", "Ih"),
                          get_eff_size(dc_llm, "Oa", "Oh"),
                          get_eff_size(dc_llm, "La", "Lh"),
                          get_eff_size(dc_llm, "Ma", "Mh"),
                          get_eff_size(dc_llm, "Qa", "Qh"))
dc_llm_eff_size

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

z_for_sign_test(as.numeric(SIGN.test(coh_llm[["La"]], c(coh_llm[["Lh"]]))$statistic))

# get z
coh_llm_z <- rbind(get_z(coh_llm, "Ra", "Rh"),
                   get_z(coh_llm, "Ia", "Ih"),
                   get_z(coh_llm, "Oa", "Oh"),
                   get_z(coh_llm, "La", "Lh"),
                   get_z(coh_llm, "Ma", "Mh"),
                   get_z(coh_llm, "Qa", "Qh"))
coh_llm_z

# get effect size
coh_llm_eff_size <- rbind(get_eff_size(coh_llm, "Ra", "Rh"),
                         get_eff_size(coh_llm, "Ia", "Ih"),
                         get_eff_size(coh_llm, "Oa", "Oh"),
                         get_eff_size(coh_llm, "La", "Lh"),
                         get_eff_size(coh_llm, "Ma", "Mh"),
                         get_eff_size(coh_llm, "Qa", "Qh"))
coh_llm_eff_size

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

z_for_sign_test(as.numeric(SIGN.test(len_llm[["La"]], c(len_llm[["Lh"]]))$statistic))

# get z
len_llm_z <- rbind(get_z(len_llm, "Ra", "Rh"),
                   get_z(len_llm, "Ia", "Ih"),
                   get_z(len_llm, "Oa", "Oh"),
                   get_z(len_llm, "La", "Lh"),
                   get_z(len_llm, "Ma", "Mh"),
                   get_z(len_llm, "Qa", "Qh"))
len_llm_z

# get effect size
len_llm_eff_size <- rbind(get_eff_size(len_llm, "Ra", "Rh"),
                          get_eff_size(len_llm, "Ia", "Ih"),
                          get_eff_size(len_llm, "Oa", "Oh"),
                          get_eff_size(len_llm, "La", "Lh"),
                          get_eff_size(len_llm, "Ma", "Mh"),
                          get_eff_size(len_llm, "Qa", "Qh"))
len_llm_eff_size

## FOR GPT-4o EXPLANATIONS #####################

# LEN

len_gpt4o <- read.csv("len_gpt4o.csv")
get_quartiles(len_gpt4o)

wilcox.test(len_gpt4o$To, len_gpt4o$Tn)
get_z(len_gpt4o, "To", "Tn")

# get effect size
get_eff_size(len_gpt4o, "To", "Tn")

# COH

coh_gpt4o <- read.csv("coh_gpt4o.csv") 
get_quartiles(coh_gpt4o)

wilcox.test(coh_gpt4o$To, coh_gpt4o$Tn)
get_z(coh_gpt4o, "To", "Tn")

# get effect size
get_eff_size(coh_gpt4o, "To", "Tn")

# FRE

fre_gpt4o <- read.csv("fre_gpt4o.csv") 
get_quartiles(fre_gpt4o)

wilcox.test(fre_gpt4o$To, fre_gpt4o$Tn)

get_z(fre_gpt4o, "To", "Tn")

# get effect size
get_eff_size(fre_gpt4o, "To", "Tn")

# DC

dc_gpt4o <- read.csv("dc_gpt4o.csv") 
get_quartiles(dc_gpt4o)

wilcox.test(dc_gpt4o$o, dc_gpt4o$n)

get_z(dc_gpt4o, "To", "Tn")

# get effect size
get_eff_size(dc_gpt4o, "To", "Tn")

## FOR GPT-4o SUMMARIES #####################

# FRE

fre_gpt4o_summ <- read.csv("fre_gpt4o_summ.csv") 
get_quartiles(fre_gpt4o)

get_p(fre_gpt4o_summ, "o", "n")

# get effect size
fre_gpt4o_summ_eff_size <- get_eff_size(fre_gpt4o_summ, "o", "n")

# DC

dc_gpt4o_summ <- read.csv("dc_gpt4o_summ.csv") 
get_quartiles(dc_gpt4o_summ)

get_p(dc_gpt4o_summ, "o", "n")

# get effect size
dc_gpt4o_summ_eff_size <- get_eff_size(dc_gpt4o_summ, "o", "n")

# LEN

len_gpt4o_summ <- read.csv("len_gpt4o_summ.csv")
get_quartiles(len_gpt4o_summ)

get_p(len_gpt4o_summ, "o", "n")

# get effect size
len_gpt4o_summ_eff_size <- get_eff_size(len_gpt4o_summ, "o", "n")
