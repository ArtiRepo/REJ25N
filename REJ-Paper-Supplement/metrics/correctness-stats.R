path <- "../interrater_reliability/data/"

odds_ratio <- function(base, new, base_total=77, new_total=77) {
  fisher.test(
    matrix(c(new, base, new_total-new, base_total-base),
           nrow = 2,
           dimnames = list(Guess = c("other", "standard"),
                           Truth = c("correct", "incorrect")))
  )
}

# DeepSeek
a <- read.csv(paste0(path,"Ra-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Rh-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))

# Gemini
a <- read.csv(paste0(path,"Ia-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Ih-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))

# GPT
a <- read.csv(paste0(path,"Oa-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Oh-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))

# Llama
a <- read.csv(paste0(path,"La-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Lh-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))

# Mistral
a <- read.csv(paste0(path,"Ma-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Mh-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))

# Qwen
a <- read.csv(paste0(path,"Qa-raters.csv"))$Consensus[0:77]
h <- read.csv(paste0(path,"Qh-raters.csv"))$Consensus[0:77]
odds_ratio(sum(a), sum(h))
