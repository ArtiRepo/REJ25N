cohens_kappa <- function(files) {
  both <- 0
  neither <- 0
  r1_only <- 0
  r2_only <- 0
  for (file in files) {
    ratings <- read.csv(file)
    codes <- paste0(ratings$Rater1, ratings$Rater2)

    both <- both + length(which(codes == "11"))
    neither <- neither + length(which(codes == "00"))
    r1_only <- r1_only + length(which(codes == "10"))
    r2_only <- r2_only + length(which(codes == "01"))
  }
  total <- both + neither + r1_only + r2_only
  p_agree <- (neither + both) / total
  p_rand_corr <- ((both + r1_only) / total) * ((both + r2_only) / total)
  p_rand_incorr <- ((neither + r1_only) / total) * ((neither + r1_only) / total)
  p_rand_agree <- p_rand_corr + p_rand_incorr
  (p_agree - p_rand_agree) / (1 - p_rand_agree)
}


path <- "./data/"

# base
files <- c(
  paste0(path,"Ra-raters.csv"),
           paste0(path,"Ia-raters.csv"),
           paste0(path,"Oa-raters.csv"),
           paste0(path,"La-raters.csv"),
           paste0(path,"Ma-raters.csv"),
           paste0(path,"Qa-raters.csv"),
           paste0(path,"Rh-raters.csv"),
           paste0(path,"Ih-raters.csv"),
           paste0(path,"Oh-raters.csv"),
           paste0(path,"Lh-raters.csv"),
           paste0(path,"Mh-raters.csv"),
           paste0(path,"Qh-raters.csv"),
           paste0(path,"Rb-expert-raters.csv"),
           paste0(path,"Rb-hsteacher-raters.csv"),
           paste0(path,"Rb-student-raters.csv"),
           paste0(path,"Rc-raters.csv"),
           paste0(path,"Rd-raters.csv"),
           paste0(path,"Re-raters.csv"),
           paste0(path,"Rf-raters.csv"),
           paste0(path,"Rn-raters.csv"),
           paste0(path,"Ro-raters.csv")
           )
cohens_kappa(files)

