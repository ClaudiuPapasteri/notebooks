# O.1.A   24 feb 2019
library(tidyverse)
library(ggplot2)
library(ggpubr) 
library(broom)

# -----------------------------------------------------------------------------------------------------------------------------
# Read
# -----------------------------------------------------------------------------------------------------------------------------
wd = "C:/Users/Mihai/Desktop/R Notebooks/notebooks/o1a-report"
setwd(wd)
file = "DATE O1A.xlsx"
filepath = file.path(wd, file)

Date <- readxl::read_xlsx(filepath, sheet = "date", skip = 5, col_names = FALSE)

# -----------------------------------------------------------------------------------------------------------------------------
# Clean
# -----------------------------------------------------------------------------------------------------------------------------

# # collapse 2 rows - NO NEED ANY MORE
# mapply(paste, sep = "-", Date[c(T,F),], Date[c(F,T),])
# 
# varnamereplace <- Date[1:2,] %>%
#   rowwise() %>% 
#   str_c() %>% 
#   str_replace_all("([\"])", "") %>% 
#   str_replace_all("([c()])", "") %>% 
#   str_replace(", NA", "") %>% 
#   str_replace(", ", "_")

varnames <- c("Nr_crt", "ID", "Nume_Prenume", "Zi", "Vas_stres_pre", "Vas_bine_pre",
             sprintf("Stais_pre_%01d", seq(1,20)),
             "SOP",
             "IOS_mama", "IOS_tata", "IOS_iubit", "IOS_prieten", "IOS_personalitate",
             "Vas_rel_global", "Vas_rel_arousal",
             "CRQ_1", "CRQ_2", "CRQ_3", "CRQ_4", "CRQ_5", "CRQ_6",
             "Vas_stres_post", "Vas_bine_post",
             sprintf("Stais_post_%01d", seq(1,20))
)
names(Date) <- varnames   # nume noi
Date <- Date[-c(1:2),]    # scoatem randurile cu numele precedente
Date$Nr_crt <- 1:nrow(Date)   # era gol, asa ca numerotam randurile ca sa avem acesta variabila 

# -----------------------------------------------------------------------------------------------------------------------------
# Process NAs
# -----------------------------------------------------------------------------------------------------------------------------
## Scoatem randuri care au doar NA la coloanele relevante (trebuie excluse var de ID pt ca sunt completate degeaba)
# Date %>% 
#   select(-c(Nr_crt, ID, Nume_Prenume, Zi)) %>% 
#   filter_all(any_vars(!is.na(.)))                   # mai bine marcam randurile cu NA-uri multe ca e mai reproductibil
Date <- Date %>% 
    mutate(NA_per_row = rowSums(is.na(.)))     # count NAs by row

Date <- Date %>% 
    filter(NA_per_row < 20)         # arbitrary cutoff for NAs on columns ... it is normal to have 4 NAs for all columns

# -----------------------------------------------------------------------------------------------------------------------------
# Compute new variables 
# -----------------------------------------------------------------------------------------------------------------------------
## Recode CONDITION variable  &  IOS variable
# Date %>% select(Nr_crt, IOS_mama, IOS_tata, IOS_iubit, IOS_prieten, IOS_personalitate) %>% 
#   gather(type, value, -Nr_crt) %>% # na.omit() %>% 
#   select(-value) %>% arrange(Nr_crt)
Conditie <- Date %>% 
    select(Nr_crt, ID, IOS_mama, IOS_tata, IOS_iubit, IOS_prieten, IOS_personalitate) %>% 
    gather(type, value, -c(Nr_crt, ID)) %>% 
    mutate(Conditie = ifelse(!is.na(value), type, NA) ) %>%
    mutate(Conditie = str_replace(Conditie, "IOS_", "")) %>%
    arrange(Nr_crt) %>%
    select(Conditie) %>% na.omit() 
Date$Conditie <- Conditie$Conditie     # tidyverse returns tibble, must do this
IOS <- Date %>% 
  mutate(IOS = coalesce(IOS_mama, IOS_tata, IOS_iubit, IOS_prieten, IOS_personalitate)) %>%
  select(IOS)
Date$IOS <- IOS$IOS   # tidyverse returns tibble, must do this
rm(Conditie, IOS)    # remove 2 tibbles

# -----------------------------------------------------------------------------------------------------------------------------
# Scoring Stai   (si convert in numeric la VAS)
# ----------------------------------------------------------------------------------------------------------------------------- 
# colnames(Date)[7:26]
# colnames(Date)[43:62]
# type.convert(Date) 


itemiVAS <- c(5, 6, 41, 42)

itemiStaiS_pre <- 7:26
itemiStaiS_post <- 43:62
ReversedItems <- c(1,2,5,8,10,11,15,16,19,20)

Date <- Date %>%                 
  replace(Date == "na", NA) %>%        # scimbam codarea cu na a Doinei
  mutate_at(vars(itemiStaiS_pre), funs(as.numeric)) %>%        # facem coloanele numerice pt STAI
  mutate_at(vars(itemiStaiS_post), funs(as.numeric)) %>% 
  mutate_at(vars(itemiVAS), funs(as.numeric))

Date[ ,itemiStaiS_pre[ReversedItems]] = 5 - Date[ ,itemiStaiS_pre[ReversedItems]]
Date[ ,itemiStaiS_post[ReversedItems]] = 5 - Date[ ,itemiStaiS_post[ReversedItems]]

Date$StaiS_pre = rowSums(Date[ ,itemiStaiS_pre], na.rm=T ) * NA ^ (rowSums(!is.na(Date[ ,itemiStaiS_pre])) == 0)
Date$StaiS_post = rowSums(Date[ ,itemiStaiS_post], na.rm=T ) * NA ^ (rowSums(!is.na(Date[ ,itemiStaiS_post])) == 0)

# -----------------------------------------------------------------------------------------------------------------------------
# Statistici
# -----------------------------------------------------------------------------------------------------------------------------

## IOS doar de fun ----------------
ggplot(aes(x = Conditie, y = as.numeric(as.character(IOS)), data = Date)) + 
       geom_boxplot() +
       geom_point(aes(color = ID), position = position_dodge(0.4)) +
       geom_line(aes(color = ID, group = ID), position = position_dodge(0.4)) + 
       theme(legend.position="none")

## STAI
Staimelt <- Date[, c("ID", "Conditie", "StaiS_pre","StaiS_post")] %>% 
  gather("StaiS_pre", "StaiS_post", key = "Stai_cond", value = "value") %>% 
  mutate_at(vars(c(1, 2,3)), funs(as.factor)) %>% 
  mutate(Stai_cond = factor(Stai_cond, levels = c("StaiS_pre","StaiS_post"))) # %>%    # change factor order for plot pre, post
 # mutate(Conditie = factor(Conditie, levels = levels(Conditie)[c(3, 2, 5, 1, 4)] ))  

ggplot(aes(x = Conditie, y = value, fill = Stai_cond), data = Staimelt) + 
  geom_boxplot() 

 
ggplot(Staimelt, aes(x = Stai_cond, y = value)) +
  geom_boxplot() +
  facet_wrap(~Conditie) +
  ggpubr::stat_compare_means(method = "t.test", paired = TRUE, comparisons = list(c("StaiS_pre","StaiS_post")))


Date %>% 
  group_by(Conditie) %>% 
  do(broom::tidy(t.test(.$StaiS_pre, 
                        .$StaiS_post, 
                        mu = 0, 
                        alt = "two.sided", 
                        paired = TRUE, 
                        conf.level = 0.95)))  

 
Date %>% 
  group_by(Conditie) %>% 
  dplyr::summarize(mean_pre = mean(StaiS_pre, na.rm=TRUE),
                   mean_post = mean(StaiS_post, na.rm=TRUE),
                   mean_diff = mean(StaiS_pre, na.rm=TRUE) - mean(StaiS_post, na.rm=TRUE),   # m_pre - m_post
                   sd_pooled = sd(c(StaiS_pre, StaiS_post), na.rm=TRUE),                     # pooled sd
                   n_pre = sum(!is.na(StaiS_pre)),                                           # n 
                   n_post = sum(!is.na(StaiS_post))) %>%                                     # n 
  rowwise()  %>%                   
  do(broom::tidy(power.t.test(power = .8, 
                              delta =  .$mean_diff,
                              sd =   .$sd_pooled,
                              sig.level = .05,
                              type = "paired",
                              alternative = "two.sided")))                                  # determine sample size requirement


# aov_stai <- aov(before ~ sex, data = Data)    # Two way between ANOVA
aov_stai_within <- aov(value ~ Stai_cond + Error(ID/Stai_cond), data = Staimelt) # One-way within ANOVA
summary(aov_stai_within)
# aov_stai_mix <- aov(value ~ x*Stai_cond + Error(ID/Stai_cond), data = Staimelt) # Mixed design ANOVA - x e subject level ca age
# aov_stai_mix <- aov(value ~ Conditie*Stai_cond + Error(ID/Stai_cond), data = Staimelt) # Mixed design ANOVA



## VAS_stres ----------------
Vasstresmelt <- Date[, c("ID", "Conditie", "Vas_stres_pre","Vas_stres_post")] %>% 
  gather("Vas_stres_pre","Vas_stres_post", key = "Vas_stres_cond", value = "value") %>% 
  mutate_at(vars(c(1,2,3)), funs(as.factor)) %>% 
  mutate_at(vars(c(4)), funs(as.numeric)) %>% 
  mutate(Vas_stres_cond = factor(Vas_stres_cond, levels = c("Vas_stres_pre","Vas_stres_post"))) # change factor order for plot pre, post

ggplot(aes(x = Conditie, y = value, fill = Vas_stres_cond), data = Vasstresmelt) + 
  geom_boxplot() 

ggplot(Vasstresmelt, aes(x = Vas_stres_cond, y = value)) +
  geom_boxplot() +
  facet_wrap(~Conditie) +
  ggpubr::stat_compare_means(method = "t.test", paired = TRUE, comparisons = list(c("Vas_stres_pre","Vas_stres_post")))

Date %>% 
  group_by(Conditie) %>% 
  do(broom::tidy(t.test(.$Vas_stres_pre, 
                        .$Vas_stres_post, 
                        mu = 0, 
                        alt = "two.sided", 
                        paired = TRUE, 
                        conf.level = 0.95)))  


Date %>% 
  group_by(Conditie) %>% 
  dplyr::summarize(mean_pre = mean(Vas_stres_pre, na.rm=TRUE),
                   mean_post = mean(Vas_stres_post, na.rm=TRUE),
                   mean_diff = mean(Vas_stres_pre, na.rm=TRUE) - mean(Vas_stres_post, na.rm=TRUE),   # m_pre - m_post
                   sd_pooled = sd(c(Vas_stres_pre, Vas_stres_post), na.rm=TRUE),                     # pooled sd
                   n_pre = sum(!is.na(Vas_stres_pre)),                                           # n 
                   n_post = sum(!is.na(Vas_stres_post))) %>%                                     # n 
  rowwise()  %>%                   
  do(broom::tidy(power.t.test(power = .8, 
                              delta =  .$mean_diff,
                              sd =   .$sd_pooled,
                              sig.level = .05,
                              type = "paired",
                              alternative = "two.sided")))                                  # determine sample size requirement


## Vas_bine ----------------
Vasbinemelt <- Date[, c("ID", "Conditie", "Vas_bine_pre","Vas_bine_post")] %>% 
  gather("Vas_bine_pre","Vas_bine_post", key = "Vas_stres_cond", value = "value") %>% 
  mutate_at(vars(c(1,2,3)), funs(as.factor)) %>% 
  mutate_at(vars(c(4)), funs(as.numeric)) %>% 
  mutate(Vas_stres_cond = factor(Vas_stres_cond, levels = c("Vas_bine_pre","Vas_bine_post"))) # change factor order for plot pre, post

ggplot(aes(x = Conditie, y = value, fill = Vas_stres_cond), data = Vasbinemelt) + 
  geom_boxplot() 

ggplot(Vasbinemelt, aes(x = Vas_stres_cond, y = value)) +
  geom_boxplot() +
  facet_wrap(~Conditie) +
  ggpubr::stat_compare_means(method = "t.test", paired = TRUE, comparisons = list(c("Vas_bine_pre","Vas_bine_post")))

Date %>% 
  group_by(Conditie) %>% 
  do(broom::tidy(t.test(.$Vas_bine_pre, 
                        .$Vas_bine_post, 
                        mu = 0, 
                        alt = "two.sided", 
                        paired = TRUE, 
                        conf.level = 0.95))) 

Date %>% 
  group_by(Conditie) %>% 
  dplyr::summarize(mean_pre = mean(Vas_bine_pre, na.rm=TRUE),
                   mean_post = mean(Vas_bine_post, na.rm=TRUE),
                   mean_diff = mean(Vas_bine_pre, na.rm=TRUE) - mean(Vas_bine_post, na.rm=TRUE),   # m_pre - m_post
                   sd_pooled = sd(c(Vas_bine_pre, Vas_bine_post), na.rm=TRUE),                     # pooled sd
                   n_pre = sum(!is.na(Vas_bine_pre)),                                           # n 
                   n_post = sum(!is.na(Vas_bine_post))) %>%                                     # n 
  rowwise()  %>%                   
  do(broom::tidy(power.t.test(power = .8, 
                              delta =  .$mean_diff,
                              sd =   .$sd_pooled,
                              sig.level = .05,
                              type = "paired",
                              alternative = "two.sided")))                                  # determine sample size requirement


# Date %>% 
#   select(ID, Conditie, Vas_bine_pre, Vas_bine_post) %>%       # ca idee cati oameni intra pe conditie
#   group_by(Conditie) %>% count()


