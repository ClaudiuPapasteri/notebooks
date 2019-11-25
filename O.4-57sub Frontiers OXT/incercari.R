Data_merged_wide_all %>%
  filter(!ID %in% c(19, 43)) %>%
  filter(Nr_zi_OGL == 1) %>%
  filter(Gen == "f") %>%
  #summarise(counts = sum(Gen == "f", na.rm = TRUE))
  func_t_box("ID", "OXT_Pre_OGL", "OXT_Post_OGL")
#########################################################
# Overall Pre-Post (pe genuri nu e nimic)

vars_OglEcran <- grep("OGL|ECRAN", colnames(Data_merged_wide_all))

Data_notOglEcran_2 <-
  Data_merged_wide_all %>%
  gather(variable, value, vars_OglEcran, -c(ID)) %>%                                      # gather Data and Nr_zi as these are OglEcran level
  tidyr::separate(variable,  c("variable", "OglEcran"), "_(?=[^_]+$)") %>%                # split only on last "_"
  spread(variable, value) %>% 
  rename_at(vars(vars_OglEcran), list(~stringr::str_remove_all(., c("_OGL|_ECRAN")))) %>%   
  mutate_at(vars(c(45:46, 48:53)), list(~as.numeric(as.character(.)))) %>%
  mutate(Diff_OXT = OXT_Post - OXT_Pre,
         Diff_Vas_Stres = Vas_Stres_Post - Vas_Stres_Pre,
         Diff_Vas_Bine = Vas_Bine_Post - Vas_Bine_Pre
  ) %>%
  arrange(ID)

Correlations_With_One(Data_notOglEcran_2, variable = "Diff_OXT", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # Stres, C






############################# 
wilcox.test(Data_difscores$Diff_OXT_OGL, Data_difscores$Diff_OXT_ECRAN, paired = TRUE)  # nope

Data_difscores %>% 
  select(Diff_OXT_OGL) %>% 
  summarise(counts = sum(Diff_OXT_OGL > 0, na.rm = TRUE))    # 33/55, 17/55 < 0, 5 NA

Data_difscores %>% 
  select(Diff_OXT_ECRAN) %>% 
  summarise(counts = sum(Diff_OXT_ECRAN > 0, na.rm = TRUE))  # 32/55, 18/55 < 0, 5 NA


df_OXTdecrese <- 
  Data_difscores %>% 
  filter(Diff_OXT_OGL < 0)


df_BDIexclude <-
  Data_difscores %>%
  filter(ScorBDI < 13)   # 26/38, 12/38 < 0, 0 NA



func_t_box(df_BDIexclude, "ID", "Vas_Stres_Pre_OGL", "Vas_Stres_Post_OGL") 
func_t_box(df_BDIexclude, "ID", "OXT_Pre_OGL", "OXT_Post_OGL") 


func_t_box(Data_difscores, "ID", "OXT_Pre_OGL", "OXT_Post_OGL") 
func_t_box(Data_difscores, "ID", "OXT_Pre_ECRAN", "OXT_Post_ECRAN")


############################# 
bla <- 
  Data_difscores %>% 
  filter(Nr_zi_OGL == 2)


rez1 <- lm(OXT_Post_ECRAN ~ OXT_Pre_ECRAN + E, data = Data_difscores);  par(mfrow=c(1,2)); termplot(rez1,partial.resid = TRUE, se = TRUE)
pterms1 <- predict(rez1, type="terms"); partial.residuals1 <- as.data.frame( apply(pterms1,2,function(x)x+resid(rez1)) )
############################




df_lm_e <-
  Data_merged_wide_all %>%
  mutate(Varsta = as.numeric(as.character(Varsta))) %>%         # var interes: Gen, Varsta, E, C (nu iese deloc), Stres, IOS
  filter(Varsta < 35) %>%
  filter(!ID %in% c(19, 43)) %>%
  select("ID", "OXT_Pre_OGL", "OXT_Post_OGL", "OXT_Pre_ECRAN", "OXT_Post_ECRAN", "StaiSbrut", "Gen") %>%
  tidyr::drop_na() %>%
  gather("OXT_Pre_OGL", "OXT_Post_OGL", "OXT_Pre_ECRAN", "OXT_Post_ECRAN", key = "Cond", value = "value") %>%
  mutate(PrePost = case_when(stringr::str_detect(.$"Cond", "Pre") ~ "Pre",
                             stringr::str_detect(.$"Cond", "Post") ~ "Post",
                             TRUE ~ NA_character_),
         OglEcran = case_when(stringr::str_detect(.$"Cond", "OGL") ~ "OGL",
                              stringr::str_detect(.$"Cond", "ECRAN") ~ "ECRAN",
                              TRUE ~ NA_character_)) %>%
  mutate(PrePost = as.factor(PrePost),
         OglEcran = as.factor(OglEcran)) %>%
  select(-"Cond") %>%
  spread("PrePost", "value")

summary(lm(Post ~ Pre * Gen + OglEcran, data = df_lm_e))

jmv::ancova(
  formula = Post ~ Pre * OglEcran * E,
  data = df_lm_e,
  homo = TRUE,
  ss = "3",
  postHoc = ~ OglEcran,
  emMeans = ~ OglEcran,
  postHocCorr = list("tukey"),
  effectSize = list("eta", "partEta"),
  emmPlots = TRUE, emmPlotData = TRUE, emmPlotError = "ci"
)


############################

df_anova_e <- 
  df_lm_e %>%
  mutate(Dif = Post - Pre)

jmv::ancova(
  formula = Dif ~ OglEcran * E,
  data = df_anova_e,
  homo = TRUE,
  ss = "3",
  postHoc = ~ OglEcran,
  postHocCorr = list("tukey"),
  effectSize = list("eta", "partEta")
)

############################

Data_split <- 
  Data_merged_wide_all %>%                 # 57 subs
  filter(!ID %in% c(19, 43)) %>%           # 55 subs remain after this
  filter(Varsta < 40) %>%                  # 53 subs remain after this
  filter(ScorBDI < 30) %>%                 # 50 subs remain after this
  filter(Gen == "f")
  
  func_t_box(Data_split, "ID", "OXT_Pre_OGL", "OXT_Post_OGL")
  func_t_box(Data_split, "ID", "OXT_Pre_ECRAN", "OXT_Post_ECRAN")
  
