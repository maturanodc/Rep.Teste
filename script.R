library(tidyverse)
library(tibble)
options(pillar.sigfig = 10)
library(readxl)
library(stargazer)
library(xtable)
library(psych)
library(lmtest)
library(sandwich)
library(plm)
library(AER)
library(car)
df <- read_xlsx("df.xlsx")
gdp_quan <- quantile(df$gdp, na.rm = TRUE, names =  TRUE)
gdp_ppp_quan <- quantile(df$gdp_ppp, na.rm = TRUE, names = TRUE)
df <- df %>%
  mutate(miser=0, pobre=0, medio=0, rico=0, miser_ppp=0,
         pobre_ppp=0, medio_ppp=0, rico_ppp=0, ssdf=0) %>%
  add_column(fin = NA) %>% add_column(inc = NA) %>% add_column(inc_ppp = NA)
  df$fin <- replace(df$fin, df$gfa/df$cfa >= 2/3, 2)
    # fin=2: financiamento governamental de 67% dos tratamentos (beveridge)
  df$fin <- replace(df$fin,  df$chi/df$cfa >= 2/3, 3)
    # fin=3: financiamento privado compulsório de 67% dos tratamentos (bismarck)
  df$fin <- replace(df$fin, df$oop > 67, 1)
    # fin=1: oop >= 1.5(média_internacional), pgtos privados pelo paciente
  df$fin <- replace(df$fin, is.na(df$fin), 4)
    # fin=4: financiamento misto, nem segurado "bismark" nem SUS "beveridge"
  df$fin <- as.factor(df$fin)
  levels(df$fin) <- c('.oop', '.bev', '.bis', '.mix')
  df$miser <- replace(df$miser, df$gdp>=gdp_quan[1] & df$gdp<gdp_quan[2], 1)
  df$pobre <- replace(df$pobre, df$gdp>=gdp_quan[2] & df$gdp<gdp_quan[3], 1)
  df$medio <- replace(df$medio, df$gdp>=gdp_quan[3] & df$gdp<gdp_quan[4], 1)
  df$rico <- replace(df$rico, df$gdp>=gdp_quan[4], 1)
  df$inc <- replace(df$inc, df$miser == 1, 1)
  df$inc <- replace(df$inc, df$pobre == 1, 2)
  df$inc <- replace(df$inc, df$medio == 1, 3)
  df$inc <- replace(df$inc, df$rico == 1, 4)
  df_mis <- subset(df, inc==1, select = country:inc_ppp)
  df_pob <- subset(df, inc==2, select = country:inc_ppp)
  df_med <- subset(df, inc==3, select = country:inc_ppp)
  df_ric <- subset(df, inc==4, select = country:inc_ppp)
  df$inc <- as.factor(df$inc)
  levels(df$inc) <- c('.miser', '.pobre', '.medio', '.rico')
  df$miser_ppp <- replace(df$miser_ppp, df$gdp_ppp>=gdp_ppp_quan[1] &
                        df$gdp_ppp<gdp_ppp_quan[2], 1)
  df$pobre_ppp <- replace(df$pobre_ppp, df$gdp_ppp>=gdp_ppp_quan[2] &
                        df$gdp_ppp<gdp_ppp_quan[3], 1)
  df$medio_ppp <- replace(df$medio_ppp, df$gdp_ppp>=gdp_ppp_quan[3] &
                        df$gdp_ppp<gdp_ppp_quan[4], 1)
  df$rico_ppp <- replace(df$rico_ppp, df$gdp_ppp>=gdp_ppp_quan[4], 1)
  df$inc_ppp <- replace(df$inc_ppp, df$miser_ppp == 1, 1)
  df$inc_ppp <- replace(df$inc_ppp, df$pobre_ppp == 1, 2)
  df$inc_ppp <- replace(df$inc_ppp, df$medio_ppp == 1, 3)
  df$inc_ppp <- replace(df$inc_ppp, df$rico_ppp == 1, 4)
  df$inc_ppp <- as.factor(df$inc_ppp)
  levels(df$inc_ppp) <- c('.miser', '.pobre', '.medio', '.rico')
  df$ssdf <- replace(df$ssdf, df$uhc_sc < 67,1)
  df <- rename(df, pib = gdp)
  df <- rename(df, pib_ppp = gdp_ppp)
  df <- rename(df, che_pib = che_gdp)
  #ssdf: sistema de saúde deficiente ou falido
View(df)


#  VISUALIZACAO DE DADOS, ANALISE DESCRITIVA
summary(df$fin)
df %>% group_by(fin) %>% summarise(mean(exp, na.rm = TRUE),
                                   mean(mor, na.rm = TRUE),
                                   mean(pib, na.rm = TRUE),
                                   mean(che_pib, na.rm = TRUE),
                                   mean(idh, na.rm = TRUE))
df %>% summarise(mean(exp, na.rm = TRUE),
                 mean(mor, na.rm = TRUE),
                 mean(pib, na.rm = TRUE),
                 mean(che_pib, na.rm = TRUE),
                 mean(idh, na.rm = TRUE))
df %>% group_by(fin) %>% summarise(sd(exp, na.rm = TRUE),
                                   sd(mor, na.rm = TRUE),
                                   sd(pib, na.rm = TRUE),
                                   sd(che_pib, na.rm = TRUE),
                                   sd(idh, na.rm = TRUE))
df %>% summarise(sd(exp, na.rm = TRUE),
                 sd(mor, na.rm = TRUE),
                 sd(pib, na.rm = TRUE),
                 sd(che_pib, na.rm = TRUE),
                 sd(idh, na.rm = TRUE))

summary(df_mis$fin)
summary(df_pob$fin)
summary(df_med$fin)
summary(df_ric$fin)



#  TESTANDO PARA HETEROCEDASTICIDADE

exp1 <- lm(formula = exp ~ log(pib)+rural+san+che_pib+fin+
            ssdf, data = df)
se_e1 <- sqrt(diag(vcovHC(exp1, type = "HC3")))
lmtest::bptest(exp1)

exp2 <- lm(formula = exp ~ log(pib_ppp)+rural+san+che_ppp+fin+
             ssdf, data = df)
se_e2 <- sqrt(diag(vcovHC(exp2, type = "HC3")))
lmtest::bptest(exp2)

exp3 <- lm(formula = exp ~ inc+rural+san+che_pib+fin+
             ssdf, data = df)
se_e3 <- sqrt(diag(vcovHC(exp3, type = "HC3")))
lmtest::bptest(exp3)

exp4 <- lm(formula = exp ~ idh+rural+san+che_pib+fin+
             ssdf, data = df)
se_e4 <- sqrt(diag(vcovHC(exp4, type = "HC3")))
lmtest::bptest(exp4)


mor1 <- lm(formula = mor ~ log(pib)+rural+san+che_pib+fin+
                 ssdf, data = df)
  se_m1 <- sqrt(diag(vcovHC(mor1, type = "HC3")))
    lmtest::bptest(mor1)
      
mor2 <- lm(formula = mor ~ log(pib_ppp)+rural+san+che_ppp+fin+
                   ssdf, data = df)
  se_m2 <- sqrt(diag(vcovHC(mor2, type = "HC3")))
    lmtest::bptest(mor2)
      
mor3 <- lm(formula = mor ~ inc+rural+san+che_pib+fin+
                   ssdf, data = df)
  se_m3 <- sqrt(diag(vcovHC(mor3, type = "HC3")))
    lmtest::bptest(mor3)
    
mor4 <- lm(formula = mor ~ idh+rural+san+che_pib+fin+
                 ssdf, data = df)
  se_m4 <- sqrt(diag(vcovHC(mor4, type = "HC3")))
    lmtest::bptest(mor4)

#  TESTANDO PARA MULTICOLINEARIDADE
      
vif(exp1)
vif(exp2)
vif(exp3)
vif(exp4)

#  TABELAS DE REGRESSÃO

stargazer(exp1, exp2, exp3, exp4, type = "text",
          se=list(se_e1, se_e2, se_e3, se_e4))
stargazer(mor1, mor2, mor3, mor4, type = "text",
          se=list(se_m1, se_m2, se_m3, se_m4))

stargazer(exp2, exp3, exp4, mor2, mor3, mor4, type = "latex",
          se=list(se_e2, se_e3, se_e4, se_m2, se_m3, se_m4))

#  BEVERIDGE != BISMARCK?
linearHypothesis(exp1, c("fin.bev=fin.bis"))
linearHypothesis(exp2, c("fin.bev=fin.bis"))
linearHypothesis(exp3, c("fin.bev=fin.bis"))
linearHypothesis(exp4, c("fin.bev=fin.bis"))
linearHypothesis(mor1, c("fin.bev=fin.bis"))
linearHypothesis(mor2, c("fin.bev=fin.bis"))
linearHypothesis(mor3, c("fin.bev=fin.bis"))
linearHypothesis(mor4, c("fin.bev=fin.bis"))

#  ANALISE DE SIGNIFICANCIA CONJUNTA
  
linearHypothesis(exp1, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(exp2, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(exp3, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(exp4, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(mor1, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(mor2, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(mor3, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))
linearHypothesis(mor4, c("fin.bev=0", "fin.bis=0", "fin.mix=0"))


