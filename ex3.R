library(tidyverse)
library(haven)
library(stargazer)
library(xtable)
library(psych)
library(lmtest)
library(sandwich)
library(plm)
library(AER)
library(car)


df <- read_dta("base_trab_3.dta")
df <- subset(df, is.na(medio_mat) == FALSE | is.na(medio_pt) == FALSE |
               is.na(final_mat) == FALSE | is.na(final_pt) == FALSE |
               is.na(inicial_mat) == FALSE | is.na(inicial_pt) == FALSE)
df <- df %>% rename(
  municipio = co_municipio,
  escola = codigo_escola,
  uf = co_uf,
  dpadm = tp_dependencia,
  esgoto = in_esgoto_inexistente,
  energia = in_energia_inexistente,
  bib = in_biblioteca,
  func = nu_funcionarios,
  sala = in_sala_leitura,
  aliment = in_alimentacao)%>%
  mutate(
    rural = ifelse(tp_localizacao == 2, 1, 0),
    urbano = ifelse(tp_localizacao == 1, 1, 0),
    alunprof_creche = num_matri_creche_total/num_profcreche,
    alunprof_pre = num_matri_preescola_total/num_profpreescola,
    alunprof_fund = num_matri_fund_total_geral/num_proffundamental,
    alunprof_fundin = num_matri_fund_1a4/num_proffundamental,
    alunprof_fundfi = num_matri_fund_5a8/num_proffundamental,
    turmprof_creche = num_turmas_creche_total/num_profcreche,
    turmprof_pre = num_turmas_preescola_total/num_profpreescola,
    turmprof_fund = num_turmas_fund_total_geral/num_proffundamental,
    turmprof_fundin = (num_turmas_fund_1 + num_turmas_fund_2 +
                    num_turmas_fund_3 + num_turmas_fund_4)/num_proffundamental,
    turmprof_fundfi = (num_turmas_fund_5 + num_turmas_fund_6 +
                    num_turmas_fund_7 + num_turmas_fund_8)/num_proffundamental,
    alunturm_creche = num_matri_creche_total/num_turmas_creche_total,
    alunturm_pre = num_matri_preescola_total/num_turmas_preescola_total,
    alunturm_fund = num_matri_fund_total_geral/num_turmas_fund_total_geral,
    alunturm_fundin = num_matri_fund_1a4/(num_turmas_fund_1 + num_turmas_fund_2 +
                                            num_turmas_fund_3 + num_turmas_fund_4),
    alunturm_fundfi = num_matri_fund_5a8/(num_turmas_fund_5 + num_turmas_fund_6 +
                                            num_turmas_fund_7 + num_turmas_fund_8),
    municipio = as.factor(municipio),
    escola = as.factor(escola),
    uf = as.factor(uf),
    dpadm = as.factor(dpadm)
  )
df$tp_localizacao <- NULL
View(df)

sum_inicial_uf <- as.data.frame(df %>%
  group_by(uf) %>%
  summarise(
    obs = length(inicial_pt),
    PTmed = mean(inicial_pt, na.rm = TRUE),
    PTmdn = median(inicial_pt, na.rm = TRUE),
    PTsd = sd(inicial_pt, na.rm = TRUE),
    PTmax = max(inicial_pt, na.rm = TRUE),
    PTmin = min(inicial_pt, na.rm = TRUE),
    MTmed = mean(inicial_mat, na.rm = TRUE),
    MTmdn = median(inicial_mat, na.rm = TRUE),
    MTsd = sd(inicial_mat, na.rm = TRUE),
    MTmax = max(inicial_mat, na.rm = TRUE),
    MTmin = min(inicial_mat, na.rm = TRUE)))
print(xtable(sum_inicial_uf, type = "latex"), file = "sum_inicial_uf.tex",
      include.rownames=FALSE)
sum_inicial_na <- as.data.frame(df %>%
                                  summarise(
                                    obs = length(inicial_pt),
                                    PTmed = mean(inicial_pt, na.rm = TRUE),
                                    PTmdn = median(inicial_pt, na.rm = TRUE),
                                    PTsd = sd(inicial_pt, na.rm = TRUE),
                                    PTmax = max(inicial_pt, na.rm = TRUE),
                                    PTmin = min(inicial_pt, na.rm = TRUE),
                                    MTmed = mean(inicial_mat, na.rm = TRUE),
                                    MTmdn = median(inicial_mat, na.rm = TRUE),
                                    MTsd = sd(inicial_mat, na.rm = TRUE),
                                    MTmax = max(inicial_mat, na.rm = TRUE),
                                    MTmin = min(inicial_mat, na.rm = TRUE)))
print(xtable(sum_inicial_na, type = "latex"), file = "sum_inicial_na.tex",
      include.rownames=FALSE)

sum_final_uf <- as.data.frame(df %>%
                                  group_by(uf) %>%
                                  summarise(
                                    obs = length(final_pt),
                                    PTmed = mean(final_pt, na.rm = TRUE),
                                    PTmdn = median(final_pt, na.rm = TRUE),
                                    PTsd = sd(final_pt, na.rm = TRUE),
                                    PTmax = max(final_pt, na.rm = TRUE),
                                    PTmin = min(final_pt, na.rm = TRUE),
                                    MTmed = mean(final_mat, na.rm = TRUE),
                                    MTmdn = median(final_mat, na.rm = TRUE),
                                    MTsd = sd(final_mat, na.rm = TRUE),
                                    MTmax = max(final_mat, na.rm = TRUE),
                                    MTmin = min(final_mat, na.rm = TRUE)))
print(xtable(sum_final_uf, type = "latex"), file = "sum_final_uf.tex",
      include.rownames=FALSE)
sum_final_na <- as.data.frame(df %>%
                                  summarise(
                                    obs = length(final_pt),
                                    PTmed = mean(final_pt, na.rm = TRUE),
                                    PTmdn = median(final_pt, na.rm = TRUE),
                                    PTsd = sd(final_pt, na.rm = TRUE),
                                    PTmax = max(final_pt, na.rm = TRUE),
                                    PTmin = min(final_pt, na.rm = TRUE),
                                    MTmed = mean(final_mat, na.rm = TRUE),
                                    MTmdn = median(final_mat, na.rm = TRUE),
                                    MTsd = sd(final_mat, na.rm = TRUE),
                                    MTmax = max(final_mat, na.rm = TRUE),
                                    MTmin = min(final_mat, na.rm = TRUE)))
print(xtable(sum_final_na, type = "latex"), file = "sum_final_na.tex",
      include.rownames=FALSE)

sum_medio_uf <- as.data.frame(df %>%
                                  group_by(uf) %>%
                                  summarise(
                                    obs = length(medio_pt),
                                    PTmed = mean(medio_pt, na.rm = TRUE),
                                    PTmdn = median(medio_pt, na.rm = TRUE),
                                    PTsd = sd(medio_pt, na.rm = TRUE),
                                    PTmax = max(medio_pt, na.rm = TRUE),
                                    PTmin = min(medio_pt, na.rm = TRUE),
                                    MTmed = mean(medio_mat, na.rm = TRUE),
                                    MTmdn = median(medio_mat, na.rm = TRUE),
                                    MTsd = sd(medio_mat, na.rm = TRUE),
                                    MTmax = max(medio_mat, na.rm = TRUE),
                                    MTmin = min(medio_mat, na.rm = TRUE)))
print(xtable(sum_medio_uf, type = "latex"), file = "sum_medio_uf.tex",
      include.rownames=FALSE)
sum_medio_na <- as.data.frame(df %>%
                                  summarise(
                                    obs = length(medio_pt),
                                    PTmed = mean(medio_pt, na.rm = TRUE),
                                    PTmdn = median(medio_pt, na.rm = TRUE),
                                    PTsd = sd(medio_pt, na.rm = TRUE),
                                    PTmax = max(medio_pt, na.rm = TRUE),
                                    PTmin = min(medio_pt, na.rm = TRUE),
                                    MTmed = mean(medio_mat, na.rm = TRUE),
                                    MTmdn = median(medio_mat, na.rm = TRUE),
                                    MTsd = sd(medio_mat, na.rm = TRUE),
                                    MTmax = max(medio_mat, na.rm = TRUE),
                                    MTmin = min(medio_mat, na.rm = TRUE)))
print(xtable(sum_medio_na, type = "latex"), file = "sum_medio_na.tex",
      include.rownames=FALSE)

sum_estrutura_na <- as.data.frame(df %>%
  summarise(
    funcionarios = mean(func, na.rm = TRUE),
    urbano = 100*mean(urbano, na.rm = TRUE),
    biblioteca = 100*mean(bib, na.rm = TRUE),
    sala_leitura = 100*mean(sala, na.rm = TRUE),
    alimentacao = 100*mean(aliment, na.rm = TRUE),
    esgoto = 100*(1 - mean(esgoto, na.rm = TRUE)),
    energia = 100*(1 - mean(energia, na.rm = TRUE))))
print(xtable(sum_estrutura_na, type = "latex"), file = "sum_estrutura_na.tex",
      include.rownames=FALSE)
sum_estrutura_uf <- as.data.frame(df %>%
  group_by(uf) %>%
  summarise(
    funcionarios = mean(func, na.rm = TRUE),
    urbano = 100*mean(urbano, na.rm = TRUE),
    biblioteca = 100*mean(bib, na.rm = TRUE),
    sala_leitura = 100*mean(sala, na.rm = TRUE),
    alimentacao = 100*mean(aliment, na.rm = TRUE),
    esgoto = 100*(1 - mean(esgoto, na.rm = TRUE)),
    energia = 100*(1 - mean(energia, na.rm = TRUE))))
print(xtable(sum_estrutura_uf, type = "latex"), file = "sum_estrutura_uf.tex",
      include.rownames=FALSE)

sum_funcionarios_uf <- as.data.frame(df %>%
  group_by(uf) %>%
    summarise(
      n = length(func),
      Média = mean(func, na.rm = TRUE),
      Mediana = median(func, na.rm = TRUE),
      Desvio_Padrão = sd(func, na.rm = TRUE),
      Máximo = max(func, na.rm = TRUE),
      Mínimo = min(func, na.rm = TRUE)))
print(xtable(sum_funcionarios_uf, type = "latex"), file = "sum_funcionarios_uf.tex",
      include.rownames=FALSE)

sum_funcionarios_na <- as.data.frame(df %>%
summarise(
  n = length(func),
Média = mean(func, na.rm = TRUE),
Mediana = median(func, na.rm = TRUE),
Desvio_Padrão = sd(func, na.rm = TRUE),
Máximo = max(func, na.rm = TRUE),
Mínimo = min(func, na.rm = TRUE)))
print(xtable(sum_funcionarios_na, type = "latex"), file = "sum_funcionarios_na.tex",
      include.rownames=FALSE)

summary(df$urbano)
summary(df$bib)
summary(df$sala)
summary(df$esgoto)
summary(df$aliment)
summary(df$energia)
summary.factor(df$dpadm)
df_creche <- subset(df, is.na(num_turmas_creche_total) == FALSE)
describe(df_creche[ , c(53, 58, 63)], na.rm = TRUE)
df_pre <- subset(df, is.na(num_turmas_preescola_total) == FALSE)
describe(df_pre[ , c(54, 59, 64)], na.rm = TRUE)
df_fund <- subset(df, is.na(num_turmas_fund_total_geral) == FALSE)
describe(df_fund[ , c(55, 60, 65)], na.rm = TRUE)
df_fundin <- subset(df, is.na(num_turmas_fund_1) == FALSE |
                      is.na(num_turmas_fund_2) == FALSE |
                      is.na(num_turmas_fund_3) == FALSE |
                      is.na(num_turmas_fund_4) == FALSE)
describe(df_fundin[ , c(56, 61, 66)], na.rm = FALSE)
df_fundfi <- subset(df, is.na(num_turmas_fund_5) == FALSE |
                      is.na(num_turmas_fund_6) == FALSE |
                      is.na(num_turmas_fund_7) == FALSE |
                      is.na(num_turmas_fund_8) == FALSE)
describe(df_fundfi[ , c(57, 62, 67)], na.rm = TRUE)
view(df_fundfi)

df_imat <- df %>%
  filter(!is.na(inicial_mat), is.na(final_mat), is.na(medio_mat)) %>%
  nrow(.)
df_ipt <- df %>%
  filter(!is.na(inicial_pt), is.na(final_pt), is.na(medio_pt)) %>%
  nrow(.)
df_ipt
df_imat

df_fmat <- df %>%
  filter(is.na(inicial_mat), !is.na(final_mat), is.na(medio_mat)) %>%
  nrow(.)
df_fpt <- df %>%
  filter(is.na(inicial_pt), !is.na(final_pt), is.na(medio_pt)) %>%
  nrow(.)
df_fpt
df_fmat

df_mmat <- df %>%
  filter(is.na(inicial_mat), is.na(final_mat), !is.na(medio_mat)) %>%
  nrow(.)
df_mpt <- df %>%
  filter(is.na(inicial_pt), is.na(final_pt), !is.na(medio_pt)) %>%
  nrow(.)
df_mpt
df_mmat

df_ifmat <- df %>%
  filter(!is.na(inicial_mat), !is.na(final_mat), is.na(medio_mat)) %>%
  nrow(.)
df_ifpt <- df %>%
  filter(!is.na(inicial_pt), !is.na(final_pt), is.na(medio_pt)) %>%
  nrow(.)
df_ifpt
df_ifmat

df_immat <- df %>%
  filter(!is.na(inicial_mat), is.na(final_mat), !is.na(medio_mat)) %>%
  nrow(.)
df_impt <- df %>%
  filter(!is.na(inicial_pt), is.na(final_pt), !is.na(medio_pt)) %>%
  nrow(.)
df_impt
df_immat

df_fmmat <- df %>%
  filter(is.na(inicial_mat), !is.na(final_mat), !is.na(medio_mat)) %>%
  nrow(.)
df_fmpt <- df %>%
  filter(is.na(inicial_pt), !is.na(final_pt), !is.na(medio_pt)) %>%
  nrow(.)
df_fmpt
df_fmmat

df_ifmmat <- df %>%
  filter(!is.na(inicial_mat), !is.na(final_mat), !is.na(medio_mat)) %>%
  nrow(.)
df_ifmpt <- df %>%
  filter(!is.na(inicial_pt), !is.na(final_pt), !is.na(medio_pt)) %>%
  nrow(.)
df_fmpt
df_fmmat

df_i <- df_ipt
df_f <- df_fpt
df_m <- df_mpt
df_if <- df_ifpt
df_im <- df_impt
df_fm <- df_fmpt
df_ifm <- df_ifmpt
rm(df_ipt, df_fpt, df_mpt, df_ifpt, df_impt, df_fmpt, df_ifmpt,
   df_imat, df_fmat, df_mmat, df_ifmat, df_immat, df_fmmat, df_ifmmat,
   df_creche, df_fund, df_fundin, df_fundfi, df_pre, sum_estrutura_na,
   sum_estrutura_uf, sum_final_na, sum_final_uf, sum_funcionarios_na,
   sum_funcionarios_uf, sum_inicial_na, sum_inicial_uf, sum_medio_na,
   sum_medio_uf)

eqI <- lm(formula = inicial_pt~func*dpadm+sala*bib+urbano, data=df)
eqII <- lm(formula = inicial_pt~func*dpadm+sala*bib+urbano+aliment+energia+
              urbano+esgoto, data = df)
eqIII <- lm(formula = inicial_pt~uf+func*dpadm+sala*bib+urbano+aliment+energia+
              urbano+esgoto, data = df)
cov1 <- vcovHC(eqI, type = "HC1")
se1 <- sqrt(diag(cov1))
cov2 <- vcovHC(eqII, type = "HC1")
se2 <- sqrt(diag(cov2))
cov3 <- vcovHC(eqIII, type = "HC1")
se3 <- sqrt(diag(cov3))
stargazer(eqI, eqII, eqIII, type="text", se = list(se1, se2, se3))

ipt <- lm(formula = inicial_pt~uf+func*dpadm+sala*bib+urbano+aliment+energia+
                 urbano+esgoto, data = df)
se_ipt <- sqrt(diag(vcovHC(ipt, type = "HC1")))
imt <- lm(formula = inicial_mat~uf+func*dpadm+sala*bib+urbano+aliment+energia+
            urbano+esgoto, data = df)
se_imt <- sqrt(diag(vcovHC(imt, type = "HC1")))
fpt <- lm(formula = final_pt~uf+func*dpadm+sala*bib+urbano+aliment+energia+
            urbano+esgoto, data = df)
se_fpt <- sqrt(diag(vcovHC(fpt, type = "HC1")))
fmt <- lm(formula = final_mat~uf+func*dpadm+sala*bib+urbano+aliment+energia+
            urbano+esgoto, data = df)
se_fmt <- sqrt(diag(vcovHC(fmt, type = "HC1")))
mpt <- lm(formula = medio_pt~uf+func*dpadm+sala*bib+urbano+aliment+energia+
            urbano+esgoto, data = df)
se_mpt <- sqrt(diag(vcovHC(mpt, type = "HC1")))
mmt <- lm(formula = medio_mat~uf+func*dpadm+sala*bib+urbano+aliment+energia+
            urbano+esgoto, data = df)
se_mmt <- sqrt(diag(vcovHC(mmt, type = "HC1")))
stargazer(ipt, imt, fpt, fmt, mpt, mmt,
          type="text",
          se = list(se_ipt, se_imt, se_fpt, se_fmt, se_mpt, se_mmt))
vif(ipt)
vif(fpt)