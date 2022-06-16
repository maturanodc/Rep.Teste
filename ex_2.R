library(tidyverse)
library(haven)
library(stargazer)
library(data.table)
library(ggplot2)


df <- read_dta("df.dta") %>%
  mutate_at("p05402", ~replace_na(., 0)) %>%
  mutate(
    p045=as.factor(p045),  # p045 = TV
    p050=as.factor(p050),  # p050 = Classificação de fumante
    c006=as.factor(c006),  # c006 = Sexo
    c009=as.factor(c009),  # c009 = Cor
    c011=as.factor(c011),  # c011 = Estado civil
    d001=as.factor(d001),  # d001 = Sabe ler e escrever
    c008=as.numeric(c008),  # c008 = Idade
    p05402=as.numeric(p05402),  # p05402 = Nº de cigarros
    p020=as.numeric(p020),  # p020 = Dias de refri
    w00103=as.numeric(w00103),  # w00103 = Peso (kg)
    w00203=as.numeric(w00203),  # w00203 = Altura (cm)
  ) %>%
  within(., 
         p045 <- relevel(p045, ref = 8),
         p050 <- relevel(p050, ref = 3),
         c006 <- relevel(c006, ref = 1),
         c009 <- relevel(c009, ref = 1),
         c011 <- relevel(c011, ref = 5),
         d001 <- relevel(d001, ref = 1)
  )
df[1] <- NULL
df[2:4] <- list(NULL)
df[4] <- NULL
df[7] <- NULL
df[12] <- NULL
df <- df %>% mutate(w00103 = w00103 * 1000)
df <- na.omit(df)
View(df)

#  CÁLCULO DAS ESTATÍSTICAS DESCRITIVAS

df %>%
  count(p050 == 3, p045)
df %>%
  count(p050 == 2, p045)
df %>%
  count(p050 == 1, p045)


as.data.frame(df %>%
  group_by(p050) %>%
  summarise(
    PESOobs = length(w00103),
    PESOmedia = mean(w00103, na.rm = TRUE),
    PESOmedian = median(w00103, na.rm = TRUE),
    PESOsd = sd(w00103, na.rm = TRUE),
    PESOmax = max(w00103, na.rm = TRUE),
    PESOmin = min(w00103, na.rm = TRUE)
  ))


as.data.frame(df %>%
  group_by(p050) %>%
  summarise(
    REFobs = length(p020),
    REFmedia = mean(p020, na.rm = TRUE),
    REFmedian = median(p020, na.rm = TRUE),
    REFsd = sd(p020, na.rm = TRUE),
    REFmax = max(p020, na.rm = TRUE),
    REFmin = min(p020, na.rm = TRUE)
  ))


as.data.frame(df %>%
#  group_by(p050) %>%
  summarise(
    CIGobs = length(p05402),
    CIGmedia = mean(p05402, na.rm = TRUE),
    CIGmedian = median(p05402, na.rm = TRUE),
    CIGsd = sd(p05402, na.rm = TRUE),
    CIGmax = max(p05402, na.rm = TRUE),
    CIGmin = min(p05402, na.rm = TRUE)
  )) 


#   PLOT DISTRIBUIÇÃO TV
par(mar=c(4,6,3,2))
barplot(table(df$p045), space = 0,
        ylab = "Frequência\n", main = "Horas assistidas de TV (Indiscriminado)",
        border="black", col="grey",las=2, names.arg = c("0", "(0, 1)", "[1, 2)",
        "[2, 3)", "[3, 4)", "[4, 5)", "[5, 6)", "[6, 24]"),
        ylim=range(pretty(c(0, 15000))))

#   FILTRO DE BASE DE DADOS P/ REG
df_fum <- df %>% filter(p050 == 1)
df_soc <- df %>% filter(p050 == 2)
df_nfu <- df %>% filter(p050 == 3)


#   CÁLCULO DAS REGRESSÕES

model1fum <- lm(w00103 ~ p020 + p045 + p05402, data = df_fum)
model1soc <- lm(w00103 ~ p020 + p045 + p05402, data = df_soc)
model1nfu <- lm(w00103 ~ p020 + p045 + p05402, data = df_nfu)
model1 <- lm(w00103 ~ p020 + p045 + p05402, data = df)
stargazer(model1fum, model1soc, model1nfu, model1)

model2fum <- lm(log(w00103) ~ p020 + p045 + log(p05402 + 1), data = df_fum)
model2soc <- lm(log(w00103) ~ p020 + p045 + log(p05402 + 1), data = df_soc)
model2nfu <- lm(log(w00103) ~ p020 + p045 + log(p05402 + 1), data = df_nfu)
model2 <- lm(log(w00103) ~ p020 + p045 + log(p05402 + 1), data = df)
stargazer(model2fum, model2soc, model2nfu, model2)
summary(model2)

model3.1 <- lm(w00103 ~ p020 + p045 + p05402 + c006 +
                w00203 + d001, data = df)
model3.2 <- lm(w00103 ~ p020 + p045 + p05402 + c006 +
                 w00203 + d001 +
                 c008 + c011 + c009, data = df)
model3.3 <- lm(log(w00103) ~ p020 + p045 + log(p05402+1) + c006 +
                 log(w00203) + d001, data = df)
model3.4 <- lm(log(w00103) ~ p020 + p045 + log(p05402+1) + c006 +
                 log(w00203) + d001 +
                 c008 + c011 + c009, data = df)
stargazer(model3.1, model3.2, model3.3, model3.4)

