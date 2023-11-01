library(haven) ## Package required to upload STATA/SPSS data
library(kableExtra) ## For better tabular presentations
library(tidyverse)

library(janitor)
p_load( Ecdat)

data("Caschool")
#caschool <- read_dta("caschool.dta") ## Read Data

caschool <- Caschool

caschool |> dim()

caschool |> glimpse()

## Save as csv
#caschool_save <- write_csv(caschool, file="caschool.csv")

## Save as SPSS

#caschool_save <- write_sav(caschool, file="caschool.sav")

## Save as STAT

#caschool_save <- write_dta(caschool, file="caschool.dta")

caschool ## First 10 rows of a tibble

head(caschool) ## Top 6 rows

caschool |> glimpse()  ## To get an overview of data

colnames(caschool)

## Select to retain few needed columns
caschool |> select(testscr, str, avginc, elpct, mealpct,
                   calwpct) -> caschool_sm ## To select only required variables

caschool_sm |> summarise(across(where(is.numeric),sd,na.rm=TRUE))


library(dataxray)

caschool_sm |> make_xray() |>  view_xray()

library(gt)
library(gtsummary)
library(gtExtras)

caschool_sm |> gt_plt_summary() ## summary and plot

library(skimr)
caschool_sm |> skim()

library(modelsummary)  ## For presenting regression results in nice tables
library(rstatix)  ## For testing of hypothesis

#library(help=rstatix)


caschool_sm <-
  caschool_sm |> mutate(elq2 = ifelse(elpct >= 1.9 &
                                        elpct < 8.8, 1, 0))

caschool_sm<-caschool_sm |> mutate(elq3=ifelse(elpct>=8.8 & elpct<23,1,0))

caschool_sm<-caschool_sm |> mutate(elq4=ifelse(elpct>=23,1,0))

caschool_sm<-caschool_sm |>   mutate(str_20 = ifelse(str<20, 1,0))

caschool_sm <- caschool_sm %>%
  mutate(
    elq2 = case_when(
      elpct >= 1.9 & elpct < 8.8 ~ 1,
      TRUE ~ 0
    ),
    elq3 = case_when(
      elpct >= 8.8 & elpct < 23 ~ 1,
      TRUE ~ 0
    ),
    elq4 = case_when(
      elpct >= 23 ~ 1,
      TRUE ~ 0
    ),
    str_20 = case_when(
      str < 20 ~ 1,
      TRUE ~ 0
    )
  )

# For each column (elq2, elq3, elq4, and str_20), we use case_when to define the
# conditions and their outcomes.
#
# The TRUE ~ 0 part serves as the default outcome if none of the conditions are
# met.
#
# This code achieves the same result as your original code but in a more concise
# and readable form using case_when.









caschool_sm |> mutate(elpct = case_when(elpct<=1.9 ~ "low",
                                        elpct >=1.9 & elpct <8.8  ~ "medium",
                                        elpct >=8.8 & elpct <23  ~ "high",
                                        elpct >23~"very high")) -> caschool_sm

## Testing of hypothesis

overall<- caschool_sm |> t_test(testscr~str_20,var.equal=T)

#el_1.9<-caschool_sm |> filter(elq1==1) |> t_test(testscr~str_20,var.equal=T)

el_8.8<-caschool_sm |> filter(elq2==1) |> t_test(testscr~str_20,var.equal=T)

el_23<-caschool_sm |> filter(elq3==1) |> t_test(testscr~str_20,var.equal=T)

el_gr23<-caschool_sm |> filter(elq4==1) |> t_test(testscr~str_20,var.equal=T)

table_6.1 <- bind_rows(overall, el_8.8, el_23, el_gr23)

df_tbl<- as_tibble(table_6.1)

df_tbl

df_tbl<-df_tbl |> select(-".y.")

df_tbl$statistic <- round(df_tbl$statistic, 2)

df_tbl$p <- round(df_tbl$p, 3)

df_tbl |> gt() |> gt_theme_guardian()

lm(testscr~str,data=caschool_sm)

summary(lm(testscr~str,data=caschool_sm))


 lm(testscr ~ str + elpct, data = caschool_sm) |> tidy()

models <- list(
  mod1 <- lm(testscr ~ str, data = caschool_sm),
  mod2 <- lm(testscr ~ str + elpct, data = caschool_sm)
)

mod1 <- lm(testscr ~ str, data = caschool_sm)


mod2 <- lm(testscr ~ str + elpct, data = caschool_sm)

modelsummary(
  models,
  fmt = 2,
  estimate  = c(
    "{estimate} ({std.error}){stars}"),
  statistic = NULL)

# estimate the multiple regression model

library(car)

model <- lm(testscr ~ str + elpct + mealpct, data = caschool_sm)

# execute the function on the model object and provide both linear restrictions
# to be tested as strings
lm(testscr~mealpct, data=caschool_sm)
library(lmtest)
linearHypothesis(model, c("str=0", "elpct=0"))


linearHypothesis(model,c('elpct+mealpct=1'))

## Correlations

library(corrr)

caschool_sm |> select(testscr, str, elpct, mealpct, calwpct) |>
  correlate()

## gt, gtsummary, gtExtras

caschool_sm |> select(testscr, str, elpct, mealpct, calwpct) |>
  correlate()  |> gt() |> fmt_number(columns = 2:6, decimals = 2)
## title of table
caschool_sm |> select(testscr, str, elpct, mealpct, calwpct) |>
  correlate()   |> gt()|> fmt_number(columns = 2:5, decimals = 2) |>
  tab_header(title="Correlation between variables")

## theme of a table
library(gtExtras)

caschool_sm |> select(testscr, str, elpct, mealpct, calwpct) |>
  correlate()  |> gt()|> fmt_number(columns = 2:5, decimals = 2) |>
  tab_header(title="Correlation between variables") |>
  gt_theme_pff()

## Correlation plot plot
caschool_sm |> select(testscr, str, elpct, mealpct, calwpct) |>
  correlate() |>
  autoplot() + geom_text(aes(label = round(r, digits = 2)), size = 4)


## Plots (Arrange plots easily)

p1<- ggplot(caschool_sm)+aes(x=elpct,y=testscr)+geom_point()+
  labs(x="percent",y="Test Score")
p2<- ggplot(caschool_sm)+aes(x=mealpct,y=testscr)+geom_point()+
  labs(x="percent",y="Test Score")
p3<- ggplot(caschool_sm)+aes(x=calwpct,y=testscr)+geom_point()+
  labs(x="percent",y="Test Score")

## Plots in a row
library(patchwork)

p1/(p2+p3)





## Regression Models

m1<-lm(testscr~str,data = caschool_sm)
m2<-lm(testscr~str+elpct,data = caschool_sm)
m3<-lm(testscr~str+elpct+mealpct,data = caschool_sm)
m4<-lm(testscr~str+elpct+calwpct,data = caschool_sm)
m5<-lm(testscr~str+elpct+mealpct+calwpct,data = caschool_sm)
library(fixest)
library(modelsummary)
models<-list(m1,m2,m3,m4,m5)
#etable(m1,m2,m3,m4)
modelsummary(models,estimate = "{estimate}{stars}", output="huxtable")
#modelsummary(models, fmt=4)
#modelsummary(models,
#            statistic = "{std.error} ({p.value})")
#modelsummary(models,
#            estimate = "{estimate}{stars}",
#           gof_omit = ".*")

ggplot(caschool_sm) +aes(x=str,y=testscr)+geom_point()

caschool_sm |> mutate(elpct=as_factor(elpct)) -> caschool_sm

ggplot(caschool_sm) +aes(x=str,y=testscr, color=elpct)+geom_point()+  facet_wrap(~elpct)





