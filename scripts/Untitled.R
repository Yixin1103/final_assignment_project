library(tidyverse)
library(haven)
library(here)

getwd()
filename <-  here::here("inputs/data/Bronzini-Iachini_dataset.dta")
data <- read_dta(filename)

#Exploreatory Analysis
bandwidth <- 5
#number of firms treated and not treated 
data %>%
  select(c("FIRM", "score","treat","ASSETS","SALES","LEMPL","AGE", "LWAGE")) %>%
  mutate(data,
         WAGE = 10**LWAGE)%>%
  filter(score >= (75 - bandwidth) & score < (75 + bandwidth) )%>%
  group_by(treat)%>%
  summarise(mean(WAGE,na.rm=T))

bandwidth <- 4
filteredData %>% 
  filter(score >= (75 - bandwidth) & score < 75) %>%
  nrow()

filteredData %>% 
  filter(score < (75 + bandwidth) & score >= 75) %>%
  nrow()


## Impact on Dropout

&nbsp; &nbsp; The imme


```{r, include=FALSE}
# import data
clean_cut <- read.csv(here("inputs/data/bandwidth02.csv"))
clean_cut2 <- read.csv(here("inputs/data/bandwidth03.csv"))
clean_cut3<- read.csv(here("inputs/data/bandwidth06.csv"))
```

```{r, include=FALSE}
class(clean_cut$status)
clean_cut$status <- as.factor(clean_cut$status)
levels(clean_cut$status)
# Set students on academic probation as the baseline group
clean_cut$status <- relevel(clean_cut$status, "On Academic Probation")
clean_cut2$status <- as.factor(clean_cut2$status)
clean_cut2$status <- relevel(clean_cut2$status, "On Academic Probation")
clean_cut3$status <- as.factor(clean_cut3$status)
clean_cut3$status <- relevel(clean_cut3$status, "On Academic Probation")
```


```{r, include=FALSE}
# Logistic Regression to estimate the impact of probation on student dropout
# Bandwidth 0.2
dropout_02 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut)
#summary(dropout_02)
# Bandwidth 0.3
dropout_03 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut2)
# Bandwidth 0.6
dropout_06 <- glm(left_school ~ status + dist_from_cut + gender + highHS + first_language ,family = "binomial", data = clean_cut3)
```


```{r dropsummary, tab.cap="Logistic Regression for the Impact of Academic Probation on Student Dropout", echo = FALSE, message = FALSE, warning=FALSE}
# Reference the following to customize modelsummary table: https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html
# Note: Broom package could not exponentiate the standard error right now
# Reference: https://github.com/tidymodels/broom/issues/422
modelsummary(list("0.2 GPA Bandwidth" = dropout_02, "0.3 GPA Bandwidth" = dropout_03,"0.6 GPA Bandwidth" =  dropout_06),
             statistic = "{std.error} ({p.value})",
             stars =TRUE,
             coef_map = c("(Intercept)" = "Constant", "statusIn Good Standing" = "In Good Standing", "dist_from_cut" = "f(Cutoff Distance)","genderMale" = "Gender Male", "highHS" = "Highschool Above Average", "first_languageOther" = "First Language Not English "),
             exponentiate = TRUE,
             title = "Logistic Regression for the Impact of Academic Probation on Student Dropout",
             gof_omit = "BIC") %>% 
  add_footnote("Odds Ratio in First Row. Standard Error of Log of Odds in Second Row. P-value in Parentheses", notation = "number") %>% 
  kable_styling(latex_options = "hold_position")
```

```{r dropfig, fig.align='center', fig.cap="Academic Probation Impact on Dropout", fig.width=8, fig.height=4, echo= FALSE, message=FALSE, warning=FALSE}
clean_cut$probation_year1 <- as.factor(clean_cut$probation_year1)
ggplot(data= clean_cut, aes(x = dist_from_cut, y = left_school, color = status))+
  geom_point(alpha = 0.05)+
  geom_smooth(method = "glm", method.args = list(family = 'binomial'))+
  theme_light()+
  theme(legend.title = element_blank(), legend.position = "bottom", plot.title = element_text(hjust = 0.5))+
  guides(color=guide_legend(override.aes=list(fill=NA)))+
  labs(title = "Academic Probation Impact on Dropout",
       x = "Year 1 GPA minus Probation Cutoff",
       y = "Left University")  
