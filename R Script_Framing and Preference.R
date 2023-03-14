################################################################################
########    R Coding Script    #################################################
################################################################################
########    The Effects of News Framing on Policy Preference            ########

########    Data:  2021 Cooperative Election Study Module               ########
################################################################################
################################################################################

rm(list = ls())

### Get data: 2021 CES Module
install.packages("haven")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("labelled")

library(haven)
library(dplyr)
library(tidyverse)
library(labelled)

data <- read_sav("CES21_Module_output.sav")


### Re-code dependent variables (DVs) and independent variables (IVs)

## DV: Policy preference; categorical (binary) variable

# DV-(1) Policy preference, post-exposure to *thematic* frame
table(data$UTB434a)
#   1   2   3 
# 231 171  81 

data$prefer_thematic[data$UTB434a == 3] <- NA
data$prefer_thematic[data$UTB434a == 2] <- 0
data$prefer_thematic[data$UTB434a == 1] <- 1
table(data$prefer_thematic) 
#   0   1
# 171 231 
summary(data$prefer_thematic)

# Factor DV-(1)
data <- data %>%
  mutate(prefer_thematic_f = factor(prefer_thematic, levels = c(0,1), 
                                     labels = c("no", "yes")))
select(data, prefer_thematic, prefer_thematic_f)
table(data$prefer_thematic_f)
#  no yes
# 171 231


# DV-(2) Policy preference, post-exposure to *episodic* frame

table(data$UTB436a)
#   1   2   3 
# 270 142  67 

data$prefer_episodic[data$UTB436a == 3] <- NA
data$prefer_episodic[data$UTB436a == 2] <- 0
data$prefer_episodic[data$UTB436a == 1] <- 1
table(data$prefer_episodic) 
#   0   1
# 142 270 
summary(data$prefer_episodic)

# Factor DV-(2)
data <- data %>%
  mutate(prefer_episodic_f = factor(prefer_episodic, levels = c(0,1), 
                                    labels = c("no", "yes")))
select(data, prefer_episodic, prefer_episodic_f)
table(data$prefer_episodic_f)
#  no yes 
# 142 270 


## IV1: Type of frame exposure 
##     (Rs who are randomly assigned to 'thematic' vs. 'episodic' frame)
data$treatment[data$UTB434a <= 3] <- 1 # Rs assigned to thematic frame (N=483)
data$treatment[data$UTB436a <= 3] <- 2 # Rs assigned to episodic frame (N=479)
count(data, treatment)
table(data$treatment)

data <- data %>%
  mutate(treatment_f = factor(treatment, levels = c(1, 2),
                              labels = c("Thematic", "Episodic")))
select(data, treatment, treatment_f)
count(data, treatment_f)
#    treatment     n
# 1  Thematic    483
# 2  Episodic    479
# 3  NA           38


## IV2: Party identification (Dem vs. Rep)
table(data$pid3)
#   D   R   I Other NS
#   1   2   3   4   5 
# 388 248 281  40  43 

data$republican[data$pid3 == 1] <- 1
data$republican[data$pid3 == 2] <- 2
table(data$republican)
#   1   2 
# 388 248

data <- data %>%
  mutate(republican_f = factor(republican, levels = c(1, 2), 
                               labels = c("Democrat", "Republican")))
select(data, republican, republican_f)
table(data$republican_f)
#    Democrat Republican 
#         388         248 


## IV3: Political knowledge (PK)
##     (Responses to two control-of-party questions)

table(data$CC21_310a)
#   1   2   3   4 
#  81 717  22 179 

table(data$CC21_310b)
#   1   2   3   4 
# 118 493 195 193 

data$PK[data$CC21_310a == 2 & data$CC21_310b == 2] <- 3 # highest PK

data$PK[data$CC21_310a == 1 & data$CC21_310b == 1] <- 1 # lowest PK
data$PK[data$CC21_310a == 3 & data$CC21_310b == 3] <- 1
data$PK[data$CC21_310a == 1 & data$CC21_310b == 3] <- 1
data$PK[data$CC21_310a == 3 & data$CC21_310b == 1] <- 1

data$PK[data$CC21_310a == 2 & data$CC21_310b == 1] <- 2 # somewhat PK
data$PK[data$CC21_310a == 1 & data$CC21_310b == 2] <- 2
data$PK[data$CC21_310a == 2 & data$CC21_310b == 3] <- 2
data$PK[data$CC21_310a == 3 & data$CC21_310b == 2] <- 2

data <- data %>%
  mutate(PK_f = factor(PK, levels = c(1, 2, 3), 
                             labels = c("low PK", "somewhat PK", "high PK")))
select(data, PK, PK_f)
table(data$PK_f)
# low PK somewhat PK     high PK 
#     53         302         440


## IV4: Policy preference, prior to frame exposure
table(data$UTB431)
#   1   2   3   4 
# 152 236 374 199 

data$prefer_pre[data$UTB431 == 1] <- 1 
data$prefer_pre[data$UTB431 == 2] <- 2
data$prefer_pre[data$UTB431 == 3] <- 3 

table(data$prefer_pre)
#   1   2   3 
# 152 236 374 

data <- data %>%
  mutate(prefer_pre_f = factor(prefer_pre, levels = c(1, 2, 3), 
                             labels = c("Less policies", "Fine as is", "More policies")))
select(data, prefer_pre, prefer_pre_f)
table(data$prefer_pre_f)
#  Less Fine    as is     More 
#        152      236      374 

# Make the variable binary: "no" "yes (more policies)" 
data$prefer_pre_binary[data$prefer_pre <= 2] <- 0
data$prefer_pre_binary[data$prefer_pre == 3] <- 1
table(data$prefer_pre_binary)
#   0   1 
# 388 374

data <- data %>%
  mutate(prefer_pre_binary_f = factor(prefer_pre_binary, levels = c(0, 1), 
                               labels = c("no", "yes")))
select(data, prefer_pre_binary, prefer_pre_binary_f)
table(data$prefer_pre_binary_f)
#  no yes 
# 388 374 


## Age
table(data$birthyr)
data$age <- 2021 - (data$birthyr)
table(data$age)


## Gender
table(data$gender4)
#   1   2   3   4 
# 422 567   8   3 

data$female[data$gender4 == 1] <- 1
data$female[data$gender4 == 2] <- 2

data <- data %>%
  mutate(female_f = factor(female, levels = c(1, 2), 
                           labels = c("male", "female")))
select(data, female, female_f) 
table(data$female_f)
#   1   2
# 422 567


## Income
table(data$faminc_new)
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  97 
# 48  76 103 104  85  94  61  64  82  59  49  41  15  12   4   5  98
data$income <- data$faminc_new 
data$income[data$faminc_new == 97] <- NA # remove "97"
table(data$income)
#  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16 
# 48  76 103 104  85  94  61  64  82  59  49  41  15  12   4   5


### Measure difference in policy preference 'prior to' vs. 'upon' frame exposure

## Make a new variable "outcome": "1" (who said 'YES' in either treatment group)
##                                "0" (who said 'NO'  in either treatment group)
data$outcome[data$UTB434a == 1] <- 1 
data$outcome[data$UTB434a == 2] <- 0
data$outcome[data$UTB436a == 1] <- 1
data$outcome[data$UTB436a == 2] <- 0
count(data, outcome)
#   outcome     n
# 1       0   313
# 2       1   501
# 3      NA   186

data <- data %>%
  mutate(outcome_f = factor(outcome, levels = c(0, 1),
                            labels = c("no", "yes")))
select(data, outcome, outcome_f)
count(data, outcome_f)


################################################################################
### t-test (two sample) + bar plots
library(ggpubr)
library(rstatix)

head(data, 4)

# computing summary statistics
data %>% get_summary_stats(prefer_thematic, prefer_episodic, type = "mean_sd")

#   variable            n  mean    sd
# 1 prefer_thematic   402 0.575 0.495
# 2 prefer_episodic   412 0.655 0.476

t.test(data$prefer_thematic, data$prefer_episodic, var.equal = TRUE)

# Two Sample t-test

# data:  data$prefer_thematic and data$prefer_episodic
# t = -2.3719, df = 812, p-value = 0.01793
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.14750857 -0.01391731
# sample estimates:
#   mean of x mean of y 
# 0.5746269 0.6553398 


### Histogram of selective exposure to two news headline questions

table(data$UTB454)
#   1   2   3   4 
# 183 303 227 282 

table(data$UTB455)
#   1   2   3   4 
# 168 346 193 290 

# Condition 1
data$frame1[data$UTB454 == 1] <- 1 
data$frame1[data$UTB454 == 2] <- 2
data$frame1[data$UTB454 == 3] <- 3
count(data, frame1)
#   frame1     n
# 1      1   183
# 2      2   303
# 3      3   227
# 4     NA   287

#library(ggplot2)
h1 <- ggplot(data, aes(frame1)) +
  labs(x = "News frame type",
       y = "Number of respondents") +
  annotate(geom = "text", x = 1, y = 190, label = "183", color = "black") +
  annotate(geom = "text", x = 2, y = 310, label = "303", color = "black") +
  annotate(geom = "text", x = 3, y = 234, label = "227", color = "black") +
  geom_bar(fill = "black", alpha = 0.3) +
  scale_x_discrete(limits = c("1", "2", "3"), 
                   labels = c("thematic", "episodic", "control")) +
  theme_light() +
  lims(y = c(0, 360)); h1   # Figure 1

# Condition 2
data$frame2[data$UTB455 == 1] <- 1 
data$frame2[data$UTB455 == 2] <- 2
data$frame2[data$UTB455 == 3] <- 3
count(data, frame2)
#   frame1     n
# 1      1   168
# 2      2   346
# 3      3   193
# 4     NA   293

h2 <- ggplot(data, aes(frame2)) +
  labs(x = "News frame type",
       y = "Number of respondents") +
  annotate(geom = "text", x = 1, y = 175, label = "168", color = "black") +
  annotate(geom = "text", x = 2, y = 353, label = "346", color = "black") +
  annotate(geom = "text", x = 3, y = 200, label = "193", color = "black") +
  geom_bar(fill = "black", alpha = 0.3) +
  scale_x_discrete(limits = c("1", "2", "3"), 
                   labels = c("thematic", "episodic", "control")) +
  theme_light() +
  lims(y = c(0, 360)); h2   # Figure 2


### Bar graph of issue preference of Rs exposed to thematic vs. episodic frame
###    (x = "treatment" frame group; y = "outcome" (overall issue preference)
#install.packages("tidyverse")  
install.packages("scales")
install.packages("stringr")
install.packages("Hmisc") 
install.packages("forcats")
install.packages("ggthemes") 

#library("tidyverse") 
library("scales") 
library("stringr") 
library("Hmisc") 
library("forcats") 
library("ggthemes")  

mean_sd_outcome <- data %>% 
  group_by(treatment_f) %>%
  summarise(
    mean_outcome = mean(outcome, na.rm =TRUE),
    sd_outcome = sd(outcome, na.rm = TRUE)
    )
mean_sd_outcome

# Adding p-values as asterisks to the graph
pvalue_outcome <- tibble(
  x = c("thematic", "thematic", "episodic", "episodic"),
  y = c(0.72, 0.75, 0.75, 0.72)
)
# Creating bar graph
p1 <- mean_sd_outcome %>%
  filter(!is.na(treatment_f)) %>%
  ggplot(aes(x = treatment_f, mean_outcome)) +
  geom_col(aes(fill = treatment_f), color = "black", width = 0.75) +
# adding errorbars (standard deviations)
  geom_errorbar(aes(ymin = mean_outcome - sd_outcome,
                    ymax = mean_outcome + sd_outcome),
                color = "#22292F",
                width = 0.1) +
# adding p-value as asterisks
  geom_line(data = pvalue_outcome, 
            aes(x = x, y = y, group = 1)) +
  annotate("text", x = 1.5, y = 0.78,
           label = "p-value = 0.018*",
           size = 6, color = "#22292F") +
# adding percentage
  annotate("text", x = 1, y = 0.61,
           label = "57%",
           size = 6) +
  annotate("text", x = 2, y = 0.69,
           label = "66%",
           size = 6) +
# filling in the bars
  scale_fill_grey(start = 0.4) +
  scale_y_continuous(limits = c(0, 0.8), expand = c(0, 0)
                     #, labels = scales::percent
  ) +
# removing legend + legend color
  guides(fill = "none") +
# getting rid of grey background
  theme_minimal() +
# labeling
  labs(
    x = "News Frame Exposure",
    y = "Proportion of Policy Preference",
    title = ""
    , caption = "Significance codes: . p < .1, * p < .05, ** p < .01, *** p < .001"
  ) +
# adjusting the spacing and the font
  theme(
    plot.title = element_text(size = 20,
                              face = "bold",
                              margin = margin(b = 35)),
    plot.margin = unit(rep(1, 4), "cm"),
    axis.text = element_text(size = 16, color = "#22292F"),
    axis.title = element_text(size = 20, hjust = 1),
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.x = element_text(margin = margin(t = 5)),
    axis.text.y = element_text(margin = margin(r = 5)),
    plot.caption = element_text(size = 12,
                                face = "italic",
                                color = "#606F7B",
                                margin = margin(t = 15)), 
# adding axis ticks and axis lines
    axis.line = element_line(color = "#3D4852"), 
    axis.ticks = element_line(color = "#3D4852"),
    panel.grid.major.y = element_line(color = "#DAE1E7"), # setting color of horizontal grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) 
p1 # label = "Overall issue preference by treatment group"


### Test effects of thematic versus episodic news frames
m1 <- glm(outcome_f ~ treatment_f, 
                data = data, family = "binomial"); summary(m1) # Model 1

m2 <- glm(outcome_f ~ treatment_f + republican_f + PK_f + prefer_pre_binary_f
          , data = data, family = "binomial"); summary(m2) # Model 2

m3 <- glm(outcome ~ treatment_f + republican_f + PK_f + prefer_pre_binary_f +
            (republican_f)*(PK_f)
          , data = data, family = "binomial"); summary(m3) # Model 3

m4 <- glm(outcome ~ treatment_f + republican_f + PK_f + prefer_pre_binary_f +
            (republican_f)*(PK_f) +
            age + female_f + income
          , data = data, family = "binomial"); summary(m4) # Model 4


## Testing if Rs were *randomly assigned* to two treatment groups
m0 <- glm(treatment_f ~ age + female_f + race + income + 
            republican_f + PK_f + prefer_pre_binary_f, 
          data = data, family = "binomial"); summary(m0)


### Confidence interval (CI) plots (Figure 6)

install.packages("dotwhisker")
library(dotwhisker)
#library(dplyr)

dwplot(m4) # creating CI plot of m4
confint(m4) # CIs using profiled log-likelihood
confint.default(m4) # CIs using standard errors

# adding aesthetics legend, etc. to the plot
plot0 <- dwplot(m4,
                vline = geom_vline(
                  xintercept = 0,
                  colour = "grey60",
                  linetype = 2
                ),
                dot_args = list(size = 4),
                whisker_args = list(size = 1.5),
                vars_order = c("treatment_fepisodic", "republican_fRepublican", "PK_fsomewhat PK", "PK_fhigh PK", "prefer_pre_binary_fyes", "republican_fRepublican:PK_fsomewhat PK",
                               "republican_fRepublican:PK_fhigh PK", "age", "female_ffemale", "income")
) +
  theme_bw(base_size = 30) +
  xlab("coefficient estimate") + 
  ylab("") +
  ggtitle("") +
  theme(
    legend.justification = c(0, 0),
    legend.position = "none",
    legend.background = element_rect(colour = "grey80"),
    legend.title = element_blank(),
    #plot.title = element_text(face = "bold", size = 28),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 16)
  ) +
  scale_colour_grey(
    start = .5,
    end = .5
  ) +
  scale_y_discrete(labels=c(
    treatment_fepisodic = "Episodic frame exposure",
    "republican_fRepublican" = "Republican",
    "PK_fsomewhat PK" = "Somewhat PK",
    "PK_fhigh PK" = "High PK",
    "prefer_pre_binary_fyes" = "Prior preference",
    age = "Age",
    "female_ffemale" = "Female",
    income = "Income",
    "republican_fRepublican:PK_fsomewhat PK" = "Republican:Somewhat PK",
    "republican_fRepublican:PK_fhigh PK" = "Republican:High PK")
  ); plot0   # Figure 6


### Interaction effects between party identification and political knowledge (Figure 7)
#install.packages("effects")
#install.packages("sjPlot")
library(effects)
library(sjPlot)

plot_model(m4, type = "int", title = "",
           axis.title = "Policy Preference",
           legend.title = "Political Knowledge",
           dot.size = 4,
           line.size = 1.5,
           grid = FALSE,
           color = "gs") + 
  labs(x = "Party Identification") + theme_bw(base_size = 19) # colored labels


### Interaction effects between party identification and political knowledge (Figure 8)
plot_model(m4, type = "int", title = "",
           axis.title = "Policy Preference",
           legend.title = "Political Knowledge",
           dot.size = 4,
           line.size = 1.5,
           grid = TRUE,
           color = "gs") +
  labs(x = "Party Identification") + 
  set_theme(base = theme_bw(base_size = 14),
            theme.font = '',
            axis.title.size = 1.5,
            axis.textsize.x = 1,
            axis.textsize.y = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # colored labels


### Create plots of interaction effects by each predictor (Figure 9)
install.packages("emmeans")
library(emmeans)

emmeans_m4 <- emmeans(m4, ~ republican_f*PK_f); emmeans_m4
pairs(emmeans_m4)
contrast(emmeans_m4, "consec", simple = "each", combine = TRUE, adjust = "mvt")

emmip(m4, republican_f ~ PK_f, CIs = TRUE, plotit = T,
      xlab = "Political Knowledge",
      ylab = "Policy Preference",
      tlab = "Party ID",
      main = "")+
  theme_bw() +
  scale_colour_grey(start = .7, end = 0) +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  theme(legend.position = "right") 

emmip(m4, PK_f ~ republican_f, CIs = TRUE, plotit = T,
      xlab = "Party Identification",
      ylab = "Policy Preference",
      tlab = "Political Knowledge",
      main = "") +
  theme_bw() +
  scale_colour_grey(start = .7, end = 0) +
  scale_linetype_manual(values = c("solid", "dotdash")) +
  theme(legend.position = "right") 


### Waffle plots for descriptive statistics
install.packages("waffle")
library(waffle)

waffle_prefer = c("No more policies" = 313, "More policies" = 501)
waffle_republican <- c("Democrat" = 388, "Republican" = 248)
waffle_PK <- c("Low PK" = 53, "Somewhat PK" = 302, "High PK" = 440)
waffle_prefer_pre <- c("Less policies" = 152, "Fine as is" = 236, "More policies" = 374)
waffle_prefer_pre_binary <- c("No more policies" = 388, "More policies" = 374)


w1 <- waffle(waffle_prefer/30, rows = 4, colors = c("#DFE0E2", "#0A0B0B", "white")) +
  theme(legend.text = element_text(size = 14),
        legend.key.size = unit(1, "cm")); w1  # Figure 3

w2 <- waffle(waffle_prefer_pre/35, rows = 4, colors = c("#DFE0E2", "#888D96", "#0A0B0B", "white")) +
  theme(legend.text = element_text(size=15),
        legend.key.size = unit(1, "cm")); w2

w3<- waffle(waffle_prefer_pre_binary/35, rows = 4, colors = c("#DFE0E2", "#0A0B0B", "white")) +
  theme(legend.text = element_text(size=15),
        legend.key.size = unit(1, "cm")); w3  # Figure 4

w4 <- waffle(waffle_republican/25, rows = 4, colors = c("#DFE0E2", "#0A0B0B", "white")) +
  theme(legend.text = element_text(size=15),
        legend.key.size = unit(1, "cm")); w4

w5 <- waffle(waffle_PK/25, rows = 4, colors = c("#DFE0E2", "#888D96", "#0A0B0B", "white")) +
  theme(legend.text = element_text(size=15),
        legend.key.size = unit(1, "cm")); w5

################################################################################
################################END OF SCRIPT###################################