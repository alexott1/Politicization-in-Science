# =============================================================================
# What about the Politicization of Science in Europe?
# Evaluating public trust in science, Germany 1980-2021
#
# Replication script. Run from the repository root.
# Data (not included): place the two ALLBUS files in ./data/
#   - ZA5274_v1-1-0.dta  (ALLBUS cumulation 1980-2018)
#   - ZA5280_v2-0-0.sav  (ALLBUS 2021)
# =============================================================================

library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)
library(grid)
library(gridExtra)
library(sjPlot)

dir.create("output", showWarnings = FALSE)
theme_set(theme_bw())


# -----------------------------------------------------------------------------
# 1. Load and merge data
# -----------------------------------------------------------------------------

allbus1980_2018 <- read_dta("data/ZA5274_v1-1-0.dta")
allbus2021      <- read_sav("data/ZA5280_v2-0-0.sav")

# In ALLBUS 2021 the ISCED-97 variable is called `isced97` (in the cumulation:
# `iscd975`). Without this step, all 2021 respondents are dropped from the
# regression models because education is missing. ISCED 6 (only coded in 2021)
# is merged into 5, as the cumulation only goes up to 5.
allbus2021 <- allbus2021 %>%
  mutate(iscd975 = if_else(as.numeric(isced97) == 6, 5, as.numeric(isced97)))

vars <- c("year", "sm01", "ingle", "ps03", "ma04", "pn16", "pn11", "pv01",
          "rp01", "age", "di01a", "incc", "sex", "iscd975", "va01", "pa01",
          "fr04a", "dw01", "pt11", "eastwest", "im20", "im21", "wghtpew")

alldata <- bind_rows(
  allbus1980_2018 %>% select(any_of(vars)) %>% zap_labels(),
  allbus2021      %>% select(any_of(vars)) %>% zap_labels() %>% mutate(year = 2021)
)

# All negative codes are ALLBUS missing codes (no answer, don't know,
# not asked, ...). Note: for di01a/incc this also drops "no income" (-50).
alldata <- alldata %>%
  mutate(across(-c(year, wghtpew), ~ if_else(.x < 0, NA_real_, as.numeric(.x))))


# -----------------------------------------------------------------------------
# 2. Recoding
# -----------------------------------------------------------------------------

alldata <- alldata %>%
  mutate(
    # Dependent variable: trust in universities (1-7) -> 1 = 5-7, 0 = 1-4
    pt11_01 = case_when(pt11 >= 5 ~ 1, pt11 >= 1 ~ 0),

    # Left-right self-placement (1-10)
    pa01_01 = case_when(pa01 <= 3 ~ "left",
                        pa01 <= 7 ~ "moderate",
                        pa01 <= 10 ~ "right"),
    left     = as.numeric(pa01_01 == "left"),
    moderate = as.numeric(pa01_01 == "moderate"),
    right    = as.numeric(pa01_01 == "right"),

    # Education (ISCED-97). Reference: basic + lower secondary (ISCED 1-2).
    # (ISCED 1 alone is only ~1 % of the sample - too small as a reference.)
    edu_upper_sec = as.numeric(iscd975 == 3),
    edu_post_sec  = as.numeric(iscd975 == 4),
    edu_tertiary  = as.numeric(iscd975 == 5),

    # Controls
    east      = case_when(eastwest == 2 ~ 1, eastwest == 1 ~ 0),
    female    = case_when(sex == 2 ~ 1, sex %in% c(1, 3) ~ 0),
    rp01_01   = case_when(rp01 <= 3 ~ 1, rp01 >= 4 ~ 0),  # church at least monthly
    age_div10 = age / 10,
    agesq     = age_div10^2,
    time      = year - 1980,
    # Rescaled versions for the models (otherwise glmer does not converge):
    # time in decades since 1980, age in decades centered at 50 years
    time10    = time / 10,
    age_c     = age_div10 - 5,
    agesq_c   = age_c^2,
    cohort    = year - age,

    # Cultural break: 1 = survey year after the founding of the AfD (2013)
    after_afd = as.numeric(year >= 2013)
  )

# Income: net income of respondent. In 2021 the open question (di01a) is only
# answered by ~25 %; the remaining respondents answered in categories (incc).
# For 2021 the category midpoints fill the gaps. Income is logged and
# z-standardized WITHIN each survey year (inflation, 40 years of data).
incc_mid <- c(100, 250, 350, 450, 550, 675, 812.5, 937.5, 1062.5, 1187.5,
              1312.5, 1437.5, 1625, 1875, 2125, 2375, 2625, 2875, 3250,
              3750, 4250, 4750, 5500, 6750, 8750, 12500)

alldata <- alldata %>%
  mutate(income = coalesce(di01a, if_else(year == 2021, incc_mid[incc], NA_real_))) %>%
  group_by(year) %>%
  mutate(z_income = as.numeric(scale(log(income)))) %>%
  ungroup()


# -----------------------------------------------------------------------------
# 3. Conservatism index (12 items, based on Minkenberg 1992)
# -----------------------------------------------------------------------------
# NOTE: The items are not asked in every survey year (e.g. 2012: only 5-7 of
# the 12 items). The original version replaced missing items with 0, so in
# years with few items almost nobody reached the threshold (East 2012: n = 1
# "strictly conservative"). The index is therefore now the SHARE of fulfilled
# items among the items a respondent answered (min. 5 answered items).
# Thresholds correspond to 4/12 and 5/12 of the original index.
#
# To check (content): sm01 = 1 means "union member", dw01 = 6 means "worker".
# These are social-structure items, not attitudes - please justify them in
# the paper with Minkenberg (1992) or drop them.

alldata <- alldata %>%
  mutate(
    c_sm01  = case_when(sm01 == 1 ~ 1, sm01 == 2 ~ 0),           # union member
    c_dw01  = case_when(dw01 == 6 ~ 1, !is.na(dw01) ~ 0),         # worker
    c_ingle = case_when(ingle == 4 ~ 1, ingle <= 3 ~ 0),          # materialist
    c_fr04a = case_when(fr04a <= 2 ~ 1, fr04a >= 3 ~ 0),          # woman should stay home
    c_ps03  = case_when(ps03 >= 5 ~ 1, ps03 <= 4 ~ 0),            # dissatisfied w/ democracy
    c_ma04  = case_when(ma04 >= 6 ~ 1, ma04 <= 5 ~ 0),            # foreigners marry among themselves
    c_rp01  = rp01_01,                                            # church attendance
    c_pn11  = case_when(pn11 <= 2 ~ 1, pn11 >= 3 ~ 0),            # proud to be German
    c_pn16  = case_when(pn16 <= 2 ~ 1, pn16 >= 3 ~ 0),            # attached to Germany
    c_va01  = case_when(va01 <= 2 ~ 1, va01 >= 3 ~ 0),            # law and order important
    c_im20  = case_when(im20 <= 2 ~ 1, im20 >= 3 ~ 0),            # rank differences acceptable
    c_im21  = case_when(im21 <= 2 ~ 1, im21 >= 3 ~ 0)             # social differences just
  )

c_items <- grep("^c_", names(alldata), value = TRUE)

alldata <- alldata %>%
  mutate(
    cons_answered = rowSums(!is.na(across(all_of(c_items)))),
    cons_share    = if_else(cons_answered >= 5,
                            rowMeans(across(all_of(c_items)), na.rm = TRUE),
                            NA_real_),
    moderately_conservative = as.numeric(cons_share >= 4 / 12),
    strictly_conservative   = as.numeric(cons_share >= 5 / 12)
  )


# -----------------------------------------------------------------------------
# 4. Descriptive figures
# -----------------------------------------------------------------------------
# "Germany" panels are weighted with the east-west weight (wghtpew), because
# East Germans are oversampled in ALLBUS since 1991.

plotdata <- alldata %>% filter(!is.na(pt11_01), !is.na(pa01_01))

region_means <- function(data, group_var, value_var) {
  bind_rows(
    data %>% filter(east == 0) %>% mutate(region = "West Germany", w = 1),
    data %>% filter(east == 1) %>% mutate(region = "East Germany", w = 1),
    data %>% mutate(region = "Germany", w = wghtpew)
  ) %>%
    filter(!is.na({{ value_var }}), !is.na({{ group_var }})) %>%
    group_by(region, year, group = {{ group_var }}) %>%
    summarise(mean = weighted.mean({{ value_var }}, w), n = n(), .groups = "drop") %>%
    mutate(region = factor(region, c("West Germany", "East Germany", "Germany")))
}

# Figure: trust by ideology
fig_trust_ideology <- region_means(plotdata, pa01_01, pt11_01) %>%
  ggplot(aes(factor(year), mean, colour = group, group = group)) +
  geom_line() + geom_point() +
  facet_wrap(~ region) +
  coord_cartesian(ylim = c(0.5, 0.85)) +
  labs(x = "Year", y = "Trust in Universities and Higher Education", colour = NULL,
       title = "Trust in Universities and Higher Education")
ggsave("output/fig_trust_ideology.png", fig_trust_ideology, width = 11, height = 5)

# Figure: right vs. conservative
cons_long <- plotdata %>%
  mutate(id = row_number()) %>%
  select(id, year, east, wghtpew, pt11_01, right, moderately_conservative, strictly_conservative) %>%
  pivot_longer(c(right, moderately_conservative, strictly_conservative),
               names_to = "group", values_to = "member") %>%
  filter(member == 1) %>%
  mutate(group = recode(group, right = "right",
                        moderately_conservative = "moderately conservative",
                        strictly_conservative = "strictly conservative"))

fig_trust_conservative <- region_means(cons_long, group, pt11_01) %>%
  ggplot(aes(year, mean, colour = group)) +
  geom_line() + geom_point() +
  facet_wrap(~ region) +
  coord_cartesian(ylim = c(0.4, 0.9)) +
  scale_colour_manual(values = c("right" = "steelblue2",
                                 "moderately conservative" = "darkgrey",
                                 "strictly conservative" = "black")) +
  labs(x = "Year", y = "Trust in Universities and Higher Education", colour = NULL,
       title = "Trust in Universities and Higher Education: Right and Conservative")
ggsave("output/fig_trust_conservative.png", fig_trust_conservative, width = 11, height = 5)

# Group sizes behind the conservative figure (check for small n!)
print(region_means(cons_long, group, pt11_01) %>% select(-mean) %>%
        pivot_wider(names_from = group, values_from = n), n = Inf)

# Figure: ideological distribution (respondents with valid pt11)
share_plot <- function(data, title) {
  bind_rows(
    data %>% filter(east == 0) %>% mutate(region = "West Germany", w = 1),
    data %>% filter(east == 1) %>% mutate(region = "East Germany", w = 1),
    data %>% mutate(region = "Germany", w = wghtpew)
  ) %>%
    mutate(region = factor(region, c("West Germany", "East Germany", "Germany"))) %>%
    ggplot(aes(factor(year), weight = w, fill = pa01_01)) +
    geom_bar(position = "fill") +
    facet_wrap(~ region) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(x = "Year", y = NULL, fill = NULL, title = title)
}

fig_ideology <- share_plot(plotdata, "Proportion of Survey Participants: Left/Moderate/Right")
ggsave("output/fig_ideology_distribution.png", fig_ideology, width = 11, height = 5)

# Figure: ideology of AfD voters (vote intention, pv01 = 42)
afd <- alldata %>% filter(pv01 == 42, !is.na(pa01_01))
fig_afd <- share_plot(afd, "Distribution of AfD Voters over Political Ideologies")
ggsave("output/fig_afd_voters.png", fig_afd, width = 11, height = 5)


# -----------------------------------------------------------------------------
# 5. Model data and descriptive table
# -----------------------------------------------------------------------------

model_vars <- c("pt11_01", "time", "time10", "female", "east",
                "edu_upper_sec", "edu_post_sec", "edu_tertiary",
                "rp01_01", "z_income", "age_div10", "agesq",
                "left", "right", "moderately_conservative", "strictly_conservative",
                "after_afd", "cohort")

modeldata <- alldata %>% drop_na(all_of(model_vars))

# Observations per year in the models
print(table(modeldata$year))

desc_table <- modeldata %>%
  summarise(across(all_of(model_vars),
                   list(mean = mean, sd = sd, min = min, max = max))) %>%
  pivot_longer(everything(), names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd|min|max)$")
print(desc_table, n = Inf)
write_csv(desc_table, "output/descriptives.csv")


# -----------------------------------------------------------------------------
# 6. Models
# -----------------------------------------------------------------------------
# Hierarchical APC models (Yang & Land 2006, 2008): period and age as fixed
# effects, cohort as random intercept. Reference ideology: moderate.
# time10 = decades since 1980; age_c = age in decades, centered at 50 years.

ctrl <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

base <- pt11_01 ~ time10 + female + east + edu_upper_sec + edu_post_sec + edu_tertiary +
  rp01_01 + z_income + age_c + agesq_c +
  left + right + moderately_conservative + strictly_conservative + after_afd

f1 <- update(base, . ~ . + (1 | cohort))
f2 <- update(base, . ~ . + time10:left + time10:right + time10:moderately_conservative +
               time10:strictly_conservative + (1 | cohort))
f3 <- update(base, . ~ . + after_afd:left + after_afd:right + after_afd:moderately_conservative +
               after_afd:strictly_conservative + (1 | cohort))

# Logistic mixed models (main models)
model1 <- glmer(f1, data = modeldata, family = binomial, control = ctrl)
model2 <- glmer(f2, data = modeldata, family = binomial, control = ctrl)
model3 <- glmer(f3, data = modeldata, family = binomial, control = ctrl)

# Linear mixed models (robustness check, linear probability model)
model4 <- lmer(f1, data = modeldata)
model5 <- lmer(f3, data = modeldata)

summary(model1)
summary(model2)
summary(model3)

tab_model(model1, model2, model3, show.ci = FALSE,
          dv.labels = c("Model 1", "Model 2", "Model 3"),
          file = "output/table_logit.html")
tab_model(model4, model5, show.ci = FALSE,
          dv.labels = c("LPM 1", "LPM 2"),
          file = "output/table_lpm.html")


# -----------------------------------------------------------------------------
# 7. Average marginal effects (Model 1)
# -----------------------------------------------------------------------------
# AME of a dummy = mean predicted probability with x = 1 minus with x = 0,
# averaged over all respondents.

ame <- function(model, data, var) {
  d1 <- data; d1[[var]] <- 1
  d0 <- data; d0[[var]] <- 0
  mean(predict(model, d1, type = "response")) - mean(predict(model, d0, type = "response"))
}

dummies <- c("female", "east", "edu_upper_sec", "edu_post_sec", "edu_tertiary",
             "rp01_01", "left", "right", "moderately_conservative",
             "strictly_conservative", "after_afd")

ames <- tibble(variable = dummies,
               AME = map_dbl(dummies, ~ ame(model1, modeldata, .x)))
print(ames)
write_csv(ames, "output/ame_model1.csv")
