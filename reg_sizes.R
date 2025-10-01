# -------------------------
# Libraries
# -------------------------
library(fastDummies)
library(stargazer)
library(lme4)
library(car)
library(effectsize)
library(dplyr)

# -------------------------
# Load and preprocess
# -------------------------
dat <- read.csv("../data/hackathons_combined.csv")
dat <- dat[dat$actual_participants_N_x > 0, ]

# Compute women %
dat$women_perc <- dat$females_participating_x / dat$actual_participants_N_x


# Dummies (keep only if you actually use them)
dat <- dummy_cols(dat, select_columns = "classification_method4")
dat <- dummy_cols(dat, select_columns = "continent")

# Create event size bins (adjust thresholds if needed)
# -------------------------
# Create quartiles by event size
# -------------------------
dat$event_size_quartile <- cut(
  dat$actual_participants_N_x,
  breaks = quantile(dat$actual_participants_N_x, probs = seq(0, 1, 0.25), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Q1: Smallest 25%", 
             "Q2: 25–50%", 
             "Q3: 50–75%", 
             "Q4: Largest 25%")
)

table(dat$event_size_quartile)

# Build: per-quartile Type II ANOVA p-values + BH adjustment
anova_bh <- lapply(names(models_q), function(q) {
  m  <- models_q[[q]]
  a2 <- car::Anova(m, type = 2)                 # term-level F-tests
  df <- as.data.frame(a2)
  df$term <- rownames(df)
  df$quartile <- q
  tibble(
    quartile = q,
    term     = rownames(a2),
    F        = a2[,"F value"],
    p_raw    = a2[,"Pr(>F)"]
  )
}) |> bind_rows() |>
  group_by(quartile) |>
  mutate(p_bh = p.adjust(p_raw, method = "BH"),
         sig  = case_when(p_bh < .001 ~ "***",
                          p_bh < .01  ~ "**",
                          p_bh < .05  ~ "*",
                          TRUE        ~ "")) |>
  ungroup()

# If you also want to merge in partial eta^2 (from your prior code):
etas_df <- do.call(rbind, etas_q) |>
  rename(term = Parameter) |>
  mutate(partial_eta2 = as.numeric(Eta2_partial)) |>
  select(quartile, term, partial_eta2)

results_terms <- anova_bh |>
  left_join(etas_df, by = c("quartile","term"))

print(results_terms, n=36)


# -------------------------
# Stratified regressions by quartile
# -------------------------
models_q <- list()
for (q in levels(dat$event_size_quartile)) {
  cat("\n--- Results for", q, "---\n")
  
  subdat <- droplevels(subset(dat, event_size_quartile == q))
  
  m_q <- lm(
    women_perc ~
      classification_method4_1 +
      classification_method4_2 +
      is_learning +
      actual_participants_N_x +   # you can drop this if you don’t want size inside quartile
      registered_N_x +
      duration_days +
      participation_type_x +
      year_x,
    data = subdat
  )
  
  print(summary(m_q))
  models_q[[q]] <- m_q
}


etas_q <- lapply(names(models_q), function(q) {
  a2 <- car::Anova(models_q[[q]], type = 2)
  es <- effectsize::eta_squared(a2, partial = TRUE, ci = 0.95)
  cbind(quartile = q, as.data.frame(es))
})

etas_q

eta_tbl <- bind_rows(etas_q)

# Show the dummies and learning indicator
eta_tbl %>%
  filter(Parameter %in% c("classification_method4_1", "classification_method4_2", "is_learning")) %>%
  select(quartile, Parameter, Eta2, CI_low, CI_high) %>%
  arrange(Parameter, quartile) %>%
  print()

vif(models_q[[1]])
vif(models_q[[2]])
vif(models_q[[3]])
vif(models_q[[4]])

