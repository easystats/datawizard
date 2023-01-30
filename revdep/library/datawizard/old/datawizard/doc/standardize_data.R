## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
options(knitr.kable.NA = "")
knitr::opts_chunk$set(
  comment = "#>",
  message = FALSE,
  warning = FALSE,
  dpi = 300
)

pkgs <- c(
  "datawizard",
  "poorman",
  "see",
  "ggplot2",
  "parameters",
  "lme4"
)

if (!all(sapply(pkgs, requireNamespace, quietly = TRUE))) {
  knitr::opts_chunk$set(eval = FALSE)
}

## -----------------------------------------------------------------------------
library(datawizard)
library(effectsize) # for data

# let's have a look at what the data look like
data("hardlyworking", package = "effectsize")
head(hardlyworking)

# let's use both methods of standardization
hardlyworking$xtra_hours_z <- standardize(hardlyworking$xtra_hours)
hardlyworking$xtra_hours_zr <- standardize(hardlyworking$xtra_hours, robust = TRUE)

## ---- eval=FALSE--------------------------------------------------------------
#  library(dplyr)
#  
#  hardlyworking %>%
#    select(starts_with("xtra_hours")) %>%
#    data_to_long() %>%
#    group_by(Name) %>%
#    summarise(
#      mean = mean(Value),
#      sd = sd(Value),
#      median = median(Value),
#      mad = mad(Value)
#    )

## ---- echo=FALSE--------------------------------------------------------------
library(poorman)

hardlyworking %>%
  select(starts_with("xtra_hours")) %>%
  reshape_longer(names_to = "name", values_to = "value") %>%
  group_by(name) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    mad = mad(value)
  ) %>%
  knitr::kable(digits = 4)

## -----------------------------------------------------------------------------
hardlyworking_z <- standardize(hardlyworking)

## ---- eval=FALSE--------------------------------------------------------------
#  hardlyworking_z %>%
#    select(-xtra_hours_z, -xtra_hours_zr) %>%
#    data_to_long() %>%
#    group_by(Name) %>%
#    summarise(
#      mean = mean(Value),
#      sd = sd(Value),
#      median = median(Value),
#      mad = mad(Value)
#    )

## ---- echo=FALSE--------------------------------------------------------------
hardlyworking_z %>%
  select(-xtra_hours_z, -xtra_hours_zr) %>%
  reshape_longer(names_to = "name", values_to = "value") %>%
  group_by(name) %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value),
    mad = mad(value)
  ) %>%
  knitr::kable(digits = 4)

## ---- eval=FALSE--------------------------------------------------------------
#  # Download the 'emotion' dataset
#  load(url("https://raw.githubusercontent.com/neuropsychology/psycho.R/master/data/emotion.rda"))
#  
#  # Discard neutral pictures (keep only negative)
#  emotion <- emotion %>% filter(Emotion_Condition == "Negative")
#  
#  # Summary
#  emotion %>%
#    drop_na(Subjective_Valence, Autobiographical_Link) %>%
#    group_by(Participant_ID) %>%
#    summarise(
#      n_Trials = n(),
#      Valence_Mean = mean(Subjective_Valence),
#      Valence_SD = sd(Subjective_Valence)
#    )

## ---- echo=FALSE--------------------------------------------------------------
load(url("https://raw.githubusercontent.com/neuropsychology/psycho.R/master/data/emotion.rda"))

# Discard neutral pictures (keep only negative)
emotion <- emotion %>% filter(Emotion_Condition == "Negative")

# Summary
emotion %>%
  subset(!(is.na(Subjective_Valence) | is.na(Autobiographical_Link))) %>%
  group_by(Participant_ID) %>%
  summarise(
    n_Trials = n(),
    Valence_Mean = mean(Subjective_Valence),
    Valence_SD = sd(Subjective_Valence)
  )

## ---- warning=FALSE-----------------------------------------------------------
Z_VariableWise <- emotion %>%
  standardize()

Z_ParticipantWise <- emotion %>%
  group_by(Participant_ID) %>%
  standardize()

Z_Full <- emotion %>%
  group_by(Participant_ID) %>%
  standardize() %>%
  ungroup() %>%
  standardize()

## ---- eval=FALSE--------------------------------------------------------------
#  # Create a convenient function to print
#  summarise_Subjective_Valence <- function(data) {
#    df_name <- deparse(substitute(data))
#    data %>%
#      ungroup() %>%
#      summarise(
#        DF = df_name,
#        Mean = mean(Subjective_Valence),
#        SD = sd(Subjective_Valence)
#      )
#  }
#  # Check the results
#  rbind(
#    summarise_Subjective_Valence(Z_VariableWise),
#    summarise_Subjective_Valence(Z_ParticipantWise),
#    summarise_Subjective_Valence(Z_Full)
#  )

## ---- echo=FALSE--------------------------------------------------------------
# Create a convenient function to print
summarise_Subjective_Valence <- function(data) {
  df_name <- deparse(substitute(data))
  data <- data %>%
    ungroup() %>%
    summarise(
      Mean = mean(Subjective_Valence),
      SD = sd(Subjective_Valence)
    )
  cbind(DF = df_name, data)
}
# Check the results
rbind(
  summarise_Subjective_Valence(Z_VariableWise),
  summarise_Subjective_Valence(Z_ParticipantWise),
  summarise_Subjective_Valence(Z_Full)
) %>%
  knitr::kable(digits = 2)

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center'-------
library(see)
library(ggplot2)

ggplot() +
  geom_density(aes(Z_VariableWise$Subjective_Valence,
    color = "Z_VariableWise"
  ), linewidth = 1) +
  geom_density(aes(Z_ParticipantWise$Subjective_Valence,
    color = "Z_ParticipantWise"
  ), linewidth = 1) +
  geom_density(aes(Z_Full$Subjective_Valence,
    color = "Z_Full"
  ), linewidth = 1) +
  see::theme_modern() +
  labs(color = "")

## ---- eval=FALSE--------------------------------------------------------------
#  # Create convenient function
#  print_participants <- function(data) {
#    df_name <- deparse(substitute(data))
#    data %>%
#      group_by(Participant_ID) %>%
#      summarise(
#        DF = df_name,
#        Mean = mean(Subjective_Valence),
#        SD = sd(Subjective_Valence)
#      ) %>%
#      head(5) %>%
#      select(DF, everything())
#  }
#  
#  # Check the results
#  rbind(
#    print_participants(Z_VariableWise),
#    print_participants(Z_ParticipantWise),
#    print_participants(Z_Full)
#  )

## ---- echo=FALSE--------------------------------------------------------------
# Create convenient function
print_participants <- function(data) {
  df_name <- deparse(substitute(data))
  data %>%
    group_by(Participant_ID) %>%
    summarise(
      Mean = mean(Subjective_Valence),
      SD = sd(Subjective_Valence)
    ) %>%
    cbind(DF = df_name, .) %>%
    head(5) %>%
    select(DF, everything())
}

# Check the results
rbind(
  print_participants(Z_VariableWise),
  print_participants(Z_ParticipantWise),
  print_participants(Z_Full)
) %>%
  knitr::kable(digits = 2)

## ---- fig.width=7, fig.height=4.5, results='markup', fig.align='center'-------
r <- cor.test(
  Z_VariableWise$Subjective_Valence,
  Z_ParticipantWise$Subjective_Valence
)

data.frame(
  Original = emotion$Subjective_Valence,
  VariableWise = Z_VariableWise$Subjective_Valence,
  ParticipantWise = Z_ParticipantWise$Subjective_Valence
) %>%
  ggplot(aes(x = VariableWise, y = ParticipantWise, colour = Original)) +
  geom_point(alpha = 0.75, shape = 16) +
  geom_smooth(method = "lm", color = "black") +
  scale_color_distiller(palette = 1) +
  ggtitle(paste0("r = ", round(r$estimate, 2))) +
  see::theme_modern()

## -----------------------------------------------------------------------------
library(lme4)
m_raw <- lmer(
  formula = Subjective_Valence ~ Autobiographical_Link + (1 | Participant_ID),
  data = emotion
)
m_VariableWise <- update(m_raw, data = Z_VariableWise)
m_ParticipantWise <- update(m_raw, data = Z_ParticipantWise)
m_Full <- update(m_raw, data = Z_Full)

## -----------------------------------------------------------------------------
# Convenient function
get_par <- function(model) {
  mod_name <- deparse(substitute(model))
  parameters::model_parameters(model) %>%
    mutate(Model = mod_name) %>%
    select(-Parameter) %>%
    select(Model, everything()) %>%
    .[-1, ]
}

# Run the model on all datasets
rbind(
  get_par(m_raw),
  get_par(m_VariableWise),
  get_par(m_ParticipantWise),
  get_par(m_Full)
)

