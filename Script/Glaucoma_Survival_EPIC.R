library(survival)
library(survminer)
library(dplyr)
library(haven)
library(lubridate)
library(readr)
library(broom)
library(survminer)

# Glaucoma_SR_GWAS, Glaucoma_SR_3HC, Deaths, EyeStudy

censor_date <- as.Date("2018-04-01")
PRS <- read_tsv("../Links/Best_POAG_MTAG_MA_and_MTAG_2023_PRS_EPIC_survival_data_RL_21K_05MAR2026.tsv")
Best_EUR_MTAG_PRS_EPIC <- read_table("../Data/Best_EUR_MTAG_PRS_EPIC_for_RL_26FEB26_1.profile")
Best_MA_MTAG_PRS_EPIC <- read_table("../Data/Best_MA_MTAG_PRS_EPIC_for_RL_26FEB26_1.profile")
EyeStudy <- read_dta("/hddraid5/Store/EPIC/Glaucoma/EPIC_Glaucoma_POAG_akid.dta") %>% select(-sex, -akid)
POAG <- read_dta("/hddraid5/Store/EPIC/Glaucoma/EPIC_ICD10_GlaucomaInpatient_POAG_omicsid.dta")
GWAS <- read_dta("/hddraid5/Store/EPIC/Phenotypes/EPIC_genetics_pheno_20210413.dta")
Glaucoma_SR_GWAS <- read_dta("../Data/glaucoma_SR_GWAS_MinDiag_omicsid.dta")
Glaucoma_HES <- read_dta("../Data/Glaucoma_HES_Age_omicsid.dta")
Glaucoma_EyeStudy <- read_dta("../Glaucoma_Age/EPIC_glaucoma_age_diagnosis_20260506_omicsid.dta")
Deaths <- read_dta("../Data/EPICN_Deaths_omicsid.dta") %>% select(-akid)


# Create a survival dataset
Best_EUR_MTAG_PRS_EPIC <- Best_EUR_MTAG_PRS_EPIC %>%
                          mutate(PRS_EU = SCORESUM) %>%
                          mutate(qPRS_EU = statar::xtile(PRS_EU, 5)) %>%
                          mutate(dPRS_EU = statar::xtile(PRS_EU, 10)) %>%
                          mutate(ePRS_EU = case_when(dPRS_EU==1 ~ 1, dPRS_EU>1 & dPRS_EU<9 ~ 2, dPRS_EU==10 ~ 3)) %>% 
                          mutate(omicsid=IID) %>% select(omicsid, PRS_EU, qPRS_EU, ePRS_EU)

Best_MA_MTAG_PRS_EPIC <- Best_MA_MTAG_PRS_EPIC %>%
                         mutate(PRS_MA = SCORESUM) %>%
                         mutate(qPRS_MA = statar::xtile(PRS_MA, 5))  %>%
                         mutate(dPRS_MA = statar::xtile(PRS_MA, 10)) %>%
                         mutate(ePRS_MA = case_when(dPRS_MA==1 ~ 1, dPRS_MA>1 & dPRS_MA<9 ~ 2, dPRS_MA==10 ~ 3)) %>% 
                         mutate(omicsid=IID) %>% select(omicsid, PRS_MA, qPRS_MA, ePRS_MA)


# Create quintiles and categories
PRS <- PRS %>% 
     mutate(omicsid=IID) %>% 
     mutate(PRS2026_MA = Best_MA_MTAG_std) %>%
     mutate(qPRS2026_MA = statar::xtile(PRS2026_MA, 5))  %>%
     mutate(dPRS2026_MA = statar::xtile(PRS2026_MA, 10)) %>%
     mutate(ePRS2026_MA = case_when(dPRS2026_MA==1 ~ 1, dPRS2026_MA>1 & dPRS2026_MA<9 ~ 2, dPRS2026_MA==10 ~ 3)) %>% 
     mutate(PRS2023_MA = MTAG_2023_std) %>%
     mutate(qPRS2023_MA = statar::xtile(PRS2023_MA, 5))  %>%
     mutate(dPRS2023_MA = statar::xtile(PRS2023_MA, 10)) %>%
     mutate(ePRS2023_MA = case_when(dPRS2023_MA==1 ~ 1, dPRS2023_MA>1 & dPRS2023_MA<9 ~ 2, dPRS2023_MA==10 ~ 3)) %>% 
     select(omicsid,matches("PRS2026"),matches("PRS2023"))
   

# Join the various data sources
df <- left_join(Best_EUR_MTAG_PRS_EPIC, Best_MA_MTAG_PRS_EPIC, by="omicsid")
df <- left_join(df,PRS, by="omicsid")
df <- left_join(df,EyeStudy, by="omicsid")
df <- left_join(df,Glaucoma_SR_GWAS, by="omicsid")
df <- left_join(df,Glaucoma_HES, by="omicsid")
df <- left_join(df,Deaths, by="omicsid")
df <- left_join(df,Glaucoma_EyeStudy, by="omicsid")


# Construct event age and glaucoma status
# Note that status1 and age_at_event1 do not consider glaucoma events at 3HC while
# status2 and age_at_event2 use the 3HC events (glaucoma_indicator and approx_age variables)
df <- df %>%
      mutate(age2018 = (as.numeric(difftime(censor_date, dob, units = "days")) / 365.25)) %>% 
      mutate(agedeath = (as.numeric(difftime(dod, dob, units = "days")) / 365.25)) %>% 
		mutate(approx_age = ifelse(is.na(approx_age),age3,approx_age)) %>% 
      mutate(status1 = ifelse(glaucoma_SR==1 | glaucoma_hosp==1, 1, 0)) %>% 
      mutate(age_at_event1 = pmin(min_diag_age,glaucoma_hosp_age,agedeath,age2018,na.rm = TRUE)) %>% 
      mutate(status2 = ifelse(glaucoma_SR==1 | glaucoma_hosp==1 | glaucoma_indicator==1, 1, 0)) %>% 
      mutate(age_at_event2 = pmin(min_diag_age,glaucoma_hosp_age,approx_age,agedeath,age2018,na.rm = TRUE))

# Comment these out as required
# Without 3HC events
# df <- df %>% mutate(status = status1, age_at_event=age_at_event1)
# Glaucoma_Definition <- "Glaucoma defined using hospital episode statistics and self-report."
# With 3HC events
df <- df %>% mutate(status = status2, age_at_event=age_at_event2)
Glaucoma_Definition <- "Glaucoma defined using hospital episode statistics, self-report and clinical diagnosis where available."

df <- df %>% mutate(status = ifelse(is.na(status),0,status))

# Manually correct some odd-looking data
df <- df %>% mutate(sex = ifelse(sex==-9 & omicsid=="OmicsE00717", 2, sex))
df <- df %>% mutate(sex = ifelse(sex==-9 & omicsid=="OmicsE04205", 2, sex))
df <- df %>% mutate(sex = ifelse(sex==-9 & omicsid=="OmicsE06284", 1, sex))
df <- df %>% mutate(min_diag_age = ifelse(min_diag_age<0, glaucoma_hosp_age, min_diag_age))
df <- df %>% mutate(age_at_event = ifelse(age_at_event<0, glaucoma_hosp_age, age_at_event))

# Stata copy of the dataset
df %>% write_dta("../Data/EPICN_Survival_V2.dta")

# Make quantiles into factors
df <- df %>% mutate(qPRS2026_MA = factor(qPRS2026_MA),
                    ePRS2026_MA = factor(ePRS2026_MA),
                    qPRS2023_MA = factor(qPRS2023_MA),
                    ePRS2023_MA = factor(ePRS2023_MA))

# Remove prevalent cases
df <- df %>% rename(age_at_recruitment=age)
df <- df %>% filter(age_at_event > age_at_recruitment)

# Count of remaining cohort
CohortN <- df %>% count %>% pull
CohortN <- formatC(CohortN, big.mark=",")

# Create survival objects and fits for MH, Cox HR and trend
surv_fit_MA26_1 <- survfit(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ qPRS2026_MA, data = df)
surv_fit_MA26_2 <- survfit(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ ePRS2026_MA, data = df)
cox_fit_MA26_1 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ qPRS2026_MA, data = df)
cox_fit_MA26_2 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ ePRS2026_MA, data = df)
trend_fit_MA26_1 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ as.numeric(qPRS2026_MA), data = df)
trend_fit_MA26_2 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ as.numeric(ePRS2026_MA), data = df)

surv_fit_MA23_1 <- survfit(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ qPRS2023_MA, data = df)
surv_fit_MA23_2 <- survfit(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ ePRS2023_MA, data = df)
cox_fit_MA23_1 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ qPRS2023_MA, data = df)
cox_fit_MA23_2 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ ePRS2023_MA, data = df)
trend_fit_MA23_1 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ as.numeric(qPRS2023_MA), data = df)
trend_fit_MA23_2 <- coxph(Surv(time=age_at_recruitment, time2=age_at_event, event=status) ~ as.numeric(ePRS2023_MA), data = df)


# Use broom::tidy to get HRs, p-values and trend from the Cox models
tidy_cox_MA26_1 <- tidy(cox_fit_MA26_1, exponentiate = TRUE, conf.int = TRUE)
tidy_cox_MA26_2 <- tidy(cox_fit_MA26_2, exponentiate = TRUE, conf.int = TRUE)
tidy_cox_MA23_1 <- tidy(cox_fit_MA23_1, exponentiate = TRUE, conf.int = TRUE)
tidy_cox_MA23_2 <- tidy(cox_fit_MA23_2, exponentiate = TRUE, conf.int = TRUE)

# Pull the HR, CIs and P value for the top category
q5_stats_MA26_1 <- tidy_cox_MA26_1 %>% filter(term == "qPRS2026_MA5")
q5_stats_MA26_2 <- tidy_cox_MA26_2 %>% filter(term == "ePRS2026_MA3")
q5_stats_MA23_1 <- tidy_cox_MA23_1 %>% filter(term == "qPRS2023_MA5")
q5_stats_MA23_2 <- tidy_cox_MA23_2 %>% filter(term == "ePRS2023_MA3")

# Get tidy trends
tidy_trend_MA26_1 <- tidy(trend_fit_MA26_1)
tidy_trend_MA26_2 <- tidy(trend_fit_MA26_2)
tidy_trend_MA23_1 <- tidy(trend_fit_MA23_1)
tidy_trend_MA23_2 <- tidy(trend_fit_MA23_2)

# Pull out the P value for trend
p_trend_MA26_1 <- tidy_trend_MA26_1 %>% filter(term == "as.numeric(qPRS2026_MA)") %>% pull(p.value) %>% format.pval(digits = 3)
p_trend_MA26_2 <- tidy_trend_MA26_2 %>% filter(term == "as.numeric(ePRS2026_MA)") %>% pull(p.value) %>% format.pval(digits = 3)
p_trend_MA23_1 <- tidy_trend_MA23_1 %>% filter(term == "as.numeric(qPRS2023_MA)") %>% pull(p.value) %>% format.pval(digits = 3)
p_trend_MA23_2 <- tidy_trend_MA23_2 %>% filter(term == "as.numeric(ePRS2023_MA)") %>% pull(p.value) %>% format.pval(digits = 3)

# Create plot labels
label_MA26_1 <- paste0(
  "Q5 vs Q1 HR: ", round(q5_stats_MA26_1$estimate, 2), " (95% CI: ", round(q5_stats_MA26_1$conf.low, 2), "-", round(q5_stats_MA26_1$conf.high, 2), ")\n",
  "Q5 vs Q1 p: ", format.pval(q5_stats_MA26_1$p.value, digits = 3), "\n",
  "P-trend across quintiles: ", p_trend_MA26_1)
label_MA26_2 <- paste0(
  "Top 10% vs bottom 10% HR: ", round(q5_stats_MA26_2$estimate, 2), " (95% CI: ", round(q5_stats_MA26_2$conf.low, 2), "-", round(q5_stats_MA26_2$conf.high, 2), ")\n",
  "Top 10% vs bottom 10% p: ", format.pval(q5_stats_MA26_2$p.value, digits = 3), "\n",
  "P-trend across categories: ", p_trend_MA26_2)
label_MA23_1 <- paste0(
  "Q5 vs Q1 HR: ", round(q5_stats_MA23_1$estimate, 2), " (95% CI: ", round(q5_stats_MA23_1$conf.low, 2), "-", round(q5_stats_MA23_1$conf.high, 2), ")\n",
  "Q5 vs Q1 p: ", format.pval(q5_stats_MA23_1$p.value, digits = 3), "\n",
  "P-trend across quintiles: ", p_trend_MA23_1)
label_MA23_2 <- paste0(
  "Top 10% vs bottom 10% HR: ", round(q5_stats_MA23_2$estimate, 2), " (95% CI: ", round(q5_stats_MA23_2$conf.low, 2), "-", round(q5_stats_MA23_2$conf.high, 2), ")\n",
  "Top 10% vs bottom 10% p: ", format.pval(q5_stats_MA23_2$p.value, digits = 3), "\n",
  "P-trend across categories: ", p_trend_MA23_2)

# Create Kaplan–Meier plots as ggplot objects
KMPlot_MA26_1 <- ggsurvplot(
  surv_fit_MA26_1, 
  data = df,
  xlim = c(38, 92),          # Starts the x-axis at 38 and ends at 90
  break.x.by = 10,           # Adds a vertical tick mark every 5 years
  axes.offset = FALSE,       # Optional: Forces the y-axis to meet the x-axis at 40
  conf.int = TRUE,           # Adds confidence intervals
  risk.table = TRUE,         # Shows the number of people at risk at each age
  pval = FALSE,              # Adds the Log-Rank test p-value
  legend.labs = c("Q1", "Q2", "Q3", "Q4", "Q5"), # Labels for your quintiles
  xlab = "Age (years)",
  ylab = "Disease-Free Probability",
  palette = "viridis",       # Also tried palette = "jco"
  ggtheme = theme_minimal()
) 

KMPlot_MA26_2 <- ggsurvplot(
  surv_fit_MA26_2, 
  data = df,
  xlim = c(38, 92),          # Starts the x-axis at 38 and ends at 92
  break.x.by = 10,           # Adds a vertical tick mark every 5 years
  axes.offset = FALSE,       # Optional: Forces the y-axis to meet the x-axis at 40
  conf.int = TRUE,           # Adds confidence intervals
  risk.table = TRUE,         # Shows the number of people at risk at each age
  pval = FALSE,              # Adds the Log-Rank test p-value
  legend.labs = c("Decile 1", "Deciles 2-9", "Decile 10"), # Labels for your quantiles
  xlab = "Age (years)",
  ylab = "Disease-Free Probability",
  palette = "viridis",       # Also tried palette = "jco"
  ggtheme = theme_minimal()
) 

KMPlot_MA23_1 <- ggsurvplot(
  surv_fit_MA23_1, 
  data = df,
  xlim = c(38, 92),          # Starts the x-axis at 38 and ends at 90
  break.x.by = 10,           # Adds a vertical tick mark every 5 years
  axes.offset = FALSE,       # Optional: Forces the y-axis to meet the x-axis at 40
  conf.int = TRUE,           # Adds confidence intervals
  risk.table = TRUE,         # Shows the number of people at risk at each age
  pval = FALSE,              # Adds the Log-Rank test p-value
  legend.labs = c("Q1", "Q2", "Q3", "Q4", "Q5"), # Labels for your quintiles
  xlab = "Age (years)",
  ylab = "Disease-Free Probability",
  palette = "viridis",       # Also tried palette = "jco"
  ggtheme = theme_minimal()
) 

KMPlot_MA23_2 <- ggsurvplot(
  surv_fit_MA23_2, 
  data = df,
  xlim = c(38, 92),          # Starts the x-axis at 38 and ends at 92
  break.x.by = 10,           # Adds a vertical tick mark every 5 years
  axes.offset = FALSE,       # Optional: Forces the y-axis to meet the x-axis at 40
  conf.int = TRUE,           # Adds confidence intervals
  risk.table = TRUE,         # Shows the number of people at risk at each age
  pval = FALSE,              # Adds the Log-Rank test p-value
  legend.labs = c("Decile 1", "Deciles 2-9", "Decile 10"), # Labels for your quantiles
  xlab = "Age (years)",
  ylab = "Disease-Free Probability",
  palette = "viridis",       # Also tried palette = "jco"
  ggtheme = theme_minimal()
) 


#  Add titles and annotate the plots including survival analysis statistics.  Note that ggsurvplot output is a list including a plot
KMPlot_MA26_1$plot <- KMPlot_MA26_1$plot + 
  labs(
    title = "Glaucoma-free survival by multi-ancestry (2026) PRS quintile",
    subtitle = paste0("EPIC-Norfolk Study n=",CohortN, " excluding prevalent disease at baseline.\n",Glaucoma_Definition)
  ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate( "text", x = 42, y = 0.2, label = label_MA26_1, hjust = 0, size = 4, fontface = "plain", lineheight = 1.1)

KMPlot_MA26_2$plot <- KMPlot_MA26_2$plot + 
  labs(
    title = "Glaucoma-free survival by multi-ancestry (2026) PRS category",
    subtitle = paste0("EPIC-Norfolk Study n=",CohortN, " excluding prevalent disease at baseline.\n",Glaucoma_Definition)
  ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate( "text", x = 42, y = 0.2, label = label_MA26_2, hjust = 0, size = 4, fontface = "plain", lineheight = 1.1)

KMPlot_MA23_1$plot <- KMPlot_MA23_1$plot + 
  labs(
    title = "Glaucoma-free survival by multi-ancestry (2023) PRS quintile",
    subtitle = paste0("EPIC-Norfolk Study n=",CohortN, " excluding prevalent disease at baseline.\n",Glaucoma_Definition)
  ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate( "text", x = 42, y = 0.2, label = label_MA23_1, hjust = 0, size = 4, fontface = "plain", lineheight = 1.1)

KMPlot_MA23_2$plot <- KMPlot_MA23_2$plot + 
  labs(
    title = "Glaucoma-free survival by multi-ancestry (2023) PRS category",
   subtitle = paste0("EPIC-Norfolk Study n=",CohortN, " excluding prevalent disease at baseline.\n",Glaucoma_Definition)
  ) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  annotate( "text", x = 42, y = 0.2, label = label_MA23_2, hjust = 0, size = 4, fontface = "plain", lineheight = 1.1)


# Print out the Kaplan–Meier plots 
cairo_pdf("../Output/KMPlit_MA2026_1a.pdf", width = 12, height = 8) # you can also use png()
print(KMPlot_MA26_1, newpage = FALSE)
dev.off()

cairo_pdf("../Output/KMPlit_MA2026_2a.pdf", width = 12, height = 8) # you can also use png()
print(KMPlot_MA26_2, newpage = FALSE)
dev.off()

cairo_pdf("../Output/KMPlit_MA2023_1a.pdf", width = 12, height = 8) # you can also use png()
print(KMPlot_MA23_1, newpage = FALSE)
dev.off()

cairo_pdf("../Output/KMPlit_MA2023_2a.pdf", width = 12, height = 8) # you can also use png()
print(KMPlot_MA23_2, newpage = FALSE)
dev.off()


## Check for proportional hazards

PHCheck_MA26_1 <- cox.zph(cox_fit_MA26_1)
zph_plot_MA26_1 <- ggcoxzph(PHCheck_MA26_1,font.main = c(16, "bold", "darkblue"), font.x = c(14, "bold"), font.y = c(14, "bold"), ggtheme = theme_minimal())
ggsave("../Output/Schoenfeld_Residuals_MA2026_1a.pdf", plot = zph_plot_MA26_1[[1]], device = cairo_pdf, width = 7, height = 5)


PHCheck_MA26_2 <- cox.zph(cox_fit_MA26_2)
zph_plot_MA26_2 <- ggcoxzph(PHCheck_MA26_2,font.main = c(16, "bold", "darkblue"), font.x = c(14, "bold"), font.y = c(14, "bold"), ggtheme = theme_minimal())
ggsave("../Output/Schoenfeld_Residuals_MA2026_2a.pdf", plot = zph_plot_MA26_2[[1]], device = cairo_pdf, width = 7, height = 5)

PHCheck_MA23_1 <- cox.zph(cox_fit_MA23_1)
zph_plot_MA23_1 <- ggcoxzph(PHCheck_MA23_1,font.main = c(16, "bold", "darkblue"), font.x = c(14, "bold"), font.y = c(14, "bold"), ggtheme = theme_minimal())
ggsave("../Output/Schoenfeld_Residuals_MA2023_1a.pdf", plot = zph_plot_MA23_1[[1]], device = cairo_pdf, width = 7, height = 5)

PHCheck_MA23_2 <- cox.zph(cox_fit_MA23_2)
zph_plot_MA23_2 <- ggcoxzph(PHCheck_MA23_2,font.main = c(16, "bold", "darkblue"), font.x = c(14, "bold"), font.y = c(14, "bold"), ggtheme = theme_minimal())
ggsave("../Output/Schoenfeld_Residuals_MA2023_2a.pdf", plot = zph_plot_MA23_2[[1]], device = cairo_pdf, width = 7, height = 5)


