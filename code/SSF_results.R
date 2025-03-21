library(tidyverse)
library(amt)
library(terra)

m1 <- readRDS("outputs/script_3/iSSF_field_edges.rds")
summ <- summary(m1)

# Extract coefficient estimates
coefs <- as.data.frame(summ$coefficients)
colnames(coefs) <- c("coef", "exp_coef", "se_coef", "z", "p")

# Extract confidence intervals
conf_int <- as.data.frame(summ$conf.int)
colnames(conf_int) <- c("exp_coef", "exp_neg_coef", "lower_95", "upper_95")

# Merge the two, ensuring the order of rows matches
coefs$variable <- rownames(coefs)
conf_int$variable <- rownames(conf_int)

# Merge the coefficient table with confidence intervals
issf_results <- merge(coefs, conf_int[, c("variable", "lower_95", "upper_95")], by = "variable")

# View the results
print(issf_results)

df <- issf_results %>%
        filter(variable != "hab8") %>%
        mutate(
            variable = case_when(
                variable == "cos_ta_" ~ "cos(Turning angle)", 
                variable == "feed" ~ "Distance to feeders", 
                variable == "field_edges" ~ "Distance to field edges", 
                variable == "hab10" ~ "Built-up areas and gardens", 
                variable == "hab11" ~ "Cover crop", 
                variable == "hab2" ~ "Coniferous woodland", 
                variable == "hab3" ~ "Arable and horticulture", 
                variable == "hab33" ~ "Woodland within 500m of release pen", 
                variable == "hab4" ~ "Improved grassland", 
                variable == "hab5" ~ "Semi-natural grassland", 
                variable == "hab6" ~ "Mountain, heath and bog", 
                variable == "hedges" ~ "Distance to hedgerows", 
                variable == "log_sl_" ~ "log(Step length)", 
                variable == "pen" ~ "Distance to release pen", 
                variable == "pen:SinceRel" ~ "Distance to release pen : Time since release", 
                variable == "sl_" ~ "Step length", 
                variable == "wood" ~ "Distance to woodland"
            )
        ) %>%
        rename(
            Variable = variable,
            Coefficient = coef, 
            `exp(Coefficient)` = exp_coef, 
            `Standard error` = se_coef, 
            `z value` = z, 
            `p value` = p, 
            `2.5% Confidence Interval` = lower_95, 
            `97.5% Confidence Interval` = upper_95
        ) %>%
        arrange(Variable)

knitr::kable(df, row.names = FALSE) %>%
            kableExtra::kable_classic(full_width = F, html_font = "Cambria") %>%
            cat(., file = "../../report_writeup/ssf_results.html")


