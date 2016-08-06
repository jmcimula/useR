library(survminer)

require("survival")

#Fitting survival curves

# Loading required package: survival
fit <- survfit(Surv(time, status) ~ sex, data = lung)

#Basic plot
ggsurvplot(fit)

#Customized survival curves
ggsurvplot(fit,  size = 1,  # change line size
           palette = c("#E7B800", "#2E9FDF"), # custom color palettes
           conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Risk table color by groups
           legend.labs = c("Male", "Female"), # Change legend labels
           risk.table.height = 0.25, # Useful to change when you have multiple groups
           ggtheme = theme_bw() # Change ggplot2 theme
           )
		   

		   