
# load libraries ---------------------------------------------------------------
library(tidyverse)


# critval ----------------------------------------------------------------------
critval <- 1.96 ## approx 95% CI


# predictions: spoke_ambassador="No" -------------------------------------------
newdata <- data.frame(
  age = rep(
    seq(
      min(dat$age, na.rm = TRUE),
      max(dat$age, na.rm = TRUE),
      length.out = 100
    )
  ),
  spoke_ambassador = rep("No", 100)
)

preds <- 
  predict(
    mod, 
    newdata = newdata, 
    type = "link", 
    se.fit = TRUE
  )

# fit
fit_link <- preds$fit

fit_response <- mod$family$linkinv(fit_link)

# ci
upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod$family$linkinv(upr_link)

lwr_response <- mod$family$linkinv(lwr_link)

fit_no <- as.data.frame(
  cbind(
    newdata,
    fit_response, 
    upr_response,
    lwr_response
  )
)


# predictions: spoke_ambassador="Yes" ------------------------------------------
newdata <- data.frame(
  age = rep(
    seq(
      min(dat$age, na.rm = TRUE),
      max(dat$age, na.rm = TRUE),
      length.out = 100
    )
  ),
  spoke_ambassador = rep("Yes", 100)
)

preds <- 
  predict(
    mod, 
    newdata = newdata, 
    type = "link", 
    se.fit = TRUE
  )

# fit
fit_link <- preds$fit

fit_response <- mod$family$linkinv(fit_link)

# ci
upr_link <- preds$fit + (critval * preds$se.fit)

lwr_link <- preds$fit - (critval * preds$se.fit)

upr_response <- mod$family$linkinv(upr_link)

lwr_response <- mod$family$linkinv(lwr_link)

fit_yes <- as.data.frame(
  cbind(
    newdata,
    fit_response, 
    upr_response,
    lwr_response
  )
)


# rbind predictions ------------------------------------------------------------
fit <- rbind(fit_no, fit_yes)
fit <- fit %>% mutate(spoke_ambassador = factor(spoke_ambassador, levels = c("Yes", "No")))


# plot -------------------------------------------------------------------------
fit %>%
  
  ggplot(
    aes(
      x = age, 
      y = fit_response,
      ymin = lwr_response,
      ymax = upr_response,
      linetype = spoke_ambassador)
  ) +
  
  scale_y_continuous(
    labels = scales::percent
  ) +
  
  geom_line() +
  
  geom_ribbon(
    alpha = 0.1
  ) +
  
  ggtitle("") +
  
  scale_x_continuous(
    limits = c(20, 70),
    breaks = seq(20, 70, 10)
  ) +
  
  xlab("Age") +
  
  ylab("Probability of Reduced \nVaccine Hesitancy") +
  
  labs(linetype = "Spoke to \nVaccine \nAmbassador?") +
  
  theme_bw() +
  
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
    )


# save -------------------------------------------------------------------------
ggsave(
  "out/figure.png", 
  width = 5.5, 
  height = 3
  )
