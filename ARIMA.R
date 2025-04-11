## Line 3-55 for Figure 3

#model 1
canadaM1 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"Mfrequency"],trace = F)
israelM1 <- auto.arima(israel_train1[,"case_growth_rate"], xreg = israela_train1[,"Mfrequency"],trace = F)
italyM1 <- auto.arima(italy_train1[,"case_growth_rate"], xreg = italy_train1[,"Mfrequency"],trace = F)
franceM1 <- auto.arima(france_train1[,"case_growth_rate"], xreg = france_train1[,"Mfrequency"],trace = F)

M1_plot <- plot_models(
  canadaM1, israelM1, italyM1, franceM1,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Mf",
  title = "Mf-14 & Case Growth Rate",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-2.3, .8)

#model 2
canadaM2 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"Gfrequency"],trace = F)
israelM2 <- auto.arima(israel_train1[,"case_growth_rate"], xreg = israela_train1[,"Gfrequency"],trace = F)
italyM2 <- auto.arima(italy_train1[,"case_growth_rate"], xreg = italy_train1[,"Gfrequency"],trace = F)
franceM2 <- auto.arima(france_train1[,"case_growth_rate"], xreg = france_train1[,"Gfrequency"],trace = F)

M2_plot <- plot_models(
  canadaM2, israelM2, italyM2, franceM2,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Gf",
  title = "Gf-14 & Case Growth Rate",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-2.3, .8)

#model 3
canadaM3 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"booster"],trace = F)
israelM3 <- auto.arima(israel_train1[,"case_growth_rate"], xreg = israela_train1[,"booster"],trace = F)
italyM3 <- auto.arima(italy_train1[,"case_growth_rate"], xreg = italy_train1[,"booster"],trace = F)
franceM3 <- auto.arima(france_train1[,"case_growth_rate"], xreg = france_train1[,"booster"],trace = F)

M3_plot <- plot_models(
  canadaM3, israelM3, italyM3, franceM3,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Booster",
  title = "booster-14 & Case Growth Rate",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-2.3, .8)


## Line 60-112 for Figure 4

#model 3(y:case_growth_rate)
canadaM3 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"vaccination"],trace = F)
israelM3 <- auto.arima(israel_train1[,"case_growth_rate"], xreg = israela_train1[,"vaccination"],trace = F)
italyM3 <- auto.arima(italy_train1[,"case_growth_rate"], xreg = italy_train1[,"vaccination"],trace = F)
franceM3 <- auto.arima(france_train1[,"case_growth_rate"], xreg = france_train1[,"vaccination"],trace = F)

M3_casegrowthrate_plot <- plot_models(
  canadaM3, israelM3, italyM3, franceM3,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Vac",
  title = "Vaccination-14 & Case Growth Rate",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-10, 10)

#model 3(y:death_case_per_m)
canadaM3 <- auto.arima(canada_train1[,"death_case_per_m"], xreg = canada_train1[,"vaccination"],trace = F)
israelM3 <- auto.arima(israel_train1[,"death_case_per_m"], xreg = israela_train1[,"vaccination"],trace = F)
italyM3 <- auto.arima(italy_train1[,"death_case_per_m"], xreg = italy_train1[,"vaccination"],trace = F)
franceM3 <- auto.arima(france_train1[,"death_case_per_m"], xreg = france_train1[,"vaccination"],trace = F)

M3_deathcaseperm_plot <- plot_models(
  canadaM3, israelM3, italyM3, franceM3,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Vac",
  title = "Vaccination-28 & death_per_m",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-140, 50)

#model 3(y:ICU_case_per_m)
canadaM3 <- auto.arima(canada_train1[,"ICU_case_per_m"], xreg = canada_train1[,"vaccination"],trace = F)
israelM3 <- auto.arima(israel_train1[,"ICU_case_per_m"], xreg = israela_train1[,"vaccination"],trace = F)
italyM3 <- auto.arima(italy_train1[,"ICU_case_per_m"], xreg = italy_train1[,"vaccination"],trace = F)
franceM3 <- auto.arima(france_train1[,"ICU_case_per_m"], xreg = france_train1[,"vaccination"],trace = F)

M3_ICUcaseperm_plot <- plot_models(
  canadaM3, israelM3, italyM3, franceM3,
  rm.terms =c("ar1", "ar2","ar3","ar4","ar5", "ma1","ma2","ma3","ma4","ma5","intercept"),
  m.labels = c("canada" ,"israel","italy","france"),
  spacing = 1.1,
  colors = c("#377EB8", "#4DAF4A","#984EA3","#E41A1C" ),
  axis.labels = "Vac",
  title = "Vaccination-14 & icu_per_m",
  value.size = 5,
  show.values = T, show.p = T, p.shape = F, digits = 3
) + ylim(-1200, 1200)


## Line 117-129 for Table 1 (Take Canada as an example in the Alpha & Delta Period)

canadaM1 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"Mfrequency"],trace = F)
canadaM2 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"Gfrequency"],trace = F)
canadaM3 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,"vaccination"],trace = F)
canadaM4 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,c("Mfrequency", "vaccination")],trace = F)
canadaM5 <- auto.arima(canada_train1[,"case_growth_rate"], xreg = canada_train1[,c("Gfrequency", "vaccination")],trace = F)
canadaM6 <- auto.arima(canada_train1[,"case_growth_rate"], trace = F)

summary(canadaM1)
summary(canadaM2)
summary(canadaM3)
summary(canadaM4)
summary(canadaM5)
summary(canadaM6)
