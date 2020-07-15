############# Model partner und Gesamtbezugsdauer ########


## 1.1 model
lm1_gesamt <- lm(
  gesamtdauerneu ~ F1_1 + S01  + S07   +
    S08  + f2_5kor +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    S11  + S09,
  data = KBG_work_2020.konto
)
summary(lm1_gesamt)
plot(lm1_gesamt)

rlm1_gesamt <- coeftest(lm1_gesamt, vcov = vcovHC(lm1_gesamt, type = "HC3")) # robust version of stata

lm2_gesamt <- lm(
  gesamtdauerneu ~ F1_1 + S01 + S02 + S07 + S05  +
    S08 + F1_4 + f2_5kor +
    FAC1_poly_highcor + FAC2_poly_highcor + FAC3_poly_highcor + FAC4_poly_highcor +
    S11 + F3_4_5 + F3_4_2 + S09,
  data = KBG_work_2020.konto
)
summary(lm2_gesamt)
plot(lm2_gesamt)


# sobald man auf F3_4_2 kontrolliert werden Faktoren insign
+ S04 + F1_4 + f2_5kor 