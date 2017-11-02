
library(ggplot2)
library(ResourceSelection)

d0 = data.frame(dose=rep(0, 391), death=c(rep(1, 13), rep(0, 378)))
d1 = data.frame(dose=rep(1, 205), death=c(rep(1, 5), rep(0, 200)))
d10 = data.frame(dose=rep(10, 156), death=c(rep(1, 5), rep(0, 151)))
d50 = data.frame(dose=rep(50, 50), death=c(rep(1,3), rep(0, 47)))
d100 = data.frame(dose=rep(100, 35), death=c(rep(1,4), rep(0, 31)))
d200 = data.frame(dose=rep(200, 51), death=c(rep(1,18), rep(0, 33)))
data = rbind(d0, d1, d10, d50, d100, d200)

dim(data)
table(data)

ggplot(data=data, aes(factor(dose), death, group=dose, size=4)) +
    stat_summary(fun.y="mean", geom="point", color="blue") +
    labs(x="Radiation Dose", y="Mean Deaths Leukaemia", title="Mean Deaths from Leukaemia Vs Radiation Dose", 
         color="Radiation\nDose", subtitle="From set 13 of Cox and Snell, 1981, using data from Otake 1979.") +
    theme(legend.position = "none")

glm_logit = glm(death ~ dose, data=data, family=binomial(link=logit))
summary(glm_logit)

anova(glm_logit, test="Chisq")

hl_test = hoslem.test(data$death, fitted(glm_logit), g=4)
tab = data.frame(Interval = rownames(hl_test$observed), 
           Observed_NoDeath = hl_test$observed[,1], 
           Expected_NoDeath = hl_test$expected[,1],
           Observed_Death = hl_test$observed[,2],
           Expected_Death = hl_test$expected[,2])
tab
hl_test

sesL = data.frame(ses = rep("L", 430+321+230+169),
                 iq = c(rep("L", 430), rep("LM", 321), rep("UM", 230), rep("H", 169)),
                 college_plans = c(rep(1, 17), rep(0, 413), rep(1, 42), rep(0,279), rep(1,50), rep(0, 180), rep(1,59),rep(0,110)))

sesLM = data.frame(ses = rep("LM", 345+367+312+274),
                 iq = c(rep("L", 345), rep("LM", 367), rep("UM", 312), rep("H", 274)),
                 college_plans = c(rep(1, 29), rep(0, 316), rep(1, 71), rep(0,296), rep(1,105), rep(0, 207), rep(1,136),rep(0,138)))

sesUM = data.frame(ses = rep("UM", 312+310+357+319),
                 iq = c(rep("L", 312), rep("LM", 310), rep("UM", 357), rep("H", 319)),
                 college_plans = c(rep(1, 55), rep(0, 257), rep(1, 80), rep(0,230), rep(1,165), rep(0, 192), rep(1,204),rep(0,115)))

sesH = data.frame(ses = rep("H", 148+265+339+493),
                 iq = c(rep("L", 148), rep("LM", 265), rep("UM", 339), rep("H", 493)),
                 college_plans = c(rep(1, 43), rep(0, 105), rep(1, 128), rep(0,137), rep(1,233), rep(0, 106), rep(1,422),rep(0,71)))

data = rbind(sesL, sesLM, sesUM, sesH)

ggplot(data, aes(iq)) +
    geom_density(aes(colour=iq, fill=iq), alpha=0.3) +    
    #geom_bar(stat='density') +
    stat_summary(data=data, aes(iq, college_plans), fun.y = "mean", geom="point", size=2, colour="blue") +
    facet_wrap(~ses) +
    labs(x="IQ", y="Density", title="Density of Each IQ for boys within Each Socio Economic Group",
        subtitle="Blue points: Proportion of boys that have college plans within each Socio Economic Group") 

college_glm = glm(college_plans ~ ses + iq, data=data, family=binomial(link=logit))

summary(college_glm)

anova(college_glm, test="Chisq")

hl_test = hoslem.test(data$college_plans, fitted(college_glm), g=10)
tab = data.frame(Interval = rownames(hl_test$observed), 
           Observed_NoCollege = hl_test$observed[,1], 
           Expected_NoCollege = hl_test$expected[,1],
           Observed_College = hl_test$observed[,2],
           Expected_College = hl_test$expected[,2])
tab
hl_test

control = data.frame(Trt = c("C","C","C"), Centrifuge = c(40,150,350), Anthers =c(55,52,57), Total = c(102,99,108))
treatment = data.frame(Trt = c("T", "T", "T"), Centrifuge = c(40,150,350), Anthers =c(55,50,50), Total = c(76,81,90))
data = rbind(control, treatment)
data$proportion = data$Anthers / data$Total
data

ggplot(data=data, aes(log(Centrifuge), proportion)) +
    geom_point(aes(group=Trt)) +
    geom_text(aes(label=Trt, colour=Trt), vjust=-.6) +
    labs(x="Log (Centrifuging Force)", y="Proportion Germinated", 
         title = "Proportion Germinated Anthers Vs Log(Centrifuging Force)") +
    theme(legend.position = "none")

msreg = lm(proportion ~ Trt + log(Centrifuge), data=data)
summary(msreg)

coef = data.frame(R_lm = round(coefficients(msreg),4), Book_Estimates = c(0.877,0.0407,-0.155))
coef

fit = data.frame(Observed = round(data$proportion, 3),
                 R_lm_fitted = round(fitted(msreg), 3), 
                 Book_Fitted_Model = c(0.576,0.526,0.493,0.671,0.625,0.593))
fit

x_sq_mod = sum((data$proportion - fitted(msreg))^2/((data$proportion * (1 - data$proportion))/data$Total))
data.frame(Modified_x_sq = round(x_sq_mod,3), Book_D = 2.619, d.f = 3)
