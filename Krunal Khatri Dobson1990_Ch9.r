
library(MASS)
library(ggplot2)
library(scales)

vaccine = data.frame(trt = c(rep("Placebo", 38), rep("Vaccine", 35)),
                     response = c(rep("Small", 25), rep("Moderate", 8), rep("Large", 5),
                                  rep("Small", 6), rep("Moderate", 18), rep("Large", 11)))

vacTab = table(vaccine)
addmargins(table(vaccine))

con_test = chisq.test(vacTab)

cat("            Expected               Observed")
cbind(con_test$expected, con_test$observed)

con_test
data.frame(residuals(con_test, "deviance"))

log_lin = loglin(vacTab, margin=c(c(1,2)), print=T, param=T, fit=T)

cat("Log-Likelihood Ratio Statistic: D = ",log_lin$lrt)
cat("\nChi-Squared Statistic: X^2 = ", log_lin$pearson)
cat("\nBoth the above Statistics with degrees of freedom = ",log_lin$df)

cat("           Expected                 Observed")
cbind(log_lin$fit, table(vaccine))

cat( "         ____|Standarized Residuals|____")
ftable(vacTab - log_lin$fit)/sqrt(log_lin$fit)

log_lin$param

ContactOtherResidents = c(rep("Low",65+54+100+130+76+111+67+48+62),rep("High",34+47+100+141+116+191+130+105+104))

Satisfaction=c(rep("Low",65+130+67),rep("Meduim",54+76+48),rep("High",100+111+62),
                                  rep("Low",34+141+130),rep("Meduim",47+116+105),rep("High",100+191+104))

HousingType = c(rep("Tower_Block",65),rep("Apartments",130),rep("Houses",67),
               rep("Tower_Block",54),rep("Apartments",76),rep("Houses",48),
               rep("Tower_Block",100),rep("Apartments",111),rep("Houses",62),
               rep("Tower_Block",34),rep("Apartments",141),rep("Houses",130),
               rep("Tower_Block",47),rep("Apartments",116),rep("Houses",105),
               rep("Tower_Block",100),rep("Apartments",191),rep("Houses",104))

housing = data.frame(HousingType, Satisfaction, ContactOtherResidents)
dim(housing)

table(housing$HousingType)

cat("Percentage of places where people lived and their contact with other residents")
round(prop.table(table(housing$ContactOtherResidents, housing$HousingType), 2) * 100, 2)

a = round(addmargins(prop.table(table(housing$Satisfaction, housing$HousingType),2)) * 100, 2)
b = round(addmargins(prop.table(table(housing$Satisfaction, housing$HousingType),1)) * 100, 2)
cat("Rows = Satisfaction Level\nColumns = Housing Type\n\n        Column Totals                       Row Totals")
cbind(a,b)
ggplot(housing, aes(Satisfaction)) +
    geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=HousingType)) +
    labs(y="Percent", title="People with Satisfaction ratings by Housing type",
        subtitle = "Each housing type consists of 100% people for that housing type. (Column Totals)") +
    scale_y_continuous(labels=percent) +    
    facet_wrap(~HousingType) +
    theme(legend.position = "none")

a = round(addmargins(prop.table(table(housing$ContactOtherResidents, housing$HousingType), 2)) * 100, 2)
b = round(addmargins(prop.table(table(housing$ContactOtherResidents, housing$HousingType), 1)) * 100, 2)
cat("Rows = Contact with Other Residents\nColumns = Housing Types\n\n        Column Totals                    Row Totals")
cbind(a,b)
ggplot(housing, aes(ContactOtherResidents)) +
    geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=HousingType)) +
    labs(y="Percent", x="Contact With Other Residents", title="% People Contacting Other Residents for each Housing Type",
        subtitle = "Each housing type consists of 100% people for that housing type. (Column Totals)") +
    scale_y_continuous(labels = percent) +
    facet_wrap(~HousingType) +
    theme(legend.position ="none")

ggplot(housing, aes(ContactOtherResidents)) +
    geom_bar(aes(y=..count../sum(..count..)),fill="skyblue") +
    scale_y_continuous(label=percent) +
    labs(x = "Contact with Other Residents", y = "Percent of Total", 
         title="% of People who were in contact with Other Residents (% of total)",
         subtitle="                                                                Satisfaction") +
    facet_grid(HousingType~Satisfaction)

tabData = table(housing)
# 1 = Housing
# 2 = Satisfaction
# 3 = Contact

fullModel = loglm(~ 1 + 2 + 3 + 1:2 + 1:3 + 2:3, data=tabData)
Hinteraction = loglm(~ 1 + 2 + 3 + 1:2 + 1:3, data=tabData)
HSInteraction = loglm(~ 1 + 2 + 3 + 1:2, data=tabData)
simpleModel = loglm(~ 1 + 2 + 3, data=tabData)

res = data.frame(d.f = c(fullModel$df, Hinteraction$df, HSInteraction$df, simpleModel$df),
          Pearson_Statistic = c(fullModel$pearson, Hinteraction$pearson, HSInteraction$pearson, simpleModel$pearson),
          Likelihood_Ratio_Statistic = c(fullModel$lrt, Hinteraction$lrt, HSInteraction$lrt, simpleModel$lrt))
rownames(res) = c("2_Interactions", "Housing_Interaction", "Housing_*_Satisfaction", "Simple_Model")
res

anova(fullModel, Hinteraction, HSInteraction, simpleModel)

fullModel$param[c(1,2,3,4)]

a = data.frame(residuals(fullModel, type="deviance"))
a = cbind(a, data.frame(residuals(fullModel, "response"))$Freq)
a = cbind(a, data.frame(tabData)$Freq)
colnames(a) = c("HousingType","Satisfaction","ContactOtherResidents", "Std.Residuals","Raw.Residuals","Observed")
a$Expected = round(a$Observed - a$Raw.Residuals,2)
a
