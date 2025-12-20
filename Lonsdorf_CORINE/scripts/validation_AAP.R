#validate model accuracy

#read data-----

dat <- read.csv(file = "Results/Junta_validation_data_outputs.csv")
dat$Abundancia_x_min <- (dat$Abundancia_abejas/dat$Esfuerzo_min/dat$Esfuerzo_m2)*100
head(dat)

#dat <- subset(dat, Abundancia_abejas > 0) #Removing zeroes improve a bit things. Think about it.

#plot

plot(dat$BayRidge ~ dat$Abundancia_x_min)
plot(dat$SVR ~ dat$Abundancia_x_min)
plot(dat$GB ~ dat$Abundancia_x_min)
plot(dat$Lonsdorf_big ~ dat$Abundancia_x_min)
plot(dat$Lonsdorf_small ~ dat$Abundancia_x_min)

#calculate RMSE

sqrt(sum((scale(dat$BayRidge) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) # 18
sqrt(sum((scale(dat$SVR) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) # 19
sqrt(sum((scale(dat$GB) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) # 19
sqrt(sum((scale(dat$Lonsdorf_big) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) # 16
sqrt(sum((scale(dat$Lonsdorf_small) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) # 17

#combine londsdorfs

plot(dat$Lonsdorf_big, dat$Lonsdorf_small)
dat$Lonsdorf <- (scale(dat$Lonsdorf_big) + scale(dat$Lonsdorf_small)) / 2
sqrt(sum((scale(dat$Lonsdorf) - scale(dat$Abundancia_x_min))^2)/nrow(dat)) #16.99

cor.test(dat$BayRidge, dat$Abundancia_x_min, method = "spearman")
cor.test(dat$SVR, dat$Abundancia_x_min, method = "spearman")
cor.test(dat$GB, dat$Abundancia_x_min, method = "spearman")
cor.test(dat$Lonsdorf, dat$Abundancia_x_min, method = "spearman") #0.16

scatter.smooth(dat$Lonsdorf ~ dat$Abundancia_x_min)

#which sites make more errors

dat$error <- dat$Lonsdorf - scale(dat$Abundancia_x_min)

head(dat)
dat[order(dat$error),c("Sitio", "Habitat", "error")]

dat$error2 <- abs(scale(dat$Lonsdorf) - scale(dat$Abundancia_x_min))

head(dat)
dat[order(dat$error2),c("Sitio", "Habitat", "error2")]

#Most error is in Trillo dataset
dat2 <- subset(dat, Habitat != "Matorral")
head(dat2)
scatter.smooth(dat2$Lonsdorf ~ dat2$Abundancia_x_min)
cor.test(dat2$BayRidge, dat2$Abundancia_x_min, method = "spearman") #-0.11
cor.test(dat2$Lonsdorf_small, dat2$Abundancia_x_min, method = "spearman") #0.38
sqrt(sum((scale(dat2$BayRidge) - scale(dat2$Abundancia_x_min))^2)/nrow(dat)) #12
sqrt(sum((scale(dat2$Lonsdorf_small) - scale(dat2$Abundancia_x_min))^2)/nrow(dat)) #11
nrow(dat2) #84 puntos de validación.

#study influence

boxplot(dat$Abundancia_x_min ~ dat$Habitat)
boxplot(dat$error ~ dat$Habitat)
abline(h = 0)

# remove high errors?? No...

dat[order(dat$error),c("Sitio", "Habitat", "error")]
#error > 2... pred > obs:  mostly Trillo daraset
#error > -2... obs > pred: mostly Naranjos, Olivar, Melocoton...


