library(dplyr)
library(ggplot2)
library(rstan)
library(stargazer)
options(mc.cores = parallel::detectCores())

## Load the data

data_polarization <- read.csv("DL-Daten V1.0.csv", sep = ";")

## Check colums and interesting variables
colnames(data_polarization)

table(data_polarization$F05, data_polarization$R03)


## Make first plot
data_plot <- data_polarization[data_polarization$F05>0 & data_polarization$F05<20 & data_polarization$R03>0 & data_polarization$R03<20, ]

data_plot$count <- 1

data_plot <- data_plot[is.na(data_plot$F05)==F & is.na(data_plot$R03)==F,]



data_plot1 <- data_plot %>% 
  group_by(F05) %>% 
  summarize(supp_w1 = sum(count)/3576)

data_plot2 <- data_plot %>% 
  group_by(R03) %>% 
  summarize(supp_w2 = sum(count)/3576)

data_plot$changes <- paste0(data_plot$F05, "_", data_plot$R03)

data_plot <- data_plot %>% 
  group_by(F05, R03, changes) %>% 
  summarize(avgChange = sum(count)/3576)


### First plot

png("Vote changes 1st plot.png", 1500, 2500, res = 300)
ggplot()+
  geom_point(aes(x=0, y=data_plot1$F05, size = data_plot1$supp_w1, alpha = 0.4))+
  geom_point(aes(x=1, y=data_plot2$R03, size = data_plot2$supp_w2, alpha = 0.4))+
  geom_segment(aes(x = 0, xend = 1, y = data_plot$F05, yend = data_plot$R03, size = data_plot$avgChange), alpha = 0.3)+
  scale_y_continuous(breaks = c(1:10), labels = c("SVP", "SP", "FDP", "GPS", "CVP", "GLP", "EVP", "BDP", "EDU", "AL"))+
  scale_x_continuous(breaks = c(0,1), labels = c("1st wave", "2nd wave"))+
  xlab("Survey wave")+
  ylab("Vote intention for parties")+
  theme_minimal()+
  theme(legend.position = "none")
dev.off()
####### Now let's try to place these parties on a latent dimension #####

## Run IRT on rstan to do so ##

swissvotes <- read.csv(url("https://swissvotes.ch/page/dataset/swissvotes_dataset.csv"), sep = ";")

colnames(swissvotes)

### Select right columns 

swissvotes <- swissvotes[swissvotes$rechtsform!=4,]

swissvotes <- swissvotes[,c(1, 2, 3, # ANR, date, short german title
                            47:49, # Recommendations FDP, SP, SVP
                            52, 53, 62, # GPS, GLP, CVP
                            51, 63, 57)] # EVP, BDP, EDU


### Keep date between the 2015 and 2019 Elections

## Change date format in Swissvotes

swissvotes$datum <- as.Date(swissvotes$datum, "%d.%m.%Y")

swissvotes <- swissvotes[swissvotes$datum>as.Date("2016-01-01") & swissvotes$datum<as.Date("2020-01-01"),]

Party <- c("FDP", "SP", "SVP", "GPS", "GLP", "CVP", "EVP", "BDP", "EDU")

## Reshape the data
for (i in 1:9) {
  data <- swissvotes[,c(1:3, 3+i)]
  
  col <- c("anr", "date", "titel", "partyPos")
  
  colnames(data) <- col
  
  data$Party <- Party[i]
  
  if(i == 1) {
    swissvotes_lon <- data
  } else {
    swissvotes_lon <- rbind(swissvotes_lon, data)
  }
}


## Define values for IRT
swissvotes_lon <- swissvotes_lon %>% 
  mutate(y = ifelse(partyPos==1, 1,
                    ifelse(partyPos==2, 0, NA)),
         j = as.numeric(as.factor(Party)),
         i = as.numeric(as.factor(anr)))

y <- swissvotes_lon$y
NAS <- which(is.na(y))
y <- y[-NAS]
N <- length(y)
i <- swissvotes_lon[-NAS,]$i
j <- swissvotes_lon[-NAS,]$j
I <- max(i)
J <- max(j)

## Run the model
irt.data <- list(N=N, I=I, J=J,
                 y=y, i=i, j=j)


model <- stan(file = "IRT Party.stan", 
              data = irt.data, iter = 2000, warmup = 1000, chains = 4, 
              thin = 1, verbose = T, cores = 4, seed = 1234, init = 0,
              control = list(adapt_delta = 0.99,
                             max_treedepth = 20))


## Make plots of parties positions
thetas <- summary(model, pars="theta")
thetas <- as.data.frame(thetas)

table(swissvotes_lon$Party, swissvotes_lon$j)

thetas$party <- c("BDP", "CVP", "EDU", "EVP", "FDP", "GLP", "GPS", "SP", "SVP")

thetas$order <- NA
orders <- order(thetas$c_summary.50..chain.1)

for (i in 1:9) {
  thetas[i,]$order <- which(i == orders)
}

# Second plot
png("VPos party.png", 1500, 1500, res = 300)
ggplot(thetas)+
  geom_point(aes(x=c_summary.50..chain.1, y=order))+
  geom_segment(aes(x = c_summary.2.5..chain.1, xend = c_summary.97.5..chain.1, y = order, yend = order))+
  geom_text(aes(x = c_summary.50..chain.1, y = order+0.2, label = party), size = 2)+
  theme_minimal()+
  xlab(expression(theta[j]))+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank())
dev.off()

### See whether the change in party means polarizazion.

theta1 <- thetas[,c(15, 39)]
colnames(theta1) <- c("theta1", "party1")
theta2 <- thetas[,c(15, 39)]
colnames(theta2) <- c("theta2", "party2")


data_plot <- data_plot %>% 
  mutate(theta1 = ifelse(F05 == 1, 4.47,
                         ifelse(F05==2, -4.62,
                                ifelse(F05 == 3, 0.78,
                                       ifelse(F05==4,  -13.39,
                                              ifelse(F05==5, 0.39,
                                                     ifelse(F05 == 6, -0.17,
                                                            ifelse(F05==7, -0.47, 
                                                                   ifelse(F05==8, 0.31,
                                                                          ifelse(F05==9, 3.42, NA))))))))),
         theta2 = ifelse(R03 == 1, 4.47,
                         ifelse(R03==2, -4.62,
                                ifelse(R03 == 3, 0.78,
                                       ifelse(R03==4,  -13.39,
                                              ifelse(R03==5, 0.39,
                                                     ifelse(R03 == 6, -0.17,
                                                            ifelse(R03==7, -0.47, 
                                                                   ifelse(R03==8, 0.31,
                                                                          ifelse(R03==9, 3.42, NA))))))))),
         abs1 = abs(theta1),
         abs2 = abs(theta2))


data_plot$diff_vote <- data_plot$theta1 - data_plot$theta2
data_plot$diff_abs <- data_plot$abs1 - data_plot$abs2

data_plot <- data_plot %>% 
  mutate(direction = ifelse(diff_vote>0, "More progressive",
                            ifelse(diff_vote<0, "More conservative",
                                   ifelse(diff_vote==0, "No changes", NA))),
         polarization = ifelse(diff_abs<0, "More polarized",
                               ifelse(diff_abs>0, "More centralised",
                                      ifelse(diff_abs==0, "No change", NA))))


png("Vote changes 2nd plot.png", 1500, 2500, res = 300)
ggplot()+
  geom_point(aes(x=0, y=data_plot1[data_plot1$F05!=10,]$F05, size = data_plot1[data_plot1$F05!=10,]$supp_w1, alpha = 0.4))+
  geom_point(aes(x=1, y=data_plot2[data_plot2$R03!=10,]$R03, size = data_plot2[data_plot2$R03!=10,]$supp_w2, alpha = 0.4))+
  geom_segment(aes(x = 0, xend = 1, y = data_plot[is.na(data_plot$diff_vote)==F,]$F05, yend = data_plot[is.na(data_plot$diff_vote)==F,]$R03,
                   size = data_plot[is.na(data_plot$diff_vote)==F,]$avgChange, color = data_plot[is.na(data_plot$diff_vote)==F,]$direction), alpha = 0.3)+
  scale_y_continuous(breaks = c(1:10), labels = c("SVP", "SP", "FDP", "GPS", "CVP", "GLP", "EVP", "BDP", "EDU", "AL"))+
  scale_x_continuous(breaks = c(0,1), labels = c("1st wave", "2nd wave"))+
  xlab("Survey wave")+
  scale_color_manual(values = c("blue", "red", "black"))+
  ylab("Vote intention for parties")+
  theme_minimal()+
  theme(legend.position = "none")
dev.off()

png("Vote changes 3rd plot.png", 1500, 2500, res = 300)
ggplot()+
  geom_point(aes(x=0, y=data_plot1[data_plot1$F05!=10,]$F05, size = data_plot1[data_plot1$F05!=10,]$supp_w1, alpha = 0.4))+
  geom_point(aes(x=1, y=data_plot2[data_plot2$R03!=10,]$R03, size = data_plot2[data_plot2$R03!=10,]$supp_w2, alpha = 0.4))+
  geom_segment(aes(x = 0, xend = 1, y = data_plot[is.na(data_plot$diff_vote)==F,]$F05, yend = data_plot[is.na(data_plot$diff_vote)==F,]$R03,
                   size = data_plot[is.na(data_plot$diff_vote)==F,]$avgChange, color = data_plot[is.na(data_plot$diff_vote)==F,]$polarization), alpha = 0.3)+
  scale_y_continuous(breaks = c(1:10), labels = c("SVP", "SP", "FDP", "GPS", "CVP", "GLP", "EVP", "BDP", "EDU", "AL"))+
  scale_x_continuous(breaks = c(0,1), labels = c("1st wave", "2nd wave"))+
  xlab("Survey wave")+
  ylab("Vote intention for parties")+
  scale_color_manual(values = c("green", "red", "black"))+
  theme_minimal()+
  theme(legend.position = "none")
dev.off()



#### Try models 

data_polarization <- data_polarization[data_polarization$F05>0 & data_polarization$F05<20 & data_polarization$R03>0 & data_polarization$R03<20, ]

data_polarization <- data_polarization[is.na(data_polarization$F05)==F & is.na(data_polarization$R03)==F,]


data_polarization$changes <- paste0(data_polarization$F05, "_", data_polarization$R03)


data_polarization <- merge(data_polarization, data_plot, by = "changes")


linear1 <- lm(diff_vote ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), data = data_polarization)
summary(linear1)

linear2 <- lm(diff_abs ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), data = data_polarization)
summary(linear2)

linear3 <- lm(diff_vote ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), 
              data = data_polarization[data_polarization$theta1>0 & data_polarization$theta2>0,])
summary(linear3)

linear4 <- lm(diff_abs ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), 
              data = data_polarization[data_polarization$theta1>0 & data_polarization$theta2>0,])
summary(linear4)

linear5 <- lm(diff_vote ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), 
              data = data_polarization[data_polarization$theta1<0 & data_polarization$theta2<0,])
summary(linear5)

linear6 <- lm(diff_abs ~ R1601 + R1602 + R1603 + R1604 + R1605 + R1606 + R1607 + R1608 + R1609 + R1610 + R1611 + factor(F05.x), 
              data = data_polarization[data_polarization$theta1<0 & data_polarization$theta2<0,])
summary(linear6)

stargazer(linear1, linear2, linear3, linear4, linear5, linear6,
          omit = "F05.x")

