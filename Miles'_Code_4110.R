California = read.csv("C:/Users/Owner/Desktop/r_directories/4110/PCA/California.csv")

#Applying Sample Selection Criteria

NEW_CA <- California[c('PERNP', 'OCCP', 'SEX', 'AGEP', 'SCHL')]
NEWCA = subset(NEW_CA, PERNP >0 & AGEP > 24 & AGEP <= 40)

#Defining Variables

Education <- (NEWCA$SCHL)


NEWCA[,'SEX'] <- as.factor(NEWCA[,'SEX']) # dummy variable for sex
Sex <- (NEWCA$SEX)

Age <- (NEWCA$AGEP)

Earnings <- (NEWCA$PERNP)

OCCP <-(NEWCA$OCCP)

#Create Dummy Variables for Sex



#Re-name Occupational Codes

Industry <- cut(OCCP, breaks =c(0010, 0430, 740, 950, 1240, 1560, 1965, 2060, 2160,
                                2550, 2920, 3540, 3655, 3955, 4150, 4250, 4650, 4965, 
                                5940, 6130, 6765, 6940, 7630, 8965, 9750, 9830), 
                
                      labels = c("Mangerial", "Business", "Finance", "Communications",
                           "Engineering", "Science", "Social Services", "Legal",
                           "Education", "Entertainment", "Medical", "Health Services",
                           "Protection", "Culinary", "Custodial", "Personal Services", 
                           "Sales", "Office-Corporate", "FFF", "Construction", "Extraction",
                           "Repairs", "Manufacturing", "Transportation", "Military"))

#Checking for multicollinearity

multico <- cbind(Earnings, Education, OCCP, Age, Sex)
cor(multico)

#Reduced models for each variable

INDmodel <-lm(Earnings ~ Industry, data = NEWCA)

M_model <- lm(Earnings ~ Male, data = NEWCA)
F_model <- lm(Earnings ~ Female, data = NEWCA)

Emodel <- lm(Earnings ~ Education, data = NEWCA)

Amodel <- lm(Earnings ~ Age, data = NEWCA)

#Complete Model

completemodel <-lm(Earnings ~ Age + Sex + Industry + Education, data = NEWCA)
summary(completemodel)

plot(completemodel, main = "Complete Model")
abline(completemodel, col = 'red')

#Evaluating Outliers

library(MASS)
studres <- studres(completemodel)

cooksd <- cooks.distance(completemodel)
sample_size <- nrow(NEWCA)

plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line

text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")

# Remove Outliers

influential_cd <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
influential_sr <- as.numeric(names(studres)[abs(studres) > 3])

revised_CA <- NEWCA [-influential_cd, -influentical_sr ]

revised_model <- lm(Earnings ~ Age + Sex + Industry + Education, data = revised_CA)