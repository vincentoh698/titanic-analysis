df <- read.csv('train_cleaned.csv')
head(df)
names(df)[names(df) == "Survived"] <- "y"
#p. 107
plot(jitter(Survived, 0.08) ~ Age, data=df) # scatterplot of y by x=width
library(gam) # for fitting generalized additive models
gam.fit <- gam(y ~ s(Age), family=binomial, data=df) # s = smooth funct.
curve(predict(gam.fit, data.frame(Age=x), type="resp"), add=TRUE)
#glm
fit <- glm(y ~ Age, family=binomial, data=df) # link=logit is default
curve(predict(fit, data.frame(Age=x), type="resp"), add=TRUE)
summary(fit)

#p. 110
library(car)
Anova(fit) # likelihood-ratio test of width effect

#p. 117
fit <- glm(y ~ factor(Pclass) + factor(Sex) + Age + SibSp, family=binomial, data=df)
summary(fit)
Anova(fit)

#p. 120
fit2 <- glm(y ~ Pclass + Sex + Age + SibSp, family=binomial, data=df)
summary(fit2)
Anova(fit2)
p3 <- I(df$Pclass == 3)
f <- I(df$Sex == 'female')
fit3 <- glm(y ~ p3 + f, family=binomial, data=df)
summary(fit3)
anova(fit3, fit, test="LRT")

#p. 121
glm(y ~ Age + p3 + Age:p3, family=binomial, data=df)


#p.124
fit3 <- glm(y ~ Age + p3, family=binomial, data=df)
library(mfx)
logitmfx(fit3, atmean=FALSE, data=df)

#p. 125
prop <- sum(df$y)/nrow(df) # sample proportion of 1's for y variable
fit <- glm(y ~ Age + factor(Pclass), family=binomial, data=df)
predicted <- as.numeric(fitted(fit) > prop) 
xtabs(~ df$y + predicted)

#p. 127
library(pROC)
rocplot <- roc(y ~ fitted(fit), data=df)
plot.roc(rocplot, legacy.axes=TRUE) # Specficity on x axis if legacy.axes=F
auc(rocplot) # auc = area under ROC curve = concordance index
cor(df$y, fitted(fit))#correlation:R

#p.118
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming df is your data frame and it has been loaded correctly.
# Fit the model with interaction between Pclass and Sex
fit <- glm(y ~ Age * factor(Pclass) * factor(Sex), family = binomial, data = df)

# Create a new data frame for prediction
age_range <- seq(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE), length.out = 100)
classes <- c(1, 2, 3)
sexes <- c("female", "male")
prediction_data <- expand.grid(Age = age_range, Pclass = classes, Sex = sexes)

# Predict probabilities
prediction_data$predicted_probability <- predict(fit, newdata = prediction_data, type = "response")

# Plot
ggplot(prediction_data, aes(x = Age, y = predicted_probability, color = factor(Pclass), linetype = Sex)) +
  geom_line(size = 1.5) +
  labs(title = "Predicted Probability by Age, Pclass, and Sex",
       x = "Age",
       y = "Predicted Probability") +
  scale_color_discrete(name = "Pclass") +
  scale_linetype_discrete(name = "Sex")
