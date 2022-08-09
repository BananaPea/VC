# Exploratory data analysis

library(tidyverse)
library(ggplot2, warn.conflicts = FALSE) 
library(GGally)
library(gridExtra)
library(kableExtra)
library(dplyr)

df = read.csv('housing.csv')

#Deal categorical variables
df$parking = as.factor(df$parking)

#Summary Table 
library(vtable)
labs = c('Elevation of the base of the house','Distance to Amenity 1',
         'Distance to Amenity 2','Distance to Amenity 3',
         'Number of bathrooms','Square footage of the house',
         ' Parking type','Amount of precipitation',' Final House Sale Price')
st(df, labels = labs)

#Detect outliers
par(mfrow=c(2,4))
boxplot(df$elevation,ylab = 'elevation of the base of the house')
boxplot(df$dist_am1,ylab = 'distance to Amenity 1')
boxplot(df$dist_am2,ylab = 'distance to Amenity 2')
boxplot(df$dist_am3,ylab = 'distance to Amenity 3')
boxplot(df$bath,ylab = 'number of bathrooms')
boxplot(df$sqft,ylab = 'square footage of the house')
boxplot(df$precip,ylab = 'amount of precipitation')
boxplot(df$price, ylab = 'final house sale price')
par(mfrow=c(1,1))

out_evl <- which(df$elevation %in% c(boxplot.stats(df$elevation)$out))    #Outliers of elevation
out_distam1 <- which(df$dist_am1 %in% c(boxplot.stats(df$dist_am1)$out))  #Outliers of dist_am1
out_distam2 <- which(df$dist_am2 %in% c(boxplot.stats(df$dist_am2)$out))  #Outliers of dist_am2
out_distam3 <- which(df$dist_am3 %in% c(boxplot.stats(df$dist_am3)$out))  #Outliers of dist_am3
out_bath <- which(df$bath %in% c(boxplot.stats(df$bath)$out))             #Outliers of bath
out_sqft <- which(df$sqft %in% c(boxplot.stats(df$sqft)$out))             #Outliers of sqft
out_precip <- which(df$precip %in% c(boxplot.stats(df$precip)$out))       #Outliers of precip
out_price <- which(df$price %in% c(boxplot.stats(df$price)$out))          #Outliers of price

#Rows with outliers
out = unique(c(out_evl, out_distam1, out_distam2, out_distam3, out_bath, out_precip, out_price, out_price))
df[out,]

#Cooks distance
mod <- lm(price ~ ., data=df)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
#Delete outliers
df = df[-348,]


#Distribution after delete outliers
df$bath = as.factor(df$bath)
par(mfrow=c(3,3))
hist(df$elevation, xlab = 'elevation of the base of the house', main = 'Histogram of elevation')
hist(df$dist_am1, xlab = 'distance to amenity 1', main = 'Histogram of distance to amenity 1')
hist(df$dist_am2, xlab = 'distance to amenity 2', main = 'Histogram of distance to amenity 2')
hist(df$dist_am3, xlab = 'distance to amenity 3', main = 'Histogram of distance to amenity 3')
barplot(table(df$bath), xlab = 'number of bathrooms', main = 'Barplot of number of bathrooms')
hist(df$sqft, xlab = 'square footage of the house', main = 'Histogram of square footage of the house')
hist(df$precip, xlab = 'amount of precipitation', main = 'Histogram of amount of preciptation')
hist(df$price, xlab = 'final house sale price', main = 'Histogram of final house sale price')
barplot(table(df$parking), xlab = 'parking type', main = 'Barplot of parking type')
par(mfrow=c(1,1))



#Check missing value
which(is.na(df))

#Summary table
library(vtable)
st(df, labels = labs)

#Check Multicollinearity between variables
ggpairs(df, upper=list(continuous=wrap("points", alpha=0.4, color="#d73027")),
        lower="blank", axisLabels="none",
        columnLabels = c("elevation",'distance to amenity 1','distance to amentiry 2', 'distance to amenity 3',
                         'number of bathrooms','square footage','parking type','precipitation','price'))

library(ggcorrplot)
co = cor(subset(df, select = -c(bath,parking)))
row.names(co) = c("elevation",'distance to amenity 1','distance to amentiry 2', 'distance to amenity 3',
                  'square footage','precipitation','price')
colnames(co) = c("elevation",'distance to amenity 1','distance to amentiry 2', 'distance to amenity 3',
             'square footage','precipitation','price')
ggcorrplot(co,hc.order = TRUE, type = "lower", lab = TRUE) # Detected there is high correlation between dist_am1 and dist_am3

library(car)
model1 = lm(price~., data = df)    #all GIVF(1/Df) are less than 4, there isn't a issue for the correlation between variables 
a = vif(model1)                    # https://stacyderuiter.github.io/s245-notes-bookdown/collinearity-and-multicollinearity.html
rownames(a) = c("elevation",'distance to amenity 1','distance to amentiry 2', 'distance to amenity 3','number of bathrooms',
                'square footage','parking','precipitation')
a %>%
  kbl()%>%
  kable_styling()



#Distribution of different parking type
box_parking = ggplot(df, aes(x = parking, y = price)) + 
  geom_boxplot() + 
  labs(x = "Parking type", y = "Price", title = "Parking Type vs Price") + 
  theme(axis.text.x = element_text(angle = 60))
# There is not huge difference of distributions among houses with different parking types 

#Distribution of houses with different number of bathrooms
box_bath = ggplot(df, aes(x = bath, y = price)) + 
  geom_boxplot() + 
  labs(x = "Number of bathrooms", y = "Price", title = "Bathrooms vs Price")
# There is huge difference of distributions among houses with different number of bathrooms
# More bathrooms with higher price

#Relationship between square footage of the house and price
point_sqft = ggplot(df, aes(sqft, price)) + 
  geom_point() + 
  labs(x = "Square footage", y = "Price", title = "Square footage vs. Price") + 
  geom_smooth(method=lm)
#There isn't a clear relationship between square footage and price

#Relationship between elevation of the base of the house of the house and price
point_ele = ggplot(df, aes(elevation, price)) + 
  geom_point() + 
  labs(x = "Elevation", y = "Price", title = "Elevation vs. Price") + 
  geom_smooth(method=lm)

#Relationship between amount of precipitation and price
point_pre = ggplot(df, aes(precip, price)) + 
  geom_point() + 
  labs(x = "Amount of precipitation", y = "Price", title = "Precipitation vs. Price") + 
  geom_smooth(method=lm)

#Relationship between distance to amenity 1,2,3 and price
point_dist1 = ggplot(df, aes(dist_am1, price)) + 
  geom_point() + 
  labs(x = "Distance to Amenity 1", y = "Price", title = "Dist_am1 vs. Price") + 
  geom_smooth(method=lm)

point_dist2 = ggplot(df, aes(dist_am2, price)) + 
  geom_point() + 
  labs(x = "Distance to Amenity 2", y = "Price", title = "Dist_am2 vs. Price") + 
  geom_smooth(method=lm)

point_dist3 = ggplot(df, aes(dist_am3, price)) + 
  geom_point() + 
  labs(x = "Distance to Amenity 3", y = "Price", title = "Dist_am3 vs. Price") + 
  geom_smooth(method=lm)

grid.arrange(box_parking, box_bath, point_ele, point_pre, point_sqft, point_dist1, point_dist2, point_dist3  ,nrow = 2)


#Model Fitting

library(expss)
df = apply_labels(df,
                  elevation = 'elevation of the base',
                  dist_am1 = 'distance to amenity 1',
                  dist_am2 = 'distance to amenity 2',
                  dist_am3 = 'distance to amenity 3',
                  bath = 'number of bathrooms',
                  sqft = 'square footage',
                  parking = 'parking type',
                  precip = 'preciptation',
                  price = 'price'
)

#Linear Regression
library(sjPlot)
df$bath = as.numeric(df$bath)
full_lm_model = lm(price~., data = df)

#forward and backward model selection
best_lm = step(full_lm_model, direction = "both")
tab_model(best_lm, full_lm_model, show.aic = T)

#model check
par(mfrow=c(2,2))
plot(best_lm)
par(mfrow=c(1,1))

#Using the Bayesian Adaptive Sampling (BAS) Package for Bayesian Model Averaging and Variable Selection
library(BAS)
df.ZS <- bas.lm(price ~ .,
                   data = df,
                   prior = "ZS-null",
                   modelprior = uniform(), initprobs = "eplogp",
                   force.heredity = FALSE, pivot = TRUE
)
plot(df.ZS, which = 4, ask = FALSE, caption = "", sub.caption = "")
options(width = 100)
summary(df.ZS) #a list of the top 5 models (in terms of posterior probability) with the zero-one indicators for variable inclusion
image(df.ZS, rotate = F) #Visualization of the Model Space

#Posterior Distributions of Coefficients
coef.ZS = coef(df.ZS)
plot(coef.ZS, subset = c(5:6), ask = F)
confint(coef.ZS)

bas_lm = lm(price~bath, data = df)
tab_model(bas_lm,show.aic = T)
par(mfrow=c(2,2))
plot(bas_lm)
par(mfrow=c(1,1))

# For bas.lm, price ~ bath. For lm, price ~ bath + sqft. 
# For bas.lm, R2 is 0.8594. For lm, R2 is 0.86


#GLM of Gaussian 
plot(density(df$price))

glm_id= glm(price~., family = Gamma(link = "identity"),data = df)   #Identity link
glm_inv = glm(price~., family = Gamma(link = "inverse"),data = df)  #Inverse link
glm_log = glm(price~., family = Gamma(link = "log"),data = df)      #Log link

glm_id_best = step(glm_id, direction = "both")
tab_model(glm_id_best, show.aic = TRUE)

glm_inv_best = step(glm_inv, direction = "both")
tab_model(glm_inv_best, show.aic = TRUE)

glm_log_best = step(glm_log, direction = "both")
tab_model(glm_log_best, show.aic = TRUE)

summary(glm_id_best)$aic
summary(glm_inv_best)$aic
summary(glm_log_best)$aic
tab_model(glm_id_best,glm_inv_best,glm_log_best, show.aic = T, show.r2 = T)
#Gaussian with Identity link has lowest aic

summary(glm_id_best)
summary(glm_id_best)$null.deviance - summary(glm_id_best)$deviance > qchisq(0.95,498-492)  
## TRUE. we can reject the null hypothesis, and the terms are all significant

resp <- resid(glm_id_best, type = "pearson")
resd <- resid(glm_id_best, type = "deviance")
p1<- ggplot(glm_id_best, aes(sample = resp)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Pearson residuals")
p2<- ggplot(glm_id_best, aes(sample = resd)) + geom_point(stat = "qq", color = "#7fc97f") +
  ylab("Deviance residuals")
p3<- ggplot(glm_id_best, aes(x = predict(glm_id_best, type="link"), y =resd))+
  geom_point(col = "#7fc97f") +
  ylab("Deviance residuals") + xlab("Linear predictor")
grid.arrange(p1, p2, p3, nrow = 1)


#Comparison between linear regression final model and GLM final model
tab_model(best_lm, glm_id_best, show.aic = T)



#Lasso Regression

library(caret)
df1 = df
df1$bath = as.numeric(df1$bath)
#Regularization
cols_reg = c('elevation', 'dist_am1', 'dist_am2', 'dist_am3','bath', 'sqft','parking','precip', 'price')
dummies = dummyVars(price ~ ., data = df1[,cols_reg])
train_dummies = predict(dummies, newdata = df1[,cols_reg])
x = as.matrix(train_dummies)
#colnames(x) = c('elevation_of_base', 'distance_to_amenity 1', 'distance to amenity 2', 'distance to amenity 3',
#                'number of bathrooms', 'square footage', 'parking type: Covered', 'parking type: No parking',
#                'parking type: Not provided','parking type: Open', 'preciptation')
y_train = df1$price

library(glmnet)
lasso_fit <- glmnet(x, y = y_train, alpha = 1)
coef(lasso_fit,s=0.5) #lambda = 0.5
coef(lasso_fit,s=1)   #lambda = 1
plot(lasso_fit, xvar = "lambda", label = TRUE)

library(plotmo)
plot_glmnet(lasso_fit, label=TRUE)   
plot_glmnet(lasso_fit, label=8, xvar ="norm")                  # label the 5 biggest final coefs

cv_lasso_fit <- cv.glmnet(x, y = y_train, alpha = 1, nfolds = 5)
plot(cv_lasso_fit)
cv_lasso_fit$lambda.min                                        # Best lambda

eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

lasso_model <- glmnet(x, y_train, alpha = 1, lambda = cv_lasso_fit$lambda.min, standardize = TRUE)
predictions_lasso = predict(lasso_model, s = cv_lasso_fit$lambda.min, newx = x)
eval_results(y_train, predictions_lasso, df1)

coef(lasso_model)
head(predict(lasso_model, newx = x))
head(df1$price)

b = round(coef(lasso_model),2)
b = summary(b)
b = data.frame(Variable = colnames(b)[b$j],
           Estimate = b$x)
b[1,1] = 'Intercept'
b[2,1] = 'number of bathrooms'
b[3,1] = 'square footage'
b[4,1] = 'parking type: Covered'
b %>%
  kbl()%>%
  kable_styling()


#Ridge Regression

ridge_reg = glmnet(x, y_train,alpha = 0, family = 'gaussian')
summary(ridge_reg)

plot_glmnet(ridge_reg, label=TRUE)  
plot_glmnet(ridge_reg, label=8, xvar ="norm")

cv_ridge <- cv.glmnet(x, y_train, alpha = 0)
plot(cv_ridge)
optimal_lambda <- cv_ridge$lambda.min

ridge_model = glmnet(x,y_train,alpha = 0,lambda = optimal_lambda,standardize = T)
c = round(coef(ridge_model),2)
c = summary(c)
c = data.frame(variable = colnames(c)[c$j],
               Estimate = c$x)
c[1,1] = 'Intercept'
c[2,1] = 'elevation of the base'
c[3,1] = 'distance to amenity 1'
c[4,1] = 'distance to amenity 2'
c[5,1] = 'distance to amenity 3'
c[6,1] = 'number of bathroom'
c[7,1] = 'square footage'
c[8,1] = 'parking type: covered'
c[9,1] = 'parking type: no parking'
c[10,1] = 'parking type: not provided'
c[11,1] = 'parking type: open'
c[12,1] = 'amount of preciptation'
c %>%
  kbl()%>%
  kable_styling()

predictions_reg <- predict(ridge_reg, s = optimal_lambda, newx = x)
eval_results(y_train, predictions_reg, df1)


# Generalized Linear Mixed Model
#library(lme4)
#glmm1 <- glmer(price ~ 1 + dist_am1 + dist_am2 + dist_am2 + dist_am3 +sqft + parking + precip + (1|bath),
#               data=df, family=Gamma(link = "identity"))
#summary(glmm1, corr=FALSE)
#step(glmm1)

# Random Forest
library(randomForest)
library(vip)

rf_fit = randomForest(price~., data = df)
rf_fit

plot(rf_fit)
which.min(rf_fit$mse)
sqrt(rf_fit$mse[which.min(rf_fit$mse)])

varImpPlot(rf_fit)

features = setdiff(names(df), "price")
set.seed(123)
m2 = tuneRF(
  x = df[features],
  y = df$price,
  ntreeTry = 500,
  mtryStart = 4,
  setpFactor = 1.5,
  improve = 0.01,
  trace = F
)

rf_final = randomForest(price~., data = df, mtry = 4, ntree = 124, importance = T)
rf_final

varImpPlot(rf_final)
vip(rf_final, geom = "point")

predictions_train = predict(rf_final, newdata = df)
eval_results(df$price, predictions_train, df)

head(df$price)
head(predictions_train)
