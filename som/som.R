#install.packages("kohonen")
library(kohonen)
data(wines)
set.seed(7)
 
training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ])
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
 
## SOM => Unsupervised 
som.wines = som(Xtraining, grid = somgrid(5, 5, "hexagonal"))
 
som.prediction <- predict(som.wines, newdata = Xtest,
                          trainX = Xtraining,
                          trainY = factor(wine.classes[training]))

table(wine.classes[-training], som.prediction$prediction)

plot(som.wines)

summary(som.wines)

#####################
plot(kohmap, type="changes")
plot(kohmap, type="codes", main = c("Codes X", "Codes Y"))
plot(kohmap, type="counts")

## palette suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
rainbow(n, end=4/6, alpha=alpha)[n:1]
}

plot(kohmap, type="quality", palette.name = coolBlueHotRed)
plot(kohmap, type="mapping", labels = wine.classes, col = wine.classes+1, main = "mapping plot")
