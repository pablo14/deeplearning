library(kohonen)
data(wines)
set.seed(7)
 
training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ])
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
 
set.seed(099999)
## SOM => Unsupervised 
som.wines = som(Xtraining, grid = somgrid(5, 5, "hexagonal"))

plot(som.wines, type="counts", palette.name=coolBlueHotRed)

## cases per neuron
library(dplyr)
t=group_by(data.frame(v=som.wines$unit.classif),v) %>% summarize(q=n()) %>% arrange(-q)

 
som.prediction <- predict(sm.wines, newdata = Xtest,
                          trainX = Xtraining,
                          trainY = factor(wine.classes[training]))

table(wine.classes[-training], som.prediction$prediction)

plot(som.wines)

summary(som.wines)

## The lower the better since the neuron is representing quite well the input case
plot(som.wines, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)

plot(som.wines, type="changes")
plot(som.wines, type="codes", main = c("Codes X", "Codes Y"))
plot(som.wines, type="counts")


#####################
## palette suggested by Leo Lopes
coolBlueHotRed <- function(n, alpha = 1) {
rainbow(n, end=4/6, alpha=alpha)[n:1]
}

plot(kohmap, type="quality", palette.name = coolBlueHotRed)
plot(kohmap, type="mapping", labels = wine.classes, col = wine.classes+1, main = "mapping plot")
