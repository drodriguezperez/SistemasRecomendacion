##
##  Filtrado colaborativo
##
##  Created by Daniel Rodríguez Pérez on 31/10/2015.
##  Copyright (c) 2015 Daniel Rodríguez Pérez. All rights reserved.
##

# *****************************************************************************
# Librerias ----
library(BBmisc)
library(plyr)

# *****************************************************************************
# Carga de los datos  ----
beer_reviews <- read.csv(bzfile("inst/csv/beer_reviews.csv.bz2"), stringsAsFactors = FALSE)

# *****************************************************************************
# Calculo de la puntuación promedio del mismo usuario  ----
beer_reviews <- ddply(beer_reviews, c('review_profilename', 'beer_name'),
                      review_overall = mean(review_overall),
                      num_reviews    = length(review_overall))
beer_reviews <- beer_reviews[, c('beer_name', 'review_profilename', 'review_overall')]

# *****************************************************************************
# Evaluación del numero de registros  ----
beer_name          <- unique(beer_reviews$beer_name)
review_profilename <- unique(beer_reviews$review_profilename)

# *****************************************************************************
# Ordenación de las cervezas por reviews  ----
num_reviews <- ddply(beer_reviews, c('beer_name'), summarise,
                     num_reviews = length(beer_name))
hist(num_reviews$num_reviews)

num_reviews <- num_reviews[num_reviews$num_reviews > 2000, ]

# *****************************************************************************
# Filtado de los datos con revisiones  ----
beer_reviews       <- beer_reviews[beer_reviews$beer_name %in% num_reviews$beer_name, ]
beer_name          <- unique(beer_reviews$beer_name)
review_profilename <- unique(beer_reviews$review_profilename)

# *****************************************************************************
# Creación de la matriz de coocurencia  ----
coocuMatrix <- matrix(0, length(beer_name), length(beer_name))

bar <- makeProgressBar(max   = length(review_profilename),
                       label = "Calculando coocurencia...")

for (u in review_profilename) {
  idx <- beer_reviews[beer_reviews$review_profilename == u, ]
  idx <- which(beer_name %in% unique(idx$beer_name))
  m   <- merge(idx, idx)
  for (i in 1:nrow(m)) {
    coocuMatrix[m$x[i], m$y[i]] <- coocuMatrix[m$x[i], m$y[i]] + 1
  }
  
  bar$inc(1)
}

# *****************************************************************************
# Algortimo de recomendación ----
num_beer    <- length(beer_name)
user_review <- beer_reviews[beer_reviews$review_profilename == review_profilename[1],
                            c('beer_name', 'review_overall')]

user_vector <- rep(0, num_beer)
for (i in 1:num_beer) {
  beer_score <- mean(user_review$review_overall[user_review$beer_name == beer_name[i]]) 
  if (is.na(beer_score)) {
    user_vector[i] <- 0
  } else {
    user_vector[i] <- beer_score
  }
}

r                  <- coocuMatrix %*% user_vector
r[user_vector > 0] <- 0

beer_recomedation <- beer_name[order(r, decreasing = TRUE)]
beer_recomedation <- beer_recomedation[1:5]
