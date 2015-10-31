##
##  Sistema de recomendación de cervezas por similaritud
##
##  Created by Daniel Rodríguez Pérez on 31/10/2015.
##  Copyright (c) 2015 Daniel Rodríguez Pérez. All rights reserved.
##

# *****************************************************************************
# Librerias ----
library(BBmisc)
library(plyr)

# *****************************************************************************
# Funciones  ----
common_reviewers_by_id <- function(data, productid1, productid2) {
  # Obtienen la lista de usuarios que han realizado una revisión de ambos
  # productos.
  #
  # Args:
  #   data: el conjunto de datos
  #   productid1: el identificador del primer producto
  #   productid2: el identificador del segundo producto
  #
  # Returns:
  #   Una lista de usuarios que han realizado una revisión de ambos productos.
  
  reviews1          <- subset(data, beer_name == productid1)
  reviews2          <- subset(data, beer_name == productid2)
  reviewers_sameset <- intersect(reviews1[,'review_profilename'],
                                 reviews2[,'review_profilename'])
  
  if (length(reviewers_sameset)==0) {
    return(NA)
  } else {
    return(reviewers_sameset)
  }
}

get_review_metrics <- function(data, productid, userset) {
  # Obtiene la métrica para un producto y una lista de usuarios.
  #
  # Args:
  #   data: el conjunto de datos
  #   productid: el identificador del primer producto
  #   userset: la lista de usuarios
  #
  # Returns:
  #   El valor de la métrica para un producto y una lista de usuarios.
  
  sub.data <- subset(data,  beer_name == productid & review_profilename %in% userset)
  sub.data <- sub.data[order(sub.data$review_profilename), ]
  dups     <- duplicated(sub.data$review_profilename) == FALSE
  sub.data <- sub.data[dups, ]
  
  return(sub.data[, 'review_overall'])
}

calc_similarity <- function(data, productid1, productid2) {
  # Estima la correlación entre las evaluaciones.
  #
  # Args:
  #   data: el conjunto de datos
  #   productid1: el identificador del primer producto
  #   productid2: el identificador del segundo producto
  #
  # Returns:
  #   El valor de la correlación entre las evaluaciones.
  
  common_users <- common_reviewers_by_id(data, productid1, productid2)
  if (length(common_users) < 3 || (length(common_users) == 1 && is.na(common_users))) {
    return(-2)
  }
  
  reviews1 <- get_review_metrics(data, productid1, common_users)
  reviews2 <- get_review_metrics(data, productid2, common_users)
  
  return(cor(reviews1, reviews2))
}

# *****************************************************************************
# Carga de los datos  ----
beer_reviews <- read.csv(bzfile("inst/csv/beer_reviews.csv.bz2"), stringsAsFactors = FALSE)
beer_reviews <- beer_reviews[, c('beer_name', 'review_profilename', 'review_overall')]

# *****************************************************************************
# Proceso de cálculo ----
num_reviews <- ddply(beer_reviews, c('beer_name'), summarise,
                     num_reviews = length(beer_name))
beer_name   <- num_reviews[order(num_reviews$num_reviews, decreasing = TRUE), 'beer_name']
beer_name   <- head(beer_name, 15)

beer_sim     <- expand.grid(beer_1 = beer_name, beer_2 = beer_name, stringsAsFactors = FALSE)
beer_sim$sim <- NA

bar <- makeProgressBar(max   = dim(beer_sim)[1],
                       label = "calculating")

for (i in 1:dim(beer_sim)[1]) {
  pos <- beer_sim$beer_1 == beer_sim$beer_2[i] & beer_sim$beer_2 == beer_sim$beer_1[i]
  if (is.na(beer_sim$sim[pos])) {
    beer_sim$sim[i]   <- calc_similarity(beer_reviews, beer_sim$beer_1[i], beer_sim$beer_2[i])
    beer_sim$sim[pos] <- beer_sim$sim[i]
  }
  
  bar$inc(1)
}

beer_sim <- beer_sim[!is.na(beer_sim$sim), ]
beer_sim <- beer_sim[beer_sim$beer_1 != beer_sim$beer_2, ]

# *****************************************************************************
# Selección de las cervezas mas similares  ----
sim <- beer_sim[beer_sim$beer_2 == beer_sim$beer_1[1], ]
sim <- sim[order(sim$sim, decreasing = TRUE), -2]

head(sim, 5)
