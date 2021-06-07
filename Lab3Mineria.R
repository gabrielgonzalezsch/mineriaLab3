library(maxent)
library(tm)

# Cargar Dataset de Rotten Tomatoes
ruta <- file.choose()
allData <- read.csv(ruta, fill=TRUE, header=TRUE, sep=",", row.names=NULL)

###Pre-procesamiento del dataset de Rotten Tomatoes

# Eliminar todas las filas que presenten valores Vacios en sus Columnas
allData$rotten_tomatoes_link [allData$rotten_tomatoes_link == ""] <- NA
allData$critic_name [allData$critic_name == ""] <- NA
allData$top_critic[allData$top_critic == ""] <- NA
allData$publisher_name[allData$publisher_name == ""] <- NA
allData$review_type[allData$review_type == ""] <- NA
allData$review_score[allData$review_score == ""] <- NA
allData$review_date[allData$review_date == ""] <- NA
allData$review_content[allData$review_content == ""] <- NA
data <- na.omit(allData)

# # Eliminar todas las filas que presenten RatinScore de Letra 
# data <- data[data$review_score != "A", ]
# data <- data[data$review_score != "B", ]
# data <- data[data$review_score != "C", ]
# data <- data[data$review_score != "D", ]
# data <- data[data$review_score != "A+", ]
# data <- data[data$review_score != "B+", ]
# data <- data[data$review_score != "C+", ]
# data <- data[data$review_score != "D+", ]
# data <- data[data$review_score != "A-", ]
# data <- data[data$review_score != "B-", ]
# data <- data[data$review_score != "C-", ]
# data <- data[data$review_score != "D-", ]
# data <- data[data$review_score != "F", ]
# # casos especiales
# data <- data[data$review_score != "C  -", ]
# data <- data[data$review_score != "A  -", ]
# 
# # Eliminar todas las filas que presenten RatinScore de valor 0 
# data <- data[data$review_score != "0", ]

data2 <- data

#reemplazar review_score con columna de proporcion equivalente, esto evita los rating con valores muy diferentes. como por ejemplo (1/10, 10/100, 1/4, etc.) 
# require(stringr)
# aux <- str_split_fixed(data2$review_score,"/",n=2)
# review_score <- c(as.numeric(aux[,1])/as.numeric(aux[,2]))
# data2 <- data2[,-6]
# data2$review_score <- review_score
# data2 <- data2[data$review_score != 0,]
# data2 <- data2[data$review_score != Inf,]
# summary(data2,maxsum=20)


#Conjunto de textos relacionados a cada categoria.
corpus <- Corpus(VectorSource(data2$review_content))
print(corpus)

#Pasar todos los elementos a minusculas
corpus <- tm_map(corpus, content_transformer(tolower))

#Remover numeros.
corpus <- tm_map(corpus, content_transformer(removeNumbers))

#Remover puntuacion (, ; .)
corpus <- tm_map(corpus, content_transformer(removePunctuation))

#Eliminar salto de linea.
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, '\n')
#Elimianar stopwords.
corpus <- tm_map(corpus, content_transformer(removeWords),stopwords("english"))
#Eliminar espacios en blanco.
corpus <- tm_map(corpus, stripWhitespace)


#### Aplicando el metodo
# Ver filtros persoalizados para analizar bigramas (ejm. cuenta corriente)
matrix <- DocumentTermMatrix(corpus[1:8000])
sparse <- as.compressed.matrix(matrix) 

# Relacion de datos 1/3.
#set.seed(2021)
f <- tune.maxent(sparse[1:3500,],data2$review_type[1:3500],nfold = 3, showall = TRUE, verbose = TRUE)
print(f)

model <- maxent(sparse[1:3500,],data2$review_type[1:3500],l1_regularizer = 0.0, l2_regularizer = 0.6, use_sgd = FALSE, set_heldout = 0, verbose = FALSE)

results <- predict(model,sparse[3501:8000,])

results <- as.data.frame(results)

#Probabilidad de que alguno de estos casos pertenezcan a una de las categorias (columnas?)




#Fresh relevante y >= 0.7 es recuperado
#Fresh relevante y < 0.7 es no recuperado
#Rotten no relevante y >= 0.7 es recuperado
#Rotten no relevante y < 0.7 no recuperado
evaluarModelo <- function(results,df,umbral.fresh){

  for (i in 1:nrow(results)) {
    if(results$labels[i] == "Fresh" && as.numeric(results$Fresh[i]) >= umbral.fresh){
      df$relevante[1] <- 1 + df$relevante[1]
    }
    else if(results$labels[i] == "Fresh" && as.numeric(results$Fresh[i]) < umbral.fresh){
      df$relevante[2] <- 1 + df$relevante[2]
    }
    else if(results$labels[i] == "Rotten" && as.numeric(results$Fresh[i]) >= umbral.fresh){
      df$no.relevante[1] <- 1 + df$no.relevante[1]
    }
    else if(results$labels[i] == "Rotten" && as.numeric(results$Fresh[i]) < umbral.fresh){
      df$no.relevante[2] <- 1 + df$no.relevante[2]
    }
  }
  
  return(df)
}


#Calculando la precisión
precision <- function(eficiencia){
  precision <- eficiencia$relevante[1]/(eficiencia$relevante[1] + eficiencia$no.relevante[1]) 
  return(precision)
} 

#Calculando la sensibilidad
sensibilidad <- function(eficiencia){
  precision <- eficiencia$relevante[1]/(eficiencia$relevante[1] + eficiencia$relevante[2]) 
  return(precision)
} 

#Calculando f1
f1 <- function(p,r){
  f<-(2*p*r)/(p + r)
  return(f)
}

#Hay que identificar cuales osn las etiquetas (label) relevantes
df <- data.frame(matrix(nrow = 2 ,ncol = 2))
columnas <- c("relevante","no.relevante")
colnames(df) <- columnas
df$relevante[1] <- 0
df$relevante[2] <- 0
df$no.relevante[1] <- 0
df$no.relevante[2] <- 0
umbral.fresh <- 0.6
eficiencia <- evaluarModelo(results,df,umbral.fresh)


rownames(eficiencia) <- c("Recuperados","No Recuperados")
p <- precision(eficiencia)
r<- sensibilidad(eficiencia)
f <- f1(as.numeric(p),as.numeric(r))

