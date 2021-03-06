
# Pruebas:

# j <- 3
# c <- 2
# m <- j + c
# 
# 
# while (c>0 & j>0) {
#   dado <- sample(1:6,1)
#   if (dado == 4) {
#     j <- j +1
#     c <- c -1
#   } else {
#     j <- j - 1
#     c <- c + 1
#   }  
# }
# 
# 
# p <- 0.3
# muchos <- rep(NA, 1000)
# for (i in 1:1000) {
#   muchos[i] <- runif(1) <= p #rnd
# }
# 
# mean(muchos)
# 
# bernoulli <- function(p) {
#   salida <- runif(1) <= p
#   salida
# }
# 
# prueba <- c()
# for (i in 1:1000) {
#   prueba <- c(prueba, bernoulli(0.9))  
# }
# mean(prueba)


# 1
una_jugada <- function(p){
  j <- 3
  c <- 2
  while(c > 0 & j > 0) {
    if (runif(1) <= p) {
      j <- j + 1
      c <- c -1
    } else {
      j <- j - 1
      c <- c + 1
    }
  }
  if (c == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# 2
# gana_juan <- c()
# for (i in 1:1000) {
#   gana_juan <- c(gana_juan,una_jugada(1/6))
# }
# mean(gana_juan)


# 3
# p <- c(0.2, 0.5, 0.8)
# prop_gana <- c()
# for (x in p) {
#   gana_juan <- c()
#   for (i in 1:1000) {
#     gana_juan <- c(gana_juan,una_jugada(x))
#   }
#   prop_gana <- c(prop_gana, mean(gana_juan))
# }
# 
# print(p)
# print(prop_gana)

#4
juan_se_arruina <- function(j, m, p){
  c <- m-j
  while(c > 0 & j > 0) {
    if (runif(1) <= p) {
      j <- j + 1
      c <- c -1
    } else {
      j <- j - 1
      c <- c + 1
    }
  }
  if (c == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#5
# arruina_juan <- c()
# for (i in 1:1000) {
#   arruina_juan <- c(arruina_juan,juan_se_arruina(3,5,1/6))
# }
# 1-mean(arruina_juan)


#6
estimacion_juan_gana <- function(j,m,p,n_rep) {
  gana_j <- c()
  ji <- j
  ci <- m-j
  for (i in 1:n_rep) {
    j <- ji
    c <- ci
    while(c > 0 & j > 0) {
      if (runif(1) <= p) {
        j <- j + 1
        c <- c -1
      } else {
        j <- j - 1
        c <- c + 1
      }
    }
    if (c == 0) {
      gana_j <- c(gana_j,TRUE)
    } else {
      gana_j <- c(gana_j,FALSE)
    }
  }
  return(mean(gana_j))
}



#7
# resultados <- c()
# for (i in 1:5) {
#   resultados <- c(resultados,estimacion_juan_gana(i,5,1/6,1000))
# }
# print(resultados)


#8
# resultados <- c()
# for (i in 1:5) {
#   resultados <- c(resultados,estimacion_juan_gana(i,5,1/6,1000))
# }
# plot(1:5,resultados, xlab = "j: Cantidad de monedas iniciales de Juan", ylab = "Proporción de veces que gana Juan", col="red")
# 
# resultados <- c()
# for (i in 1:5) {
#   resultados <- c(resultados,estimacion_juan_gana(i,5,0.5,1000))
# }
# points(1:5,resultados, col="blue")
# 
# resultados <- c()
# for (i in 1:5) {
#   resultados <- c(resultados,estimacion_juan_gana(i,5,0.8,1000))
# }
# points(1:5,resultados, col="green")
# title("Proporción que gana Juan vs monedas iniciales vs probabilidad de ganar")
# legend(x = "bottomright", legend = c("p = 1/6", "p = 1/2", "p = 4/5"), fill = c("red", "blue", "green"))

#9
# resultados <- c()
# for (i in 1:10) {
#   resultados <- c(resultados,estimacion_juan_gana(i,10,0.5,1000))
# }
# plot(1:10,resultados, xlab = "j: Cantidad de monedas iniciales de Juan", ylab = "Proporción de veces que gana Juan", col="red")
# 
# resultados <- c()
# for (i in 1:20) {
#   resultados <- c(resultados,estimacion_juan_gana(i,20,0.5,1000))
# }
# points(1:20,resultados, col="blue")
# 
# resultados <- c()
# for (i in 1:30) {
#   resultados <- c(resultados,estimacion_juan_gana(i,30,0.5,1000))
# }
# points(1:30,resultados, col="green")
# 
# resultados <- c()
# for (i in 1:50) {
#   resultados <- c(resultados,estimacion_juan_gana(i,50,0.5,1000))
# }
# points(1:50,resultados, col="yellow")
# 
# title("Proporción que gana Juan vs monedas iniciales vs cant. monedas total")
# legend(x = "topleft", legend = c("m = 10", "m = 20","m = 30","m = 50"), fill = c("red", "blue", "green", "yellow"))

#10
# cuando j (cant. inicial de monedas de Juan) es mayor, mayor es la proporción de veces que gana Juan
# cuando m (cant. inicial de monedas totales) es mayor, menor es la proporción de veces que gana Juan
# Una conjetura entonces podría ser: prop = j/m
# prop <- c()
# for (i in 1:10) {
#   prop <- c(prop, i/10)
# }
# points(1:10, prop, pch=4, col="red")
# 
# prop <- c()
# for (i in 1:20) {
#   prop <- c(prop, i/20)
# }
# points(1:20, prop, pch=4, col="blue")
# 
# prop <- c()
# for (i in 1:30) {
#   prop <- c(prop, i/30)
# }
# points(1:30, prop, pch=4, col="green")
# 
# prop <- c()
# for (i in 1:50) {
#   prop <- c(prop, i/50)
# }
# points(1:50, prop, pch=4, col="yellow")


#11
# par(mfrow=c(1,3))

#1er gráfico
# resultados <- c()
# for (i in 1:10) {
#   resultados <- c(resultados,estimacion_juan_gana(i,10,1/6,1000))
# }
# plot(1:10,resultados, xlab = "j: Cantidad de monedas iniciales de Juan", ylab = "Proporción de veces que gana Juan", col="red")
# 
# resultados <- c()
# for (i in 1:20) {
#   resultados <- c(resultados,estimacion_juan_gana(i,20,1/6,1000))
# }
# points(1:20,resultados, col="blue")
# 
# resultados <- c()
# for (i in 1:30) {
#   resultados <- c(resultados,estimacion_juan_gana(i,30,1/6,1000))
# }
# points(1:30,resultados, col="green")
# 
# resultados <- c()
# for (i in 1:50) {
#   resultados <- c(resultados,estimacion_juan_gana(i,50,1/6,1000))
# }
# points(1:50,resultados, col="yellow")
# 
# title("P = 1/6")
# legend(x = "topleft", legend = c("m = 10", "m = 20","m = 30","m = 50"), fill = c("red", "blue", "green", "yellow"))


#2do gráfico
# resultados <- c()
# for (i in 1:10) {
#   resultados <- c(resultados,estimacion_juan_gana(i,10,0.5,1000))
# }
# plot(1:10,resultados, xlab = "j: Cantidad de monedas iniciales de Juan", ylab = "Proporción de veces que gana Juan", col="red")
# 
# resultados <- c()
# for (i in 1:20) {
#   resultados <- c(resultados,estimacion_juan_gana(i,20,0.5,1000))
# }
# points(1:20,resultados, col="blue")
# 
# resultados <- c()
# for (i in 1:30) {
#   resultados <- c(resultados,estimacion_juan_gana(i,30,0.5,1000))
# }
# points(1:30,resultados, col="green")
# 
# resultados <- c()
# for (i in 1:50) {
#   resultados <- c(resultados,estimacion_juan_gana(i,50,0.5,1000))
# }
# points(1:50,resultados, col="yellow")
# 
# title("P = 1/2")
# legend(x = "topleft", legend = c("m = 10", "m = 20","m = 30","m = 50"), fill = c("red", "blue", "green", "yellow"))


#3er gráfico
# resultados <- c()
# for (i in 1:10) {
#   resultados <- c(resultados,estimacion_juan_gana(i,10,4/5,1000))
# }
# plot(1:10,resultados, xlab = "j: Cantidad de monedas iniciales de Juan", ylab = "Proporción de veces que gana Juan", col="red")
# 
# resultados <- c()
# for (i in 1:20) {
#   resultados <- c(resultados,estimacion_juan_gana(i,20,4/5,1000))
# }
# points(1:20,resultados, col="blue")
# 
# resultados <- c()
# for (i in 1:30) {
#   resultados <- c(resultados,estimacion_juan_gana(i,30,4/5,1000))
# }
# points(1:30,resultados, col="green")
# 
# resultados <- c()
# for (i in 1:50) {
#   resultados <- c(resultados,estimacion_juan_gana(i,50,4/5,1000))
# }
# points(1:50,resultados, col="yellow")
# 
# title("P = 4/5")
# legend(x = "topleft", legend = c("m = 10", "m = 20","m = 30","m = 50"), fill = c("red", "blue", "green", "yellow"))
