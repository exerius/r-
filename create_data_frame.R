name.by.number <- function(number){
  return(c(paste("Магазин", number, "_Поставки.in", sep=""), paste("Магазин", number, "_Продажа.out", sep="")))
}
count.sales <- function(number){
  name <- name.by.number(number)
  return(Reduce("+", lapply(data[[name[[1]]]][2:len, 2:(length(goods)+1)], as.integer)))
}
count.income <- function(number){
  name <- name.by.number(number)
  return(sum(mapply("*", lapply(data[[name[[1]]]][2:len, 2:(length(goods)+1)], function(x) as.integer(x)), as.integer(sale.price))))
}
count.realisation <- function(number){
  name <- name.by.number(number)
  return(sum(as.integer(unlist(data[[name.by.number(number)[[1]]]][2:len, 2:(length(goods)+1)]))))
}
count.utilisation <- function(number){
  name <- name.by.number(number)
  summ.bought <- sum(as.integer(unlist(data[[name.by.number(number)[[1]]]][2:len, 2:(length(goods)+1)])))
  summ.sold <- sum(as.integer(unlist(data[[name.by.number(number)[[2]]]][2:len, 2:(length(goods)+1)])))
  return(summ.bought-summ.sold)
}
count.profit <- function(number){
  income <- count.income(number)
  name <- name.by.number(number)
  goods.cost <- sum(mapply("*", lapply(data[[name[[2]]]][2:len, 2:(length(goods)+1)], function(x) as.integer(x)), as.integer(buy.price)))
  utilisation.cost <- sum((sapply(data[[name[[1]]]][2:len, 2:(length(goods)+1)], function(x) sum(as.integer(x)))
                      -sapply(data[[name[[2]]]][2:len, 2:(length(goods)+1)], function(x) sum(as.integer(x))))*util.price)
  return(income-goods.cost-utilisation.cost)
}
count.sd <- function(number){
  name <- name.by.number(number)
  return(sd(as.integer(unlist(data[[name[[2]]]][2:len, 2:length(goods)]))))
}
count.everything <- function(number){
  return(c(count.income(number), count.profit(number), count.realisation(number),
           count.utilisation(number), count.sd(number), count.max.sales(number),
           count.which.max.sales(number), count.min.sales(number), count.which.min.sales(number),count.max.utilisation(number),
           count.which.max.sales(number)))
}
count.max.sales <- function(number){
  return(max(count.sales(number)))
}
count.which.max.sales <- function(number){
  return(which.max(count.sales(number)))
}
count.min.sales <- function(number){
  return(min(count.sales(number)))
}
count.which.min.sales <- function(number){
  return(which.min(count.sales(number)))
}
count.buyings <- function(number){
  name <- name.by.number(number)
  return(Reduce("+", lapply(data[[name[[2]]]][2:len, 2:(length(goods)+1)], as.integer)))
}
count.utilisation.by.day <- function(number){
  return(count.sales(number)-count.buyings(number))
}
count.max.utilisation <- function(number){
  return(max(count.utilisation.by.day(number)))
}
count.which.max.utilisation <- function(number){
  return(which.max(count.utilisation.by.day(number)))
}
{
len <- length(data[[1]][[1]])
shops.number <- length(data)/2
goods <- as.character(data[[1]][1,2:length(data[[1]][1,])])
buy.price <- sample(10:100, length(goods))
sale.price <- sapply(buy.price, function(x) sample(x:101, 1))
util.price <- sample(1:10, length(goods))
names(buy.price) <- goods
names(sale.price) <- goods
names(util.price) <- goods
files <- file.path("~", "Магазины", "Анализ", list.files(file.path("~", "Магазины", "Анализ")))
data <- lapply(files, read.table)
names(data) <- list.files(file.path("~", "Магазины", "Анализ"))
tab <- data.frame(matrix(0, ncol = 11, nrow = shops.number+2))
row.names(tab) = c(paste(rep("Магазин"), 1:shops.number), "Итого", "Среднее")
colnames(tab) <-  c("Выручка, руб", "Прибыль, руб", "Реализация", "Списание", 
                    "Равномерность продаж", "Продажи макс","День", "Продажи мин",
                    "День", "Списание макс", "День")
tab[1:shops.number, 1:11] <- t(sapply(1:shops.number, count.everything))
tab[shops.number+1, 1:5] <- mapply(sum, tab[1:shops.number, 1:5]) 
tab[shops.number+2, 1:5] <- mapply(mean, tab[1:shops.number, 1:5])
}