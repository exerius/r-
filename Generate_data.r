{
generate.supply <- function(min=100, max=140, days=7){
  #supply <- data.frame(1:days, as.integer(runif(days, min, max)))
  supply <- as.integer(runif(days, min, max))
  #colnames(supply) <- c("Дни", "Поставлено")
  return(supply)
  
}
generate.sales <- function(supply, sale.level=-1){
  if(sale.level==-1){
    return(sapply(supply, function(x) x-sample.int(x, size=1)))}
  else{
    return(sapply(supply, function(x) round(x*sale.level/100)))
  }
}
create.supply <- function(
  n, min, max, days, namess=c("Вода", "Хлеб", "Мука"), sales=-1){
  if (! dir.exists(file.path("~", "Магазины"))){
  dir.create(file.path("~", "Магазины"))}
  shop.numbers <- 1:n
  for(i in shop.numbers){
    cur.supply <- generate.supply(min, max, days)
    cur.sales <- generate.sales(cur.supply, sale.level = sales)
    dirr <- file.path("~","Магазины", paste0("Магазин", i))
    if (! dir.exists(dirr)){dir.create(dirr)}
    cur.supply <- data.frame(1:days)
    cur.sales <- data.frame(1:days)
    for(j in 1:length(namess)){
      supply <- generate.supply(min, max, days)
      cur.supply <- cbind.data.frame(cur.supply, supply)
      cur.sales <- cbind.data.frame(cur.sales, generate.sales(supply, sales))
    }
    colnames(cur.supply) <- c("Дни", namess)
    colnames(cur.sales) <- c("Дни", namess)
    write.table(cur.supply, file.path(dirr, paste0("Поставки", ".in")), row.names = FALSE)
    write.table(cur.sales, file.path(dirr, paste0("Продажа", ".out")), row.names = FALSE)
  }
}
move.files.to.analyse <- function(path){
  path <- path[-1]
  files=list.files(path)
  file.copy(file.path(path, files), file.path("~", "Магазины", "Анализ", paste(basename(path),"_",files, sep="")), overwrite = TRUE)
}
move.dirs.analyse <- function(dir){
  dirs <- list.dirs(dir)
  dirs <- dirs[-1]
  move.files.to.analyse(dirs)
}
goods <- c("Вода", "Хлеб", "Мука")
buy.price <- c(50, 70)
sale.price <- c(70, 100)
#names(buy.price) <- goods
#names(sale.price) <- goods
unlink(file.path("~", "Магазины"), recursive = TRUE)
create.supply(n=10, min=100, max=140, days=17)
if (! dir.exists(file.path("~", "Магазины", "Анализ"))){dir.create(file.path("~", "Магазины", "Анализ"))}
move.dirs.analyse(file.path("~", "Магазины"))
files <- file.path("~", "Магазины", "Анализ", list.files(file.path("~", "Магазины", "Анализ")))
data <- lapply(files, read.table)
#names(data) <- list.files(file.path("~", "Магазины", "Анализ"))
}