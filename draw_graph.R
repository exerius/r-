name.by.number <- function(number){
  return(c(paste("�������", number, "_��������.in", sep=""), paste("�������", number, "_�������.out", sep="")))
}
draw.good <- function(goods.sold,  goods.bought, days){
  utilisation <- as.integer(goods.bought[2:length(goods.bought)]) - as.integer(goods.sold[2:length(goods.sold)]);
  print(length(goods.sold));
  print(length(utilisation));
  plot(x = days, y = goods.sold, type = "l");
  lines(x = days, y = utilisation);
}
draw.shop <- function(number){
  name <- name.by.number(number);
  tab.sold <- read.table(file.path("��������", "������", name[[2]]));
  tab.bought <- read.table(file.path("��������", "������", name[[1]]));
  draw.good(tab.sold[[2]], tab.bought[[2]], tab.sold[[1]]);
}