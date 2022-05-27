multiplot <- function(row, col) {
  par(mfrow = c(row, col), pty = "m")
}

dates <- c('01','06','09','23','11','23','25')
months <- c('03','03','03','03','04','04','05')
multiplot(2,2)

for(i in 1:length(dates)) {
  path <- paste("/Users/luca/Desktop/macro/data./eurodollar.system/eurodollar-prices-intraday",months,dates,"2022.csv",sep = "-")
  data <- read.csv(path[i], header = T)
  
  summary(data)
  
  
  data$yield <- 100 - data$Last
  data$index <- 1:length(data[,1])
  
  data <- subset(data,data$index < 30)
  
  plot(data$index,data$yield, type = 'l',lwd=5,ylab = "Yield",xlab = "",ylim = c(data$yield[1],5),xaxt='n',main = i)
  axis(side = 1, at = 1:nrow(data), labels = data$Contract,las = 2)
  rect(xleft = 9,xright = 13,ybottom = 0,ytop = 5,col= rgb(1,0,0,alpha=0.25))
  rect(xleft = 13,xright = 17,ybottom = 0,ytop = 5,col= rgb(0,1,0,alpha=0.25))
  rect(xleft = 17,xright = 21,ybottom = 0,ytop = 5,col= rgb(0,0,1,alpha=0.25))
  rect(xleft = 21,xright = 25,ybottom = 0,ytop = 5,col= rgb(1,215/255,178/255,alpha=0.50),border = NULL)
  
}

## Treasury Curve

# treasury <- read.csv('daily-treasury-rates.csv',header = T,sep = ',')

#treasury$index <- 1:length(treasury[,1])

#plot(treasury$index[1:5],treasury[6,2:6],xlim = c(1,7),type = 'l',lwd=5)

