library(ggplot2)
library(extraDistr)

plot.hyper<- function(m, n, k){
  miny<- max(0, k-n)
  maxy<- min(k, m)
  y<- c(miny:maxy)
  proby<- dhyper(y, m, n, k)
  Bars<- as.data.frame(cbind(y, proby))
  Dist<- paste("n = ", as.character(k), ",", 
               " M = ", as.character(m), ",",
               " N = ", as.character(m+n),
               sep = "")
  ggplot(Bars, aes(x = y, y = proby))+ 
    geom_bar(stat="identity", width = 1, fill = "blue", 
             colour = "black")+
    labs(x = "y",
         y = "p(y)",
         title = "Hypergeometric Distribution",
         subtitle = Dist)+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
}


plot.nhyper<- function(m, n, k){
  y<- c(0:(qnhyper(0.9999, n, m, k)))
  proby<- dnhyper(y+k, n, m, k)
  Bars<- as.data.frame(cbind(y, proby))
  Dist<- paste("r = ", as.character(k), ",",
               " M = ", as.character(m), ",",
               " N = ", as.character(m+n),
               sep = "")
  plot <- ggplot(Bars, aes(x = y, y = proby))+ 
    geom_bar(stat="identity", width = 1, fill = "blue", 
             colour = "black")+
    labs(x = "y",
         y = "p(y)",
         title = "Negative Hypergeometric Distribution",
         subtitle = Dist)+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  
  return(plot)
}

plot.pois<- function(lambda){
  y<- c(0:qpois(0.9999, lambda))
  proby<- dpois(y, lambda)
  Bars<- as.data.frame(cbind(y, proby))
  Dist<- paste("lambda =", as.character(lambda))
  plot <- ggplot(Bars, aes(x = y, y = proby))+ 
    geom_bar(stat="identity", width = 1, fill = "blue", 
             colour = "black")+
    labs(x = "y",
         y = "p(y)",
         title = "Poisson Distribution",
         subtitle = Dist)+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))
  return(plot)
}

plot.unif <- function(A, B){
  y<- seq(A, B, length.out=1000)
  fy<- dunif(y, A, B)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Min = ", as.character(A), ",", 
               " Max = ", as.character(B), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    geom_segment(aes(x = A, y = 0, 
                     xend = A - 0.1*(B - A), yend = 0), 
                 arrow = arrow(angle = 10))+
    geom_segment(aes(x = B, y = 0, 
                     xend = B + 0.1*(B - A), yend = 0), 
                 arrow = arrow(angle = 10))+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Uniform Distribution p.d.f.",
         subtitle = Dist)
}

plot.norm<- function(mu, sigma){
  y<- seq(mu - 4*sigma, mu+4*sigma, length.out=1000)
  fy<- dnorm(y, mu, sigma)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Mean = ", as.character(mu), ",", 
               " Std. Dev. = ", as.character(sigma), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust = 0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Normal Distribution p.d.f.",
         subtitle = Dist)
}

plot.gamma <- function(alpha, beta){
  y<- seq(0, qgamma(0.9995, shape = alpha, scale = beta), length.out=1000)
  fy<- dgamma(y, shape = alpha, scale = beta)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Shape = ", as.character(alpha), ",", 
               " Scale = ", as.character(beta), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Gamma Distribution p.d.f",
         subtitle = Dist)
}

plot.exp <- function(lambda){
  y<- seq(0, qexp(0.9995, lambda), length.out=1000)
  fy<- dexp(y, lambda)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Rate = ", as.character(lambda), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Exponential Distribution p.d.f",
         subtitle = Dist)
}

plot.chisq <- function(nu){
  y<- seq(0, qchisq(0.9995, nu), length.out=1000)
  fy<- dchisq(y, nu)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Degrees of Freedom = ", as.character(nu), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Chi-Squared Distribution p.d.f",
         subtitle = Dist)
}

plot.beta <- function(alpha, beta){
  y<- seq(0, 1, length.out=1000)
  fy<- dbeta(y, alpha, beta)
  plotdata<- data.frame(cbind(y, fy))
  Dist<- paste("Shape 1 = ", as.character(alpha), ",", 
               " Shape 2 = ", as.character(beta), sep = "")
  ggplot(plotdata, aes(x = y, y = fy))+
    geom_line()+
    theme_bw()+
    theme(axis.title.y = element_text(size = rel(1.4)),
          axis.title.x = element_text(size = rel(1.4)),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_text(size = rel(1.2)),
          plot.title = element_text(hjust=0.5, size = rel(1.75)),
          plot.subtitle = element_text(hjust=0.5, size = rel(1.5)))+
    labs(x = "y",
         y = "f(y)",
         title = "Beta Distribution p.d.f",
         subtitle = Dist)
}

plot.QQ <- function(vals, distr){
  qqplot(vals, distr, 
         xlab="Empirical Quantile", ylab="Theoretical Quantile")
  abline(a=0, b=1, col="red")
}