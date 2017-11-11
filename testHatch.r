rm(list=ls())
library(plotly)

df <- data.frame(x=7, y=8, z=4, row.names=NULL)
df2 <- data.frame(x=c("a", "b", "c"), y=c(8,4,7),
                  z=c(2,6,3), row.names=NULL)


p1 <- plot_ly() %>%
    add_trace(data=df2, x=~x, y=~y,
              type="bar") %>%
    add_trace(data=df2, x=~x, y=~z,
              type="bar") %>%
    layout(barmode="stack")


p1

p1 <- add_trace(p1, x=df2$x[which(df2$x %in% c("b", "c"))],
              y=c(10,10),
              type="bar", marker=list(color='rgb(10,10,10)',
                              line=list(color='rgb(50,40,10)')),
              offset=-.4, opacity=.4) %>%
    layout(barmode="overlay")

p1




p1 <- plot_ly() %>%
    add_trace(data=df2, x=~x, y=~y,
              type="bar") %>%
    add_trace(x=df2$x[which(df2$x %in% c("b", "c"))],
              y=df2$y[which(df2$x %in% c("b", "c"))],
              type="bar", marker=list(color='rgb(10,10,10)',
                              line=list(color='rgb(50,40,10)')),
              offset=-.4, opacity=.4)
p1
