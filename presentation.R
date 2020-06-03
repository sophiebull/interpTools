series <- simXt(D = 3, n = 150, vary = "Mt", numFreq = 2)

series_df <- data.frame(xt = series$Xt[[3]], mt = series$Mt[[3]], tt = series$Tt[[3]], wt = series$Wt[[3]])

ggplot() + geom_line(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), lwd = 0.2) + 
  geom_point(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), size = 0.5) + 
  labs(x = paste0("time (",bquote(t),")"), y = bquote(x[t])) + theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

#mt
ggplot() + geom_line(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", lwd = 0.2) + 
  geom_point(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", size = 0.5) +   
  geom_line(data = series_df, aes(x = 1:length(mt), y = as.numeric(mt)), color = "red", lwd = 0.4) + 

  
  labs(x = paste0("time (",bquote(t),")"), y = bquote(m[t])) + theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

#tt
ggplot() + geom_line(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", lwd = 0.2) + 
  geom_point(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", size = 0.5) +   
  geom_line(data = series_df, aes(x = 1:length(tt), y = as.numeric(tt) + as.numeric(mt)), color = "red", lwd = 0.4) + 

  
  labs(x = paste0("time (",bquote(t),")"), y = bquote(m[t] + t[t])) + theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

#wt
ggplot() + geom_line(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", lwd = 0.2) + 
  geom_point(data = series_df, aes(x = 1:length(xt), y = as.numeric(xt)), color = "grey", size = 0.5) + 
  geom_line(data = series_df, aes(x = 1:length(wt), y = as.numeric(wt) + as.numeric(mt) + as.numeric(tt)), color = "red", lwd = 0.4) + 
  
  labs(x = paste0("time (",bquote(t),")"), y = bquote(m[t] + t[t] + xi[t])) + theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

#forecast
ggplot() + xlim(1,150) + geom_line(data = series_df[1:125,], aes(x = 1:125, y = as.numeric(xt)), lwd = 0.2) + 
  geom_point(data = series_df[1:125,], aes(x = 1:length(xt), y = as.numeric(xt)), size = 0.5) + 
  
  geom_line(data = series_df[125:150,], aes(x = 125:150, y = as.numeric(xt)), color = "red", lwd = 0.4, lty = 3) +
  #geom_point(data = series_df[126:150,], aes(x = 126:150, y = as.numeric(xt)), color = "red", size = 0.5) + 
  
  labs(x = paste0("time (",bquote(t),")"), y = bquote(x[t])) + theme_light() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

#spectrum
spec <- spec.mtm(timeSeries = series$Xt[[3]], plot = F)
spectrum <- spec$spec
spectrum_df <- data.frame(hf = spectrum)

ggplot() + geom_line(data = spectrum_df, aes(x = 1:length(hf), y = hf), lwd = 0.2)  + 
  #geom_point(data = spectrum_df, aes(x = 1:length(hf), y = hf), size = 0.5)  +
  
  theme_light() + 
  
  scale_x_continuous(breaks = round(seq(1, 260, length.out = 10))) + 
  
  labs(x = "frequency", y = "spectrum estimate") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 13))


#stationarity
mt_y <- simMt(n = 150, numTrend = 0)$value
mt_n <- simMt(n = 150, numTrend = 1)$value

tt_y <- simTt(n = 150, numFreq = 2)$value
tt_n <- simTt(n = 150, numFreq = 2)$value
tt_n[80:125] <- tt_n[80:125]*10

wt_y <- simWt(n = 150, var = 1)$value

xt_y <- mt_y + tt_y + wt_y
xt_n1 <- mt_n + tt_y + wt_y
xt_n2 <- mt_y + tt_n + wt_y

the_series <- data.frame(xt_y = xt_y, xt_n1 = xt_n1, xt_n2 = xt_n2, mt_y = mt_y, mt_n = mt_n)
curve <- dnorm(-22:23, sd = 10) * 100
var_band <- rep(var(the_series$xt_y), length(the_series$xt_n2))
var_band[75:120] <- var_band[75:120] + curve

## good

good <- ggplot() + geom_line(data = the_series, aes(x = 1:length(xt_y), y = as.numeric(xt_y)), lwd = 0.2) + 
          geom_line(data = the_series, aes(x = 1:length(mt_y), y = as.numeric(mt_y)), color = "chartreuse4", lwd = 0.4) +  
          geom_ribbon(data = the_series, aes(x = 1:length(xt_y), ymin = mt_y + var(xt_y), ymax = mt_y - var(xt_y)), fill = "chartreuse4", alpha = 0.2) + 
          theme_light() + 
          labs(x = "time", y = bquote(x[t])) +
          ylim(-6,8) + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_text(size = 15))

## bad (1)
bad1 <- ggplot() + geom_line(data = the_series, aes(x = 1:length(xt_n1), y = as.numeric(xt_n1)), lwd = 0.2) + 
          geom_ribbon(data = the_series, aes(x = 1:length(xt_n1), ymin = mt_n + var(xt_y), ymax = mt_n - var(xt_y)), fill = "red", alpha = 0.2) + 
          geom_line(data = the_series, aes(x = 1:length(mt_n), y = as.numeric(mt_n)), color = "red", lwd = 0.4) +
          theme_light() + 
          labs(x = "time", y = bquote(x[t])) +
          ylim(-8,8) + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(),
                axis.text.y = element_blank())

## bad (2)
bad2 <- ggplot() + geom_line(data = the_series, aes(x = 1:length(xt_n2), y = as.numeric(xt_n2)), lwd = 0.2) + 
          geom_ribbon(data = the_series, aes(x = 1:length(xt_n2), ymin = mt_y + var_band, ymax = mt_y - var_band), fill = "red", alpha = 0.2) + 
          geom_line(data = the_series, aes(x = 1:length(mt_y), y = as.numeric(mt_y)), color = "red", lwd = 0.4) +
          theme_light() + 
          labs(x = "time", y = bquote(x[t])) +
          ylim(-8,10) + 
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                axis.title.x = element_blank(), 
                axis.title.y = element_blank(),
                axis.text.y = element_blank())
        

grid.arrange(good,bad1,bad2, nrow = 1, bottom = "time")


# gap plots

makeGapPlots <- function(orig, prop_missing, gap_width, 
                         axis.text.x = T, axis.text.y = T, 
                         axis.title.x = T, axis.title.y = T){
  
  gappy <- gaps(x = as.numeric(orig), prop_missing = prop_missing, gap_width = gap_width)
  
  replaced <- orig
  
  
  if(axis.title.x){
    t_axis.title.x = element_text(size = 15)
  }
  
  if(!axis.title.x){
    t_axis.title.x = element_blank()
  }
  
  if(axis.title.y){
    t_axis.title.y = element_text(size = 15)
    t_y <- bquote(x[t])
  }
  
  if(!axis.title.y){
    t_axis.title.y = element_text(size = 15)
    t_y = ""
  }
  
  if(axis.text.x){
    t_axis.text.x = element_text()
  }
  
  if(!axis.text.x){
    t_axis.text.x = element_blank()
  }
  
  if(axis.text.y){
    t_axis.text.y = element_text()
  }

  if(!axis.text.y){
    t_axis.text.y = element_blank()
  }
  
  ## to make dotted line - need value before and after
  replaced_index <- which(is.na(gappy))
  sandwich <- c((replaced_index - 1), (replaced_index + 1))
  replaced_index <- c(replaced_index,sandwich)
  replaced_index <- unique(replaced_index)
  
  index <- 1:length(orig)
  replaced[!(index %in% replaced_index)] <- NA
  
  ## to make empty points
  act_replaced <- orig 
  act_replaced[which(!is.na(gappy))] <- NA
  
  ## 
  gappy_data <- data.frame(orig = as.numeric(orig), gappy = as.numeric(gappy), replaced = as.numeric(replaced), act_replaced = as.numeric(act_replaced))
  
  ## gappified
  plot <- ggplot() + geom_line(data = gappy_data, aes(x = 1:length(orig), y = replaced), color = "red", lwd = 0.2, lty = 2) + 
    geom_point(data = gappy_data, aes(x = 1:length(orig), y = gappy), size = 0.5) + 
    
    geom_line(data = gappy_data, aes(x = 1:length(orig), y = gappy), lwd = 0.2) + 
    geom_point(data = gappy_data, aes(x = 1:length(orig), y = act_replaced), color = "red", size = 1, shape = 1) + 
    
    geom_ribbon(data = gappy_data, aes(x = 1:length(orig), ymin = replaced-1, ymax = replaced+1), fill = "red", alpha = 0.2) +
    
    theme_light() + 
    labs(x = "time", y = t_y) + 
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = t_axis.title.x,
          axis.title.y = t_axis.title.y,
          axis.text.x = t_axis.text.x,
          axis.text.y = t_axis.text.y)
  
  return(plot)
}

# contiguity
orig <- simXt(D = 1, n = 150, numTrend = 2, vary = "Tt")$Xt[[1]]
makeGapPlots(orig = orig, prop_missing = 0.3, gap_width = 5)


# severe gaps
orig <- simXt(D = 1, n = 80, numTrend = 2, vary = "Tt")$Xt[[1]]

## increasing prop_missing
p1 <- makeGapPlots(orig = orig, prop_missing = 0.05, gap_width = 1, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = F)
p2 <- makeGapPlots(orig = orig, prop_missing = 0.10, gap_width = 1, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = T)
p3 <- makeGapPlots(orig = orig, prop_missing = 0.15, gap_width = 1, axis.text.x = T, axis.text.y = T, axis.title.x = F, axis.title.y = F)

grid.arrange(p1,p2,p3, bottom = "time", ncol = 1)

## increasing gap_width
p1 <- makeGapPlots(orig = orig, prop_missing = 0.20, gap_width = 1, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = F)
p2 <- makeGapPlots(orig = orig, prop_missing = 0.20, gap_width = 8, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = T)
p3 <- makeGapPlots(orig = orig, prop_missing = 0.20, gap_width = 16, axis.text.x = T, axis.text.y = T, axis.title.x = F, axis.title.y = F)

grid.arrange(p1,p2,p3, bottom = "time", ncol = 1)


# Increasing trendss

makeOrigPlots <- function(mt, tt1, tt2 = NULL, tt3 = NULL, wt,
                          axis.text.x = T, axis.text.y = T, 
                          axis.title.x = T, axis.title.y = T){
  
  n = length(mt)
  t = seq(1,80, length.out = n)
  
  
  if(is.null(tt2)){
    tt2 <- rep(0,n)
  }
  if(is.null(tt3)){
    tt3 <- rep(0,n)
  }
  
  
  if(axis.title.x){
    t_axis.title.x = element_text(size = 15)
  }
  
  if(!axis.title.x){
    t_axis.title.x = element_blank()
  }
  
  if(axis.title.y){
    t_axis.title.y = element_text(size = 15)
    t_y <- bquote(x[t])
  }
  
  if(!axis.title.y){
    t_axis.title.y = element_text(size = 15)
    t_y = ""
  }
  
  if(axis.text.x){
    t_axis.text.x = element_text()
  }
  
  if(!axis.text.x){
    t_axis.text.x = element_blank()
  }
  
  if(axis.text.y){
    t_axis.text.y = element_text()
  }
  
  if(!axis.text.y){
    t_axis.text.y = element_blank()
  }
  
  orig_data <- data.frame(mt = mt, tt1 = tt1, tt2 = tt2, tt3 = tt3, wt = wt)
  
  plot <- ggplot() + geom_line(data = orig_data, aes(x = 1:n, y = mt+tt1 + tt2 + tt3 + wt), lwd = 0.3) + 
    geom_line(data = orig_data, aes(x = 1:n, y = mt + tt1 + tt2 + tt3), color = "red", lwd = 0.5) + 
    theme_light() + 
    
    labs(x = "time", y = t_y) + 
    
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x = t_axis.title.x,
          axis.title.y = t_axis.title.y,
          axis.text.x = t_axis.text.x,
          axis.text.y = t_axis.text.y)
  
  
  return(plot)
}

## generating components
t <- seq(1,80, length.out = 200)

mt <- (1/14)*t^2 

tt1 <- 50*sin(t/2)
tt2 <- 70*sin(t+40)
tt3 <- 60*sin(t*2)

wt <- rnorm(length(t), sd = 40)


## using function to plot
p1 <- makeOrigPlots(mt = mt, tt1 = tt1, wt = wt, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = F)
p2 <- makeOrigPlots(mt = mt, tt1 = tt1, tt2 = tt2, wt = wt, axis.text.x = F, axis.text.y = T, axis.title.x = F, axis.title.y = T)
p3 <- makeOrigPlots(mt = mt, tt1 = tt1, tt2 = tt2, tt3 = tt3, wt = wt, axis.text.x = T, axis.text.y = T, axis.title.x = F, axis.title.y = F)

grid.arrange(p1,p2,p3, bottom = "time", ncol = 1)


# absolute differences
orig <- simXt(D = 1, n = 100, trendType = "polynomial", bandwidth = 2, numTrend = 2, vary = "Tt", snr = 1.5)$Xt[[1]]
gappy <- gaps(x = orig, prop_missing = 0.3, gap_width = 5)
int <- tsinterp::interpolate(gap = which(is.na(gappy) == TRUE), progress = FALSE, z = gappy)[[1]]

the_gaps_index <- which(is.na(gappy)) # index positions of gaps


the_gaps_actual <- rep(NA, length(orig)) 
the_gaps_actual[the_gaps_index] <- orig[the_gaps_index] # values that were removed

the_gaps <- c(the_gaps_index-1,the_gaps_index+1) 
the_gaps <- unique(the_gaps) # extending bounds by 1 for ribbon


replaced <- int[the_gaps] # values that were replaced, extended by 1 for ribbon

the_missing <- rep(NA, length(orig))
the_missing[the_gaps] <- orig[the_gaps]
the_replaced <- rep(NA, length(int))
the_replaced[the_gaps] <- replaced

the_replaced_actual <- rep(NA, length(int))
the_replaced_actual[the_gaps_index] <- int[the_gaps_index]

factor <- rep("original", length(orig))
factor[the_gaps_index] <- "interpolated"

data <- data.frame(factor = factor, orig = as.numeric(orig), gappy = as.numeric(gappy), int = as.numeric(int), the_missing = the_missing, the_replaced = the_replaced, the_gaps_actual = the_gaps_actual, the_replaced_actual = the_replaced_actual)

plot <- ggplot() + geom_line(data = data, aes(x = 1:100, y = orig), lwd = 0.2) +
  
  geom_line(data = data, aes(x = 1:100, y = the_replaced), color = "red", lwd = 0.2) + # replaced line
  
  geom_point(data = data, aes(x = 1:100, y = the_replaced_actual), color = "red", size = 0.5) + # replaced points
  
  geom_point(data = data, aes(x = 1:100, y = gappy), size = 0.5) + # original points
  
  geom_point(data = data, aes(x = 1:100, y = the_gaps_actual), size = 1, shape = 1) + 
  
  geom_ribbon(data = data, aes(x = 1:100, ymin = the_missing, ymax = the_replaced), fill = "red", alpha = 0.2) + 

  theme_light() + 
  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  
  labs(x = "time", y = bquote(X[t])) 


# aggregate metrics 
# ORIGINAL SERIES 

t <- 1:100

mt <- 3*t 
tt <- 40*sin(t) + 30*sin(t)
wt <- rnorm(length(t), sd = 40)

xt <- mt + tt + wt

data <- data.frame(xt = xt, mt = mt, tt = tt, wt = wt)

plot <- ggplot() + geom_line(data = data, aes(x = 1:length(xt), y = xt), lwd = 0.2) +
  geom_point(data = data, aes(x = 1:length(xt), y = xt), size = 0.5) + 
  theme_light() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x = "time", y = bquote(X[t])) 

# Sampling distribution
values.sim <- rf(1000, df1 = 5, df2 = 29)
dataF <- data.frame(values.sim = values.sim)
ggplot(data = dataF, aes(x = values.sim)) + geom_histogram(aes(y = ..density..), color = "white", fill = "red") + 
  
  geom_vline(xintercept = mean(values.sim)) + 
  geom_vline(xintercept = median(values.sim), lty = 2) + 
  
  theme_light() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  labs(x = "value of        ")



# surface plot
ag <- load_obj("/home/sophie/sophTools/files/new_1/new.ag.rda")
plotSurface(d = 1, agEval = ag, m = "HWI", crit = "NRMSD", layer_type = "method", f = "median", highlight = "HWI", highlight_color = "#FF6666")

# cross-section
plotCS(d = 1, agEval = ag, m = "HWI", cross_section = "p", crit = "NRMSD", layer_type = "method", f = "median", highlight = "HWI", highlight_color = "#FF6666")
plotCS(d = 1, agEval = ag, m = "HWI", cross_section = "g", crit = "NRMSD", layer_type = "method", f = "median", highlight = "HWI", highlight_color = "#FF6666")

# heatmap
heatmapGrid2(d = 1, agEval = ag, m = "HWI", crit = "NRMSD", f = "median", colors = rev(c("#ff6666","#ff7a7a","#ff8d8d","#ffa1a1", "#ffb4b4", "#ffc8c8", "#ffdcdc")))

# data generation
simData <- load_obj("/home/sophie/sophTools/files/new_1/new.simData.rda")

plotXt(simData = simData, d = 1, return = "freq", cptwise = T)
plotXt(simData = simData, d = 2, return = "freq", cptwise = T)
plotXt(simData = simData, d = 3, return = "freq", cptwise = T)
plotXt(simData = simData, d = 4, return = "freq", cptwise = T)
plotXt(simData = simData, d = 5, return = "freq", cptwise = T, axisLabels = T)

# results (cubic splines bad) 
ag <- load_obj("/home/sophie/sophTools/files/new_1/new.ag.rda")
plotSurface(agEval = ag, d = 1, highlight = "HWI", highlight_color = "#33CEFF", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"), layer_type = "method", crit = "NRMSD")
plotCS(agEval = ag, d = 1, highlight = "HWI", highlight_color = "#33CEFF", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"), layer_type = "method", crit = "NRMSD", cross_section = "g")

# performance with psi
multiSurface(agEval = ag, d = 1:5 , m = c("EWMA","KAF","HWI"), highlight = "HWI", highlight_color = "#33CEFF", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"), layer_type = "method", crit = "NRMSD")
multiHeatmap(agEval = ag, d = 1:5, crit = "NRMSD", m = c("EWMA","KAF","HWI"), by = "method", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"))

# IQR
multiHeatmap(agEval = ag, d = 1:5, f = "iqr", crit = "abs_differences", m = c("EWMA","KAF","HWI"), by = "method", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"))

# showing dataset 3
plotSurface(agEval = ag, d = 3, f = "iqr", crit = "abs_differences", m = c("EWMA","KAF","HWI"),  highlight_color = "#33CEFF", layer_type = "method", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"))

# p and G cross sections
plotCS(agEval = ag, d = 3, f = "median", crit = "NRMSD", m = c("EWMA","KAF","HWI"), highlight_color = "#33CEFF", layer_type = "method", colors = c("#FFC300","#FF5733","#C70039","#900C3F", "#581845"))

# frequency shifting
ag_freq <- load_obj("/home/sophie/sophTools/files/investigate/shift2/agEval.shift.2.rda")
ag_orig <- load_obj("/home/sophie/sophTools/files/new_1/new.ag.rda")
sim_freq <- load_obj("/home/sophie/sophTools/files/investigate/shift/simData.shift.rda")
sim_orig <- load_obj("/home/sophie/sophTools/files/new_1/new.simData.rda")

p1 <- plotXt(simData = sim_orig, d = 4, return = "freq", cptwise = T, plot.title = T)
p2 <- plotXt(simData = sim_freq, d = 1, return = "freq", cptwise = T, plot.title = F, axisLabels = T)
grid.arrange(p1,p2, ncol = 1)

heatmapGrid2(ag = ag_orig, crit = "NRMSD", m = "HWI", f = "median", d = 4)
heatmapGrid2(ag = ag_freq, crit = "NRMSD", m = "HWI", f = "median", d = 4)

# Low period vs high period signal
t <- seq(1,80, length.out = 200)

mt <- (1/14)*t^2 

tt1 <- 50*sin(t/2)
tt2 <- 50*sin(5*t/2)

wt <- rnorm(length(t), sd = 40)

p1 <- makeOrigPlots(mt = mt, tt1 = tt1, wt = wt, axis.text.x = F, axis.title.x = F)
p2 <- makeOrigPlots(mt = mt, tt1 = tt2, wt = wt, axis.text.x = T)

grid.arrange(p1,p2, ncol = 1)


# plot CS thesis fix
ag <- load_obj("/home/sophie/sophTools/files/new_1")

multiCS(agEval = ag, crit = "NRMSD", f = "median", m = c("HWI", "KAF", "EWMA"), d = 1:5, cross_section = "g")


