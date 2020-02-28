multiSurface <- function(agEval, 
                         d=1:length(agEval),
                         m=names(agEval[[1]][[1]][[1]]),
                         crit,
                         layer_type = "method",
                         f = "median",
                         highlight = "HWI",
                         highlight_color = "#FA4032",
                         colors = c("#EAECEE","#D5D8DC","#ABB2B9","#808B96","#566573","#2C3E50")
                         ){
 
  stopifnot(length(crit) == 1)
  
  D <- length(d)
  M <- length(m)
  
  # BUILD LEGEND ###########################################################################
  z_list <- compileMatrix(agEval=agEval)[[f]]
  
  data_list_names = names(z_list[[1]][[1]])[d]
  method_list_names <- names(z_list[[1]])[names(z_list[[1]]) %in% m]
  
  if(layer_type == "method"){
  colorList <- colorRampPalette(colors)(M)
  
  colorListMatch <- colorList[1:M]
  names(colorListMatch) <- method_list_names
  colorListMatch[[highlight]] <- highlight_color
  
  nameSurface = method_list_names
  bound = M
  oth_bound = D
  }
  
  else if(layer_type == "dataset"){
  colorList <- colorRampPalette(colors)(D)
  
  colorListMatch <- colorList[1:D]
  names(colorListMatch) <- data_list_names
  colorListMatch[grepl(highlight, data_list_names)] <- highlight_color
  
  nameSurface = data_list_names
  bound = D
  oth_bound = M
  }
  
  manualLegend <- data.frame(color = as.character(colorListMatch), 
                             nameSurface = nameSurface, 
                             x = rep(0,bound), 
                             y = seq(0.20,0.80,length.out =bound), 
                             textcol = rep("black",bound),
                             stringsAsFactors = FALSE)
  
  theLegend <- plot_ly(data = manualLegend, showlegend = F) %>%
    
    add_markers(x = manualLegend$x,
                y = manualLegend$y, 
                size = 50, 
                name = manualLegend$nameSurface,
                color = I(manualLegend$color)) %>%
    
    add_text(x = manualLegend$x + 0.03,
             y = manualLegend$y,
             text = manualLegend$nameSurface,
             color = I(manualLegend$textcol),
             textposition = "right") %>% 
    
    layout(xaxis = list(title = "", 
                        showgrid = F, 
                        showticklabels = F,
                        zeroline = F,
                        range = c(-0.15,0.3)), 
           yaxis = list(title = "", 
                        showgrid = F, 
                        showticklabels = F,
                        zeroline = F,
                        range = c(0,1.05)))
  
  
  # GENERATE PLOTS #######################################################################################
  
  ps <- plotSurface(crit=crit, 
                    agEval = agEval, 
                    layer_type = layer_type,
                    f = f,
                    m = m, 
                    d = d,
                    highlight = highlight,
                    highlight_color = highlight_color,
                    colors = colors)
  
  titleList <- list()
  
  if(layer_type == "method"){
    for(i in 1:oth_bound){
      titleList[[i]] <- list(
        text = paste("Dataset ",gsub("D","",names(ps[[1]])[i]),sep=""),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 0.75,
        showarrow = FALSE
      )
    }
  }
  
  
  else if(layer_type == "dataset"){
    
    for(i in 1:oth_bound){
      titleList[[i]] <- list(
        text = gsub("Average","Avg", gsub("."," ", names(ps[[1]])[i],fixed = TRUE)),
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 0.75,
        showarrow = FALSE
      )
    }
  }
  
  plotList <- list()

  for(i in 1:oth_bound){
    plotList[[i]] <- ps[[1]][[i]] %>% layout(annotations = titleList[[i]])
  }
  
  nrows = round_any(oth_bound, 2, f = ceiling)/2
  
  p_string <- paste0("plotList[[",1:(oth_bound-1),"]],")
  p_string <- paste0(c("subplot(",p_string,"plotList[[",oth_bound,"]], theLegend, nrows = ",nrows,") %>%"), collapse = "")

  x1 <- rep(c(0,1/2), nrows)
  x2 <- rep(c(1/2,1), nrows)
  
  y1 <- rep(((nrows-1):0)/nrows, each = 2)
  y2 <- rep((nrows:1)/nrows, each = 2)
  
  scene_string <- paste0("scene",2:(oth_bound+1))
  scene_string <- c("scene",scene_string)
  
  
  l_string <- paste0("layout(title = crit,")
  
  l_string <- c(l_string, paste0(scene_string[1:oth_bound]," = list(domain = list(x = c(",x1[1:oth_bound],",",x2[1:oth_bound],"), y = c(",y1[1:oth_bound],",",y2[1:oth_bound],")),
                                 aspectmode = 'manual',
                                 aspectratio = list(x = 1, y= 1, z= 0.8),
                                 camera = list(eye = list(x = 1.65, y = -1.15, z = 0.30))),"))
  l_string <- c(l_string, paste0(scene_string[oth_bound+1], " = list(domain = list(x = c(",x1[oth_bound+1],",",x2[oth_bound+1],"), y = c(",y1[oth_bound+1],",",y2[oth_bound+1],"))))"))

  call <- paste0(c(p_string,l_string))
  
  return(eval(parse(text = call)))
}
