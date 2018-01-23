# library(grid)
# library(tree)

grid.leaf <- function(fill = NULL) {
  
  grid.circle()
}

grid.branch <- function(var = NULL,
                        high.label = NULL, 
                        low.label= NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.5,y=0.5)
  grid.move.to(x=0.5,y=1)
  grid.line.to(x=0.5,y=0)
  grid.line.to(x=1,y=0)
  grid.move.to(x=0.5,y=1)
  grid.line.to(x=1,y=1)
  
  grid.branch.annotate(var.label = var,
                       high.label = high.label, 
                       low.label = low.label)
}

grid.branch.annotate <- function(var.label,
                                 high.label,
                                 low.label) {
  
  # Perform arg check
  if (missing(var.label))
    stop("Variable label missing")
  if (!is.character(var.label))
    stop("Variable label must be a character")
  if (!is.character(high.label))
    stop(paste0("High label for", var.label,
                "must be a character"))
  if(!is.character(low.label))
    stop(paste0("Low label for", var.label,
                "must be character"))
  
  # Determine longest input string
  string.list <- c(var.label, high.label, low.label)
  max.string <- string.list[nchar(string.list)==max(nchar(string.list))]
  max.string = paste0(max.string, " ") # add margin
  
  # Layout to vertically align text
  vplay <- grid.layout(1, 3,
                       widths=unit(c(1, 1, 0.5),
                                   c("null", "strwidth", "npc"),
                                   list(NULL, max.string, NULL)))
  
  pushViewport(viewport(layout=vplay, name = "top"))
  pushViewport(viewport(layout.pos.col=2))
  
  # Divide text column in half
  vplay2 <- grid.layout(2, 1)
  
  pushViewport(viewport(layout=vplay2))
  pushViewport(viewport(layout.pos.row=1))
  
  # Layout high bound and variable name in top half
  vplay3 <- grid.layout(3, 1,
                        heights=unit(c(1, 1, 1),
                                     c("line", "null", "line")))
  
  pushViewport(viewport(layout=vplay3))
  pushViewport(viewport(layout.pos.row=1))
  
  grid.text(high.label)
  
  popViewport()
  pushViewport(viewport(layout.pos.row=3))
  
  grid.text(var.label)
  
  popViewport(3)
  
  pushViewport(viewport(layout.pos.row=2))
  
  # Layout low bound in bottom half
  vplay4 <- grid.layout(2, 1,
                        heights=unit(c(1,1),
                                     c("null", "line")))
  
  pushViewport(viewport(layout=vplay4))
  
  pushViewport(viewport(layout.pos.row=2))
  
  grid.text(low.label)
  
  popViewport(6)
}

# To layout
# grid.grow <- function(var,
#                       high.label,
#                       low.label) {
#   
#   if (is.null(var))
#     stop()
#   else if (var == "<leaf>")
#     grid.leaf()
#   else
#     grid.branch(var = var,
#                 high.label = high.label,
#                 low.label = low.label)
#   
# }

get.node.details <- function(df, node=1) {
  
  list(var = as.character(df[as.character(node), ]$var),
       high.label = as.character(df[as.character(node), ]$cutright),
       low.label = as.character(df[as.character(node), ]$cutleft))
}

grid.grow <- function(df, node=1, width) {
  
  node.details <- get.node.details(df=df, node=node)
  
  if (is.na(node.details$var)) {
    invisible()
  }
  else if (node.details$var == "<leaf>") {
    #pushViewport(vp)
    grid.leaf()
  }
  else {
    #pushViewport(vp)
    grid.branch(var = node.details$var,
                high.label = node.details$high.label,
                low.label = node.details$low.label)
    
    currentvp <- current.viewport()
    
    pushViewport(viewport(x = unit(1, "npc") + 0.5*width,
                          y = unit(0, "npc"),
                          width = currentvp$width,
                          height = currentvp$height,
                          name = "downChild"))
    
    grid.grow(df, node + 1, width=width)
    popViewport()

    pushViewport(viewport(x = unit(1, "npc") + 0.5*width,
                          y = unit(1, "npc"),
                          width = currentvp$width,
                          height = currentvp$height,
                          name = "upChild"))
    
    grid.grow(df, node + 2, width=width)
    popViewport()
    # grid.grow(df, node + 2,
    #           vp = viewport(x = currentvp$x + currentvp$width,
    #                         y = currentvp$y + 0.5*currentvp$height,
    #                         width = currentvp$width,
    #                         height = 0.5*currentvp$height))
  }
}

grid.tree <- function(tree.in,
                      vp = viewport(name = "treeCanvas")) {
  
  if(inherits(tree.in, "singlenode"))
    stop("Cannot plot a single node tree")
  if(!inherits(tree.in, "tree"))
    stop("First argument must be of class tree")
  if(is.null(tree.in$frame))
    stop("Tree data frame is null")
  if(nrow(tree.in$frame) < 1)
    stop("Tree data frame is empty")
  
  tree.df <- tree.in$frame
  tree.splits <- unlist(tree.test$splits)
  tree.df <- cbind(tree.df, tree.splits)
  
  pushViewport(vp)
  
  width <- current.viewport()$width
  width <- 0.1*convertWidth(width, "cm")
  
  pushViewport(viewport(x=unit(0.05, "npc"), 
                        width = width, 
                        height=unit(0.5, "npc"),
                        name = "trunk"))
  
  grid.grow(tree.df, width = width)
  
  popViewport(2)
  # grid.grow(var = as.character(tree.df$var[2]),
  #           high.label = as.character(tree.df$cutright[[2]]),
  #           low.label = as.character(tree.df$cutleft[[2]]))
}