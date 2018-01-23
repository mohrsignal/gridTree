# library(grid)
# library(tree)

grid.leaf <- function(fill = NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.5,y=0.5)
  grid.circle(r=0.1, gp=gpar(fill=fill))
}

grid.branch <- function(var = NULL,
                        high.label = NULL, 
                        low.label= NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.75,y=0.5)
  grid.move.to(x=0.75,y=1)
  grid.line.to(x=0.75,y=0)
  grid.line.to(x=1,y=0)
  grid.move.to(x=0.75,y=1)
  grid.line.to(x=1,y=1)
  
  grid.branch.annotate(var.label = var,
                       high.label = high.label, 
                       low.label = low.label)
}

# Determine longest input string
longest.string.length <- function(x, y, z) {
  
  string.list <- c(x, y, z)
  max.string <- string.list[nchar(string.list)==max(nchar(string.list))]
  max.string <- paste0(max.string, " ") # add margin
  stringWidth(max.string)
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
  max.string <- longest.string(var.label, high.label, low.label)
  
  # Layout to vertically align text
  vplay <- grid.layout(1, 3,
                       widths=unit(c(1, 1, 0.25),
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
  
  grid.text(high.label, just="left")
  
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
  
  grid.text(low.label, just="left")
  
  popViewport(6)
}

get.node.details <- function(df, node=1) {
  
  list(var = as.character(df[as.character(node), ]$var),
       high.label = as.character(df[as.character(node), ]$cutright),
       low.label = as.character(df[as.character(node), ]$cutleft))
}

resize.viewport <- function(vp, string.length) {
  
  currentvp.width <- convertWidth(current.viewport()$width,
                                  "cm")
  string.width <- convertWidth(string.length,
                               "cm")
  
  if (currentvp.width < string.width)
    vp$width <- string.length
  
  vp
}

grid.grow <- function(df, node=1,
                      vp) {
  
  if (missing(vp)) {
    vp <- viewport(x=unit(0.1, "npc"),
                   y=unit(0.5, "npc"),
                   width=unit(0.1, "npc"),
                   height=unit(0.5,"npc"))
  }
  
  node.details <- get.node.details(df=df, node=node)
  
  string.length <- longest.string.length(node.details$var,
                                         node.details$high.label,
                                         node.details$low.label)
  
  vp$width <- unit.pmax(vp$width, string.length*1.4) # empirically determined multiplier
  
  pushViewport(vp)
  
  if (is.na(node.details$var)) {
    invisible()
  }
  else if (node.details$var == "<leaf>") {
    grid.leaf()
  }
  else {
    grid.branch(var = node.details$var,
                high.label = node.details$high.label,
                low.label = node.details$low.label)
    
    currentvp <- current.viewport()
    
    vp.down <- viewport(x = unit(1, "npc") + 0.5*currentvp$width,
                          y = unit(0, "npc"),
                          width = currentvp$width,
                          height = currentvp$height,
                          name = "downChild")
    grid.grow(df, node + 1, vp=vp.down)
    popViewport()
    
    vp.up <- viewport(x = unit(1, "npc") + 0.5*currentvp$width,
                          y = unit(1, "npc"),
                          width = currentvp$width,
                          height = currentvp$height,
                          name = "upChild")
    grid.grow(df, node + 2, vp=vp.up)
    popViewport()
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
  grid.grow(tree.df)
  popViewport()
}