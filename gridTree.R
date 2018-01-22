# library(grid)
# library(tree)

grid.leaf <- function(fill = NULL) {
  
  grid.circle()
}

grid.branch <- function(var = NULL,
                        splits.cutdown = NULL, 
                        splits.cutup = NULL, 
                        yprob.down = NULL, 
                        yprob.up = NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.5,y=0.5)
  grid.move.to(x=0.5,y=1)
  grid.line.to(x=0.5,y=0)
  grid.line.to(x=1,y=0)
  grid.move.to(x=0.5,y=1)
  grid.line.to(x=1,y=1)
  
  grid.branch.annotate()
}

grid.branch.annotate <- function() {
  
  # Determine longest input string
  string.list <- c("High", "Low", "Variable")
  max.string <- string.list[nchar(string.list)==max(nchar(string.list))]
  max.string = paste0(max.string, " ") # add margin
  
  # Layout to vertically align text
  vplay <- grid.layout(1, 3,
                       widths=unit(c(1, 1, 0.5),
                                   c("null", "strwidth", "npc"),
                                   list(NULL, max.string, NULL)))
  
  pushViewport(viewport(layout=vplay))
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
  
  grid.text("High")
  
  popViewport()
  pushViewport(viewport(layout.pos.row=3))
  
  grid.text("Variable")
  
  popViewport(3)
  
  pushViewport(viewport(layout.pos.row=2))
  
  # Layout low bound in bottom half
  vplay4 <- grid.layout(2, 1,
                        heights=unit(c(1,1),
                                     c("null", "line")))
  
  pushViewport(viewport(layout=vplay4))
  
  pushViewport(viewport(layout.pos.row=2))
  
  grid.text("Low")
  
  popViewport(0)
}

grid.tree <- function(tree.in) {
  
  if(!class(tree.in) == "tree")
    stop("First argument must be of class tree")
  if(is.null(tree.in[[1]]))
    stop("Tree data frame is null")
  if(nrow(tree.in[[1]]) < 1)
    stop("Tree data frame is empty")
  
  tree.df <- tree.in[[1]]
}