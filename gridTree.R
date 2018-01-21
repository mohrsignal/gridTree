# library(grid)

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