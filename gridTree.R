# library(grid)
# library(tree)
# library(dplyr)
# library(colorspace)

# User-called function
grid_tree <- function(tree_in,
                      vp = viewport(name = "treeCanvas",
                                    gp=gpar(fontsize=10)),
                      color_by = NULL,
                      new_page = TRUE) {
  
  if(inherits(tree_in, "singlenode"))
    stop("Cannot plot a single node tree")
  if(!inherits(tree_in, "tree"))
    stop("First argument must be of class tree")
  if(is.null(tree_in$frame))
    stop("Tree data frame is null")
  if(nrow(tree_in$frame) < 1)
    stop("Tree data frame is empty")
  
  tree_df <- tree_in$frame
  tree_splits <- unlist(tree_df$splits)
  tree_df <- cbind(tree_df, tree_splits)
  
  if (is.numeric(tree_df$yval)) {
    tree_df$yval <- round(tree_df$yval, digits=2)
  }
  
  if (new_page) {
    grid.newpage()
  }
  
  pushViewport(vp)
  grid_grow(tree_df)
  popViewport()
}

# Recursively build tree
grid_grow <- function(df, node=1,
                      vp) {
  
  if (missing(vp)) {
    vp <- viewport(x=unit(0.1, "npc"),
                   y=unit(0.5, "npc"),
                   width=unit(0.1, "npc"),
                   height=unit(0.5,"npc"))
  }
  
  node_details <- get_node_details(df=df, node=node)
  
  string_length <- longest_string_length(node_details$var,
                                         node_details$high_label,
                                         node_details$low_label)
  string_height <- stringHeight(node_details$var)
  
  vp$width <- unit.pmax(vp$width, string_length*1.4) # empirically determined multiplier
  vp$height <- unit.pmax(vp$height, string_height*6)
  
  pushViewport(vp)
  
  if (is.na(node_details$var)) {
    invisible()
  }
  else if (node_details$var == "<leaf>") {
    grid_leaf(yval=node_details$yval)
  }
  else {
    grid_branch(var = node_details$var,
                high_label = node_details$high_label,
                low_label = node_details$low_label)
    
    currentvp <- current.viewport()
    
    vp_down <- viewport(x = unit(1, "npc") + 0.5*currentvp$width,
                        y = unit(0, "npc"),
                        width = currentvp$width,
                        height = currentvp$height,
                        gp=gpar(),
                        name = "downChild")
    grid_grow(df, node*2, vp=vp_down)
    popViewport()
    
    vp_up <- viewport(x = unit(1, "npc") + 0.5*currentvp$width,
                      y = unit(1, "npc"),
                      width = currentvp$width,
                      height = currentvp$height,
                      gp=gpar(),
                      name = "upChild")
    grid_grow(df, node*2+1, vp=vp_up)
    popViewport()
  }
}

# Create terminal node
grid_leaf <- function(fill = "white",
                      yval = NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.5,y=0.5)
  grid.circle(r=unit(0.1, "cm"), gp=gpar(fill=fill))
  grid.text(yval,x=0.8, y=0.5)
}

# Create non-terminal node
grid_branch <- function(var = NULL,
                        high_label = NULL, 
                        low_label= NULL) {
  
  grid.move.to(x=0,y=0.5)
  grid.line.to(x=0.75,y=0.5)
  grid.move.to(x=0.75,y=1)
  grid.line.to(x=0.75,y=0)
  grid.line.to(x=1,y=0)
  grid.move.to(x=0.75,y=1)
  grid.line.to(x=1,y=1)
  
  grid_branch_annotate(var_label = var,
                       high_label = high_label, 
                       low_label = low_label)
}

# Annotate non-terminal node
grid_branch_annotate <- function(var_label,
                                 high_label,
                                 low_label) {
  
  # Perform arg check
  if (missing(var_label))
    stop("Variable label missing")
  if (!is.character(var_label))
    stop("Variable label must be a character")
  if (!is.character(high_label))
    stop(paste0("High label for", var_label,
                "must be a character"))
  if(!is.character(low_label))
    stop(paste0("Low label for", var_label,
                "must be character"))
  
  # Determine longest input string
  max_string <- longest_string(var_label, high_label, low_label)
  
  # Layout to vertically align text
  vplay <- grid.layout(1, 3,
                       widths=unit(c(1, 1, 0.25),
                                   c("null", "strwidth", "npc"),
                                   list(NULL, max_string, NULL)))
  
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
  
  grid.text(high_label, just="left")
  
  popViewport()
  pushViewport(viewport(layout.pos.row=3))
  
  grid.text(var_label)
  
  popViewport(3)
  
  pushViewport(viewport(layout.pos.row=2))
  
  # Layout low bound in bottom half
  vplay4 <- grid.layout(2, 1,
                        heights=unit(c(1,1),
                                     c("null", "line")))
  
  pushViewport(viewport(layout=vplay4))
  
  pushViewport(viewport(layout.pos.row=2))
  
  grid.text(low_label, just="left")
  
  popViewport(6)
}

# Get critical node parameters
get_node_details <- function(df, node=1) {
  
  list(var = as.character(df[as.character(node), ]$var),
       high_label = as.character(df[as.character(node), ]$cutright),
       low_label = as.character(df[as.character(node), ]$cutleft),
       yval = as.character(df[as.character(node), ]$yval))
}

# Determine length of longest input string
longest_string_length <- function(x, y, z) {
  
  string_list <- c(x, y, z)
  max_string <- string_list[nchar(string_list)==max(nchar(string_list))]
  if (length(max_string) > 1) {
    max_string = max_string[[1]]
  }
  max_string <- paste0(max_string, " ") # add margin
  stringWidth(max_string)
}

# Determine longest input string
longest_string <- function(x, y, z) {
  string_list <- c(x, y, z)
  max_string <- string_list[nchar(string_list)==max(nchar(string_list))]
  paste0(max_string, " ") # add margin
}

create_color_scale <- function(yvals) {
  
  nval <- length(yvals)
  
  lims <- range(yvals)
  
  yvals_perc <- (yvals - yvals[1])/diff(lims)
  
  yvals_index <- floor(nval*yvals)
  yvals_index[yvals_index == 0] <- NA_integer_
  
  cols <- sequential_hcl(nval)
  
  data.frame(yval = yvals, col = cols[yvals_index])
}