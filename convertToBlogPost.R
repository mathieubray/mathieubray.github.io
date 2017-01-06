
convert.to.blog.post <- function(filename){
  
  # Print out user message
  message(paste0("Converting '", filename, "'"))
  
  # Load packages
  require(knitr, quietly=TRUE, warn.conflicts=FALSE)
  require(lubridate, quietly=TRUE, warn.conflicts=FALSE)
  
  # Get full file path
  blog <- paste0(getwd(),"/_rmd/",filename,".Rmd")
  
  # Read in blog post, isolate header, and add layout tag
  blog.content <- readLines(blog)
  blog.header <- which(substr(blog.content, 1, 3) == '---')
  blog.amended <- c(blog.content[1:(blog.header[2]-1)],"layout: post",blog.content[5:length(blog.content)])
  
  # Get new file path
  blog.new <- paste0(getwd(),"/_posts/",today(),"-",filename,".md")
  
  # Set options for output
  opts_knit$set(out.format='markdown') 
  opts_knit$set(base.url = "/")
  opts_chunk$set(fig.path = 'plots/')           
  opts_chunk$set(fig.width  = 8.5,
                 fig.height = 5.25)
  
  # Convert blog post
  invisible(knit(text=blog.amended, output=blog.new))
}




