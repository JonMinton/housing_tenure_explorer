# adjust the paired colour scheme so reds and greens are not next to each other
adjusted_paired <- brewer.pal(12, "Paired")[c(1,2,3,4,9,10,7,8,5,6,11,12)]


divlist  <- c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
names(divlist) <- divlist
quallist <- c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
names(quallist) <- quallist
seqlist  <- c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd",
              "PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")
names(seqlist) <- seqlist		

brew_pals <-c(divlist,quallist,seqlist)


palette_options <- c("New" = "adjusted_paired", "Cube YF" = "cubeyf", brew_pals)