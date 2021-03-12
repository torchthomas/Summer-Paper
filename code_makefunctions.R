# SCRIPT TO CREATE FUNCTIONS FOR SUMMER PAPER 2020
# AUTHOR: THOMAS DEVINE
# currdir: "C:/Users/tdevine/Desktop/2020 Summer Paper/Analysis"
# CREATED: (when I made my code neat) 8.20.2020

#functions
{
    # str_smallest = function(s){ min(nchar(as.character(s))) } 
    all.na = function(x){ # used to get the biggest dataset with NO missing values, I use it to check columns at a various aggregate-level (i.e., I look at when I lose tables at more granular levels))
        allmisscols <- apply(x,2, function(x)all(is.na(x)));  
        colswithallmiss <-names(allmisscols[allmisscols>0]);    
        print("the columns with all values missing");    
        print(colswithallmiss)
    }
    head.list <- function(obj, n = 6L, ...)
    { # All credit to Harold Pimentel, taken from: https://gist.github.com/pimentel/256fc8c9b5191da63819. Exquisite function
        stopifnot(length(n) == 1L)
        origN <- n
        n <- if (n < 0L)
            max(length(obj) + n, 0L)
        else min(n, length(obj))
        lapply(obj[seq_len(n)], function(x)
        {
            tryCatch({
                head(x, origN, ...)
            }, error = function(e) {
                x
            })
        })
    }
    
    readInPackages = function(needPacks){
        packsInstalling <- needPacks[!needPacks %in% installed.packages()]
        for(lib in packsInstalling) install.packages(lib, dependencies = TRUE)
        sapply(needPacks, require, character=TRUE)
    }
    library(zoo)
    
    na.locf2 <- function(x) na.locf(x, na.rm = FALSE)
    
    set_plot_dimensions <- function(width_choice, height_choice) {
        options(repr.plot.width=width_choice, repr.plot.height=height_choice)
    }
}