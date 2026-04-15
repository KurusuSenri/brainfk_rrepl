list.files("R/", full.names = TRUE) |> sapply(source)

brainfk <- function(input_str){
  ctx <- init()
  ctx <- load_ins(ctx, tokenize_bf(input_str))
  ctx <- eval(ctx)
  return(ctx)
}

repl <- function(){
  ctx <- init()
  print("Enter 'q' to quit.")
  while(TRUE){
    input_str <- readline(">>> ")
    if (input_str == "q"){
      return(ctx)
    } else {
      ins <- tokenize_bf(input_str)
      if(length(ins) == 0){
        next
      }
      if(chk_bkt_map(ins) > 0){
        while(TRUE){
          ins <- c(ins, tokenize_bf(readline("+++ ")))
          map <- chk_bkt_map(ins)
          if (map == 0){
            break
          }
          if (map < 0){
            stop("ummapped brackets")
          }
        }
      } else if (chk_bkt_map(ins) < 0){
        stop("ummapped brackets")
      }
      ctx <- load_ins(ctx, ins)
      ctx <- eval(ctx)
      print(ctx)
    }
  }
}

rrepl_helloworld <- "
++++++++++[>+++++ +++++<-] ignoreabcdefg>++++.---.+++++ ++..+++.>>>+++[>+++++ +++++<-]
>++.<<<<+++++ +++.----- ---.+++.----- -.----- ---.>>+++++ +++++.
"