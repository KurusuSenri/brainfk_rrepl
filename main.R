list.files("R/", full.names = TRUE) |> sapply(source)

interpret <- function(input_str) {
  ctx <- init()
  ctx <- load_ins(ctx, tokenize_bf(input_str))
  ctx <- eval(ctx)
  dbg_print(ctx)
}

repl <- function() {
  ctx <- init()
  cat("[q]uit, [r]egisters, [i]nstructions, [p]rint_all\n")
  while (TRUE) {
    input_str <- readline(">>> ")
    if (input_str == "q") {
      dbg_print(ctx)
      break
    } else if (input_str == "r"){
      dbg_print_reg(ctx)
    } else if (input_str == "m"){
      dbg_print_mem(ctx)
    } else if (input_str == "i"){
      dbg_print_ins(ctx)
    } else if (input_str == "p"){
      dbg_print(ctx)
    } else {
      ins <- tokenize_bf(input_str)
      if (length(ins) == 0) {
        next
      }
      if (chk_bkt_map(ins) > 0) {
        while (TRUE) {
          ins <- c(ins, tokenize_bf(readline("+++ ")))
          map <- chk_bkt_map(ins)
          if (map == 0) {
            break
          }
          if (map < 0) {
            stop("ummapped brackets")
          }
        }
      } else if (chk_bkt_map(ins) < 0) {
        stop("ummapped brackets")
      }
      ctx <- load_ins(ctx, ins)
      ctx <- eval(ctx)
      dbg_print(ctx)
    }
  }
}

rrepl_helloworld <- "
++++++++++[>+++++ +++++<-] ignoreabcdefg>+++
+.---.+++++ ++..+++.>>>+++[>+++++ +++++<-]
>++.<<<<+++++ +++.----- ---.+++.----- -.----- ---.>>+++++ +++
++.++++
"
