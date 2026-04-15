# initialize an empty context
init <- function() {
  ctx <- list(
    mem = integer(16),
    ptr = 1,
    pc = 1,
    ins = c(),
    jmp_tbl = integer(0)
  )
  return(ctx)
}

# append new instructions to context and generate new jump table
load_ins <- function(ctx, ins){
  ins <- c(ctx$ins, ins)
  ctx <- list(
    mem = ctx$mem,
    ptr = ctx$ptr,
    pc = ctx$pc,
    ins = ins,
    jmp_tbl = gen_jmp_tbl(ins)
  )
  return(ctx)
}

# evaluate context till the end of instructions
eval <- function(ctx) {
  ops <- list(
    "+" = op_add_blk,
    "-" = op_sub_blk,
    ">" = op_inc_ptr,
    "<" = op_dec_ptr,
    "," = op_get_chr,
    "." = op_put_chr,
    "[" = op_jmp_fwd,
    "]" = op_jmp_bkd
  )
  
  while(ctx$pc <= length(ctx$ins)) {
    token <- ctx$ins[ctx$pc]
    ctx <- ops[[token]](ctx)
  }
  
  return(ctx)
}

# tokenize input string to brainfk instructions
tokenize_bf <- function(input_str) {
  valid_tokens <- c(">", "<", "+", "-", ".", ",", "[", "]")
  raw_vec <- strsplit(input_str, "")[[1]]
  clean_vec <- raw_vec[raw_vec %in% valid_tokens]
  return(clean_vec)
}

# check whether square brackets match 
chk_bkt_map <- function(ins) {
  n <- 0
  for (tkn in ins) {
    if (tkn == "[") {
      n <- n + 1
    } else if (tkn == "]") {
      n <- n - 1
    }
  }
  return(n)
}

# generate square brackets mapping
gen_jmp_tbl <- function(code) {
  jmp_tbl <- integer(length(code))
  stack <- integer()
  for(i in 1:length(code)){
    tkn <- code[[i]]
    if (tkn == "[") {
      stack <- c(stack, i)
    }
    else if(tkn == "]"){
      left_idx <- tail(stack, 1)
      stack <- head(stack, -1)
      jmp_tbl[left_idx] <- i
      jmp_tbl[i] <- left_idx
    }
  }
  return(jmp_tbl)
}

# print user friendly registers
dbg_print_reg <- function(ctx) {
  cat(sprintf("ptr: %04d val: %04d pc: %04d\n", ctx$ptr, ctx$mem[ctx$ptr], ctx$pc))
}

# print user friendly memory
dbg_print_mem <- function(ctx) {
  mem_len <- length(ctx$mem)
  num_row <- mem_len %/% 8
  cat("mem:\n")
  for(row in 1:num_row){
    cat(sprintf("[%04d] ", (row - 1) * 8))
    for(col in 1:8){
      cat(sprintf("%04d ", ctx$mem[(row - 1) * 8 + col]))
    }
    cat("\n")
  }
}

# print instructions and jump table
dbg_print_ins <- function(ctx) {
  cat("ins:\n")
  cat(ctx$ins, "\n", sep = "")
  
  cat("jmp_tbl:\n")
  active_indices <- which(ctx$jmp_tbl != 0)
  for (i in active_indices) {
    if (i < ctx$jmp_tbl[i]) {
      cat(sprintf("[%04d]:[%04d]\n", i - 1, ctx$jmp_tbl[i] - 1))
    }
  }
}

# print all context
dbg_print <- function(ctx) {
  cat("\n")
  dbg_print_reg(ctx)
  dbg_print_mem(ctx)
  dbg_print_ins(ctx)
}