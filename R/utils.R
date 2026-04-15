# initialize an empty context
init <- function() {
  ctx <- list(
    mem = integer(64),
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
