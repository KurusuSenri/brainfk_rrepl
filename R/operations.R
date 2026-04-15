# increases memory pointer, or moves the pointer to the right 1 block
op_inc_ptr <- function(ctx) {
  ctx$ptr <- ctx$ptr + 1
  ctx$pc <- ctx$pc + 1
  return(ctx)
}

# decreases memory pointer, or moves the pointer to the left 1 block
op_dec_ptr <- function(ctx) {
  ctx$ptr <- ctx$ptr - 1
  ctx$pc <- ctx$pc + 1
  return(ctx)
}

# increases value stored at the block pointed to by the memory pointer
op_add_blk <- function(ctx) {
  ctx$mem[ctx$ptr] <- (ctx$mem[ctx$ptr] + 1) %% 256
  ctx$pc <- ctx$pc + 1
  return(ctx)
}

# decreases value stored at the block pointed to by the memory pointer
op_sub_blk <- function(ctx) {
  ctx$mem[ctx$ptr] <- (ctx$mem[ctx$ptr] - 1) %% 256
  ctx$pc <- ctx$pc + 1
  return(ctx)
}

# if block currently pointed to has zero value, jump forward to corresponding ]
op_jmp_fwd <- function(ctx) {
  if (ctx$mem[ctx$ptr] == 0) {
    ctx$pc <- ctx$jmp_tbl[ctx$pc]
  } else {
    ctx$pc <- ctx$pc + 1
  }
  return(ctx)
}

# if block currently pointed to has non zero value, jump backward to corresponding [
op_jmp_bkd <- function(ctx) {
  if (ctx$mem[ctx$ptr] != 0) {
    ctx$pc <- ctx$jmp_tbl[ctx$pc]
  } else {
    ctx$pc <- ctx$pc + 1
  }
  return(ctx)
}

# input 1 character
op_get_chr <- function(ctx) {
  chr <- readline()
  chr_ascii <- utf8ToInt(chr)[[1]]
  ctx$mem[ctx$ptr] <- chr_ascii
  ctx$pc <- ctx$pc + 1
  return(ctx)
}

# print 1 character to the console
op_put_chr <- function(ctx) {
  chr_ascii <- ctx$mem[ctx$ptr]
  chr <- intToUtf8(chr_ascii)
  print(chr)
  ctx$pc <- ctx$pc + 1
  return(ctx)
}
