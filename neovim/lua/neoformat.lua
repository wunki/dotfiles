-- Automatically format Rust buffers
vim.api.nvim_exec([[
augroup fmt
  autocmd!
  autocmd BufWritePre *.rs try | undojoin | Neoformat | catch /^Vim\%((\a\+)\)\=:E790/ | finally | silent Neoformat | endtry
augroup END
]], true)
