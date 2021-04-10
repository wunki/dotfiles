require'gitsigns'.setup({
  signs = {
    add          = {hl = 'diffAdded', text = '┃'},
    change       = {hl = 'diffRemoved', text = '┃'},
    delete       = {hl = 'diffRemoved', text = '_'},
    topdelete    = {hl = 'diffRemoved', text = '‾'},
    changedelete = {hl = 'diffRemoved', text = '~'},
  },
})
