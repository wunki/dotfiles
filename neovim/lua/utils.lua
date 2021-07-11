local M = {}

function M.is_buffer_empty()
   -- Check whether the current buffer is empty
   return vim.fn.empty(vim.fn.expand('%:t')) == 1
end

function M.has_width_gt(cols)
   -- Check if the windows width is greater than a given number of columns
   return vim.fn.winwidth(0) / 2 > cols
end

-- file extension specific tabbing
-- vim.cmd([[autocmd Filetype python setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4]])

-- blankline config
M.blankline = function()
   vim.g.indentLine_enabled = 1
   vim.g.indent_blankline_char = '·'

   vim.g.indent_blankline_filetype_exclude = { 'help', 'terminal', 'dashboard' }
   vim.g.indent_blankline_buftype_exclude = { 'terminal' }

   vim.g.indent_blankline_show_trailing_blankline_indent = false
   vim.g.indent_blankline_show_first_indent_level = false
end

return M
