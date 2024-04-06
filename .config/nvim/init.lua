local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

plugins = {
  "tpope/vim-commentary",
  "junegunn/vim-easy-align"
}
require("lazy").setup(plugins, opts)

vim.api.nvim_create_autocmd(
  { "BufWritePre" },
  { command = "%s/\\s\\+$//e" }
)

vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.tw = 80
vim.opt.cc = '+0'
vim.opt.autoindent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 0
vim.opt.termguicolors = true
vim.opt.background = "light"
vim.opt.autochdir = true
vim.g.netrw_browsex_viewer = "xdg-open"

vim.cmd.syntax "off"
