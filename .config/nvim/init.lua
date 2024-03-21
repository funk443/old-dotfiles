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
}
require("lazy").setup(plugins, opts)

vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.cc = '80'
vim.opt.autoindent = true
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.tabstop = 2
vim.opt.shiftwidth = 0
vim.opt.termguicolors = true
vim.opt.background = "light"

vim.cmd.syntax "off"
