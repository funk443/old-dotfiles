#!/bin/bash

for dir in ~/.var/app/*
do
  cp -rv ~/dotfiles/.config/fontconfig ${dir}/config
done
