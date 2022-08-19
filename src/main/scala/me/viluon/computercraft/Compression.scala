package me.viluon.computercraft

import me.viluon.computercraft.compression.Lzw
import me.viluon.lua.computercraft.CCLibrary

trait Compression extends CCLibrary
  with Lzw
