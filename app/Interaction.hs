#if defined(Windows)

module Interaction
  (
    module InteractionWindows
  )
where

import InteractionWindows

#elif defined(Linux)

module Interaction
  (
    module InteractionLinux
  )
where

import InteractionLinux

#else

#error Unsuported platform

#endif

