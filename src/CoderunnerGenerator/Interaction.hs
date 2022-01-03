#if defined(windows)

module CoderunnerGenerator.Interaction
  (
    module CoderunnerGenerator.Interaction.Windows
  )
where

import CoderunnerGenerator.Interaction.Windows

#elif defined(linux)

module CoderunnerGenerator.Interaction
  (
    module CoderunnerGenerator.Interaction.Linux
  )
where

import CoderunnerGenerator.Interaction.Linux

#else

#error Unsupported Platform

#endif

