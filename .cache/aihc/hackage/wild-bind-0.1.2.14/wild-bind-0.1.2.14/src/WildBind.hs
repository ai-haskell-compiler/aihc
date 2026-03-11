-- |
-- Module: WildBind
-- Description: WildBind main module
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
--
module WildBind
       ( module WildBind.Binding,
         -- | Defines 'Binding' and many functions to build it.

         module WildBind.FrontEnd,
         -- | Defines 'FrontEnd', an interface between 'Binding' and a
         -- desktop environment.

         module WildBind.Exec,
         -- | Defines functions to combine 'Binding' and 'FrontEnd'
         -- into an executable action. You can customize its behavior
         -- via 'Option'.

         module WildBind.Description,
         -- | Defines 'ActionDescription'.

         module WildBind.Input.NumPad
         -- | Defines input symbol types for number pad keys.

-- * Support modules
--
--
-- | The following modules are not re-exported from this module.
--
-- - "WildBind.Seq": support module to build a binding to key
--   sequences. /Since: 0.1.1.0/
       ) where


import           WildBind.Binding
import           WildBind.Description
import           WildBind.Exec
import           WildBind.FrontEnd
import           WildBind.Input.NumPad
