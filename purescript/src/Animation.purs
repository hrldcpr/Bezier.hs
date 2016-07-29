module Animation where

import Prelude
import Control.Monad.Eff (Eff)

type Milliseconds = Number

foreign import data ANIMATION :: !
foreign import data AnimationId :: *

foreign import requestAnimationFrame :: forall eff a. (Milliseconds -> Eff (animation :: ANIMATION | eff) a) -> Eff (animation :: ANIMATION | eff) AnimationId
foreign import cancelAnimationFrame :: forall eff. AnimationId -> Eff (animation :: ANIMATION | eff) Unit
