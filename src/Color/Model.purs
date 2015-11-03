module Color.Model where

import Prelude

import Browser.WebStorage (WebStorage(), localStorage, setItem, getItem)
import Control.Apply ((*>))
import Control.Monad.Eff (Eff())
import Css.Stylesheet (Css())
import Data.Array ((!!), drop)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Regex (match, noFlags, regex)
import Data.Traversable (sequence)

import qualified Css.Color (Color(..), rgb, white, black, clamp) as Css
import qualified Data.Int (fromString) as I

data Color = RGB Int Int Int

instance showColor :: Show Color where
  show (RGB r g b) = "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"

increment :: Int
increment = 15

increaseRed :: Color -> Color
increaseRed (RGB r g b) = RGB (Css.clamp (r + increment)) g b

decreaseRed :: Color -> Color
decreaseRed (RGB r g b) = RGB (Css.clamp (r - increment)) g b

increaseGreen :: Color -> Color
increaseGreen (RGB r g b) = RGB r (Css.clamp (g + increment)) b

decreaseGreen :: Color -> Color
decreaseGreen (RGB r g b) = RGB r (Css.clamp (g - increment)) b

increaseBlue :: Color -> Color
increaseBlue (RGB r g b) = RGB r g (Css.clamp (b + increment))

decreaseBlue :: Color -> Color
decreaseBlue (RGB r g b) = RGB r g (Css.clamp (b - increment))

average :: Color -> Int
average (RGB r g b) = (r + g + b) / 3

black :: Color
black = RGB 0 0 0

white :: Color
white = RGB 255 255 255

neutralVisibleForegroundColor :: Color -> Color
neutralVisibleForegroundColor color | average color < 128 = white
neutralVisibleForegroundColor color | otherwise = black

toCssColor :: Color -> Css.Color
toCssColor (RGB r g b) = Css.rgb r g b

fromString :: String -> Maybe Color
fromString string = RGB <$> (intArray !! 0) <*> (intArray !! 1) <*> (intArray !! 2)
  where
  expression = regex """rgb\((\d{1,3}),(\d{1,3}),(\d{1,3})\)""" noFlags

  stringArray :: Array String
  stringArray = drop 1 $ fromMaybe [] $ sequence $ fromMaybe [] $ match expression string

  intArray :: Array Int
  intArray = fromMaybe [] $ sequence $ I.fromString <$> stringArray

save :: forall eff. Color -> Eff (webStorage :: WebStorage | eff) Unit
save = setItem localStorage "color" <<< show

load :: forall eff. Eff (webStorage :: WebStorage | eff) (Maybe Color)
load = getItem localStorage "color" >>= ((>>= fromString) >>> pure)

