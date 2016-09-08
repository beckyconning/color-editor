module Color.Component where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Eff.Class (MonadEff)
import Data.Maybe (maybe)
import Data.NaturalTransformation (Natural())
import Browser.WebStorage (WebStorage())

import Css.Font (color) as Css

import Halogen
import Halogen.HTML.Core (ClassName(), className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed as C

import Color.Component.State (Color(), save, load, toCssColor, increaseRed, increaseGreen, increaseBlue, decreaseRed, decreaseGreen, decreaseBlue)
import Color.Component.Query (ColorQuery(..))

colorComponent :: forall g eff. (MonadEff (webStorage :: WebStorage | eff) g) => Component Color ColorQuery g
colorComponent = component render eval
  where

  render :: Color -> ComponentHTML ColorQuery
  render color =
    H.div
      [ P.class_ colorClass, C.style (Css.color $ toCssColor color) ]
      [ H.span_ [ H.text $ show color ] ]

  eval :: Natural ColorQuery (ComponentDSL Color ColorQuery g)
  eval (IncreaseRed next) = modify increaseRed *> pure next
  eval (DecreaseRed next) = modify decreaseRed *> pure next
  eval (IncreaseGreen next) = modify increaseGreen *> pure next
  eval (DecreaseGreen next) = modify decreaseGreen *> pure next
  eval (IncreaseBlue next) = modify increaseBlue *> pure next
  eval (DecreaseBlue next) = modify decreaseBlue *> pure next
  eval (LoadColor next) = (liftEff' load >>= maybe (pure unit) (const >>> modify)) *> pure next
  eval (SaveColor next) = (get >>= (save >>> liftEff')) *> pure next

  colorClass :: ClassName
  colorClass = className "ce-color"

