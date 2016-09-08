module Color.Component where

import Prelude

import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (class MonadEff)
import Data.Maybe (maybe)
import Browser.WebStorage (WebStorage())

import CSS.Font (color) as Css

import Halogen
import Halogen.HTML.Core (ClassName(), className)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.CSS.Indexed as C

import Color.Component.State (Color(), save, load, toCssColor, increaseRed, increaseGreen, increaseBlue, decreaseRed, decreaseGreen, decreaseBlue)
import Color.Component.Query (ColorQuery(..))

type ComponentEffects eff = HalogenEffects (webStorage :: WebStorage | eff)

colorComponent :: forall eff. Component Color ColorQuery (Aff (ComponentEffects eff))
colorComponent = component { render, eval }
  where

  render :: Color -> ComponentHTML ColorQuery
  render color =
    H.div
      [ P.class_ colorClass, C.style (Css.color $ toCssColor color) ]
      [ H.span_ [ H.text $ show color ] ]

  eval :: ColorQuery ~> ComponentDSL Color ColorQuery (Aff (ComponentEffects eff))
  eval (IncreaseRed next) = modify increaseRed *> pure next
  eval (DecreaseRed next) = modify decreaseRed *> pure next
  eval (IncreaseGreen next) = modify increaseGreen *> pure next
  eval (DecreaseGreen next) = modify decreaseGreen *> pure next
  eval (IncreaseBlue next) = modify increaseBlue *> pure next
  eval (DecreaseBlue next) = modify decreaseBlue *> pure next
  eval (LoadColor next) = (fromEff load >>= maybe (pure unit) (const >>> modify)) *> pure next
  eval (SaveColor next) = (get >>= (save >>> fromEff)) *> pure next

  colorClass :: ClassName
  colorClass = className "ce-color"

