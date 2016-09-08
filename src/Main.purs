module Main where

import Prelude

import Browser.WebStorage (WebStorage())
import Control.Monad.Eff (Eff())

import Halogen (HalogenEffects, parentState, runUI)
import Halogen.Util (awaitBody, runHalogenAff)

import ColorEditor.Component (colorEditorComponent)

main :: Eff (HalogenEffects (webStorage :: WebStorage)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI colorEditorComponent (parentState unit) body
