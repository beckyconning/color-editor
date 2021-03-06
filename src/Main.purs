module Main where

import Prelude

import Browser.WebStorage (WebStorage())
import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)

import Halogen
import Halogen.Util (appendToBody)

import ColorEditor.Component (colorEditorComponent)

main :: Eff (HalogenEffects (webStorage :: WebStorage)) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI colorEditorComponent (installedState unit)
  appendToBody app.node
