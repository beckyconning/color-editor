module Keyboard where

import Prelude
import Control.Monad.Aff (Aff())
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console
import Control.Monad.Eff.Ref
import Control.Monad.Free (Free())
import Data.Functor.Coproduct (left)
import Data.Functor (($>))
import Data.KeyCombo
import Data.Maybe (Maybe(..))
import Data.Monoid
import Data.Traversable (sequence)

import Halogen as H
import Halogen.Component as H
import Data.Function (Fn2(), runFn2, Fn3(), runFn3, Fn4(), runFn4, Fn5(), runFn5)
import Data.Map as M

import DOM (DOM())
import DOM.Node.Types as DOM
import DOM.HTML as DOM
import DOM.HTML.Window as DOM
import DOM.HTML.Types as DOM

newtype KeyboardState = KeyboardState (Maybe KeyCombo)

instance semigroupKeyboardState :: Semigroup KeyboardState where
  append (KeyboardState Nothing) ks = ks
  append ks (KeyboardState Nothing) = ks
  append (KeyboardState (Just kc)) (KeyboardState (Just kc')) = KeyboardState (Just (append kc kc'))

instance monoidKeyboardState :: Monoid KeyboardState where
  mempty = KeyboardState Nothing

foreign import data KEYBOARD :: !
foreign import addEventListenerImpl :: forall ev eff a . Fn3 String (ev -> Eff (keyboard :: KEYBOARD | eff) a) DOM.Document (Eff (keyboard :: KEYBOARD | eff) Unit)

hole :: forall a. a
hole = Unsafe.Coerce.unsafeCoerce ""

type KeyboardEventR =
  { keyCode :: Int
  , ctrlKey :: Boolean
  , altKey :: Boolean
  , metaKey :: Boolean
  }

foreign import data KeyboardEvent :: *
foreign import readKeyboardEvent :: KeyboardEvent -> KeyboardEventR
foreign import keyboardEventPreventDefault :: forall eff. KeyboardEvent -> Eff (keyboard :: KEYBOARD | eff) Unit

onKeyDown
  :: forall eff
   . DOM.Document
   -> (KeyboardEvent -> Eff (keyboard :: KEYBOARD | eff) Unit)
   -> Eff (keyboard :: KEYBOARD | eff) Unit
onKeyDown document fn = runFn3 addEventListenerImpl "keydown" fn document

onKeyUp
  :: forall eff
   . DOM.Document
  -> (KeyboardEvent -> Eff (keyboard :: KEYBOARD | eff) Unit)
  -> Eff (keyboard :: KEYBOARD | eff) Unit
onKeyUp document fn = runFn3 addEventListenerImpl "keyup" fn document

onKeyCombo
  :: forall eff
   . DOM.Document
  -> (KeyboardState -> Eff (keyboard :: KEYBOARD, ref :: REF | eff) Unit)
  -> Eff (keyboard :: KEYBOARD, ref :: REF | eff) Unit
onKeyCombo doc listener = do
  ref <- newRef mempty
  onKeyUp doc \e -> do
    keyboardEventPreventDefault e
    readRef ref >>= listener
    modifyRef ref (const mempty)
  onKeyDown doc \e -> do
    keyboardEventPreventDefault e
    modifyRef ref (<> (keyCodeToKeyboardState (readKeyboardEvent e).keyCode))

type KeyboardEffects eff =
  ( keyboard :: KEYBOARD
  , avar :: AVAR
  , dom :: DOM
  | eff
  )

keyCodeToKeyboardState :: Int -> KeyboardState
keyCodeToKeyboardState = KeyboardState <<< Just <<< keyCodeToKeyCombo
