module ColorEditor.Component where

import Prelude

import Browser.WebStorage (WebStorage())
import Control.Apply ((*>))
import Control.Monad.Aff.AVar (AVAR())
import Control.Monad.Aff (Aff())
import Control.Monad.Eff.Ref (REF())
import Data.Const (Const(..))
import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Functor.Coproduct (Coproduct(), coproduct, left, right)
import Data.Generic (Generic, gEq, gCompare)
import Data.NaturalTransformation (Natural())
import Data.Void (Void(), absurd)
import Keyboard
import Data.KeyCombo

import Halogen
import Halogen.HTML.Core (ClassName(), className)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as EH

import DOM as DOM
import DOM.Node.Types as DOM
import DOM.HTML as DOM
import DOM.HTML.Window as DOM
import DOM.HTML.Types as DOM

import Halogen.Menu.Component (MenuQueryP(), MenuP(), SubmenuSlotAddress(), menuComponent)
import Halogen.Menu.Submenu.Query (SubmenuQuery(..))
import Halogen.Menu.Query (MenuQuery(..))

import ColorEditor.Model (ColorEditor())
import ColorEditor.Query (ColorEditorQuery(..))

import Color.Model (Color(), white)
import Color.Query (ColorQuery(..))
import Color.Component (colorComponent)

import ColorEditorMenu.Model (colorEditorMenu)

type X eff = Aff (keyboard :: KEYBOARD, ref :: REF, webStorage :: WebStorage, avar :: AVAR, dom :: DOM.DOM| eff)

type ColorEditorMenu g = MenuP (ColorQuery Unit) g
type ColorEditorMenuQuery = MenuQueryP (ColorQuery Unit)

data ColorEditorMenuSlotAddress = ColorEditorMenuSlotAddress
data ColorSlotAddress = ColorSlotAddress

derive instance genericColorEditorMenuSlotAddress :: Generic ColorEditorMenuSlotAddress
instance eqColorEditorMenuSlotAddress :: Eq ColorEditorMenuSlotAddress where eq = gEq
instance ordColorEditorMenuSlotAddress :: Ord ColorEditorMenuSlotAddress where compare = gCompare

derive instance genericColorSlotAddress :: Generic ColorSlotAddress
instance eqColorSlotAddress :: Eq ColorSlotAddress where eq = gEq
instance ordColorSlotAddress :: Ord ColorSlotAddress where compare = gCompare

type ColorEditorChild g = Either (ColorEditorMenu g) Color
type ColorEditorChildQuery = Coproduct ColorEditorMenuQuery ColorQuery
type ColorEditorChildSlotAddress = Either ColorEditorMenuSlotAddress ColorSlotAddress

type ColorEditorP g = InstalledState ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddress
type ColorEditorQueryP = Coproduct ColorEditorQuery (ChildF ColorEditorChildSlotAddress ColorEditorChildQuery)

cpColorEditorMenu :: forall g. ChildPath (ColorEditorMenu g) (ColorEditorChild g) ColorEditorMenuQuery ColorEditorChildQuery ColorEditorMenuSlotAddress ColorEditorChildSlotAddress
cpColorEditorMenu = cpL

cpColor :: forall g. ChildPath Color (ColorEditorChild g) ColorQuery ColorEditorChildQuery ColorSlotAddress ColorEditorChildSlotAddress
cpColor = cpR

colorEditorComponent :: forall eff. Component (ColorEditorP (X eff)) ColorEditorQueryP (X eff)
colorEditorComponent = parentComponent' render eval peek
  where

  render :: ColorEditor -> ParentHTML (ColorEditorChild (X eff)) ColorEditorQuery ColorEditorChildQuery (X eff) ColorEditorChildSlotAddress
  render _ =
      H.div
        [ E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> action DismissAll)
        , P.class_ colorEditorClass
        , P.initializer \_ -> action Init
        ]
        [ H.div
            [ P.class_ menuClass ]
            [ H.slot' cpColorEditorMenu ColorEditorMenuSlotAddress \_ ->
                { component: menuComponent
                , initialState: installedState colorEditorMenu
                }
            ]
        , H.slot' cpColor ColorSlotAddress \_ ->
            { component: colorComponent
            , initialState: white
            }
        ]

  colorEditorClass :: ClassName
  colorEditorClass = className "ce"

  menuClass :: ClassName
  menuClass = className "ce-menu"

  eval :: Natural ColorEditorQuery (ParentDSL ColorEditor (ColorEditorChild (X eff)) ColorEditorQuery ColorEditorChildQuery (X eff) ColorEditorChildSlotAddress)
  eval (DismissAll next) =
    query' cpColorEditorMenu ColorEditorMenuSlotAddress (left $ action DismissSubmenu) *> pure next
  eval (Init next) = do
    document <- liftH $ liftEff' $ DOM.window >>= DOM.document <#> DOM.htmlDocumentToDocument
    liftH $ subscribe $ eventSource (onKeyUp document) \e ->
      let i = readKeyboardEvent e in
      if i.ctrlKey && i.keyCode == 83 then do
        keyboardEventPreventDefault e
        pure saveColor
      else if i.ctrlKey && i.keyCode == 76 then do
        keyboardEventPreventDefault e
        pure loadColor
      else
        pure dismissSubmenu
    pure next

  dismissSubmenu = ChildF (Left ColorEditorMenuSlotAddress) (left $ left $ DismissSubmenu unit)
  saveColor = ChildF (Right ColorSlotAddress) (right $ SaveColor unit)
  loadColor = ChildF (Right ColorSlotAddress) (right $ LoadColor unit)

  s :: Char
  s = fromCharCode 83

  l :: Char
  l = fromCharCode 76

  peek :: forall a. (ChildF ColorEditorChildSlotAddress ColorEditorChildQuery) a -> ParentDSL ColorEditor (ColorEditorChild (X eff)) ColorEditorQuery ColorEditorChildQuery (X eff) ColorEditorChildSlotAddress Unit
  peek (ChildF _ q) = coproduct peekMenu (const (pure unit)) q

  peekMenu :: forall a. ColorEditorMenuQuery a -> ParentDSL ColorEditor (ColorEditorChild (X eff)) ColorEditorQuery ColorEditorChildQuery (X eff) ColorEditorChildSlotAddress Unit
  peekMenu = coproduct (const (pure unit)) peekSubmenu

  peekSubmenu :: forall a. (ChildF SubmenuSlotAddress (SubmenuQuery (ColorQuery Unit))) a -> ParentDSL ColorEditor (ColorEditorChild (X eff)) ColorEditorQuery ColorEditorChildQuery (X eff) ColorEditorChildSlotAddress Unit
  peekSubmenu (ChildF _ (SelectSubmenuItem q _)) = query' cpColor ColorSlotAddress q *> pure unit
