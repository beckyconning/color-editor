module ColorEditor.Component where

import Prelude

import Browser.WebStorage (WebStorage())
import Control.Apply ((*>))
import Control.Monad.Eff.Class (MonadEff)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct(), coproduct, left)
import Data.Generic (Generic, gEq, gCompare)
import Data.NaturalTransformation (Natural())
import Data.Void (Void(), absurd)

import Halogen
import Halogen.HTML.Core (ClassName(), className)
import Halogen.Component.ChildPath (ChildPath(), cpL, cpR)
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Handler as EH

import Halogen.Menu.Component (MenuQueryP(), MenuP(), SubmenuSlotAddress(), menuComponent)
import Halogen.Menu.Submenu.Component.Query (SubmenuQuery(..))
import Halogen.Menu.Component.Query (MenuQuery(..))

import ColorEditor.Component.State (ColorEditor())
import ColorEditor.Component.Query (ColorEditorQuery(..))

import Color.Component.State (Color(), white)
import Color.Component.Query (ColorQuery())
import Color.Component (colorComponent)

import ColorEditorMenu.Component.State (colorEditorMenu)

type ColorEditorMenu g = MenuP (ColorQuery Unit) g
type ColorEditorMenuQuery = MenuQueryP (ColorQuery Unit)

data ColorEditorMenuSlotAddressAddress = ColorEditorMenuSlotAddressAddress
data ColorSlotAddressAddress = ColorSlotAddressAddress

derive instance genericColorEditorMenuSlotAddressAddress :: Generic ColorEditorMenuSlotAddressAddress
instance eqColorEditorMenuSlotAddressAddress :: Eq ColorEditorMenuSlotAddressAddress where eq = gEq
instance ordColorEditorMenuSlotAddressAddress :: Ord ColorEditorMenuSlotAddressAddress where compare = gCompare

derive instance genericColorSlotAddressAddress :: Generic ColorSlotAddressAddress
instance eqColorSlotAddressAddress :: Eq ColorSlotAddressAddress where eq = gEq
instance ordColorSlotAddressAddress :: Ord ColorSlotAddressAddress where compare = gCompare

type ColorEditorChild g = Either (ColorEditorMenu g) Color
type ColorEditorChildQuery = Coproduct ColorEditorMenuQuery ColorQuery
type ColorEditorChildSlotAddressAddress = Either ColorEditorMenuSlotAddressAddress ColorSlotAddressAddress

type ColorEditorP g = InstalledState ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress
type ColorEditorQueryP = Coproduct ColorEditorQuery (ChildF ColorEditorChildSlotAddressAddress ColorEditorChildQuery)

cpColorEditorMenu :: forall g. ChildPath (ColorEditorMenu g) (ColorEditorChild g) ColorEditorMenuQuery ColorEditorChildQuery ColorEditorMenuSlotAddressAddress ColorEditorChildSlotAddressAddress
cpColorEditorMenu = cpL

cpColor :: forall g. ChildPath Color (ColorEditorChild g) ColorQuery ColorEditorChildQuery ColorSlotAddressAddress ColorEditorChildSlotAddressAddress
cpColor = cpR

colorEditorComponent :: forall g eff. (MonadEff (webStorage :: WebStorage | eff) g, Functor g) => Component (ColorEditorP g) ColorEditorQueryP g
colorEditorComponent = parentComponent' render eval peek
  where

  render :: ColorEditor -> ParentHTML (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress
  render _ =
      H.div
        [ E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> action DismissAll)
        , P.class_ colorEditorClass
        ]
        [ H.div
            [ P.class_ menuClass ]
            [ H.slot' cpColorEditorMenu ColorEditorMenuSlotAddressAddress \_ ->
                { component: menuComponent
                , initialState: installedState colorEditorMenu
                }
            ]
        , H.slot' cpColor ColorSlotAddressAddress \_ ->
            { component: colorComponent
            , initialState: white
            }
        ]

  colorEditorClass :: ClassName
  colorEditorClass = className "ce"

  menuClass :: ClassName
  menuClass = className "ce-menu"

  eval :: Natural ColorEditorQuery (ParentDSL ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress)
  eval (DismissAll next) =
    query' cpColorEditorMenu ColorEditorMenuSlotAddressAddress (left $ action DismissSubmenu) *> pure next

  peek :: forall a. (ChildF ColorEditorChildSlotAddressAddress ColorEditorChildQuery) a -> ParentDSL ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress Unit
  peek (ChildF _ q) = coproduct peekMenu (const (pure unit)) q

  peekMenu :: forall a. ColorEditorMenuQuery a -> ParentDSL ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress Unit
  peekMenu = coproduct (const (pure unit)) peekSubmenu

  peekSubmenu :: forall a. (ChildF SubmenuSlotAddress (SubmenuQuery (ColorQuery Unit))) a -> ParentDSL ColorEditor (ColorEditorChild g) ColorEditorQuery ColorEditorChildQuery g ColorEditorChildSlotAddressAddress Unit
  peekSubmenu (ChildF _ (SelectSubmenuItem q _)) = query' cpColor ColorSlotAddressAddress q *> pure unit

