module ColorEditorMenu.Component.State where

import Prelude

import Data.Maybe (Maybe(..))

import Color.Component.Query (ColorQuery(..))
import Halogen.Menu.Component.State (Menu(), makeMenu)

colorEditorMenu :: Menu (ColorQuery Unit)
colorEditorMenu = makeMenu
  [ { label: "Color"
    , submenu:
        [ { label: "Load color"
          , shortcutLabel: Nothing
          , value: LoadColor unit
          }
        , { label: "Save color"
          , shortcutLabel: Nothing
          , value: SaveColor unit
          }
        ]
    }
  , { label: "Edit"
    , submenu:
        [ { label: "Increase red"
          , shortcutLabel: Nothing
          , value: IncreaseRed unit
          }
        , { label: "Decrease red"
          , shortcutLabel: Nothing
          , value: DecreaseRed unit
          }
        , { label: "Increase green"
          , shortcutLabel: Nothing
          , value: IncreaseGreen unit
          }
        , { label: "Decrease green"
          , shortcutLabel: Nothing
          , value: DecreaseGreen unit
          }
        , { label: "Increase blue"
          , shortcutLabel: Nothing
          , value: IncreaseBlue unit
          }
        , { label: "Decrease blue"
          , shortcutLabel: Nothing
          , value: DecreaseBlue unit
          }
        ]
    }
  ]
