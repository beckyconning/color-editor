module ColorEditorMenu.Model where

import Prelude

import Color.Query (ColorQuery(..))
import Halogen.Menu.Model (Menu(), makeMenu)

colorEditorMenu :: Menu (ColorQuery Unit)
colorEditorMenu = makeMenu
  [ { label: "Color"
    , submenu:
        [ { label: "Load color"
          , value: LoadColor unit
          }
        , { label: "Save color"
          , value: SaveColor unit
          }
        ]
    }
  , { label: "Edit"
    , submenu:
        [ { label: "Increase red"
          , value: IncreaseRed unit
          }
        , { label: "Decrease red"
          , value: DecreaseRed unit
          }
        , { label: "Increase green"
          , value: IncreaseGreen unit
          }
        , { label: "Decrease green"
          , value: DecreaseGreen unit
          }
        , { label: "Increase blue"
          , value: IncreaseBlue unit
          }
        , { label: "Decrease blue"
          , value: DecreaseBlue unit
          }
        ]
    }
  ]
