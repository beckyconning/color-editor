module ColorEditor.Query where

import Data.Const (Const())
import Data.Void (Void())

data ColorEditorQuery a = DismissAll a | Init a

