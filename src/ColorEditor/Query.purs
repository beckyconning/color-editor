module ColorEditor.Query where

import Data.Const (Const())
import Data.Void (Void())

newtype ColorEditorQuery a = DismissAll a
