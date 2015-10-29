module Color.Query where

data ColorQuery a = IncreaseRed a
                  | DecreaseRed a
                  | IncreaseGreen a
                  | DecreaseGreen a
                  | IncreaseBlue a
                  | DecreaseBlue a
                  | LoadColor a
                  | SaveColor a

