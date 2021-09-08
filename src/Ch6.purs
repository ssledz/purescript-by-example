module Ch6 where

import Prelude
import Math

-- Define a Show instance for Point. 

newtype Point = Point { x :: Number, y :: Number }

instance pointShow :: Show Point where
  show (Point p) = "Point(x = " <> (show p.x) <> ", y = " <> (show p.y) <> ")"
  
-- Define a Show instance for Complex

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance complexShow :: Show Complex where
  show (Complex p) = (show p.real) <> (sign p.imaginary) <> (show $ abs p.imaginary) <> "i"   
    where
      sign :: Number -> String
      sign x | x < 0.0 = "-"
             | otherwise = "+" 

newtype Complex' = Complex'
  { real :: Number
  , imaginary :: Number
  }

derive newtype instance complexShow' :: Show Complex'




