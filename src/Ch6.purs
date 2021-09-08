-- https://book.purescript.org/chapter6.html

module Ch6 where

import Math
import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)
import Safe.Coerce (coerce)

-- Define a Show instance for Point. 

newtype Point = Point { x :: Number, y :: Number }

instance showPoint :: Show Point where
  show (Point p) = "Point(x = " <> (show p.x) <> ", y = " <> (show p.y) <> ")"
  
-- Define a Show instance for Complex

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex p) = (show p.real) <> (sign p.imaginary) <> (show $ abs p.imaginary) <> "i"   
    where
      sign :: Number -> String
      sign x | x < 0.0 = "-"
             | otherwise = "+" 

newtype Complex' = Complex'
  { real :: Number
  , imaginary :: Number
  }

-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md
derive newtype instance showComplex' :: Show Complex'

-- Derive an Eq instance for Complex

derive newtype instance eqComplex :: Eq Complex

-- Define a Semiring instance for Complex

derive instance newTypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  add  :: Complex -> Complex -> Complex
  add = over2 Complex (+)
  zero :: Complex
  zero = wrap { real:0.0, imaginary:0.0 }
  mul  :: Complex -> Complex -> Complex
  mul = over2 Complex mul'
    where
      mul' a b =
        let
          real = a.real * b.real - a.imaginary * b.imaginary
          img = a.real * b.imaginary + a.imaginary * b.real  
        in {real: real, imaginary : img}
  one  :: Complex
  one = wrap { real:1.0, imaginary:0.0 }

  -- Derive (via newtype) a Ring instance for Complex

derive newtype instance ringComplex :: Ring Complex 

-- write an Eq instance for the type NonEmpty a

data NonEmpty a = NonEmpty a (Array a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showInstance :: Show a => Show (NonEmpty a) where
  show = genericShow

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq :: NonEmpty a -> NonEmpty a -> Boolean
  eq (NonEmpty x xs) (NonEmpty y ys) = eq x y && eq xs ys 
  
  





