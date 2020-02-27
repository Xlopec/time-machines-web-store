module Chapter4.TypeClasses where

class Priceable p where
  price :: p -> Double
  
class Nameable n where
  name :: n -> String