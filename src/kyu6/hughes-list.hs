import Data.Semigroup
import Data.Monoid

newtype Hughes a = Hughes ([a] -> [a])

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes l = Hughes (l++)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons a (Hughes h) = Hughes ((a:) . h)

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)

instance Semigroup (Hughes a) where
  (<>) = mappend

instance Monoid (Hughes a) where
  mempty = Hughes id
  (Hughes h) `mappend` (Hughes k) = Hughes (h. k)
  
------------------------------------------------------------

snocDumb :: Hughes a -> a -> Hughes a
snocDumb l a = mkHughes (runHughes l ++ [a])

snoc :: Hughes a -> a -> Hughes a
snoc (Hughes h) a = Hughes (h . (a:))
