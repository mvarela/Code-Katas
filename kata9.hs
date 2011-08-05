module Kata9 where
import qualified Data.Map as M
import Data.Maybe

-- Items have a code, a description, a price per unit, and a pricing function

data Item = Item { itemCode :: Int
                 , description :: String
                 , unitPrice :: Float
                 , pricing :: Item -> Float -> Float
                 } 

instance Show Item where
  show i = code ++ " " ++ desc ++ " " ++  perunit
    where code = show $ itemCode i
          desc = show $ description i
          perunit = show $ unitPrice i
          
instance Eq Item where 
  x == y = itemCode x == itemCode y
  
  

-- Registers store a list of items, their quantities, and the way to price them

data RegisterItem = RegisterItem { item :: Item
                                 , quantity :: Float -- quantities can be real numbers, too
                                 } deriving (Show, Eq)


-- Shooping lists are just list of items

type ShoppingList = [RegisterItem]

-- Normally, we just multiply the unit price times the amount of units

defaultPricing :: Item -> Float -> Float
defaultPricing item amount  = ip * amount
  where ip  = unitPrice item
        
        
-- Given an number of units of a certain item, we use its pricing function        
-- to calculate the subtotal for this item

calculatePrice :: RegisterItem -> Float
calculatePrice r = pricing i i q
  where i = item r
        q = quantity r
        
type RegisterMap = M.Map Int RegisterItem


-- We consolidate the shopping list and calculate total quantities
-- If an item code was not present in the map, we add it. 
-- If it was, we add the existing quantity to the actual quantity

ringItems :: ShoppingList -> RegisterMap
ringItems = foldr (\ri acc -> sumItem ri acc) M.empty

sumItem :: RegisterItem -> RegisterMap -> RegisterMap
sumItem ri m = 
  let old_item = M.lookup (itemCode $ item ri) m
      new_item = RegisterItem (item ri) (quantity ri + quantity (fromJust old_item))
  in
   case old_item of { Nothing -> M.insert (itemCode $ item ri) ri m ; otherwise -> M.update (\_ -> Just new_item) (itemCode $ item ri) m }

getTotal :: RegisterMap -> Float
getTotal = M.fold (\ri acc -> calculatePrice ri + acc) 0 

----------------------------------------------------------------------
-- Some testing data to use in ghci
----------------------------------------------------------------------

--items
salad = Item 1 "Salad" 0.79 defaultPricing
bread = Item 2 "Bread" 1.5 defaultPricing
cheese = Item 3 "Cheese" 2.3 defaultPricing
chocolate_discount = Item 4 "Chocolate - 25% off " 2 (\i q -> 0.75 * q * unitPrice i)
tuna_discount = Item 5 "Tuna - 3x2" 1 threeForTwo


-- 3x2 promotion pricing
threeForTwo :: Item -> Float -> Float
threeForTwo i q = (2*(fromIntegral $ div (floor q)  3) + (fromIntegral $ mod (floor q) 3)) * unitPrice i

-- shorthand constructor for RegisterItems
qri i q = RegisterItem i q

-- testing lists
lista1 = [qri chocolate_discount 4]
lista1' = [qri tuna_discount 4]
lista2 = [qri cheese 2, qri cheese 3]        
lista3 = [qri salad 1, qri cheese 2.2, qri chocolate_discount 4, qri salad 2, qri bread 1, qri bread 1, qri cheese 1.3, qri tuna_discount 4] 


