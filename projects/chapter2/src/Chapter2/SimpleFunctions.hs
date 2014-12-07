{-# LANGUAGE ViewPatterns #-}
module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

                
reverse2 :: [t] -> [t]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

maxmin :: Ord a => [a] -> (a, a)
maxmin [x]    = (x,x)
maxmin (x:xs) = ( if x > xs_max then x else xs_max
                , if x < xs_min then x else xs_min
                ) where (xs_max, xs_min) = maxmin xs

{-
maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min )
                      where t     = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t
-}

clientName :: Client -> String
clientName (GovOrg name)                         = name
clientName (Company name _ _ _)                  = name
clientName (Individual (Person fName lName _) _) = fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

responsibility :: Client -> String
responsibility (Company _ _ _ r) = r
responsibility _                 = "Unknown"

specialClient :: Client -> Bool
specialClient (clientName -> "Mr. Alejandro") = True
specialClient (responsibility -> "Director")  = True
specialClient _                               = False

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

ifibonacci :: Integer -> Maybe Integer
ifibonacci n | n< 0      = Nothing
ifibonacci 0             = Just 0
ifibonacci 1             = Just 1
ifibonacci n | otherwise = let (Just f1, Just f2) = (ifibonacci (n-1), ifibonacci (n-2))
                           in Just (f1 + f2)

binom _ 0          = 1
binom x y | x == y = 1
binom n k          = (binom (n-1) (k-1)) + (binom (n-1) k)

multipleOf :: Integer -> Integer -> Bool
multipleOf x y = (mod x y) == 0

specialMultiples :: Integer -> String
specialMultiples n
  | multipleOf n 2 = show n ++ " is multiple of 2"
  | multipleOf n 3 = show n ++ " is multiple of 3"
  | multipleOf n 5 = show n ++ " is multiple of 5"
  | otherwise      = show n ++ " is a beautiful number"
 

f :: Client -> String
f client = case client of
             Company _ _ (Person name _ _) "Boss" -> name ++ " is the boss"
             _                                    -> "There is no boss"

g :: Client -> String
g client = case client of
             Company _ _ (Person name _ _) pos ->
               case pos of "Boss" -> name ++ " is the boss"
             _                    -> "There is no boss"


(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 = case lst1 of
                  []   -> lst2
                  x:xs -> x:(xs +++ lst2)

sorted :: [Integer] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x : r@(y:_)) = x < y && sorted r


data Client = GovOrg     String
            | Company    String Integer Person String
            | Individual Person Bool
            deriving Show
data Person = Person String String Gender
            deriving Show
data Gender = Male | Female | Unknown
            deriving Show
data TimeMachine = TimeMachine String Integer String Bool Bool Float
                 deriving Show

