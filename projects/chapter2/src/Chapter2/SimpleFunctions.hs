module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

lst1 +++ lst2 = if null lst1 {- check emptyness -}
                then lst2     -- base case
                else (head lst1) : (tail lst1 +++ lst2)

reverse2 :: [t] -> [t]
reverse2 list = if null list
                then []
                else reverse2 (tail list) +++ [head list]

maxmin :: Ord a => [a] -> (a, a)
maxmin list = let h = head list
              in if null (tail list)
                 then (h, h)
                 else ( if h > t_max then h else t_max
                      , if h < t_min then h else t_min )
                      where t     = maxmin (tail list)
                            t_max = fst t
                            t_min = snd t

clientName :: Client -> String
clientName client = case client of
                      GovOrg  name                        -> name
                      Company name _ _ _                  -> name
                      Individual (Person fName lName _) _ -> fName ++ " " ++ lName

companyName :: Client -> Maybe String
companyName client = case client of
                       Company name _ _ _ -> Just name
                       _                  -> Nothing

fibonacci :: Integer -> Integer
fibonacci n = case n of
                0 -> 0
                1 -> 1
                _ -> fibonacci (n-1) + fibonacci (n-2)

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

