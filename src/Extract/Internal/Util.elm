module Extract.Internal.Util exposing (..)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


msequence : List (Maybe a) -> Maybe (List a)
msequence =
    List.foldr (Maybe.map2 (::)) (Just [])


mtraverse : (a -> Maybe b) -> List a -> Maybe (List b)
mtraverse f =
    List.foldr (\x -> Maybe.map2 (::) (f x)) (Just [])
