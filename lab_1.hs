
data MyMaybe a = MyNothing | MyJust a deriving (Show)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
    pure = MyJust
    MyNothing <*> _ = MyNothing
    _ <*> MyNothing = MyNothing
    (MyJust f) <*> mx = fmap f mx

instance Monad MyMaybe where
    return = MyJust
    MyNothing >>= _ = MyNothing
    MyJust x >>= f = f x

data List a = Empty | Cons a (List a)

-- instance Monad List where
--     return x = Cons x Empty
--     Empty >>= _ = Empty
--     (Cons x xs) >>= f = concatLists (f x) (xs >>= f)
--       where
--         concatLists Empty ys = ys
--         concatLists (Cons x xs) ys = Cons x (concatLists xs ys)

instance Monad List where 
    Empty >>= _ = Empty
--    Cons x xs >>= f = Cons (f x) (xs >>= f)
    Cons x xs >>= f = makeList (f x) (xs >>= f)
                    where makeList Empty ys = ys
                          makeList (Cons x xs) ys = Cons x (makeList ys xs)

instance Functor List where
    fmap _ Empty = Empty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Empty
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    Cons f fs <*> xs = concatLists (fmap f xs) (fs <*> xs)
      where
        concatLists Empty ys = ys
        concatLists (Cons y ys) zs = Cons y (concatLists ys zs)

main :: IO ()
main = return ()
