data MyMonadFail m a = MyFail String | MyValue a deriving (Show) 
 
instance Functor (MyMonadFail m) where 
    fmap _ (MyFail err) = MyFail err 
    fmap f (MyValue x) = MyValue (f x) 
 
instance Applicative (MyMonadFail m) where 
    pure = MyValue 
    MyFail err <*> _ = MyFail err 
    _ <*> MyFail err = MyFail err 
    MyValue f <*> MyValue x = MyValue (f x) 
 
instance Monad (MyMonadFail m) where 
    return = pure 
    MyFail err >>= _ = MyFail err 
    MyValue x >>= f = f x 
 
doubleIfEven :: Int -> MyMonadFail m Int 
doubleIfEven x = if even x 
                    then return (x * 2) 
                    else MyFail "no no no way"
