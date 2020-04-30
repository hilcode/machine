module Hilcode.Tuples where

class HasFirst a b where
    _1 :: a -> b

instance HasFirst (a, b) a where
    _1 (a, _) = a

instance HasFirst (a, b, c) a where
    _1 (a, _, _) = a

instance HasFirst (a, b, c, d) a where
    _1 (a, _, _, _) = a

instance HasFirst (a, b, c, d, e) a where
    _1 (a, _, _, _, _) = a

class HasSecond a b where
    _2 :: a -> b

instance HasSecond (a, b) b where
    _2 (_, b) = b

instance HasSecond (a, b, c) b where
    _2 (_, b, _) = b

instance HasSecond (a, b, c, d) b where
    _2 (_, b, _, _) = b

instance HasSecond (a, b, c, d, e) b where
    _2 (_, b, _, _, _) = b

class HasThird a b where
    _3 :: a -> b

instance HasThird (a, b, c) c where
    _3 (_, _, c) = c

instance HasThird (a, b, c, d) c where
    _3 (_, _, c, _) = c

instance HasThird (a, b, c, d, e) c where
    _3 (_, _, c, _, _) = c

class HasFourth a b where
    _4 :: a -> b

instance HasFourth (a, b, c, d) d where
    _4 (_, _, _, d) = d

instance HasFourth (a, b, c, d, e) d where
    _4 (_, _, _, d, _) = d

class HasFifth a b where
    _5 :: a -> b

instance HasFifth (a, b, c, d, e) e where
    _5 (_, _, _, _, e) = e
