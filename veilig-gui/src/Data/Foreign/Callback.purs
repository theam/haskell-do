module Data.Foreign.Callback

where

import Control.Monad.Eff

foreign import data Callback0 :: * -> *
foreign import data Callback1 :: * -> * -> *
foreign import data Callback2 :: * -> * -> * -> *
foreign import data Callback3 :: * -> * -> * -> * -> *
foreign import data Callback4 :: * -> * -> * -> * -> * -> *
foreign import data Callback5 :: * -> * -> * -> * -> * -> * -> *
foreign import data Callback6 :: * -> * -> * -> * -> * -> * -> * -> *
foreign import data Callback7 :: * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Callback8 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Callback9 :: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *
foreign import data Callback10:: * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> * -> *

foreign import callback0 :: forall z r. Eff z r -> Callback0 r

foreign import callback1 :: forall z r a. (a -> Eff z r) -> Callback1 a r

foreign import callback2 :: forall z r a b. (a -> b -> Eff z r) -> Callback2 a b r

foreign import callback3 :: forall z r a b c. (a -> b -> c -> Eff z r) -> Callback3 a b c r

foreign import callback4 :: forall z r a b c d. (a -> b -> c -> d -> Eff z r) -> Callback4 a b c d r

foreign import callback5 :: forall z r a b c d e. (a -> b -> c -> d -> e -> Eff z r) -> Callback5 a b c d e r

foreign import callback6 :: forall z r a b c d e f. (a -> b -> c -> d -> e -> f -> Eff z r) -> Callback6 a b c d e f r

foreign import callback7 :: forall z r a b c d e f g. (a -> b -> c -> d -> e -> f -> g -> Eff z r) -> Callback7 a b c d e f g r

foreign import callback8 :: forall z r a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h -> Eff z r) -> Callback8 a b c d e f g h r

foreign import callback9 :: forall z r a b c d e f g h i. (a -> b -> c -> d -> e -> f -> g -> h -> i -> Eff z r) -> Callback9 a b c d e f g h i r

foreign import callback10 :: forall z r a b c d e f g h i j. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Eff z r) -> Callback10 a b c d e f g h i j r

callback1_  eff = callback1  (\a -> eff)
callback2_  eff = callback2  (\a b -> eff)
callback3_  eff = callback3  (\a b c -> eff)
callback4_  eff = callback4  (\a b c d -> eff)
callback5_  eff = callback5  (\a b c d e -> eff)
callback6_  eff = callback6  (\a b c d e f -> eff)
callback7_  eff = callback7  (\a b c d e f g -> eff)
callback8_  eff = callback8  (\a b c d e f g h -> eff)
callback9_  eff = callback9  (\a b c d e f g h i -> eff)
callback10_ eff = callback10 (\a b c d e f g h i j -> eff)
