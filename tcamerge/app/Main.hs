{-# OPTIONS_GHC -fno-warn-tabs #-}
{-# LANGUAGE ScopedTypeVariables,FlexibleInstances,UnicodeSyntax #-}

module Main where

main :: IO ()
main = do
	putStrLn "TEST"

--infixl 4 <*>

class Applicative p where
	pure  :: a -> p a
	(<*>) :: p (a -> b) -> p a -> p b

class Functor f where
	fmap :: (a -> b) -> f a -> f b

class Category y where
	id  :: y a a
	(.) :: y b c -> y a b -> y a c

class Monad m where
	return :: a -> m a
	(>>=)  :: m a -> (a -> m b) -> m b
