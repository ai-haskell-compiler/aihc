











> module ParseMonad where

> import Control.Monad( ap )
> import Control.Applicative( Applicative(..) )
> import qualified Control.Monad.Fail as Fail

> data ParseResult a = OkP a | FailP String
> newtype P a = P (String -> Int -> ParseResult a)
> runP :: P a -> String -> Int -> ParseResult a
> runP (P f) = f


> lineP :: P Int
> lineP = P $ \_ l -> OkP l


> instance Monad P where
>	return m = P $ \ _ _ -> OkP m
>	m >>= k =  P $ \s l -> case runP m s l of
>		OkP a -> runP (k a) s l
>		FailP err -> FailP err

> instance Fail.MonadFail P where
>	fail s = P $ \ _ _ -> FailP s

> instance Functor P where
>   fmap f a = a >>= (return . f)

> instance Applicative P where
>   (<*>) = ap
>   pure = return
