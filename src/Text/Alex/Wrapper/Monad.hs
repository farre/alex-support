module Text.Alex.Wrapper.Monad (
  module Text.Alex.Wrapper.Generic,
  module Text.Alex.Wrapper.Monad,
  module Text.Alex.Wrapper.Utils
) where

import Data.Word

import Text.Alex.Wrapper.Generic
import Text.Alex.Wrapper.Utils

type Byte = Word8

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (p,c,bs,s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (p,c,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c 
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))

data AlexAcc a user
  = AlexAccNone
  | AlexAcc a
  | AlexAccSkip
  | AlexAccPred a   (AlexAccPred user) (AlexAcc a user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc a user)

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

data AlexState = AlexState {
  alex_pos   :: !AlexPosn,  -- position at current input location
  alex_inp   :: String,     -- the current input
  alex_chr   :: !Char,      -- the character before the input
  alex_bytes :: [Byte],
  alex_scd   :: !Int        -- the current startcode
}

newtype Alex a = Alex {
  unAlex :: AlexState -> Either String (AlexState, a)
}

instance Monad Alex where
  m >>= k  = Alex $ \s -> case unAlex m s of 
                            Left msg -> Left msg
                            Right (s',a) -> unAlex (k a) s'
  return a = Alex $ \s -> Right (s,a)

runAlex :: String -> Alex a -> Either String a
runAlex input (Alex f) 
   = case f (AlexState {alex_pos = alexStartPos,
                        alex_inp = input,       
                        alex_chr = '\n',
                        alex_bytes = [],
                        alex_scd = 0}) of Left msg -> Left msg
                                          Right ( _, a ) -> Right a

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_chr=c,alex_bytes=bs,alex_inp=inp} -> 
        Right (s, (pos,c,bs,inp))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,bs,inp)
 = Alex $ \s -> 
     case s { alex_pos=pos, alex_chr=c, alex_bytes=bs, alex_inp=inp } of
       s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError message = Alex $ \s -> Left message

alexGetStartCode :: Alex Int
alexGetStartCode = Alex $ \s@AlexState{alex_scd=sc} -> Right (s, sc)

alexSetStartCode :: Int -> Alex ()
alexSetStartCode sc = Alex $ \s -> Right (s{alex_scd=sc}, ())


