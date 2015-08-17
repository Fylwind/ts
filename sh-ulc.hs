{-# LANGUAGE GADTs #-}
module Main where
import Data.Foldable
import System.IO

data Literal where
  Str :: String -> Literal
  deriving (Eq, Read, Ord, Show)

data Expr where
  App :: Expr -> Expr -> Expr
  Lam :: (Expr -> Expr) -> Expr
  Lit :: Literal -> Expr

shellEscape :: String -> String
shellEscape s = "'" ++ s ++ "'" -- FIXME

primitivePrefix :: String
primitivePrefix = "_ts_"

primitive :: String -> String
primitive = (primitivePrefix ++)

primRet :: String
primRet = primitive "r"

primPush :: String
primPush = primitive "push"

primConcat :: String
primConcat = primitive "concat"

primApply :: String
primApply = primitive "apply"

primPrint :: String
primPrint = primitive "print"

shRuntime runtime =
  unlines
  [ ""
  ]

toShell :: Expr -> String
toShell (Lit (Str s)) = primRet ++ "=" ++ shellEscape s
toShell (App f x) =
  unlines
  [ f'
  , primPush ++ " \"$" ++ primRet ++ "\""
  , x'
  , primApply
  ]
  where f' = toShell f
        x' = toShell x
toShell (Lam f) =
  unlines
  [ undefined
  ]

litFun name = Lit (Str (name ++ " \"$1\""))

main :: IO ()
main = do
  hSetNewlineMode stdout noNewlineTranslation
  shRuntime <- readFile "sh-rt.sh"
  putStr shRuntime
  putStr . toShell $
    App (litFun primPrint)
    (App (App (litFun primConcat)
          (Lit (Str "hello "))) (Lit (Str "world")))

-- time bash -c '. ./sh-rt.sh; for i in {1..10000}; do : _ts_rt_expr x 1 + 1; done'
