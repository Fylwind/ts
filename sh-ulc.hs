{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}
module Main where
import Data.Foldable
import System.IO

data Expr where
  App :: Expr -> Expr -> Expr
  Lam :: (Expr -> Expr) -> Expr
  Lit :: String -> Expr
  Var :: String {- varname -} -> Expr
  Bnd :: Int -> Expr

-- newtype Shell (t :: ShellType) = Shell String

-- data ShellType = UString | UExit | UStream

-- byVar :: String -> Shell 'UString
-- byVar s = Shell _

-- byExit :: String -> Shell 'UExit
-- byExit s = Shell _

-- byOut :: String -> Shell 'UStream
-- byOut s = Shell _

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

deBruijnize :: Int -> Expr -> String
deBruijnize i (App f x) = App (deBruijnize i f) (deBruijnize i x)
deBruijnize i (Bnd j) = Bnd (i - j)
deBruijnize i (Lam f) = Lam' (deBruijnize i' (f (Bnd i')))
  where i' = i + 1
deBruijnize _ e       = e

toShell :: Int -> Expr -> String
toShell _ (Lit s) = primRet ++ "=" ++ shellEscape s
toShell i (App f x) =
  unlines
  [ f'
  , primPush ++ " \"$" ++ primRet ++ "\""
  , x'
  , primApply
  ]
  where f' = toShell i f
        x' = toShell i x
toShell i (Lam f) =
  unlines
  [ undefined
  ]
  where _ = toShell i' (f (Bnd i'))
        i' = i + 1
toShell i (Bnd j) =
  undefined
  where v = i - j -- de Bruijn index

litFun name = Lit (name ++ " \"$1\"")

main :: IO ()
main = do
  hSetNewlineMode stdout noNewlineTranslation
  shRuntime <- readFile "sh-rt.sh"
  putStr shRuntime
  putStr . toShell 0 $
    App (litFun primPrint)
    (App (App (litFun primConcat)
          (Lit "hello ")) (Lit "world"))

-- time bash -c '. ./sh-rt.sh; for i in {1..10000}; do : _ts_rt_expr x 1 + 1; done'
