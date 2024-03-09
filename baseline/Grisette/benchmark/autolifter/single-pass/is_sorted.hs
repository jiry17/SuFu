{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Grisette
import GHC.Generics
import Data.Time
import Data.Hashable
import Data.Maybe
import Data.Proxy
import Control.DeepSeq
import Control.Monad.Except
import System.CPUTime
import System.Timeout as SysTimeout

type Ident = String
      
data Unit =
      Unit
    deriving stock (Generic, Show)
    deriving (Mergeable, EvaluateSym, ToCon Unit, ExtractSymbolics)
        via (Default Unit)

------pre output end-----

------program space begin----
data List
    = Cons SymInteger List
    | Nil Unit
    deriving stock (Generic, Show)
    deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
        via (Default List)

instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go _ _ _ = error "Should not happen"

data EnvValue
  = Env0 SymBool
  | Env1 SymInteger
  | Env2 List
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
          Env1 _ -> 1 :: Int
          Env2 _ -> 2 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          1 -> SimpleStrategy $ \cond (Env1 l) (Env1 r) -> Env1 $ mrgIte cond l r
          2 -> SimpleStrategy $ \cond (Env2 l) (Env2 r) -> Env2 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
      go cond (Env1 l) (Env1 r) = Env1 $ mrgIte cond l r
      go cond (Env2 l) (Env2 r) = Env2 $ mrgIte cond l r
      go _ _ _ = error "Should not happen"

$(makeUnionWrapper "u" ''EnvValue)

-- store all var from context
newtype RefEnv = RefEnv {unRefEnv :: [(Ident, EnvValue)]}
    deriving (Show, Generic)
    deriving (EvaluateSym) via (Default RefEnv)

instance Mergeable RefEnv where
    rootStrategy = SimpleStrategy $ \cond (RefEnv l) (RefEnv r) -> RefEnv $ go cond l r
        where
            go _ [] [] = []
            go cond ((li, lv) : l) ((ri, rv) : r)
                | li == ri = (ri, mrgIte cond lv rv) : go cond l r
            go _ _ _ = error "Should not happen"

instance SimpleMergeable RefEnv where
    mrgIte cond (RefEnv l) (RefEnv r) = RefEnv $ go cond l r
        where
            go _ [] [] = []
            go cond ((li, lv) : l) ((ri, rv) : r)
                | li == ri = (ri, mrgIte cond lv rv) : go cond l r
            go _ _ _ = error "Should not happen"

evalFunc :: RefEnv -> Ident -> EnvValue
evalFunc (RefEnv env) x = case lookup x env of
  Just v -> v
  Nothing -> error "Bad program: variable not in scope"

evalVar0 :: RefEnv -> Ident -> SymBool
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> SymInteger
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

evalVar2 :: RefEnv -> Ident -> List
evalVar2 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env2 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymBool
SymInteger
-}

-- output_type: Bool
data Expr0_0
  = Ceq0_0 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cless0_0 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cleq0_0 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cand0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cor0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cnot0_0 (UnionM Expr0_0)
  | CFalse0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  | Al_inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

-- output_type: Bool
-- param_list: h xs t tmp1
data Expr1_0
  = Param31_0
  | Cless1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Head1_1 (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Param21_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: Bool
data Expr2_0
  = Param02_0
  | Ceq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cor2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cnot2_0 (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_0) (UnionM Expr2_1) (UnionM Expr2_1)
  | Al_inf2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgCzero0_1] ++ [mrgAl_inf0_1]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq0_0 e1_0 e1_1] ++ [mrgCless0_0 e1_2 e1_3] ++ [mrgCleq0_0 e1_4 e1_5] ++ [mrgCand0_0 e0_0 e0_1] ++ [mrgCor0_0 e0_2 e0_3] ++ [mrgCnot0_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam31_0]
    genSingle1 = [mrgParam01_1]
    genSingle2 = [mrgParam21_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCless1_0 e1_2 e1_3] ++ [mrgCand1_0 e0_0 e0_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgHead1_1 e2_0])
        return res
    gen2 gendepth = chooseUnionFresh genSingle2


instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam02_0]
    genSingle1 = [mrgCzero2_1] ++ [mrgAl_inf2_1]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq2_0 e1_0 e1_1] ++ [mrgCless2_0 e1_2 e1_3] ++ [mrgCleq2_0 e1_4 e1_5] ++ [mrgCand2_0 e0_0 e0_1] ++ [mrgCor2_0 e0_2 e0_3] ++ [mrgCnot2_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e0_0 e1_4 e1_5])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> SymBool
eval0_0 env (Ceq0_0 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_0 env (Cless0_0 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_0 env (Cleq0_0 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_0 env (Cand0_0 p0 p1) =  (evalU0_0 env p0) &&~ (evalU0_0 env p1) 
eval0_0 env (Cor0_0 p0 p1) =  (evalU0_0 env p0) ||~ (evalU0_0 env p1) 
eval0_0 env (Cnot0_0 p0) =  mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_0 env (CFalse0_0) = (toSym False)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> SymBool
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Al_inf0_1) = (100 :: SymInteger)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval1_0 :: RefEnv -> Expr1_0 -> SymBool
eval1_0 env (Param31_0) = evalVar0 env "tmp1"
eval1_0 env (Cless1_0 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_0 env (Cand1_0 p0 p1) =  (evalU1_0 env p0) &&~ (evalU1_0 env p1) 

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymBool
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param01_1) = evalVar1 env "h"
eval1_1 env (Head1_1 p1) = al_head (evalU1_2 env p1)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> List
eval1_2 env (Param21_2) = evalVar2 env "t"

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> List
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Param02_0) = evalVar0 env "tmp2"
eval2_0 env (Ceq2_0 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_0 env (Cless2_0 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_0 env (Cleq2_0 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_0 env (Cand2_0 p0 p1) =  (evalU2_0 env p0) &&~ (evalU2_0 env p1) 
eval2_0 env (Cor2_0 p0 p1) =  (evalU2_0 env p0) ||~ (evalU2_0 env p1) 
eval2_0 env (Cnot2_0 p0) =  mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymBool
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Al_inf2_1) = (100 :: SymInteger)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs 

Hole grammar for #1
h xs t tmp1 

Hole grammar for #2
tmp2 xs 
-}

al_head :: List -> SymInteger
al_head l =
    case l of 
        Nil _ -> inf
        Cons h t -> h

single_pass v xs = 
    let run = (let f xs = 
                            case xs of
                                Nil _ -> 
                                    evalU0_0 (RefEnv []) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
                                Cons h t -> 
                                    let tmp1 = (f t) in 
                                        evalU1_0 (RefEnv [("tmp1", (Env0 tmp1)), ("h", (Env1 h)), ("t", (Env2 t))]) ((genSym (3::Int) "hole1") :: (UnionM Expr1_0))
                            in
                f ) in 
        let tmp2 = (run xs) in 
            evalU2_0 (RefEnv [("tmp2", (Env0 tmp2))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))

inf = 
    100

is_sorted :: List -> SymBool
is_sorted = 
    let f pre xs = 
                case xs of
                    Nil _ -> (toSym True)
                    Cons h t -> mrgIte (pre >=~ h)
                            ((toSym False))
                            (f h t)

                in
    f (-100)

main' = 
    single_pass is_sorted

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(List, Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(List, Bool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((Cons (3) (Cons (-5) (Cons (1) (Cons (0) (Nil Unit))))), False)
                , ((Cons (2) (Cons (1) (Cons (5) (Cons (0) (Cons (2) (Cons (-1) (Nil Unit))))))), False)
                , ((Nil Unit), True)
                , ((Cons (5) (Cons (-2) (Cons (-1) (Cons (3) (Nil Unit))))), False)
                , ((Cons (-3) (Cons (-2) (Cons (-1) (Cons (1) (Nil Unit))))), True)
                , ((Cons (-5) (Cons (4) (Cons (3) (Nil Unit)))), False)
                , ((Cons (5) (Cons (5) (Cons (2) (Cons (1) (Cons (1) (Cons (1) (Nil Unit))))))), False)
                , ((Cons (-4) (Cons (-5) (Cons (-4) (Nil Unit)))), False)
                , ((Cons (1) (Cons (-2) (Cons (-1) (Cons (3) (Cons (4) (Cons (-1) (Cons (-4) (Cons (5) (Cons (2) (Nil Unit)))))))))), False)
                , ((Nil Unit), True)
                , ((Cons (-1) (Cons (-2) (Nil Unit))), False)
                , ((Nil Unit), True)
                , ((Cons (-5) (Cons (2) (Cons (-3) (Cons (-2) (Cons (5) (Cons (-1) (Nil Unit))))))), False)
                , ((Cons (0) (Nil Unit)), True)
                , ((Cons (-3) (Cons (5) (Nil Unit))), True)
                , ((Cons (2) (Nil Unit)), True)
                , ((Cons (-5) (Cons (2) (Cons (-2) (Cons (4) (Cons (-3) (Cons (3) (Cons (4) (Cons (-3) (Cons (2) (Nil Unit)))))))))), False)
                , ((Cons (0) (Cons (-5) (Cons (3) (Cons (2) (Cons (2) (Nil Unit)))))), False)
                , ((Cons (5) (Nil Unit)), True)
                , ((Cons (-5) (Cons (-3) (Cons (1) (Cons (-1) (Cons (3) (Cons (3) (Cons (-1) (Cons (0) (Nil Unit))))))))), False)
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
