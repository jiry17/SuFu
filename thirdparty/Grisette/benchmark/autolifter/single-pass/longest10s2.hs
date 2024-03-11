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
data EnvValue
  = Env0 (SymInteger, SymInteger, SymInteger)
  | Env1 SymInteger
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
          Env1 _ -> 1 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          1 -> SimpleStrategy $ \cond (Env1 l) (Env1 r) -> Env1 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
      go cond (Env1 l) (Env1 r) = Env1 $ mrgIte cond l r
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

evalVar0 :: RefEnv -> Ident -> (SymInteger, SymInteger, SymInteger)
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

{- env_type_list: 
(SymInteger, SymInteger, SymInteger)
SymInteger
-}

-- output_type: {Int,Int,Int}
-- param_list: xs
data Expr0_0
  = Prod0_0 (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1)
  | CTripleZero0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access00_1 (UnionM Expr0_0)
  | Access10_1 (UnionM Expr0_0)
  | Access20_1 (UnionM Expr0_0)
  | Al_inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Ceq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cless0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cleq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cand0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cor0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cnot0_2 (UnionM Expr0_2)
  | CFalse0_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: {Int,Int,Int}
-- param_list: h xs t tmp1
data Expr1_0
  = Param31_0
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access01_1 (UnionM Expr1_0)
  | Access11_1 (UnionM Expr1_0)
  | Access21_1 (UnionM Expr1_0)
  | Al_inf1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Ceq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  | CFalse1_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: Int
-- param_list: tmp2 xs
data Expr2_0
  = Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_2) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access02_0 (UnionM Expr2_1)
  | Access12_0 (UnionM Expr2_1)
  | Access22_0 (UnionM Expr2_1)
  | Al_inf2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param02_1
  | Prod2_1 (UnionM Expr2_0) (UnionM Expr2_0) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Ceq2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cless2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cleq2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCTripleZero0_0]
    genSingle1 = [mrgCzero0_1] ++ [mrgAl_inf0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e1_0 e1_1 e1_2])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess00_1 e0_0] ++ [mrgAccess10_1 e0_1] ++ [mrgAccess20_1 e0_2])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam31_0]
    genSingle1 = [mrgParam01_1] ++ [mrgCzero1_1] ++ [mrgAl_inf1_1]
    genSingle2 = [mrgCFalse1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e1_0 e1_1 e1_2])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess01_1 e0_0] ++ [mrgAccess11_1 e0_1] ++ [mrgAccess21_1 e0_2])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq1_2 e1_0 e1_1] ++ [mrgCless1_2 e1_2 e1_3] ++ [mrgCleq1_2 e1_4 e1_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero2_0] ++ [mrgAl_inf2_0]
    genSingle1 = [mrgParam02_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e2_0 e0_4 e0_5] ++ [mrgAccess02_0 e1_0] ++ [mrgAccess12_0 e1_1] ++ [mrgAccess22_0 e1_2])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd2_1 e0_0 e0_1 e0_2])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq2_2 e0_0 e0_1] ++ [mrgCless2_2 e0_2 e0_3] ++ [mrgCleq2_2 e0_4 e0_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

getFirst :: (SymInteger, SymInteger, SymInteger) -> SymInteger
getFirst (a, _, _) = a

getSecond :: (SymInteger, SymInteger, SymInteger) -> SymInteger
getSecond (_, b, _) = b

getThird :: (SymInteger, SymInteger, SymInteger) -> SymInteger
getThird (_, _, c) = c

eval0_0 :: RefEnv -> Expr0_0 -> (SymInteger, SymInteger, SymInteger)
eval0_0 env (Prod0_0 p0 p1 p2) = ((evalU0_1 env p0), (evalU0_1 env p1), (evalU0_1 env p2))
eval0_0 env (CTripleZero0_0) = (0,0,0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymInteger, SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access00_1 p0) = getFirst (evalU0_0 env p0)
eval0_1 env (Access10_1 p0) = getSecond (evalU0_0 env p0)
eval0_1 env (Access20_1 p0) = getThird (evalU0_0 env p0)
eval0_1 env (Al_inf0_1) = (100 :: SymInteger)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymInteger, SymInteger, SymInteger)
eval1_0 env (Param31_0) = evalVar0 env "tmp1"
eval1_0 env (Prod1_0 p0 p1 p2) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param01_1) = evalVar1 env "h"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access01_1 p0) = getFirst (evalU1_0 env p0)
eval1_1 env (Access11_1 p0) = getSecond (evalU1_0 env p0)
eval1_1 env (Access21_1 p0) = getThird (evalU1_0 env p0)
eval1_1 env (Al_inf1_1) = (100 :: SymInteger)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Access02_0 p0) = getFirst (evalU2_1 env p0)
eval2_0 env (Access12_0 p0) = getSecond (evalU2_1 env p0)
eval2_0 env (Access22_0 p0) = getThird (evalU2_1 env p0)
eval2_0 env (Al_inf2_0) = (100 :: SymInteger)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (SymInteger, SymInteger, SymInteger)
eval2_1 env (Param02_1) = evalVar0 env "tmp2"
eval2_1 env (Prod2_1 p0 p1 p2) = ((evalU2_0 env p0), (evalU2_0 env p1), (evalU2_0 env p2))

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> (SymInteger, SymInteger, SymInteger)
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_0 env p0) ==~ (evalU2_0 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_0 env p0) <~ (evalU2_0 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_0 env p0) <=~ (evalU2_0 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)


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

data List
    = Cons SymInteger List
    | Nil Unit
    deriving stock (Generic, Show)
    deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
        via (Default List)

single_pass v xs = 
    let run = (let f xs = 
                            case xs of
                                Nil _ -> 
                                    evalU0_0 (RefEnv []) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
                                Cons h t -> 
                                    let tmp1 = (f t) in 
                                        evalU1_0 (RefEnv [("tmp1", (Env0 tmp1)), ("h", (Env1 h))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0))
                            in
                f ) in 
        let tmp2 = (run xs) in 
            evalU2_0 (RefEnv [("tmp2", (Env0 tmp2))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))

inf = 
    100

max' a b = 
    mrgIte (a <~  b)
        (b)
        (a)

f :: SymBool -> SymInteger -> List -> SymInteger
f s0 len xs = case xs of
                Nil _ -> 0
                Cons h t -> 
                    let s1 = (s0 &&~ (h ==~  2)) in 
                        let s0 = ((h ==~  1) ||~  ((h ==~  0) &&~  s0)) in 
                            let len = (mrgIte (s0 ||~  s1) (len + 1) 0) in
                                let upd = (mrgIte (s1) len 0) in
                                    let len = (mrgIte (h ==~  1) 1 (mrgIte s1 0 len)) in
                                        max' upd (f s0 len t)

longest10s2 :: List -> SymInteger
longest10s2 = f (toSym False) 0

main' = 
    single_pass longest10s2

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(List, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(List, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((Cons (2) (Cons (2) (Cons (0) (Cons (1) (Cons (0) (Cons (2) (Cons (2) (Cons (2) (Nil Unit))))))))), 3)
                , ((Cons (0) (Nil Unit)), 0)
                , ((Cons (0) (Cons (0) (Cons (1) (Cons (2) (Cons (0) (Cons (0) (Cons (1) (Cons (0) (Cons (2) (Nil Unit)))))))))), 3)
                , ((Cons (0) (Cons (2) (Cons (0) (Cons (1) (Cons (2) (Cons (1) (Cons (2) (Cons (0) (Cons (0) (Cons (2) (Cons (1) (Cons (1) (Cons (1) (Cons (2) (Cons (0) (Cons (2) (Cons (0) (Cons (1) (Cons (2) (Nil Unit)))))))))))))))))))), 2)
                , ((Cons (2) (Cons (2) (Cons (2) (Cons (2) (Cons (0) (Cons (0) (Cons (0) (Cons (0) (Cons (0) (Cons (1) (Cons (2) (Nil Unit)))))))))))), 2)
                , ((Cons (1) (Cons (1) (Cons (0) (Cons (1) (Cons (2) (Cons (2) (Cons (1) (Nil Unit)))))))), 2)
                , ((Cons (1) (Cons (2) (Cons (2) (Cons (1) (Cons (0) (Cons (0) (Nil Unit))))))), 2)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (2) (Cons (1) (Cons (1) (Nil Unit))))))), 2)
                , ((Nil Unit), 0)
                , ((Cons (0) (Cons (1) (Cons (0) (Cons (1) (Cons (0) (Cons (0) (Cons (0) (Nil Unit)))))))), 0)
                , ((Cons (0) (Cons (1) (Cons (1) (Cons (0) (Cons (1) (Cons (2) (Cons (1) (Cons (1) (Cons (0) (Cons (1) (Cons (2) (Cons (0) (Cons (0) (Cons (1) (Cons (1) (Cons (1) (Cons (2) (Nil Unit)))))))))))))))))), 2)
                , ((Cons (1) (Cons (1) (Cons (2) (Cons (0) (Cons (1) (Cons (0) (Cons (0) (Cons (0) (Cons (2) (Cons (2) (Cons (2) (Cons (0) (Cons (2) (Nil Unit)))))))))))))), 5)
                , ((Cons (2) (Cons (2) (Cons (2) (Cons (2) (Cons (2) (Cons (2) (Cons (1) (Cons (1) (Cons (2) (Cons (0) (Cons (0) (Cons (2) (Cons (1) (Cons (0) (Nil Unit))))))))))))))), 2)
                , ((Cons (2) (Cons (2) (Cons (2) (Cons (2) (Cons (0) (Cons (1) (Cons (0) (Cons (1) (Nil Unit))))))))), 0)
                , ((Cons (2) (Cons (2) (Cons (2) (Cons (0) (Nil Unit))))), 0)
                , ((Cons (2) (Cons (0) (Cons (2) (Cons (0) (Cons (2) (Nil Unit)))))), 0)
                , ((Cons (0) (Cons (1) (Cons (0) (Cons (1) (Cons (2) (Cons (2) (Cons (0) (Cons (0) (Cons (1) (Cons (2) (Cons (1) (Cons (1) (Nil Unit))))))))))))), 2)
                , ((Cons (2) (Cons (2) (Cons (2) (Cons (1) (Nil Unit))))), 0)
                , ((Cons (0) (Cons (1) (Nil Unit))), 0)
                , ((Cons (1) (Cons (2) (Cons (2) (Cons (0) (Cons (2) (Nil Unit)))))), 2)
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
