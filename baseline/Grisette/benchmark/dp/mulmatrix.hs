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
    deriving stock (Generic, Show, Eq)
    deriving (Mergeable, EvaluateSym, ToCon Unit, ExtractSymbolics)
        via (Default Unit)

instance SimpleMergeable Unit where
  mrgIte cond l r = go cond l r
    where
      go cond Unit Unit = Unit
      go _ _ _ = error "Should not happen"

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
-- param_list: l xs t h
data Expr0_0
  = Param00_0
  | Param30_0
  | Prod0_0 (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access0_0_0_1 (UnionM Expr0_0)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Access2_0_0_1 (UnionM Expr0_0)
  | Mulcost0_1 (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1)
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
-- param_list: a b tmp1 xs
data Expr1_0
  = Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  | CZero31_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Param11_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access2_0_1_1 (UnionM Expr1_0)
  | Mulcost1_1 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
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
-- param_list: h t xs
data Expr2_0
  = Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_2) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access0_1_2_0 (UnionM Expr2_1)
  | Access1_1_2_0 (UnionM Expr2_1)
  | Access2_1_2_0 (UnionM Expr2_1)
  | Min2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Mulcost2_0 (UnionM Expr2_0) (UnionM Expr2_0) (UnionM Expr2_0)
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
    genSingle0 = [mrgParam00_0] ++ [mrgParam30_0]
    genSingle1 = [mrgCzero0_1]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_0_1 e0_0] ++ [mrgAccess1_0_0_1 e0_1] ++ [mrgAccess2_0_0_1 e0_2] ++ [mrgMulcost0_1 e1_6 e1_7 e1_8])
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
    genSingle0 = [mrgCZero31_0]
    genSingle1 = [mrgParam01_1] ++ [mrgParam11_1] ++ [mrgCzero1_1]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgAccess2_0_1_1 e0_2] ++ [mrgMulcost1_1 e1_6 e1_7 e1_8])
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
    genSingle0 = [mrgCzero2_0]
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
        e0_6 <- (gen0 (gendepth - 1))
        e0_7 <- (gen0 (gendepth - 1))
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e0_10 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_2_0 e1_0] ++ [mrgAccess1_1_2_0 e1_1] ++ [mrgAccess2_1_2_0 e1_2] ++ [mrgMin2_0 e0_6 e0_7] ++ [mrgMulcost2_0 e0_8 e0_9 e0_10])
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

eval0_0 :: RefEnv -> Expr0_0 -> (SymInteger, SymInteger, SymInteger)
eval0_0 env (Param00_0) = evalVar0 env "l"
eval0_0 env (Param30_0) = evalVar0 env "h"
eval0_0 env (Prod0_0 p0 p1 p2) = ((evalU0_1 env p0), (evalU0_1 env p1), (evalU0_1 env p2))

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymInteger, SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_0_0_1 p0) = get1from3 (evalU0_0 env p0)
eval0_1 env (Access1_0_0_1 p0) = get2from3 (evalU0_0 env p0)
eval0_1 env (Access2_0_0_1 p0) = get3from3 (evalU0_0 env p0)
eval0_1 env (Mulcost0_1 p0 p1 p2) = mulcost (evalU0_1 env p0) (evalU0_1 env p1) (evalU0_1 env p2)

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
eval1_0 env (Prod1_0 p0 p1 p2) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2))
eval1_0 env (CZero31_0) = (0,0,0)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param01_1) = evalVar1 env "a"
eval1_1 env (Param11_1) = evalVar1 env "b"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_0_1_1 p0) = get1from3 (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = get2from3 (evalU1_0 env p0)
eval1_1 env (Access2_0_1_1 p0) = get3from3 (evalU1_0 env p0)
eval1_1 env (Mulcost1_1 p0 p1 p2) = mulcost (evalU1_1 env p0) (evalU1_1 env p1) (evalU1_1 env p2)

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
eval2_0 env (Access0_1_2_0 p0) = get1from3 (evalU2_1 env p0)
eval2_0 env (Access1_1_2_0 p0) = get2from3 (evalU2_1 env p0)
eval2_0 env (Access2_1_2_0 p0) = get3from3 (evalU2_1 env p0)
eval2_0 env (Min2_0 p0 p1) = min' (evalU2_0 env p0) (evalU2_0 env p1)
eval2_0 env (Mulcost2_0 p0 p1 p2) = mulcost (evalU2_0 env p0) (evalU2_0 env p1) (evalU2_0 env p2)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (SymInteger, SymInteger, SymInteger)
eval2_1 env (Param02_1) = evalVar0 env "h"
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
l xs t h 

Hole grammar for #1
a b tmp1 xs 

Hole grammar for #2
h t xs 
-}

type Plan = (SymInteger, SymInteger, SymInteger)

data List
  = Nil Unit
  | Cons SymInteger List
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data Tree
  = Leaf SymInteger SymInteger
  | Node Tree Tree
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data PlanList
  = Pnil Unit
  | Pcons Plan PlanList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon PlanList, ExtractSymbolics)
    via (Default PlanList)

instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go cond (Cons l1 r1) (Nil r2) = Cons l1 r1
      go cond (Nil l) (Cons l2 r2) = Cons l2 r2
      go _ _ _ = error "Should not happen"
      
instance SimpleMergeable PlanList where
  mrgIte cond l r = go cond l r
    where
      go cond (Pcons l1 r1) (Pcons l2 r2) = Pcons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Pnil l) (Pnil r) = Pnil Unit
      go cond (Pcons l1 r1) (Pnil r2) = Pcons l1 r1
      go cond (Pnil l) (Pcons l2 r2) = Pcons l2 r2
      go _ _ _ = error "Should not happen"

div' :: SymInteger -> SymInteger -> SymInteger
div' a b = 
    let 
        a1 = toCon (ssym "a" :: SymInteger) :: Maybe Integer 
    in
    let
        b1 = toCon (ssym "b" :: SymInteger) :: Maybe Integer 
    in
        case a1 of 
            Nothing -> 0 :: SymInteger
            Just a2 ->
                case b1 of
                    Nothing -> 0 :: SymInteger
                    Just b2 -> if b2 == 0 then 0::SymInteger else let c = a2 `div` b2 in (toSym c)

get1from3 (a, _, _) = a
get2from3 (_, b, _) = b
get3from3 (_, _, c) = c

get1from4 (a, _, _, _) = a
get2from4 (_, b, _, _) = b
get3from4 (_, _, c, _) = c
get4from4 (_, _, _, d) = d

get1from5 (a, _, _, _, _) = a
get2from5 (_, b, _, _, _) = b
get3from5 (_, _, c, _, _) = c
get4from5 (_, _, _, d, _) = d
get5from5 (_, _, _, _, e) = e

get1from6 (a, _, _, _, _, _) = a
get2from6 (_, b, _, _, _, _) = b
get3from6 (_, _, c, _, _, _) = c
get4from6 (_, _, _, d, _, _) = d
get5from6 (_, _, _, _, e, _) = e
get6from6 (_, _, _, _, _, f) = f


mulcost l mid r = 
  (l *  r) *  mid

merge' = 
  let
    f xs ys = 
      case xs of
        Pnil _ -> ys
        Pcons h t -> Pcons h (f t ys)
  in
  f 

combine = 
  let
    combine_single l =
      (let
        f xs = 
          case xs of
            Pnil _ -> xs
            Pcons h t -> Pcons (evalU0_0 (RefEnv [("l", (Env0 l)), ("h", (Env0 h))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0))) (f t)
      in
      f )
  in
  let
    f xs ys = 
      case xs of
        Pnil _ -> xs
        Pcons h t -> merge' (combine_single h ys) (f t ys)
      
  in
  f 

snoc w = 
  let
    f xs = 
      case xs of
        Nil _ -> Cons w (Nil Unit)
        Cons h t -> Cons h (f t)
      
  in
  f 

generate = 
  let
    f xs = 
      case xs of
        Nil _ -> Pnil Unit
        Cons _ (Nil _) -> Pnil Unit
        Cons a (Cons b (Nil _)) -> Pcons (let
            tmp1 =
              (Leaf a b)
          in
          evalU1_0 (RefEnv [("a", (Env1 a)), ("b", (Env1 b))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))) (Pnil Unit)
        Cons h t -> let
            g l r = 
              case r of
                Cons h newr -> 
                  let
                    newl =
                      (snoc h l)
                  in
                  let
                    res =
                      (combine (f newl) (f r))
                  in
                  case newr of
                    Cons w (Nil _) -> res
                    _ -> merge' res (g newl newr)
                  
              
          in
          g  (Cons h (Nil Unit)) t
      
  in
  f 

eval t = 
  (get1from3 (let
    f t = 
      case t of
        Leaf l r -> (0, l, r)
        Node l r -> 
          let
            lres =
              (f l)
          in
          let
            rres =
              (f r)
          in
          ((((get1from3 lres) +  (get1from3 rres)) +  (mulcost (get2from3 lres) (get3from3 lres) (get3from3 rres))), (get2from3 lres), (get3from3 rres))
  in
  f  t))

min' a b = 
  mrgIte (a <~  b)
    (a)
    (b)

get_best = 
  let
    f xs = 
      case xs of
        Pnil _ -> 20000
        Pcons h t -> min' (evalU2_0 (RefEnv [("h", (Env0 h))]) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))) (f t)
      
  in
  f 

main' xs = 
  get_best (generate xs)

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
                ((((Nil Unit))), (20000))
                , ((((Cons ((4)) ((Cons ((1)) ((Cons ((4)) ((Cons ((5)) ((Cons ((1)) ((Nil Unit))))))))))))), (28))
                , ((((Cons ((1)) ((Cons ((5)) ((Cons ((1)) ((Nil Unit))))))))), (5))
                , ((((Cons ((2)) ((Cons ((3)) ((Nil Unit))))))), (0))
                , ((((Nil Unit))), (20000))
                , ((((Nil Unit))), (20000))
                , ((((Cons ((4)) ((Cons ((5)) ((Cons ((4)) ((Cons ((3)) ((Nil Unit))))))))))), (120))
                , ((((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((4)) ((Cons ((5)) ((Nil Unit))))))))))))), (51))
                , ((((Cons ((1)) ((Nil Unit))))), (20000))
                , ((((Cons ((5)) ((Cons ((2)) ((Nil Unit))))))), (0))
                , ((((Cons ((5)) ((Cons ((3)) ((Cons ((5)) ((Cons ((1)) ((Cons ((5)) ((Nil Unit))))))))))))), (55))
                , ((((Cons ((2)) ((Nil Unit))))), (20000))
                , ((((Cons ((5)) ((Cons ((5)) ((Cons ((1)) ((Cons ((1)) ((Cons ((2)) ((Nil Unit))))))))))))), (37))
                , ((((Cons ((5)) ((Nil Unit))))), (20000))
                , ((((Cons ((3)) ((Cons ((4)) ((Cons ((4)) ((Cons ((2)) ((Cons ((1)) ((Nil Unit))))))))))))), (36))
                , ((((Cons ((2)) ((Nil Unit))))), (20000))
                , ((((Nil Unit))), (20000))
                , ((((Nil Unit))), (20000))
                , ((((Cons ((2)) ((Cons ((2)) ((Cons ((2)) ((Nil Unit))))))))), (8))
                , ((((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Nil Unit))))))))))))), (33))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"