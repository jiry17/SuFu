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
type Point = (SymInteger, SymInteger)

type Plan = (PointList, PointList)

data List
  = Nil Unit
  | Cons SymInteger List
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data PointList
  = Ponil Unit
  | Pocons Point PointList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon PointList, ExtractSymbolics)
    via (Default PointList)

data PlanList
  = Pnil Unit
  | Pcons SymInteger PlanList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon PlanList, ExtractSymbolics)
    via (Default PlanList)
{-
instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go cond (Cons l1 r1) (Nil r2) = Cons l1 r1
      go cond (Nil l) (Cons l2 r2) = Cons l2 r2
      go _ _ _ = error "Should not happen"
      -}

data EnvValue
  = Env0 SymInteger
  | Env1 (SymInteger, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> SymInteger
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> (SymInteger, SymInteger)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymInteger
(SymInteger, SymInteger)
-}

-- output_type: Int
-- param_list: p h t xs
data Expr0_0
  = Param10_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_2) (UnionM Expr0_0) (UnionM Expr0_0)
  | Access0_1_0_0 (UnionM Expr0_1)
  | Access1_1_0_0 (UnionM Expr0_1)
  | Dis0_0 (UnionM Expr0_1) (UnionM Expr0_1)
  | Sqr0_0 (UnionM Expr0_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param00_1
  | Prod0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Ceq0_2 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cless0_2 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cleq0_2 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cand0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cor0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cnot0_2 (UnionM Expr0_2)
  | CFalse0_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: Int
-- param_list: p h h1 t xs
data Expr1_0
  = Param11_0
  | Param21_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_2) (UnionM Expr1_0) (UnionM Expr1_0)
  | Access0_1_1_0 (UnionM Expr1_1)
  | Access1_1_1_0 (UnionM Expr1_1)
  | Dis1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  | Sqr1_0 (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Prod1_1 (UnionM Expr1_0) (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Ceq1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cless1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cleq1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  | CFalse1_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: Int
-- param_list: tmp1 xs polist w pos
data Expr2_0
  = Param32_0
  | Param42_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_1) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access0_2_2_0 (UnionM Expr2_2)
  | Access1_2_2_0 (UnionM Expr2_2)
  | Dis2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Sqr2_0 (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Ceq2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cless2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cleq2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cand2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cor2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cnot2_1 (UnionM Expr2_1)
  | CFalse2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Prod2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | CZero22_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: Int
-- param_list: h t xs
data Expr3_0
  = Param03_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_1) (UnionM Expr3_0) (UnionM Expr3_0)
  | Access0_2_3_0 (UnionM Expr3_2)
  | Access1_2_3_0 (UnionM Expr3_2)
  | Min3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Dis3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Sqr3_0 (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Ceq3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cless3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cleq3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cand3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cor3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cnot3_1 (UnionM Expr3_1)
  | CFalse3_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Prod3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | CZero23_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr3_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam10_0] ++ [mrgCzero0_0]
    genSingle1 = [mrgParam00_1]
    genSingle2 = [mrgCFalse0_2]
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
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_0_0 e1_0] ++ [mrgAccess1_1_0_0 e1_1] ++ [mrgDis0_0 e1_2 e1_3] ++ [mrgSqr0_0 e0_6])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd0_1 e0_0 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq0_2 e0_0 e0_1] ++ [mrgCless0_2 e0_2 e0_3] ++ [mrgCleq0_2 e0_4 e0_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam11_0] ++ [mrgParam21_0] ++ [mrgCzero1_0]
    genSingle1 = [mrgParam01_1]
    genSingle2 = [mrgCFalse1_2]
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
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_1_0 e1_0] ++ [mrgAccess1_1_1_0 e1_1] ++ [mrgDis1_0 e1_2 e1_3] ++ [mrgSqr1_0 e0_6])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd1_1 e0_0 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq1_2 e0_0 e0_1] ++ [mrgCless1_2 e0_2 e0_3] ++ [mrgCleq1_2 e0_4 e0_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam32_0] ++ [mrgParam42_0] ++ [mrgCzero2_0]
    genSingle1 = [mrgCFalse2_1]
    genSingle2 = [mrgCZero22_2]
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
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e1_0 e0_4 e0_5] ++ [mrgAccess0_2_2_0 e2_0] ++ [mrgAccess1_2_2_0 e2_1] ++ [mrgDis2_0 e2_2 e2_3] ++ [mrgSqr2_0 e0_6])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
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
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq2_1 e0_0 e0_1] ++ [mrgCless2_1 e0_2 e0_3] ++ [mrgCleq2_1 e0_4 e0_5] ++ [mrgCand2_1 e1_0 e1_1] ++ [mrgCor2_1 e1_2 e1_3] ++ [mrgCnot2_1 e1_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd2_2 e0_0 e0_1])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam03_0] ++ [mrgCzero3_0]
    genSingle1 = [mrgCFalse3_1]
    genSingle2 = [mrgCZero23_2]
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
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e1_0 e0_4 e0_5] ++ [mrgAccess0_2_3_0 e2_0] ++ [mrgAccess1_2_3_0 e2_1] ++ [mrgMin3_0 e0_6 e0_7] ++ [mrgDis3_0 e2_2 e2_3] ++ [mrgSqr3_0 e0_8])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
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
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq3_1 e0_0 e0_1] ++ [mrgCless3_1 e0_2 e0_3] ++ [mrgCleq3_1 e0_4 e0_5] ++ [mrgCand3_1 e1_0 e1_1] ++ [mrgCor3_1 e1_2 e1_3] ++ [mrgCnot3_1 e1_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd3_2 e0_0 e0_1])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param10_0) = evalVar0 env "h"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)
eval0_0 env (Access0_1_0_0 p0) = fst (evalU0_1 env p0)
eval0_0 env (Access1_1_0_0 p0) = snd (evalU0_1 env p0)
eval0_0 env (Dis0_0 p0 p1) = dis (evalU0_1 env p0) (evalU0_1 env p1)
eval0_0 env (Sqr0_0 p0) = sqr (evalU0_0 env p0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> SymInteger
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> (SymInteger, SymInteger)
eval0_1 env (Param00_1) = evalVar1 env "p"
eval0_1 env (Prod0_1 p0 p1) = ((evalU0_0 env p0), (evalU0_0 env p1))

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> (SymInteger, SymInteger)
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_0 env p0) ==~ (evalU0_0 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_0 env p0) <~ (evalU0_0 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_0 env p0) <=~ (evalU0_0 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> SymInteger
eval1_0 env (Param11_0) = evalVar0 env "h"
eval1_0 env (Param21_0) = evalVar0 env "h1"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)
eval1_0 env (Access0_1_1_0 p0) = fst (evalU1_1 env p0)
eval1_0 env (Access1_1_1_0 p0) = snd (evalU1_1 env p0)
eval1_0 env (Dis1_0 p0 p1) = dis (evalU1_1 env p0) (evalU1_1 env p1)
eval1_0 env (Sqr1_0 p0) = sqr (evalU1_0 env p0)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymInteger
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> (SymInteger, SymInteger)
eval1_1 env (Param01_1) = evalVar1 env "p"
eval1_1 env (Prod1_1 p0 p1) = ((evalU1_0 env p0), (evalU1_0 env p1))

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> (SymInteger, SymInteger)
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_0 env p0) ==~ (evalU1_0 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_0 env p0) <~ (evalU1_0 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_0 env p0) <=~ (evalU1_0 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Param32_0) = evalVar0 env "w"
eval2_0 env (Param42_0) = evalVar0 env "pos"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Access0_2_2_0 p0) = fst (evalU2_2 env p0)
eval2_0 env (Access1_2_2_0 p0) = snd (evalU2_2 env p0)
eval2_0 env (Dis2_0 p0 p1) = dis (evalU2_2 env p0) (evalU2_2 env p1)
eval2_0 env (Sqr2_0 p0) = sqr (evalU2_0 env p0)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymBool
eval2_1 env (Ceq2_1 p0 p1) =  (evalU2_0 env p0) ==~ (evalU2_0 env p1) 
eval2_1 env (Cless2_1 p0 p1) =  (evalU2_0 env p0) <~ (evalU2_0 env p1) 
eval2_1 env (Cleq2_1 p0 p1) =  (evalU2_0 env p0) <=~ (evalU2_0 env p1) 
eval2_1 env (Cand2_1 p0 p1) =  (evalU2_1 env p0) &&~ (evalU2_1 env p1) 
eval2_1 env (Cor2_1 p0 p1) =  (evalU2_1 env p0) ||~ (evalU2_1 env p1) 
eval2_1 env (Cnot2_1 p0) =  mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_1 env (CFalse2_1) = (toSym False)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymBool
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> (SymInteger, SymInteger)
eval2_2 env (Prod2_2 p0 p1) = ((evalU2_0 env p0), (evalU2_0 env p1))
eval2_2 env (CZero22_2) = (0,0)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> (SymInteger, SymInteger)
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Param03_0) = evalVar0 env "h"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Access0_2_3_0 p0) = fst (evalU3_2 env p0)
eval3_0 env (Access1_2_3_0 p0) = snd (evalU3_2 env p0)
eval3_0 env (Min3_0 p0 p1) = min' (evalU3_0 env p0) (evalU3_0 env p1)
eval3_0 env (Dis3_0 p0 p1) = dis (evalU3_2 env p0) (evalU3_2 env p1)
eval3_0 env (Sqr3_0 p0) = sqr (evalU3_0 env p0)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymInteger
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymBool
eval3_1 env (Ceq3_1 p0 p1) =  (evalU3_0 env p0) ==~ (evalU3_0 env p1) 
eval3_1 env (Cless3_1 p0 p1) =  (evalU3_0 env p0) <~ (evalU3_0 env p1) 
eval3_1 env (Cleq3_1 p0 p1) =  (evalU3_0 env p0) <=~ (evalU3_0 env p1) 
eval3_1 env (Cand3_1 p0 p1) =  (evalU3_1 env p0) &&~ (evalU3_1 env p1) 
eval3_1 env (Cor3_1 p0 p1) =  (evalU3_1 env p0) ||~ (evalU3_1 env p1) 
eval3_1 env (Cnot3_1 p0) =  mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_1 env (CFalse3_1) = (toSym False)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymBool
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> (SymInteger, SymInteger)
eval3_2 env (Prod3_2 p0 p1) = ((evalU3_0 env p0), (evalU3_0 env p1))
eval3_2 env (CZero23_2) = (0,0)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> (SymInteger, SymInteger)
evalU3_2 env = onUnion (eval3_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
p h t xs 

Hole grammar for #1
p h h1 t xs 

Hole grammar for #2
tmp1 xs polist w pos 

Hole grammar for #3
h t xs 
-}


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

sqr x = 
  x *  x

dis x y = 
  (sqr ((fst x) -  (fst y))) +  (sqr ((snd x) -  (snd y)))

head' xs = 
  case xs of
    Ponil _ -> (0, 0)
    Pocons h _ -> h
  

extend p = 
  let
    f xs = 
      case xs of
        Pnil _ -> xs
        Pcons h t -> 
          let
            h1 =
              (evalU0_0 (RefEnv [("h", (Env0 h)), ("p", (Env1 p))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0)))
          in
          let
            h2 =
              (evalU1_0 (RefEnv [("h", (Env0 h)), ("h1", (Env0 h1)), ("p", (Env1 p))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0)))
          in
          Pcons h1 (Pcons h2 (f t))
      
  in
  f 

generate = 
  let
    f pos xs = 
      case xs of
        Nil _ -> Pnil Unit
        Cons w (Nil _) -> 
          let
            polist =
              (Pocons (pos, w) (Ponil Unit))
          in
          Pcons (let
            tmp1 =
              (polist, polist)
          in
          evalU2_0 (RefEnv [("w", (Env0 w)), ("pos", (Env0 pos))]) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))) (Pnil Unit)
        Cons h t -> 
          let
            res =
              (f (pos +  1) t)
          in
          extend (pos, h) res
      
  in
  f  0

eval_plist = 
  let
    f pre xs = 
      case xs of
        Ponil _ -> 0
        Pocons p t -> (dis p pre) +  (f p t)
      
  in
  f 

min' a b = 
  mrgIte (a <~  b)
    (a)
    (b)

get_best = 
  let
    eval p =
      (let
        first =
          (head' (fst p))
      in
      (eval_plist first (fst p)) +  (eval_plist first (snd p)))
  in
  let
    f xs = 
      case xs of
        Pnil _ -> 1000
        Pcons h t -> min' (evalU3_0 (RefEnv [("h", (Env0 h))]) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))) (f t)
      
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
                ((((Nil Unit))), (1000))
                , ((((Nil Unit))), (1000))
                , ((((Cons ((4)) ((Cons ((4)) ((Cons ((4)) ((Cons ((3)) ((Cons ((5)) ((Cons ((4)) ((Cons ((3)) ((Cons ((0)) ((Nil Unit))))))))))))))))))), (54))
                , ((((Cons ((5)) ((Cons ((5)) ((Nil Unit))))))), (2))
                , ((((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Cons ((1)) ((Cons ((2)) ((Cons ((3)) ((Cons ((5)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit))))))))))))))))))))), (52))
                , ((((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((2)) ((Nil Unit))))))))))), (20))
                , ((((Cons ((0)) ((Cons ((4)) ((Cons ((5)) ((Cons ((1)) ((Cons ((2)) ((Cons ((3)) ((Cons ((4)) ((Cons ((5)) ((Nil Unit))))))))))))))))))), (60))
                , ((((Cons ((5)) ((Cons ((1)) ((Cons ((3)) ((Cons ((1)) ((Nil Unit))))))))))), (34))
                , ((((Cons ((1)) ((Nil Unit))))), (0))
                , ((((Cons ((1)) ((Cons ((0)) ((Cons ((4)) ((Cons ((4)) ((Cons ((0)) ((Cons ((2)) ((Cons ((1)) ((Nil Unit))))))))))))))))), (40))
                , ((((Cons ((5)) ((Cons ((0)) ((Nil Unit))))))), (52))
                , ((((Cons ((2)) ((Cons ((2)) ((Cons ((0)) ((Cons ((1)) ((Cons ((2)) ((Cons ((4)) ((Nil Unit))))))))))))))), (38))
                , ((((Nil Unit))), (1000))
                , ((((Cons ((4)) ((Cons ((2)) ((Cons ((2)) ((Cons ((0)) ((Cons ((5)) ((Cons ((1)) ((Cons ((3)) ((Nil Unit))))))))))))))))), (46))
                , ((((Cons ((5)) ((Cons ((1)) ((Cons ((1)) ((Nil Unit))))))))), (38))
                , ((((Nil Unit))), (1000))
                , ((((Cons ((4)) ((Cons ((0)) ((Cons ((4)) ((Nil Unit))))))))), (38))
                , ((((Cons ((5)) ((Cons ((2)) ((Cons ((2)) ((Nil Unit))))))))), (24))
                , ((((Nil Unit))), (1000))
                , ((((Cons ((1)) ((Cons ((0)) ((Cons ((1)) ((Cons ((1)) ((Cons ((1)) ((Cons ((3)) ((Cons ((4)) ((Cons ((4)) ((Nil Unit))))))))))))))))))), (42))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
