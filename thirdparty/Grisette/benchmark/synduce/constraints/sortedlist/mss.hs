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

instance SimpleMergeable Unit where
  mrgIte cond l r = go cond l r
    where
      go cond Unit Unit = Unit
      go _ _ _ = error "Should not happen"

------pre output end-----

------program space begin----
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

-- output_type: {Int,Int}
-- param_list: xs a
data Expr0_0
  = Prod0_0 (UnionM Expr0_1) (UnionM Expr0_1)
  | CZero20_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param10_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access0_0_0_1 (UnionM Expr0_0)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Min0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Max0_1 (UnionM Expr0_1) (UnionM Expr0_1)
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

-- output_type: {Int,Int}
-- param_list: l xs w tmp1 s r
data Expr1_0
  = Param31_0
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param21_1
  | Param41_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Min1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Max1_1 (UnionM Expr1_1) (UnionM Expr1_1)
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

-- output_type: {Int,Int}
-- param_list: l xs w tmp3 tmp2 r
data Expr2_0
  = Param32_0
  | Param42_0
  | Prod2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param22_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access0_0_2_1 (UnionM Expr2_0)
  | Access1_0_2_1 (UnionM Expr2_0)
  | Min2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Max2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Ceq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: {Int,Int,Int,Int}
-- param_list: xs tmp4 inp
data Expr3_0
  = Prod3_0 (UnionM Expr3_2) (UnionM Expr3_2) (UnionM Expr3_2) (UnionM Expr3_2)
  | CZero43_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Prod3_1 (UnionM Expr3_2) (UnionM Expr3_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Cadd3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Csub3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Czero3_2
  | CIte3_2 (UnionM Expr3_3) (UnionM Expr3_2) (UnionM Expr3_2)
  | Access0_1_3_2 (UnionM Expr3_1)
  | Access1_1_3_2 (UnionM Expr3_1)
  | Access0_0_3_2 (UnionM Expr3_0)
  | Access1_0_3_2 (UnionM Expr3_0)
  | Access2_0_3_2 (UnionM Expr3_0)
  | Access3_0_3_2 (UnionM Expr3_0)
  | Min3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Max3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

data Expr3_3
  = Ceq3_3 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cless3_3 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cleq3_3 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cand3_3 (UnionM Expr3_3) (UnionM Expr3_3)
  | Cor3_3 (UnionM Expr3_3) (UnionM Expr3_3)
  | Cnot3_3 (UnionM Expr3_3)
  | CFalse3_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_3)
    via (Default Expr3_3)

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
$(makeUnionWrapper "mrg" ''Expr3_3)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero20_0]
    genSingle1 = [mrgParam10_1] ++ [mrgCzero0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_0_1 e0_0] ++ [mrgAccess1_0_0_1 e0_1] ++ [mrgMin0_1 e1_6 e1_7] ++ [mrgMax0_1 e1_8 e1_9])
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
    genSingle1 = [mrgParam21_1] ++ [mrgParam41_1] ++ [mrgCzero1_1]
    genSingle2 = [mrgCFalse1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgMin1_1 e1_6 e1_7] ++ [mrgMax1_1 e1_8 e1_9])
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
    genSingle0 = [mrgParam32_0] ++ [mrgParam42_0]
    genSingle1 = [mrgParam22_1] ++ [mrgCzero2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_2_1 e0_0] ++ [mrgAccess1_0_2_1 e0_1] ++ [mrgMin2_1 e1_6 e1_7] ++ [mrgMax2_1 e1_8 e1_9])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq2_2 e1_0 e1_1] ++ [mrgCless2_2 e1_2 e1_3] ++ [mrgCleq2_2 e1_4 e1_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero43_0]
    genSingle1 = [mrgParam13_1]
    genSingle2 = [mrgCzero3_2]
    genSingle3 = [mrgCFalse3_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e2_0 e2_1 e2_2 e2_3])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd3_1 e2_0 e2_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e2_6 <- (gen2 (gendepth - 1))
        e2_7 <- (gen2 (gendepth - 1))
        e2_8 <- (gen2 (gendepth - 1))
        e2_9 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd3_2 e2_0 e2_1] ++ [mrgCsub3_2 e2_2 e2_3] ++ [mrgCIte3_2 e3_0 e2_4 e2_5] ++ [mrgAccess0_1_3_2 e1_0] ++ [mrgAccess1_1_3_2 e1_1] ++ [mrgAccess0_0_3_2 e0_0] ++ [mrgAccess1_0_3_2 e0_1] ++ [mrgAccess2_0_3_2 e0_2] ++ [mrgAccess3_0_3_2 e0_3] ++ [mrgMin3_2 e2_6 e2_7] ++ [mrgMax3_2 e2_8 e2_9])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq3_3 e2_0 e2_1] ++ [mrgCless3_3 e2_2 e2_3] ++ [mrgCleq3_3 e2_4 e2_5] ++ [mrgCand3_3 e3_0 e3_1] ++ [mrgCor3_3 e3_2 e3_3] ++ [mrgCnot3_3 e3_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymInteger, SymInteger)
eval0_0 env (Prod0_0 p0 p1) = ((evalU0_1 env p0), (evalU0_1 env p1))
eval0_0 env (CZero20_0) = (0,0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Param10_1) = evalVar0 env "a"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_0_0_1 p0) = fst (evalU0_0 env p0)
eval0_1 env (Access1_0_0_1 p0) = snd (evalU0_0 env p0)
eval0_1 env (Min0_1 p0 p1) = min' (evalU0_1 env p0) (evalU0_1 env p1)
eval0_1 env (Max0_1 p0 p1) = max' (evalU0_1 env p0) (evalU0_1 env p1)

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

eval1_0 :: RefEnv -> Expr1_0 -> (SymInteger, SymInteger)
eval1_0 env (Param31_0) = evalVar1 env "tmp1"
eval1_0 env (Prod1_0 p0 p1) = ((evalU1_1 env p0), (evalU1_1 env p1))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param21_1) = evalVar0 env "w"
eval1_1 env (Param41_1) = evalVar0 env "s"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_0_1_1 p0) = fst (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = snd (evalU1_0 env p0)
eval1_1 env (Min1_1 p0 p1) = min' (evalU1_1 env p0) (evalU1_1 env p1)
eval1_1 env (Max1_1 p0 p1) = max' (evalU1_1 env p0) (evalU1_1 env p1)

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

eval2_0 :: RefEnv -> Expr2_0 -> (SymInteger, SymInteger)
eval2_0 env (Param32_0) = evalVar1 env "tmp3"
eval2_0 env (Param42_0) = evalVar1 env "tmp2"
eval2_0 env (Prod2_0 p0 p1) = ((evalU2_1 env p0), (evalU2_1 env p1))

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param22_1) = evalVar0 env "w"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access0_0_2_1 p0) = fst (evalU2_0 env p0)
eval2_1 env (Access1_0_2_1 p0) = snd (evalU2_0 env p0)
eval2_1 env (Min2_1 p0 p1) = min' (evalU2_1 env p0) (evalU2_1 env p1)
eval2_1 env (Max2_1 p0 p1) = max' (evalU2_1 env p0) (evalU2_1 env p1)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger, SymInteger, SymInteger)
eval3_0 env (Prod3_0 p0 p1 p2 p3) = ((evalU3_2 env p0), (evalU3_2 env p1), (evalU3_2 env p2), (evalU3_2 env p3))
eval3_0 env (CZero43_0) = (0,0,0,0)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger, SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (SymInteger, SymInteger)
eval3_1 env (Param13_1) = evalVar1 env "tmp4"
eval3_1 env (Prod3_1 p0 p1) = ((evalU3_2 env p0), (evalU3_2 env p1))

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (SymInteger, SymInteger)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymInteger
eval3_2 env (Cadd3_2 p0 p1) =  (evalU3_2 env p0) + (evalU3_2 env p1) 
eval3_2 env (Csub3_2 p0 p1) =  (evalU3_2 env p0) - (evalU3_2 env p1) 
eval3_2 env (Czero3_2) = 0
eval3_2 env (CIte3_2 p0 p1 p2) = mrgIte ((evalU3_3 env p0) ==~ (toSym True)) (evalU3_2 env p1) (evalU3_2 env p2)
eval3_2 env (Access0_1_3_2 p0) = fst (evalU3_1 env p0)
eval3_2 env (Access1_1_3_2 p0) = snd (evalU3_1 env p0)
eval3_2 env (Access0_0_3_2 p0) = get1from4 (evalU3_0 env p0)
eval3_2 env (Access1_0_3_2 p0) = get2from4 (evalU3_0 env p0)
eval3_2 env (Access2_0_3_2 p0) = get3from4 (evalU3_0 env p0)
eval3_2 env (Access3_0_3_2 p0) = get4from4 (evalU3_0 env p0)
eval3_2 env (Min3_2 p0 p1) = min' (evalU3_2 env p0) (evalU3_2 env p1)
eval3_2 env (Max3_2 p0 p1) = max' (evalU3_2 env p0) (evalU3_2 env p1)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymInteger
evalU3_2 env = onUnion (eval3_2 env)

eval3_3 :: RefEnv -> Expr3_3 -> SymBool
eval3_3 env (Ceq3_3 p0 p1) =  (evalU3_2 env p0) ==~ (evalU3_2 env p1) 
eval3_3 env (Cless3_3 p0 p1) =  (evalU3_2 env p0) <~ (evalU3_2 env p1) 
eval3_3 env (Cleq3_3 p0 p1) =  (evalU3_2 env p0) <=~ (evalU3_2 env p1) 
eval3_3 env (Cand3_3 p0 p1) =  (evalU3_3 env p0) &&~ (evalU3_3 env p1) 
eval3_3 env (Cor3_3 p0 p1) =  (evalU3_3 env p0) ||~ (evalU3_3 env p1) 
eval3_3 env (Cnot3_3 p0) =  mrgIte ((evalU3_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_3 env (CFalse3_3) = (toSym False)

evalU3_3 :: RefEnv -> UnionM Expr3_3 -> SymBool
evalU3_3 env = onUnion (eval3_3 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs a 

Hole grammar for #1
l xs w tmp1 s r 

Hole grammar for #2
l xs w tmp3 tmp2 r 

Hole grammar for #3
xs tmp4 inp 
-}

data List
  = Elt SymInteger
  | Cons SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data CList
  = Single SymInteger
  | Concat SymInteger CList CList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon CList, ExtractSymbolics)
    via (Default CList)

instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Elt l) (Elt r) = Elt (mrgIte cond l r)
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

cat = 
  let
    f a b = 
      case a of
        Elt w -> Cons w b
        Cons h t -> Cons h (f t b)
      
  in
  f 

repr = 
  let
    f c = 
      case c of
        Single w -> Elt w
        Concat w l r -> cat (f l) (f r)
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

min' a b = 
  mrgIte (a <~  b)
    (a)
    (b)

lmax = 
  let
    f c = 
      case c of
        Single w -> w
        Concat w l r -> max' (f l) (f r)
      
  in
  f 

lmin = 
  let
    f c = 
      case c of
        Single w -> w
        Concat w l r -> min' (f l) (f r)
      
  in
  f 

is_parti = 
  let
    f c = 
      case c of
        Single w -> (toSym True)
        Concat w l r -> (((lmax l) <~  w) &&~  (w <~  (lmin r))) &&~  ((f l) &&~  (f r))
      
  in
  f 

spec = 
  let
    f xs = 
      case xs of
        Elt w -> (w, (max' w 0), (max' w 0), (max' w 0))
        Cons h t -> 
          let
            res =
              (f t)
          in
          (((get1from4 res) +  h), (max' (get2from4 res) ((get1from4 res) +  h)), (max' ((get3from4 res) +  h) 0), (max' (get4from4 res) ((get3from4 res) +  h)))
      
  in
  f 

sum' = 
  let
    f c = 
      case c of
        Single a -> a
        Concat a l r -> (f l) +  (f r)
      
  in
  f 

target = 
  let
    f xs = 
      case xs of
        Single a -> 
          evalU0_0 (RefEnv [("a", (Env0 a))]) ((genSym (3::Int) "hole0") :: (UnionM Expr0_0))
        Concat w l r -> mrgIte (w <~  0)
            (let
              s =
                (sum' l)
            in
            let
              tmp1 =
                (f r)
            in
            evalU1_0 (RefEnv [("tmp1", (Env1 tmp1)), ("w", (Env0 w)), ("s", (Env0 s))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0)))
            (let
              tmp2 =
                (f l)
            in
            let
              tmp3 =
                (f r)
            in
            evalU2_0 (RefEnv [("tmp3", (Env1 tmp3)), ("tmp2", (Env1 tmp2)), ("w", (Env0 w))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0)))
      
  in
  f 

insert w = 
  let
    f xs = 
      case xs of
        Elt a -> mrgIte (w <~  a)
            (Cons w (Elt a))
            (Cons a (Elt w))
        Cons h t -> mrgIte (w <~  h)
            (Cons w xs)
            (Cons h (f t))
      
  in
  f 

sort = 
  let
    f xs = 
      case xs of
        Elt w -> xs
        Cons h t -> insert h (f t)
      
  in
  f 

access x = 
  case x of
    Elt w -> (w, x)
    Cons h t -> (h, t)
  

fill = 
  let
    f c xs = 
      case c of
        Single _ -> 
          let
            info =
              (access xs)
          in
          ((Single (fst info)), (snd info))
        Concat _ l r -> 
          let
            lres =
              (f l xs)
          in
          let
            info =
              (access (snd lres))
          in
          let
            rres =
              (f r (snd info))
          in
          ((Concat (fst info) (fst lres) (fst rres)), (snd rres))
      
  in
  f 

flatten = 
  let
    f c = 
      case c of
        Single w -> Elt w
        Concat w l r -> cat (f l) (Cons w (f r))
      
  in
  f 

gen c = 
  let
    xs =
      (sort (flatten c))
  in
  (fst (fill c xs))

main' xs = 
  let
    inp =
      (gen xs)
  in
  mrgIte (is_parti inp)
    (let
      tmp4 =
        (target inp)
    in
    evalU3_0 (RefEnv [("tmp4", (Env1 tmp4))]) ((genSym (3::Int) "hole3") :: (UnionM Expr3_0)))
    ((0, 0, 0, 0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(CList, (Integer, Integer, Integer, Integer))] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(CList, (Integer, Integer, Integer, Integer))] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Single (-4)))), (((-4)), ((0)), ((0)), ((0))))
                , ((((Concat ((-2)) ((Concat ((5)) ((Single (3))) ((Single (5))))) ((Concat ((-5)) ((Single (1))) ((Concat ((-5)) ((Single (3))) ((Single (5)))))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((3)) ((Concat ((5)) ((Single (-5))) ((Concat ((4)) ((Concat ((4)) ((Single (-4))) ((Single (1))))) ((Single (-2))))))) ((Single (-1)))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((1)) ((Single (3))) ((Concat ((4)) ((Single (-1))) ((Single (-2)))))))), (((3)), ((5)), ((3)), ((5))))
                , ((((Concat ((-4)) ((Concat ((0)) ((Single (-3))) ((Single (5))))) ((Concat ((2)) ((Single (4))) ((Concat ((-5)) ((Single (-3))) ((Single (-5)))))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((-4)) ((Single (-3))) ((Concat ((0)) ((Single (2))) ((Single (-3)))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Single (-3)))), (((-3)), ((0)), ((0)), ((0))))
                , ((((Concat ((5)) ((Single (2))) ((Single (-1)))))), (((4)), ((5)), ((4)), ((5))))
                , ((((Concat ((-2)) ((Concat ((0)) ((Single (-4))) ((Single (1))))) ((Concat ((-1)) ((Single (-3))) ((Concat ((-5)) ((Single (-1))) ((Single (5)))))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((1)) ((Concat ((1)) ((Single (-2))) ((Single (-4))))) ((Single (1)))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((-2)) ((Concat ((-4)) ((Concat ((-3)) ((Single (-3))) ((Single (-3))))) ((Single (1))))) ((Concat ((3)) ((Single (3))) ((Single (-4)))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((-4)) ((Single (0))) ((Single (5)))))), (((1)), ((5)), ((1)), ((5))))
                , ((((Single (3)))), (((3)), ((3)), ((3)), ((3))))
                , ((((Concat ((3)) ((Concat ((-5)) ((Single (-4))) ((Single (-2))))) ((Concat ((-3)) ((Single (-1))) ((Single (-2)))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Single (0)))), (((0)), ((0)), ((0)), ((0))))
                , ((((Single (-4)))), (((-4)), ((0)), ((0)), ((0))))
                , ((((Concat ((2)) ((Single (-1))) ((Concat ((-4)) ((Single (1))) ((Concat ((-1)) ((Single (3))) ((Concat ((1)) ((Single (4))) ((Single (-2)))))))))))), (((0)), ((0)), ((0)), ((0))))
                , ((((Concat ((-1)) ((Single (-3))) ((Single (-2)))))), (((-4)), ((0)), ((0)), ((0))))
                , ((((Concat ((-2)) ((Single (-4))) ((Single (2)))))), (((-2)), ((2)), ((0)), ((2))))
                , ((((Concat ((-5)) ((Single (4))) ((Concat ((0)) ((Concat ((2)) ((Single (-4))) ((Single (-3))))) ((Concat ((2)) ((Single (-1))) ((Single (2)))))))))), (((0)), ((0)), ((0)), ((0))))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
