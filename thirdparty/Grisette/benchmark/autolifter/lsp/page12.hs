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
  | Env1 (SymInteger, SymInteger, SymInteger)
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

evalVar1 :: RefEnv -> Ident -> (SymInteger, SymInteger, SymInteger)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymInteger
(SymInteger, SymInteger, SymInteger)
-}

-- output_type: Int
-- param_list: suf x l lpre res
data Expr0_0
  = Param40_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_2) (UnionM Expr0_0) (UnionM Expr0_0)
  | Access0_1_0_0 (UnionM Expr0_1)
  | Access1_1_0_0 (UnionM Expr0_1)
  | Access2_1_0_0 (UnionM Expr0_1)
  | Lim0_0
  | Max0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param30_1
  | Prod0_1 (UnionM Expr0_0) (UnionM Expr0_0) (UnionM Expr0_0)
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

-- output_type: Bool
-- param_list: l len res x lpre suf
data Expr1_0
  = Ceq1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cor1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cnot1_0 (UnionM Expr1_0)
  | CFalse1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param11_1
  | Param21_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_0) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_2_1_1 (UnionM Expr1_2)
  | Access1_2_1_1 (UnionM Expr1_2)
  | Access2_2_1_1 (UnionM Expr1_2)
  | Lim1_1
  | Max1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Param41_2
  | Prod1_2 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: {Int,Int,Int}
-- param_list: l x t h len res suf lpre
data Expr2_0
  = Param72_0
  | Prod2_0 (UnionM Expr2_1) (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param32_1
  | Param42_1
  | Param52_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access0_0_2_1 (UnionM Expr2_0)
  | Access1_0_2_1 (UnionM Expr2_0)
  | Access2_0_2_1 (UnionM Expr2_0)
  | Lim2_1
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

-- output_type: {Int,Int,Int}
-- param_list: l x t h len res suf lpre
data Expr3_0
  = Param73_0
  | Prod3_0 (UnionM Expr3_1) (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param33_1
  | Param43_1
  | Param53_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_2) (UnionM Expr3_1) (UnionM Expr3_1)
  | Access0_0_3_1 (UnionM Expr3_0)
  | Access1_0_3_1 (UnionM Expr3_0)
  | Access2_0_3_1 (UnionM Expr3_0)
  | Lim3_1
  | Max3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Ceq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: {Int,Int,Int}
-- param_list: x tmp1
data Expr4_0
  = Prod4_0 (UnionM Expr4_1) (UnionM Expr4_1) (UnionM Expr4_1)
  | CZero34_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Cadd4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Csub4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Czero4_1
  | CIte4_1 (UnionM Expr4_2) (UnionM Expr4_1) (UnionM Expr4_1)
  | Access0_0_4_1 (UnionM Expr4_0)
  | Access1_0_4_1 (UnionM Expr4_0)
  | Access2_0_4_1 (UnionM Expr4_0)
  | Lim4_1
  | Max4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Ceq4_2 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cless4_2 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cleq4_2 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cand4_2 (UnionM Expr4_2) (UnionM Expr4_2)
  | Cor4_2 (UnionM Expr4_2) (UnionM Expr4_2)
  | Cnot4_2 (UnionM Expr4_2)
  | CFalse4_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_2)
    via (Default Expr4_2)

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
$(makeUnionWrapper "mrg" ''Expr4_0)
$(makeUnionWrapper "mrg" ''Expr4_1)
$(makeUnionWrapper "mrg" ''Expr4_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam40_0] ++ [mrgCzero0_0] ++ [mrgLim0_0]
    genSingle1 = [mrgParam30_1]
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
        e0_7 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_0_0 e1_0] ++ [mrgAccess1_1_0_0 e1_1] ++ [mrgAccess2_1_0_0 e1_2] ++ [mrgMax0_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd0_1 e0_0 e0_1 e0_2])
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
    genSingle0 = [mrgCFalse1_0]
    genSingle1 = [mrgParam11_1] ++ [mrgParam21_1] ++ [mrgCzero1_1] ++ [mrgLim1_1]
    genSingle2 = [mrgParam41_2]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq1_0 e1_0 e1_1] ++ [mrgCless1_0 e1_2 e1_3] ++ [mrgCleq1_0 e1_4 e1_5] ++ [mrgCand1_0 e0_0 e0_1] ++ [mrgCor1_0 e0_2 e0_3] ++ [mrgCnot1_0 e0_4])
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e0_0 e1_4 e1_5] ++ [mrgAccess0_2_1_1 e2_0] ++ [mrgAccess1_2_1_1 e2_1] ++ [mrgAccess2_2_1_1 e2_2] ++ [mrgMax1_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd1_2 e1_0 e1_1 e1_2])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam72_0]
    genSingle1 = [mrgParam32_1] ++ [mrgParam42_1] ++ [mrgParam52_1] ++ [mrgCzero2_1] ++ [mrgLim2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e1_0 e1_1 e1_2])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_2_1 e0_0] ++ [mrgAccess1_0_2_1 e0_1] ++ [mrgAccess2_0_2_1 e0_2] ++ [mrgMax2_1 e1_6 e1_7])
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
    genSingle0 = [mrgParam73_0]
    genSingle1 = [mrgParam33_1] ++ [mrgParam43_1] ++ [mrgParam53_1] ++ [mrgCzero3_1] ++ [mrgLim3_1]
    genSingle2 = [mrgCFalse3_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e1_0 e1_1 e1_2])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_3_1 e0_0] ++ [mrgAccess1_0_3_1 e0_1] ++ [mrgAccess2_0_3_1 e0_2] ++ [mrgMax3_1 e1_6 e1_7])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq3_2 e1_0 e1_1] ++ [mrgCless3_2 e1_2 e1_3] ++ [mrgCleq3_2 e1_4 e1_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero34_0]
    genSingle1 = [mrgCzero4_1] ++ [mrgLim4_1]
    genSingle2 = [mrgCFalse4_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd4_0 e1_0 e1_1 e1_2])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd4_1 e1_0 e1_1] ++ [mrgCsub4_1 e1_2 e1_3] ++ [mrgCIte4_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_4_1 e0_0] ++ [mrgAccess1_0_4_1 e0_1] ++ [mrgAccess2_0_4_1 e0_2] ++ [mrgMax4_1 e1_6 e1_7])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq4_2 e1_0 e1_1] ++ [mrgCless4_2 e1_2 e1_3] ++ [mrgCleq4_2 e1_4 e1_5] ++ [mrgCand4_2 e2_0 e2_1] ++ [mrgCor4_2 e2_2 e2_3] ++ [mrgCnot4_2 e2_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param40_0) = evalVar0 env "res"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)
eval0_0 env (Access0_1_0_0 p0) = get1from3 (evalU0_1 env p0)
eval0_0 env (Access1_1_0_0 p0) = get2from3 (evalU0_1 env p0)
eval0_0 env (Access2_1_0_0 p0) = get3from3 (evalU0_1 env p0)
eval0_0 env (Lim0_0) = lim
eval0_0 env (Max0_0 p0 p1) = max' (evalU0_0 env p0) (evalU0_0 env p1)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> SymInteger
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> (SymInteger, SymInteger, SymInteger)
eval0_1 env (Param30_1) = evalVar1 env "lpre"
eval0_1 env (Prod0_1 p0 p1 p2) = ((evalU0_0 env p0), (evalU0_0 env p1), (evalU0_0 env p2))

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> (SymInteger, SymInteger, SymInteger)
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

eval1_0 :: RefEnv -> Expr1_0 -> SymBool
eval1_0 env (Ceq1_0 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_0 env (Cless1_0 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_0 env (Cleq1_0 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_0 env (Cand1_0 p0 p1) =  (evalU1_0 env p0) &&~ (evalU1_0 env p1) 
eval1_0 env (Cor1_0 p0 p1) =  (evalU1_0 env p0) ||~ (evalU1_0 env p1) 
eval1_0 env (Cnot1_0 p0) =  mrgIte ((evalU1_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_0 env (CFalse1_0) = (toSym False)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymBool
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param11_1) = evalVar0 env "len"
eval1_1 env (Param21_1) = evalVar0 env "res"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_0 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_2_1_1 p0) = get1from3 (evalU1_2 env p0)
eval1_1 env (Access1_2_1_1 p0) = get2from3 (evalU1_2 env p0)
eval1_1 env (Access2_2_1_1 p0) = get3from3 (evalU1_2 env p0)
eval1_1 env (Lim1_1) = lim
eval1_1 env (Max1_1 p0 p1) = max' (evalU1_1 env p0) (evalU1_1 env p1)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> (SymInteger, SymInteger, SymInteger)
eval1_2 env (Param41_2) = evalVar1 env "lpre"
eval1_2 env (Prod1_2 p0 p1 p2) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2))

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> (SymInteger, SymInteger, SymInteger)
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymInteger, SymInteger, SymInteger)
eval2_0 env (Param72_0) = evalVar1 env "lpre"
eval2_0 env (Prod2_0 p0 p1 p2) = ((evalU2_1 env p0), (evalU2_1 env p1), (evalU2_1 env p2))

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymInteger, SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param32_1) = evalVar0 env "h"
eval2_1 env (Param42_1) = evalVar0 env "len"
eval2_1 env (Param52_1) = evalVar0 env "res"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access0_0_2_1 p0) = get1from3 (evalU2_0 env p0)
eval2_1 env (Access1_0_2_1 p0) = get2from3 (evalU2_0 env p0)
eval2_1 env (Access2_0_2_1 p0) = get3from3 (evalU2_0 env p0)
eval2_1 env (Lim2_1) = lim
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

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger, SymInteger)
eval3_0 env (Param73_0) = evalVar1 env "lpre"
eval3_0 env (Prod3_0 p0 p1 p2) = ((evalU3_1 env p0), (evalU3_1 env p1), (evalU3_1 env p2))

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param33_1) = evalVar0 env "h"
eval3_1 env (Param43_1) = evalVar0 env "len"
eval3_1 env (Param53_1) = evalVar0 env "res"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Access0_0_3_1 p0) = get1from3 (evalU3_0 env p0)
eval3_1 env (Access1_0_3_1 p0) = get2from3 (evalU3_0 env p0)
eval3_1 env (Access2_0_3_1 p0) = get3from3 (evalU3_0 env p0)
eval3_1 env (Lim3_1) = lim
eval3_1 env (Max3_1 p0 p1) = max' (evalU3_1 env p0) (evalU3_1 env p1)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> (SymInteger, SymInteger, SymInteger)
eval4_0 env (Prod4_0 p0 p1 p2) = ((evalU4_1 env p0), (evalU4_1 env p1), (evalU4_1 env p2))
eval4_0 env (CZero34_0) = (0,0,0)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> (SymInteger, SymInteger, SymInteger)
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymInteger
eval4_1 env (Cadd4_1 p0 p1) =  (evalU4_1 env p0) + (evalU4_1 env p1) 
eval4_1 env (Csub4_1 p0 p1) =  (evalU4_1 env p0) - (evalU4_1 env p1) 
eval4_1 env (Czero4_1) = 0
eval4_1 env (CIte4_1 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_1 env p1) (evalU4_1 env p2)
eval4_1 env (Access0_0_4_1 p0) = get1from3 (evalU4_0 env p0)
eval4_1 env (Access1_0_4_1 p0) = get2from3 (evalU4_0 env p0)
eval4_1 env (Access2_0_4_1 p0) = get3from3 (evalU4_0 env p0)
eval4_1 env (Lim4_1) = lim
eval4_1 env (Max4_1 p0 p1) = max' (evalU4_1 env p0) (evalU4_1 env p1)

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymInteger
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> SymBool
eval4_2 env (Ceq4_2 p0 p1) =  (evalU4_1 env p0) ==~ (evalU4_1 env p1) 
eval4_2 env (Cless4_2 p0 p1) =  (evalU4_1 env p0) <~ (evalU4_1 env p1) 
eval4_2 env (Cleq4_2 p0 p1) =  (evalU4_1 env p0) <=~ (evalU4_1 env p1) 
eval4_2 env (Cand4_2 p0 p1) =  (evalU4_2 env p0) &&~ (evalU4_2 env p1) 
eval4_2 env (Cor4_2 p0 p1) =  (evalU4_2 env p0) ||~ (evalU4_2 env p1) 
eval4_2 env (Cnot4_2 p0) =  mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval4_2 env (CFalse4_2) = (toSym False)

evalU4_2 :: RefEnv -> UnionM Expr4_2 -> SymBool
evalU4_2 env = onUnion (eval4_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
suf x l lpre res 

Hole grammar for #1
l len res x lpre suf 

Hole grammar for #2
l x t h len res suf lpre 

Hole grammar for #3
l x t h len res suf lpre 

Hole grammar for #4
x tmp1 
-}

data List
  = Cons SymInteger List
  | Nil Unit
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)
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

head' x default' = 
  case x of
    Cons h t -> h
    _ -> default'
  

fold f x w0 = 
  let
    g x = 
      case x of
        Cons h t -> f h (g t)
        _ -> w0
      
  in
  g  x

length' x = 
  fold (\a b -> b +  1) x 0

sum' x = 
  fold (\a b -> a +  b) x 0

lim = 
  10

minimum' x = 
  fold (\a b -> mrgIte (a <~  b)
    (a)
    (b)) x 0

pushback = 
  let
    f x a = 
      case x of
        Cons h t -> Cons h (f t a)
        Nil _ -> Cons a (Nil Unit)
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

tail x = 
  case x of
    Cons h t -> t
    Nil _ -> x
  

lsp pred x = 
  let
    f l suf lpre res = 
      let
        len =
          (evalU0_0 (RefEnv [("res", (Env0 res)), ("lpre", (Env1 lpre))]) ((genSym (5::Int) "hole0") :: (UnionM Expr0_0)))
      in
      mrgIte ((len ==~  0) ||~  (evalU1_0 (RefEnv [("len", (Env0 len)), ("res", (Env0 res)), ("lpre", (Env1 lpre))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0))))
        (case l of
          Cons h t -> f t suf (evalU2_0 (RefEnv [("lpre", (Env1 lpre)), ("h", (Env0 h)), ("len", (Env0 len)), ("res", (Env0 res))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0))) (max' res len)
          Nil _ -> max' res len
        )
        (case suf of
          Cons h t -> f l t (evalU3_0 (RefEnv [("lpre", (Env1 lpre)), ("h", (Env0 h)), ("len", (Env0 len)), ("res", (Env0 res))]) ((genSym (5::Int) "hole3") :: (UnionM Expr3_0))) res
          _ -> 0
        )
  in
  f  x x (let
    tmp1 =
      (Nil Unit)
  in
  evalU4_0 (RefEnv []) ((genSym (5::Int) "hole4") :: (UnionM Expr4_0))) 0

isvalid x = 
  (sum' x) <~  lim

main' x = 
  mrgIte ((minimum' x) <~  0)
    (0)
    (lsp isvalid x)

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
                ((((Nil Unit))), (0))
                , ((((Nil Unit))), (0))
                , ((((Cons ((2)) ((Cons ((4)) ((Cons ((3)) ((Cons ((1)) ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((-4)) ((Nil Unit))))))))))))))))))), (0))
                , ((((Cons ((4)) ((Cons ((5)) ((Nil Unit))))))), (2))
                , ((((Cons ((-3)) ((Cons ((1)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((-1)) ((Cons ((1)) ((Cons ((4)) ((Cons ((1)) ((Cons ((5)) ((Nil Unit))))))))))))))))))))), (0))
                , ((((Cons ((-1)) ((Cons ((2)) ((Cons ((-3)) ((Cons ((-2)) ((Nil Unit))))))))))), (0))
                , ((((Cons ((-5)) ((Cons ((3)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((-1)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit))))))))))))))))))), (0))
                , ((((Cons ((4)) ((Cons ((-4)) ((Cons ((1)) ((Cons ((-2)) ((Nil Unit))))))))))), (0))
                , ((((Cons ((-3)) ((Nil Unit))))), (0))
                , ((((Cons ((-3)) ((Cons ((-4)) ((Cons ((2)) ((Cons ((3)) ((Cons ((-4)) ((Cons ((0)) ((Cons ((-3)) ((Nil Unit))))))))))))))))), (0))
                , ((((Cons ((4)) ((Cons ((-5)) ((Nil Unit))))))), (0))
                , ((((Cons ((-1)) ((Cons ((-1)) ((Cons ((-4)) ((Cons ((-3)) ((Cons ((0)) ((Cons ((2)) ((Nil Unit))))))))))))))), (0))
                , ((((Nil Unit))), (0))
                , ((((Cons ((2)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((-5)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((2)) ((Nil Unit))))))))))))))))), (0))
                , ((((Cons ((4)) ((Cons ((-2)) ((Cons ((-2)) ((Nil Unit))))))))), (0))
                , ((((Nil Unit))), (0))
                , ((((Cons ((3)) ((Cons ((-5)) ((Cons ((2)) ((Nil Unit))))))))), (0))
                , ((((Cons ((5)) ((Cons ((-1)) ((Cons ((0)) ((Nil Unit))))))))), (0))
                , ((((Nil Unit))), (0))
                , ((((Cons ((-2)) ((Cons ((-4)) ((Cons ((-3)) ((Cons ((-3)) ((Cons ((-3)) ((Cons ((1)) ((Cons ((3)) ((Cons ((3)) ((Nil Unit))))))))))))))))))), (0))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
