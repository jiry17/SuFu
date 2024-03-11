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
-- param_list: t w
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
-- param_list: l tmp2 t w tmp1 r
data Expr1_0
  = Param11_0
  | Param41_0
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param31_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
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
-- param_list: t w
data Expr2_0
  = Prod2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | CZero22_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param12_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access0_0_2_1 (UnionM Expr2_0)
  | Access1_0_2_1 (UnionM Expr2_0)
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

-- output_type: {Int,Int}
-- param_list: tmp3 r l w t tmp4
data Expr3_0
  = Param03_0
  | Param53_0
  | Prod3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param33_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_2) (UnionM Expr3_1) (UnionM Expr3_1)
  | Access0_0_3_1 (UnionM Expr3_0)
  | Access1_0_3_1 (UnionM Expr3_0)
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

-- output_type: Int
-- param_list: t tmp5
data Expr4_0
  = Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_2) (UnionM Expr4_0) (UnionM Expr4_0)
  | Access0_1_4_0 (UnionM Expr4_1)
  | Access1_1_4_0 (UnionM Expr4_1)
  | Max4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param14_1
  | Prod4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Ceq4_2 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cless4_2 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cleq4_2 (UnionM Expr4_0) (UnionM Expr4_0)
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_0_1 e0_0] ++ [mrgAccess1_0_0_1 e0_1] ++ [mrgMax0_1 e1_6 e1_7])
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
    genSingle0 = [mrgParam11_0] ++ [mrgParam41_0]
    genSingle1 = [mrgParam31_1] ++ [mrgCzero1_1]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgMax1_1 e1_6 e1_7])
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
    genSingle0 = [mrgCZero22_0]
    genSingle1 = [mrgParam12_1] ++ [mrgCzero2_1]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_2_1 e0_0] ++ [mrgAccess1_0_2_1 e0_1] ++ [mrgMax2_1 e1_6 e1_7])
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
    genSingle0 = [mrgParam03_0] ++ [mrgParam53_0]
    genSingle1 = [mrgParam33_1] ++ [mrgCzero3_1]
    genSingle2 = [mrgCFalse3_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e1_0 e1_1])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_3_1 e0_0] ++ [mrgAccess1_0_3_1 e0_1] ++ [mrgMax3_1 e1_6 e1_7])
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
    genSingle0 = [mrgCzero4_0]
    genSingle1 = [mrgParam14_1]
    genSingle2 = [mrgCFalse4_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd4_0 e0_0 e0_1] ++ [mrgCsub4_0 e0_2 e0_3] ++ [mrgCIte4_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_4_0 e1_0] ++ [mrgAccess1_1_4_0 e1_1] ++ [mrgMax4_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd4_1 e0_0 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq4_2 e0_0 e0_1] ++ [mrgCless4_2 e0_2 e0_3] ++ [mrgCleq4_2 e0_4 e0_5] ++ [mrgCand4_2 e2_0 e2_1] ++ [mrgCor4_2 e2_2 e2_3] ++ [mrgCnot4_2 e2_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymInteger, SymInteger)
eval0_0 env (Prod0_0 p0 p1) = ((evalU0_1 env p0), (evalU0_1 env p1))
eval0_0 env (CZero20_0) = (0,0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Param10_1) = evalVar0 env "w"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_0_0_1 p0) = fst (evalU0_0 env p0)
eval0_1 env (Access1_0_0_1 p0) = snd (evalU0_0 env p0)
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
eval1_0 env (Param11_0) = evalVar1 env "tmp2"
eval1_0 env (Param41_0) = evalVar1 env "tmp1"
eval1_0 env (Prod1_0 p0 p1) = ((evalU1_1 env p0), (evalU1_1 env p1))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param31_1) = evalVar0 env "w"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_0_1_1 p0) = fst (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = snd (evalU1_0 env p0)
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
eval2_0 env (Prod2_0 p0 p1) = ((evalU2_1 env p0), (evalU2_1 env p1))
eval2_0 env (CZero22_0) = (0,0)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param12_1) = evalVar0 env "w"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access0_0_2_1 p0) = fst (evalU2_0 env p0)
eval2_1 env (Access1_0_2_1 p0) = snd (evalU2_0 env p0)
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

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger)
eval3_0 env (Param03_0) = evalVar1 env "tmp3"
eval3_0 env (Param53_0) = evalVar1 env "tmp4"
eval3_0 env (Prod3_0 p0 p1) = ((evalU3_1 env p0), (evalU3_1 env p1))

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param33_1) = evalVar0 env "w"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Access0_0_3_1 p0) = fst (evalU3_0 env p0)
eval3_1 env (Access1_0_3_1 p0) = snd (evalU3_0 env p0)
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

eval4_0 :: RefEnv -> Expr4_0 -> SymInteger
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Access0_1_4_0 p0) = fst (evalU4_1 env p0)
eval4_0 env (Access1_1_4_0 p0) = snd (evalU4_1 env p0)
eval4_0 env (Max4_0 p0 p1) = max' (evalU4_0 env p0) (evalU4_0 env p1)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> (SymInteger, SymInteger)
eval4_1 env (Param14_1) = evalVar1 env "tmp5"
eval4_1 env (Prod4_1 p0 p1) = ((evalU4_0 env p0), (evalU4_0 env p1))

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> (SymInteger, SymInteger)
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> SymBool
eval4_2 env (Ceq4_2 p0 p1) =  (evalU4_0 env p0) ==~ (evalU4_0 env p1) 
eval4_2 env (Cless4_2 p0 p1) =  (evalU4_0 env p0) <~ (evalU4_0 env p1) 
eval4_2 env (Cleq4_2 p0 p1) =  (evalU4_0 env p0) <=~ (evalU4_0 env p1) 
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
t w 

Hole grammar for #1
l tmp2 t w tmp1 r 

Hole grammar for #2
t w 

Hole grammar for #3
tmp3 r l w t tmp4 

Hole grammar for #4
t tmp5 
-}

data Tree
  = Telt SymInteger
  | Tnode SymInteger Tree Tree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data PTree
  = Pelt SymInteger
  | Pnode SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon PTree, ExtractSymbolics)
    via (Default PTree)

data List
  = Elt PTree
  | Cons PTree List
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

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

repr = 
  let
    f x = 
      case x of
        Telt a -> Pelt a
        Tnode a l r -> 
          let
            repr_list =
              (let
                g y = 
                  case y of
                    Telt a -> 
                      let
                        z =
                          (Pelt a)
                      in
                      Elt z
                    Tnode a l r -> Cons (Pelt a) (Cons (f l) (g r))
                  
              in
              g )
          in
          Pnode a (Cons (f l) (repr_list r))
      
  in
  f 

spec = 
  let
    f x = 
      case x of
        Pelt a -> a
        Pnode a l -> 
          let
            maxh_aux =
              (let
                g y = 
                  case y of
                    Elt a -> f a
                    Cons hd tl -> (f hd) +  (g tl)
                  
              in
              g )
          in
          let
            res =
              (maxh_aux l)
          in
          max' a res
      
  in
  f 

target = 
  let
    f t = 
      let
        aux =
          (let
            g t = 
              case t of
                Telt w -> 
                  evalU0_0 (RefEnv [("w", (Env0 w))]) ((genSym (2::Int) "hole0") :: (UnionM Expr0_0))
                Tnode w l r -> 
                  let
                    tmp1 =
                      (f l)
                  in
                  let
                    tmp2 =
                      (g r)
                  in
                  evalU1_0 (RefEnv [("tmp2", (Env1 tmp2)), ("tmp1", (Env1 tmp1)), ("w", (Env0 w))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))
              
          in
          g )
      in
      case t of
        Telt w -> 
          evalU2_0 (RefEnv [("w", (Env0 w))]) ((genSym (2::Int) "hole2") :: (UnionM Expr2_0))
        Tnode w l r -> 
          let
            tmp3 =
              (f l)
          in
          let
            tmp4 =
              (aux r)
          in
          evalU3_0 (RefEnv [("tmp3", (Env1 tmp3)), ("tmp4", (Env1 tmp4)), ("w", (Env0 w))]) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))
      
  in
  f 

main' t = 
  let
    tmp5 =
      (target t)
  in
  evalU4_0 (RefEnv [("tmp5", (Env1 tmp5))]) ((genSym (2::Int) "hole4") :: (UnionM Expr4_0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(Tree, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(Tree, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((Telt (-4))), (-4))
                , (((Tnode ((-2)) ((Tnode ((5)) ((Telt (3))) ((Telt (5))))) ((Tnode ((-5)) ((Telt (1))) ((Tnode ((-5)) ((Telt (3))) ((Telt (5))))))))), (7))
                , (((Tnode ((3)) ((Tnode ((5)) ((Telt (-5))) ((Tnode ((4)) ((Tnode ((4)) ((Telt (-4))) ((Telt (1))))) ((Telt (-2))))))) ((Telt (-1))))), (4))
                , (((Tnode ((1)) ((Telt (3))) ((Tnode ((4)) ((Telt (-1))) ((Telt (-2))))))), (4))
                , (((Tnode ((-4)) ((Tnode ((0)) ((Telt (-3))) ((Telt (5))))) ((Tnode ((2)) ((Telt (4))) ((Tnode ((-5)) ((Telt (-3))) ((Telt (-5))))))))), (-4))
                , (((Tnode ((-4)) ((Telt (-3))) ((Tnode ((0)) ((Telt (2))) ((Telt (-3))))))), (-4))
                , (((Telt (-3))), (-3))
                , (((Tnode ((5)) ((Telt (2))) ((Telt (-1))))), (5))
                , (((Tnode ((-2)) ((Tnode ((0)) ((Telt (-4))) ((Telt (1))))) ((Tnode ((-1)) ((Telt (-3))) ((Tnode ((-5)) ((Telt (-1))) ((Telt (5))))))))), (-2))
                , (((Tnode ((1)) ((Tnode ((1)) ((Telt (-2))) ((Telt (-4))))) ((Telt (1))))), (2))
                , (((Tnode ((-2)) ((Tnode ((-4)) ((Tnode ((-3)) ((Telt (-3))) ((Telt (-3))))) ((Telt (1))))) ((Tnode ((3)) ((Telt (3))) ((Telt (-4))))))), (0))
                , (((Tnode ((-4)) ((Telt (0))) ((Telt (5))))), (5))
                , (((Telt (3))), (3))
                , (((Tnode ((3)) ((Tnode ((-5)) ((Telt (-4))) ((Telt (-2))))) ((Tnode ((-3)) ((Telt (-1))) ((Telt (-2))))))), (3))
                , (((Telt (0))), (0))
                , (((Telt (-4))), (-4))
                , (((Tnode ((2)) ((Telt (-1))) ((Tnode ((-4)) ((Telt (1))) ((Tnode ((-1)) ((Telt (3))) ((Tnode ((1)) ((Telt (4))) ((Telt (-2))))))))))), (2))
                , (((Tnode ((-1)) ((Telt (-3))) ((Telt (-2))))), (-1))
                , (((Tnode ((-2)) ((Telt (-4))) ((Telt (2))))), (-2))
                , (((Tnode ((-5)) ((Telt (4))) ((Tnode ((0)) ((Tnode ((2)) ((Telt (-4))) ((Telt (-3))))) ((Tnode ((2)) ((Telt (-1))) ((Telt (2))))))))), (9))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
