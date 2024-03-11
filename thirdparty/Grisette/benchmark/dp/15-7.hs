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
  = Env0 (SymBool, SymInteger, SymInteger, SymBool)
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

evalVar0 :: RefEnv -> Ident -> (SymBool, SymInteger, SymInteger, SymBool)
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
(SymBool, SymInteger, SymInteger, SymBool)
SymInteger
-}

-- output_type: {Bool,Int,Int,Bool}
-- param_list: target h xs t w c
data Expr0_0
  = Param10_0
  | Prod0_0 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param00_1
  | Param40_1
  | Param50_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Access2_0_0_1 (UnionM Expr0_0)
  | One0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Access0_0_0_2 (UnionM Expr0_0)
  | Access3_0_0_2 (UnionM Expr0_0)
  | Ceq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cless0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cleq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cand0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cor0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cnot0_2 (UnionM Expr0_2)
  | CFalse0_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: {Bool,Int,Int,Bool}
-- param_list: cs global0 graph global1 xs tmp1 full_graph
data Expr1_0
  = Prod1_0 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_2)
  | CBIIB1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param31_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access2_0_1_1 (UnionM Expr1_0)
  | One1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Access0_0_1_2 (UnionM Expr1_0)
  | Access3_0_1_2 (UnionM Expr1_0)
  | Ceq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  | CFalse1_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: Bool
-- param_list: h xs t global0 global1
data Expr2_0
  = Access0_2_2_0 (UnionM Expr2_2)
  | Access3_2_2_0 (UnionM Expr2_2)
  | Ceq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cor2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cnot2_0 (UnionM Expr2_0)
  | CFalse2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param42_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_0) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access1_2_2_1 (UnionM Expr2_2)
  | Access2_2_2_1 (UnionM Expr2_2)
  | Max2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | One2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Param02_2
  | Prod2_2 (UnionM Expr2_0) (UnionM Expr2_1) (UnionM Expr2_1) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: Int
-- param_list: h xs t global0 global1
data Expr3_0
  = Param43_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_2) (UnionM Expr3_0) (UnionM Expr3_0)
  | Access1_1_3_0 (UnionM Expr3_1)
  | Access2_1_3_0 (UnionM Expr3_1)
  | Max3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | One3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param03_1
  | Prod3_1 (UnionM Expr3_2) (UnionM Expr3_0) (UnionM Expr3_0) (UnionM Expr3_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Access0_1_3_2 (UnionM Expr3_1)
  | Access3_1_3_2 (UnionM Expr3_1)
  | Ceq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cless3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cleq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
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
    genSingle0 = [mrgParam10_0]
    genSingle1 = [mrgParam00_1] ++ [mrgParam40_1] ++ [mrgParam50_1] ++ [mrgCzero0_1] ++ [mrgOne0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e2_0 e1_0 e1_1 e2_1])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_0_1 e0_0] ++ [mrgAccess2_0_0_1 e0_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_0_2 e0_0] ++ [mrgAccess3_0_0_2 e0_1] ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCBIIB1_0]
    genSingle1 = [mrgParam31_1] ++ [mrgCzero1_1] ++ [mrgOne1_1]
    genSingle2 = [mrgCFalse1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e2_0 e1_0 e1_1 e2_1])
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_1_1 e0_0] ++ [mrgAccess2_0_1_1 e0_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_1_2 e0_0] ++ [mrgAccess3_0_1_2 e0_1] ++ [mrgCeq1_2 e1_0 e1_1] ++ [mrgCless1_2 e1_2 e1_3] ++ [mrgCleq1_2 e1_4 e1_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse2_0]
    genSingle1 = [mrgParam42_1] ++ [mrgCzero2_1] ++ [mrgOne2_1]
    genSingle2 = [mrgParam02_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_2_2_0 e2_0] ++ [mrgAccess3_2_2_0 e2_1] ++ [mrgCeq2_0 e1_0 e1_1] ++ [mrgCless2_0 e1_2 e1_3] ++ [mrgCleq2_0 e1_4 e1_5] ++ [mrgCand2_0 e0_0 e0_1] ++ [mrgCor2_0 e0_2 e0_3] ++ [mrgCnot2_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e0_0 e1_4 e1_5] ++ [mrgAccess1_2_2_1 e2_0] ++ [mrgAccess2_2_2_1 e2_1] ++ [mrgMax2_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd2_2 e0_0 e1_0 e1_1 e0_1])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam43_0] ++ [mrgCzero3_0] ++ [mrgOne3_0]
    genSingle1 = [mrgParam03_1]
    genSingle2 = [mrgCFalse3_2]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_3_0 e1_0] ++ [mrgAccess2_1_3_0 e1_1] ++ [mrgMax3_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd3_1 e2_0 e0_0 e0_1 e2_1])
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
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_1_3_2 e1_0] ++ [mrgAccess3_1_3_2 e1_1] ++ [mrgCeq3_2 e0_0 e0_1] ++ [mrgCless3_2 e0_2 e0_3] ++ [mrgCleq3_2 e0_4 e0_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymInteger, SymInteger, SymBool)
eval0_0 env (Param10_0) = evalVar0 env "h"
eval0_0 env (Prod0_0 p0 p1 p2 p3) = ((evalU0_2 env p0), (evalU0_1 env p1), (evalU0_1 env p2), (evalU0_2 env p3))

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymInteger, SymInteger, SymBool)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Param00_1) = evalVar1 env "target"
eval0_1 env (Param40_1) = evalVar1 env "w"
eval0_1 env (Param50_1) = evalVar1 env "c"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access1_0_0_1 p0) = get2from4 (evalU0_0 env p0)
eval0_1 env (Access2_0_0_1 p0) = get3from4 (evalU0_0 env p0)
eval0_1 env (One0_1) = one

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Access0_0_0_2 p0) = get1from4 (evalU0_0 env p0)
eval0_2 env (Access3_0_0_2 p0) = get4from4 (evalU0_0 env p0)
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymInteger, SymInteger, SymBool)
eval1_0 env (Prod1_0 p0 p1 p2 p3) = ((evalU1_2 env p0), (evalU1_1 env p1), (evalU1_1 env p2), (evalU1_2 env p3))
eval1_0 env (CBIIB1_0) = ((toSym False),0,0,(toSym False))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymInteger, SymInteger, SymBool)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param31_1) = evalVar1 env "global1"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access1_0_1_1 p0) = get2from4 (evalU1_0 env p0)
eval1_1 env (Access2_0_1_1 p0) = get3from4 (evalU1_0 env p0)
eval1_1 env (One1_1) = one

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Access0_0_1_2 p0) = get1from4 (evalU1_0 env p0)
eval1_2 env (Access3_0_1_2 p0) = get4from4 (evalU1_0 env p0)
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Access0_2_2_0 p0) = get1from4 (evalU2_2 env p0)
eval2_0 env (Access3_2_2_0 p0) = get4from4 (evalU2_2 env p0)
eval2_0 env (Ceq2_0 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_0 env (Cless2_0 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_0 env (Cleq2_0 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_0 env (Cand2_0 p0 p1) =  (evalU2_0 env p0) &&~ (evalU2_0 env p1) 
eval2_0 env (Cor2_0 p0 p1) =  (evalU2_0 env p0) ||~ (evalU2_0 env p1) 
eval2_0 env (Cnot2_0 p0) =  mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_0 env (CFalse2_0) = (toSym False)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymBool
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param42_1) = evalVar1 env "global1"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access1_2_2_1 p0) = get2from4 (evalU2_2 env p0)
eval2_1 env (Access2_2_2_1 p0) = get3from4 (evalU2_2 env p0)
eval2_1 env (Max2_1 p0 p1) = max' (evalU2_1 env p0) (evalU2_1 env p1)
eval2_1 env (One2_1) = one

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> (SymBool, SymInteger, SymInteger, SymBool)
eval2_2 env (Param02_2) = evalVar0 env "h"
eval2_2 env (Prod2_2 p0 p1 p2 p3) = ((evalU2_0 env p0), (evalU2_1 env p1), (evalU2_1 env p2), (evalU2_0 env p3))

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> (SymBool, SymInteger, SymInteger, SymBool)
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Param43_0) = evalVar1 env "global1"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Access1_1_3_0 p0) = get2from4 (evalU3_1 env p0)
eval3_0 env (Access2_1_3_0 p0) = get3from4 (evalU3_1 env p0)
eval3_0 env (Max3_0 p0 p1) = max' (evalU3_0 env p0) (evalU3_0 env p1)
eval3_0 env (One3_0) = one

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymInteger
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (SymBool, SymInteger, SymInteger, SymBool)
eval3_1 env (Param03_1) = evalVar0 env "h"
eval3_1 env (Prod3_1 p0 p1 p2 p3) = ((evalU3_2 env p0), (evalU3_0 env p1), (evalU3_0 env p2), (evalU3_2 env p3))

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (SymBool, SymInteger, SymInteger, SymBool)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Access0_1_3_2 p0) = get1from4 (evalU3_1 env p0)
eval3_2 env (Access3_1_3_2 p0) = get4from4 (evalU3_1 env p0)
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_0 env p0) ==~ (evalU3_0 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_0 env p0) <~ (evalU3_0 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_0 env p0) <=~ (evalU3_0 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
target h xs t w c 

Hole grammar for #1
cs global0 graph global1 xs tmp1 full_graph 

Hole grammar for #2
h xs t global0 global1 

Hole grammar for #3
h xs t global0 global1 
-}

type Plan = (SymBool, SymInteger, SymInteger, SymBool)

data List
  = Nil Unit
  | Cons SymInteger List
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data EdgeList
  = Enil Unit
  | Econs SymInteger SymInteger EdgeList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon EdgeList, ExtractSymbolics)
    via (Default EdgeList)

data Graph
  = Gnil Unit
  | Gcons EdgeList Graph
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon Graph, ExtractSymbolics)
    via (Default Graph)

data PlanList
  = Pnil Unit
  | Pcons Plan PlanList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon PlanList)
    via (Default PlanList)

instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go cond (Cons l1 r1) (Nil r2) = Cons l1 r1
      go cond (Nil l) (Cons l2 r2) = Cons l2 r2
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

graph_size :: Graph -> SymInteger
graph_size = 
  let
    f g = 
      case g of
        Gnil _ -> 0
        Gcons h t -> 1 +  (f t)
  in
  f 

length' = 
  let
    f xs = 
      case xs of
        Enil _ -> 0
        Econs _ _ t -> 1 +  (f t)
      
  in
  f 

size = 
  let
    f xs = 
      case xs of
        Nil _ -> 0
        Cons h t -> 1 +  (f t)
      
  in
  f 

is_valid_graph g = 
  let
    size =
      (graph_size g)
  in
  let
    f g = 
      case g of
        Gnil _ -> (toSym True)
        Gcons h t -> mrgIte (size ==~  (length' h))
            (f t)
            ((toSym False))
      
  in
  f  g

one = 
  1

take_last xs num = 
  (snd (let
    f xs = 
      case xs of
        Nil _ -> (0, xs)
        Cons h t -> 
          let
            res =
              (f t)
          in
          mrgIte ((fst res) <~  num)
            ((((fst res) +  1), (Cons h (snd res))))
            (res)
      
  in
  f  xs))

merge' = 
  let
    f xs ys = 
      case xs of
        Pnil _ -> ys
        Pcons h t -> Pcons h (f t ys)
      
  in
  f 

extend c w target = 
  let
    f xs = 
      case xs of
        Pnil _ -> Pnil Unit
        Pcons h t -> Pcons (evalU0_0 (RefEnv [("h", (Env0 h)), ("target", (Env1 target)), ("w", (Env1 w)), ("c", (Env1 c))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))) (f t)
      
  in
  f 

generate_paths global0 global1 full_graph = 
  let
    gen =
      (let
        f xs graph cs = 
          case cs of
            Nil _ -> Pcons (let
                tmp1 =
                  (Enil Unit)
              in
              evalU1_0 (RefEnv [("global1", (Env1 global1))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))) (Pnil Unit)
            Cons c ct -> 
              case (xs, graph) of
                ((Econs a b t), (Gcons next_edges remains)) -> merge' (extend a b c (f next_edges full_graph ct)) (f t remains cs)
                _ -> Pnil Unit
      in
      f )
  in
  case full_graph of
    Gnil _ -> Pnil Unit
    Gcons h _ -> gen h full_graph global0
  

is_path_match = 
  let
    f p xs = 
      case (p, xs) of
        ((Enil _), (Nil _)) -> (toSym True)
        ((Econs _ h1 t1), (Cons h2 t2)) -> (h1 ==~  h2) &&~  (f t1 t2)
        (_, _) -> (toSym False)
      
  in
  f 

eval = 
  let
    f path = 
      case path of
        Econs _ w t -> w +  (f t)
        Enil _ -> 0
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

get_best global0 global1 = 
  let
    f xs = 
      case xs of
        Pnil _ -> 0
        Pcons h t -> mrgIte (evalU2_0 (RefEnv [("global1", (Env1 global1)), ("h", (Env0 h))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0)))
            (max' (evalU3_0 (RefEnv [("global1", (Env1 global1)), ("h", (Env0 h))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0))) (f t))
            (f t)
      
  in
  f 

main' global0 global1 graph = 
  mrgIte ((is_valid_graph graph) &&~  ((size global0) ==~  global1))
    (get_best global0 global1 (generate_paths global0 global1 graph))
    (0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [((List, SymInteger, Graph), Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [((List, SymInteger, Graph), Integer)] -> SymBool
        constraint [] = con True
        constraint (((x1,x2,x3), y) : xs) = main' x1 x2 x3 ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Cons ((2)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((0)) ((Econs ((3)) ((2)) ((Enil Unit)))))) ((Gcons ((Econs ((2)) ((1)) ((Econs ((0)) ((4)) ((Enil Unit)))))) ((Gnil Unit))))))), (2))
                , ((((Cons ((5)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((2)) ((Econs ((3)) ((5)) ((Enil Unit)))))) ((Gcons ((Econs ((0)) ((3)) ((Econs ((2)) ((3)) ((Enil Unit)))))) ((Gnil Unit))))))), (5))
                , ((((Cons ((4)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((0)) ((Econs ((2)) ((4)) ((Enil Unit)))))) ((Gcons ((Econs ((0)) ((0)) ((Econs ((4)) ((0)) ((Enil Unit)))))) ((Gnil Unit))))))), (4))
                , ((((Cons ((5)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((2)) ((5)) ((Econs ((3)) ((3)) ((Enil Unit)))))) ((Gcons ((Econs ((4)) ((0)) ((Econs ((0)) ((0)) ((Enil Unit)))))) ((Gnil Unit))))))), (5))
                , ((((Cons ((4)) ((Cons ((4)) ((Cons ((4)) ((Cons ((4)) ((Cons ((4)) ((Nil Unit)))))))))))), ((5)), ((Gcons ((Econs ((0)) ((4)) ((Enil Unit)))) ((Gnil Unit))))), (20))
                , ((((Cons ((2)) ((Cons ((2)) ((Cons ((2)) ((Nil Unit)))))))), ((3)), ((Gcons ((Econs ((2)) ((2)) ((Enil Unit)))) ((Gnil Unit))))), (6))
                , ((((Cons ((5)) ((Cons ((5)) ((Cons ((5)) ((Nil Unit)))))))), ((3)), ((Gcons ((Econs ((0)) ((5)) ((Enil Unit)))) ((Gnil Unit))))), (15))
                , ((((Cons ((1)) ((Cons ((1)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((0)) ((1)) ((Enil Unit)))) ((Gnil Unit))))), (2))
                , ((((Cons ((3)) ((Cons ((3)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((3)) ((3)) ((Enil Unit)))) ((Gnil Unit))))), (6))
                , ((((Cons ((1)) ((Cons ((1)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((1)) ((1)) ((Enil Unit)))) ((Gnil Unit))))), (2))
                , ((((Cons ((2)) ((Cons ((2)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((0)) ((2)) ((Enil Unit)))) ((Gnil Unit))))), (4))
                , ((((Cons ((5)) ((Cons ((5)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((4)) ((5)) ((Enil Unit)))) ((Gnil Unit))))), (10))
                , ((((Cons ((4)) ((Cons ((4)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((4)) ((4)) ((Enil Unit)))) ((Gnil Unit))))), (8))
                , ((((Cons ((2)) ((Cons ((2)) ((Nil Unit)))))), ((2)), ((Gcons ((Econs ((1)) ((2)) ((Enil Unit)))) ((Gnil Unit))))), (4))
                , ((((Cons ((1)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((2)) ((1)) ((Enil Unit)))) ((Gnil Unit))))), (1))
                , ((((Cons ((3)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((3)) ((Enil Unit)))) ((Gnil Unit))))), (3))
                , ((((Cons ((3)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((3)) ((Enil Unit)))) ((Gnil Unit))))), (3))
                , ((((Cons ((3)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((4)) ((3)) ((Enil Unit)))) ((Gnil Unit))))), (3))
                , ((((Cons ((3)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((3)) ((3)) ((Enil Unit)))) ((Gnil Unit))))), (3))
                , ((((Cons ((2)) ((Nil Unit)))), ((1)), ((Gcons ((Econs ((3)) ((2)) ((Enil Unit)))) ((Gnil Unit))))), (2))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
