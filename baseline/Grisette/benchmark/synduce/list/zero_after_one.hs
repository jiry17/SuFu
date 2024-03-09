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
  = Env0 SymBool
  | Env1 (SymBool, SymBool, SymBool, SymBool)
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

evalVar0 :: RefEnv -> Ident -> SymBool
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> (SymBool, SymBool, SymBool, SymBool)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymBool
(SymBool, SymBool, SymBool, SymBool)
-}

-- output_type: {Bool,Bool,Bool,Bool}
-- param_list: xs tmp1
data Expr0_0
  = Prod0_0 (UnionM Expr0_2) (UnionM Expr0_2) (UnionM Expr0_2) (UnionM Expr0_2)
  | CFalse40_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Access0_0_0_2 (UnionM Expr0_0)
  | Access1_0_0_2 (UnionM Expr0_0)
  | Access2_0_0_2 (UnionM Expr0_0)
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

-- output_type: {Bool,Bool,Bool,Bool}
-- param_list: a tmp2 xs
data Expr1_0
  = Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  | CFalse41_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access2_0_1_1 (UnionM Expr1_0)
  | Access3_0_1_1 (UnionM Expr1_0)
  | Ceq1_1 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cless1_1 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cleq1_1 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cand1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cor1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cnot1_1 (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Cadd1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Csub1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Czero1_2
  | CIte1_2 (UnionM Expr1_1) (UnionM Expr1_2) (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: {Bool,Bool,Bool,Bool}
-- param_list: b a tmp3 tmp4 xs
data Expr2_0
  = Param22_0
  | Param32_0
  | Prod2_0 (UnionM Expr2_2) (UnionM Expr2_2) (UnionM Expr2_2) (UnionM Expr2_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Access0_0_2_2 (UnionM Expr2_0)
  | Access1_0_2_2 (UnionM Expr2_0)
  | Access2_0_2_2 (UnionM Expr2_0)
  | Access3_0_2_2 (UnionM Expr2_0)
  | Ceq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: Bool
-- param_list: xs tmp5
data Expr3_0
  = Access0_1_3_0 (UnionM Expr3_1)
  | Access1_1_3_0 (UnionM Expr3_1)
  | Access2_1_3_0 (UnionM Expr3_1)
  | Access3_1_3_0 (UnionM Expr3_1)
  | Ceq3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cless3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cleq3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cand3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cor3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cnot3_0 (UnionM Expr3_0)
  | CFalse3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Prod3_1 (UnionM Expr3_0) (UnionM Expr3_0) (UnionM Expr3_0) (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Cadd3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Csub3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Czero3_2
  | CIte3_2 (UnionM Expr3_0) (UnionM Expr3_2) (UnionM Expr3_2)
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
    genSingle0 = [mrgCFalse40_0]
    genSingle1 = [mrgCzero0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e2_0 e2_1 e2_2 e2_3])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5])
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
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_0_2 e0_0] ++ [mrgAccess1_0_0_2 e0_1] ++ [mrgAccess2_0_0_2 e0_2] ++ [mrgAccess3_0_0_2 e0_3] ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse41_0]
    genSingle1 = [mrgParam01_1]
    genSingle2 = [mrgCzero1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e1_0 e1_1 e1_2 e1_3])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgAccess2_0_1_1 e0_2] ++ [mrgAccess3_0_1_1 e0_3] ++ [mrgCeq1_1 e2_0 e2_1] ++ [mrgCless1_1 e2_2 e2_3] ++ [mrgCleq1_1 e2_4 e2_5] ++ [mrgCand1_1 e1_0 e1_1] ++ [mrgCor1_1 e1_2 e1_3] ++ [mrgCnot1_1 e1_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd1_2 e2_0 e2_1] ++ [mrgCsub1_2 e2_2 e2_3] ++ [mrgCIte1_2 e1_0 e2_4 e2_5])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam22_0] ++ [mrgParam32_0]
    genSingle1 = [mrgCzero2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e2_0 e2_1 e2_2 e2_3])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5])
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
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_2_2 e0_0] ++ [mrgAccess1_0_2_2 e0_1] ++ [mrgAccess2_0_2_2 e0_2] ++ [mrgAccess3_0_2_2 e0_3] ++ [mrgCeq2_2 e1_0 e1_1] ++ [mrgCless2_2 e1_2 e1_3] ++ [mrgCleq2_2 e1_4 e1_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse3_0]
    genSingle1 = [mrgParam13_1]
    genSingle2 = [mrgCzero3_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_1_3_0 e1_0] ++ [mrgAccess1_1_3_0 e1_1] ++ [mrgAccess2_1_3_0 e1_2] ++ [mrgAccess3_1_3_0 e1_3] ++ [mrgCeq3_0 e2_0 e2_1] ++ [mrgCless3_0 e2_2 e2_3] ++ [mrgCleq3_0 e2_4 e2_5] ++ [mrgCand3_0 e0_0 e0_1] ++ [mrgCor3_0 e0_2 e0_3] ++ [mrgCnot3_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd3_1 e0_0 e0_1 e0_2 e0_3])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd3_2 e2_0 e2_1] ++ [mrgCsub3_2 e2_2 e2_3] ++ [mrgCIte3_2 e0_0 e2_4 e2_5])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymBool, SymBool, SymBool)
eval0_0 env (Prod0_0 p0 p1 p2 p3) = ((evalU0_2 env p0), (evalU0_2 env p1), (evalU0_2 env p2), (evalU0_2 env p3))
eval0_0 env (CFalse40_0) = ((toSym False),(toSym False),(toSym False),(toSym False))

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymBool, SymBool, SymBool)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Access0_0_0_2 p0) = get1from4 (evalU0_0 env p0)
eval0_2 env (Access1_0_0_2 p0) = get2from4 (evalU0_0 env p0)
eval0_2 env (Access2_0_0_2 p0) = get3from4 (evalU0_0 env p0)
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

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymBool, SymBool, SymBool)
eval1_0 env (Prod1_0 p0 p1 p2 p3) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2), (evalU1_1 env p3))
eval1_0 env (CFalse41_0) = ((toSym False),(toSym False),(toSym False),(toSym False))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymBool, SymBool, SymBool)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymBool
eval1_1 env (Param01_1) = evalVar0 env "a"
eval1_1 env (Access0_0_1_1 p0) = get1from4 (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = get2from4 (evalU1_0 env p0)
eval1_1 env (Access2_0_1_1 p0) = get3from4 (evalU1_0 env p0)
eval1_1 env (Access3_0_1_1 p0) = get4from4 (evalU1_0 env p0)
eval1_1 env (Ceq1_1 p0 p1) =  (evalU1_2 env p0) ==~ (evalU1_2 env p1) 
eval1_1 env (Cless1_1 p0 p1) =  (evalU1_2 env p0) <~ (evalU1_2 env p1) 
eval1_1 env (Cleq1_1 p0 p1) =  (evalU1_2 env p0) <=~ (evalU1_2 env p1) 
eval1_1 env (Cand1_1 p0 p1) =  (evalU1_1 env p0) &&~ (evalU1_1 env p1) 
eval1_1 env (Cor1_1 p0 p1) =  (evalU1_1 env p0) ||~ (evalU1_1 env p1) 
eval1_1 env (Cnot1_1 p0) =  mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymBool
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymInteger
eval1_2 env (Cadd1_2 p0 p1) =  (evalU1_2 env p0) + (evalU1_2 env p1) 
eval1_2 env (Csub1_2 p0 p1) =  (evalU1_2 env p0) - (evalU1_2 env p1) 
eval1_2 env (Czero1_2) = 0
eval1_2 env (CIte1_2 p0 p1 p2) = mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (evalU1_2 env p1) (evalU1_2 env p2)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymInteger
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymBool, SymBool, SymBool, SymBool)
eval2_0 env (Param22_0) = evalVar1 env "tmp3"
eval2_0 env (Param32_0) = evalVar1 env "tmp4"
eval2_0 env (Prod2_0 p0 p1 p2 p3) = ((evalU2_2 env p0), (evalU2_2 env p1), (evalU2_2 env p2), (evalU2_2 env p3))

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymBool, SymBool, SymBool, SymBool)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Access0_0_2_2 p0) = get1from4 (evalU2_0 env p0)
eval2_2 env (Access1_0_2_2 p0) = get2from4 (evalU2_0 env p0)
eval2_2 env (Access2_0_2_2 p0) = get3from4 (evalU2_0 env p0)
eval2_2 env (Access3_0_2_2 p0) = get4from4 (evalU2_0 env p0)
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymBool
eval3_0 env (Access0_1_3_0 p0) = get1from4 (evalU3_1 env p0)
eval3_0 env (Access1_1_3_0 p0) = get2from4 (evalU3_1 env p0)
eval3_0 env (Access2_1_3_0 p0) = get3from4 (evalU3_1 env p0)
eval3_0 env (Access3_1_3_0 p0) = get4from4 (evalU3_1 env p0)
eval3_0 env (Ceq3_0 p0 p1) =  (evalU3_2 env p0) ==~ (evalU3_2 env p1) 
eval3_0 env (Cless3_0 p0 p1) =  (evalU3_2 env p0) <~ (evalU3_2 env p1) 
eval3_0 env (Cleq3_0 p0 p1) =  (evalU3_2 env p0) <=~ (evalU3_2 env p1) 
eval3_0 env (Cand3_0 p0 p1) =  (evalU3_0 env p0) &&~ (evalU3_0 env p1) 
eval3_0 env (Cor3_0 p0 p1) =  (evalU3_0 env p0) ||~ (evalU3_0 env p1) 
eval3_0 env (Cnot3_0 p0) =  mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_0 env (CFalse3_0) = (toSym False)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymBool
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (SymBool, SymBool, SymBool, SymBool)
eval3_1 env (Param13_1) = evalVar1 env "tmp5"
eval3_1 env (Prod3_1 p0 p1 p2 p3) = ((evalU3_0 env p0), (evalU3_0 env p1), (evalU3_0 env p2), (evalU3_0 env p3))

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (SymBool, SymBool, SymBool, SymBool)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymInteger
eval3_2 env (Cadd3_2 p0 p1) =  (evalU3_2 env p0) + (evalU3_2 env p1) 
eval3_2 env (Csub3_2 p0 p1) =  (evalU3_2 env p0) - (evalU3_2 env p1) 
eval3_2 env (Czero3_2) = 0
eval3_2 env (CIte3_2 p0 p1 p2) = mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (evalU3_2 env p1) (evalU3_2 env p2)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymInteger
evalU3_2 env = onUnion (eval3_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs tmp1 

Hole grammar for #1
a tmp2 xs 

Hole grammar for #2
b a tmp3 tmp4 xs 

Hole grammar for #3
xs tmp5 
-}

data BList
  = Nil Unit
  | Cons SymBool BList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon BList, ExtractSymbolics)
    via (Default BList)

data CList
  = Emp Unit
  | Single SymBool
  | Concat CList CList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon CList, ExtractSymbolics)
    via (Default CList)
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

cat_list = 
  let
    f xs ys = 
      case xs of
        Nil _ -> ys
        Cons hd tl -> Cons hd (f tl ys)
      
  in
  f 

repr = 
  let
    f xs = 
      case xs of
        Emp _ -> 
          let
            tmp1 =
              (Nil Unit)
          in
          evalU0_0 (RefEnv []) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
        Single a -> 
          let
            tmp2 =
              (Cons a (Nil Unit))
          in
          evalU1_0 (RefEnv [("a", (Env0 a))]) ((genSym (3::Int) "hole1") :: (UnionM Expr1_0))
        Concat a b -> 
          let
            tmp3 =
              (f a)
          in
          let
            tmp4 =
              (f b)
          in
          evalU2_0 (RefEnv [("tmp3", (Env1 tmp3)), ("tmp4", (Env1 tmp4))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0))
      
  in
  f 

f :: BList -> (SymBool, SymBool, SymBool)
f xs = 
      case xs of
        Nil _ -> ((toSym False), (toSym False), (toSym False))
        Cons hd tl -> 
          let
            result =
              (f tl)
          in
          let
            new_seen1 =
              ((get1from3 result) ||~  hd)
          in
          let
            new_res =
              ((get2from3 result) ||~  ((get1from3 result) &&~  (mrgIte ((hd) ==~ (toSym True)) (toSym False) (toSym True))))
          in
          let
            new_aux =
              ((get3from3 result) ||~  (mrgIte ((hd) ==~ (toSym True)) (toSym False) (toSym True)))
          in
          (new_seen1, new_res, new_aux)

spec xs = 
  (get2from3 (f xs))

main' xs = 
  let
    tmp5 =
      (repr xs)
  in
  evalU3_0 (RefEnv [("tmp5", (Env1 tmp5))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(CList, Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(CList, Bool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((Emp Unit)), (toSym False))
                , (((Concat ((Emp Unit)) ((Concat ((Concat ((Single (toSym False))) ((Single (toSym False))))) ((Single (toSym True))))))), (toSym True))
                , (((Single (toSym False))), (toSym False))
                , (((Concat ((Concat ((Single (toSym True))) ((Concat ((Concat ((Emp Unit)) ((Single (toSym False))))) ((Emp Unit)))))) ((Emp Unit)))), (toSym False))
                , (((Concat ((Concat ((Single (toSym True))) ((Concat ((Single (toSym True))) ((Emp Unit)))))) ((Concat ((Single (toSym False))) ((Emp Unit)))))), (toSym False))
                , (((Concat ((Concat ((Emp Unit)) ((Concat ((Concat ((Single (toSym True))) ((Single (toSym False))))) ((Single (toSym False))))))) ((Emp Unit)))), (toSym False))
                , (((Concat ((Concat ((Single (toSym False))) ((Emp Unit)))) ((Concat ((Single (toSym True))) ((Single (toSym True))))))), (toSym True))
                , (((Concat ((Concat ((Emp Unit)) ((Single (toSym False))))) ((Concat ((Emp Unit)) ((Concat ((Emp Unit)) ((Emp Unit)))))))), (toSym False))
                , (((Concat ((Single (toSym True))) ((Emp Unit)))), (toSym False))
                , (((Concat ((Emp Unit)) ((Concat ((Emp Unit)) ((Concat ((Emp Unit)) ((Emp Unit)))))))), (toSym False))
                , (((Concat ((Concat ((Concat ((Emp Unit)) ((Emp Unit)))) ((Emp Unit)))) ((Single (toSym False))))), (toSym False))
                , (((Concat ((Concat ((Emp Unit)) ((Single (toSym False))))) ((Concat ((Single (toSym True))) ((Single (toSym False))))))), (toSym True))
                , (((Concat ((Concat ((Emp Unit)) ((Emp Unit)))) ((Concat ((Concat ((Emp Unit)) ((Single (toSym True))))) ((Single (toSym False))))))), (toSym False))
                , (((Concat ((Emp Unit)) ((Concat ((Concat ((Single (toSym True))) ((Single (toSym True))))) ((Single (toSym False))))))), (toSym False))
                , (((Concat ((Concat ((Concat ((Single (toSym False))) ((Emp Unit)))) ((Emp Unit)))) ((Concat ((Single (toSym False))) ((Single (toSym False))))))), (toSym False))
                , (((Emp Unit)), (toSym False))
                , (((Concat ((Emp Unit)) ((Single (toSym False))))), (toSym False))
                , (((Concat ((Emp Unit)) ((Concat ((Emp Unit)) ((Single (toSym False))))))), (toSym False))
                , (((Concat ((Concat ((Emp Unit)) ((Emp Unit)))) ((Concat ((Emp Unit)) ((Single (toSym False))))))), (toSym False))
                , (((Concat ((Single (toSym False))) ((Emp Unit)))), (toSym False))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
