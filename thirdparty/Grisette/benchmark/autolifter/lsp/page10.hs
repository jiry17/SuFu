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
  | Env2 ((SymInteger, SymInteger, SymInteger), SymInteger)
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

evalVar2 :: RefEnv -> Ident -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalVar2 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env2 sym -> sym
      _ -> error "evalVar2: variable type not matched"

{- env_type_list: 
SymInteger
(SymInteger, SymInteger, SymInteger)
((SymInteger, SymInteger, SymInteger), SymInteger)
-}

-- output_type: Bool
-- param_list: res h x ms t
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
  = Param10_1
  | Param30_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access1_3_0_1 (UnionM Expr0_3)
  | Access0_2_0_1 (UnionM Expr0_2)
  | Access1_2_0_1 (UnionM Expr0_2)
  | Access2_2_0_1 (UnionM Expr0_2)
  | Max0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Param20_2
  | Access0_3_0_2 (UnionM Expr0_3)
  | Prod0_2 (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

data Expr0_3
  = Param00_3
  | Prod0_3 (UnionM Expr0_2) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_3)
    via (Default Expr0_3)

-- output_type: {Int,Int,Int}
-- param_list: res h x ms t
data Expr1_0
  = Param21_0
  | Access0_2_1_0 (UnionM Expr1_2)
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param11_1
  | Param31_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_3) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access1_2_1_1 (UnionM Expr1_2)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access2_0_1_1 (UnionM Expr1_0)
  | Max1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Inf1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Param01_2
  | Prod1_2 (UnionM Expr1_0) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

data Expr1_3
  = Ceq1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cor1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cnot1_3 (UnionM Expr1_3)
  | CFalse1_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_3)
    via (Default Expr1_3)

-- output_type: Int
-- param_list: res h x ms t
data Expr2_0
  = Param12_0
  | Param32_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_3) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access1_1_2_0 (UnionM Expr2_1)
  | Access0_2_2_0 (UnionM Expr2_2)
  | Access1_2_2_0 (UnionM Expr2_2)
  | Access2_2_2_0 (UnionM Expr2_2)
  | Max2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Inf2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param02_1
  | Prod2_1 (UnionM Expr2_2) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Param22_2
  | Access0_1_2_2 (UnionM Expr2_1)
  | Prod2_2 (UnionM Expr2_0) (UnionM Expr2_0) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

data Expr2_3
  = Ceq2_3 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cless2_3 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cleq2_3 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cand2_3 (UnionM Expr2_3) (UnionM Expr2_3)
  | Cor2_3 (UnionM Expr2_3) (UnionM Expr2_3)
  | Cnot2_3 (UnionM Expr2_3)
  | CFalse2_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_3)
    via (Default Expr2_3)

-- output_type: {Int,Int,Int}
-- param_list: x ms tmp1 t h res
data Expr3_0
  = Param03_0
  | Access0_2_3_0 (UnionM Expr3_2)
  | Prod3_0 (UnionM Expr3_1) (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Param43_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_3) (UnionM Expr3_1) (UnionM Expr3_1)
  | Access0_0_3_1 (UnionM Expr3_0)
  | Access1_0_3_1 (UnionM Expr3_0)
  | Access2_0_3_1 (UnionM Expr3_0)
  | Access1_2_3_1 (UnionM Expr3_2)
  | Max3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Inf3_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Param53_2
  | Prod3_2 (UnionM Expr3_0) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

data Expr3_3
  = Ceq3_3 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_3 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_3 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_3 (UnionM Expr3_3) (UnionM Expr3_3)
  | Cor3_3 (UnionM Expr3_3) (UnionM Expr3_3)
  | Cnot3_3 (UnionM Expr3_3)
  | CFalse3_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_3)
    via (Default Expr3_3)

-- output_type: {Int,Int,Int}
-- param_list: x ms tmp2 t h res
data Expr4_0
  = Param04_0
  | Access0_2_4_0 (UnionM Expr4_2)
  | Prod4_0 (UnionM Expr4_1) (UnionM Expr4_1) (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param14_1
  | Param44_1
  | Cadd4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Csub4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Czero4_1
  | CIte4_1 (UnionM Expr4_3) (UnionM Expr4_1) (UnionM Expr4_1)
  | Access0_0_4_1 (UnionM Expr4_0)
  | Access1_0_4_1 (UnionM Expr4_0)
  | Access2_0_4_1 (UnionM Expr4_0)
  | Access1_2_4_1 (UnionM Expr4_2)
  | Max4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Inf4_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Param54_2
  | Prod4_2 (UnionM Expr4_0) (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_2)
    via (Default Expr4_2)

data Expr4_3
  = Ceq4_3 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cless4_3 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cleq4_3 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cand4_3 (UnionM Expr4_3) (UnionM Expr4_3)
  | Cor4_3 (UnionM Expr4_3) (UnionM Expr4_3)
  | Cnot4_3 (UnionM Expr4_3)
  | CFalse4_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_3)
    via (Default Expr4_3)

-- output_type: {Int,Int,Int}
-- param_list: x tmp3
data Expr5_0
  = Prod5_0 (UnionM Expr5_1) (UnionM Expr5_1) (UnionM Expr5_1)
  | CZero35_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Cadd5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Csub5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Czero5_1
  | CIte5_1 (UnionM Expr5_2) (UnionM Expr5_1) (UnionM Expr5_1)
  | Access0_0_5_1 (UnionM Expr5_0)
  | Access1_0_5_1 (UnionM Expr5_0)
  | Access2_0_5_1 (UnionM Expr5_0)
  | Max5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Inf5_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

data Expr5_2
  = Ceq5_2 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cless5_2 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cleq5_2 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cand5_2 (UnionM Expr5_2) (UnionM Expr5_2)
  | Cor5_2 (UnionM Expr5_2) (UnionM Expr5_2)
  | Cnot5_2 (UnionM Expr5_2)
  | CFalse5_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_2)
    via (Default Expr5_2)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr0_3)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr1_3)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr2_3)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr3_2)
$(makeUnionWrapper "mrg" ''Expr3_3)
$(makeUnionWrapper "mrg" ''Expr4_0)
$(makeUnionWrapper "mrg" ''Expr4_1)
$(makeUnionWrapper "mrg" ''Expr4_2)
$(makeUnionWrapper "mrg" ''Expr4_3)
$(makeUnionWrapper "mrg" ''Expr5_0)
$(makeUnionWrapper "mrg" ''Expr5_1)
$(makeUnionWrapper "mrg" ''Expr5_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgParam10_1] ++ [mrgParam30_1] ++ [mrgCzero0_1] ++ [mrgInf0_1]
    genSingle2 = [mrgParam20_2]
    genSingle3 = [mrgParam00_3]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5] ++ [mrgAccess1_3_0_1 e3_0] ++ [mrgAccess0_2_0_1 e2_0] ++ [mrgAccess1_2_0_1 e2_1] ++ [mrgAccess2_2_0_1 e2_2] ++ [mrgMax0_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_3_0_2 e3_0] ++ [mrgProd0_2 e1_0 e1_1 e1_2])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgProd0_3 e2_0 e1_0])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam21_0]
    genSingle1 = [mrgParam11_1] ++ [mrgParam31_1] ++ [mrgCzero1_1] ++ [mrgInf1_1]
    genSingle2 = [mrgParam01_2]
    genSingle3 = [mrgCFalse1_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_2_1_0 e2_0] ++ [mrgProd1_0 e1_0 e1_1 e1_2])
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
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e3_0 e1_4 e1_5] ++ [mrgAccess1_2_1_1 e2_0] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgAccess2_0_1_1 e0_2] ++ [mrgMax1_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd1_2 e0_0 e1_0])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq1_3 e1_0 e1_1] ++ [mrgCless1_3 e1_2 e1_3] ++ [mrgCleq1_3 e1_4 e1_5] ++ [mrgCand1_3 e3_0 e3_1] ++ [mrgCor1_3 e3_2 e3_3] ++ [mrgCnot1_3 e3_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam12_0] ++ [mrgParam32_0] ++ [mrgCzero2_0] ++ [mrgInf2_0]
    genSingle1 = [mrgParam02_1]
    genSingle2 = [mrgParam22_2]
    genSingle3 = [mrgCFalse2_3]
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
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e3_0 e0_4 e0_5] ++ [mrgAccess1_1_2_0 e1_0] ++ [mrgAccess0_2_2_0 e2_0] ++ [mrgAccess1_2_2_0 e2_1] ++ [mrgAccess2_2_2_0 e2_2] ++ [mrgMax2_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd2_1 e2_0 e0_0])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_1_2_2 e1_0] ++ [mrgProd2_2 e0_0 e0_1 e0_2])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq2_3 e0_0 e0_1] ++ [mrgCless2_3 e0_2 e0_3] ++ [mrgCleq2_3 e0_4 e0_5] ++ [mrgCand2_3 e3_0 e3_1] ++ [mrgCor2_3 e3_2 e3_3] ++ [mrgCnot2_3 e3_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam03_0]
    genSingle1 = [mrgParam13_1] ++ [mrgParam43_1] ++ [mrgCzero3_1] ++ [mrgInf3_1]
    genSingle2 = [mrgParam53_2]
    genSingle3 = [mrgCFalse3_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_2_3_0 e2_0] ++ [mrgProd3_0 e1_0 e1_1 e1_2])
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
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e3_0 e1_4 e1_5] ++ [mrgAccess0_0_3_1 e0_0] ++ [mrgAccess1_0_3_1 e0_1] ++ [mrgAccess2_0_3_1 e0_2] ++ [mrgAccess1_2_3_1 e2_0] ++ [mrgMax3_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd3_2 e0_0 e1_0])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq3_3 e1_0 e1_1] ++ [mrgCless3_3 e1_2 e1_3] ++ [mrgCleq3_3 e1_4 e1_5] ++ [mrgCand3_3 e3_0 e3_1] ++ [mrgCor3_3 e3_2 e3_3] ++ [mrgCnot3_3 e3_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam04_0]
    genSingle1 = [mrgParam14_1] ++ [mrgParam44_1] ++ [mrgCzero4_1] ++ [mrgInf4_1]
    genSingle2 = [mrgParam54_2]
    genSingle3 = [mrgCFalse4_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_2_4_0 e2_0] ++ [mrgProd4_0 e1_0 e1_1 e1_2])
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
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd4_1 e1_0 e1_1] ++ [mrgCsub4_1 e1_2 e1_3] ++ [mrgCIte4_1 e3_0 e1_4 e1_5] ++ [mrgAccess0_0_4_1 e0_0] ++ [mrgAccess1_0_4_1 e0_1] ++ [mrgAccess2_0_4_1 e0_2] ++ [mrgAccess1_2_4_1 e2_0] ++ [mrgMax4_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd4_2 e0_0 e1_0])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq4_3 e1_0 e1_1] ++ [mrgCless4_3 e1_2 e1_3] ++ [mrgCleq4_3 e1_4 e1_5] ++ [mrgCand4_3 e3_0 e3_1] ++ [mrgCor4_3 e3_2 e3_3] ++ [mrgCnot4_3 e3_4])
        return res

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero35_0]
    genSingle1 = [mrgCzero5_1] ++ [mrgInf5_1]
    genSingle2 = [mrgCFalse5_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd5_0 e1_0 e1_1 e1_2])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd5_1 e1_0 e1_1] ++ [mrgCsub5_1 e1_2 e1_3] ++ [mrgCIte5_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_5_1 e0_0] ++ [mrgAccess1_0_5_1 e0_1] ++ [mrgAccess2_0_5_1 e0_2] ++ [mrgMax5_1 e1_6 e1_7])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq5_2 e1_0 e1_1] ++ [mrgCless5_2 e1_2 e1_3] ++ [mrgCleq5_2 e1_4 e1_5] ++ [mrgCand5_2 e2_0 e2_1] ++ [mrgCor5_2 e2_2 e2_3] ++ [mrgCnot5_2 e2_4])
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
eval0_1 env (Param10_1) = evalVar0 env "h"
eval0_1 env (Param30_1) = evalVar0 env "ms"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access1_3_0_1 p0) = snd (evalU0_3 env p0)
eval0_1 env (Access0_2_0_1 p0) = get1from3 (evalU0_2 env p0)
eval0_1 env (Access1_2_0_1 p0) = get2from3 (evalU0_2 env p0)
eval0_1 env (Access2_2_0_1 p0) = get3from3 (evalU0_2 env p0)
eval0_1 env (Max0_1 p0 p1) = max' (evalU0_1 env p0) (evalU0_1 env p1)
eval0_1 env (Inf0_1) = inf

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> (SymInteger, SymInteger, SymInteger)
eval0_2 env (Param20_2) = evalVar1 env "x"
eval0_2 env (Access0_3_0_2 p0) = fst (evalU0_3 env p0)
eval0_2 env (Prod0_2 p0 p1 p2) = ((evalU0_1 env p0), (evalU0_1 env p1), (evalU0_1 env p2))

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> (SymInteger, SymInteger, SymInteger)
evalU0_2 env = onUnion (eval0_2 env)

eval0_3 :: RefEnv -> Expr0_3 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
eval0_3 env (Param00_3) = evalVar2 env "res"
eval0_3 env (Prod0_3 p0 p1) = ((evalU0_2 env p0), (evalU0_1 env p1))

evalU0_3 :: RefEnv -> UnionM Expr0_3 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalU0_3 env = onUnion (eval0_3 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymInteger, SymInteger, SymInteger)
eval1_0 env (Param21_0) = evalVar1 env "x"
eval1_0 env (Access0_2_1_0 p0) = fst (evalU1_2 env p0)
eval1_0 env (Prod1_0 p0 p1 p2) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param11_1) = evalVar0 env "h"
eval1_1 env (Param31_1) = evalVar0 env "ms"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access1_2_1_1 p0) = snd (evalU1_2 env p0)
eval1_1 env (Access0_0_1_1 p0) = get1from3 (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = get2from3 (evalU1_0 env p0)
eval1_1 env (Access2_0_1_1 p0) = get3from3 (evalU1_0 env p0)
eval1_1 env (Max1_1 p0 p1) = max' (evalU1_1 env p0) (evalU1_1 env p1)
eval1_1 env (Inf1_1) = inf

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
eval1_2 env (Param01_2) = evalVar2 env "res"
eval1_2 env (Prod1_2 p0 p1) = ((evalU1_0 env p0), (evalU1_1 env p1))

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalU1_2 env = onUnion (eval1_2 env)

eval1_3 :: RefEnv -> Expr1_3 -> SymBool
eval1_3 env (Ceq1_3 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_3 env (Cless1_3 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_3 env (Cleq1_3 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_3 env (Cand1_3 p0 p1) =  (evalU1_3 env p0) &&~ (evalU1_3 env p1) 
eval1_3 env (Cor1_3 p0 p1) =  (evalU1_3 env p0) ||~ (evalU1_3 env p1) 
eval1_3 env (Cnot1_3 p0) =  mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_3 env (CFalse1_3) = (toSym False)

evalU1_3 :: RefEnv -> UnionM Expr1_3 -> SymBool
evalU1_3 env = onUnion (eval1_3 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Param12_0) = evalVar0 env "h"
eval2_0 env (Param32_0) = evalVar0 env "ms"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_3 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Access1_1_2_0 p0) = snd (evalU2_1 env p0)
eval2_0 env (Access0_2_2_0 p0) = get1from3 (evalU2_2 env p0)
eval2_0 env (Access1_2_2_0 p0) = get2from3 (evalU2_2 env p0)
eval2_0 env (Access2_2_2_0 p0) = get3from3 (evalU2_2 env p0)
eval2_0 env (Max2_0 p0 p1) = max' (evalU2_0 env p0) (evalU2_0 env p1)
eval2_0 env (Inf2_0) = inf

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
eval2_1 env (Param02_1) = evalVar2 env "res"
eval2_1 env (Prod2_1 p0 p1) = ((evalU2_2 env p0), (evalU2_0 env p1))

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> (SymInteger, SymInteger, SymInteger)
eval2_2 env (Param22_2) = evalVar1 env "x"
eval2_2 env (Access0_1_2_2 p0) = fst (evalU2_1 env p0)
eval2_2 env (Prod2_2 p0 p1 p2) = ((evalU2_0 env p0), (evalU2_0 env p1), (evalU2_0 env p2))

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> (SymInteger, SymInteger, SymInteger)
evalU2_2 env = onUnion (eval2_2 env)

eval2_3 :: RefEnv -> Expr2_3 -> SymBool
eval2_3 env (Ceq2_3 p0 p1) =  (evalU2_0 env p0) ==~ (evalU2_0 env p1) 
eval2_3 env (Cless2_3 p0 p1) =  (evalU2_0 env p0) <~ (evalU2_0 env p1) 
eval2_3 env (Cleq2_3 p0 p1) =  (evalU2_0 env p0) <=~ (evalU2_0 env p1) 
eval2_3 env (Cand2_3 p0 p1) =  (evalU2_3 env p0) &&~ (evalU2_3 env p1) 
eval2_3 env (Cor2_3 p0 p1) =  (evalU2_3 env p0) ||~ (evalU2_3 env p1) 
eval2_3 env (Cnot2_3 p0) =  mrgIte ((evalU2_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_3 env (CFalse2_3) = (toSym False)

evalU2_3 :: RefEnv -> UnionM Expr2_3 -> SymBool
evalU2_3 env = onUnion (eval2_3 env)

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger, SymInteger)
eval3_0 env (Param03_0) = evalVar1 env "x"
eval3_0 env (Access0_2_3_0 p0) = fst (evalU3_2 env p0)
eval3_0 env (Prod3_0 p0 p1 p2) = ((evalU3_1 env p0), (evalU3_1 env p1), (evalU3_1 env p2))

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param13_1) = evalVar0 env "ms"
eval3_1 env (Param43_1) = evalVar0 env "h"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_3 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Access0_0_3_1 p0) = get1from3 (evalU3_0 env p0)
eval3_1 env (Access1_0_3_1 p0) = get2from3 (evalU3_0 env p0)
eval3_1 env (Access2_0_3_1 p0) = get3from3 (evalU3_0 env p0)
eval3_1 env (Access1_2_3_1 p0) = snd (evalU3_2 env p0)
eval3_1 env (Max3_1 p0 p1) = max' (evalU3_1 env p0) (evalU3_1 env p1)
eval3_1 env (Inf3_1) = inf

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
eval3_2 env (Param53_2) = evalVar2 env "res"
eval3_2 env (Prod3_2 p0 p1) = ((evalU3_0 env p0), (evalU3_1 env p1))

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalU3_2 env = onUnion (eval3_2 env)

eval3_3 :: RefEnv -> Expr3_3 -> SymBool
eval3_3 env (Ceq3_3 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_3 env (Cless3_3 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_3 env (Cleq3_3 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_3 env (Cand3_3 p0 p1) =  (evalU3_3 env p0) &&~ (evalU3_3 env p1) 
eval3_3 env (Cor3_3 p0 p1) =  (evalU3_3 env p0) ||~ (evalU3_3 env p1) 
eval3_3 env (Cnot3_3 p0) =  mrgIte ((evalU3_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_3 env (CFalse3_3) = (toSym False)

evalU3_3 :: RefEnv -> UnionM Expr3_3 -> SymBool
evalU3_3 env = onUnion (eval3_3 env)

eval4_0 :: RefEnv -> Expr4_0 -> (SymInteger, SymInteger, SymInteger)
eval4_0 env (Param04_0) = evalVar1 env "x"
eval4_0 env (Access0_2_4_0 p0) = fst (evalU4_2 env p0)
eval4_0 env (Prod4_0 p0 p1 p2) = ((evalU4_1 env p0), (evalU4_1 env p1), (evalU4_1 env p2))

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> (SymInteger, SymInteger, SymInteger)
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymInteger
eval4_1 env (Param14_1) = evalVar0 env "ms"
eval4_1 env (Param44_1) = evalVar0 env "h"
eval4_1 env (Cadd4_1 p0 p1) =  (evalU4_1 env p0) + (evalU4_1 env p1) 
eval4_1 env (Csub4_1 p0 p1) =  (evalU4_1 env p0) - (evalU4_1 env p1) 
eval4_1 env (Czero4_1) = 0
eval4_1 env (CIte4_1 p0 p1 p2) = mrgIte ((evalU4_3 env p0) ==~ (toSym True)) (evalU4_1 env p1) (evalU4_1 env p2)
eval4_1 env (Access0_0_4_1 p0) = get1from3 (evalU4_0 env p0)
eval4_1 env (Access1_0_4_1 p0) = get2from3 (evalU4_0 env p0)
eval4_1 env (Access2_0_4_1 p0) = get3from3 (evalU4_0 env p0)
eval4_1 env (Access1_2_4_1 p0) = snd (evalU4_2 env p0)
eval4_1 env (Max4_1 p0 p1) = max' (evalU4_1 env p0) (evalU4_1 env p1)
eval4_1 env (Inf4_1) = inf

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymInteger
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
eval4_2 env (Param54_2) = evalVar2 env "res"
eval4_2 env (Prod4_2 p0 p1) = ((evalU4_0 env p0), (evalU4_1 env p1))

evalU4_2 :: RefEnv -> UnionM Expr4_2 -> ((SymInteger, SymInteger, SymInteger), SymInteger)
evalU4_2 env = onUnion (eval4_2 env)

eval4_3 :: RefEnv -> Expr4_3 -> SymBool
eval4_3 env (Ceq4_3 p0 p1) =  (evalU4_1 env p0) ==~ (evalU4_1 env p1) 
eval4_3 env (Cless4_3 p0 p1) =  (evalU4_1 env p0) <~ (evalU4_1 env p1) 
eval4_3 env (Cleq4_3 p0 p1) =  (evalU4_1 env p0) <=~ (evalU4_1 env p1) 
eval4_3 env (Cand4_3 p0 p1) =  (evalU4_3 env p0) &&~ (evalU4_3 env p1) 
eval4_3 env (Cor4_3 p0 p1) =  (evalU4_3 env p0) ||~ (evalU4_3 env p1) 
eval4_3 env (Cnot4_3 p0) =  mrgIte ((evalU4_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval4_3 env (CFalse4_3) = (toSym False)

evalU4_3 :: RefEnv -> UnionM Expr4_3 -> SymBool
evalU4_3 env = onUnion (eval4_3 env)

eval5_0 :: RefEnv -> Expr5_0 -> (SymInteger, SymInteger, SymInteger)
eval5_0 env (Prod5_0 p0 p1 p2) = ((evalU5_1 env p0), (evalU5_1 env p1), (evalU5_1 env p2))
eval5_0 env (CZero35_0) = (0,0,0)

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> (SymInteger, SymInteger, SymInteger)
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> SymInteger
eval5_1 env (Cadd5_1 p0 p1) =  (evalU5_1 env p0) + (evalU5_1 env p1) 
eval5_1 env (Csub5_1 p0 p1) =  (evalU5_1 env p0) - (evalU5_1 env p1) 
eval5_1 env (Czero5_1) = 0
eval5_1 env (CIte5_1 p0 p1 p2) = mrgIte ((evalU5_2 env p0) ==~ (toSym True)) (evalU5_1 env p1) (evalU5_1 env p2)
eval5_1 env (Access0_0_5_1 p0) = get1from3 (evalU5_0 env p0)
eval5_1 env (Access1_0_5_1 p0) = get2from3 (evalU5_0 env p0)
eval5_1 env (Access2_0_5_1 p0) = get3from3 (evalU5_0 env p0)
eval5_1 env (Max5_1 p0 p1) = max' (evalU5_1 env p0) (evalU5_1 env p1)
eval5_1 env (Inf5_1) = inf

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> SymInteger
evalU5_1 env = onUnion (eval5_1 env)

eval5_2 :: RefEnv -> Expr5_2 -> SymBool
eval5_2 env (Ceq5_2 p0 p1) =  (evalU5_1 env p0) ==~ (evalU5_1 env p1) 
eval5_2 env (Cless5_2 p0 p1) =  (evalU5_1 env p0) <~ (evalU5_1 env p1) 
eval5_2 env (Cleq5_2 p0 p1) =  (evalU5_1 env p0) <=~ (evalU5_1 env p1) 
eval5_2 env (Cand5_2 p0 p1) =  (evalU5_2 env p0) &&~ (evalU5_2 env p1) 
eval5_2 env (Cor5_2 p0 p1) =  (evalU5_2 env p0) ||~ (evalU5_2 env p1) 
eval5_2 env (Cnot5_2 p0) =  mrgIte ((evalU5_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval5_2 env (CFalse5_2) = (toSym False)

evalU5_2 :: RefEnv -> UnionM Expr5_2 -> SymBool
evalU5_2 env = onUnion (eval5_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
res h x ms t 

Hole grammar for #1
res h x ms t 

Hole grammar for #2
res h x ms t 

Hole grammar for #3
x ms tmp1 t h res 

Hole grammar for #4
x ms tmp2 t h res 

Hole grammar for #5
x tmp3 
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

fold_list f x w0 = 
  let
    g x = 
      case x of
        Cons h t -> f h (g t)
        _ -> w0
      
  in
  g  x

rev x = 
  let
    f x y = 
      case x of
        Cons h t -> f t (Cons h y)
        _ -> y
      
  in
  f  x (Nil Unit)

max' x y = 
  mrgIte (x <~  y)
    (y)
    (x)

inf = 
  100

lsp b x = 
  (snd (let
    f x = 
      case x of
        Cons h t -> 
          let
            res =
              (f t)
          in
          let
            ms =
              (snd res)
          in
          let
            x =
              (fst res)
          in
          mrgIte (evalU0_0 (RefEnv [("h", (Env0 h)), ("ms", (Env0 ms)), ("x", (Env1 x)), ("res", (Env2 res))]) ((genSym (5::Int) "hole0") :: (UnionM Expr0_0)))
            (((evalU1_0 (RefEnv [("x", (Env1 x)), ("h", (Env0 h)), ("ms", (Env0 ms)), ("res", (Env2 res))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0))), (max' ms (1 +  (evalU2_0 (RefEnv [("h", (Env0 h)), ("ms", (Env0 ms)), ("res", (Env2 res)), ("x", (Env1 x))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0)))))))
            (mrgIte (b (Cons h (Nil Unit)))
              (((let
                tmp1 =
                  (Cons h (Nil Unit))
              in
              evalU3_0 (RefEnv [("x", (Env1 x)), ("ms", (Env0 ms)), ("h", (Env0 h)), ("res", (Env2 res))]) ((genSym (5::Int) "hole3") :: (UnionM Expr3_0))), (max' ms 1)))
              (((let
                tmp2 =
                  (Nil Unit)
              in
              evalU4_0 (RefEnv [("x", (Env1 x)), ("ms", (Env0 ms)), ("h", (Env0 h)), ("res", (Env2 res))]) ((genSym (5::Int) "hole4") :: (UnionM Expr4_0))), ms)))
        _ -> ((let
            tmp3 =
              (Nil Unit)
          in
          evalU5_0 (RefEnv []) ((genSym (5::Int) "hole5") :: (UnionM Expr5_0))), 0)
      
  in
  f  x))

min' x y = 
  mrgIte (x <~  y)
    (x)
    (y)

minimum' x = 
  fold (\h t -> min' h t) x inf

maximum' x = 
  fold (\h t -> max' h t) x (0 -  inf)

cond1 x = 
  case x of
    Cons h t -> mrgIte ((h >~  (minimum' t)) ==~ (toSym True)) (toSym False) (toSym True)
    _ -> (toSym True)
  

cond2 global0 x = 
  let
    ma =
      (maximum' x)
  in
  let
    mi =
      (minimum' x)
  in
  mrgIte (((mi +  global0) <~  ma) ==~ (toSym True)) (toSym False) (toSym True)

isval global0 x = 
  (cond1 x) &&~  (cond2 global0 x)

main' global0 = 
  lsp (isval global0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [((SymInteger, List), Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [((SymInteger, List), Integer)] -> SymBool
        constraint [] = con True
        constraint (((x1,x2), y) : xs) = main' x1 x2 ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((-5)), ((Nil Unit))), (0))
                , ((((4)), ((Cons ((-2)) ((Cons ((0)) ((Cons ((-4)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit))))))))))))), (2))
                , ((((1)), ((Nil Unit))), (0))
                , ((((1)), ((Cons ((5)) ((Cons ((3)) ((Cons ((-5)) ((Cons ((-3)) ((Cons ((1)) ((Nil Unit))))))))))))), (1))
                , ((((-5)), ((Cons ((5)) ((Nil Unit))))), (0))
                , ((((4)), ((Cons ((5)) ((Nil Unit))))), (1))
                , ((((0)), ((Cons ((-2)) ((Cons ((5)) ((Cons ((3)) ((Cons ((5)) ((Cons ((-5)) ((Cons ((1)) ((Cons ((-5)) ((Cons ((3)) ((Nil Unit))))))))))))))))))), (1))
                , ((((-2)), ((Cons ((4)) ((Nil Unit))))), (0))
                , ((((3)), ((Cons ((-5)) ((Nil Unit))))), (1))
                , ((((3)), ((Cons ((-2)) ((Cons ((-4)) ((Cons ((2)) ((Cons ((-3)) ((Cons ((-1)) ((Cons ((1)) ((Cons ((3)) ((Cons ((4)) ((Nil Unit))))))))))))))))))), (3))
                , ((((-4)), ((Cons ((3)) ((Cons ((-4)) ((Nil Unit))))))), (0))
                , ((((-2)), ((Cons ((-2)) ((Nil Unit))))), (0))
                , ((((4)), ((Cons ((-3)) ((Cons ((-5)) ((Cons ((-1)) ((Cons ((-1)) ((Nil Unit))))))))))), (3))
                , ((((-2)), ((Cons ((4)) ((Nil Unit))))), (0))
                , ((((-3)), ((Nil Unit))), (0))
                , ((((3)), ((Nil Unit))), (0))
                , ((((0)), ((Cons ((-5)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((2)) ((Cons ((5)) ((Cons ((1)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((-2)) ((Nil Unit))))))))))))))))))))), (2))
                , ((((-4)), ((Cons ((-1)) ((Cons ((-3)) ((Cons ((-5)) ((Nil Unit))))))))), (0))
                , ((((2)), ((Cons ((0)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((0)) ((Cons ((2)) ((Cons ((3)) ((Cons ((4)) ((Cons ((2)) ((Cons ((-2)) ((Nil Unit))))))))))))))))))))), (3))
                , ((((3)), ((Cons ((3)) ((Nil Unit))))), (1))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
