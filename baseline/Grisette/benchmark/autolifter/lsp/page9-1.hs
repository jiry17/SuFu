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
  | Param20_1
  | Param30_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access0_2_0_1 (UnionM Expr0_2)
  | Access1_2_0_1 (UnionM Expr0_2)
  | Max0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Param00_2
  | Prod0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: Int
-- param_list: res h x ms t
data Expr1_0
  = Param11_0
  | Param21_0
  | Param31_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_2) (UnionM Expr1_0) (UnionM Expr1_0)
  | Access0_1_1_0 (UnionM Expr1_1)
  | Access1_1_1_0 (UnionM Expr1_1)
  | Max1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Inf1_0
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
-- param_list: res h x ms t
data Expr2_0
  = Param12_0
  | Param22_0
  | Param32_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_2) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access0_1_2_0 (UnionM Expr2_1)
  | Access1_1_2_0 (UnionM Expr2_1)
  | Max2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Inf2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param02_1
  | Prod2_1 (UnionM Expr2_0) (UnionM Expr2_0)
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

-- output_type: Int
-- param_list: x ms tmp1 t h res
data Expr3_0
  = Param03_0
  | Param13_0
  | Param43_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_2) (UnionM Expr3_0) (UnionM Expr3_0)
  | Access0_1_3_0 (UnionM Expr3_1)
  | Access1_1_3_0 (UnionM Expr3_1)
  | Max3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Inf3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param53_1
  | Prod3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Ceq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cless3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cleq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: Int
-- param_list: x ms tmp2 t h res
data Expr4_0
  = Param04_0
  | Param14_0
  | Param44_0
  | Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_2) (UnionM Expr4_0) (UnionM Expr4_0)
  | Access0_1_4_0 (UnionM Expr4_1)
  | Access1_1_4_0 (UnionM Expr4_1)
  | Max4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Inf4_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param54_1
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

-- output_type: Int
-- param_list: x tmp3
data Expr5_0
  = Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_1) (UnionM Expr5_0) (UnionM Expr5_0)
  | Max5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Inf5_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Ceq5_1 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cless5_1 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cleq5_1 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cand5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cor5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cnot5_1 (UnionM Expr5_1)
  | CFalse5_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

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
$(makeUnionWrapper "mrg" ''Expr5_0)
$(makeUnionWrapper "mrg" ''Expr5_1)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgParam10_1] ++ [mrgParam20_1] ++ [mrgParam30_1] ++ [mrgCzero0_1] ++ [mrgInf0_1]
    genSingle2 = [mrgParam00_2]
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5] ++ [mrgAccess0_2_0_1 e2_0] ++ [mrgAccess1_2_0_1 e2_1] ++ [mrgMax0_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd0_2 e1_0 e1_1])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam11_0] ++ [mrgParam21_0] ++ [mrgParam31_0] ++ [mrgCzero1_0] ++ [mrgInf1_0]
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
        e0_7 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_1_0 e1_0] ++ [mrgAccess1_1_1_0 e1_1] ++ [mrgMax1_0 e0_6 e0_7])
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
    genSingle0 = [mrgParam12_0] ++ [mrgParam22_0] ++ [mrgParam32_0] ++ [mrgCzero2_0] ++ [mrgInf2_0]
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
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_2_0 e1_0] ++ [mrgAccess1_1_2_0 e1_1] ++ [mrgMax2_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd2_1 e0_0 e0_1])
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

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam03_0] ++ [mrgParam13_0] ++ [mrgParam43_0] ++ [mrgCzero3_0] ++ [mrgInf3_0]
    genSingle1 = [mrgParam53_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_3_0 e1_0] ++ [mrgAccess1_1_3_0 e1_1] ++ [mrgMax3_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd3_1 e0_0 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq3_2 e0_0 e0_1] ++ [mrgCless3_2 e0_2 e0_3] ++ [mrgCleq3_2 e0_4 e0_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam04_0] ++ [mrgParam14_0] ++ [mrgParam44_0] ++ [mrgCzero4_0] ++ [mrgInf4_0]
    genSingle1 = [mrgParam54_1]
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

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero5_0] ++ [mrgInf5_0]
    genSingle1 = [mrgCFalse5_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd5_0 e0_0 e0_1] ++ [mrgCsub5_0 e0_2 e0_3] ++ [mrgCIte5_0 e1_0 e0_4 e0_5] ++ [mrgMax5_0 e0_6 e0_7])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq5_1 e0_0 e0_1] ++ [mrgCless5_1 e0_2 e0_3] ++ [mrgCleq5_1 e0_4 e0_5] ++ [mrgCand5_1 e1_0 e1_1] ++ [mrgCor5_1 e1_2 e1_3] ++ [mrgCnot5_1 e1_4])
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
eval0_1 env (Param20_1) = evalVar0 env "x"
eval0_1 env (Param30_1) = evalVar0 env "ms"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_2_0_1 p0) = fst (evalU0_2 env p0)
eval0_1 env (Access1_2_0_1 p0) = snd (evalU0_2 env p0)
eval0_1 env (Max0_1 p0 p1) = max' (evalU0_1 env p0) (evalU0_1 env p1)
eval0_1 env (Inf0_1) = inf

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> (SymInteger, SymInteger)
eval0_2 env (Param00_2) = evalVar1 env "res"
eval0_2 env (Prod0_2 p0 p1) = ((evalU0_1 env p0), (evalU0_1 env p1))

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> (SymInteger, SymInteger)
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> SymInteger
eval1_0 env (Param11_0) = evalVar0 env "h"
eval1_0 env (Param21_0) = evalVar0 env "x"
eval1_0 env (Param31_0) = evalVar0 env "ms"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)
eval1_0 env (Access0_1_1_0 p0) = fst (evalU1_1 env p0)
eval1_0 env (Access1_1_1_0 p0) = snd (evalU1_1 env p0)
eval1_0 env (Max1_0 p0 p1) = max' (evalU1_0 env p0) (evalU1_0 env p1)
eval1_0 env (Inf1_0) = inf

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymInteger
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> (SymInteger, SymInteger)
eval1_1 env (Param01_1) = evalVar1 env "res"
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
eval2_0 env (Param12_0) = evalVar0 env "h"
eval2_0 env (Param22_0) = evalVar0 env "x"
eval2_0 env (Param32_0) = evalVar0 env "ms"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Access0_1_2_0 p0) = fst (evalU2_1 env p0)
eval2_0 env (Access1_1_2_0 p0) = snd (evalU2_1 env p0)
eval2_0 env (Max2_0 p0 p1) = max' (evalU2_0 env p0) (evalU2_0 env p1)
eval2_0 env (Inf2_0) = inf

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (SymInteger, SymInteger)
eval2_1 env (Param02_1) = evalVar1 env "res"
eval2_1 env (Prod2_1 p0 p1) = ((evalU2_0 env p0), (evalU2_0 env p1))

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> (SymInteger, SymInteger)
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

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Param03_0) = evalVar0 env "x"
eval3_0 env (Param13_0) = evalVar0 env "ms"
eval3_0 env (Param43_0) = evalVar0 env "h"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Access0_1_3_0 p0) = fst (evalU3_1 env p0)
eval3_0 env (Access1_1_3_0 p0) = snd (evalU3_1 env p0)
eval3_0 env (Max3_0 p0 p1) = max' (evalU3_0 env p0) (evalU3_0 env p1)
eval3_0 env (Inf3_0) = inf

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymInteger
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (SymInteger, SymInteger)
eval3_1 env (Param53_1) = evalVar1 env "res"
eval3_1 env (Prod3_1 p0 p1) = ((evalU3_0 env p0), (evalU3_0 env p1))

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (SymInteger, SymInteger)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_0 env p0) ==~ (evalU3_0 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_0 env p0) <~ (evalU3_0 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_0 env p0) <=~ (evalU3_0 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> SymInteger
eval4_0 env (Param04_0) = evalVar0 env "x"
eval4_0 env (Param14_0) = evalVar0 env "ms"
eval4_0 env (Param44_0) = evalVar0 env "h"
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Access0_1_4_0 p0) = fst (evalU4_1 env p0)
eval4_0 env (Access1_1_4_0 p0) = snd (evalU4_1 env p0)
eval4_0 env (Max4_0 p0 p1) = max' (evalU4_0 env p0) (evalU4_0 env p1)
eval4_0 env (Inf4_0) = inf

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> (SymInteger, SymInteger)
eval4_1 env (Param54_1) = evalVar1 env "res"
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

eval5_0 :: RefEnv -> Expr5_0 -> SymInteger
eval5_0 env (Cadd5_0 p0 p1) =  (evalU5_0 env p0) + (evalU5_0 env p1) 
eval5_0 env (Csub5_0 p0 p1) =  (evalU5_0 env p0) - (evalU5_0 env p1) 
eval5_0 env (Czero5_0) = 0
eval5_0 env (CIte5_0 p0 p1 p2) = mrgIte ((evalU5_1 env p0) ==~ (toSym True)) (evalU5_0 env p1) (evalU5_0 env p2)
eval5_0 env (Max5_0 p0 p1) = max' (evalU5_0 env p0) (evalU5_0 env p1)
eval5_0 env (Inf5_0) = inf

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> SymInteger
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> SymBool
eval5_1 env (Ceq5_1 p0 p1) =  (evalU5_0 env p0) ==~ (evalU5_0 env p1) 
eval5_1 env (Cless5_1 p0 p1) =  (evalU5_0 env p0) <~ (evalU5_0 env p1) 
eval5_1 env (Cleq5_1 p0 p1) =  (evalU5_0 env p0) <=~ (evalU5_0 env p1) 
eval5_1 env (Cand5_1 p0 p1) =  (evalU5_1 env p0) &&~ (evalU5_1 env p1) 
eval5_1 env (Cor5_1 p0 p1) =  (evalU5_1 env p0) ||~ (evalU5_1 env p1) 
eval5_1 env (Cnot5_1 p0) =  mrgIte ((evalU5_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval5_1 env (CFalse5_1) = (toSym False)

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> SymBool
evalU5_1 env = onUnion (eval5_1 env)


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

head x default' = 
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

lsp :: (List -> SymBool) -> List -> SymInteger
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
          mrgIte (evalU0_0 (RefEnv [("h", (Env0 h)), ("x", (Env0 x)), ("ms", (Env0 ms)), ("res", (Env1 res))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0)))
            (((evalU1_0 (RefEnv [("h", (Env0 h)), ("x", (Env0 x)), ("ms", (Env0 ms)), ("res", (Env1 res))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))), (max' ms (1 +  (evalU2_0 (RefEnv [("h", (Env0 h)), ("x", (Env0 x)), ("ms", (Env0 ms)), ("res", (Env1 res))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0)))))))
            (mrgIte (b (Cons h (Nil Unit)))
              (((let
                tmp1 =
                  (Cons h (Nil Unit))
              in
              evalU3_0 (RefEnv [("x", (Env0 x)), ("ms", (Env0 ms)), ("h", (Env0 h)), ("res", (Env1 res))]) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))), (max' ms 1)))
              (((let
                tmp2 =
                  (Nil Unit)
              in
              evalU4_0 (RefEnv [("x", (Env0 x)), ("ms", (Env0 ms)), ("h", (Env0 h)), ("res", (Env1 res))]) ((genSym (4::Int) "hole4") :: (UnionM Expr4_0))), ms)))
        _ -> ((let
            tmp3 =
              (Nil Unit)
          in
          evalU5_0 (RefEnv []) ((genSym (1::Int) "hole5") :: (UnionM Expr5_0))), 0)
      
  in
  f  x))

f :: List -> (SymBool, SymInteger)
f l = 
      case l of
        Cons h t -> 
          let
            res =
              (f t)
          in
          mrgIte (h >~  (snd res))
            (((toSym False), h))
            (((fst res), h))
        Nil _ -> ((toSym True), inf)

issorted :: List -> SymBool
issorted x = 
  (fst (f x))

main' = 
  lsp issorted

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
                ((Nil Unit), 0)
                , ((Nil Unit), 0)
                , ((Cons (2) (Cons (4) (Cons (3) (Cons (1) (Cons (5) (Cons (3) (Cons (1) (Cons (-4) (Nil Unit))))))))), 2)
                , ((Cons (4) (Cons (5) (Nil Unit))), 2)
                , ((Cons (-3) (Cons (1) (Cons (5) (Cons (-3) (Cons (-1) (Cons (1) (Cons (4) (Cons (1) (Cons (5) (Nil Unit)))))))))), 4)
                , ((Cons (-1) (Cons (2) (Cons (-3) (Cons (-2) (Nil Unit))))), 2)
                , ((Cons (-5) (Cons (3) (Cons (5) (Cons (-3) (Cons (-1) (Cons (1) (Cons (3) (Cons (5) (Nil Unit))))))))), 5)
                , ((Cons (4) (Cons (-4) (Cons (1) (Cons (-2) (Nil Unit))))), 2)
                , ((Cons (-3) (Nil Unit)), 1)
                , ((Cons (-3) (Cons (-4) (Cons (2) (Cons (3) (Cons (-4) (Cons (0) (Cons (-3) (Nil Unit)))))))), 3)
                , ((Cons (4) (Cons (-5) (Nil Unit))), 1)
                , ((Cons (-1) (Cons (-1) (Cons (-4) (Cons (-3) (Cons (0) (Cons (2) (Nil Unit))))))), 4)
                , ((Nil Unit), 0)
                , ((Cons (2) (Cons (0) (Cons (-2) (Cons (-5) (Cons (5) (Cons (-3) (Cons (2) (Nil Unit)))))))), 2)
                , ((Cons (4) (Cons (-2) (Cons (-2) (Nil Unit)))), 2)
                , ((Nil Unit), 0)
                , ((Cons (3) (Cons (-5) (Cons (2) (Nil Unit)))), 2)
                , ((Cons (5) (Cons (-1) (Cons (0) (Nil Unit)))), 2)
                , ((Nil Unit), 0)
                , ((Cons (-2) (Cons (-4) (Cons (-3) (Cons (-3) (Cons (-3) (Cons (1) (Cons (3) (Cons (3) (Nil Unit))))))))), 7)
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
