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
  | Env1 SymBool
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

evalVar1 :: RefEnv -> Ident -> SymBool
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymInteger
SymBool
-}

-- output_type: Bool
-- param_list: a b p
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
  = Param00_1
  | Param10_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

-- output_type: Bool
-- param_list: t a h p
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
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

-- output_type: Bool
-- param_list: t a h res p
data Expr2_0
  = Param32_0
  | Ceq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_0 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cor2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cnot2_0 (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param12_1
  | Param22_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_0) (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

-- output_type: Bool
-- param_list: t p b h
data Expr3_0
  = Ceq3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cor3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cnot3_0 (UnionM Expr3_0)
  | CFalse3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param23_1
  | Param33_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_0) (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

-- output_type: Bool
-- param_list: t res p b h
data Expr4_0
  = Param14_0
  | Ceq4_0 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cless4_0 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cleq4_0 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cand4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cor4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cnot4_0 (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param34_1
  | Param44_1
  | Cadd4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Csub4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Czero4_1
  | CIte4_1 (UnionM Expr4_0) (UnionM Expr4_1) (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

-- output_type: Bool
-- param_list: p h2 h1 t2 t1
data Expr5_0
  = Ceq5_0 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cless5_0 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cleq5_0 (UnionM Expr5_1) (UnionM Expr5_1)
  | Cand5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cor5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cnot5_0 (UnionM Expr5_0)
  | CFalse5_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Param15_1
  | Param25_1
  | Cadd5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Csub5_1 (UnionM Expr5_1) (UnionM Expr5_1)
  | Czero5_1
  | CIte5_1 (UnionM Expr5_0) (UnionM Expr5_1) (UnionM Expr5_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

-- output_type: Bool
-- param_list: h1 t1 t2 h2 p res
data Expr6_0
  = Param56_0
  | Ceq6_0 (UnionM Expr6_1) (UnionM Expr6_1)
  | Cless6_0 (UnionM Expr6_1) (UnionM Expr6_1)
  | Cleq6_0 (UnionM Expr6_1) (UnionM Expr6_1)
  | Cand6_0 (UnionM Expr6_0) (UnionM Expr6_0)
  | Cor6_0 (UnionM Expr6_0) (UnionM Expr6_0)
  | Cnot6_0 (UnionM Expr6_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr6_0)
    via (Default Expr6_0)

data Expr6_1
  = Param06_1
  | Param36_1
  | Cadd6_1 (UnionM Expr6_1) (UnionM Expr6_1)
  | Csub6_1 (UnionM Expr6_1) (UnionM Expr6_1)
  | Czero6_1
  | CIte6_1 (UnionM Expr6_0) (UnionM Expr6_1) (UnionM Expr6_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr6_1)
    via (Default Expr6_1)

-- output_type: Bool
-- param_list: h1 t1 t2 h2 p res
data Expr7_0
  = Param57_0
  | Ceq7_0 (UnionM Expr7_1) (UnionM Expr7_1)
  | Cless7_0 (UnionM Expr7_1) (UnionM Expr7_1)
  | Cleq7_0 (UnionM Expr7_1) (UnionM Expr7_1)
  | Cand7_0 (UnionM Expr7_0) (UnionM Expr7_0)
  | Cor7_0 (UnionM Expr7_0) (UnionM Expr7_0)
  | Cnot7_0 (UnionM Expr7_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr7_0)
    via (Default Expr7_0)

data Expr7_1
  = Param07_1
  | Param37_1
  | Cadd7_1 (UnionM Expr7_1) (UnionM Expr7_1)
  | Csub7_1 (UnionM Expr7_1) (UnionM Expr7_1)
  | Czero7_1
  | CIte7_1 (UnionM Expr7_0) (UnionM Expr7_1) (UnionM Expr7_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr7_1)
    via (Default Expr7_1)

-- output_type: Bool
-- param_list: p tmp1
data Expr8_0
  = Param18_0
  | Ceq8_0 (UnionM Expr8_1) (UnionM Expr8_1)
  | Cless8_0 (UnionM Expr8_1) (UnionM Expr8_1)
  | Cleq8_0 (UnionM Expr8_1) (UnionM Expr8_1)
  | Cand8_0 (UnionM Expr8_0) (UnionM Expr8_0)
  | Cor8_0 (UnionM Expr8_0) (UnionM Expr8_0)
  | Cnot8_0 (UnionM Expr8_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr8_0)
    via (Default Expr8_0)

data Expr8_1
  = Cadd8_1 (UnionM Expr8_1) (UnionM Expr8_1)
  | Csub8_1 (UnionM Expr8_1) (UnionM Expr8_1)
  | Czero8_1
  | CIte8_1 (UnionM Expr8_0) (UnionM Expr8_1) (UnionM Expr8_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr8_1)
    via (Default Expr8_1)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr4_0)
$(makeUnionWrapper "mrg" ''Expr4_1)
$(makeUnionWrapper "mrg" ''Expr5_0)
$(makeUnionWrapper "mrg" ''Expr5_1)
$(makeUnionWrapper "mrg" ''Expr6_0)
$(makeUnionWrapper "mrg" ''Expr6_1)
$(makeUnionWrapper "mrg" ''Expr7_0)
$(makeUnionWrapper "mrg" ''Expr7_1)
$(makeUnionWrapper "mrg" ''Expr8_0)
$(makeUnionWrapper "mrg" ''Expr8_1)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgParam00_1] ++ [mrgParam10_1] ++ [mrgCzero0_1]
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse1_0]
    genSingle1 = [mrgParam11_1] ++ [mrgParam21_1] ++ [mrgCzero1_1]
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam32_0]
    genSingle1 = [mrgParam12_1] ++ [mrgParam22_1] ++ [mrgCzero2_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq2_0 e1_0 e1_1] ++ [mrgCless2_0 e1_2 e1_3] ++ [mrgCleq2_0 e1_4 e1_5] ++ [mrgCand2_0 e0_0 e0_1] ++ [mrgCor2_0 e0_2 e0_3] ++ [mrgCnot2_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse3_0]
    genSingle1 = [mrgParam23_1] ++ [mrgParam33_1] ++ [mrgCzero3_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq3_0 e1_0 e1_1] ++ [mrgCless3_0 e1_2 e1_3] ++ [mrgCleq3_0 e1_4 e1_5] ++ [mrgCand3_0 e0_0 e0_1] ++ [mrgCor3_0 e0_2 e0_3] ++ [mrgCnot3_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam14_0]
    genSingle1 = [mrgParam34_1] ++ [mrgParam44_1] ++ [mrgCzero4_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq4_0 e1_0 e1_1] ++ [mrgCless4_0 e1_2 e1_3] ++ [mrgCleq4_0 e1_4 e1_5] ++ [mrgCand4_0 e0_0 e0_1] ++ [mrgCor4_0 e0_2 e0_3] ++ [mrgCnot4_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd4_1 e1_0 e1_1] ++ [mrgCsub4_1 e1_2 e1_3] ++ [mrgCIte4_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse5_0]
    genSingle1 = [mrgParam15_1] ++ [mrgParam25_1] ++ [mrgCzero5_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq5_0 e1_0 e1_1] ++ [mrgCless5_0 e1_2 e1_3] ++ [mrgCleq5_0 e1_4 e1_5] ++ [mrgCand5_0 e0_0 e0_1] ++ [mrgCor5_0 e0_2 e0_3] ++ [mrgCnot5_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd5_1 e1_0 e1_1] ++ [mrgCsub5_1 e1_2 e1_3] ++ [mrgCIte5_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr6_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr6_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam56_0]
    genSingle1 = [mrgParam06_1] ++ [mrgParam36_1] ++ [mrgCzero6_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq6_0 e1_0 e1_1] ++ [mrgCless6_0 e1_2 e1_3] ++ [mrgCleq6_0 e1_4 e1_5] ++ [mrgCand6_0 e0_0 e0_1] ++ [mrgCor6_0 e0_2 e0_3] ++ [mrgCnot6_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd6_1 e1_0 e1_1] ++ [mrgCsub6_1 e1_2 e1_3] ++ [mrgCIte6_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr7_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr7_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam57_0]
    genSingle1 = [mrgParam07_1] ++ [mrgParam37_1] ++ [mrgCzero7_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq7_0 e1_0 e1_1] ++ [mrgCless7_0 e1_2 e1_3] ++ [mrgCleq7_0 e1_4 e1_5] ++ [mrgCand7_0 e0_0 e0_1] ++ [mrgCor7_0 e0_2 e0_3] ++ [mrgCnot7_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd7_1 e1_0 e1_1] ++ [mrgCsub7_1 e1_2 e1_3] ++ [mrgCIte7_1 e0_0 e1_4 e1_5])
        return res

instance GenSym (Int) Expr8_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr8_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam18_0]
    genSingle1 = [mrgCzero8_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCeq8_0 e1_0 e1_1] ++ [mrgCless8_0 e1_2 e1_3] ++ [mrgCleq8_0 e1_4 e1_5] ++ [mrgCand8_0 e0_0 e0_1] ++ [mrgCor8_0 e0_2 e0_3] ++ [mrgCnot8_0 e0_4])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd8_1 e1_0 e1_1] ++ [mrgCsub8_1 e1_2 e1_3] ++ [mrgCIte8_1 e0_0 e1_4 e1_5])
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
eval0_1 env (Param00_1) = evalVar0 env "a"
eval0_1 env (Param10_1) = evalVar0 env "b"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

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
eval1_1 env (Param11_1) = evalVar0 env "a"
eval1_1 env (Param21_1) = evalVar0 env "h"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_0 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Param32_0) = evalVar1 env "res"
eval2_0 env (Ceq2_0 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_0 env (Cless2_0 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_0 env (Cleq2_0 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_0 env (Cand2_0 p0 p1) =  (evalU2_0 env p0) &&~ (evalU2_0 env p1) 
eval2_0 env (Cor2_0 p0 p1) =  (evalU2_0 env p0) ||~ (evalU2_0 env p1) 
eval2_0 env (Cnot2_0 p0) =  mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymBool
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param12_1) = evalVar0 env "a"
eval2_1 env (Param22_1) = evalVar0 env "h"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymBool
eval3_0 env (Ceq3_0 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_0 env (Cless3_0 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_0 env (Cleq3_0 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_0 env (Cand3_0 p0 p1) =  (evalU3_0 env p0) &&~ (evalU3_0 env p1) 
eval3_0 env (Cor3_0 p0 p1) =  (evalU3_0 env p0) ||~ (evalU3_0 env p1) 
eval3_0 env (Cnot3_0 p0) =  mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_0 env (CFalse3_0) = (toSym False)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymBool
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param23_1) = evalVar0 env "b"
eval3_1 env (Param33_1) = evalVar0 env "h"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)

eval4_0 :: RefEnv -> Expr4_0 -> SymBool
eval4_0 env (Param14_0) = evalVar1 env "res"
eval4_0 env (Ceq4_0 p0 p1) =  (evalU4_1 env p0) ==~ (evalU4_1 env p1) 
eval4_0 env (Cless4_0 p0 p1) =  (evalU4_1 env p0) <~ (evalU4_1 env p1) 
eval4_0 env (Cleq4_0 p0 p1) =  (evalU4_1 env p0) <=~ (evalU4_1 env p1) 
eval4_0 env (Cand4_0 p0 p1) =  (evalU4_0 env p0) &&~ (evalU4_0 env p1) 
eval4_0 env (Cor4_0 p0 p1) =  (evalU4_0 env p0) ||~ (evalU4_0 env p1) 
eval4_0 env (Cnot4_0 p0) =  mrgIte ((evalU4_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymBool
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymInteger
eval4_1 env (Param34_1) = evalVar0 env "b"
eval4_1 env (Param44_1) = evalVar0 env "h"
eval4_1 env (Cadd4_1 p0 p1) =  (evalU4_1 env p0) + (evalU4_1 env p1) 
eval4_1 env (Csub4_1 p0 p1) =  (evalU4_1 env p0) - (evalU4_1 env p1) 
eval4_1 env (Czero4_1) = 0
eval4_1 env (CIte4_1 p0 p1 p2) = mrgIte ((evalU4_0 env p0) ==~ (toSym True)) (evalU4_1 env p1) (evalU4_1 env p2)

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymInteger
evalU4_1 env = onUnion (eval4_1 env)

eval5_0 :: RefEnv -> Expr5_0 -> SymBool
eval5_0 env (Ceq5_0 p0 p1) =  (evalU5_1 env p0) ==~ (evalU5_1 env p1) 
eval5_0 env (Cless5_0 p0 p1) =  (evalU5_1 env p0) <~ (evalU5_1 env p1) 
eval5_0 env (Cleq5_0 p0 p1) =  (evalU5_1 env p0) <=~ (evalU5_1 env p1) 
eval5_0 env (Cand5_0 p0 p1) =  (evalU5_0 env p0) &&~ (evalU5_0 env p1) 
eval5_0 env (Cor5_0 p0 p1) =  (evalU5_0 env p0) ||~ (evalU5_0 env p1) 
eval5_0 env (Cnot5_0 p0) =  mrgIte ((evalU5_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval5_0 env (CFalse5_0) = (toSym False)

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> SymBool
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> SymInteger
eval5_1 env (Param15_1) = evalVar0 env "h2"
eval5_1 env (Param25_1) = evalVar0 env "h1"
eval5_1 env (Cadd5_1 p0 p1) =  (evalU5_1 env p0) + (evalU5_1 env p1) 
eval5_1 env (Csub5_1 p0 p1) =  (evalU5_1 env p0) - (evalU5_1 env p1) 
eval5_1 env (Czero5_1) = 0
eval5_1 env (CIte5_1 p0 p1 p2) = mrgIte ((evalU5_0 env p0) ==~ (toSym True)) (evalU5_1 env p1) (evalU5_1 env p2)

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> SymInteger
evalU5_1 env = onUnion (eval5_1 env)

eval6_0 :: RefEnv -> Expr6_0 -> SymBool
eval6_0 env (Param56_0) = evalVar1 env "res"
eval6_0 env (Ceq6_0 p0 p1) =  (evalU6_1 env p0) ==~ (evalU6_1 env p1) 
eval6_0 env (Cless6_0 p0 p1) =  (evalU6_1 env p0) <~ (evalU6_1 env p1) 
eval6_0 env (Cleq6_0 p0 p1) =  (evalU6_1 env p0) <=~ (evalU6_1 env p1) 
eval6_0 env (Cand6_0 p0 p1) =  (evalU6_0 env p0) &&~ (evalU6_0 env p1) 
eval6_0 env (Cor6_0 p0 p1) =  (evalU6_0 env p0) ||~ (evalU6_0 env p1) 
eval6_0 env (Cnot6_0 p0) =  mrgIte ((evalU6_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU6_0 :: RefEnv -> UnionM Expr6_0 -> SymBool
evalU6_0 env = onUnion (eval6_0 env)

eval6_1 :: RefEnv -> Expr6_1 -> SymInteger
eval6_1 env (Param06_1) = evalVar0 env "h1"
eval6_1 env (Param36_1) = evalVar0 env "h2"
eval6_1 env (Cadd6_1 p0 p1) =  (evalU6_1 env p0) + (evalU6_1 env p1) 
eval6_1 env (Csub6_1 p0 p1) =  (evalU6_1 env p0) - (evalU6_1 env p1) 
eval6_1 env (Czero6_1) = 0
eval6_1 env (CIte6_1 p0 p1 p2) = mrgIte ((evalU6_0 env p0) ==~ (toSym True)) (evalU6_1 env p1) (evalU6_1 env p2)

evalU6_1 :: RefEnv -> UnionM Expr6_1 -> SymInteger
evalU6_1 env = onUnion (eval6_1 env)

eval7_0 :: RefEnv -> Expr7_0 -> SymBool
eval7_0 env (Param57_0) = evalVar1 env "res"
eval7_0 env (Ceq7_0 p0 p1) =  (evalU7_1 env p0) ==~ (evalU7_1 env p1) 
eval7_0 env (Cless7_0 p0 p1) =  (evalU7_1 env p0) <~ (evalU7_1 env p1) 
eval7_0 env (Cleq7_0 p0 p1) =  (evalU7_1 env p0) <=~ (evalU7_1 env p1) 
eval7_0 env (Cand7_0 p0 p1) =  (evalU7_0 env p0) &&~ (evalU7_0 env p1) 
eval7_0 env (Cor7_0 p0 p1) =  (evalU7_0 env p0) ||~ (evalU7_0 env p1) 
eval7_0 env (Cnot7_0 p0) =  mrgIte ((evalU7_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU7_0 :: RefEnv -> UnionM Expr7_0 -> SymBool
evalU7_0 env = onUnion (eval7_0 env)

eval7_1 :: RefEnv -> Expr7_1 -> SymInteger
eval7_1 env (Param07_1) = evalVar0 env "h1"
eval7_1 env (Param37_1) = evalVar0 env "h2"
eval7_1 env (Cadd7_1 p0 p1) =  (evalU7_1 env p0) + (evalU7_1 env p1) 
eval7_1 env (Csub7_1 p0 p1) =  (evalU7_1 env p0) - (evalU7_1 env p1) 
eval7_1 env (Czero7_1) = 0
eval7_1 env (CIte7_1 p0 p1 p2) = mrgIte ((evalU7_0 env p0) ==~ (toSym True)) (evalU7_1 env p1) (evalU7_1 env p2)

evalU7_1 :: RefEnv -> UnionM Expr7_1 -> SymInteger
evalU7_1 env = onUnion (eval7_1 env)

eval8_0 :: RefEnv -> Expr8_0 -> SymBool
eval8_0 env (Param18_0) = evalVar1 env "tmp1"
eval8_0 env (Ceq8_0 p0 p1) =  (evalU8_1 env p0) ==~ (evalU8_1 env p1) 
eval8_0 env (Cless8_0 p0 p1) =  (evalU8_1 env p0) <~ (evalU8_1 env p1) 
eval8_0 env (Cleq8_0 p0 p1) =  (evalU8_1 env p0) <=~ (evalU8_1 env p1) 
eval8_0 env (Cand8_0 p0 p1) =  (evalU8_0 env p0) &&~ (evalU8_0 env p1) 
eval8_0 env (Cor8_0 p0 p1) =  (evalU8_0 env p0) ||~ (evalU8_0 env p1) 
eval8_0 env (Cnot8_0 p0) =  mrgIte ((evalU8_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU8_0 :: RefEnv -> UnionM Expr8_0 -> SymBool
evalU8_0 env = onUnion (eval8_0 env)

eval8_1 :: RefEnv -> Expr8_1 -> SymInteger
eval8_1 env (Cadd8_1 p0 p1) =  (evalU8_1 env p0) + (evalU8_1 env p1) 
eval8_1 env (Csub8_1 p0 p1) =  (evalU8_1 env p0) - (evalU8_1 env p1) 
eval8_1 env (Czero8_1) = 0
eval8_1 env (CIte8_1 p0 p1 p2) = mrgIte ((evalU8_0 env p0) ==~ (toSym True)) (evalU8_1 env p1) (evalU8_1 env p2)

evalU8_1 :: RefEnv -> UnionM Expr8_1 -> SymInteger
evalU8_1 env = onUnion (eval8_1 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
a b p 

Hole grammar for #1
t a h p 

Hole grammar for #2
t a h res p 

Hole grammar for #3
t p b h 

Hole grammar for #4
t res p b h 

Hole grammar for #5
p h2 h1 t2 t1 

Hole grammar for #6
h1 t1 t2 h2 p res 

Hole grammar for #7
h1 t1 t2 h2 p res 

Hole grammar for #8
p tmp1 
-}

data List
  = Elt SymInteger
  | Cons SymInteger List
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

type TwoList = (List, List)

is_sorted :: List -> SymBool
is_sorted xs = 
  let
    aux =
      (let
        f pre xs = 
          case xs of
            Elt x -> pre >=~  x
            Cons h t -> (pre >=~  h) &&~  (f h t)
          
      in
      f )
  in
  case xs of
    Elt x -> (toSym True)
    Cons h t -> aux h t
  
is_sorted_pair :: TwoList -> SymBool
is_sorted_pair p = 
  (is_sorted (fst p)) &&~  (is_sorted (snd p))

find w = 
  let
    f xs = 
      case xs of
        Elt x -> x ==~  w
        Cons h t -> (w ==~  h) ||~  (f t)
      
  in
  f 

spec p = 
  let
    f xs = 
      case xs of
        Elt a -> find a (fst p)
        Cons h t -> (find h (fst p)) ||~  (f t)
      
  in
  f  (snd p)

target = 
  let
    f p = 
      case p of
        ((Elt a), (Elt b)) -> 
          evalU0_0 (RefEnv [("a", (Env0 a)), ("b", (Env0 b))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
        ((Elt a), (Cons h t)) -> mrgIte (a >~  h)
            (evalU1_0 (RefEnv [("a", (Env0 a)), ("h", (Env0 h))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0)))
            (let
              res =
                (f ((Elt a), t))
            in
            evalU2_0 (RefEnv [("res", (Env1 res)), ("a", (Env0 a)), ("h", (Env0 h))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0)))
        ((Cons h t), (Elt b)) -> mrgIte (b >~  h)
            (evalU3_0 (RefEnv [("b", (Env0 b)), ("h", (Env0 h))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0)))
            (let
              res =
                (f (t, (Elt b)))
            in
            evalU4_0 (RefEnv [("res", (Env1 res)), ("b", (Env0 b)), ("h", (Env0 h))]) ((genSym (1::Int) "hole4") :: (UnionM Expr4_0)))
        ((Cons h1 t1), (Cons h2 t2)) -> mrgIte (h1 ==~  h2)
            (evalU5_0 (RefEnv [("h2", (Env0 h2)), ("h1", (Env0 h1))]) ((genSym (1::Int) "hole5") :: (UnionM Expr5_0)))
            (mrgIte (h1 >~  h2)
              (let
                res =
                  (f (t1, (snd p)))
              in
              evalU6_0 (RefEnv [("res", (Env1 res)), ("h1", (Env0 h1)), ("h2", (Env0 h2))]) ((genSym (1::Int) "hole6") :: (UnionM Expr6_0)))
              (let
                res =
                  (f ((fst p), t2))
              in
              evalU7_0 (RefEnv [("res", (Env1 res)), ("h1", (Env0 h1)), ("h2", (Env0 h2))]) ((genSym (1::Int) "hole7") :: (UnionM Expr7_0))))
      
  in
  f 

main' p = 
  mrgIte (is_sorted_pair p)
    (let
      tmp1 =
        (target p)
    in
    evalU8_0 (RefEnv [("tmp1", (Env1 tmp1))]) ((genSym (1::Int) "hole8") :: (UnionM Expr8_0)))
    ((toSym False))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(TwoList, Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(TwoList, Bool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((((Cons ((1)) ((Elt (-2))))) ((Cons ((5)) ((Cons ((5)) ((Cons ((5)) ((Cons ((4)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((-3)) ((Elt (-5))))))))))))))))))), (toSym True))
                , (((((Cons ((5)) ((Cons ((2)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((-2)) ((Elt (-2))))))))))))) ((Cons ((4)) ((Cons ((4)) ((Cons ((0)) ((Elt (-2))))))))))), (toSym True))
                , (((((Cons ((4)) ((Cons ((1)) ((Elt (-2))))))) ((Cons ((5)) ((Cons ((4)) ((Cons ((3)) ((Cons ((3)) ((Cons ((1)) ((Cons ((1)) ((Elt (-1))))))))))))))))), (toSym True))
                , (((((Cons ((-1)) ((Elt (-3))))) ((Cons ((4)) ((Cons ((-1)) ((Cons ((-2)) ((Cons ((-3)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (-3))))))))))))))))), (toSym True))
                , (((((Cons ((-1)) ((Cons ((-2)) ((Cons ((-5)) ((Elt (-5))))))))) ((Cons ((4)) ((Cons ((-1)) ((Cons ((-5)) ((Cons ((-5)) ((Elt (-5))))))))))))), (toSym True))
                , (((((Cons ((1)) ((Cons ((1)) ((Cons ((-1)) ((Elt (-3))))))))) ((Cons ((0)) ((Cons ((-1)) ((Cons ((-1)) ((Cons ((-1)) ((Elt (-5))))))))))))), (toSym True))
                , (((((Cons ((-2)) ((Elt (-5))))) ((Cons ((5)) ((Cons ((2)) ((Cons ((1)) ((Cons ((-2)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (-4))))))))))))))))), (toSym True))
                , (((((Cons ((3)) ((Cons ((-1)) ((Cons ((-2)) ((Cons ((-2)) ((Elt (-3))))))))))) ((Cons ((1)) ((Cons ((1)) ((Cons ((-1)) ((Elt (-1))))))))))), (toSym True))
                , (((((Cons ((1)) ((Cons ((0)) ((Cons ((-3)) ((Cons ((-4)) ((Cons ((-4)) ((Elt (-4))))))))))))) ((Cons ((5)) ((Cons ((2)) ((Elt (-4))))))))), (toSym True))
                , (((((Cons ((0)) ((Cons ((0)) ((Cons ((-2)) ((Elt (-4))))))))) ((Cons ((3)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((-3)) ((Elt (-3))))))))))))), (toSym True))
                , (((((Cons ((3)) ((Cons ((0)) ((Cons ((0)) ((Cons ((0)) ((Cons ((-2)) ((Elt (-5))))))))))))) ((Cons ((-2)) ((Cons ((-2)) ((Elt (-3))))))))), (toSym True))
                , (((((Cons ((3)) ((Cons ((3)) ((Cons ((-5)) ((Cons ((-5)) ((Elt (-5))))))))))) ((Cons ((3)) ((Cons ((0)) ((Cons ((-1)) ((Elt (-1))))))))))), (toSym True))
                , (((((Cons ((4)) ((Cons ((1)) ((Cons ((0)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((-2)) ((Elt (-5))))))))))))))) ((Cons ((-2)) ((Elt (-4))))))), (toSym True))
                , (((((Cons ((4)) ((Cons ((3)) ((Cons ((-2)) ((Elt (-4))))))))) ((Cons ((4)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((-3)) ((Elt (-3))))))))))))), (toSym True))
                , (((((Cons ((2)) ((Cons ((2)) ((Cons ((-2)) ((Cons ((-2)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (-4))))))))))))))) ((Cons ((2)) ((Elt (0))))))), (toSym True))
                , (((((Cons ((5)) ((Cons ((4)) ((Elt (-5))))))) ((Cons ((5)) ((Cons ((2)) ((Cons ((-1)) ((Cons ((-2)) ((Cons ((-3)) ((Elt (-3))))))))))))))), (toSym True))
                , (((((Cons ((4)) ((Cons ((1)) ((Elt (-2))))))) ((Cons ((4)) ((Cons ((1)) ((Cons ((0)) ((Cons ((-1)) ((Cons ((-3)) ((Elt (-3))))))))))))))), (toSym True))
                , (((((Cons ((5)) ((Cons ((0)) ((Cons ((0)) ((Cons ((-2)) ((Elt (-3))))))))))) ((Cons ((5)) ((Cons ((5)) ((Cons ((-3)) ((Elt (-4))))))))))), (toSym True))
                , (((((Cons ((3)) ((Cons ((3)) ((Elt (0))))))) ((Cons ((3)) ((Cons ((0)) ((Cons ((-1)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (-5))))))))))))))), (toSym True))
                , (((((Cons ((3)) ((Cons ((3)) ((Cons ((3)) ((Cons ((-1)) ((Cons ((-2)) ((Elt (-2))))))))))))) ((Cons ((3)) ((Cons ((1)) ((Elt (-5))))))))), (toSym True))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
