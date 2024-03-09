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

-- output_type: Int
-- param_list: op tmp2 t1 t2 tmp1 t
data Expr0_0
  = Param10_0
  | Param40_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_1) (UnionM Expr0_0) (UnionM Expr0_0)
  | Max0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Ceq0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cless0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cleq0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cand0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cor0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cnot0_1 (UnionM Expr0_1)
  | CFalse0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

-- output_type: Int
-- param_list: tmp3 t op
data Expr1_0
  = Param01_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_1) (UnionM Expr1_0) (UnionM Expr1_0)
  | Max1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Ceq1_1 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cless1_1 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cleq1_1 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cand1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cor1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cnot1_1 (UnionM Expr1_1)
  | CFalse1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

-- output_type: Int
-- param_list: t tmp4 i
data Expr2_0
  = Param22_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_1) (UnionM Expr2_0) (UnionM Expr2_0)
  | Max2_0 (UnionM Expr2_0) (UnionM Expr2_0)
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

-- output_type: Int
-- param_list: t tmp5 i
data Expr3_0
  = Param23_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_1) (UnionM Expr3_0) (UnionM Expr3_0)
  | Max3_0 (UnionM Expr3_0) (UnionM Expr3_0)
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

-- output_type: Int
-- param_list: t tmp6 i
data Expr4_0
  = Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_1) (UnionM Expr4_0) (UnionM Expr4_0)
  | Max4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param24_1
  | Ceq4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cless4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cleq4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cand4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cor4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cnot4_1 (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

-- output_type: Int
-- param_list: tmp7 xs
data Expr5_0
  = Param05_0
  | Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_1) (UnionM Expr5_0) (UnionM Expr5_0)
  | Max5_0 (UnionM Expr5_0) (UnionM Expr5_0)
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

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam10_0] ++ [mrgParam40_0] ++ [mrgCzero0_0]
    genSingle1 = [mrgCFalse0_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e1_0 e0_4 e0_5] ++ [mrgMax0_0 e0_6 e0_7])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq0_1 e0_0 e0_1] ++ [mrgCless0_1 e0_2 e0_3] ++ [mrgCleq0_1 e0_4 e0_5] ++ [mrgCand0_1 e1_0 e1_1] ++ [mrgCor0_1 e1_2 e1_3] ++ [mrgCnot0_1 e1_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam01_0] ++ [mrgCzero1_0]
    genSingle1 = [mrgCFalse1_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e1_0 e0_4 e0_5] ++ [mrgMax1_0 e0_6 e0_7])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq1_1 e0_0 e0_1] ++ [mrgCless1_1 e0_2 e0_3] ++ [mrgCleq1_1 e0_4 e0_5] ++ [mrgCand1_1 e1_0 e1_1] ++ [mrgCor1_1 e1_2 e1_3] ++ [mrgCnot1_1 e1_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam22_0] ++ [mrgCzero2_0]
    genSingle1 = [mrgCFalse2_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e1_0 e0_4 e0_5] ++ [mrgMax2_0 e0_6 e0_7])
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

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam23_0] ++ [mrgCzero3_0]
    genSingle1 = [mrgCFalse3_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e1_0 e0_4 e0_5] ++ [mrgMax3_0 e0_6 e0_7])
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

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero4_0]
    genSingle1 = [mrgParam24_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd4_0 e0_0 e0_1] ++ [mrgCsub4_0 e0_2 e0_3] ++ [mrgCIte4_0 e1_0 e0_4 e0_5] ++ [mrgMax4_0 e0_6 e0_7])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq4_1 e0_0 e0_1] ++ [mrgCless4_1 e0_2 e0_3] ++ [mrgCleq4_1 e0_4 e0_5] ++ [mrgCand4_1 e1_0 e1_1] ++ [mrgCor4_1 e1_2 e1_3] ++ [mrgCnot4_1 e1_4])
        return res

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam05_0] ++ [mrgCzero5_0]
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

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param10_0) = evalVar0 env "tmp2"
eval0_0 env (Param40_0) = evalVar0 env "tmp1"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)
eval0_0 env (Max0_0 p0 p1) = max' (evalU0_0 env p0) (evalU0_0 env p1)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> SymInteger
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymBool
eval0_1 env (Ceq0_1 p0 p1) =  (evalU0_0 env p0) ==~ (evalU0_0 env p1) 
eval0_1 env (Cless0_1 p0 p1) =  (evalU0_0 env p0) <~ (evalU0_0 env p1) 
eval0_1 env (Cleq0_1 p0 p1) =  (evalU0_0 env p0) <=~ (evalU0_0 env p1) 
eval0_1 env (Cand0_1 p0 p1) =  (evalU0_1 env p0) &&~ (evalU0_1 env p1) 
eval0_1 env (Cor0_1 p0 p1) =  (evalU0_1 env p0) ||~ (evalU0_1 env p1) 
eval0_1 env (Cnot0_1 p0) =  mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_1 env (CFalse0_1) = (toSym False)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymBool
evalU0_1 env = onUnion (eval0_1 env)

eval1_0 :: RefEnv -> Expr1_0 -> SymInteger
eval1_0 env (Param01_0) = evalVar0 env "tmp3"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)
eval1_0 env (Max1_0 p0 p1) = max' (evalU1_0 env p0) (evalU1_0 env p1)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymInteger
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymBool
eval1_1 env (Ceq1_1 p0 p1) =  (evalU1_0 env p0) ==~ (evalU1_0 env p1) 
eval1_1 env (Cless1_1 p0 p1) =  (evalU1_0 env p0) <~ (evalU1_0 env p1) 
eval1_1 env (Cleq1_1 p0 p1) =  (evalU1_0 env p0) <=~ (evalU1_0 env p1) 
eval1_1 env (Cand1_1 p0 p1) =  (evalU1_1 env p0) &&~ (evalU1_1 env p1) 
eval1_1 env (Cor1_1 p0 p1) =  (evalU1_1 env p0) ||~ (evalU1_1 env p1) 
eval1_1 env (Cnot1_1 p0) =  mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_1 env (CFalse1_1) = (toSym False)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymBool
evalU1_1 env = onUnion (eval1_1 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Param22_0) = evalVar0 env "i"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Max2_0 p0 p1) = max' (evalU2_0 env p0) (evalU2_0 env p1)

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

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Param23_0) = evalVar0 env "i"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Max3_0 p0 p1) = max' (evalU3_0 env p0) (evalU3_0 env p1)

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

eval4_0 :: RefEnv -> Expr4_0 -> SymInteger
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_1 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Max4_0 p0 p1) = max' (evalU4_0 env p0) (evalU4_0 env p1)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymBool
eval4_1 env (Param24_1) = evalVar1 env "i"
eval4_1 env (Ceq4_1 p0 p1) =  (evalU4_0 env p0) ==~ (evalU4_0 env p1) 
eval4_1 env (Cless4_1 p0 p1) =  (evalU4_0 env p0) <~ (evalU4_0 env p1) 
eval4_1 env (Cleq4_1 p0 p1) =  (evalU4_0 env p0) <=~ (evalU4_0 env p1) 
eval4_1 env (Cand4_1 p0 p1) =  (evalU4_1 env p0) &&~ (evalU4_1 env p1) 
eval4_1 env (Cor4_1 p0 p1) =  (evalU4_1 env p0) ||~ (evalU4_1 env p1) 
eval4_1 env (Cnot4_1 p0) =  mrgIte ((evalU4_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymBool
evalU4_1 env = onUnion (eval4_1 env)

eval5_0 :: RefEnv -> Expr5_0 -> SymInteger
eval5_0 env (Param05_0) = evalVar0 env "tmp7"
eval5_0 env (Cadd5_0 p0 p1) =  (evalU5_0 env p0) + (evalU5_0 env p1) 
eval5_0 env (Csub5_0 p0 p1) =  (evalU5_0 env p0) - (evalU5_0 env p1) 
eval5_0 env (Czero5_0) = 0
eval5_0 env (CIte5_0 p0 p1 p2) = mrgIte ((evalU5_1 env p0) ==~ (toSym True)) (evalU5_0 env p1) (evalU5_0 env p2)
eval5_0 env (Max5_0 p0 p1) = max' (evalU5_0 env p0) (evalU5_0 env p1)

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
op tmp2 t1 t2 tmp1 t 

Hole grammar for #1
tmp3 t op 

Hole grammar for #2
t tmp4 i 

Hole grammar for #3
t tmp5 i 

Hole grammar for #4
t tmp6 i 

Hole grammar for #5
tmp7 xs 
-}

data ArithOp
  = APlus Unit
  | AMinus Unit
  | AGt Unit
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon ArithOp, ExtractSymbolics)
    via (Default ArithOp)

data BoolOp
  = BNot Unit
  | BAnd Unit
  | BOr Unit
  | BEq Unit
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon BoolOp, ExtractSymbolics)
    via (Default BoolOp)

data Term
  = TArithBin ArithOp Term Term
  | TBoolBin BoolOp Term Term
  | TArithUn ArithOp Term
  | TBoolUn BoolOp Term
  | TVar SymInteger
  | TCInt SymInteger
  | TCBool SymBool
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Term, ExtractSymbolics)
    via (Default Term)

data Op
  = OpPlus Unit
  | OpMinus Unit
  | OpNot Unit
  | OpAnd Unit
  | OpOr Unit
  | OpGt Unit
  | OpEq Unit
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Op, ExtractSymbolics)
    via (Default Op)

data Term2
  = Bin Op Term2 Term2
  | Un Op Term2
  | Var SymInteger
  | CInt SymInteger
  | CBool SymBool
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Term2, ExtractSymbolics)
    via (Default Term2)
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

mk_bin t1 t2 op = 
  case op of
    OpPlus _ -> TArithBin (APlus Unit) t1 t2
    OpMinus _ -> TArithBin (AMinus Unit) t1 t2
    OpNot _ -> TBoolBin (BNot Unit) t1 t2
    OpAnd _ -> TBoolBin (BAnd Unit) t1 t2
    OpOr _ -> TBoolBin (BOr Unit) t1 t2
    OpGt _ -> TArithBin (AGt Unit) t1 t2
    OpEq _ -> TBoolBin (BEq Unit) t1 t2
  

mk_un t op = 
  case op of
    OpPlus _ -> TArithUn (APlus Unit) t
    OpMinus _ -> TArithUn (AMinus Unit) t
    OpNot _ -> TBoolUn (BNot Unit) t
    OpAnd _ -> TBoolUn (BAnd Unit) t
    OpOr _ -> TBoolUn (BOr Unit) t
    OpGt _ -> TArithUn (AGt Unit) t
    OpEq _ -> TBoolUn (BEq Unit) t
  

repr = 
  let
    f t = 
      case t of
        Bin op t1 t2 -> mk_bin (f t1) (f t2) op
        Un o x -> mk_un (f x) o
        Var i -> TVar i
        CInt i -> TCInt i
        CBool b -> TCBool b
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

spec :: Term -> SymInteger
spec = 
  let
    f t = 
      case t of
        TArithBin op t1 t2 -> 1 +  (max' (f t1) (f t2))
        TBoolBin op t1 t2 -> 1 +  (max' (f t1) (f t2))
        TArithUn op t -> 1 +  (f t)
        TBoolUn op t -> 1 +  (f t)
        TVar _ -> 1
        TCInt _ -> 1
        TCBool _ -> 1
  in
  f 

f :: Term2 -> SymInteger
f t = 
      case t of
        Bin op t1 t2 -> 
          let
            tmp1 =
              (f t1)
          in
          let
            tmp2 =
              (f t2)
          in
          evalU0_0 (RefEnv [("tmp2", (Env0 tmp2)), ("tmp1", (Env0 tmp1))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0))
        Un op t -> 
          let
            tmp3 =
              (f t)
          in
          evalU1_0 (RefEnv [("tmp3", (Env0 tmp3))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))
        Var i -> 
          let
            tmp4 =
              (Var i)
          in
          evalU2_0 (RefEnv [("i", (Env0 i))]) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))
        CInt i -> 
          let
            tmp5 =
              (CInt i)
          in
          evalU3_0 (RefEnv [("i", (Env0 i))]) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))
        CBool i -> 
          let
            tmp6 =
              (CBool i)
          in
          evalU4_0 (RefEnv [("i", (Env1 i))]) ((genSym (4::Int) "hole4") :: (UnionM Expr4_0))

target :: Term2 -> SymInteger
target = f

main' xs = 
  let
    tmp7 =
      (target xs)
  in
  evalU5_0 (RefEnv [("tmp7", (Env0 tmp7))]) ((genSym (1::Int) "hole5") :: (UnionM Expr5_0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(Term2, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(Term2, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((Bin ((OpNot Unit)) ((Un ((OpEq Unit)) ((Bin ((OpGt Unit)) ((CInt (5))) ((Var (-5))))))) ((Un ((OpOr Unit)) ((Un ((OpPlus Unit)) ((Un ((OpGt Unit)) ((Var (5))))))))))), (5))
                , (((Un ((OpEq Unit)) ((CInt (5))))), (2))
                , (((Un ((OpPlus Unit)) ((CInt (4))))), (2))
                , (((Bin ((OpNot Unit)) ((Un ((OpMinus Unit)) ((Var (2))))) ((Bin ((OpMinus Unit)) ((Un ((OpNot Unit)) ((Var (1))))) ((Bin ((OpGt Unit)) ((CInt (4))) ((Var (-1))))))))), (4))
                , (((Un ((OpGt Unit)) ((Un ((OpMinus Unit)) ((Var (0))))))), (3))
                , (((Un ((OpNot Unit)) ((CBool (toSym False))))), (2))
                , (((CBool (toSym True))), (1))
                , (((Bin ((OpPlus Unit)) ((Var (-3))) ((Un ((OpAnd Unit)) ((Un ((OpOr Unit)) ((CBool (toSym True))))))))), (4))
                , (((Bin ((OpPlus Unit)) ((Bin ((OpGt Unit)) ((Bin ((OpAnd Unit)) ((CBool (toSym False))) ((Bin ((OpEq Unit)) ((CBool (toSym True))) ((CInt (2))))))) ((CBool (toSym True))))) ((Un ((OpEq Unit)) ((CBool (toSym True))))))), (5))
                , (((Un ((OpPlus Unit)) ((Un ((OpAnd Unit)) ((CInt (-1))))))), (3))
                , (((Un ((OpPlus Unit)) ((CInt (2))))), (2))
                , (((Bin ((OpEq Unit)) ((CInt (-1))) ((Un ((OpAnd Unit)) ((Var (2))))))), (3))
                , (((Un ((OpEq Unit)) ((Bin ((OpGt Unit)) ((Bin ((OpGt Unit)) ((Un ((OpGt Unit)) ((Var (-5))))) ((Un ((OpGt Unit)) ((Var (1))))))) ((CInt (-4))))))), (5))
                , (((CInt (-4))), (1))
                , (((Bin ((OpOr Unit)) ((Un ((OpOr Unit)) ((CInt (-4))))) ((CBool (toSym False))))), (3))
                , (((Bin ((OpNot Unit)) ((CInt (-4))) ((Un ((OpAnd Unit)) ((Un ((OpOr Unit)) ((CBool (toSym True))))))))), (4))
                , (((Un ((OpNot Unit)) ((CInt (-3))))), (2))
                , (((Bin ((OpOr Unit)) ((CBool (toSym True))) ((Un ((OpPlus Unit)) ((CInt (-2))))))), (3))
                , (((Var (-4))), (1))
                , (((Bin ((OpGt Unit)) ((CInt (-1))) ((Bin ((OpMinus Unit)) ((Un ((OpOr Unit)) ((Var (-1))))) ((Bin ((OpGt Unit)) ((Var (1))) ((Un ((OpEq Unit)) ((CBool (toSym True))))))))))), (5))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
