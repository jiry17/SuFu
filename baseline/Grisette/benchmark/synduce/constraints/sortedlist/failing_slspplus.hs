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
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
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

{- env_type_list: 
SymInteger
-}

-- output_type: Int
-- param_list: xs a
data Expr0_0
  = Param10_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_1) (UnionM Expr0_0) (UnionM Expr0_0)
  | Min0_0 (UnionM Expr0_0) (UnionM Expr0_0)
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
-- param_list: h xs tmp1 t
data Expr1_0
  = Param01_0
  | Param21_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_1) (UnionM Expr1_0) (UnionM Expr1_0)
  | Min1_0 (UnionM Expr1_0) (UnionM Expr1_0)
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
-- param_list: c tmp2 x
data Expr2_0
  = Param12_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_1) (UnionM Expr2_0) (UnionM Expr2_0)
  | Min2_0 (UnionM Expr2_0) (UnionM Expr2_0)
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
-- param_list: r c piv tmp3 l
data Expr3_0
  = Param23_0
  | Param33_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_1) (UnionM Expr3_0) (UnionM Expr3_0)
  | Min3_0 (UnionM Expr3_0) (UnionM Expr3_0)
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
-- param_list: tmp4 tmp5 l r piv c
data Expr4_0
  = Param04_0
  | Param14_0
  | Param44_0
  | Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_1) (UnionM Expr4_0) (UnionM Expr4_0)
  | Min4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Max4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Ceq4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cless4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cleq4_1 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cand4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cor4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Cnot4_1 (UnionM Expr4_1)
  | CFalse4_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

-- output_type: Int
-- param_list: tmp6 c
data Expr5_0
  = Param05_0
  | Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_1) (UnionM Expr5_0) (UnionM Expr5_0)
  | Min5_0 (UnionM Expr5_0) (UnionM Expr5_0)
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
    genSingle0 = [mrgParam10_0] ++ [mrgCzero0_0]
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
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e1_0 e0_4 e0_5] ++ [mrgMin0_0 e0_6 e0_7] ++ [mrgMax0_0 e0_8 e0_9])
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
    genSingle0 = [mrgParam01_0] ++ [mrgParam21_0] ++ [mrgCzero1_0]
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
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e1_0 e0_4 e0_5] ++ [mrgMin1_0 e0_6 e0_7] ++ [mrgMax1_0 e0_8 e0_9])
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
    genSingle0 = [mrgParam12_0] ++ [mrgCzero2_0]
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
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e1_0 e0_4 e0_5] ++ [mrgMin2_0 e0_6 e0_7] ++ [mrgMax2_0 e0_8 e0_9])
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
    genSingle0 = [mrgParam23_0] ++ [mrgParam33_0] ++ [mrgCzero3_0]
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
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e1_0 e0_4 e0_5] ++ [mrgMin3_0 e0_6 e0_7] ++ [mrgMax3_0 e0_8 e0_9])
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
    genSingle0 = [mrgParam04_0] ++ [mrgParam14_0] ++ [mrgParam44_0] ++ [mrgCzero4_0]
    genSingle1 = [mrgCFalse4_1]
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
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd4_0 e0_0 e0_1] ++ [mrgCsub4_0 e0_2 e0_3] ++ [mrgCIte4_0 e1_0 e0_4 e0_5] ++ [mrgMin4_0 e0_6 e0_7] ++ [mrgMax4_0 e0_8 e0_9])
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
        e0_8 <- (gen0 (gendepth - 1))
        e0_9 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd5_0 e0_0 e0_1] ++ [mrgCsub5_0 e0_2 e0_3] ++ [mrgCIte5_0 e1_0 e0_4 e0_5] ++ [mrgMin5_0 e0_6 e0_7] ++ [mrgMax5_0 e0_8 e0_9])
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
eval0_0 env (Param10_0) = evalVar0 env "a"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)
eval0_0 env (Min0_0 p0 p1) = min' (evalU0_0 env p0) (evalU0_0 env p1)
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
eval1_0 env (Param01_0) = evalVar0 env "h"
eval1_0 env (Param21_0) = evalVar0 env "tmp1"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)
eval1_0 env (Min1_0 p0 p1) = min' (evalU1_0 env p0) (evalU1_0 env p1)
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
eval2_0 env (Param12_0) = evalVar0 env "tmp2"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Min2_0 p0 p1) = min' (evalU2_0 env p0) (evalU2_0 env p1)
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
eval3_0 env (Param23_0) = evalVar0 env "piv"
eval3_0 env (Param33_0) = evalVar0 env "tmp3"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Min3_0 p0 p1) = min' (evalU3_0 env p0) (evalU3_0 env p1)
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
eval4_0 env (Param04_0) = evalVar0 env "tmp4"
eval4_0 env (Param14_0) = evalVar0 env "tmp5"
eval4_0 env (Param44_0) = evalVar0 env "piv"
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_1 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Min4_0 p0 p1) = min' (evalU4_0 env p0) (evalU4_0 env p1)
eval4_0 env (Max4_0 p0 p1) = max' (evalU4_0 env p0) (evalU4_0 env p1)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymBool
eval4_1 env (Ceq4_1 p0 p1) =  (evalU4_0 env p0) ==~ (evalU4_0 env p1) 
eval4_1 env (Cless4_1 p0 p1) =  (evalU4_0 env p0) <~ (evalU4_0 env p1) 
eval4_1 env (Cleq4_1 p0 p1) =  (evalU4_0 env p0) <=~ (evalU4_0 env p1) 
eval4_1 env (Cand4_1 p0 p1) =  (evalU4_1 env p0) &&~ (evalU4_1 env p1) 
eval4_1 env (Cor4_1 p0 p1) =  (evalU4_1 env p0) ||~ (evalU4_1 env p1) 
eval4_1 env (Cnot4_1 p0) =  mrgIte ((evalU4_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval4_1 env (CFalse4_1) = (toSym False)

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymBool
evalU4_1 env = onUnion (eval4_1 env)

eval5_0 :: RefEnv -> Expr5_0 -> SymInteger
eval5_0 env (Param05_0) = evalVar0 env "tmp6"
eval5_0 env (Cadd5_0 p0 p1) =  (evalU5_0 env p0) + (evalU5_0 env p1) 
eval5_0 env (Csub5_0 p0 p1) =  (evalU5_0 env p0) - (evalU5_0 env p1) 
eval5_0 env (Czero5_0) = 0
eval5_0 env (CIte5_0 p0 p1 p2) = mrgIte ((evalU5_1 env p0) ==~ (toSym True)) (evalU5_0 env p1) (evalU5_0 env p2)
eval5_0 env (Min5_0 p0 p1) = min' (evalU5_0 env p0) (evalU5_0 env p1)
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
xs a 

Hole grammar for #1
h xs tmp1 t 

Hole grammar for #2
c tmp2 x 

Hole grammar for #3
r c piv tmp3 l 

Hole grammar for #4
tmp4 tmp5 l r piv c 

Hole grammar for #5
tmp6 c 
-}

data List
  = Elt SymInteger
  | Cons SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data NList
  = Line List
  | Ncons List NList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon NList, ExtractSymbolics)
    via (Default NList)

data CList
  = Sglt List
  | Cat CList SymInteger CList
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

c2n = 
  let
    aux =
      (let
        f pre c = 
          case c of
            Sglt a -> Ncons a pre
            Cat l _ r -> f (f pre r) l
          
      in
      f )
  in
  let
    f c = 
      case c of
        Sglt a -> Line a
        Cat l _ r -> aux (f r) l
      
  in
  f 

lsum = 
  let
    f xs = 
      case xs of
        Elt x -> x
        Cons h t -> h +  (f t)
      
  in
  f 

min' a b = 
  mrgIte (a <~  b)
    (a)
    (b)

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

sorted = 
  let
    lmin =
      (let
        f c = 
          case c of
            Sglt a -> lsum a
            Cat l _ r -> min' (f l) (f r)
          
      in
      f )
  in
  let
    lmax =
      (let
        f c = 
          case c of
            Sglt a -> lsum a
            Cat l _ r -> max' (f l) (f r)
          
      in
      f )
  in
  let
    f c = 
      case c of
        Sglt a -> (toSym True)
        Cat l piv r -> (((lmax l) <~  piv) &&~  (piv <~  (lmin r))) &&~  ((f l) &&~  (f r))
      
  in
  f 

spec xs = 
  (fst (let
    f xs = 
      case xs of
        Line a -> ((max' 0 (lsum a)), ((lsum a) >=~  0))
        Ncons h t -> 
          let
            res =
              (f t)
          in
          let
            line_sum =
              (lsum h)
          in
          ((mrgIte ((snd res) &&~  (line_sum >=~  0))
            ((fst res) +  line_sum)
            ((fst res))), ((snd res) &&~  (line_sum >=~  0)))
      
  in
  f  xs))

target = 
  let
    list_repr =
      (let
        f xs = 
          case xs of
            Elt a -> 
              evalU0_0 (RefEnv [("a", (Env0 a))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
            Cons h t -> 
              let
                tmp1 =
                  (f t)
              in
              evalU1_0 (RefEnv [("h", (Env0 h)), ("tmp1", (Env0 tmp1))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))
          
      in
      f )
  in
  let
    f c = 
      case c of
        Sglt x -> 
          let
            tmp2 =
              (list_repr x)
          in
          evalU2_0 (RefEnv [("tmp2", (Env0 tmp2))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))
        Cat l piv r -> mrgIte (piv <=~  0)
            (let
              tmp3 =
                (f r)
            in
            evalU3_0 (RefEnv [("piv", (Env0 piv)), ("tmp3", (Env0 tmp3))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0)))
            (let
              tmp4 =
                (f l)
            in
            let
              tmp5 =
                (f r)
            in
            evalU4_0 (RefEnv [("tmp4", (Env0 tmp4)), ("tmp5", (Env0 tmp5)), ("piv", (Env0 piv))]) ((genSym (1::Int) "hole4") :: (UnionM Expr4_0)))
      
  in
  f 

main' c = 
  mrgIte (sorted c)
    (let
      tmp6 =
        (target c)
    in
    evalU5_0 (RefEnv [("tmp6", (Env0 tmp6))]) ((genSym (1::Int) "hole5") :: (UnionM Expr5_0)))
    (0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(CList, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(CList, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Sglt (Cons ((-1)) ((Cons ((-3)) ((Cons ((-1)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((4)) ((Elt (-2))))))))))))))))))))), (5))
                , ((((Sglt (Cons ((-4)) ((Cons ((4)) ((Cons ((-3)) ((Cons ((4)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((2)) ((Cons ((-3)) ((Elt (-3))))))))))))))))))))), (1))
                , ((((Sglt (Cons ((-1)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((-3)) ((Cons ((3)) ((Cons ((5)) ((Cons ((-4)) ((Cons ((-1)) ((Elt (5))))))))))))))))))))), (6))
                , ((((Sglt (Cons ((-2)) ((Cons ((3)) ((Cons ((-1)) ((Cons ((4)) ((Cons ((-3)) ((Cons ((3)) ((Cons ((-4)) ((Cons ((-1)) ((Elt (3))))))))))))))))))))), (2))
                , ((((Sglt (Cons ((5)) ((Cons ((4)) ((Cons ((5)) ((Cons ((3)) ((Cons ((-1)) ((Cons ((-5)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (-4))))))))))))))))))))), (1))
                , ((((Sglt (Cons ((-4)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((-4)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((-2)) ((Cons ((4)) ((Elt (3))))))))))))))))))))), (3))
                , ((((Sglt (Cons ((2)) ((Cons ((-2)) ((Cons ((-2)) ((Cons ((-2)) ((Cons ((4)) ((Cons ((5)) ((Cons ((0)) ((Cons ((-1)) ((Elt (-1))))))))))))))))))))), (3))
                , ((((Sglt (Cons ((-2)) ((Cons ((-2)) ((Cons ((-5)) ((Cons ((5)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((4)) ((Cons ((4)) ((Elt (-5))))))))))))))))))))), (3))
                , ((((Sglt (Cons ((-1)) ((Cons ((-4)) ((Cons ((3)) ((Cons ((1)) ((Cons ((5)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((-1)) ((Elt (-4))))))))))))))))))))), (3))
                , ((((Sglt (Cons ((5)) ((Cons ((-4)) ((Cons ((3)) ((Cons ((-2)) ((Cons ((-2)) ((Cons ((-1)) ((Cons ((4)) ((Cons ((-1)) ((Elt (4))))))))))))))))))))), (6))
                , ((((Sglt (Cons ((-5)) ((Cons ((-5)) ((Cons ((5)) ((Cons ((3)) ((Cons ((-3)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((5)) ((Elt (-3))))))))))))))))))))), (1))
                , ((((Sglt (Cons ((-3)) ((Cons ((4)) ((Cons ((2)) ((Cons ((-2)) ((Cons ((-1)) ((Cons ((-1)) ((Cons ((3)) ((Cons ((-1)) ((Elt (4))))))))))))))))))))), (5))
                , ((((Sglt (Cons ((-3)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((1)) ((Cons ((-2)) ((Cons ((4)) ((Cons ((3)) ((Elt (-2))))))))))))))))))))), (4))
                , ((((Sglt (Cons ((-4)) ((Cons ((-4)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((2)) ((Elt (-4))))))))))))))))))))), (1))
                , ((((Sglt (Cons ((0)) ((Cons ((1)) ((Cons ((-2)) ((Cons ((-1)) ((Cons ((-1)) ((Cons ((4)) ((Cons ((-3)) ((Cons ((-2)) ((Elt (5))))))))))))))))))))), (1))
                , ((((Sglt (Cons ((-4)) ((Cons ((-2)) ((Cons ((-1)) ((Cons ((5)) ((Cons ((-4)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((3)) ((Elt (3))))))))))))))))))))), (2))
                , ((((Sglt (Cons ((-5)) ((Cons ((4)) ((Cons ((-2)) ((Cons ((-3)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((1)) ((Cons ((-2)) ((Elt (5))))))))))))))))))))), (2))
                , ((((Sglt (Cons ((-1)) ((Cons ((4)) ((Cons ((5)) ((Cons ((-2)) ((Cons ((0)) ((Cons ((-1)) ((Cons ((-3)) ((Cons ((-3)) ((Elt (4))))))))))))))))))))), (3))
                , ((((Sglt (Cons ((4)) ((Cons ((-2)) ((Cons ((-3)) ((Cons ((-4)) ((Cons ((-1)) ((Cons ((4)) ((Cons ((-1)) ((Cons ((2)) ((Elt (3))))))))))))))))))))), (2))
                , ((((Sglt (Cons ((-2)) ((Cons ((3)) ((Cons ((5)) ((Cons ((-3)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((3)) ((Cons ((-1)) ((Elt (-5))))))))))))))))))))), (4))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
