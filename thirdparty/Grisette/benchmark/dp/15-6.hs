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
  = Env0 SymInteger
  | Env1 (SymInteger, SymInteger)
  | Env2 SymBool
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

evalVar1 :: RefEnv -> Ident -> (SymInteger, SymInteger)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

evalVar2 :: RefEnv -> Ident -> SymBool
evalVar2 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env2 sym -> sym
      _ -> error "evalVar2: variable type not matched"

{- env_type_list: 
SymInteger
(SymInteger, SymInteger)
SymBool
-}

-- output_type: Bool
-- param_list: r l w
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
  = Param20_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access0_2_0_1 (UnionM Expr0_2)
  | Access1_2_0_1 (UnionM Expr0_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Param00_2
  | Param10_2
  | Prod0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: {Int,Int}
-- param_list: tmp2 r is_used w l
data Expr1_0
  = Param11_0
  | Param41_0
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Param31_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Param21_2
  | Ceq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: {Int,Int}
-- param_list: r is_used w l
data Expr2_0
  = Param02_0
  | Param32_0
  | Prod2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param12_1
  | Ceq2_1 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cless2_1 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cleq2_1 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cand2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cor2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cnot2_1 (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Param22_2
  | Cadd2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Csub2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Czero2_2
  | CIte2_2 (UnionM Expr2_1) (UnionM Expr2_2) (UnionM Expr2_2)
  | Access0_0_2_2 (UnionM Expr2_0)
  | Access1_0_2_2 (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: {Int,Int}
-- param_list: r is_used tmp3 w l
data Expr3_0
  = Param03_0
  | Param43_0
  | Prod3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Ceq3_1 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cless3_1 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cleq3_1 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cand3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cor3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cnot3_1 (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Param23_2
  | Param33_2
  | Cadd3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Csub3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Czero3_2
  | CIte3_2 (UnionM Expr3_1) (UnionM Expr3_2) (UnionM Expr3_2)
  | Access0_0_3_2 (UnionM Expr3_0)
  | Access1_0_3_2 (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: {Int,Int}
-- param_list: t
data Expr4_0
  = Prod4_0 (UnionM Expr4_1) (UnionM Expr4_1)
  | CZero24_0
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

-- output_type: Int
-- param_list: h t xs
data Expr5_0
  = Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_2) (UnionM Expr5_0) (UnionM Expr5_0)
  | Access0_1_5_0 (UnionM Expr5_1)
  | Access1_1_5_0 (UnionM Expr5_1)
  | Max5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Param05_1
  | Prod5_1 (UnionM Expr5_0) (UnionM Expr5_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

data Expr5_2
  = Ceq5_2 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cless5_2 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cleq5_2 (UnionM Expr5_0) (UnionM Expr5_0)
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
$(makeUnionWrapper "mrg" ''Expr5_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgParam20_1] ++ [mrgCzero0_1]
    genSingle2 = [mrgParam00_2] ++ [mrgParam10_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5] ++ [mrgAccess0_2_0_1 e2_0] ++ [mrgAccess1_2_0_1 e2_1])
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
    genSingle0 = [mrgParam11_0] ++ [mrgParam41_0]
    genSingle1 = [mrgParam01_1] ++ [mrgParam31_1] ++ [mrgCzero1_1]
    genSingle2 = [mrgParam21_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1])
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
    genSingle0 = [mrgParam02_0] ++ [mrgParam32_0]
    genSingle1 = [mrgParam12_1]
    genSingle2 = [mrgParam22_2] ++ [mrgCzero2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e2_0 e2_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq2_1 e2_0 e2_1] ++ [mrgCless2_1 e2_2 e2_3] ++ [mrgCleq2_1 e2_4 e2_5] ++ [mrgCand2_1 e1_0 e1_1] ++ [mrgCor2_1 e1_2 e1_3] ++ [mrgCnot2_1 e1_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd2_2 e2_0 e2_1] ++ [mrgCsub2_2 e2_2 e2_3] ++ [mrgCIte2_2 e1_0 e2_4 e2_5] ++ [mrgAccess0_0_2_2 e0_0] ++ [mrgAccess1_0_2_2 e0_1])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam03_0] ++ [mrgParam43_0]
    genSingle1 = [mrgParam13_1]
    genSingle2 = [mrgParam23_2] ++ [mrgParam33_2] ++ [mrgCzero3_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e2_0 e2_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq3_1 e2_0 e2_1] ++ [mrgCless3_1 e2_2 e2_3] ++ [mrgCleq3_1 e2_4 e2_5] ++ [mrgCand3_1 e1_0 e1_1] ++ [mrgCor3_1 e1_2 e1_3] ++ [mrgCnot3_1 e1_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd3_2 e2_0 e2_1] ++ [mrgCsub3_2 e2_2 e2_3] ++ [mrgCIte3_2 e1_0 e2_4 e2_5] ++ [mrgAccess0_0_3_2 e0_0] ++ [mrgAccess1_0_3_2 e0_1])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero24_0]
    genSingle1 = [mrgCzero4_1]
    genSingle2 = [mrgCFalse4_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd4_0 e1_0 e1_1])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd4_1 e1_0 e1_1] ++ [mrgCsub4_1 e1_2 e1_3] ++ [mrgCIte4_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_4_1 e0_0] ++ [mrgAccess1_0_4_1 e0_1])
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

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero5_0]
    genSingle1 = [mrgParam05_1]
    genSingle2 = [mrgCFalse5_2]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd5_0 e0_0 e0_1] ++ [mrgCsub5_0 e0_2 e0_3] ++ [mrgCIte5_0 e2_0 e0_4 e0_5] ++ [mrgAccess0_1_5_0 e1_0] ++ [mrgAccess1_1_5_0 e1_1] ++ [mrgMax5_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd5_1 e0_0 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq5_2 e0_0 e0_1] ++ [mrgCless5_2 e0_2 e0_3] ++ [mrgCleq5_2 e0_4 e0_5] ++ [mrgCand5_2 e2_0 e2_1] ++ [mrgCor5_2 e2_2 e2_3] ++ [mrgCnot5_2 e2_4])
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
eval0_1 env (Param20_1) = evalVar0 env "w"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_2_0_1 p0) = fst (evalU0_2 env p0)
eval0_1 env (Access1_2_0_1 p0) = snd (evalU0_2 env p0)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> (SymInteger, SymInteger)
eval0_2 env (Param00_2) = evalVar1 env "r"
eval0_2 env (Param10_2) = evalVar1 env "l"
eval0_2 env (Prod0_2 p0 p1) = ((evalU0_1 env p0), (evalU0_1 env p1))

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> (SymInteger, SymInteger)
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymInteger, SymInteger)
eval1_0 env (Param11_0) = evalVar1 env "r"
eval1_0 env (Param41_0) = evalVar1 env "l"
eval1_0 env (Prod1_0 p0 p1) = ((evalU1_1 env p0), (evalU1_1 env p1))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param01_1) = evalVar0 env "tmp2"
eval1_1 env (Param31_1) = evalVar0 env "w"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_0_1_1 p0) = fst (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = snd (evalU1_0 env p0)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Param21_2) = evalVar2 env "is_used"
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymInteger, SymInteger)
eval2_0 env (Param02_0) = evalVar1 env "r"
eval2_0 env (Param32_0) = evalVar1 env "l"
eval2_0 env (Prod2_0 p0 p1) = ((evalU2_2 env p0), (evalU2_2 env p1))

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymBool
eval2_1 env (Param12_1) = evalVar2 env "is_used"
eval2_1 env (Ceq2_1 p0 p1) =  (evalU2_2 env p0) ==~ (evalU2_2 env p1) 
eval2_1 env (Cless2_1 p0 p1) =  (evalU2_2 env p0) <~ (evalU2_2 env p1) 
eval2_1 env (Cleq2_1 p0 p1) =  (evalU2_2 env p0) <=~ (evalU2_2 env p1) 
eval2_1 env (Cand2_1 p0 p1) =  (evalU2_1 env p0) &&~ (evalU2_1 env p1) 
eval2_1 env (Cor2_1 p0 p1) =  (evalU2_1 env p0) ||~ (evalU2_1 env p1) 
eval2_1 env (Cnot2_1 p0) =  mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymBool
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymInteger
eval2_2 env (Param22_2) = evalVar0 env "w"
eval2_2 env (Cadd2_2 p0 p1) =  (evalU2_2 env p0) + (evalU2_2 env p1) 
eval2_2 env (Csub2_2 p0 p1) =  (evalU2_2 env p0) - (evalU2_2 env p1) 
eval2_2 env (Czero2_2) = 0
eval2_2 env (CIte2_2 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_2 env p1) (evalU2_2 env p2)
eval2_2 env (Access0_0_2_2 p0) = fst (evalU2_0 env p0)
eval2_2 env (Access1_0_2_2 p0) = snd (evalU2_0 env p0)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymInteger
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger)
eval3_0 env (Param03_0) = evalVar1 env "r"
eval3_0 env (Param43_0) = evalVar1 env "l"
eval3_0 env (Prod3_0 p0 p1) = ((evalU3_2 env p0), (evalU3_2 env p1))

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymBool
eval3_1 env (Param13_1) = evalVar2 env "is_used"
eval3_1 env (Ceq3_1 p0 p1) =  (evalU3_2 env p0) ==~ (evalU3_2 env p1) 
eval3_1 env (Cless3_1 p0 p1) =  (evalU3_2 env p0) <~ (evalU3_2 env p1) 
eval3_1 env (Cleq3_1 p0 p1) =  (evalU3_2 env p0) <=~ (evalU3_2 env p1) 
eval3_1 env (Cand3_1 p0 p1) =  (evalU3_1 env p0) &&~ (evalU3_1 env p1) 
eval3_1 env (Cor3_1 p0 p1) =  (evalU3_1 env p0) ||~ (evalU3_1 env p1) 
eval3_1 env (Cnot3_1 p0) =  mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymBool
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymInteger
eval3_2 env (Param23_2) = evalVar0 env "tmp3"
eval3_2 env (Param33_2) = evalVar0 env "w"
eval3_2 env (Cadd3_2 p0 p1) =  (evalU3_2 env p0) + (evalU3_2 env p1) 
eval3_2 env (Csub3_2 p0 p1) =  (evalU3_2 env p0) - (evalU3_2 env p1) 
eval3_2 env (Czero3_2) = 0
eval3_2 env (CIte3_2 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_2 env p1) (evalU3_2 env p2)
eval3_2 env (Access0_0_3_2 p0) = fst (evalU3_0 env p0)
eval3_2 env (Access1_0_3_2 p0) = snd (evalU3_0 env p0)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymInteger
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> (SymInteger, SymInteger)
eval4_0 env (Prod4_0 p0 p1) = ((evalU4_1 env p0), (evalU4_1 env p1))
eval4_0 env (CZero24_0) = (0,0)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> (SymInteger, SymInteger)
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymInteger
eval4_1 env (Cadd4_1 p0 p1) =  (evalU4_1 env p0) + (evalU4_1 env p1) 
eval4_1 env (Csub4_1 p0 p1) =  (evalU4_1 env p0) - (evalU4_1 env p1) 
eval4_1 env (Czero4_1) = 0
eval4_1 env (CIte4_1 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_1 env p1) (evalU4_1 env p2)
eval4_1 env (Access0_0_4_1 p0) = fst (evalU4_0 env p0)
eval4_1 env (Access1_0_4_1 p0) = snd (evalU4_0 env p0)

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

eval5_0 :: RefEnv -> Expr5_0 -> SymInteger
eval5_0 env (Cadd5_0 p0 p1) =  (evalU5_0 env p0) + (evalU5_0 env p1) 
eval5_0 env (Csub5_0 p0 p1) =  (evalU5_0 env p0) - (evalU5_0 env p1) 
eval5_0 env (Czero5_0) = 0
eval5_0 env (CIte5_0 p0 p1 p2) = mrgIte ((evalU5_2 env p0) ==~ (toSym True)) (evalU5_0 env p1) (evalU5_0 env p2)
eval5_0 env (Access0_1_5_0 p0) = fst (evalU5_1 env p0)
eval5_0 env (Access1_1_5_0 p0) = snd (evalU5_1 env p0)
eval5_0 env (Max5_0 p0 p1) = max' (evalU5_0 env p0) (evalU5_0 env p1)

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> SymInteger
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> (SymInteger, SymInteger)
eval5_1 env (Param05_1) = evalVar1 env "h"
eval5_1 env (Prod5_1 p0 p1) = ((evalU5_0 env p0), (evalU5_0 env p1))

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> (SymInteger, SymInteger)
evalU5_1 env = onUnion (eval5_1 env)

eval5_2 :: RefEnv -> Expr5_2 -> SymBool
eval5_2 env (Ceq5_2 p0 p1) =  (evalU5_0 env p0) ==~ (evalU5_0 env p1) 
eval5_2 env (Cless5_2 p0 p1) =  (evalU5_0 env p0) <~ (evalU5_0 env p1) 
eval5_2 env (Cleq5_2 p0 p1) =  (evalU5_0 env p0) <=~ (evalU5_0 env p1) 
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
r l w 

Hole grammar for #1
tmp2 r is_used w l 

Hole grammar for #2
r is_used w l 

Hole grammar for #3
r is_used tmp3 w l 

Hole grammar for #4
t 

Hole grammar for #5
h t xs 
-}

type Plan = (SymInteger, SymInteger)

data Tree
  = Leaf Unit
  | Node SymInteger Tree Tree
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data PlanList
  = Pnil Unit
  | Pcons Plan PlanList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon PlanList, ExtractSymbolics)
    via (Default PlanList)
      
instance SimpleMergeable PlanList where
  mrgIte cond l r = go cond l r
    where
      go cond (Pcons l1 r1) (Pcons l2 r2) = Pcons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Pnil l) (Pnil r) = Pnil Unit
      go cond (Pcons l1 r1) (Pnil r2) = Pcons l1 r1
      go cond (Pnil l) (Pcons l2 r2) = Pcons l2 r2
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

is_valid_tree t = 
  case t of
    Leaf _ -> (toSym False)
    Node _ w (Leaf _) -> (toSym True)
    _ -> (toSym False)
  

merge' = 
  let
    f xs ys = 
      case xs of
        Pnil _ -> ys
        Pcons h t -> Pcons h (f t ys)
      
  in
  f 

f :: Tree -> SymBool
f p = 
    case p of
    Leaf _ -> (toSym False)
    Node w _ r -> mrgIte (w >~  0)
        ((toSym True))
        (f r)

extend :: Plan -> Plan -> SymInteger -> PlanList
extend l r w = 
  let
    is_used =
      (let
        tmp1 = f
      in
      evalU0_0 (RefEnv [("w", (Env0 w)), ("r", (Env1 r)), ("l", (Env1 l))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0)))
  in
  mrgIte (is_used)
    (Pcons (let
      tmp2 =
        0
    in
    evalU1_0 (RefEnv [("r", (Env1 r)), ("l", (Env1 l)), ("tmp2", (Env0 tmp2)), ("w", (Env0 w)), ("is_used", (Env2 is_used))]) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))) (Pnil Unit))
    (Pcons (evalU2_0 (RefEnv [("r", (Env1 r)), ("l", (Env1 l)), ("is_used", (Env2 is_used)), ("w", (Env0 w))]) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))) (Pcons (let
      tmp3 =
        0
    in
    evalU3_0 (RefEnv [("r", (Env1 r)), ("l", (Env1 l)), ("is_used", (Env2 is_used)), ("tmp3", (Env0 tmp3)), ("w", (Env0 w))]) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))) (Pnil Unit)))

extend_all w = 
  let
    extend_one l =
      (let
        f xs pre = 
          case xs of
            Pnil _ -> pre
            Pcons h t -> merge' (extend l h w) (f t pre)
          
      in
      f )
  in
  let
    f xs ys = 
      case xs of
        Pnil _ -> Pnil Unit
        Pcons h t -> extend_one h ys (f t ys)
      
  in
  f 

generate = 
  let
    f t = 
      case t of
        Leaf _ -> Pcons (evalU4_0 (RefEnv []) ((genSym (4::Int) "hole4") :: (UnionM Expr4_0))) (Pnil Unit)
        Node w l r -> 
          let
            lres =
              (f l)
          in
          let
            rres =
              (f r)
          in
          extend_all w lres rres
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

get_best = 
  let
    eval =
      (let
        f t = 
          case t of
            Leaf _ -> 0
            Node w l r -> w +  ((f l) +  (f r))
          
      in
      f )
  in
  let
    f xs = 
      case xs of
        Pnil _ -> 0
        Pcons h t -> max' (evalU5_0 (RefEnv [("h", (Env1 h))]) ((genSym (4::Int) "hole5") :: (UnionM Expr5_0))) (f t)
      
  in
  f 

main' t = 
  mrgIte (is_valid_tree t)
    (get_best (generate t))
    (0)

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
                ((((Node ((1)) ((Leaf Unit)) ((Node ((0)) ((Leaf Unit)) ((Leaf Unit))))))), (0))
                , ((((Node ((2)) ((Leaf Unit)) ((Node ((4)) ((Leaf Unit)) ((Node ((4)) ((Leaf Unit)) ((Leaf Unit))))))))), (0))
                , ((((Leaf Unit))), (0))
                , ((((Leaf Unit))), (0))
                , ((((Node ((1)) ((Node ((5)) ((Node ((5)) ((Leaf Unit)) ((Node ((3)) ((Leaf Unit)) ((Leaf Unit)))))) ((Leaf Unit)))) ((Leaf Unit))))), (9))
                , ((((Node ((0)) ((Node ((3)) ((Node ((3)) ((Leaf Unit)) ((Leaf Unit)))) ((Leaf Unit)))) ((Node ((4)) ((Leaf Unit)) ((Leaf Unit))))))), (0))
                , ((((Node ((0)) ((Leaf Unit)) ((Leaf Unit))))), (0))
                , ((((Leaf Unit))), (0))
                , ((((Node ((4)) ((Node ((5)) ((Leaf Unit)) ((Node ((3)) ((Leaf Unit)) ((Node ((5)) ((Leaf Unit)) ((Leaf Unit)))))))) ((Leaf Unit))))), (13))
                , ((((Leaf Unit))), (0))
                , ((((Node ((3)) ((Leaf Unit)) ((Node ((3)) ((Leaf Unit)) ((Leaf Unit))))))), (0))
                , ((((Leaf Unit))), (0))
                , ((((Node ((2)) ((Leaf Unit)) ((Leaf Unit))))), (2))
                , ((((Node ((5)) ((Leaf Unit)) ((Leaf Unit))))), (5))
                , ((((Node ((2)) ((Leaf Unit)) ((Node ((0)) ((Leaf Unit)) ((Node ((1)) ((Leaf Unit)) ((Leaf Unit))))))))), (0))
                , ((((Node ((1)) ((Leaf Unit)) ((Node ((2)) ((Node ((0)) ((Leaf Unit)) ((Leaf Unit)))) ((Leaf Unit))))))), (0))
                , ((((Node ((5)) ((Leaf Unit)) ((Leaf Unit))))), (5))
                , ((((Node ((2)) ((Node ((5)) ((Node ((0)) ((Leaf Unit)) ((Leaf Unit)))) ((Leaf Unit)))) ((Leaf Unit))))), (5))
                , ((((Leaf Unit))), (0))
                , ((((Node ((2)) ((Leaf Unit)) ((Node ((5)) ((Leaf Unit)) ((Node ((3)) ((Leaf Unit)) ((Leaf Unit))))))))), (0))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
