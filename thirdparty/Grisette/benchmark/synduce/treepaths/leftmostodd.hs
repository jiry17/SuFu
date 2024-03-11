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
  = Env0 (SymBool, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> (SymBool, SymInteger)
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
(SymBool, SymInteger)
SymInteger
-}

-- output_type: {Bool,Int}
-- param_list: tmp1 t
data Expr0_0
  = Prod0_0 (UnionM Expr0_2) (UnionM Expr0_1)
  | CFalseZero0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Max0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Mod20_1 (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Access0_0_0_2 (UnionM Expr0_0)
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

-- output_type: {Bool,Int}
-- param_list: r tmp3 a l tmp2 t
data Expr1_0
  = Param11_0
  | Param41_0
  | Prod1_0 (UnionM Expr1_2) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param21_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Max1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Mod21_1 (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Access0_0_1_2 (UnionM Expr1_0)
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

-- output_type: {Bool,Int}
-- param_list: tmp4 z
data Expr2_0
  = Prod2_0 (UnionM Expr2_2) (UnionM Expr2_1)
  | CFalseZero2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access1_0_2_1 (UnionM Expr2_0)
  | Max2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Mod22_1 (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Access0_0_2_2 (UnionM Expr2_0)
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

-- output_type: {Bool,Int}
-- param_list: z zz w tmp6 tree tmp5
data Expr3_0
  = Param33_0
  | Param53_0
  | Prod3_0 (UnionM Expr3_2) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param23_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_2) (UnionM Expr3_1) (UnionM Expr3_1)
  | Access1_0_3_1 (UnionM Expr3_0)
  | Max3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Mod23_1 (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Access0_0_3_2 (UnionM Expr3_0)
  | Ceq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: {Bool,Int}
-- param_list: z tmp8 zz w tree tmp7
data Expr4_0
  = Param14_0
  | Param54_0
  | Prod4_0 (UnionM Expr4_2) (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param34_1
  | Cadd4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Csub4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Czero4_1
  | CIte4_1 (UnionM Expr4_2) (UnionM Expr4_1) (UnionM Expr4_1)
  | Access1_0_4_1 (UnionM Expr4_0)
  | Max4_1 (UnionM Expr4_1) (UnionM Expr4_1)
  | Mod24_1 (UnionM Expr4_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Access0_0_4_2 (UnionM Expr4_0)
  | Ceq4_2 (UnionM Expr4_1) (UnionM Expr4_1)
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
-- param_list: z tmp9
data Expr5_0
  = Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_2) (UnionM Expr5_0) (UnionM Expr5_0)
  | Access1_1_5_0 (UnionM Expr5_1)
  | Max5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Mod25_0 (UnionM Expr5_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Param15_1
  | Prod5_1 (UnionM Expr5_2) (UnionM Expr5_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

data Expr5_2
  = Access0_1_5_2 (UnionM Expr5_1)
  | Ceq5_2 (UnionM Expr5_0) (UnionM Expr5_0)
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
    genSingle0 = [mrgCFalseZero0_0]
    genSingle1 = [mrgCzero0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e2_0 e1_0])
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
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_0_1 e0_0] ++ [mrgMax0_1 e1_6 e1_7] ++ [mrgMod20_1 e1_8])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
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
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_0_2 e0_0] ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam11_0] ++ [mrgParam41_0]
    genSingle1 = [mrgParam21_1] ++ [mrgCzero1_1]
    genSingle2 = [mrgCFalse1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e2_0 e1_0])
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
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_1_1 e0_0] ++ [mrgMax1_1 e1_6 e1_7] ++ [mrgMod21_1 e1_8])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
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
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_1_2 e0_0] ++ [mrgCeq1_2 e1_0 e1_1] ++ [mrgCless1_2 e1_2 e1_3] ++ [mrgCleq1_2 e1_4 e1_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalseZero2_0]
    genSingle1 = [mrgCzero2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e2_0 e1_0])
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
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_2_1 e0_0] ++ [mrgMax2_1 e1_6 e1_7] ++ [mrgMod22_1 e1_8])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
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
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_2_2 e0_0] ++ [mrgCeq2_2 e1_0 e1_1] ++ [mrgCless2_2 e1_2 e1_3] ++ [mrgCleq2_2 e1_4 e1_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam33_0] ++ [mrgParam53_0]
    genSingle1 = [mrgParam23_1] ++ [mrgCzero3_1]
    genSingle2 = [mrgCFalse3_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e2_0 e1_0])
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
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_3_1 e0_0] ++ [mrgMax3_1 e1_6 e1_7] ++ [mrgMod23_1 e1_8])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
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
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_3_2 e0_0] ++ [mrgCeq3_2 e1_0 e1_1] ++ [mrgCless3_2 e1_2 e1_3] ++ [mrgCleq3_2 e1_4 e1_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam14_0] ++ [mrgParam54_0]
    genSingle1 = [mrgParam34_1] ++ [mrgCzero4_1]
    genSingle2 = [mrgCFalse4_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd4_0 e2_0 e1_0])
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
        e1_8 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd4_1 e1_0 e1_1] ++ [mrgCsub4_1 e1_2 e1_3] ++ [mrgCIte4_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_4_1 e0_0] ++ [mrgMax4_1 e1_6 e1_7] ++ [mrgMod24_1 e1_8])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
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
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_4_2 e0_0] ++ [mrgCeq4_2 e1_0 e1_1] ++ [mrgCless4_2 e1_2 e1_3] ++ [mrgCleq4_2 e1_4 e1_5] ++ [mrgCand4_2 e2_0 e2_1] ++ [mrgCor4_2 e2_2 e2_3] ++ [mrgCnot4_2 e2_4])
        return res

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero5_0]
    genSingle1 = [mrgParam15_1]
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
        e0_8 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd5_0 e0_0 e0_1] ++ [mrgCsub5_0 e0_2 e0_3] ++ [mrgCIte5_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_5_0 e1_0] ++ [mrgMax5_0 e0_6 e0_7] ++ [mrgMod25_0 e0_8])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd5_1 e2_0 e0_0])
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
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_1_5_2 e1_0] ++ [mrgCeq5_2 e0_0 e0_1] ++ [mrgCless5_2 e0_2 e0_3] ++ [mrgCleq5_2 e0_4 e0_5] ++ [mrgCand5_2 e2_0 e2_1] ++ [mrgCor5_2 e2_2 e2_3] ++ [mrgCnot5_2 e2_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymInteger)
eval0_0 env (Prod0_0 p0 p1) = ((evalU0_2 env p0), (evalU0_1 env p1))
eval0_0 env (CFalseZero0_0) = ((toSym False),0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access1_0_0_1 p0) = snd (evalU0_0 env p0)
eval0_1 env (Max0_1 p0 p1) = max' (evalU0_1 env p0) (evalU0_1 env p1)
eval0_1 env (Mod20_1 p0) = mod2 (evalU0_1 env p0)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Access0_0_0_2 p0) = fst (evalU0_0 env p0)
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymInteger)
eval1_0 env (Param11_0) = evalVar0 env "tmp3"
eval1_0 env (Param41_0) = evalVar0 env "tmp2"
eval1_0 env (Prod1_0 p0 p1) = ((evalU1_2 env p0), (evalU1_1 env p1))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param21_1) = evalVar1 env "a"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access1_0_1_1 p0) = snd (evalU1_0 env p0)
eval1_1 env (Max1_1 p0 p1) = max' (evalU1_1 env p0) (evalU1_1 env p1)
eval1_1 env (Mod21_1 p0) = mod2 (evalU1_1 env p0)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Access0_0_1_2 p0) = fst (evalU1_0 env p0)
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymBool, SymInteger)
eval2_0 env (Prod2_0 p0 p1) = ((evalU2_2 env p0), (evalU2_1 env p1))
eval2_0 env (CFalseZero2_0) = ((toSym False),0)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymBool, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access1_0_2_1 p0) = snd (evalU2_0 env p0)
eval2_1 env (Max2_1 p0 p1) = max' (evalU2_1 env p0) (evalU2_1 env p1)
eval2_1 env (Mod22_1 p0) = mod2 (evalU2_1 env p0)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Access0_0_2_2 p0) = fst (evalU2_0 env p0)
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> (SymBool, SymInteger)
eval3_0 env (Param33_0) = evalVar0 env "tmp6"
eval3_0 env (Param53_0) = evalVar0 env "tmp5"
eval3_0 env (Prod3_0 p0 p1) = ((evalU3_2 env p0), (evalU3_1 env p1))

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymBool, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param23_1) = evalVar1 env "w"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Access1_0_3_1 p0) = snd (evalU3_0 env p0)
eval3_1 env (Max3_1 p0 p1) = max' (evalU3_1 env p0) (evalU3_1 env p1)
eval3_1 env (Mod23_1 p0) = mod2 (evalU3_1 env p0)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Access0_0_3_2 p0) = fst (evalU3_0 env p0)
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> (SymBool, SymInteger)
eval4_0 env (Param14_0) = evalVar0 env "tmp8"
eval4_0 env (Param54_0) = evalVar0 env "tmp7"
eval4_0 env (Prod4_0 p0 p1) = ((evalU4_2 env p0), (evalU4_1 env p1))

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> (SymBool, SymInteger)
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> SymInteger
eval4_1 env (Param34_1) = evalVar1 env "w"
eval4_1 env (Cadd4_1 p0 p1) =  (evalU4_1 env p0) + (evalU4_1 env p1) 
eval4_1 env (Csub4_1 p0 p1) =  (evalU4_1 env p0) - (evalU4_1 env p1) 
eval4_1 env (Czero4_1) = 0
eval4_1 env (CIte4_1 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_1 env p1) (evalU4_1 env p2)
eval4_1 env (Access1_0_4_1 p0) = snd (evalU4_0 env p0)
eval4_1 env (Max4_1 p0 p1) = max' (evalU4_1 env p0) (evalU4_1 env p1)
eval4_1 env (Mod24_1 p0) = mod2 (evalU4_1 env p0)

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> SymInteger
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> SymBool
eval4_2 env (Access0_0_4_2 p0) = fst (evalU4_0 env p0)
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
eval5_0 env (Access1_1_5_0 p0) = snd (evalU5_1 env p0)
eval5_0 env (Max5_0 p0 p1) = max' (evalU5_0 env p0) (evalU5_0 env p1)
eval5_0 env (Mod25_0 p0) = mod2 (evalU5_0 env p0)

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> SymInteger
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> (SymBool, SymInteger)
eval5_1 env (Param15_1) = evalVar0 env "tmp9"
eval5_1 env (Prod5_1 p0 p1) = ((evalU5_2 env p0), (evalU5_0 env p1))

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> (SymBool, SymInteger)
evalU5_1 env = onUnion (eval5_1 env)

eval5_2 :: RefEnv -> Expr5_2 -> SymBool
eval5_2 env (Access0_1_5_2 p0) = fst (evalU5_1 env p0)
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
tmp1 t 

Hole grammar for #1
r tmp3 a l tmp2 t 

Hole grammar for #2
tmp4 z 

Hole grammar for #3
z zz w tmp6 tree tmp5 

Hole grammar for #4
z tmp8 zz w tree tmp7 

Hole grammar for #5
z tmp9 
-}

data BTree
  = Empty Unit
  | Node SymInteger BTree BTree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon BTree, ExtractSymbolics)
    via (Default BTree)

data Zipper
  = Top Unit
  | Left' SymInteger BTree Zipper
  | Right' SymInteger BTree Zipper
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Zipper, ExtractSymbolics)
    via (Default Zipper)
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

mod2 x = 
  x -  ((div' x 2) *  2)

spec t = 
  (snd (let
    f t = 
      case t of
        Empty _ -> ((toSym False), 1)
        Node a l r -> 
          let
            result =
              (f l)
          in
          mrgIte ((fst result))
            (result)
            (mrgIte (1 ==~  (mod2 a))
              (((toSym True), a))
              (f r))
      
  in
  f  t))

repr = 
  let
    f z = 
      case z of
        Top _ -> Empty Unit
        Left' w tree zz -> Node w tree (f zz)
        Right' w tree zz -> Node w (f zz) tree
      
  in
  f 

tree_rec = 
  let
    f t = 
      case t of
        Empty _ -> 
          let
            tmp1 =
              (Empty Unit)
          in
          evalU0_0 (RefEnv []) ((genSym (2::Int) "hole0") :: (UnionM Expr0_0))
        Node a l r -> 
          let
            tmp2 =
              (f l)
          in
          let
            tmp3 =
              (f r)
          in
          evalU1_0 (RefEnv [("tmp3", (Env0 tmp3)), ("tmp2", (Env0 tmp2)), ("a", (Env1 a))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0))
      
  in
  f 

zip_rec = 
  let
    f z = 
      case z of
        Top _ -> 
          let
            tmp4 =
              (Top Unit)
          in
          evalU2_0 (RefEnv []) ((genSym (2::Int) "hole2") :: (UnionM Expr2_0))
        Left' w tree zz -> 
          let
            tmp5 =
              (tree_rec tree)
          in
          let
            tmp6 =
              (f zz)
          in
          evalU3_0 (RefEnv [("tmp6", (Env0 tmp6)), ("tmp5", (Env0 tmp5)), ("w", (Env1 w))]) ((genSym (5::Int) "hole3") :: (UnionM Expr3_0))
        Right' w tree zz -> 
          let
            tmp7 =
              (tree_rec tree)
          in
          let
            tmp8 =
              (f zz)
          in
          evalU4_0 (RefEnv [("tmp8", (Env0 tmp8)), ("tmp7", (Env0 tmp7)), ("w", (Env1 w))]) ((genSym (5::Int) "hole4") :: (UnionM Expr4_0))
      
  in
  f 

main' z = 
  let
    tmp9 =
      (zip_rec z)
  in
  evalU5_0 (RefEnv [("tmp9", (Env0 tmp9))]) ((genSym (2::Int) "hole5") :: (UnionM Expr5_0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(Zipper, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(Zipper, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((Left' ((-3)) ((Empty Unit)) ((Right' ((-4)) ((Empty Unit)) ((Top Unit)))))), (1))
                , (((Left' ((-1)) ((Node ((-1)) ((Empty Unit)) ((Empty Unit)))) ((Right' ((2)) ((Empty Unit)) ((Top Unit)))))), (1))
                , (((Top Unit)), (1))
                , (((Top Unit)), (1))
                , (((Right' ((-2)) ((Node ((5)) ((Node ((5)) ((Empty Unit)) ((Node ((1)) ((Empty Unit)) ((Empty Unit)))))) ((Empty Unit)))) ((Top Unit)))), (5))
                , (((Right' ((-4)) ((Empty Unit)) ((Right' ((-4)) ((Empty Unit)) ((Right' ((-2)) ((Node ((-2)) ((Empty Unit)) ((Empty Unit)))) ((Top Unit)))))))), (1))
                , (((Left' ((-5)) ((Empty Unit)) ((Top Unit)))), (1))
                , (((Top Unit)), (1))
                , (((Right' ((3)) ((Node ((5)) ((Empty Unit)) ((Node ((0)) ((Empty Unit)) ((Node ((4)) ((Empty Unit)) ((Empty Unit)))))))) ((Top Unit)))), (3))
                , (((Top Unit)), (1))
                , (((Left' ((1)) ((Empty Unit)) ((Right' ((1)) ((Empty Unit)) ((Top Unit)))))), (1))
                , (((Top Unit)), (1))
                , (((Left' ((0)) ((Empty Unit)) ((Top Unit)))), (1))
                , (((Right' ((4)) ((Empty Unit)) ((Top Unit)))), (1))
                , (((Left' ((-1)) ((Node ((-1)) ((Empty Unit)) ((Empty Unit)))) ((Left' ((-3)) ((Empty Unit)) ((Top Unit)))))), (1))
                , (((Left' ((-4)) ((Node ((5)) ((Empty Unit)) ((Empty Unit)))) ((Left' ((3)) ((Empty Unit)) ((Top Unit)))))), (5))
                , (((Left' ((5)) ((Empty Unit)) ((Top Unit)))), (5))
                , (((Right' ((-1)) ((Node ((5)) ((Node ((-4)) ((Empty Unit)) ((Empty Unit)))) ((Empty Unit)))) ((Top Unit)))), (5))
                , (((Top Unit)), (1))
                , (((Left' ((-1)) ((Empty Unit)) ((Left' ((5)) ((Node ((-1)) ((Empty Unit)) ((Empty Unit)))) ((Top Unit)))))), (5))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
