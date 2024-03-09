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
  = Env0 (SymInteger, SymInteger)
  | Env1 (SymBool, SymInteger, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> (SymInteger, SymInteger)
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> (SymBool, SymInteger, SymInteger)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
(SymInteger, SymInteger)
(SymBool, SymInteger, SymInteger)
-}

-- output_type: {Bool,Int,Int}
-- param_list: info x xs
data Expr0_0
  = Prod0_0 (UnionM Expr0_3) (UnionM Expr0_2) (UnionM Expr0_2)
  | CFalseZero20_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param00_1
  | Prod0_1 (UnionM Expr0_2) (UnionM Expr0_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Cadd0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Csub0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Czero0_2
  | CIte0_2 (UnionM Expr0_3) (UnionM Expr0_2) (UnionM Expr0_2)
  | Access0_1_0_2 (UnionM Expr0_1)
  | Access1_1_0_2 (UnionM Expr0_1)
  | Access1_0_0_2 (UnionM Expr0_0)
  | Access2_0_0_2 (UnionM Expr0_0)
  | Min0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Max0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

data Expr0_3
  = Access0_0_0_3 (UnionM Expr0_0)
  | Ceq0_3 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cless0_3 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cleq0_3 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cand0_3 (UnionM Expr0_3) (UnionM Expr0_3)
  | Cor0_3 (UnionM Expr0_3) (UnionM Expr0_3)
  | Cnot0_3 (UnionM Expr0_3)
  | CFalse0_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_3)
    via (Default Expr0_3)

-- output_type: {Bool,Int,Int}
-- param_list: info xs t tmp1 h
data Expr1_0
  = Param31_0
  | Prod1_0 (UnionM Expr1_3) (UnionM Expr1_2) (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Prod1_1 (UnionM Expr1_2) (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Cadd1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Csub1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Czero1_2
  | CIte1_2 (UnionM Expr1_3) (UnionM Expr1_2) (UnionM Expr1_2)
  | Access0_1_1_2 (UnionM Expr1_1)
  | Access1_1_1_2 (UnionM Expr1_1)
  | Access1_0_1_2 (UnionM Expr1_0)
  | Access2_0_1_2 (UnionM Expr1_0)
  | Min1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Max1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

data Expr1_3
  = Access0_0_1_3 (UnionM Expr1_0)
  | Ceq1_3 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cless1_3 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cleq1_3 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cand1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cor1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cnot1_3 (UnionM Expr1_3)
  | CFalse1_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_3)
    via (Default Expr1_3)

-- output_type: Bool
-- param_list: xs tmp2
data Expr2_0
  = Access0_1_2_0 (UnionM Expr2_1)
  | Ceq2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cless2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cleq2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cand2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cor2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cnot2_0 (UnionM Expr2_0)
  | CFalse2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param12_1
  | Prod2_1 (UnionM Expr2_0) (UnionM Expr2_2) (UnionM Expr2_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Cadd2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Csub2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Czero2_2
  | CIte2_2 (UnionM Expr2_0) (UnionM Expr2_2) (UnionM Expr2_2)
  | Access1_1_2_2 (UnionM Expr2_1)
  | Access2_1_2_2 (UnionM Expr2_1)
  | Min2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Max2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

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

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalseZero20_0]
    genSingle1 = [mrgParam00_1]
    genSingle2 = [mrgCzero0_2]
    genSingle3 = [mrgCFalse0_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e3_0 e2_0 e2_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd0_1 e2_0 e2_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e2_6 <- (gen2 (gendepth - 1))
        e2_7 <- (gen2 (gendepth - 1))
        e2_8 <- (gen2 (gendepth - 1))
        e2_9 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd0_2 e2_0 e2_1] ++ [mrgCsub0_2 e2_2 e2_3] ++ [mrgCIte0_2 e3_0 e2_4 e2_5] ++ [mrgAccess0_1_0_2 e1_0] ++ [mrgAccess1_1_0_2 e1_1] ++ [mrgAccess1_0_0_2 e0_0] ++ [mrgAccess2_0_0_2 e0_1] ++ [mrgMin0_2 e2_6 e2_7] ++ [mrgMax0_2 e2_8 e2_9])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgAccess0_0_0_3 e0_0] ++ [mrgCeq0_3 e2_0 e2_1] ++ [mrgCless0_3 e2_2 e2_3] ++ [mrgCleq0_3 e2_4 e2_5] ++ [mrgCand0_3 e3_0 e3_1] ++ [mrgCor0_3 e3_2 e3_3] ++ [mrgCnot0_3 e3_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam31_0]
    genSingle1 = [mrgParam01_1]
    genSingle2 = [mrgCzero1_2]
    genSingle3 = [mrgCFalse1_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e3_0 e2_0 e2_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd1_1 e2_0 e2_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e2_6 <- (gen2 (gendepth - 1))
        e2_7 <- (gen2 (gendepth - 1))
        e2_8 <- (gen2 (gendepth - 1))
        e2_9 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd1_2 e2_0 e2_1] ++ [mrgCsub1_2 e2_2 e2_3] ++ [mrgCIte1_2 e3_0 e2_4 e2_5] ++ [mrgAccess0_1_1_2 e1_0] ++ [mrgAccess1_1_1_2 e1_1] ++ [mrgAccess1_0_1_2 e0_0] ++ [mrgAccess2_0_1_2 e0_1] ++ [mrgMin1_2 e2_6 e2_7] ++ [mrgMax1_2 e2_8 e2_9])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgAccess0_0_1_3 e0_0] ++ [mrgCeq1_3 e2_0 e2_1] ++ [mrgCless1_3 e2_2 e2_3] ++ [mrgCleq1_3 e2_4 e2_5] ++ [mrgCand1_3 e3_0 e3_1] ++ [mrgCor1_3 e3_2 e3_3] ++ [mrgCnot1_3 e3_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse2_0]
    genSingle1 = [mrgParam12_1]
    genSingle2 = [mrgCzero2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_1_2_0 e1_0] ++ [mrgCeq2_0 e2_0 e2_1] ++ [mrgCless2_0 e2_2 e2_3] ++ [mrgCleq2_0 e2_4 e2_5] ++ [mrgCand2_0 e0_0 e0_1] ++ [mrgCor2_0 e0_2 e0_3] ++ [mrgCnot2_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd2_1 e0_0 e2_0 e2_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e2_6 <- (gen2 (gendepth - 1))
        e2_7 <- (gen2 (gendepth - 1))
        e2_8 <- (gen2 (gendepth - 1))
        e2_9 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd2_2 e2_0 e2_1] ++ [mrgCsub2_2 e2_2 e2_3] ++ [mrgCIte2_2 e0_0 e2_4 e2_5] ++ [mrgAccess1_1_2_2 e1_0] ++ [mrgAccess2_1_2_2 e1_1] ++ [mrgMin2_2 e2_6 e2_7] ++ [mrgMax2_2 e2_8 e2_9])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymInteger, SymInteger)
eval0_0 env (Prod0_0 p0 p1 p2) = ((evalU0_3 env p0), (evalU0_2 env p1), (evalU0_2 env p2))
eval0_0 env (CFalseZero20_0) = ((toSym False),0,0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> (SymInteger, SymInteger)
eval0_1 env (Param00_1) = evalVar0 env "info"
eval0_1 env (Prod0_1 p0 p1) = ((evalU0_2 env p0), (evalU0_2 env p1))

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> (SymInteger, SymInteger)
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymInteger
eval0_2 env (Cadd0_2 p0 p1) =  (evalU0_2 env p0) + (evalU0_2 env p1) 
eval0_2 env (Csub0_2 p0 p1) =  (evalU0_2 env p0) - (evalU0_2 env p1) 
eval0_2 env (Czero0_2) = 0
eval0_2 env (CIte0_2 p0 p1 p2) = mrgIte ((evalU0_3 env p0) ==~ (toSym True)) (evalU0_2 env p1) (evalU0_2 env p2)
eval0_2 env (Access0_1_0_2 p0) = fst (evalU0_1 env p0)
eval0_2 env (Access1_1_0_2 p0) = snd (evalU0_1 env p0)
eval0_2 env (Access1_0_0_2 p0) = get2from3 (evalU0_0 env p0)
eval0_2 env (Access2_0_0_2 p0) = get3from3 (evalU0_0 env p0)
eval0_2 env (Min0_2 p0 p1) = min' (evalU0_2 env p0) (evalU0_2 env p1)
eval0_2 env (Max0_2 p0 p1) = max' (evalU0_2 env p0) (evalU0_2 env p1)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymInteger
evalU0_2 env = onUnion (eval0_2 env)

eval0_3 :: RefEnv -> Expr0_3 -> SymBool
eval0_3 env (Access0_0_0_3 p0) = get1from3 (evalU0_0 env p0)
eval0_3 env (Ceq0_3 p0 p1) =  (evalU0_2 env p0) ==~ (evalU0_2 env p1) 
eval0_3 env (Cless0_3 p0 p1) =  (evalU0_2 env p0) <~ (evalU0_2 env p1) 
eval0_3 env (Cleq0_3 p0 p1) =  (evalU0_2 env p0) <=~ (evalU0_2 env p1) 
eval0_3 env (Cand0_3 p0 p1) =  (evalU0_3 env p0) &&~ (evalU0_3 env p1) 
eval0_3 env (Cor0_3 p0 p1) =  (evalU0_3 env p0) ||~ (evalU0_3 env p1) 
eval0_3 env (Cnot0_3 p0) =  mrgIte ((evalU0_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_3 env (CFalse0_3) = (toSym False)

evalU0_3 :: RefEnv -> UnionM Expr0_3 -> SymBool
evalU0_3 env = onUnion (eval0_3 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymInteger, SymInteger)
eval1_0 env (Param31_0) = evalVar1 env "tmp1"
eval1_0 env (Prod1_0 p0 p1 p2) = ((evalU1_3 env p0), (evalU1_2 env p1), (evalU1_2 env p2))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> (SymInteger, SymInteger)
eval1_1 env (Param01_1) = evalVar0 env "info"
eval1_1 env (Prod1_1 p0 p1) = ((evalU1_2 env p0), (evalU1_2 env p1))

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> (SymInteger, SymInteger)
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymInteger
eval1_2 env (Cadd1_2 p0 p1) =  (evalU1_2 env p0) + (evalU1_2 env p1) 
eval1_2 env (Csub1_2 p0 p1) =  (evalU1_2 env p0) - (evalU1_2 env p1) 
eval1_2 env (Czero1_2) = 0
eval1_2 env (CIte1_2 p0 p1 p2) = mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (evalU1_2 env p1) (evalU1_2 env p2)
eval1_2 env (Access0_1_1_2 p0) = fst (evalU1_1 env p0)
eval1_2 env (Access1_1_1_2 p0) = snd (evalU1_1 env p0)
eval1_2 env (Access1_0_1_2 p0) = get2from3 (evalU1_0 env p0)
eval1_2 env (Access2_0_1_2 p0) = get3from3 (evalU1_0 env p0)
eval1_2 env (Min1_2 p0 p1) = min' (evalU1_2 env p0) (evalU1_2 env p1)
eval1_2 env (Max1_2 p0 p1) = max' (evalU1_2 env p0) (evalU1_2 env p1)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymInteger
evalU1_2 env = onUnion (eval1_2 env)

eval1_3 :: RefEnv -> Expr1_3 -> SymBool
eval1_3 env (Access0_0_1_3 p0) = get1from3 (evalU1_0 env p0)
eval1_3 env (Ceq1_3 p0 p1) =  (evalU1_2 env p0) ==~ (evalU1_2 env p1) 
eval1_3 env (Cless1_3 p0 p1) =  (evalU1_2 env p0) <~ (evalU1_2 env p1) 
eval1_3 env (Cleq1_3 p0 p1) =  (evalU1_2 env p0) <=~ (evalU1_2 env p1) 
eval1_3 env (Cand1_3 p0 p1) =  (evalU1_3 env p0) &&~ (evalU1_3 env p1) 
eval1_3 env (Cor1_3 p0 p1) =  (evalU1_3 env p0) ||~ (evalU1_3 env p1) 
eval1_3 env (Cnot1_3 p0) =  mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_3 env (CFalse1_3) = (toSym False)

evalU1_3 :: RefEnv -> UnionM Expr1_3 -> SymBool
evalU1_3 env = onUnion (eval1_3 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Access0_1_2_0 p0) = get1from3 (evalU2_1 env p0)
eval2_0 env (Ceq2_0 p0 p1) =  (evalU2_2 env p0) ==~ (evalU2_2 env p1) 
eval2_0 env (Cless2_0 p0 p1) =  (evalU2_2 env p0) <~ (evalU2_2 env p1) 
eval2_0 env (Cleq2_0 p0 p1) =  (evalU2_2 env p0) <=~ (evalU2_2 env p1) 
eval2_0 env (Cand2_0 p0 p1) =  (evalU2_0 env p0) &&~ (evalU2_0 env p1) 
eval2_0 env (Cor2_0 p0 p1) =  (evalU2_0 env p0) ||~ (evalU2_0 env p1) 
eval2_0 env (Cnot2_0 p0) =  mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_0 env (CFalse2_0) = (toSym False)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymBool
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (SymBool, SymInteger, SymInteger)
eval2_1 env (Param12_1) = evalVar1 env "tmp2"
eval2_1 env (Prod2_1 p0 p1 p2) = ((evalU2_0 env p0), (evalU2_2 env p1), (evalU2_2 env p2))

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> (SymBool, SymInteger, SymInteger)
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymInteger
eval2_2 env (Cadd2_2 p0 p1) =  (evalU2_2 env p0) + (evalU2_2 env p1) 
eval2_2 env (Csub2_2 p0 p1) =  (evalU2_2 env p0) - (evalU2_2 env p1) 
eval2_2 env (Czero2_2) = 0
eval2_2 env (CIte2_2 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_2 env p1) (evalU2_2 env p2)
eval2_2 env (Access1_1_2_2 p0) = get2from3 (evalU2_1 env p0)
eval2_2 env (Access2_1_2_2 p0) = get3from3 (evalU2_1 env p0)
eval2_2 env (Min2_2 p0 p1) = min' (evalU2_2 env p0) (evalU2_2 env p1)
eval2_2 env (Max2_2 p0 p1) = max' (evalU2_2 env p0) (evalU2_2 env p1)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymInteger
evalU2_2 env = onUnion (eval2_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
info x xs 

Hole grammar for #1
info xs t tmp1 h 

Hole grammar for #2
xs tmp2 
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

min' a b = 
  mrgIte (a >~  b)
    (b)
    (a)

interval = 
  let
    f xs = 
      case xs of
        Elt a -> (a, a)
        Cons hd tl -> 
          let
            result =
              (f tl)
          in
          ((min' hd (fst result)), (max' hd (snd result)))
      
  in
  f 

spec xs = 
  (get3from3 (let
    f xs = 
      case xs of
        Line a -> 
          let
            result =
              (interval a)
          in
          ((fst result), (snd result), (toSym True))
        Ncons hd tl -> 
          let
            r1 =
              (f tl)
          in
          let
            r2 =
              (interval hd)
          in
          ((min' (get1from3 r1) (fst r2)), (max' (get2from3 r1) (snd r2)), ((get3from3 r1) &&~  (((get1from3 r1) <=~  (fst r2)) &&~  ((get2from3 r1) <=~  (snd r2)))))
      
  in
  f  xs))

target = 
  let
    f xs = 
      case xs of
        Line x -> 
          let
            info =
              (interval x)
          in
          evalU0_0 (RefEnv [("info", (Env0 info))]) ((genSym (3::Int) "hole0") :: (UnionM Expr0_0))
        Ncons h t -> 
          let
            info =
              (interval h)
          in
          let
            tmp1 =
              (f t)
          in
          evalU1_0 (RefEnv [("tmp1", (Env1 tmp1)), ("info", (Env0 info))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0))
      
  in
  f 

main' xs = 
  let
    tmp2 =
      (target xs)
  in
  evalU2_0 (RefEnv [("tmp2", (Env1 tmp2))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(NList, Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(NList, Bool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                (((Ncons ((Cons ((4)) ((Elt (3))))) ((Line (Cons ((3)) ((Elt (5)))))))), (toSym False))
                , (((Line (Cons ((0)) ((Cons ((-2)) ((Cons ((1)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((5)) ((Elt (5)))))))))))))))), (toSym True))
                , (((Line (Cons ((-5)) ((Cons ((3)) ((Elt (5)))))))), (toSym True))
                , (((Ncons ((Cons ((5)) ((Cons ((-4)) ((Elt (4))))))) ((Line (Cons ((-2)) ((Cons ((5)) ((Cons ((3)) ((Elt (5)))))))))))), (toSym False))
                , (((Ncons ((Cons ((-5)) ((Cons ((-4)) ((Cons ((-2)) ((Cons ((3)) ((Cons ((4)) ((Elt (5))))))))))))) ((Line (Elt (5)))))), (toSym False))
                , (((Ncons ((Cons ((3)) ((Elt (-2))))) ((Line (Elt (-2)))))), (toSym True))
                , (((Line (Cons ((-3)) ((Cons ((-3)) ((Elt (1)))))))), (toSym True))
                , (((Line (Elt (2)))), (toSym True))
                , (((Line (Cons ((0)) ((Cons ((-3)) ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((-5)) ((Elt (-3)))))))))))))))), (toSym True))
                , (((Line (Elt (-5)))), (toSym True))
                , (((Line (Cons ((4)) ((Elt (5)))))), (toSym True))
                , (((Line (Elt (-4)))), (toSym True))
                , (((Ncons ((Cons ((-2)) ((Elt (-5))))) ((Ncons ((Elt (2))) ((Line (Elt (2)))))))), (toSym False))
                , (((Ncons ((Cons ((4)) ((Cons ((-2)) ((Cons ((-2)) ((Elt (2))))))))) ((Line (Cons ((1)) ((Cons ((-1)) ((Cons ((-3)) ((Elt (-5)))))))))))), (toSym True))
                , (((Ncons ((Elt (-1))) ((Line (Elt (5)))))), (toSym False))
                , (((Line (Cons ((2)) ((Cons ((3)) ((Elt (4)))))))), (toSym True))
                , (((Ncons ((Elt (-4))) ((Ncons ((Cons ((-5)) ((Elt (3))))) ((Line (Elt (1)))))))), (toSym False))
                , (((Ncons ((Cons ((-4)) ((Cons ((-1)) ((Cons ((-1)) ((Elt (-2))))))))) ((Line (Cons ((-4)) ((Elt (0)))))))), (toSym False))
                , (((Ncons ((Cons ((-1)) ((Cons ((-4)) ((Elt (1))))))) ((Ncons ((Cons ((2)) ((Elt (-1))))) ((Line (Cons ((-5)) ((Elt (-4)))))))))), (toSym False))
                , (((Line (Cons ((-1)) ((Elt (-2)))))), (toSym True))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
