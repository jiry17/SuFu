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
-- param_list: a tmp1 x
data Expr0_0
  = Param00_0
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
-- param_list: a z l tmp2 y x r
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
-- param_list: a tmp3 tmp5 l y r x tmp4
data Expr2_0
  = Param02_0
  | Param22_0
  | Param72_0
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
-- param_list: a tmp7 l r x tmp6
data Expr3_0
  = Param03_0
  | Param13_0
  | Param53_0
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
-- param_list: tmp8 x
data Expr4_0
  = Param04_0
  | Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_1) (UnionM Expr4_0) (UnionM Expr4_0)
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

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam00_0] ++ [mrgCzero0_0]
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
    genSingle0 = [mrgParam02_0] ++ [mrgParam22_0] ++ [mrgParam72_0] ++ [mrgCzero2_0]
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
    genSingle0 = [mrgParam03_0] ++ [mrgParam13_0] ++ [mrgParam53_0] ++ [mrgCzero3_0]
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
    genSingle0 = [mrgParam04_0] ++ [mrgCzero4_0]
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

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param00_0) = evalVar0 env "a"
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
eval1_0 env (Param01_0) = evalVar0 env "a"
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
eval2_0 env (Param02_0) = evalVar0 env "a"
eval2_0 env (Param22_0) = evalVar0 env "tmp5"
eval2_0 env (Param72_0) = evalVar0 env "tmp4"
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
eval3_0 env (Param03_0) = evalVar0 env "a"
eval3_0 env (Param13_0) = evalVar0 env "tmp7"
eval3_0 env (Param53_0) = evalVar0 env "tmp6"
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
eval4_0 env (Param04_0) = evalVar0 env "tmp8"
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_1 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
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


------program space end----

------spec begin-------
{-
Hole grammar for #0
a tmp1 x 

Hole grammar for #1
a z l tmp2 y x r 

Hole grammar for #2
a tmp3 tmp5 l y r x tmp4 

Hole grammar for #3
a tmp7 l r x tmp6 

Hole grammar for #4
tmp8 x 
-}

data Tree
  = Telt SymInteger
  | Tnode SymInteger Tree Tree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data PTree
  = Pelt SymInteger
  | Pnode SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon PTree, ExtractSymbolics)
    via (Default PTree)

data List
  = Elt PTree
  | Cons PTree List
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

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

repr = 
  let
    f x = 
      case x of
        Telt a -> 
          let
            tmp1 =
              (Pelt a)
          in
          evalU0_0 (RefEnv [("a", (Env0 a))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
        Tnode a l r -> 
          let
            repr_list =
              (let
                g y = 
                  case y of
                    Telt a -> 
                      let
                        z =
                          (Pelt a)
                      in
                      let
                        tmp2 =
                          (Elt z)
                      in
                      evalU1_0 (RefEnv [("a", (Env0 a))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))
                    Tnode a l r -> 
                      let
                        tmp3 =
                          (Pelt a)
                      in
                      let
                        tmp4 =
                          (f l)
                      in
                      let
                        tmp5 =
                          (g r)
                      in
                      evalU2_0 (RefEnv [("a", (Env0 a)), ("tmp5", (Env0 tmp5)), ("tmp4", (Env0 tmp4))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))
                  
              in
              g )
          in
          let
            tmp6 =
              (f l)
          in
          let
            tmp7 =
              (repr_list r)
          in
          evalU3_0 (RefEnv [("a", (Env0 a)), ("tmp7", (Env0 tmp7)), ("tmp6", (Env0 tmp6))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0))
      
  in
  f 

spec = 
  let
    f x = 
      case x of
        Pelt a -> a
        Pnode a l -> 
          let
            maxh_aux =
              (let
                g y = 
                  case y of
                    Elt a -> f a
                    Cons hd tl -> f hd
                  
              in
              g )
          in
          max' a (maxh_aux l)
      
  in
  f 

main' x = 
  let
    tmp8 =
      (repr x)
  in
  evalU4_0 (RefEnv [("tmp8", (Env0 tmp8))]) ((genSym (1::Int) "hole4") :: (UnionM Expr4_0))

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
                (((Telt (-4))), (-4))
                , (((Tnode ((-2)) ((Tnode ((5)) ((Telt (3))) ((Telt (5))))) ((Tnode ((-5)) ((Telt (1))) ((Tnode ((-5)) ((Telt (3))) ((Telt (5))))))))), (5))
                , (((Tnode ((3)) ((Tnode ((5)) ((Telt (-5))) ((Tnode ((4)) ((Tnode ((4)) ((Telt (-4))) ((Telt (1))))) ((Telt (-2))))))) ((Telt (-1))))), (5))
                , (((Tnode ((1)) ((Telt (3))) ((Tnode ((4)) ((Telt (-1))) ((Telt (-2))))))), (3))
                , (((Tnode ((-4)) ((Tnode ((0)) ((Telt (-3))) ((Telt (5))))) ((Tnode ((2)) ((Telt (4))) ((Tnode ((-5)) ((Telt (-3))) ((Telt (-5))))))))), (0))
                , (((Tnode ((-4)) ((Telt (-3))) ((Tnode ((0)) ((Telt (2))) ((Telt (-3))))))), (-3))
                , (((Telt (-3))), (-3))
                , (((Tnode ((5)) ((Telt (2))) ((Telt (-1))))), (5))
                , (((Tnode ((-2)) ((Tnode ((0)) ((Telt (-4))) ((Telt (1))))) ((Tnode ((-1)) ((Telt (-3))) ((Tnode ((-5)) ((Telt (-1))) ((Telt (5))))))))), (0))
                , (((Tnode ((1)) ((Tnode ((1)) ((Telt (-2))) ((Telt (-4))))) ((Telt (1))))), (1))
                , (((Tnode ((-2)) ((Tnode ((-4)) ((Tnode ((-3)) ((Telt (-3))) ((Telt (-3))))) ((Telt (1))))) ((Tnode ((3)) ((Telt (3))) ((Telt (-4))))))), (-2))
                , (((Tnode ((-4)) ((Telt (0))) ((Telt (5))))), (0))
                , (((Telt (3))), (3))
                , (((Tnode ((3)) ((Tnode ((-5)) ((Telt (-4))) ((Telt (-2))))) ((Tnode ((-3)) ((Telt (-1))) ((Telt (-2))))))), (3))
                , (((Telt (0))), (0))
                , (((Telt (-4))), (-4))
                , (((Tnode ((2)) ((Telt (-1))) ((Tnode ((-4)) ((Telt (1))) ((Tnode ((-1)) ((Telt (3))) ((Tnode ((1)) ((Telt (4))) ((Telt (-2))))))))))), (2))
                , (((Tnode ((-1)) ((Telt (-3))) ((Telt (-2))))), (-1))
                , (((Tnode ((-2)) ((Telt (-4))) ((Telt (2))))), (-2))
                , (((Tnode ((-5)) ((Telt (4))) ((Tnode ((0)) ((Tnode ((2)) ((Telt (-4))) ((Telt (-3))))) ((Tnode ((2)) ((Telt (-1))) ((Telt (2))))))))), (4))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
