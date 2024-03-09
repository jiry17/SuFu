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
-- param_list: tmp1 t x global0
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
  | Param30_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_0) (UnionM Expr0_1) (UnionM Expr0_1)
  | Min0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Max0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

-- output_type: Bool
-- param_list: t r s a tmp2 global0 l
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
  = Param21_1
  | Param31_1
  | Param51_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_0) (UnionM Expr1_1) (UnionM Expr1_1)
  | Min1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Max1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

-- output_type: Bool
-- param_list: tmp3 a t tmp4 s r global0 l
data Expr2_0
  = Param02_0
  | Param32_0
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
  | Param42_1
  | Param62_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_0) (UnionM Expr2_1) (UnionM Expr2_1)
  | Min2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Max2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

-- output_type: Bool
-- param_list: t inp tmp6 global0
data Expr3_0
  = Param23_0
  | Ceq3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_0 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cor3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cnot3_0 (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param33_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_0) (UnionM Expr3_1) (UnionM Expr3_1)
  | Min3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Max3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse0_0]
    genSingle1 = [mrgParam20_1] ++ [mrgParam30_1] ++ [mrgCzero0_1]
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
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e0_0 e1_4 e1_5] ++ [mrgMin0_1 e1_6 e1_7] ++ [mrgMax0_1 e1_8 e1_9])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse1_0]
    genSingle1 = [mrgParam21_1] ++ [mrgParam31_1] ++ [mrgParam51_1] ++ [mrgCzero1_1]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e0_0 e1_4 e1_5] ++ [mrgMin1_1 e1_6 e1_7] ++ [mrgMax1_1 e1_8 e1_9])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam02_0] ++ [mrgParam32_0]
    genSingle1 = [mrgParam12_1] ++ [mrgParam42_1] ++ [mrgParam62_1] ++ [mrgCzero2_1]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e0_0 e1_4 e1_5] ++ [mrgMin2_1 e1_6 e1_7] ++ [mrgMax2_1 e1_8 e1_9])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam23_0]
    genSingle1 = [mrgParam33_1] ++ [mrgCzero3_1]
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
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e1_8 <- (gen1 (gendepth - 1))
        e1_9 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e0_0 e1_4 e1_5] ++ [mrgMin3_1 e1_6 e1_7] ++ [mrgMax3_1 e1_8 e1_9])
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
eval0_1 env (Param20_1) = evalVar0 env "x"
eval0_1 env (Param30_1) = evalVar0 env "global0"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_0 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Min0_1 p0 p1) = min' (evalU0_1 env p0) (evalU0_1 env p1)
eval0_1 env (Max0_1 p0 p1) = max' (evalU0_1 env p0) (evalU0_1 env p1)

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
eval1_1 env (Param21_1) = evalVar0 env "s"
eval1_1 env (Param31_1) = evalVar0 env "a"
eval1_1 env (Param51_1) = evalVar0 env "global0"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_0 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Min1_1 p0 p1) = min' (evalU1_1 env p0) (evalU1_1 env p1)
eval1_1 env (Max1_1 p0 p1) = max' (evalU1_1 env p0) (evalU1_1 env p1)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Param02_0) = evalVar1 env "tmp3"
eval2_0 env (Param32_0) = evalVar1 env "tmp4"
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
eval2_1 env (Param42_1) = evalVar0 env "s"
eval2_1 env (Param62_1) = evalVar0 env "global0"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Min2_1 p0 p1) = min' (evalU2_1 env p0) (evalU2_1 env p1)
eval2_1 env (Max2_1 p0 p1) = max' (evalU2_1 env p0) (evalU2_1 env p1)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymBool
eval3_0 env (Param23_0) = evalVar1 env "tmp6"
eval3_0 env (Ceq3_0 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_0 env (Cless3_0 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_0 env (Cleq3_0 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_0 env (Cand3_0 p0 p1) =  (evalU3_0 env p0) &&~ (evalU3_0 env p1) 
eval3_0 env (Cor3_0 p0 p1) =  (evalU3_0 env p0) ||~ (evalU3_0 env p1) 
eval3_0 env (Cnot3_0 p0) =  mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymBool
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param33_1) = evalVar0 env "global0"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Min3_1 p0 p1) = min' (evalU3_1 env p0) (evalU3_1 env p1)
eval3_1 env (Max3_1 p0 p1) = max' (evalU3_1 env p0) (evalU3_1 env p1)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
tmp1 t x global0 

Hole grammar for #1
t r s a tmp2 global0 l 

Hole grammar for #2
tmp3 a t tmp4 s r global0 l 

Hole grammar for #3
t inp tmp6 global0 
-}

data Tree
  = Leaf SymInteger
  | Node SymInteger Tree Tree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data MTree
  = Mleaf SymInteger
  | Mnode SymInteger SymInteger MTree MTree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon MTree, ExtractSymbolics)
    via (Default MTree)
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

min' a b = 
  mrgIte (a <~  b)
    (a)
    (b)

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

tmin = 
  let
    f t = 
      case t of
        Mleaf w -> w
        Mnode w _ l r -> min' w (min' (f l) (f r))
      
  in
  f 

tmax = 
  let
    f t = 
      case t of
        Mleaf w -> w
        Mnode w _ l r -> max' w (max' (f l) (f r))
      
  in
  f 

tsum = 
  let
    f t = 
      case t of
        Mleaf w -> w
        Mnode w _ l r -> w +  ((f l) +  (f r))
      
  in
  f 

is_bst = 
  let
    f t = 
      case t of
        Mleaf w -> w >~  0
        Mnode w s l r -> ((w >~  0) &&~  (s ==~  ((tsum l) +  (tsum r)))) &&~  (((w >=~  (tmax l)) &&~  (w <=~  (tmin r))) &&~  ((f l) &&~  (f r)))
      
  in
  f 

spec global0 t = 
  (fst (let
    f t = 
      case t of
        Leaf x -> ((x <=~  global0), x)
        Node a l r -> 
          let
            lres =
              (f l)
          in
          let
            rres =
              (f r)
          in
          let
            sum =
              (a +  ((snd lres) +  (snd rres)))
          in
          (((sum <=~  global0) &&~  ((fst lres) &&~  (fst rres))), sum)
      
  in
  f  t))

drop_tag = 
  let
    f t = 
      case t of
        Mleaf x -> Leaf x
        Mnode a s l r -> Node a (f l) (f r)
      
  in
  f 

add_tag = 
  let
    f t = 
      case t of
        Leaf x -> Mleaf x
        Node a l r -> 
          let
            lres =
              (f l)
          in
          let
            rres =
              (f r)
          in
          let
            s =
              ((tsum lres) +  (tsum rres))
          in
          Mnode a s lres rres
      
  in
  f 

target global0 = 
  let
    f t = 
      case t of
        Mleaf x -> 
          let
            tmp1 =
              (Mleaf x)
          in
          evalU0_0 (RefEnv [("x", (Env0 x)), ("global0", (Env0 global0))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
        Mnode a s l r -> mrgIte (a >~  global0)
            (let
              tmp2 =
                (Mnode a s l r)
            in
            evalU1_0 (RefEnv [("s", (Env0 s)), ("a", (Env0 a)), ("global0", (Env0 global0))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0)))
            (let
              tmp3 =
                (f l)
            in
            let
              tmp4 =
                (f r)
            in
            evalU2_0 (RefEnv [("tmp3", (Env1 tmp3)), ("tmp4", (Env1 tmp4)), ("a", (Env0 a)), ("s", (Env0 s)), ("global0", (Env0 global0))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0)))
      
  in
  f 

main' global0 t = 
  let
    inp =
      (add_tag t)
  in
  mrgIte (is_bst inp)
    (let
      tmp5 =
        (spec global0)
    in
    let
      tmp6 =
        (target global0 inp)
    in
    evalU3_0 (RefEnv [("tmp6", (Env1 tmp6)), ("global0", (Env0 global0))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0)))
    ((toSym False))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [((SymInteger, Tree), Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [((SymInteger, Tree), Bool)] -> SymBool
        constraint [] = con True
        constraint (((x1,x2), y) : xs) = main' x1 x2 ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((3)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((3)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (3)))))), (toSym True))
                , ((((3)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (3)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((3)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((4)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                , ((((5)), ((Node ((2)) ((Leaf (1))) ((Leaf (2)))))), (toSym True))
                , ((((5)), ((Node ((1)) ((Leaf (1))) ((Leaf (3)))))), (toSym True))
                , ((((3)), ((Node ((1)) ((Leaf (1))) ((Leaf (1)))))), (toSym True))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
