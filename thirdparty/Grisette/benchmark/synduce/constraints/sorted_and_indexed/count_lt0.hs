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
-- param_list: xs x
data Expr0_0
  = Param10_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_1) (UnionM Expr0_0) (UnionM Expr0_0)
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
-- param_list: t xs idx h
data Expr1_0
  = Param21_0
  | Param31_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_1) (UnionM Expr1_0) (UnionM Expr1_0)
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
-- param_list: t xs tmp1 idx h
data Expr2_0
  = Param22_0
  | Param32_0
  | Param42_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_1) (UnionM Expr2_0) (UnionM Expr2_0)
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
-- param_list: inp tmp2
data Expr3_0
  = Param13_0
  | Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_1) (UnionM Expr3_0) (UnionM Expr3_0)
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
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e1_0 e0_4 e0_5])
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
    genSingle0 = [mrgParam21_0] ++ [mrgParam31_0] ++ [mrgCzero1_0]
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
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e1_0 e0_4 e0_5])
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
    genSingle0 = [mrgParam22_0] ++ [mrgParam32_0] ++ [mrgParam42_0] ++ [mrgCzero2_0]
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
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e1_0 e0_4 e0_5])
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
    genSingle0 = [mrgParam13_0] ++ [mrgCzero3_0]
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
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e1_0 e0_4 e0_5])
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

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param10_0) = evalVar0 env "x"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)

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
eval1_0 env (Param21_0) = evalVar0 env "idx"
eval1_0 env (Param31_0) = evalVar0 env "h"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_1 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)

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
eval2_0 env (Param22_0) = evalVar0 env "tmp1"
eval2_0 env (Param32_0) = evalVar0 env "idx"
eval2_0 env (Param42_0) = evalVar0 env "h"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)

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
eval3_0 env (Param13_0) = evalVar0 env "tmp2"
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)

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


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs x 

Hole grammar for #1
t xs idx h 

Hole grammar for #2
t xs tmp1 idx h 

Hole grammar for #3
inp tmp2 
-}

data List
  = Elt SymInteger
  | Cons SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

data IDList
  = Ielt SymInteger
  | Icons SymInteger SymInteger IDList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon IDList, ExtractSymbolics)
    via (Default IDList)
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

is_sorted :: IDList -> SymBool
is_sorted xs = 
  let
    aux =
      (let
        f pre xs = 
          case xs of
            Ielt x -> pre >~  x
            Icons h _ t -> (pre >~  h) &&~  (f h t)
          
      in
      f )
  in
  case xs of
    Ielt x -> (toSym True)
    Icons h _ t -> aux h t
  

len = 
  let
    f xs = 
      case xs of
        Ielt _ -> 1
        Icons _ _ t -> 1 +  (f t)
      
  in
  f 

len_raw = 
  let
    f xs = 
      case xs of
        Elt _ -> 1
        Cons _ t -> 1 +  (f t)
      
  in
  f 

is_indexed = 
  let
    f xs = 
      case xs of
        Ielt _ -> (toSym True)
        Icons _ id t -> (id ==~  (len xs)) &&~  (f t)
      
  in
  f 

add_index = 
  let
    f xs = 
      case xs of
        Elt a -> Ielt a
        Cons h t -> Icons h (len_raw xs) (f t)
      
  in
  f 

drop_index = 
  let
    f xs = 
      case xs of
        Ielt a -> Elt a
        Icons h _ t -> Cons h (f t)
      
  in
  f 

spec :: List -> SymInteger
spec = 
  let
    f xs = 
      case xs of
        Elt x -> mrgIte (x <~  0)
            (1)
            (0)
        Cons h t -> (f t) +  (mrgIte (h <~  0)
            (1)
            (-1))
  in
  f 

target = 
  let
    f xs = 
      case xs of
        Ielt x -> 
          evalU0_0 (RefEnv [("x", (Env0 x))]) ((genSym (2::Int) "hole0") :: (UnionM Expr0_0))
        Icons h idx t -> mrgIte (h <~  0)
            (evalU1_0 (RefEnv [("idx", (Env0 idx)), ("h", (Env0 h))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0)))
            (let
              tmp1 =
                (f t)
            in
            evalU2_0 (RefEnv [("tmp1", (Env0 tmp1)), ("idx", (Env0 idx)), ("h", (Env0 h))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0)))
      
  in
  f 

main' inp = 
  mrgIte ((is_sorted inp) &&~  (is_indexed inp))
    (let
      tmp2 =
        (target inp)
    in
    evalU3_0 (RefEnv [("tmp2", (Env0 tmp2))]) ((genSym (2::Int) "hole3") :: (UnionM Expr3_0)))
    (0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(IDList, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(IDList, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Icons ((5)) ((-3)) ((Icons ((-2)) ((5)) ((Icons ((-5)) ((4)) ((Ielt (5)))))))))), (0))
                , ((((Icons ((-2)) ((-3)) ((Icons ((-1)) ((4)) ((Icons ((5)) ((3)) ((Icons ((5)) ((2)) ((Icons ((0)) ((4)) ((Icons ((4)) ((3)) ((Icons ((-2)) ((1)) ((Ielt (-2)))))))))))))))))), (0))
                , ((((Icons ((-1)) ((-3)) ((Icons ((-3)) ((3)) ((Icons ((4)) ((-3)) ((Ielt (-4)))))))))), (0))
                , ((((Icons ((-2)) ((-4)) ((Icons ((0)) ((-2)) ((Icons ((-3)) ((5)) ((Icons ((2)) ((5)) ((Icons ((0)) ((-5)) ((Icons ((-3)) ((2)) ((Ielt (-3)))))))))))))))), (0))
                , ((((Icons ((-4)) ((-2)) ((Icons ((2)) ((0)) ((Icons ((2)) ((5)) ((Icons ((-4)) ((-3)) ((Ielt (-1)))))))))))), (0))
                , ((((Ielt (-3)))), (1))
                , ((((Icons ((-5)) ((5)) ((Icons ((2)) ((-3)) ((Icons ((2)) ((3)) ((Icons ((-1)) ((1)) ((Icons ((4)) ((-4)) ((Icons ((-2)) ((-2)) ((Icons ((2)) ((-4)) ((Icons ((1)) ((-1)) ((Icons ((1)) ((-3)) ((Ielt (-5)))))))))))))))))))))), (0))
                , ((((Icons ((-5)) ((-1)) ((Icons ((-2)) ((5)) ((Icons ((-1)) ((1)) ((Ielt (-2)))))))))), (0))
                , ((((Ielt (4)))), (0))
                , ((((Icons ((4)) ((-4)) ((Icons ((-3)) ((-5)) ((Icons ((3)) ((-3)) ((Icons ((1)) ((-4)) ((Icons ((-4)) ((3)) ((Icons ((-4)) ((-3)) ((Ielt (5)))))))))))))))), (0))
                , ((((Icons ((2)) ((-4)) ((Icons ((0)) ((3)) ((Icons ((0)) ((-1)) ((Icons ((-1)) ((1)) ((Icons ((2)) ((1)) ((Ielt (2)))))))))))))), (0))
                , ((((Icons ((-5)) ((-5)) ((Icons ((-4)) ((4)) ((Icons ((2)) ((-3)) ((Ielt (-1)))))))))), (0))
                , ((((Icons ((5)) ((-4)) ((Icons ((-5)) ((0)) ((Ielt (1)))))))), (0))
                , ((((Ielt (-1)))), (1))
                , ((((Icons ((-1)) ((-1)) ((Icons ((-4)) ((0)) ((Icons ((-4)) ((-1)) ((Icons ((3)) ((-5)) ((Icons ((5)) ((4)) ((Icons ((-2)) ((-5)) ((Icons ((-1)) ((5)) ((Icons ((-1)) ((-3)) ((Icons ((-2)) ((4)) ((Ielt (-2)))))))))))))))))))))), (0))
                , ((((Icons ((-4)) ((-4)) ((Icons ((-4)) ((4)) ((Icons ((-4)) ((-5)) ((Icons ((4)) ((1)) ((Icons ((0)) ((2)) ((Icons ((-4)) ((3)) ((Icons ((-5)) ((2)) ((Icons ((-1)) ((1)) ((Ielt (2)))))))))))))))))))), (0))
                , ((((Icons ((-4)) ((2)) ((Icons ((-5)) ((-3)) ((Icons ((-3)) ((-4)) ((Icons ((5)) ((3)) ((Icons ((-4)) ((5)) ((Icons ((-4)) ((3)) ((Icons ((1)) ((3)) ((Icons ((0)) ((-3)) ((Ielt (-3)))))))))))))))))))), (0))
                , ((((Icons ((4)) ((-5)) ((Icons ((4)) ((4)) ((Ielt (-1)))))))), (0))
                , ((((Icons ((4)) ((-3)) ((Icons ((-5)) ((-2)) ((Icons ((3)) ((1)) ((Icons ((-5)) ((2)) ((Ielt (-1)))))))))))), (0))
                , ((((Icons ((5)) ((0)) ((Ielt (1)))))), (0))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
