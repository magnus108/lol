{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Variadic where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)), insert, (<|))

-- FLYT DENNE måske ikke muligt
data N = Z | S N

type TupleNKind :: N -> Type
type family TupleNKind n = r | r -> n where
  TupleNKind Z = Type
  TupleNKind (S n) = Type -> TupleNKind n

type TupleN :: forall (n :: N). TupleNKind n
type family TupleN where
  TupleN = (,)

{-
data Mk a where
  New :: a -> Mk a
  Add :: a -> Mk a -> Mk a

type family GM (n :: N) a where
  GM Z a = Mk a
  GM (S n) a = GM n a

class ERM n a where
  build :: a -> Mk a -> GM n a

instance ERM Z a where
  build = const . New
  -}

-- instance ERM (S n) a => ERM (S (S n)) a where
--  build a x = build undefined undefined

-- instance NELBuilder (S n) a => NELBuildr (S (S n)) a where
-- buildNEL d x = buildNEL (d . (x <|))
--
--

f :: TupleN Int Int -> (Int, Int)
f = id

type family NELBuildFunc (n :: N) a = r | r -> n a where
  NELBuildFunc Z a = NonEmpty a
  NELBuildFunc (S n) a = a -> NELBuildFunc n a

class NELBuilder n a where
  buildNEL :: (NonEmpty a -> NonEmpty a) -> NELBuildFunc n a

instance NELBuilder (S Z) a where
  buildNEL d x = d (x :| [])

instance NELBuilder (S n) a => NELBuilder (S (S n)) a where
  buildNEL d x = buildNEL (d . (x <|))

nel :: NELBuilder n a => NELBuildFunc n a
nel = buildNEL id

class Builder n a where
  build :: BuildT n a

instance Builder (Z) a where
  build = []

instance Builder (S Z) a where
  build x = x : []

instance Builder (S n) a => Builder (S (S n)) a where
  build = (\x xs -> build)

type family BuildT (n :: N) a = r | r -> n a where
  BuildT Z a = [a]
  BuildT (S n) a = a -> BuildT n a

buildIt :: Builder n a => BuildT n a
buildIt = build
