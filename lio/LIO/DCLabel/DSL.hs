{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  This module implements a simple, embedded domain specific language
  to create 'Component's, privilage descriptions and labels from
  conjunctions of principal disjunctions.

  A 'DCLabel' consists of a secrecy 'Component' and an integrity
  'Component'.  The '%%' operator allows one to construct a 'DCLabel'
  by joining a secrecy 'Component' (on the left) with an integrity
  'Component' on the right.  This is similar to 'dcLabel', except that
  the arguments can also be instances of 'ToComponent'.  For example,
  the following expresses data that can be exported by the principal
  \"Alice\" and written by anybody:  @\"Alice\" '%%' 'True'@.  (The
  component 'True' or 'dcTrue' indicates a trivially satisfiable label
  component, which in this case means a label with no integrity
  guarantees.)

  A 'Component' or 'DCPrivDesc' is created using the ('\/') and ('/\')
  operators.  The disjunction operator ('\/') is used to create a
  'Clause' from 'Principal's, ByteStrings, or a disjunctive
  sub-expression. For example:

  @
     p1 = 'principal' \"p1\"
     p2 = 'principal' \"p2\"
     p3 = 'principal' \"p3\"
     e1 = p1 '\/' p2
     e2 = e1 '\/' \"p4\"
  @

  Similarly, the conjunction operator ('/\') is used to create category-sets
  from 'Principal's, ByteStrings, and conjunctive or disjunctive sub-expressions.
  For example:

  @
     e3 = p1 '\/' p2
     e4 = e1 '/\' \"p4\" '/\' p3
  @

  /Note/ that because a clause consists of a disjunction of principals, and a
  component is composed of the conjunction of categories, ('\/') binds
  more tightly than ('/\').

  Given two 'Component's, one for secrecy and one for integrity, you
  can create a 'DCLabel' with 'dcLabel'.  Given a 'DCPriv' and
  'DCPrivDesc' you can create a new minted privilege with
  'dcDelegatePriv'.
  
  
  Consider the following, example:

  @
l1 = \"Alice\" '\/' \"Bob\" '/\' \"Carla\"
l2 = \"Alice\" '/\' \"Carla\"
dc1 = 'dcLabel' l1 l2
dc2 = 'dcLabel' ('toComponent' \"Djon\") ('toComponent' \"Alice\")
pr = PrivTCB $ toComponent $ \"Alice\" '/\' \"Carla\"
  @

  This will result in the following:

>>> dc1
"Carla" /\ ("Alice" \/ "Bob") %% "Alice" /\ "Carla"
>>> dc2
"Djon" %% "Alice"
>>> canFlowTo dc1 dc2
False
>>> canFlowToP pr dc1 dc2
True

-}

module LIO.DCLabel.DSL (
  -- * Operators
    (%%), (\/), (/\), ToComponent(..)
  , fromList, toList
  ) where

import qualified Data.Set as Set

import LIO.Privs
import LIO.DCLabel.Core

-- | Convert a type (e.g., 'Clause', 'Principal') to a label component.
class ToComponent a where
  -- | Convert to 'Component'
  toComponent :: a -> Component

infix 5 %%

-- | Create a `DCLabel` from a secrecy `ToComponent` and integrity
-- `ToComponent`. E.g.:
--
-- @
--   \"secrecy\" %% \"integrity\"
-- @
--
-- @
-- infix 5 %%
-- @
(%%) :: (ToComponent a, ToComponent b) => a -> b -> DCLabel
(%%) sec int = dcLabel (toComponent sec) (toComponent int)

-- | Identity of 'Component'.
instance ToComponent Component where
  {-# INLINE toComponent #-}
  toComponent = id
instance ToComponent (Priv Component) where
  {-# INLINE toComponent #-}
  toComponent = privDesc
-- | Convert singleton 'Clause' to 'Component'.
instance ToComponent Clause    where
  toComponent c = DCFormula $! Set.singleton c
-- | Convert singleton 'Principal' to 'Component'.
instance ToComponent Principal where
  toComponent p = toComponent . Clause $! Set.singleton p

-- | Convert singleton 'Principal' (in the form of a 'String')to 'Component'.
instance ToComponent String where
  toComponent = toComponent . principal

{-
-- | Convert singleton 'Principal' (in the form of a @ByteString@)to
-- 'Component'.
instance ToComponent S8.ByteString where
  toComponent = toComponent . Principal
-- XXX we should consider eliminating S8.ByteString instance, as it
-- makes it hard to use toComponent with OverloadedStrings
-}

instance ToComponent Bool where
  {-# INLINE toComponent #-}
  toComponent True = DCFormula Set.empty
  toComponent False = DCFalse

infixl 7 \/
infixl 6 /\

-- | Conjunction of two 'Principal'-based elements.
-- 
-- @
-- infixl 6 /&#92;
-- @
--
(/\) :: (ToComponent a, ToComponent b) => a -> b -> Component
a /\ b = dcReduce $! toComponent a `dcAnd` toComponent b

-- | Disjunction of two 'Principal'-based elements.
-- 
-- @
-- infixl 7 \\/
-- @
--
(\/) :: (ToComponent a, ToComponent b) => a -> b -> Component
a \/ b = dcReduce $! toComponent a `dcOr` toComponent b

{-
--
-- Aliases
--

XXX - These are bad, because in English saying "everybody can read"
and "anybody can read" sound like the same thing.  If we are going to
have aliases (though I don't think we should), they should be
something like impossible or unsatisfiable (for dcFalse) and open or
unrestricted (for dcTrue).

-- | Logical falsehood can be thought of as the component containing
-- every possible principal:
--
-- > everybody = dcFalse
--
everybody :: Component
everybody = dcFalse

-- | Logical truth can be thought of as the component containing
-- no specific principal:
--
-- > anybody = dcTrue
--
anybody :: Component
anybody = dcTrue
-}


-- | Convert a 'Component' to a list of list of 'Principal's if the
-- 'Component' does not have the value 'DCFalse'. In the latter case
-- the function returns an exception.
toList :: Component -> [[Principal]]
toList DCFalse        = error "toList: Invalid use, expected DCFormula"
toList (DCFormula cs) = map (Set.toList . unClause) $! Set.toList cs

-- | Convert a list of list of 'Principal's to a 'Component'. Each
-- inner list is considered to correspond to a 'Clause'.
fromList :: [[Principal]] -> Component
fromList ps = DCFormula . Set.fromList $! map (Clause . Set.fromList) ps
