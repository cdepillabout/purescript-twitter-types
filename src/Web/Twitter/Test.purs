
module Web.Twitter.Test where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe)

data Foo = Foo
  { int :: Int
  , foo :: Maybe Foo
  }

derive instance genericFoo :: Generic Foo _



-- Trying to use genericEq to define eq gives the following error:
--
--   [1/1 CycleInDeclaration] src/Web/Twitter/Test.purs:19:1
--
--     25  instance eqFoo :: Eq Foo where eq = genericEq
--         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
--
--     The value of eqFoo is undefined here, so this reference is not allowed.
-- instance eqFoo :: Eq Foo where eq = genericEq



-- However, it is possible to easily define this by hand:
instance eqFoo :: Eq Foo where
  eq :: Foo -> Foo -> Boolean
  eq (Foo fooA) (Foo fooB) =
    fooA.int == fooB.int && fooA.foo == fooB.foo
