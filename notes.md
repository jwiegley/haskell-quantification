ExplicitForAll
ScopedTypeVariables
RankNTypes
ExistentialQuantification
GADTs
ImpredicativeTypes
LiberalTypeSynonyms
UnicodeSyntax
TypeInType (explicit kind quantification)

-Wunused-foralls

-fprint-explicit-foralls

data Exists = forall a. Exists a

That represents an existential type, to get at the "existential type" you need
to unwrap it by pattern matching:

unexist :: Exists -> r
unexist (Exists x) = foo x

newtype Exists = Exists (forall r. (forall a. a -> r) -> r)

  ∀ x. x ⇔ ¬ ∃ x. ¬ x
¬ ∀ x. x ⇔   ∃ x. ¬ x
  ∃ x. x ⇔ ¬ ∀ x. ¬ x
¬ ∃ x. x ⇔   ∀ x. ¬ x

Unification

Skolems

The ST Monad

Local information hiding

Free theorems

Generic programming

Con(ForallC [TyVarBndr] Cxt Con)
Type(ForallT [TyVarBndr] Cxt Type)

# 9.21. Impredicative polymorphism

-XImpredicativeTypes
    Implies:	RankNTypes

Allow impredicative polymorphic types.

In general, GHC will only instantiate a polymorphic function at a monomorphic
type (one with no foralls). For example,

    runST :: (forall s. ST s a) -> a
    id :: forall b. b -> b
    
    foo = id runST   -- Rejected

The definition of foo is rejected because one would have to instantiate id‘s
type with b := (forall s. ST s a) -> a, and that is not allowed. Instantiating
polymorphic type variables with polymorphic types is called impredicative
polymorphism.

GHC has extremely flaky support for impredicative polymorphism, enabled with
-XImpredicativeTypes. If it worked, this would mean that you could call a
polymorphic function at a polymorphic type, and parameterise data structures
over polymorphic types. For example:

    f :: Maybe (forall a. [a] -> [a]) -> Maybe ([Int], [Char])
    f (Just g) = Just (g [3], g "hello")
    f Nothing  = Nothing

Notice here that the Maybe type is parameterised by the polymorphic type
(forall a. [a] -> [a]). However the extension should be considered highly
experimental, and certainly un-supported. You are welcome to try it, but
please don’t rely on it working consistently, or working the same in
subsequent releases. See this wiki page for more details.

If you want impredicative polymorphism, the main workaround is to use a
newtype wrapper. The id runST example can be written using theis workaround
like this:

    runST :: (forall s. ST s a) -> a
    id :: forall b. b -> b
    
    nwetype Wrap a = Wrap { unWrap :: (forall s. ST s a) -> a }
    
    foo :: (forall s. ST s a) -> a
    foo = unWrap (id (Wrap runST))
          -- Here id is called at monomorphic type (Wrap a)

