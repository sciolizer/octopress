module Differentiate where

type Id = String

data Term p
  = Primitive p
  | Lambda Id (Term p)
  | App (Term p) (Term p)
  | Var Id
  deriving (Eq, Ord, Read, Show)

differentiate :: MonadId m => (p -> m (Term p)) -> Term p -> m (Term p)
differentiate differentiatePrimitive = diff where
  diff term =
    case term of
      Primitive p -> differentiatePrimitive p

      Lambda var term -> do
        let dvar = "d" ++ var
        rememberId var var $ generateId dvar $ \var' -> do
          term' <- rememberId dvar var' $ diff term
          return (Lambda var (Lambda var' term'))

      App s t -> do
        s' <- diff s

        -- t and t' will often share common sub-expressions.
        -- A better implementation would factor their commonalities out,
        -- to avoid redundant computation at runtime.
        t' <- diff t

        return (App (App s' t) t')

      Var var -> do
        var' <- recallId var
        return (Var var')

class Monad m => MonadId m where
  -- Return a unique string that starts with the given string.
  generateId :: String -> (String -> m a) -> m a
  -- Add mapping from old variable name to new variable name
  rememberId :: String -> String -> m a -> m a
  -- Lookup the new variable name that was mapped to the given old variable name.
  recallId :: String -> m String
