module HaskellCI.Config.Jobs where

import Control.Applicative ((<|>))

import qualified Distribution.Pretty             as C
import qualified Text.PrettyPrint                as PP
import qualified Distribution.Compat.CharParsing as C
import qualified Distribution.Parsec.Class       as C

data Jobs
    = CabalJobs Int
    | GhcJobs Int
    | BothJobs Int Int
  deriving (Show)

cabalJobs :: Jobs -> Maybe Int
cabalJobs (CabalJobs n)  = Just n
cabalJobs (GhcJobs _)    = Nothing
cabalJobs (BothJobs n _) = Just n

ghcJobs :: Jobs -> Maybe Int
ghcJobs (CabalJobs _)  = Nothing
ghcJobs (GhcJobs m)    = Just m
ghcJobs (BothJobs _ m) = Just m

instance C.Parsec Jobs where
    parsec = ghc <|> rest where
        ghc  = C.char ':' *> (GhcJobs <$> C.integral)
        rest = do
            n <- C.integral
            m' <- C.optional (C.char ':' *> C.integral)
            return $ case m' of
                Nothing -> CabalJobs n
                Just m  -> BothJobs n m

instance C.Pretty Jobs where
    pretty (BothJobs n m) = PP.int n PP.<> PP.colon PP.<> PP.int m
    pretty (CabalJobs n)  = PP.int n
    pretty (GhcJobs m)    = PP.colon PP.<> PP.int m
