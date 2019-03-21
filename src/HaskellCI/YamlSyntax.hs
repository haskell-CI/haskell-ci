{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
module HaskellCI.YamlSyntax (
    Yaml (..),
    reann,
    ToYaml (..),
    prettyYaml,
    ) where

import Prelude ()
import Prelude.Compat

import Data.Bifunctor     (first)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid        (Endo (..))
import Data.String        (IsString (..))

import qualified Data.Aeson              as Aeson
import qualified Data.List.NonEmpty      as NE
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.YAML               as YAML


-- $setup
-- >>> :set -XOverloadedStrings

-------------------------------------------------------------------------------
-- Yaml syntx
-------------------------------------------------------------------------------

-- | This is not complete YAML document tree;
-- only as much as we need in @haskell-ci@.
data Yaml ann
    = YString ann String
    | YBool ann Bool
    | YList ann [Yaml ann]
    | YKeyValues ann [(ann, String, Yaml ann)]
    | YValue ann Aeson.Value  -- ^ inline JSON (for compactness)
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Monoid ann => IsString (Yaml ann) where
    fromString = YString mempty

-- | Re-annotate top-level term
reann :: (ann -> ann) -> Yaml ann -> Yaml ann
reann f (YString ann s)     = YString (f ann) s
reann f (YBool ann b)       = YBool (f ann) b
reann f (YList ann xs)      = YList (f ann) xs
reann f (YKeyValues ann xs) = YKeyValues (f ann) xs
reann f (YValue ann v)      = YValue (f ann) v

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class ToYaml a where
    toYaml :: a -> Yaml [String]

instance ann ~ [String] => ToYaml (Yaml ann) where
    toYaml = id

instance ToYaml Bool where
    toYaml = YBool []

instance ToYaml a => ToYaml [a] where
    toYaml = YList [] . map toYaml

instance ToYaml Aeson.Value where
    toYaml = YValue []

-------------------------------------------------------------------------------
-- Converting to string
-------------------------------------------------------------------------------

-- | Convert 'Yaml' to 'String'. @ann@ can be converted to comments.
--
-- == Examples
--
-- >>> let demo = putStr . prettyYaml lines
--
-- >>> demo "foo"
-- foo
--
-- >>> demo "foo: bar"
-- "foo: bar"
--
-- >>> demo $ YString "a comment" "foo"
-- # a comment
-- foo
--
-- >>> demo $ YBool "a comment" True
-- # a comment
-- true
--
-- >>> demo $ YList "" []
-- []
--
-- >>> demo $ YList "" ["foo", "foo: bar"]
-- - foo
-- - "foo: bar"
--
-- >>> demo $ YList "comment1" [YString "comment2" "foo", YString "comment3" "foo: bar"]
-- # comment1
-- #
-- # comment2
-- - foo
-- # comment3
-- - "foo: bar"
--
-- >>> demo $ YKeyValues "" []
-- {}
--
-- >>> demo $ YKeyValues "" [("", "foo", "bar"), ("", "help", "welcome")]
-- foo: bar
-- help: welcome
--
-- >>> let nested = YKeyValues "comment1" [("comment2", "foo", YString "comment3" "bar"), ("comment4", "help", YString "comment5" "welcome")]
-- >>> demo nested
-- # comment1
-- #
-- # comment2
-- #
-- # comment3
-- foo: bar
-- # comment4
-- #
-- # comment5
-- help: welcome
--
-- >>> demo $ YKeyValues "top" [("", "nested", nested)]
-- # top
-- nested:
--   # comment1
--   # 
--   # comment2
--   # 
--   # comment3
--   foo: bar
--   # comment4
--   # 
--   # comment5
--   help: welcome
--
-- >>> demo $ YValue "inline json" $ Aeson.toJSON [True, False, True]
-- # inline json
-- [true,false,true]
--
-- >>> demo $ YKeyValues "" [ ("", "addons", YValue "" $ Aeson.toJSON $ [Just "foo", Just "bar", Nothing]) ]
-- addons: ["foo","bar",null]
--
-- >>> demo $ YString "" $ unlines ["foo","bar","baz"]
-- "foo\nbar\nbaz\n"
--
-- >>> let multiline = YString "" $ unlines ["foo", "bar", "baz"]
-- >>> demo $ YList "" [multiline, multiline]
-- - |
--   foo
--   bar
--   baz
-- - |
--   foo
--   bar
--   baz
--
-- >>> demo $ YKeyValues "" [("", "keyA", multiline), ("", "keyB", multiline)]
-- keyA: |
--   foo
--   bar
--   baz
-- keyB: |
--   foo
--   bar
--   baz
--
prettyYaml :: forall ann. (ann -> [String]) -> Yaml ann -> String
prettyYaml comment' = flatten . go where
    comment :: ann -> [String]
    comment = concatMap lines' . comment' where
        lines' "" = [""]
        lines' s  = lines s 

    go :: Yaml ann -> NonEmpty (Int, Line)
    go (YString ann s) = case literal s of
        Just ss -> pure (0, Line (comment ann) ss)
        Nothing -> pure (0, Line (comment ann) (shows s))

    go (YBool ann b) =
        pure (0, Line (comment ann) (showString $ if b then "true" else "false"))

    go (YValue ann v) =
        pure (0, Line (comment ann) (showString $ TL.unpack $ TLE.decodeUtf8 $ Aeson.encode v))

    go (YList ann [])     = pure (0, Line (comment ann) (showString "[]"))
    go (YList ann (x:xs)) = y :| (ys ++ yss)
      where
        y :: (Int, Line)
        ys :: [(Int, Line)]
        ~(y :| ys) = case goSub x of
            Right ((_, Line cs z) :| zs) ->
                (0, Line (comment ann +++ cs) $ showString "- " . z) :|
                fmap (first succ) zs

            Left (cs, ls) -> 
                (0, Line (comment ann +++ cs) $ showString "- |") :|
                [ (1, Line [] (showString l))
                | l <- ls
                ]         

        yss :: [(Int, Line)]
        yss = do
            e <- goSub <$> xs
            case e of 
                Right ((_, Line cs z) :| zs) ->
                    (0, Line cs (showString "- " . z)) :
                    fmap (first succ) zs
                Left (cs, ls) -> 
                    (0, Line cs $ showString "- |") :
                    [ (1, Line [] (showString l))
                    | l <- ls
                    ]  

    go (YKeyValues ann [])     = pure (0, Line (comment ann) (showString "{}"))
    go (YKeyValues ann (x:xs)) = kv (comment ann) x <+> (xs >>= NE.toList . kv [])
      where
        kv :: [String] -> (ann, String, Yaml ann) -> NonEmpty (Int, Line)
        kv cs (ann', k, v) = case goSub v of
            -- single line
            Right ((_, Line cs' s) :| []) | isScalar v ->
                (0, Line (cs +++ comment ann' +++ cs') $
                    showString k . showString ": " . s) :|
                    []
            -- multiline non escaped
            Left (cs', ls) -> 
                (0, Line (cs +++ comment ann' +++ cs') $
                    showString k . showString ": |") :|
                    [ (1, Line [] (showString l))
                    | l <- ls
                    ]
            -- multiline
            Right vs -> 
                (0, Line (cs +++ comment ann') $ showString k . showChar ':') :|
                NE.toList (fmap (first succ) vs)

    -- which values can be on the same line with `:`
    isScalar YBool {}   = True
    isScalar YString {} = True
    isScalar YValue {}  = True
    isScalar _          = False

    goSub :: Yaml ann -> Either ([String], [String]) (NonEmpty (Int, Line))
    goSub (YString ann s) = case literal s of
        Just ss -> Right (pure (0, Line (comment ann) ss))
        Nothing -> case multiline s of
            Just ll -> Left (comment ann, ll)
            Nothing -> Right (pure (0, Line (comment ann) (shows s)))
    goSub y = Right (go y)
        
    -- given "foo" can it be encode without quotes:
    -- 
    --    foo
    --
    literal :: String -> Maybe ShowS
    literal s = case YAML.decodeStrict bs of
        Right [t'] | t == t' -> Just (showString s)
        _                    -> Nothing
      where
        t  = T.pack s
        bs = TE.encodeUtf8 t
        
    -- when not top level, we can encode "foo\nbar\n" as
    --
    --     - |
    --       foo
    --       bar
    --
    -- Note: the input have to end with @\n@ for this to be triggered.
    --
    multiline :: String -> Maybe [String]
    multiline s = case YAML.decodeStrict bs of
        Right [[t']] | t == t' -> Just ls
        _                      -> Nothing
      where
        ls = lines s
        t  = T.pack s

        ys = "- |\n" ++ concatMap (\l -> "  " ++ l ++ "\n") ls
        yt  = T.pack ys
        bs = TE.encodeUtf8 yt

    -- when concatenating comment blocks, we add an empty line in between
    (+++) :: [String] -> [String] -> [String]
    [] +++ xs = xs
    xs +++ [] = xs
    xs +++ ys = xs ++ [""] ++ ys

    -- We can concatenate a list to a 'NonEmpty' list, the result is 'NonEmpty'.
    (<+>) :: NonEmpty a -> [a] -> NonEmpty a
    (x :| xs) <+> ys = x :| (xs ++ ys)

    flatten :: NonEmpty (Int, Line) -> String
    flatten xs = appEndo (foldMap f xs) "" where
        f (lvl, Line cs s) =
            foldMap showComment cs <> g s
          where
            showComment "" = g (showString "#")
            showComment c  = g (showString "# " . showString c)
            g x = Endo (showString lvl' . x . showChar '\n')
            lvl' = replicate (lvl * 2) ' '

-- a 'Line' is comments before in and actual text after!
data Line = Line [String] ShowS
