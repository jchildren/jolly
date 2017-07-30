{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import           System.Console.Repline
import           System.Environment
import           System.Exit

import           Control.Monad.State.Strict

import           Data.List                  (foldl', isPrefixOf)
import qualified Data.Map                   as Map
import           Data.Monoid
import qualified Data.Text.Lazy             as L
import qualified Data.Text.Lazy.IO          as L

import           Text.Megaparsec

import           Jolly

data IState = IState
  { typectx :: TypeEnv
  , termctx :: TermEnv
  }

initState :: IState
initState = IState emptyTypeEnv emptyTermEnv

type Repl a = HaskelineT (StateT IState IO) a

hoistErr :: Show e => Either e a -> Repl a
hoistErr (Right val) = return val
hoistErr (Left err) = do
  liftIO $ print err
  abort

hoistParseErr :: (ShowErrorComponent e) 
              => L.Text -> Either (ParseError (Token L.Text) e) a -> Repl a
hoistParseErr _ (Right val) = return val
hoistParseErr stream (Left err) = do
  liftIO $ putStr $ parseErrorPretty' stream err
  abort

instance ShowErrorComponent Decl where
  showErrorComponent decl = show decl

main :: IO ()
main = do
  args <- getArgs
  case args of
    []              -> shell (return ())
    [fname]         -> shell (load [fname])
    ["test", fname] -> shell (load [fname] >> browse [] >> quit ())
    _               -> putStrLn "invalid arguments"

shell :: Repl a -> IO ()
shell pre =
  flip evalStateT initState $ evalRepl "Jolly> " cmd options completer pre

cmd :: String -> Repl ()
cmd source = exec True (L.pack source)

evalDef :: TermEnv -> (String, Expr) -> TermEnv
evalDef env (nm, ex) = tmctx'
  where
    (_, tmctx') = runEval env nm ex

exec :: Bool -> L.Text -> Repl ()
exec update source = do
  st <- get
  modl <- hoistParseErr source $ runParseModule "<stdin>" source
  typectx' <- hoistErr $ inferTop (typectx st) modl
  let st' =
        st
        { termctx = foldl' evalDef (termctx st) modl
        , typectx = typectx' <> typectx st
        }
  when update (put st')
  case lookup "it" modl of
    Nothing -> return ()
    Just ex -> do
      let (val, _) = runEval (termctx st') "it" ex
      showOutput (show val) st'

showOutput :: String -> IState -> Repl ()
showOutput arg st =
  case lookupType "it" (typectx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing  -> return ()

options :: [(String, [String] -> Repl ())]
options =
  [("load", load), ("browse", browse), ("quit", quit), ("type", Main.typeof)]

browse :: [String] -> Repl ()
browse _ = do
  st <- get
  liftIO $ mapM_ putStrLn $ ppenv (typectx st)

load :: [String] -> Repl ()
load args = do
  contents <- liftIO $ L.readFile (unwords args)
  exec True contents

typeof :: [String] -> Repl ()
typeof args = do
  st <- get
  let arg = unwords args
  case lookupType arg (typectx st) of
    Just val -> liftIO $ putStrLn $ ppsignature (arg, val)
    Nothing  -> exec False (L.pack arg)

quit :: a -> Repl ()
quit _ = liftIO exitSuccess

completer :: CompleterStyle (StateT IState IO)
completer = Prefix (wordCompleter comp) defaultMatcher

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [(":load", fileCompleter)]

comp :: (Monad m, MonadState IState m) => WordCompleter m
comp n = do
  let cmds = [":load", ":browse", ":quit", ":type"]
  TypeEnv ctx <- gets typectx
  let defs = Map.keys ctx
  return $ filter (isPrefixOf n) (cmds ++ defs)
