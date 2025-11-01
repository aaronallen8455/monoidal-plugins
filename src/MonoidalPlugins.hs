{-# LANGUAGE CPP #-}
module MonoidalPlugins
  ( MonoidalPlugin(..)
  , foldPlugins
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Foldable
import           Data.IORef
import           GHC.Driver.Plugins
#if MIN_VERSION_ghc(9,8,0)
import           GHC.Tc.Errors.Hole.Plugin
#else
import           GHC.Tc.Errors.Hole.FitTypes
#endif
import qualified GHC.Tc.Types as Tc

foldPlugins
  :: forall f. (Foldable f, Coercible (f Plugin) (f MonoidalPlugin))
  => f Plugin -> Plugin
foldPlugins = coerce . fold @f @MonoidalPlugin . coerce

newtype MonoidalPlugin = MonoidalPlugin { unMonoidalPlugin :: Plugin }

instance Semigroup MonoidalPlugin where
  MonoidalPlugin a <> MonoidalPlugin b = MonoidalPlugin Plugin
    { installCoreToDos = \opts ->
        installCoreToDos a opts >=> installCoreToDos b opts
    , tcPlugin = \opts -> coerce $
        (coerce $ tcPlugin a opts :: Maybe SGTcPlugin)
        <> coerce (tcPlugin b opts)
    , defaultingPlugin = \opts -> coerce $
        (coerce $ defaultingPlugin a opts :: Maybe SGDefaultingPlugin)
        <> coerce (defaultingPlugin b opts)
    , holeFitPlugin = \opts -> coerce $
        (coerce $ holeFitPlugin a opts :: Maybe SGHoleFitPlugin)
        <> coerce (holeFitPlugin b opts)
    , driverPlugin = \opts -> driverPlugin a opts >=> driverPlugin b opts
#if MIN_VERSION_ghc(9,10,0)
    , latePlugin = \hscEnv opts -> latePlugin a hscEnv opts >=> latePlugin b hscEnv opts
#endif
    , pluginRecompile = \opts ->
        pluginRecompile a opts <> pluginRecompile b opts
    , parsedResultAction = \opts modSum ->
        parsedResultAction a opts modSum >=> parsedResultAction b opts modSum
    , renamedResultAction = \opts gblEnv ->
        renamedResultAction a opts gblEnv >=> uncurry (renamedResultAction b opts)
    , typeCheckResultAction = \opts modSum ->
        typeCheckResultAction a opts modSum >=> typeCheckResultAction b opts modSum
    , spliceRunAction = \opts ->
        spliceRunAction a opts >=> spliceRunAction b opts
    , interfaceLoadAction = \opts ->
        interfaceLoadAction a opts >=> interfaceLoadAction b opts
    }

instance Monoid MonoidalPlugin where
  mempty = MonoidalPlugin defaultPlugin { pluginRecompile = mempty }

newtype SGHoleFitPlugin = SGHoleFitPlugin HoleFitPluginR

instance Semigroup SGHoleFitPlugin where
  SGHoleFitPlugin (HoleFitPluginR initA runA stopA)
    <> SGHoleFitPlugin (HoleFitPluginR initB runB stopB) =
    SGHoleFitPlugin HoleFitPluginR
    { hfPluginInit = do
        r1 <- initA
        r2 <- initB
        liftIO $ newIORef (r1, r2)
    , hfPluginRun = \ref ->
        HoleFitPlugin
          { candPlugin = \hole xs -> do
              (r1, r2) <- liftIO $ readIORef ref
              candPlugin (runA r1) hole xs >>= candPlugin (runB r2) hole
          , fitPlugin = \hole xs -> do
              (r1, r2) <- liftIO $ readIORef ref
              fitPlugin (runA r1) hole xs >>= fitPlugin (runB r2) hole
          }
    , hfPluginStop = \ref -> do
        (r1, r2) <- liftIO $ readIORef ref
        stopA r1
        stopB r2
    }

newtype SGDefaultingPlugin = SGDefaultingPlugin Tc.DefaultingPlugin

instance Semigroup SGDefaultingPlugin where
  SGDefaultingPlugin (Tc.DefaultingPlugin initA runA stopA)
    <> SGDefaultingPlugin (Tc.DefaultingPlugin initB runB stopB) =
    SGDefaultingPlugin Tc.DefaultingPlugin
    { Tc.dePluginInit = (,) <$> initA <*> initB
    , Tc.dePluginRun = \(s1, s2) wc -> do
        (<>) <$> runA s1 wc <*> runB s2 wc
    , Tc.dePluginStop = \(s1, s2) -> stopA s1 >> stopB s2
    }

newtype SGTcPlugin = SGTcPlugin Tc.TcPlugin

instance Semigroup SGTcPlugin where
  SGTcPlugin (Tc.TcPlugin initA solveA rewriteA stopA)
    <> SGTcPlugin (Tc.TcPlugin initB solveB rewriteB stopB) =
    SGTcPlugin Tc.TcPlugin
    { Tc.tcPluginInit = (,) <$> initA <*> initB
    , Tc.tcPluginSolve = \(s1, s2) evBinds givens wanteds -> do
        r1 <- solveA s1 evBinds givens wanteds
        r2 <- solveB s2 evBinds givens wanteds
        pure Tc.TcPluginSolveResult
          { Tc.tcPluginInsolubleCts = Tc.tcPluginInsolubleCts r1 <> Tc.tcPluginInsolubleCts r2
          , Tc.tcPluginSolvedCts = Tc.tcPluginSolvedCts r1 <> Tc.tcPluginSolvedCts r2
          , Tc.tcPluginNewCts = Tc.tcPluginNewCts r1 <> Tc.tcPluginNewCts r2
          }
    , Tc.tcPluginRewrite = \(s1, s2) -> rewriteA s1 <> rewriteB s2
    , Tc.tcPluginStop = \(s1, s2) -> stopA s1 >> stopB s2
    }
