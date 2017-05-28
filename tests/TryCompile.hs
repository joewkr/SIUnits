module Main where

import Control.Applicative ((<$))
import Control.Monad ((>=>))
import DynFlags
import GHC
import GHC.Paths ( libdir )
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Compile-time behaviour: correct samples" $ do
        it "compiles multiplication" $ do
            tryCompile "tests/Multiply.hs" `shouldReturn` True
        it "compiles division" $ do
            tryCompile "tests/Divide.hs" `shouldReturn` True
        it "compiles sum" $ do
            tryCompile "tests/Sum.hs" `shouldReturn` True
        it "compiles difference" $ do
            tryCompile "tests/Diff.hs" `shouldReturn` True
    describe "Compile-time behaviour: malformed samples" $ do
        it "rejects malformed sum" $ do
            tryCompile "tests/MalformedSum.hs" `shouldReturn` False

tryCompile :: String -> IO Bool
tryCompile = compile >=> toBool
  where
    toBool Succeeded = return True
    toBool Failed = return False

    compile test = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags (dflags{
                  ghcLink = LinkInMemory
                , hscTarget = HscInterpreted
                , includePaths = ["src/"] ++ includePaths dflags
                , importPaths = ["src/"] ++ importPaths dflags
                })
            target <- guessTarget test Nothing
            setTargets [target]
            r <- load LoadAllTargets
            setTargets []
            _ <- load LoadAllTargets
            return r