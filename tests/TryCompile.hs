module Main where

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
        it "compiles exponentiation" $ do
            tryCompile "tests/Exp.hs" `shouldReturn` True
        it "compiles square roots" $ do
            tryCompile "tests/Sqrt.hs" `shouldReturn` True
        it "compiles quantities fractional exponents" $ do
            tryCompile "tests/QuExp.hs" `shouldReturn` True
    describe "Compile-time behaviour: malformed samples" $ do
        it "rejects malformed sum" $ do
            tryCompile "tests/MalformedSum.hs" `shouldReturn` False

-- We are not interested in the actual text of compile errors, just
-- in ghc's return code.
messager :: FatalMessager
messager _ = return ()

logAction :: LogAction
logAction _ _ _ _ _ _ = return ()

tryCompile :: String -> IO Bool
tryCompile = compile >=> toBool
  where
    toBool Succeeded = return True
    toBool Failed = return False

    compile test = defaultErrorHandler messager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags (dflags{
                  ghcLink = LinkInMemory
                , hscTarget = HscInterpreted
                , includePaths = ["src/"] ++ includePaths dflags
                , importPaths = ["src/"] ++ importPaths dflags
                , log_action = logAction
                })
            target <- guessTarget test Nothing
            setTargets [target]
            r <- load LoadAllTargets
            setTargets []
            _ <- load LoadAllTargets
            return r