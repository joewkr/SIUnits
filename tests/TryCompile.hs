module Main where

import Control.Monad ((>=>))
import DynFlags
import GHC
import GHC.Paths ( libdir )
import Test.Hspec

data ResultExpectation = Good | Bad

main :: IO ()
main = hspec $ do
    describe "Compile-time behaviour: correct samples" $ do
        it "compiles multiplication" $ do
            tryCompile Good "tests/Multiply.hs" `shouldReturn` True
        it "compiles division" $ do
            tryCompile Good "tests/Divide.hs" `shouldReturn` True
        it "compiles sum" $ do
            tryCompile Good "tests/Sum.hs" `shouldReturn` True
        it "compiles difference" $ do
            tryCompile Good "tests/Diff.hs" `shouldReturn` True
        it "compiles exponentiation" $ do
            tryCompile Good "tests/Exp.hs" `shouldReturn` True
        it "compiles square roots" $ do
            tryCompile Good "tests/Sqrt.hs" `shouldReturn` True
        it "compiles quantities fractional exponents" $ do
            tryCompile Good "tests/QuExp.hs" `shouldReturn` True
        it "reduces repetition to exponentiation" $ do
            tryCompile Good "tests/MultipleRepetitions.hs" `shouldReturn` True
        it "reduces nested exponentiation" $ do
            tryCompile Good "tests/NestedExp.hs" `shouldReturn` True
        it "reduces inner nested exponentiation" $ do
            tryCompile Good "tests/InnerNestedExp.hs" `shouldReturn` True
    describe "Compile-time behaviour: malformed samples" $ do
        it "rejects malformed sum" $ do
            tryCompile Bad "tests/MalformedSum.hs" `shouldReturn` False
        it "rejects sum of mixture of raw and dimensional entities" $ do
            tryCompile Bad "tests/MixedSum.hs" `shouldReturn` False

-- We are not interested in the actual text of compile errors, just
-- in ghc's return code.
messager :: FatalMessager
messager _ = return ()

silentLogAction :: LogAction
silentLogAction _ _ _ _ _ _ = return ()

tryCompile :: ResultExpectation -> String -> IO Bool
tryCompile Good = tryCompileG defaultLogAction
tryCompile Bad = tryCompileG silentLogAction

tryCompileG :: LogAction -> String -> IO Bool
tryCompileG logAction = compile >=> toBool
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