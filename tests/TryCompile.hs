{-# LANGUAGE CPP #-}
module Main(main) where

import Control.Applicative ((<$>))
import Control.Monad ((>=>), filterM, mapM_)
import Data.List (isSuffixOf, isInfixOf)
import DynFlags
import Packages
import GHC
import GHC.Paths ( libdir )
import Test.Hspec

import Control.Monad.IO.Class
import System.Directory
import System.FilePath

ghcVersion :: String
ghcVersion = major ++ "." ++ minor ++ "." ++ patch1
#ifdef __GLASGOW_HASKELL_PATCHLEVEL2__
    ++ "." ++ patch2
#endif
  where
    versionMajor :: Int
    versionMajor = __GLASGOW_HASKELL__

    patchLevel1 :: Int
    patchLevel1 = __GLASGOW_HASKELL_PATCHLEVEL1__

    major :: String
    major = show $! versionMajor `div` 100

    minor = show $! versionMajor `mod` 100

    patch1 :: String
    patch1 = show $! patchLevel1

#ifdef __GLASGOW_HASKELL_PATCHLEVEL2__
    patchLevel2 :: Int
    patchLevel2 = __GLASGOW_HASKELL_PATCHLEVEL2__

    patch2 :: String
    patch2 = show $! patchLevel2
#endif

getSandboxPackageDb :: IO (Maybe [FilePath])
getSandboxPackageDb = do
    pwd <- getCurrentDirectory
    hasSandbox <- doesDirectoryExist $ pwd </> ".cabal-sandbox"
    pkgs <- case hasSandbox of
        False -> return Nothing
        True -> do
            let sandboxDir = pwd </> ".cabal-sandbox"
                normalize = map (sandboxDir </>)
            contents <- (normalize <$> listDirectory sandboxDir) >>= filterM doesDirectoryExist
            return . Just . filter (isInfixOf ghcVersion) $! filter (isSuffixOf "packages.conf.d") contents
    case pkgs of
        Just ps -> do
            putStrLn "using package database:"
            mapM_ putStrLn ps
        Nothing -> putStrLn "using default package database"
    return $! pkgs

main :: IO ()
main = getSandboxPackageDb >>= runTests

data ResultExpectation = Good | Bad

runTests :: Maybe [FilePath] -> IO ()
runTests pkgs = hspec $ do
    describe "Compile-time behaviour: correct samples" $ do
        it "compiles multiplication" $ do
            tryCompile Good "tests/cases/Multiply.hs" `shouldReturn` True
        it "compiles division" $ do
            tryCompile Good "tests/cases/Divide.hs" `shouldReturn` True
        it "compiles sum" $ do
            tryCompile Good "tests/cases/Sum.hs" `shouldReturn` True
        it "compiles difference" $ do
            tryCompile Good "tests/cases/Diff.hs" `shouldReturn` True
        it "compiles exponentiation" $ do
            tryCompile Good "tests/cases/Exp.hs" `shouldReturn` True
        it "compiles square roots" $ do
            tryCompile Good "tests/cases/Sqrt.hs" `shouldReturn` True
        it "compiles quantities fractional exponents" $ do
            tryCompile Good "tests/cases/QuExp.hs" `shouldReturn` True
        it "compiles sum of tagged and untagged quantities" $ do
            tryCompile Good "tests/cases/MixedTagSum.hs" `shouldReturn` True
        it "compiles difference of tagged and untagged quantities" $ do
            tryCompile Good "tests/cases/MixedTagDiff.hs" `shouldReturn` True
        it "compiles multiplication of tagged and untagged quantities" $ do
            tryCompile Good "tests/cases/MixedTagMultiply.hs" `shouldReturn` True
        it "compiles division of tagged and untagged quantities" $ do
            tryCompile Good "tests/cases/MixedTagDivide.hs" `shouldReturn` True
        it "reduces repetition to exponentiation" $ do
            tryCompile Good "tests/cases/MultipleRepetitions.hs" `shouldReturn` True
        it "reduces nested exponentiation" $ do
            tryCompile Good "tests/cases/NestedExp.hs" `shouldReturn` True
        it "reduces inner nested exponentiation" $ do
            tryCompile Good "tests/cases/InnerNestedExp.hs" `shouldReturn` True
        it "converts raw value to dimensionless one" $ do
            tryCompile Good "tests/cases/DimensionlessConstant.hs" `shouldReturn` True
    describe "Compile-time behaviour: malformed samples" $ do
        it "rejects malformed sum" $ do
            tryCompile Bad "tests/cases/MalformedSum.hs" `shouldReturn` False
        it "rejects sum of mixture of raw and dimensional entities" $ do
            tryCompile Bad "tests/cases/MixedSum.hs" `shouldReturn` False
  where
    tryCompile = case pkgs of
        Just ps -> tryCompileWithPkg ps
        Nothing -> tryCompileWithPkg []

-- We are not interested in the actual text of compile errors, just
-- in ghc's return code.
messager :: FatalMessager
messager _ = return ()

silentLogAction :: LogAction
silentLogAction _ _ _ _ _ _ = return ()

tryCompileWithPkg :: [FilePath] -> ResultExpectation -> String -> IO Bool
tryCompileWithPkg pkgs Good = tryCompileG pkgs defaultLogAction
tryCompileWithPkg pkgs Bad = tryCompileG pkgs silentLogAction

tryCompileG :: [FilePath] -> LogAction -> String -> IO Bool
tryCompileG pkgs logAction = compile >=> toBool
  where
    toBool Succeeded = return True
    toBool Failed = return False

    compile test = defaultErrorHandler messager defaultFlushOut $ do
        runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            let dflags' = dflags{
                  ghcLink = LinkInMemory
                , hscTarget = HscInterpreted
#if __GLASGOW_HASKELL__ >= 802
                , packageDBFlags = map (PackageDB . PkgConfFile) pkgs
#else
                , extraPkgConfs = (++) (map PkgConfFile pkgs)
#endif
                , includePaths = ["src/"] ++ includePaths dflags
                , importPaths = ["src/"] ++ importPaths dflags
                , log_action = logAction
                }
            (dflags'', _) <- liftIO $! initPackages dflags'
            _ <- setSessionDynFlags dflags''
            target <- guessTarget test Nothing
            setTargets [target]
            r <- load LoadAllTargets
            setTargets []
            _ <- load LoadAllTargets
            return r