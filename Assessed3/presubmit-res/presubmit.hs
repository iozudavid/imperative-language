
{-# LANGUAGE Safe #-}
{-# LANGUAGE ImplicitParams #-}

module Presubmit where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import System.Directory (doesFileExist)
import System.Environment
import System.Exit
import System.Process
import Text.Printf

{-

  OVERVIEW OF SCRIPT

  This Haskell script checks if a student solution (e.g. Formative1.hs) is
  ready for submission.

  To do that, it

  - verifies that the file exists
  - makes a temporary directory 'presubmit-temp',
  - verifies that we are in a correct git repository
  - looks at all imports, and verifies that every import is on a whitelist,
  - copies the student solution into presubmit-temp,
  - copies other auxiliary files into it (e.g. Lib.hs, Mould.hs),
  - compiles all "auxiliary pretest" modules such as Lib.hs
  - compiles student solutions such as Formative1.hs,
  - compiles Mould.hs,
  - checks if presubmit.sh detected that we are in the lab,
  - prints a concluding verdict,
  - on success, removes (recursively) the temporary directory.

  Inside Mould.hs, the student solution is imported safely, and there is a
  type assertion against all the solutions made by the students. Furthermore,
  Mould.hs re-exports all these solutions with the expected type.

  If we know that Mould.hs compiles together with the student solution file,
  then when can test the student solution via Mould.hs and everything should
  work perfectly.

  This approach avoids a couple of problems. For instance:

  - It verifies that {-# LANGUAGE Safe #-} is set in
    the student file.
  - It verifies that no other files are required in the student solution.
  - It verifies that no non-base libraries are required in the student solution.
  - It verifies that students import data types, rather than copying the
    definitions. (This prevents the marking script from working.)
  - It verifies that students did not accidentally modify an auxiliary file.

  If we find we are in a correct git repository, then we take the auxiliary files from there, rather from the working directory. We take them from
  the last commit in branch remotes/origin/master, which should be our fp-learning
  repository. Additionally, we compare the versions in the working directory
  against the versions in our commit. If they differ, we remark that students
  might find problems, but note that this is not a problem for the presubmit
  check. We suggest steps to restore the auxiliary files to the original
  version.

-}


-- CONFIGURATION
--
-- This script sets up the "configuration" once at the beginning, and then the
-- configuration is passed to all functions using an implicit parameter.

data Config = Config {
    confVerbose :: Bool,
    confStudentMods :: [String],
    confInLab :: Bool,
    confAuxiliary :: [String],
    confPretest :: [String],
    confWhitelist :: [String]
  }

verbose :: (?config::Config) => Bool
verbose = confVerbose ?config

studentFiles :: (?config::Config) => [String]
studentFiles = map (++ ".hs") studentMods

-- Same, but for human consumption
studentFilesFriendly :: (?config::Config) => String
studentFilesFriendly = intercalate ", " studentFiles

studentMods :: (?config::Config) => [String]
studentMods = confStudentMods ?config

pretestMods :: (?config::Config) => [String]
pretestMods = confPretest ?config

pretestFilesFriendly :: (?config::Config) => String
pretestFilesFriendly = intercalate ", " $ map (++".hs") pretestMods

-- STATE
--
-- We have one global variable, "taint", for which we use a StateT monad
-- transformer around IO. We use taint for situations that aren't fatal (e.g.
-- the student imported too many modules) so we want to see if the presubmit
-- would otherwise succeed. But we still remember that there was a problem in
-- the first place.

data Taint =
    -- Everything OK so far.
    Untainted
    -- Definitely problems, don't submit please.
  | Problematic
    -- Not clear if there is a problem.
  | Ambiguous deriving (Eq, Show)

type StateIO a = StateT Taint IO a

markTainted :: StateIO ()
markTainted = put Problematic

markAmbiguous = modify atLeastAmbiguous
  where
    atLeastAmbiguous Problematic = Problematic
    atLeastAmbiguous _ = Ambiguous

-- Auxiliary files that must be copied too, from environment

auxiliaryFiles :: (?config::Config) => [String]
auxiliaryFiles = confAuxiliary ?config

allowedGitOrigins = [
    "git@git.cs.bham.ac.uk:mhe/fp-learning-2017-2018.git",
    "git@git.cs.bham.ac.uk:mhe/fp-learning-2017-2018",
    "ssh://git@git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "ssh://git@git.cs.bham.ac.uk/mhe/fp-learning-2017-2018",
    "https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018",
    "http://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git",
    "http://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018"
  ]

moduleCustomWhitelist :: (?config::Config) => [String]
moduleCustomWhitelist = confWhitelist ?config

moduleWhitelist :: (?config::Config) => [String]
moduleWhitelist = moduleCustomWhitelist ++ [

    -- package 'base'
    "Control.Applicative", "Control.Arrow", "Control.Category", "Control.Concurrent", "Control.Concurrent.Chan", "Control.Concurrent.MVar", "Control.Concurrent.QSem", "Control.Concurrent.QSemN", "Control.Exception", "Control.Exception.Base", "Control.Monad", "Control.Monad.Fail", "Control.Monad.Fix", "Control.Monad.IO.Class", "Control.Monad.Instances", "Control.Monad.ST", "Control.Monad.ST.Lazy", "Control.Monad.ST.Lazy.Safe", "Control.Monad.ST.Lazy.Unsafe", "Control.Monad.ST.Safe", "Control.Monad.ST.Strict", "Control.Monad.ST.Unsafe", "Control.Monad.Zip", "Data.Bifunctor", "Data.Bits", "Data.Bool", "Data.Char", "Data.Coerce", "Data.Complex", "Data.Data", "Data.Dynamic", "Data.Either", "Data.Eq", "Data.Fixed", "Data.Foldable", "Data.Function", "Data.Functor", "Data.Functor.Classes", "Data.Functor.Compose", "Data.Functor.Const", "Data.Functor.Identity", "Data.Functor.Product", "Data.Functor.Sum", "Data.IORef", "Data.Int", "Data.Ix", "Data.Kind", "Data.List", "Data.List.NonEmpty", "Data.Maybe", "Data.Monoid", "Data.Ord", "Data.Proxy", "Data.Ratio", "Data.STRef", "Data.STRef.Lazy", "Data.STRef.Strict", "Data.Semigroup", "Data.String", "Data.Traversable", "Data.Tuple", "Data.Type.Bool", "Data.Type.Coercion", "Data.Type.Equality", "Data.Typeable", "Data.Typeable.Internal", "Data.Unique", "Data.Version", "Data.Void", "Data.Word", "Debug.Trace", "Foreign", "Foreign.C", "Foreign.C.Error", "Foreign.C.String", "Foreign.C.Types", "Foreign.Concurrent", "Foreign.ForeignPtr", "Foreign.ForeignPtr.Safe", "Foreign.ForeignPtr.Unsafe", "Foreign.Marshal", "Foreign.Marshal.Alloc", "Foreign.Marshal.Array", "Foreign.Marshal.Error", "Foreign.Marshal.Pool", "Foreign.Marshal.Safe", "Foreign.Marshal.Unsafe", "Foreign.Marshal.Utils", "Foreign.Ptr", "Foreign.Safe", "Foreign.StablePtr", "Foreign.Storable", "Numeric", "Numeric.Natural", "Prelude", "System.CPUTime", "System.Console.GetOpt", "System.Environment", "System.Exit", "System.IO", "System.IO.Error", "System.IO.Unsafe", "System.Info", "System.Timeout", "Text.ParserCombinators.ReadP", "Text.ParserCombinators.ReadPrec", "Text.Printf", "Text.Read", "Text.Read.Lex", "Text.Show", "Text.Show.Functions", "Unsafe.Coerce",

    -- package 'mtl'
    "Control.Monad.Cont", "Control.Monad.Cont.Class", "Control.Monad.Error", "Control.Monad.Error.Class", "Control.Monad.Except", "Control.Monad.Identity", "Control.Monad.List", "Control.Monad.RWS", "Control.Monad.RWS.Class", "Control.Monad.RWS.Lazy", "Control.Monad.RWS.Strict", "Control.Monad.Reader", "Control.Monad.Reader.Class", "Control.Monad.State", "Control.Monad.State.Class", "Control.Monad.State.Lazy", "Control.Monad.State.Strict", "Control.Monad.Trans", "Control.Monad.Writer", "Control.Monad.Writer.Class", "Control.Monad.Writer.Lazy", "Control.Monad.Writer.Strict",

    -- package 'random'
    "System.Random",

    -- package 'deepseq'
    "Control.DeepSeq",

    -- package 'split'
    "Data.List.Split",
    "Data.List.Split.Internals",

    -- preinstalled in lab: package 'async'
        "Control.Concurrent.Async",
    -- preinstalled in lab: package 'base16-bytestring'
        "Data.ByteString.Base16", "Data.ByteString.Base16.Lazy",
    -- preinstalled in lab: package 'base64-bytestring'
        "Data.ByteString.Base64", "Data.ByteString.Base64.Internal", "Data.ByteString.Base64.Lazy", "Data.ByteString.Base64.URL", "Data.ByteString.Base64.URL.Lazy",
    -- preinstalled in lab: package 'cryptohash-sha256'
        "Crypto.Hash.SHA256",
    -- preinstalled in lab: package 'ed25519'
        "Crypto.Sign.Ed25519",
    -- preinstalled in lab: package 'hashable'
        "Data.Hashable.Class", "Data.Hashable.Generic", "Data.Hashable",
    -- preinstalled in lab: package 'HTTP'
        "Network.Browser", "Network.BufferType", "Network.HTTP.Auth", "Network.HTTP.Base64", "Network.HTTP.Base", "Network.HTTP.Cookie", "Network.HTTP.HandleStream", "Network.HTTP.Headers", "Network.HTTP", "Network.HTTP.MD5Aux", "Network.HTTP.Proxy", "Network.HTTP.Stream", "Network.HTTP.Utils", "Network.StreamDebugger", "Network.Stream", "Network.StreamSocket", "Network.TCP", "Paths_HTTP",
    -- preinstalled in lab: package 'MonadRandom'
        "Control.Monad.Random.Class", "Control.Monad.Random",
    -- preinstalled in lab: package 'mtl'
        "Control.Monad.Cont.Class", "Control.Monad.Cont", "Control.Monad.Error.Class", "Control.Monad.Error", "Control.Monad.Except", "Control.Monad.Identity", "Control.Monad.List", "Control.Monad.Reader.Class", "Control.Monad.Reader", "Control.Monad.RWS.Class", "Control.Monad.RWS", "Control.Monad.RWS.Lazy", "Control.Monad.RWS.Strict", "Control.Monad.State.Class", "Control.Monad.State", "Control.Monad.State.Lazy", "Control.Monad.State.Strict", "Control.Monad.Trans", "Control.Monad.Writer.Class", "Control.Monad.Writer", "Control.Monad.Writer.Lazy", "Control.Monad.Writer.Strict",
    -- preinstalled in lab: package 'network'
        "Network.BSD", "Network", "Network.Socket.ByteString", "Network.Socket.ByteString.Internal", "Network.Socket.ByteString.IOVec", "Network.Socket.ByteString.Lazy", "Network.Socket.ByteString.Lazy.Posix", "Network.Socket.ByteString.MsgHdr", "Network.Socket", "Network.Socket.Internal", "Network.Socket.Types",
    -- preinstalled in lab: package 'network-uri'
        "Network.URI",
    -- preinstalled in lab: package 'old-locale'
        "System.Locale",
    -- preinstalled in lab: package 'old-time'
        "System.Time",
    -- preinstalled in lab: package 'parsec'
        "Text.Parsec.ByteString", "Text.Parsec.ByteString.Lazy", "Text.Parsec.Char", "Text.Parsec.Combinator", "Text.Parsec.Error", "Text.Parsec.Expr", "Text.Parsec", "Text.Parsec.Language", "Text.Parsec.Perm", "Text.Parsec.Pos", "Text.Parsec.Prim", "Text.Parsec.String", "Text.Parsec.Text", "Text.Parsec.Text.Lazy", "Text.Parsec.Token", "Text.ParserCombinators.Parsec.Char", "Text.ParserCombinators.Parsec.Combinator", "Text.ParserCombinators.Parsec.Error", "Text.ParserCombinators.Parsec.Expr", "Text.ParserCombinators.Parsec", "Text.ParserCombinators.Parsec.Language", "Text.ParserCombinators.Parsec.Perm", "Text.ParserCombinators.Parsec.Pos", "Text.ParserCombinators.Parsec.Prim", "Text.ParserCombinators.Parsec.Token",
    -- preinstalled in lab: package 'QuickCheck'
        "Test.QuickCheck.All", "Test.QuickCheck.Arbitrary", "Test.QuickCheck.Exception", "Test.QuickCheck.Function", "Test.QuickCheck.Gen", "Test.QuickCheck.Gen.Unsafe", "Test.QuickCheck", "Test.QuickCheck.Modifiers", "Test.QuickCheck.Monadic", "Test.QuickCheck.Poly", "Test.QuickCheck.Property", "Test.QuickCheck.Random", "Test.QuickCheck.State", "Test.QuickCheck.Test", "Test.QuickCheck.Text",
    -- preinstalled in lab: package 'random'
        "System.Random",
    -- preinstalled in lab: package 'readline'
        "System.Console.Readline", "System.Console.SimpleLineEditor",
    -- preinstalled in lab: package 'stm'
        "Control.Concurrent.STM", "Control.Concurrent.STM.TArray", "Control.Concurrent.STM.TBQueue", "Control.Concurrent.STM.TChan", "Control.Concurrent.STM.TMVar", "Control.Concurrent.STM.TQueue", "Control.Concurrent.STM.TSem", "Control.Concurrent.STM.TVar", "Control.Monad.STM", "Control.Sequential.STM",
    -- preinstalled in lab: package 'tar'
        "Codec.Archive.Tar.Check", "Codec.Archive.Tar.Entry", "Codec.Archive.Tar", "Codec.Archive.Tar.Index", "Codec.Archive.Tar.Index.IntTrie", "Codec.Archive.Tar.Index.StringTable", "Codec.Archive.Tar.Pack", "Codec.Archive.Tar.Read", "Codec.Archive.Tar.Types", "Codec.Archive.Tar.Unpack", "Codec.Archive.Tar.Write",
    -- preinstalled in lab: package 'text'
        "Data.Text.Array", "Data.Text.Encoding.Error", "Data.Text.Encoding", "Data.Text.Foreign", "Data.Text", "Data.Text.Internal.Builder.Functions", "Data.Text.Internal.Builder", "Data.Text.Internal.Builder.Int.Digits", "Data.Text.Internal.Builder.RealFloat.Functions", "Data.Text.Internal.Encoding.Fusion.Common", "Data.Text.Internal.Encoding.Fusion", "Data.Text.Internal.Encoding.Utf16", "Data.Text.Internal.Encoding.Utf32", "Data.Text.Internal.Encoding.Utf8", "Data.Text.Internal.Functions", "Data.Text.Internal.Fusion.CaseMapping", "Data.Text.Internal.Fusion.Common", "Data.Text.Internal.Fusion", "Data.Text.Internal.Fusion.Size", "Data.Text.Internal.Fusion.Types", "Data.Text.Internal", "Data.Text.Internal.IO", "Data.Text.Internal.Lazy.Encoding.Fusion", "Data.Text.Internal.Lazy.Fusion", "Data.Text.Internal.Lazy", "Data.Text.Internal.Lazy.Search", "Data.Text.Internal.Private", "Data.Text.Internal.Read", "Data.Text.Internal.Search", "Data.Text.Internal.Unsafe.Char", "Data.Text.Internal.Unsafe", "Data.Text.Internal.Unsafe.Shift", "Data.Text.IO", "Data.Text.Lazy.Builder", "Data.Text.Lazy.Builder.Int", "Data.Text.Lazy.Builder.RealFloat", "Data.Text.Lazy.Encoding", "Data.Text.Lazy", "Data.Text.Lazy.Internal", "Data.Text.Lazy.IO", "Data.Text.Lazy.Read", "Data.Text.Read", "Data.Text.Show", "Data.Text.Unsafe",
    -- preinstalled in lab: package 'tf-random'
        "System.Random.TF.Gen", "System.Random.TF", "System.Random.TF.Init", "System.Random.TF.Instances",
    -- preinstalled in lab: package 'transformers-compat'
        "Control.Monad.Trans.Instances", "Paths_transformers_compat",
    -- preinstalled in lab: package 'zlib'
        "Codec.Compression.GZip", "Codec.Compression.Zlib", "Codec.Compression.Zlib.Internal", "Codec.Compression.Zlib.Raw", "Codec.Compression.Zlib.Stream"

  ]

-- Abort with an error message.
abortMsg :: String -> IO ()
abortMsg msg = do
    die ("ERROR: " ++ msg)

abort :: IO ()
abort = exitWith (ExitFailure 1)

-- Like abort, but signals that we are not positive that there is a problem.
abort_ambiguous :: IO ()
abort_ambiguous = exitWith (ExitFailure 2)


banner :: (?config::Config) => String
banner = unlines [
    "Presubmit test, v8.",
    "",
    "This script will look at your " ++ studentFilesFriendly ++" to see whether we can",
    "mark it in the current form. is. It will either give you an error, or it",
    "will say that you are ready for submission. We expect that most people",
    "will already be ready for submission; this presubmit script is meant to",
    "catch out any things you might have done that we didn't foresee, that",
    "might cause it to be unmarkable.",
    "",
    "If it's unclear why you're getting an error, check the Facebook group, ask, or",
    "if you need to show your source code, send an email to Bram on bram@bram.xyz.",
    "",
    "Please always use the latest version of this script, which you can obtain",
    "by running 'git pull'.",

    ""
  ]

-- Run cmd with system, assert that it returned status 0. If not, abort the
-- presubmit with a failure.
--
-- In this presubmit, we only need to run static strings as commands, so
-- there is no danger of any funny business with the shell parsing a
-- generated command in a funny way. (viz. "command injection")
--
-- If `blame` is Intl, then this command is supposed to succeed regardless
-- of the student's actions. If `blame` is (Extl problem hint), then we
-- expect that the student can fix this problem themselves.

data Blame = Intl | Extl [String] [String]

assertCmd :: (?config::Config) => String -> Blame -> IO ()
assertCmd cmd blame = do
    when verbose $ do
        putStrLn $ "+ " ++ cmd

    exitCode <- system cmd
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure n -> do
            putStrLn ""
            printBlame blame cmd n
            abort

-- Run a command silently, returning its output, giving nothing on its
-- standard input. Assert that the command succeeded. Throw away the standard
-- error stream.
--
-- The command will be parsed using the shell, as with system.
runCmdSilentlyForOut :: String -> Blame -> IO String
runCmdSilentlyForOut cmd blame = do
    (exitCode, out, err) <- readCreateProcessWithExitCode (shell cmd) ""
    case exitCode of
        ExitSuccess -> return out
        ExitFailure n -> do
            putStrLn ""
            printBlame blame cmd n
            abort
            return undefined

runCmdForOut :: (?config::Config) => String -> Blame -> IO String
runCmdForOut cmd blame = do
    when verbose $ do
        putStrLn $ "+ " ++ cmd

    runCmdSilentlyForOut cmd blame



printBlame Intl cmd n = do
    putStrLn $ "INTERNAL ERROR: subcommand '" ++ cmd ++ "' failed"
    putStrLn $ "with exit code "++ show n ++ ". We could not verify that your solution is"
    putStrLn $ "ready for submission. Please inform the teaching assistants or Martin and"
    putStrLn $ "we will get it fixed. If these people are not around, then please discuss"
    putStrLn $ "on Facebook. Thanks."

printBlame (Extl problem hint) cmd n = do
    putStrLn $ "The presubmit failed on the last command. We could not verify that your solution"
    putStrLn $ "is ready for submission."
    putStrLn $ ""
    when (problem /= []) $ do
        putStrLn $ "Problem:"
        putStrLn $ ""
        printIndentedLines "    " problem
        putStrLn $ ""
    when (hint /= []) $ do
        putStrLn $ "The following action may improve the situation:"
        putStrLn $ ""
        printIndentedLines "    " hint

step_prepare :: (?config::Config) => IO ()
step_prepare = do
    assertCmd "rm -rf presubmit-temp" Intl
    assertCmd "mkdir presubmit-temp" Intl
    forM_ studentMods $ \studentMod -> do
        let studentFile = studentMod ++ ".hs"
        let studentTemplate = studentMod ++ "Template.hs"
        assertCmd ("test -e " ++ studentFile)
            (Extl
                [studentFile ++ " does not exist"]
                [printf "You should copy %s to %s and work in that." studentTemplate studentFile])
        assertCmd ("test -f " ++ studentFile)
            (Extl [studentFile ++ " not a regular file"] [])
        assertCmd (printf "cp %s presubmit-temp/" studentFile) Intl

type RemoteName = String
-- (Just remoteName) means we found a remote that we can take files from.
-- Nothing           means we're using files from the working directory.
type GitPurity = Maybe RemoteName

-- Make a list of remote names that might correspond to the official repository.
--
-- This will first include 'origin' and 'mhe' if they exist, but also all
-- other current remotes. All returned strings will be Git remotes.
--
-- Assumes that we are currently in a Git checkout.
git_remotes_prioritised :: IO [RemoteName]
git_remotes_prioritised = do
    outStr <- runCmdSilentlyForOut "git remote" Intl
    let allR = lines outStr -- all remotes
    return $ filter (`elem` allR) static ++ (allR \\ static)
  where
    static = ["origin", "mhe"]

-- Attempt to find an official Git remote for the teaching staff fp-learning
-- repository.
find_git_remote :: (?config::Config) => IO (Maybe RemoteName)
find_git_remote = do
    candidates <- git_remotes_prioritised
    found <- tryCandidates candidates
    if found == Nothing && "mhe" `elem` candidates
    then do
        abortMsg "Git remote 'mhe' has the wrong URL set. Run 'git remote remove mhe'. Aborting. Could not verify that your submission is ready for submission."
        return undefined
    else return found
  where
    tryCandidates [] = return Nothing
    tryCandidates (x:xs) = do
        url <- git_remote_url x
        if url `elem` allowedGitOrigins
        then return (Just x)
        else tryCandidates xs

git_remote_url :: (?config::Config) => String -> IO String
git_remote_url remote = strip <$> runCmdForOut cmd Intl
  where
    cmd = printf "git config --get remote.%s.url" remote

-- Check if we are currently in a Git repository that a remote that is
-- downloading from the official repository.
--
-- If so, then we fetch all auxiliary files from Git.
--
-- If not, then we show a warning and we mark the global state as tainted.
step_git_purity :: (?config::Config) => IO GitPurity
step_git_purity = do
    -- Test if we are in a Git repository
    exit1 <- system "git remote > /dev/null"
    if exit1 /= ExitSuccess
      -- Apparently not
    then do
        notPure_clone "The current directory is not a Git checkout."
    else do
        -- See if we have a good remote
        mremote <- find_git_remote
        case mremote of
          Nothing -> notPure_addRemote
          Just rem -> do
            assertCmd (printf "git rev-parse --verify -q remotes/%s/master > /dev/null" rem)
                (Extl [
                    printf "You have the right '%s' remote set up to synchronise with" rem,
                    "the official repository, but it is not synchronised yet."
                  ] [
                    "Run the following command:   git fetch --all"
                  ]
                )

            -- Does the current path contain the auxiliary files, in the current directory?
            --
            -- For each auxiliary file, try to find it in the last commit in remotes/remote/master
            exits3 <- mapM
                (\aux -> system $ printf "git rev-parse --verify -q remotes/%s/master:./%s >/dev/null" rem aux)
                auxiliaryFiles

            -- If so, then yes, we have a correct Git setup.
            if all (==ExitSuccess) exits3
            then do
                putStrLn $ printf "* Found remote that synchronises with official repo: '%s'" rem
                return (Just rem)
            else notPure_fileNotFound (map snd $ filter ((/=) ExitSuccess . fst) $ exits3 `zip` auxiliaryFiles)
  where
    notPure_clone msg = do
        printIndentedLines "! " [
            "Warning: " ++ msg,
            "Continuing, taking auxiliary files from current directory.",
            "",
            "Suggested fix: Make a fresh clone of the fp-learning repository, move your",
            printf "%s to there, and re-run the presubmit check there." studentFilesFriendly,
            ""
          ]
        return Nothing

    notPure_addRemote = do
        printIndentedLines "! " [
            "Warning: your local git repository is not synchronising with the official",
            "repository. Continuing, taking auxiliary files from current directory.",
            "",
            "Suggested fix: run the following two commands:",
            "",
            "1.  git remote add mhe git@git.cs.bham.ac.uk:mhe/fp-learning-2017-2018.git",
            "    (or: git remote add mhe https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018.git)",
            "",
            "2.  git fetch --all",
            ""
          ]
        return Nothing

    notPure_fileNotFound files = do
        printIndentedLines "! " [
            printf "Warning: could not find required files %s" $ show files,
            "in teaching repository. Continuing, taking auxiliary files from current directory.",
            "",
            "Suggested fix: ",
            "",
            "1.  Try running:    git fetch --all",
            "",
            "2.  If that doesn't solve this message, then you are probably not in",
            "    the right directory. Make a fresh clone of the fp-learning repository,",
            printf "    move your %s to there, and re-run the presubmit check there." studentFilesFriendly,
            ""
          ]
        return Nothing


-- Check modules used by student files against whitelist.
step_modules :: (?config::Config) => String -> StateIO ()
step_modules studentFile = do
    contents <- lift $ readFile studentFile
    let modulesInFile = parseModules contents :: [String]
    let notOk = filter (\mod -> not $ elem mod moduleWhitelist) modulesInFile
    let plural a b | length notOk >= 2 = b
        plural a b | otherwise         = a
    if notOk == []
    then do
        lift $ putStrLn $ printf "* All %d imports found in %s are in whitelist, good." (length modulesInFile) studentFile
    else do
        lift $ printIndentedLines "! " [
            printf "Error: found import%s that %s not in our whitelist:" (plural "" "s") (plural "is" "are"),
            "    " ++ show notOk,
            "",
            "If these are basic modules, then you can ask Martin or the",
            "teaching assistants if we will allow them.",
            ""
          ]
        markTainted

-- Given a file's contents, make a decent effort to parse the module names
-- that are imported from that file.
parseModules :: String -> [String]
parseModules contents = do
    line1 <- lines contents
    let line2 = strip line1
    -- Filter lines that start "import ", and strip that off
    line3 <- maybeToList $ stripPrefix "import " line2
    let line4 = strip line3
    -- Strip off "safe ", but don't filter here
    let line5 = line4 `fromMaybe` stripPrefix "safe " line4
    let line6 = strip line5
    -- Strip off "qualified ", but don't filter here
    let line7 = line6 `fromMaybe` stripPrefix "qualified " line6
    let line8 = strip line7
    return $ takeWhile isModuleChar line8
  where
    isModuleChar c = isAlphaNum c || c == '.'

-- Copy auxiliary files in preparation of compiling/linking.
step_prepare_link :: (?config::Config) => GitPurity -> IO ()
step_prepare_link gitPurity = mapM_ (step_prepare_link_file gitPurity) auxiliaryFiles

step_prepare_link_file :: (?config::Config) => GitPurity -> String -> IO ()
step_prepare_link_file (Just rem) fileName = do
    assertCmd (printf "git cat-file blob remotes/%s/master:./%s > presubmit-temp/%s" rem fileName fileName)
        Intl
    officialVersion <- readFile $ "presubmit-temp/" ++ fileName
    dfe <- doesFileExist fileName
    if dfe then do
        workingVersion <- readFile fileName
        when (norm officialVersion /= norm workingVersion) $ do
            warnVersionDiscrepancy
    else do
        printIndentedLines "- " [
            printf "Warning: you seem to be missing file %s." fileName,
            printf "To restore it, run:   git checkout remotes/%s/master -- %s " rem fileName
          ]
  where
    -- Normalise file contents by removing trailing whitespace and empty
    -- lines.
    norm = unlines . filter (/="") . map stripRightComm . lines
    warnVersionDiscrepancy = printIndentedLines "- " [
        printf "Warning: your version of %s seems to be different from the official" fileName,
        "version I found. This is no problem for the presubmit, but this might",
        "cause some discrepancies compared to when you test yourself.",
        "",
        "By running the command",
        printf "    git checkout remotes/%s/master -- %s" rem fileName,
        "you can restore it to the official version I found. Watch out: this",
        "immediately undoes all changes you may have made yourself."
      ]

step_prepare_link_file Nothing fileName = do
    assertCmd (printf "test -f %s" fileName)
        (Extl
            [printf "File %s not found." fileName]
            ["Restore it to its original version, or make a fresh git clone."])
    assertCmd (printf "cp %s presubmit-temp/%s" fileName fileName)
        Intl

step_link :: (?config::Config) => StateIO ()
step_link = do
    taint <- get
    forM_ pretestMods $ \pretestMod -> do
        lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe " ++ pretestMod ++ ".hs") (blameForPretest taint)
    forM_ studentMods $ \studentMod -> do
        let studentFile = studentMod ++ ".hs"
        lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe " ++ studentMod)
            (Extl
                [printf "I put your %s in a new directory together with" studentFile,
                 (intercalate "," auxiliaryFiles) ++ ", but when I",
                 printf "then let GHC compile %s in safe mode, it fails." studentFile
                 ]
                [] -- there is no fix we can suggest
                )
    lift $ assertCmd ("cd presubmit-temp && rm -f *.hi *.o && ghc -XSafe Mould")
        (Extl
            [printf "I put your %s in a new directory together with" studentFilesFriendly,
             (intercalate "," auxiliaryFiles) ++ ", but when I",
             printf "then let GHC compile Mould.hs in safe mode, it fails."
             ]
            [printf "Check that in your %s the types of all questions are exactly" studentFilesFriendly,
             "as set in the assignment description. If there are data type",
             printf "definitions in %s, then check that you import" pretestFilesFriendly,
             "those data types rather than copy their definitions."
            ])
  where
    blameForPretest Untainted = Intl
    blameForPretest _ = Extl [printf "Could not compile the %s I found in your directory." pretestFilesFriendly] []

step_check_lab :: (?config::Config) => StateIO ()
step_check_lab = do
    when (not $ confInLab ?config) $ do
        lift $ printIndentedLines "! " [
            "Error: Presubmit script is not running in the lab. Please run",
            "this script in the lab. To do so, you can ssh into tinky-winky,",
            "then use the command ssh-lab to connect to a lab machine.",
            "You can copy files across with scp:",
            printf "    scp %s abc123@tinky-winky.cs.bham.ac.uk:work/"
                (intercalate " " studentFiles),
            "to copy your file to ~/work/ on the school servers."
          ]
        markAmbiguous
step_result :: (?config::Config) => StateIO ()
step_result = do
    taint <- get
    if taint /= Untainted
    then do
        lift.putStrLn $ ""
        lift.putStrLn $ printf "Unfortunately, we could not verify that your %s %s"
            studentFilesFriendly
            (if (length studentFiles == 1) then "is" else "are")
        lift.putStrLn $ "ready for submission. Please check the messages above."
        if taint == Ambiguous
        then lift abort_ambiguous
        else lift abort
    else do
        lift.putStrLn $ ""
        lift.putStrLn $ "Conclusion: Everything looks okay! You should be fine to"
        lift.putStrLn $ printf "submit your %s." studentFilesFriendly

step_cleanup :: (?config::Config) => IO ()
step_cleanup = do
    assertCmd "rm -rf presubmit-temp" Intl


main :: IO ()
main = do
    -- Fetch environment variables
    sanity <- lookupEnv "PRESUBMIT"
    when (sanity == Nothing) $ do
        abortMsg "The presubmit script must be run by typing \"sh presubmit.sh\"."
    stuMods <- getEnvAssertList "PRESUBMIT_STUDENT_MODS"
    verb <- getEnv "PRESUBMIT_VERBOSE"
    inLab <- getEnv "PRESUBMIT_IN_LAB"
    aux <- getEnvAssertList "PRESUBMIT_AUXILIARY_FILES_COPY"
    pretest <- getEnvAssertList "PRESUBMIT_AUXILIARY_PRETEST_MODS"
    white <- getEnvAssertList "PRESUBMIT_IMPORT_WHITELIST_CUSTOM"

    -- Create config, run main'
    let ?config=Config {
        confVerbose=parseVerb verb,
        confStudentMods=parseCommaList stuMods,
        confInLab=parseInLab inLab,
        confAuxiliary=parseCommaList aux,
        confPretest=parseCommaList pretest,
        confWhitelist=parseCommaList white
      }
    runStateT main' Untainted
    return ()
  where
    parseVerb "1" = True
    parseVerb "0" = False
    parseInLab "yes" = True
    parseInLab "no" = False

    parseCommaList :: String -> [String]
    parseCommaList "" = []
    parseCommaList (',':cs) = "" : parseCommaList cs
    parseCommaList (c:[]) = [[c]]
    parseCommaList (c:cs) = (c:x) : xs
      where
        (x:xs) = parseCommaList cs

    alphanumDotComma = (",." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    listOfAlphanumDot = all (`elem` alphanumDotComma)

    getEnvAssertList envVar = do
        string <- getEnv envVar
        when (not $ listOfAlphanumDot string) $ do
            error $ printf "Environment variable %s (value %s) must be composed only of characters %s"
                envVar
                (show string)
                (show alphanumDotComma)
        return string

mainExample :: IO ()
mainExample = do
    let ?config=Config {
        confVerbose=True,
        confStudentMods=["Formative1"],
        confInLab=True,
        confAuxiliary=["Lib.hs", "Mould.hs"],
        confPretest=["Lib"],
        confWhitelist=["Lib"]
      }
    runStateT main' Untainted
    return ()

main' :: (?config::Config) => StateIO ()
main' = do
    lift $ putStrLn banner
    lift $ step_prepare
    gitPurity <- lift $ step_git_purity
    when (gitPurity == Nothing) $ do
        markAmbiguous
    forM_ studentFiles step_modules
    lift $ step_prepare_link gitPurity
    step_link
    step_check_lab
    step_result
    lift $ step_cleanup



-- UTILITY CODE

-- Print each string in the string list on its own line, indented by (length
-- prefix) spaces. The first line is indented by prefix, rather than purely by
-- spaces.
printIndentedLines :: String -> [String] -> IO ()
printIndentedLines prefix lines = do
    putStrLn $ prefix ++ head lines
    let prefix2 = replicate (length prefix) ' '
    mapM_ (\line -> putStrLn $ prefix2 ++ line) (tail lines)

-- Strip leading and trailing spaces from a string.
strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Strip trailing spaces and inline comments.
stripRightComm :: String -> String
stripRightComm = stripComment . reverse . dropWhile isSpace . reverse

stripComment "" = ""
stripComment xs | " --" `isPrefixOf` xs = ""
stripComment (x:xs) = x : stripComment xs
