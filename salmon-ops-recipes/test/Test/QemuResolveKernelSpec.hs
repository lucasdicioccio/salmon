{- | Layer 1 (pure filesystem, no root/VM needed): exercises
'Salmon.Builtin.Nodes.Qemu.resolveKernelInitrd''s three cases —
exactly one match, none, and more than one — against a scratch
@boot/@ directory built with 'System.IO.Temp.withSystemTempDirectory'.

Closes @specs/qemu-test-vms-progress.md@ §3 item 3: the prefix-match
assumption was previously only reasoned about from code review, unverified
against a rootfs holding a held-over old kernel (two @vmlinuz-*@\/
@initrd.img-*@ pairs). This confirms 'resolveKernelInitrd' throws a
caller-visible "ambiguous" error rather than silently picking one, and does
so without needing an actual multi-kernel debootstrap chroot.
-}
module Test.QemuResolveKernelSpec (tests) where

import Control.Exception (try)
import Data.List (isInfixOf)
import Salmon.Builtin.Nodes.Qemu (resolveKernelInitrd)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), withFile)
import System.IO.Error (isUserError)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

tests :: TestTree
tests =
    testGroup
        "Qemu.resolveKernelInitrd"
        [ testCase "resolves the single vmlinuz/initrd pair" resolvesSinglePair
        , testCase "throws when no kernel is present" (throwsContaining [] "no vmlinuz-")
        , testCase
            "throws when a held-over old kernel makes the match ambiguous"
            (throwsContaining ["vmlinuz-6.1.0-amd64", "initrd.img-6.1.0-amd64", "vmlinuz-5.10.0-amd64", "initrd.img-5.10.0-amd64"] "ambiguous vmlinuz-")
        ]

touch :: FilePath -> IO ()
touch path = withFile path WriteMode (const (pure ()))

withBoot :: [FilePath] -> (FilePath -> IO a) -> IO a
withBoot bootFiles act =
    withSystemTempDirectory "resolveKernelInitrd-spec" $ \rootfs -> do
        let bootDir = rootfs </> "boot"
        createDirectoryIfMissing True bootDir
        mapM_ (touch . (bootDir </>)) bootFiles
        act rootfs

resolvesSinglePair :: IO ()
resolvesSinglePair = withBoot ["vmlinuz-6.1.0-amd64", "initrd.img-6.1.0-amd64", "System.map-6.1.0-amd64"] $ \rootfs -> do
    (kernel, initrd) <- resolveKernelInitrd rootfs
    kernel @?= rootfs </> "boot" </> "vmlinuz-6.1.0-amd64"
    initrd @?= rootfs </> "boot" </> "initrd.img-6.1.0-amd64"

-- | Asserts 'resolveKernelInitrd' throws a 'userError' whose message
-- contains @needle@, given a @boot/@ populated with @bootFiles@.
throwsContaining :: [FilePath] -> String -> IO ()
throwsContaining bootFiles needle = withBoot bootFiles $ \rootfs -> do
    result <- try (resolveKernelInitrd rootfs)
    case result of
        Left e | isUserError e -> assertBool ("expected error containing " <> show needle <> ", got: " <> show e) (needle `isInfixOf` show e)
        Left e -> assertFailure ("expected a userError, got: " <> show e)
        Right r -> assertFailure ("expected resolveKernelInitrd to throw, got: " <> show r)
