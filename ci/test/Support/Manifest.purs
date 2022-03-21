module Test.Support.Manifest where

import Registry.Prelude

import Data.Array.NonEmpty as NEA
import Data.Map as Map
import Data.Newtype (unwrap)
import Foreign.SPDX as SPDX
import Registry.PackageName (PackageName)
import Registry.PackageName as PackageName
import Registry.Schema (Location(..), Manifest(..), Owner(..))
import Registry.Schema as Schema
import Registry.Version (ParseMode(..))
import Registry.Version as Version

ab ::
  { name :: PackageName
  , v1a :: Manifest
  , v1b :: Manifest
  , v2 :: Manifest
  }
ab = { name, v1a, v1b, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "ab"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies = Map.empty
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  license = unsafeFromRight $ SPDX.parse "MIT"
  locationWrong = Schema.GitHub
    { owner: "ab-wrong-user"
    , repo: "ab"
    , subdir: Nothing
    }
  location = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  description = Just "some description"
  v1a = Manifest { name, owners: Nothing, version: version1, license, location: locationWrong, dependencies, description, files: Nothing }
  v1b = Manifest { name, owners: Nothing, version: version1, license, location, dependencies, description, files: Nothing }
  v2 = Manifest { name, owners: Nothing, version: version2, license, location, dependencies, description, files: Nothing }

abc :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abc = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abc"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies1 = Map.singleton (unsafeFromRight (PackageName.parse "ab")) (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  dependencies2 = Map.singleton (unsafeFromRight (PackageName.parse "ab")) (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
  license = unsafeFromRight $ SPDX.parse "MIT"
  location = Schema.GitHub
    { owner: "abc-user"
    , repo: "abc"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, owners: Nothing, version: version1, license, location, dependencies: dependencies1, description, files: Nothing }
  v2 = Manifest { name, owners: Nothing, version: version2, license, location, dependencies: dependencies2, description, files: Nothing }

abcd :: { name :: PackageName, v1 :: Manifest, v2 :: Manifest }
abcd = { name, v1, v2 }
  where
  name = unsafeFromRight $ PackageName.parse "abcd"
  version1 = unsafeFromRight $ Version.parseVersion Strict "1.0.0"
  dependencies1 = Map.singleton (unsafeFromRight (PackageName.parse "abc")) (unsafeFromRight (Version.parseRange Strict ">=1.0.0 <2.0.0"))
  version2 = unsafeFromRight $ Version.parseVersion Strict "2.0.0"
  dependencies2 = Map.singleton (unsafeFromRight (PackageName.parse "abc")) (unsafeFromRight (Version.parseRange Strict ">=2.0.0 <3.0.0"))
  license = unsafeFromRight $ SPDX.parse "MIT"
  location = Schema.GitHub
    { owner: "abcd-user"
    , repo: "abcd"
    , subdir: Nothing
    }
  description = Just "some description"
  v1 = Manifest { name, owners: Nothing, version: version1, license, location, dependencies: dependencies1, description, files: Nothing }
  v2 = Manifest { name, owners: Nothing, version: version2, license, location, dependencies: dependencies2, description, files: Nothing }

manifest :: Manifest
manifest = Manifest
  { name: unsafeFromRight $ PackageName.parse "fixture-package-name"
  , owners: NEA.fromArray [ Owner { email: "fixture-email", keytype: "RSA", public: "fixture-public-key" } ]
  , version: unsafeFromRight $ Version.parseVersion Version.Strict "0.10.0"
  , license: unsafeFromRight $ SPDX.parse "MIT"
  , location: GitHub { subdir: Nothing, owner: "fixture-repo-owner", repo: "purescript-fixture-package-name" }
  , description: Nothing
  , files: Nothing
  , dependencies: Map.empty
  }

manifestWithDependency :: Manifest
manifestWithDependency = Manifest
  { name: unsafeFromRight $ PackageName.parse "fixture-with-dependencies-package-name"
  , owners: NEA.fromArray [ Owner { email: "fixture-email", keytype: "RSA", public: "fixture-public-key" } ]
  , version: unsafeFromRight $ Version.parseVersion Version.Strict "0.10.0"
  , license: unsafeFromRight $ SPDX.parse "MIT"
  , location: GitHub { subdir: Nothing, owner: "fixture-repo-owner", repo: "purescript-fixture-with-dependencies-package-name" }
  , description: Nothing
  , files: Nothing
  , dependencies: Map.fromFoldable [ Tuple (unwrap manifest).name (unsafeFromRight $ Version.parseRange Version.Strict ">=0.10.0 <2.0.0") ]
  }
