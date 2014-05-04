{-# LANGUAGE DeriveDataTypeable #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  $Header$
-- Copyright   :  © 2013-2014 Nicola Squartini
-- License     :  BSD3
--
-- Maintainer  :  Nicola Squartini <tensor5@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- High-level bindings to to the @libkmod@ library for manipulating Linux kernel
-- modules.
--
--------------------------------------------------------------------------------

module System.Linux.KMod
    (
    -- * Context
      Context
    , new
    , loadResources
    , unloadResources
    , Resources(..)
    , validateResources
    , Index(..)
    , dumpIndex
    , setLogPriority
    , getLogPriority
    -- * Get configuration
    , Key
    , Value
    , configGetBlacklists
    , configGetInstallCommands
    , configGetRemoveCommands
    , configGetAliases
    , configGetOptions
    , configGetSoftdeps
    -- * Modules
    , Module
    , moduleNewFromLookup
    , Filter(..)
    , moduleNewFromLookupWithFilter
    , moduleNewFromName
    , moduleNewFromPath
    , Options
    , InsertFlags(..)
    , moduleInsertModule
    , ProbeFlags(..)
    , Blacklist(..)
    , RunInstall
    , PrintAction
    , BlacklistError(..)
    , moduleProbeInsertModule
    , RemoveFlags(..)
    , moduleRemoveModule
    , moduleGetInstallCommands
    , moduleGetRemoveCommands
    , moduleGetOptions
    , moduleGetPath
    , moduleGetDependencies
    , moduleGetSoftdeps
    , Symbol
    , Bind(..)
    , CRC
    , moduleGetDependencySymbols
    , Name
    , Address
    , moduleGetSections
    , moduleGetSymbols
    , moduleGetVersions
    , moduleGetInfo
    -- * Loaded modules
    , moduleNewFromLoaded
    , Initstate(..)
    , moduleGetInitstate
    , moduleGetSize
    , moduleGetRefcnt
    , moduleGetHolders
   ) where

import           Control.Exception  (Exception, bracket, throw)
import           Control.Monad      (unless, (>=>))
import           Data.Functor       ((<$>))
import           Data.Typeable
import           Foreign            hiding (new)
import           Foreign.C
import           System.Directory   (getDirectoryContents)
import           System.Posix.Types (Fd)

#include <libkmod.h>

-- | Represent the key in a (key,value) pair.
type Key = String

-- | Represent the value in a (key,value) pair.
type Value = String

-- | Opaque object representing the library context.
newtype Context = Context (ForeignPtr Context)

withContext :: Context -> (Ptr Context -> IO a) -> IO a
withContext (Context fptr) = withForeignPtr fptr

foreign import ccall "&kmod_unref"
  p_unref :: FunPtr (Ptr Context -> IO (Ptr Context))

toContext :: Ptr Context -> IO Context
toContext = fmap Context . newForeignPtr (castFunPtr p_unref)


foreign import ccall "kmod_new" kmod_new :: CString
                                         -> Ptr CString
                                         -> IO (Ptr Context)

-- | Create a @kmod@ @'Context'@ from configuration files.
new :: Maybe FilePath   -- ^ Linux module's directory, or @'Nothing'@ for the
                        -- default (@\/lib\/modules\/`uname -r`@)
    -> Maybe [FilePath] -- ^ List of paths (directories or files) for
                        -- configuration files, or @'Nothing'@ for the default
                        -- (@\/run\/modprobe.d@, @\/etc\/modprobe.d@ and
                        -- @\/lib\/modprobe.d@)
    -> IO Context
new mdir mpaths = throwIfNull "kmod_new returned NULL"
                  (maybeWith withCString mdir $ \d ->
                       maybeWith withCStringArray mpaths $ \p ->
                           kmod_new d p)
                  >>= toContext

withCStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray xs f = bracket
                        (mapM newCString xs)
                        (mapM_ free)
                        (\ps -> withArray0 nullPtr ps f)


foreign import ccall "kmod_load_resources" load_resources :: Ptr Context
                                                          -> IO CInt
-- | Load indexes and keep them open in @'Context'@.
loadResources :: Context -> IO ()
loadResources ctx =
    throwIf_ (/= 0) (\e -> "kmod_load_resources returned " ++ show e) $
    withContext ctx load_resources

foreign import ccall "kmod_unload_resources" unload_resources :: Ptr Context
                                                              -> IO ()
-- | Unload all the indexes.
unloadResources :: Context -> IO ()
unloadResources ctx = withContext ctx unload_resources

-- | Represent the return values of @'validateResources'@.
data Resources = Ok
               | MustReload
               | MustRecreate
                 deriving (Eq, Show)

toResources :: (Eq a, Num a, Show a) => a -> Resources
toResources #{const KMOD_RESOURCES_OK} = Ok
toResources #{const KMOD_RESOURCES_MUST_RELOAD} = MustReload
toResources #{const KMOD_RESOURCES_MUST_RECREATE} = MustRecreate
toResources n = error ("System.KMod.toResources: bad argument: " ++ show n)


foreign import ccall "kmod_validate_resources"
  validate_resources :: Ptr Context -> IO CInt

-- | Check if indexes and configuration files changed on disk and the
-- current context is not valid anymore.
validateResources :: Context -> IO Resources
validateResources ctx = toResources <$> withContext ctx validate_resources

-- | Represent an index file.
data Index = ModulesDep
           | ModulesAlias
           | ModulesSymbol
           | ModulesBuiltin
             deriving (Eq, Show)

fromIndex :: Num a => Index -> a
fromIndex ModulesDep = #{const KMOD_INDEX_MODULES_DEP}
fromIndex ModulesAlias = #{const KMOD_INDEX_MODULES_ALIAS}
fromIndex ModulesSymbol = #{const KMOD_INDEX_MODULES_SYMBOL}
fromIndex ModulesBuiltin = #{const KMOD_INDEX_MODULES_BUILTIN}


foreign import ccall "kmod_dump_index"
  dump_index :: Ptr Context -> CInt -> CInt -> IO CInt

-- | Dump @'Index'@ to file descriptor.
dumpIndex :: Context -> Index -> Fd -> IO ()
dumpIndex ctx i fd =
    withContext ctx $ \p ->
        throwIf_ (/= 0) (\e -> "kmod_dump_index returned " ++ show e) $
        dump_index p (fromIndex i) (fromIntegral fd)

foreign import ccall "kmod_set_log_priority"
  set_log_priority :: Ptr Context -> CInt -> IO ()

-- | Set the current logging priority. The value controls which messages are
-- logged.
setLogPriority :: Context -> Int -> IO ()
setLogPriority ctx i =
    withContext ctx (\p -> set_log_priority p (fromIntegral i))

foreign import ccall "kmod_get_log_priority"
  get_log_priority :: Ptr Context -> IO CInt

-- | Get the current logging priority.
getLogPriority :: Context -> IO Int
getLogPriority ctx =
    withContext ctx (fmap fromIntegral . get_log_priority)

data ConfigIter

foreign import ccall "kmod_config_iter_get_key"
  config_iter_get_key :: Ptr ConfigIter -> IO CString

foreign import ccall "kmod_config_iter_get_value"
  config_iter_get_value :: Ptr ConfigIter -> IO CString

foreign import ccall unsafe "kmod_config_iter_next"
  config_iter_next :: Ptr ConfigIter -> IO Bool

foreign import ccall unsafe "kmod_config_iter_free_iter"
  config_iter_free_iter :: Ptr ConfigIter -> IO ()


configIter2List :: (Ptr ConfigIter -> IO a)
                -> Ptr ConfigIter
                -> IO [a]
configIter2List fget p =
  if p == nullPtr
    then throwErrno "configIter2List"
    else do b <- config_iter_next p
            if b
              then do x <- fget p
                      xs <- configIter2List fget p
                      return (x:xs)
              else do config_iter_free_iter p
                      return []


configIter2KeyValueList :: Ptr ConfigIter -> IO [(Key,Value)]
configIter2KeyValueList =
    configIter2List $ \p -> do key <- config_iter_get_key p >>= peekCString
                               val <- config_iter_get_value p >>= peekCString
                               return (key,val)

configIter2StringList :: Ptr ConfigIter -> IO [String]
configIter2StringList = configIter2List (config_iter_get_key >=> peekCString)

withContextAndWith :: (Ptr Context -> IO a) -> (a -> IO b) -> Context -> IO b
withContextAndWith f g m = withContext m f >>= g

foreign import ccall "kmod_config_get_blacklists"
  config_get_blacklists :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of blacklisted modules.
configGetBlacklists :: Context -> IO [String]
configGetBlacklists =
    withContextAndWith config_get_blacklists configIter2StringList

foreign import ccall "kmod_config_get_install_commands"
  config_get_install_commands :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of modules and corresponding install commands.
configGetInstallCommands :: Context -> IO [(Key,Value)]
configGetInstallCommands =
    withContextAndWith config_get_install_commands configIter2KeyValueList

foreign import ccall "kmod_config_get_remove_commands"
  config_get_remove_commands :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of modules and corresponding remove commands.
configGetRemoveCommands :: Context -> IO [(Key,Value)]
configGetRemoveCommands =
    withContextAndWith config_get_remove_commands configIter2KeyValueList

foreign import ccall "kmod_config_get_aliases"
  config_get_aliases :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of modules and corresponding aliases.
configGetAliases :: Context -> IO [(Key,Value)]
configGetAliases = withContextAndWith config_get_aliases configIter2KeyValueList

foreign import ccall "kmod_config_get_options"
  config_get_options :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of modules and corresponding options.
configGetOptions :: Context -> IO [(Key,Value)]
configGetOptions = withContextAndWith config_get_options configIter2KeyValueList

foreign import ccall "kmod_config_get_softdeps"
  config_get_softdeps :: Ptr Context -> IO (Ptr ConfigIter)

-- | Get the list of modules and corresponding soft dependencies.
configGetSoftdeps :: Context -> IO [(Key,Value)]
configGetSoftdeps =
    withContextAndWith config_get_softdeps configIter2KeyValueList


-- | Opaque object representing a module. The @'Show'@ instance of @'Module'@ is
-- achieved using @kmod_module_get_name@.
data Module = Module (ForeignPtr Module) String

withModule :: Module -> (Ptr Module -> IO a) -> IO a
withModule (Module fptr _) = withForeignPtr fptr


foreign import ccall "&kmod_module_unref"
  p_module_unref :: FunPtr (Ptr Module -> IO (Ptr Module))

foreign import ccall "kmod_module_get_name"
  module_get_name :: Ptr Module -> IO CString

toModule :: Ptr Module -> IO Module
toModule ptr = do fptr <- newForeignPtr (castFunPtr p_module_unref) ptr
                  name <- module_get_name ptr >>= peekCString
                  return $ Module fptr name

instance Show Module where
    show (Module _ name) = name

foreign import ccall "kmod_module_new_from_lookup"
  module_new_from_lookup :: Ptr Context -> CString -> Ptr (Ptr List) -> IO CInt

fromLookup :: Context -> String -> IO (Ptr List)
fromLookup ctx str =
    withContext ctx $ \p ->
        withCString str $ \q ->
            alloca $ \r ->
                do poke r nullPtr
                   e <- module_new_from_lookup p q r
                   if e < 0
                     then fail ("kmod_module_new_from_lookup returned " ++
                                show e)
                     else peek r

-- | Create a new list of @'Module'@s using an alias or module name and lookup
-- libkmod's configuration files and indexes in order to find the module. Once
-- it's found in one of the places, it stops searching and create the list of
-- @'Module'@s.
moduleNewFromLookup :: Context -> String -> IO [Module]
moduleNewFromLookup ctx str = fromLookup ctx str >>= toModuleList

-- | Same as @'moduleNewFromLookup'@ but apply  filter the output list.
moduleNewFromLookupWithFilter :: Context -> String -> Filter -> IO [Module]
moduleNewFromLookupWithFilter ctx str fil = fromLookup ctx str >>=
                                            toModuleListWithFilter ctx fil

foreign import ccall "kmod_module_new_from_name"
  module_new_from_name :: Ptr Context -> CString -> Ptr (Ptr Module) -> IO CInt

foreign import ccall "kmod_module_new_from_path"
  module_new_from_path :: Ptr Context -> CString -> Ptr (Ptr Module) -> IO CInt


moduleNewFrom :: (Ptr Context -> CString -> Ptr (Ptr Module) -> IO CInt)
              -> String
              -> Context -> String -> IO Module
moduleNewFrom fun err ctx str =
    withContext ctx $ \p ->
        withCString str $ \q ->
            alloca $ \r ->
                do e <- fun p q r
                   if e == 0
                     then peek r >>= toModule
                     else fail (err ++ " returned " ++ show e)

-- | Create a new @'Module'@ using the module name, that can not be an alias,
-- file name or anything else; it must be a module name. There's no check if the
-- module exists in the system.
moduleNewFromName :: Context -> String -> IO Module
moduleNewFromName =
    moduleNewFrom module_new_from_name "kmod_module_new_from_name"

-- | Create a new @'Module'@ using the module path.
moduleNewFromPath :: Context -> String -> IO Module
moduleNewFromPath =
    moduleNewFrom module_new_from_path "kmod_module_new_from_path"

fromBitField :: (Bits b, Num b) => [(a -> Bool, b)] -> a -> b
fromBitField [] _ = 0
fromBitField (x:xs) a = (if fst x a then snd x else 0) .|. fromBitField xs a

-- | Flags for @'moduleInsertModule'@.
data InsertFlags = InsertFlags { insertForceVerMagic   :: Bool
                               , insertForceModVersion :: Bool
                               } deriving (Eq, Show)

fromInsertFlags :: (Bits a, Num a) => InsertFlags -> a
fromInsertFlags =
    fromBitField
    [ (insertForceVerMagic, #{const KMOD_INSERT_FORCE_VERMAGIC})
    , (insertForceModVersion, #{const KMOD_INSERT_FORCE_MODVERSION})
    ]


foreign import ccall "kmod_module_insert_module"
  module_insert_module :: Ptr Module -> CUInt -> CString -> IO CInt

-- | Options for module loading to pass to Linux Kernel.
type Options = String

-- | Insert a module in Linux kernel.
moduleInsertModule :: Module -> InsertFlags -> Options -> IO ()
moduleInsertModule m i o =
    withModule m $ \p ->
        withCString o $ \q ->
            throwErrnoIf_ (/=0) "kmod_module_insert_module" $
            module_insert_module p (fromInsertFlags i) q


-- | Flags for @'moduleProbeInsertModule'@.
data ProbeFlags = ProbeFlags { probeForceVerMagic   :: Bool
                             , probeForceModVersion :: Bool
                             , probeIgnoreCommand   :: Bool
                             , probeIgnoreLoaded    :: Bool
                             , probeDryRun          :: Bool
                             , probeFailOnLoaded    :: Bool
                             } deriving (Eq, Show)

fromProbeFlags :: (Bits a, Num a) => ProbeFlags -> a
fromProbeFlags =
    fromBitField [ (probeForceVerMagic, #{const KMOD_PROBE_FORCE_VERMAGIC})
                 , (probeForceModVersion, #{const KMOD_PROBE_FORCE_MODVERSION})
                 , (probeIgnoreCommand, #{const KMOD_PROBE_IGNORE_COMMAND})
                 , (probeIgnoreLoaded, #{const KMOD_PROBE_IGNORE_LOADED})
                 , (probeDryRun, #{const KMOD_PROBE_DRY_RUN})
                 , (probeFailOnLoaded, #{const KMOD_PROBE_FAIL_ON_LOADED})
                 ]

-- | @'Blacklist'@ filter for @'moduleProbeInsertModule'@ specifies how
-- blacklist configuration should be applied.
data Blacklist = BlacklistAll       -- ^ Apply blacklist configuration to the
                                    -- module and its dependencies
               | Blacklist          -- ^ Apply blacklist configuration to the
                                    -- module alone
               | BlacklistAliasOnly -- ^ Apply blacklist configuration to the
                                    -- module only if it is an alias
               | NoBlacklist        -- ^ Do not apply blacklist configuration
                 deriving (Eq, Show)

fromBlacklist :: Num a => Blacklist -> a
fromBlacklist BlacklistAll = #{const KMOD_PROBE_APPLY_BLACKLIST_ALL}
fromBlacklist Blacklist = #{const KMOD_PROBE_APPLY_BLACKLIST}
fromBlacklist BlacklistAliasOnly =
    #{const KMOD_PROBE_APPLY_BLACKLIST_ALIAS_ONLY}
fromBlacklist NoBlacklist = 0

foreign import ccall "kmod_module_probe_insert_module"
  module_probe_insert_module :: Ptr Module
                             -> CUInt
                             -> CString
                             -> FunPtr CRunInstall
                             -> Ptr ()
                             -> FunPtr CPrintAction
                             -> IO CInt

-- | Function to run a module install commands.
type RunInstall = Module  -- ^ Module being loaded
                -> String -- ^ Command line of the install executable
                -> IO ()

type CRunInstall = Ptr Module -> CString -> Ptr () -> IO CInt

foreign import ccall "wrapper" mkCRunInstallPtr :: CRunInstall
                                                -> IO (FunPtr CRunInstall)

withRunInstall :: Maybe RunInstall -> (FunPtr CRunInstall -> IO a) -> IO a
withRunInstall Nothing f = f nullFunPtr
withRunInstall (Just ri) f = bracket
                             (mkCRunInstallPtr toCRunInstall)
                             freeHaskellFunPtr
                             f
    where toCRunInstall p cs _ =
              do m <- module_ref p >>= toModule -- Need to increment ref, or
                                                -- segfault
                 s <- peekCString cs
                 ri m s
                 return 0

-- | @'Exception'@ @'throw'@n by @'moduleProbeInsertModule'@ if the module
-- cannot be inserted due to a @'Blacklist'@ setting.
data BlacklistError = BlacklistError
                      deriving (Eq, Show, Typeable)

instance Exception BlacklistError

-- | Insert a module in Linux kernel resolving dependencies, soft dependencies,
-- install commands and applying blacklist. If the module cannot be inserted due
-- to the @'Blacklist'@ filter, @'moduleProbeInsertModule'@ @'throw'@s a
-- @'BlacklistError'@ exception.
moduleProbeInsertModule :: Module            -- ^ Module to be loaded
                        -> ProbeFlags        -- ^ Flags
                        -> Blacklist         -- ^ How to handle blacklisted
                                             -- modules
                        -> Maybe Options     -- ^ Options to be passed to the
                                             -- module
                        -> Maybe RunInstall  -- ^ Function to execute module's
                                             -- install commands, if specified
                                             -- in is configuration (see
                                             -- @'moduleGetInstallCommands'@).
                                             -- If @'Nothing'@ then use
                                             -- @system(3)@
                        -> Maybe PrintAction -- ^ Function to print the action
                                             -- being taken on module loading
                        -> IO ()
moduleProbeInsertModule m flags bl opts ri pa =
    withModule m $ \p ->
        maybeWith withCString opts $ \cs ->
            withRunInstall ri $ \fptr1 ->
                withPrintAction pa $ \fptr2 ->
                    do e <- module_probe_insert_module p
                            (fromProbeFlags flags .|. fromBlacklist bl)
                            cs fptr1 nullPtr fptr2
                       unless (e == 0) $
                              if e < 0
                               then throwErrno "kmod_module_probe_insert_module"
                               else throw BlacklistError

-- | Function to print the actions being taken during the execution of
-- @'moduleProbeInsertModule'@.
type PrintAction = Module   -- ^ Module that is going to be loaded
                 -> Bool    -- ^ True if the module has install commands
                            -- specified in its configuration
                 -> Options -- ^ Options passed to the module
                 -> IO ()

type CBool = #{type bool}

type CPrintAction = Ptr Module -> CBool -> CString -> IO ()

foreign import ccall "wrapper" mkCPrintActionPtr :: CPrintAction
                                                 -> IO (FunPtr CPrintAction)

withPrintAction :: Maybe PrintAction -> (FunPtr CPrintAction -> IO a) -> IO a
withPrintAction Nothing f = f nullFunPtr
withPrintAction (Just pa) f = bracket (mkCPrintActionPtr toCPrintAction)
                              freeHaskellFunPtr
                              f
    where toCPrintAction p b cs =
              do m <- module_ref p >>= toModule -- Need to increment ref, or
                                                -- segfault
                 s <- peekCString cs
                 pa m (toBool b) s

foreign import ccall "kmod_module_ref" module_ref :: Ptr Module
                                                  -> IO (Ptr Module)

-- | Flags for @'moduleRemoveModule'@.
data RemoveFlags = RemoveFlags { removeForce  :: Bool
                               , removeNowait :: Bool
                               } deriving (Eq, Show)

fromRemoveFlags :: (Bits a, Num a) => RemoveFlags -> a
fromRemoveFlags = fromBitField [ (removeForce, #{const KMOD_REMOVE_FORCE})
                               , (removeNowait, #{const KMOD_REMOVE_NOWAIT})
                               ]

foreign import ccall "kmod_module_remove_module"
  module_remove_module :: Ptr Module -> CUInt -> IO CInt

-- | Remove a module from Linux kernel.
moduleRemoveModule :: Module -> RemoveFlags -> IO ()
moduleRemoveModule m r =
    withModule m $ \p ->
        throwErrnoIf_ (/=0) "kmod_module_remove_module" $
        module_remove_module p (fromRemoveFlags r)


withModuleAndWith :: (Ptr Module -> IO a) -> (a -> IO b) -> Module -> IO b
withModuleAndWith f g m = withModule m f >>= g

foreign import ccall "kmod_module_get_install_commands"
  module_get_install_commands :: Ptr Module -> IO CString

-- | Get install commands for this @'Module'@. Install commands come from the
-- configuration file and are cached in @'Module'@. The first call to this
-- function will search for this module in configuration and subsequent calls
-- return the cached string. The install commands are returned as they were in
-- the configuration, concatenated by ';'. No other processing is made in this
-- string.
moduleGetInstallCommands :: Module -> IO (Maybe String)
moduleGetInstallCommands =
    withModuleAndWith module_get_install_commands (maybePeek peekCString)

foreign import ccall "kmod_module_get_remove_commands"
  module_get_remove_commands :: Ptr Module -> IO CString

-- | Get remove commands for this @'Module'@. Remove commands come from the
-- configuration file and are cached in @'Module'@. The first call to this
-- function will search for this module in configuration and subsequent calls
-- return the cached string. The remove commands are returned as they were in
-- the configuration, concatenated by ';'. No other processing is made in this
-- string.
moduleGetRemoveCommands :: Module -> IO (Maybe String)
moduleGetRemoveCommands =
    withModuleAndWith module_get_remove_commands (maybePeek peekCString)

foreign import ccall "kmod_module_get_options"
  module_get_options :: Ptr Module -> IO CString

-- | Get options of this @'Module'@. Options come from the configuration file
-- and are cached in @'Module'@. The first call to this function will search for
-- this @'Module'@ in configuration and subsequent calls return the cached
-- string.
moduleGetOptions :: Module -> IO (Maybe String)
moduleGetOptions = withModuleAndWith module_get_options (maybePeek peekCString)

foreign import ccall "kmod_module_get_path"
  module_get_path :: Ptr Module -> IO CString

-- | Get the path of this @'Module'@. If this @'Module'@ was not created by
-- path, it can search the modules.dep index in order to find out the module
-- under context's dirname.
moduleGetPath :: Module -> IO (Maybe String)
moduleGetPath = withModuleAndWith module_get_path (maybePeek peekCString)




data List

foreign import ccall unsafe "kmod_list_last" list_last :: Ptr List
                                                       -> IO (Ptr List)

foreign import ccall unsafe "kmod_list_next" list_next :: Ptr List
                                                       -> Ptr List
                                                       -> IO (Ptr List)



toList :: (Ptr List -> IO a)  -- ^ Function to get the entry value
       -> (Ptr List -> IO ()) -- ^ Function to free the list
       -> Ptr List
       -> IO [a]
toList get fr p = do l <- list_last p
                     if l == nullPtr
                       then do fr p
                               return []
                       else genList p
    where genList q = do x <- get q
                         r <- list_next p q
                         if r == nullPtr
                           then do fr p
                                   return [x]
                           else do xs <- genList r
                                   return (x:xs)


foreign import ccall unsafe "kmod_module_get_module"
  module_get_module :: Ptr List
                    -> IO (Ptr Module)

foreign import ccall unsafe "kmod_module_unref_list"
  module_unref_list :: Ptr List -> IO CInt

toModuleList :: Ptr List -> IO [Module]
toModuleList = toList
               (module_get_module >=> toModule)
               (throwErrnoIf_ (/= 0)
                "kmod_module_unref_list returned non-zero exit status"
                . module_unref_list)

foreign import ccall "kmod_module_apply_filter"
  module_apply_filter :: Ptr Context
                      -> CInt
                      -> Ptr List
                      -> Ptr (Ptr List)
                      -> IO CInt

-- | Filter type for @'moduleNewFromLookupWithFilter'@.
data Filter = Filter { filterBlacklist :: Bool
                     , filterBuiltin   :: Bool
                     } deriving (Eq, Show)

fromFilter :: (Bits a, Num a) => Filter -> a
fromFilter = fromBitField [ (filterBlacklist, #{const KMOD_FILTER_BLACKLIST})
                          , (filterBuiltin, #{const KMOD_FILTER_BUILTIN})
                          ]

toModuleListWithFilter :: Context -> Filter -> Ptr List -> IO [Module]
toModuleListWithFilter c f l =
    withContext c $ \p ->
        alloca $ \q ->
            do e <- module_apply_filter p (fromFilter f) l q
               if e >= 0
                 then peek q >>= toModuleList
                 else fail ("kmod_module_apply_filter returned " ++ show e)

foreign import ccall "kmod_module_get_dependencies"
  module_get_dependencies :: Ptr Module -> IO (Ptr List)

-- | Search the modules.dep index to find the dependencies of the given
-- @'Module'@.
moduleGetDependencies :: Module -> IO [Module]
moduleGetDependencies m =
    withModule m (module_get_dependencies >=> toModuleList)

foreign import ccall "kmod_module_get_softdeps"
  module_get_softdeps :: Ptr Module
                      -> Ptr (Ptr List)
                      -> Ptr (Ptr List)
                      -> IO CInt

-- | Get soft dependencies for this @'Module'@.
moduleGetSoftdeps :: Module -> IO ([Module], [Module])
moduleGetSoftdeps m =
    withModule m $ \p ->
        alloca $ \pre ->
            do poke pre nullPtr
               alloca $ \post ->
                   do poke post nullPtr
                      e <- module_get_softdeps p pre post
                      if e >= 0
                        then do l1 <- peek pre >>= toModuleList
                                l2 <- peek post >>= toModuleList
                                return (l1,l2)
                        else fail ("kmod_module_get_softdeps returned " ++
                                   show e)

-- | Symbol bind type.
data Bind = None
          | Local
          | Global
          | Weak
          | Undefined
            deriving (Eq, Show)

toBind :: (Eq a, Num a, Show a) => a -> Bind
toBind #{const KMOD_SYMBOL_NONE} = None
toBind #{const KMOD_SYMBOL_LOCAL} = Local
toBind #{const KMOD_SYMBOL_GLOBAL} = Global
toBind #{const KMOD_SYMBOL_WEAK} = Weak
toBind #{const KMOD_SYMBOL_UNDEF} = Undefined
toBind n = error ("System.KMod.toBind: bad argument: " ++ show n)

-- | Crc of a symbol.
type CRC = #{type uint64_t}

-- | Name of a symbol.
type Symbol = String

moduleDependencySymbolGet :: Ptr List -> IO (Symbol,Bind,CRC)
moduleDependencySymbolGet p =
    do s <- module_dependency_symbol_get_symbol p >>= peekCString
       b <- module_dependency_symbol_get_bind p
       c <- module_dependency_symbol_get_crc p
       return (s,toBind b,c)

foreign import ccall "kmod_module_dependency_symbol_get_bind"
  module_dependency_symbol_get_bind :: Ptr List -> IO CInt

foreign import ccall "kmod_module_dependency_symbol_get_crc"
  module_dependency_symbol_get_crc :: Ptr List -> IO #{type uint64_t}

foreign import ccall "kmod_module_dependency_symbol_get_symbol"
  module_dependency_symbol_get_symbol :: Ptr List -> IO CString

foreign import ccall "kmod_module_dependency_symbols_free_list"
  module_dependency_symbols_free_list :: Ptr List -> IO ()

toDependencySymbolList :: Ptr List -> IO [(Symbol,Bind,CRC)]
toDependencySymbolList =  toList
                          moduleDependencySymbolGet
                          module_dependency_symbols_free_list

foreign import ccall "kmod_module_get_dependency_symbols"
  module_get_dependency_symbols :: Ptr Module
                                -> Ptr (Ptr List)
                                -> IO CInt

moduleGet :: (Ptr Module -> Ptr (Ptr List) -> IO CInt) -- ^ Get the list
          -> (Ptr List -> IO a)                        -- ^ Convert the list
          -> String                                    -- ^ Error string
          -> Module
          -> IO a
moduleGet fget flist err m =
    withModule m $ \p ->
        alloca $ \q ->
            do poke q nullPtr
               e <- fget p q
               if e < 0
                 then fail (err ++ " returned " ++ show e)
                 else peek q >>= flist

-- | Get the list of entries in ELF section \".symtab\" or
-- \"__ksymtab_strings\".
moduleGetDependencySymbols :: Module -> IO [(Symbol,Bind,CRC)]
moduleGetDependencySymbols =
    moduleGet
    module_get_dependency_symbols
    toDependencySymbolList
    "kmod_module_get_dependency_symbols"

-- | Name of a module section.
type Name = String

-- | Address of a module section.
type Address = Word64

-- | Get a list of sections of this @'Module'@, as returned by Linux kernel
-- (implemented natively in Haskell by reading @\/sys\/module\/@).
moduleGetSections :: Module -> IO [(Name,Address)]
moduleGetSections m = do fs <- getDirectoryContents secDir
                         mapM get (drop 2 fs)
    where secDir = "/sys/module/" ++ show m ++ "/sections"
          get f = do addr <- readFile (secDir ++ '/':f)
                     return (f, read addr)

foreign import ccall "kmod_module_symbol_get_crc"
  module_symbol_get_crc :: Ptr List -> IO CRC


foreign import ccall "kmod_module_symbol_get_symbol"
  module_symbol_get_symbol :: Ptr List -> IO CString

foreign import ccall "kmod_module_symbols_free_list"
  module_symbols_free_list :: Ptr List -> IO ()

foreign import ccall "kmod_module_get_symbols"
  module_get_symbols :: Ptr Module -> Ptr (Ptr List) -> IO CInt

-- | Get the list of entries in ELF section \".symtab\" or
-- \"__ksymtab_strings\".
moduleGetSymbols :: Module -> IO [(Symbol,CRC)]
moduleGetSymbols =
    moduleGet module_get_symbols toSymbolList "kmod_module_get_symbols"

moduleSymbolGet :: Ptr List -> IO (Symbol,CRC)
moduleSymbolGet p = do s <- module_symbol_get_symbol p >>= peekCString
                       c <- module_symbol_get_crc p
                       return (s,c)

toSymbolList :: Ptr List -> IO [(Symbol,CRC)]
toSymbolList = toList
               moduleSymbolGet
               module_symbols_free_list

foreign import ccall "kmod_module_version_get_crc"
  module_version_get_crc :: Ptr List -> IO CRC


foreign import ccall "kmod_module_version_get_symbol"
  module_version_get_symbol :: Ptr List -> IO CString

foreign import ccall "kmod_module_versions_free_list"
  module_versions_free_list :: Ptr List -> IO ()

foreign import ccall "kmod_module_get_versions"
  module_get_versions :: Ptr Module -> Ptr (Ptr List) -> IO CInt

-- | Get the list of entries in ELF section \"__versions\".
moduleGetVersions :: Module -> IO [(Symbol,CRC)]
moduleGetVersions =
    moduleGet module_get_versions toVersionList "kmod_module_get_versions"

moduleVersionGet :: Ptr List -> IO (Symbol,CRC)
moduleVersionGet p = do s <- module_version_get_symbol p >>= peekCString
                        c <- module_version_get_crc p
                        return (s,c)

toVersionList :: Ptr List -> IO [(Symbol,CRC)]
toVersionList = toList
                moduleVersionGet
                module_versions_free_list



foreign import ccall "kmod_module_info_get_key"
        module_info_get_key :: Ptr List
                            -> IO CString

foreign import ccall "kmod_module_info_get_value"
        module_info_get_value :: Ptr List
                              -> IO CString

foreign import ccall "kmod_module_get_info"
  module_get_info :: Ptr Module
                  -> Ptr (Ptr List)
                  -> IO CInt

-- | Get the list of entries in ELF section \".modinfo\", these contain alias,
-- license, depends, vermagic and other keys with respective values.
moduleGetInfo :: Module -> IO [(Key,Value)]
moduleGetInfo =
    moduleGet module_get_info toInfoList "kmod_module_get_info"

moduleInfoGet :: Ptr List -> IO (String, String)
moduleInfoGet p = do k <- module_info_get_key p >>= peekCString
                     v <- module_info_get_value p >>= peekCString
                     return (k,v)

foreign import ccall "kmod_module_info_free_list"
        module_info_free_list :: Ptr List
                              -> IO ()

toInfoList :: Ptr List -> IO [(String, String)]
toInfoList = toList
             moduleInfoGet
             module_info_free_list

-- | Possible values of initialization state of a @'Module'@.
data Initstate = Builtin
               | Live
               | Coming
               | Going
                 deriving (Eq, Show)


toInitstate :: (Eq a, Num a, Show a) => a -> Initstate
toInitstate #{const KMOD_MODULE_BUILTIN} = Builtin
toInitstate #{const KMOD_MODULE_LIVE} = Live
toInitstate #{const KMOD_MODULE_COMING} = Coming
toInitstate #{const KMOD_MODULE_GOING} = Going
toInitstate n = error ("System.KMod.toInitstate: bad argument: " ++ show n)

foreign import ccall "kmod_module_new_from_loaded"
  module_new_from_loaded :: Ptr Context -> Ptr (Ptr List) -> IO CInt

-- | Get the list of @'Module'@s currently loaded in kernel.
moduleNewFromLoaded :: Context -> IO [Module]
moduleNewFromLoaded c =
    withContext c $ \p ->
        alloca $ \q ->
            do throwIfNeg_ (\e -> "kmod_module_new_from_loaded retuned " ++
                            show e ) $ module_new_from_loaded p q
               peek q >>= toModuleList

foreign import ccall "kmod_module_get_initstate"
  module_get_initstate :: Ptr Module -> IO CInt

-- | Get the @'Initstate'@ of the given @'Module'@, as returned by Linux Kernel,
-- by reading @\/sys@ filesystem.
moduleGetInitstate :: Module -> IO Initstate
moduleGetInitstate m =
    withModule m
           (fmap toInitstate
            . throwIfNeg (\e -> "kmod_module_get_initstate returned " ++ show e)
            . module_get_initstate)

foreign import ccall "kmod_module_get_size"
  module_get_size :: Ptr Module -> IO CInt

-- | Get the size of the given @'Module'@ as returned by Linux kernel.
moduleGetSize :: Module -> IO Int
moduleGetSize m = fromIntegral <$> withModule m module_get_size

foreign import ccall "kmod_module_get_refcnt"
  module_get_refcnt :: Ptr Module -> IO CInt

-- | Get the ref count of the given @'Module'@, as returned by Linux kernel, by
-- reading @\/sys@ filesystem.
moduleGetRefcnt :: Module -> IO Int
moduleGetRefcnt m = fromIntegral <$> withModule m module_get_refcnt

foreign import ccall "kmod_module_get_holders"
  module_get_holders :: Ptr Module -> IO (Ptr List)

-- | Get the list of @'Module'@s that are holding the given @'Module'@, as
-- returned by Linux kernel.
moduleGetHolders :: Module -> IO [Module]
moduleGetHolders m = withModule m (module_get_holders >=> toModuleList)
