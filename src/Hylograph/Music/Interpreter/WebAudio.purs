-- | WebAudio Interpreter for HATS
-- |
-- | Interprets a HATS Tree as audio events using the Web Audio API.
-- |
-- | Each `Elem` node in the tree is interpreted as a note, with attributes
-- | mapping to audio parameters (time, pitch, duration, volume, timbre).
-- | `Fold` nodes iterate over data, scheduling one note per datum.
-- |
-- | Follows the same interpreter pattern as English.purs and Mermaid.purs,
-- | but produces audio output instead of text.
module Hylograph.Music.Interpreter.WebAudio
  ( interpretAudio
  , MusicM
  , runMusicM
  , initMusicContext
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldl, traverse_)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Hylograph.HATS (Tree(..), Attr(..), Enumeration(..), TraversalOrder(..), runSomeFold)
import Hylograph.Music.Internal.FFI (AudioContext, createAudioContext, scheduleNote, NoteParams)

-- =============================================================================
-- Monad
-- =============================================================================

-- | The Music interpreter monad
-- |
-- | Carries an AudioContext reference for scheduling notes.
-- | Uses ReaderT pattern to thread the context through computations.
newtype MusicM a = MusicM (Ref AudioContext -> Effect a)

instance Functor MusicM where
  map f (MusicM ma) = MusicM \ctx -> map f (ma ctx)

instance Apply MusicM where
  apply (MusicM ff) (MusicM fa) = MusicM \ctx -> do
    f <- ff ctx
    a <- fa ctx
    pure (f a)

instance Applicative MusicM where
  pure a = MusicM \_ -> pure a

instance Bind MusicM where
  bind (MusicM ma) f = MusicM \ctx -> do
    a <- ma ctx
    case f a of
      MusicM mb -> mb ctx

instance Monad MusicM

instance MonadEffect MusicM where
  liftEffect eff = MusicM \_ -> eff

-- | Initialize audio context and run a music program
-- |
-- | Creates the Web Audio context and executes the computation.
-- | Must be called in response to user interaction (browser requirement).
initMusicContext :: MusicM Unit -> Effect Unit
initMusicContext (MusicM program) = do
  ctx <- createAudioContext
  ctxRef <- Ref.new ctx
  program ctxRef

-- | Run a music program with an existing context ref
runMusicM :: Ref AudioContext -> MusicM ~> Effect
runMusicM ctxRef (MusicM program) = program ctxRef

-- | Get the audio context from the monad
getContext :: MusicM AudioContext
getContext = MusicM \ctxRef -> Ref.read ctxRef

-- =============================================================================
-- Tree Interpreter
-- =============================================================================

-- | Interpret a HATS tree as audio events
-- |
-- | Walks the tree, scheduling a note for each `Elem` node.
-- | Attributes are interpreted as audio parameters:
-- | - `time` → note start time (seconds)
-- | - `pitch` → frequency (Hz)
-- | - `duration` → note length (seconds)
-- | - `volume` → amplitude (0.0 to 1.0)
-- | - `timbre` → waveform type ("sine", "square", "sawtooth", "triangle")
-- |
-- | For `Fold` nodes, iterates over the enumeration and interprets
-- | each template result, using the iteration index for default timing.
interpretAudio :: Tree -> MusicM Unit
interpretAudio = go 0
  where
  go :: Int -> Tree -> MusicM Unit
  go idx tree = case tree of
    Elem { attrs, children } -> do
      ctx <- getContext
      liftEffect $ scheduleNote ctx (extractNoteParams idx attrs)
      traverse_ (go 0) children

    MkFold someFold ->
      runSomeFold someFold \spec -> do
        let items = runEnumeration spec.enumerate
        let indexed = Array.mapWithIndex Tuple items
        traverse_ (\(Tuple i datum) ->
          go i (spec.template datum)
        ) indexed

    Empty -> pure unit

-- =============================================================================
-- Attribute Extraction
-- =============================================================================

-- | Extract audio parameters from HATS attributes
-- |
-- | Attributes come from the HATS attribute system. In templates,
-- | datum values are captured in ThunkedAttr closures. We resolve
-- | thunks and map attribute names to audio parameters.
extractNoteParams :: Int -> Array Attr -> NoteParams
extractNoteParams index attrs =
  let defaults =
        { time: toNumber index * 0.5  -- Default: 500ms apart
        , frequency: 440.0             -- Default: A4
        , duration: 0.3                -- Default: 300ms
        , volume: 0.5                  -- Default: moderate volume
        , waveform: "sine"             -- Default: pure tone
        }
  in foldl applyAttr defaults attrs
  where
  resolveValue :: Attr -> { name :: String, value :: String }
  resolveValue = case _ of
    StaticAttr name value -> { name, value }
    ThunkedAttr name thunk -> { name, value: thunk unit }

  applyAttr :: NoteParams -> Attr -> NoteParams
  applyAttr params attr =
    let { name, value } = resolveValue attr
    in case name of
      "time" -> case Number.fromString value of
        Just n -> params { time = n }
        Nothing -> params
      "pitch" -> case Number.fromString value of
        Just n -> params { frequency = n }
        Nothing -> params
      "duration" -> case Number.fromString value of
        Just n -> params { duration = n }
        Nothing -> params
      "volume" -> case Number.fromString value of
        Just n -> params { volume = n }
        Nothing -> params
      "timbre" -> params { waveform = value }
      _ -> params

-- =============================================================================
-- Enumeration
-- =============================================================================

-- | Run an enumeration to get an array of items
runEnumeration :: forall a. Enumeration a -> Array a
runEnumeration = case _ of
  FromArray arr -> arr
  FromTree spec ->
    case spec.order of
      DepthFirst -> enumerateDFS spec.root spec.children spec.includeInternal
      BreadthFirst -> enumerateBFS spec.root spec.children spec.includeInternal
  WithContext items -> map _.datum items

enumerateDFS :: forall a. a -> (a -> Array a) -> Boolean -> Array a
enumerateDFS root getChildren includeInternal = go root
  where
  go node =
    let children = getChildren node
        childResults = Array.concatMap go children
        isLeaf = Array.null children
    in if isLeaf || includeInternal
       then [node] <> childResults
       else childResults

enumerateBFS :: forall a. a -> (a -> Array a) -> Boolean -> Array a
enumerateBFS root getChildren includeInternal = go [root]
  where
  go queue = case Array.uncons queue of
    Nothing -> []
    Just { head: node, tail: rest } ->
      let children = getChildren node
          isLeaf = Array.null children
          include = isLeaf || includeInternal
          thisNode = if include then [node] else []
      in thisNode <> go (rest <> children)
