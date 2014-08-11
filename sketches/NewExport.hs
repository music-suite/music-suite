
class Functor (BackendScore) => HasBackend where
  type BackendMusic :: *
  type BackendNote :: *
  type BackendScore :: * -> *
  type BackendContext :: * -> *  
  finalizeExport :: -> BackendScore (BackendNote) -> BackendMusic

class (HasBackend) => HasBackendScore s where
  type BackendScoreEvent s :: *
  -- BackendScoreEvent a = a
  -- LyScore a = Chord (Voice (Chord a))

  -- TODO aspects should not be in *context* (passed to note)
  -- They should be given directly to finalize
  
  -- ALTERNATIVELY actually pass context (i.e. Ctxt) in BackendContext (what does this mean?)
  -- LyContext a = (LyPitch = Int, LyArt = (Double x 2), LyDyn = Double, a)
  exportScore :: -> s -> BackendScore (BackendContext (BackendScoreEvent s))

class (HasBackend) => HasBackendNote a where
  exportNote  :: -> BackendContext a   -> BackendNote
  exportChord :: -> BackendContext [a] -> BackendNote
  exportChord = error "Not implemented: exportChord"

export :: (HasBackendScore s, HasBackendNote (BackendScoreEvent s)) => -> s -> BackendMusic
export = finalizeExport . fmap (exportNote) . exportScore b

