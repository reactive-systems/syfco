-----------------------------------------------------------------------------
-- |
-- Module      :  Writer.Eval
-- License     :  MIT (see the LICENSE file)
-- Maintainer  :  Felix Klein (klein@react.uni-saarland.de)
--
-- Unfolds all high level constructs of a specification to the corresponding
-- low level constructs.
--
-----------------------------------------------------------------------------

{-# LANGUAGE

    FlexibleContexts
  , LambdaCase

  #-}

-----------------------------------------------------------------------------

module Writer.Eval
  ( eval
  , signals
  ) where

-----------------------------------------------------------------------------

import Utils
  ( iter
  )

import Data.Error
  ( cfgError
  )

import Data.Enum
  ( EnumDefinition(..)
  )

import Data.Maybe
  ( fromMaybe
  , catMaybes
  )

import Config
  ( Configuration(..)
  )

import Data.Either
  ( partitionEithers
  )

import Data.List
  ( find
  )

import Data.Char
  ( toLower
  )

import Data.LTL
  ( Atomic(..)
  , Formula(..)
  , subFormulas
  , applyAtomic
  , applySub
  , fGlobally
  , fAnd
  , fNot
  )

import Data.Types
  ( IdType(..)
  , Semantics(..)
  , SignalType(..)
  , SignalDecType(..)
  )

import Data.Binding
  ( BindExpr(..)
  )

import Data.Expression
  ( Expr(..)
  , Expr'(..)
  , ExprPos
  )

import Data.Specification
  ( Specification(..)
  )

import Data.SymbolTable
  ( IdRec(..)
  , SymbolTable
  )

import Writer.Error
  ( Error
  , errBounds
  , errBusCmp
  , errMinSet
  , errMaxSet
  , errSetCap
  , errNoMatch
  )

import Writer.Formats
  ( needsLower
  )

import Control.Monad.State
  ( StateT(..)
  , foldM
  , execStateT
  , evalStateT
  , liftM2
  , liftM
  , when
  , get
  , put
  )

import Control.Exception
  ( assert
  )

import Data.Array.IArray
  ( (!)
  )

import qualified Data.Graph as G

import qualified Data.IntMap as IM

import qualified Data.Set as S

import qualified Data.Array.IArray as A

-----------------------------------------------------------------------------

data Value =
    VEmpty
  | VNumber Int
  | VLtl Formula
  | VSet (S.Set Value)
  | VEnum String Int [Int -> Either Bool ()]
  | VSignal SignalType String
  | VBus SignalType Int String

instance Show Value where
  show v = case v of
    VEmpty      -> "VEmpty"
    VNumber i   -> "VNumber " ++ show i
    VLtl f      -> "VLtl " ++ show f
    VSet s      -> "VSet " ++ show s
    VEnum {}    -> "VEnum"
    VSignal _ s -> "VSingal " ++ s
    VBus _ i s  -> "VBus " ++ s ++ "[" ++ show i ++ "]"

instance Eq Value where
  (==) VEmpty VEmpty                 = True
  (==) (VNumber i) (VNumber j)       = i == j
  (==) (VLtl f) (VLtl f')            = f == f'
  (==) (VSet s) (VSet s')            = s == s'
  (==) (VSignal _ s) (VSignal _ s')  = s == s'
  (==) (VBus _ _ s) (VBus _ _ s')    = s == s'
  (==) (VEnum _ i xs) (VEnum _ j ys) =
     i == j && length xs == length ys &&
     map (\f -> map f [0,1..i-1]) xs ==
     map (\f -> map f [0,1..i-1]) ys
  (==) _ _                           = False

instance Ord Value where
  compare VEmpty VEmpty                 = EQ
  compare (VNumber i) (VNumber j)       = compare i j
  compare (VLtl f) (VLtl f')            = compare f f'
  compare (VSet s) (VSet s')            = compare s s'
  compare (VSignal _ s) (VSignal _ s')  = compare s s'
  compare (VBus _ _ s) (VBus _ _ s')    = compare s s'
  compare (VEnum _ i xs) (VEnum _ j ys)
    | i /= j                 = compare i j
    | length xs /= length ys = compare (length xs) (length ys)
    | otherwise             =
        compare (map (\f -> map f [0,1..i-1]) xs)
                (map (\f -> map f [0,1..i-1]) ys)
  compare x y                           = compare (cidx x) (cidx y)

    where
      cidx :: Value -> Int
      cidx VEmpty     = 0
      cidx VNumber {} = 1
      cidx VLtl {}    = 2
      cidx VSet {}    = 3
      cidx VEnum {}   = 4
      cidx VSignal {} = 5
      cidx VBus {}    = 6

-----------------------------------------------------------------------------

type Evaluator a = a -> StateT ST (Either Error) Value

-----------------------------------------------------------------------------

data ST = ST
  { tLookup :: SymbolTable
  , tValues :: IM.IntMap Value
  , delimiter :: String
  , enums :: [EnumDefinition Int]
  }

-----------------------------------------------------------------------------

-- | @eval c s@ evaluates all high level constructs of the given
-- specification @s@ under the current configuration @c@.

eval
  :: Configuration -> Specification
     -> Either Error ([Formula],[Formula],[Formula],
                     [Formula],[Formula],[Formula])

eval c s = do
  (s', st0, xs) <- initialize c s
  let ios = inputs s' ++ outputs s'

  stt <- execStateT (mapM_ (staticBinding (semantics s')) xs) st0

  (rr,sti) <- runStateT (mapM (componentSignal (semantics s')) ios) stt
  let (er,sr) = partitionEithers $ catMaybes rr
  let evalF = evalLtl $ semantics s'

  es <- evalStateT (mapM evalF $ initially s') sti
  ts <- evalStateT (mapM evalF $ preset s') sti
  rs <- evalStateT (mapM evalF $ requirements s') sti
  as <- evalStateT (mapM evalF $ assumptions s') sti
  is <- evalStateT (mapM evalF $ invariants s') sti
  gs <- evalStateT (mapM evalF $ guarantees s') sti



  return $ splitConjuncts $ overwrite s'
    ( map plainltl es, map plainltl ts, map plainltl (rs ++ er),
      map plainltl as, map plainltl (is ++ sr), map plainltl gs )

  where
    plainltl = applyAtomic apA . vltl

    apA :: Atomic -> Formula
    apA x = Atomic $ case x of
      Input y  -> Input $ repap (atSymbol c) (primeSymbol c)
                  $ lower $ last $ words y
      Output y -> Output $ repap (atSymbol c) (primeSymbol c)
                  $ lower $ last $ words y

    lower x =
      if needsLower (outputFormat c)
      then map toLower x
      else x

    overwrite sp (es,ss,rs,as,is,gs) =
      let
        og = all outputsGuarded (es ++ ss ++ rs ++ as ++ is ++ gs)
        ig = all inputsGuarded (es ++ ss ++ rs ++ as ++ is ++ gs)

        (es',ss',rs',as',is',gs') =
          if (semantics sp, owSemantics c) `elem`
             [(SemanticsStrictMealy, Just SemanticsMealy),
              (SemanticsStrictMealy, Just SemanticsMoore),
              (SemanticsStrictMoore, Just SemanticsMealy),
              (SemanticsStrictMoore, Just SemanticsMoore)]
          then (es, filter (/= TTrue) ((fWeak (fAnd is) (fNot (fAnd rs))) : ss),
                rs, as, [], gs)
          else (es,ss,rs,as,is,gs)
      in
        (map (adjustOW og ig sp) es',
         map (adjustOW og ig sp) ss',
         map (adjustOW og ig sp) rs',
         map (adjustOW og ig sp) as',
         map (adjustOW og ig sp) is',
         map (adjustOW og ig sp) gs')

    fWeak TTrue _  = TTrue
    fWeak x FFalse = fGlobally x
    fWeak x y      = Weak x y

    adjustOW og ig sp e = case owSemantics c of
      Nothing -> e
      Just m
        | (semantics sp, m) `elem`
            [(SemanticsMealy, SemanticsMealy),
             (SemanticsMoore, SemanticsMoore),
             (SemanticsStrictMealy, SemanticsStrictMealy),
             (SemanticsStrictMoore, SemanticsStrictMoore),
             (SemanticsStrictMealy, SemanticsMealy),
             (SemanticsStrictMoore, SemanticsMoore)] -> e
        | (semantics sp, m) `elem`
            [(SemanticsMealy, SemanticsMoore),
             (SemanticsStrictMealy, SemanticsMoore),
             (SemanticsStrictMealy, SemanticsStrictMoore)] ->
              if ig then unGuardInputs sp e else guardOutputs sp e
        | (semantics sp, m) `elem`
            [(SemanticsMoore, SemanticsMealy),
             (SemanticsStrictMoore, SemanticsMealy),
             (SemanticsStrictMoore, SemanticsStrictMealy)] ->
              if og then unGuardOutputs sp e else guardInputs sp e
        | semantics sp `elem`
             [SemanticsFiniteMealy, SemanticsFiniteMoore] ->
             error "Conversion from finite semantics not possible"
        | otherwise ->
             error "Conversion from non-strict -> strict semantics not possible"

    outputsGuarded e = case e of
      Next (Atomic (Output _)) -> True
      Atomic (Output _)        -> False
      _                        -> all outputsGuarded $ subFormulas e

    inputsGuarded e = case e of
      Next (Atomic (Input _)) -> True
      Atomic (Input _)        -> False
      _                       -> all inputsGuarded $ subFormulas e

    guardOutputs sp e = case e of
      Atomic (Output x) -> Next $ Atomic $ Output x
      _                 -> applySub (guardOutputs sp) e

    guardInputs sp e = case  e of
      Atomic (Input x) -> Next $ Atomic $ Input x
      _                -> applySub (guardInputs sp) e

    unGuardOutputs sp e  = case e of
      Next (Atomic (Output x)) -> Atomic $ Output x
      _                        -> applySub (unGuardOutputs sp) e

    unGuardInputs sp e  = case e of
      Next (Atomic (Input x)) -> Atomic $ Input x
      _                       -> applySub (unGuardInputs sp) e

    splitConjuncts (es,ss,rs,xs,ys,zs) =
      (concatMap splitC es,
       concatMap splitC ss,
       concatMap splitC rs,
       concatMap splitC xs,
       concatMap splitC ys,
       concatMap splitC zs)

    splitC fml = case fml of
      And xs -> concatMap splitC xs
      _      -> [fml]

-----------------------------------------------------------------------------

-- | Returns the signals of a specification using the format as
-- implied by the given configuration.

signals
  :: Configuration -> Specification
  -> Either Error ([String],[String])

signals c s = do
  (s',st,xs) <- initialize c s
  let ios = inputs s' ++ outputs s'
  stt <- execStateT (mapM_ (staticBinding (semantics s)) xs) st
  stf <- execStateT (mapM_ (componentSignal (semantics s)) ios) stt
  let
    is = map getId $ inputs s'
    os = map getId $ outputs s'

  iv <- evalStateT (mapM getV is) stf
  ov <- evalStateT (mapM getV os) stf

  return (
    map (lower . repap (atSymbol c) (primeSymbol c)) $ concat iv,
    map (lower . repap (atSymbol c) (primeSymbol c)) $ concat ov
    )

  where
    lower x =
      if needsLower (outputFormat c)
      then map toLower x
      else x

    getId v = case v of
      SDSingle (i,_) -> i
      SDBus (i,_) _  -> i
      SDEnum (i,_) _ -> i

    getV i = do
      st <- get
      case IM.lookup i (tValues st) of
        Nothing -> assert False undefined
        Just x  -> case x of
          VSignal _ y -> return [y]
          VBus _ n y  -> return [y ++ delimiter st ++ show j | j <- [0,1..n-1]]
          _           -> assert False undefined

-----------------------------------------------------------------------------

-- | Computes the initial state for the evaluation of the
-- specification, as well as the adapted specification and the
-- resolution order.

initialize
  :: Configuration -> Specification -> Either Error (Specification, ST, [Int])

initialize c s = do
  s' <- foldM overwriteParameter s $ owParameter c

  let
    s'' = s' {
      target = fromMaybe (target s') $ owTarget c
      }

    -- get identifiers definitions that are not a function
    uids =
      filter (isunary s'') $
      map bIdent $ parameters s'' ++ definitions s''

    -- get unary input and output signals
    ios =
      filter single $ inputs s'' ++ outputs s''

    -- get the ids from the signals
    ioids =
      map (\(SDSingle (x,_)) -> x) ios

    ids = ioids ++ uids

    -- construct the edges according to the dependency graph
    edges =
      concatMap
        (\i -> map (\j -> (i,j))
              $ filter (isunary s'')
              $ idDeps $ symboltable s'' ! i)
        ids

    minkey = minimum ids
    maxkey = maximum ids

    -- obtain general dependency order
    order' = if null ids then []
             else reverse
                  $ G.topSort
                  $ G.buildG (minkey,maxkey) edges

    -- filter u and
    order =
      filter (`S.member` (S.fromList uids)) order'

  st <- execStateT (mapM (componentSignal (semantics s)) ios) $ ST
         (symboltable s'')
         IM.empty
         (busDelimiter c)
         (enumerations s)

  st' <- execStateT (mapM_ enumBinding $ enumerations s) st

  return (s'', st', order)

  where
    isunary y x = null $ idArgs $ symboltable y ! x

    single = \case
      SDSingle _ -> True
      _          -> False

-----------------------------------------------------------------------------

enumBinding
  :: EnumDefinition Int -> StateT ST (Either Error) ()

enumBinding x = do
  st <- get
  let n = idName $ tLookup st ! eName x
  put $ st {
    tValues = foldl (add n $ eSize x) (tValues st) $ eValues x
    }

  where
    add n s im (v,_,fs) =
      IM.insert v (VEnum n s fs) im

-----------------------------------------------------------------------------

staticBinding
  :: Semantics -> Int -> StateT ST (Either Error) ()

staticBinding s x = do
  st <- get

  evalSet s (idBindings $ tLookup st ! x) >>= \case
    VSet bs -> case S.toList bs of
      []  -> return ()
      v:_ ->
        put $ st {
          tValues = IM.insert x v $ tValues st
          }
    _       -> assert False undefined

-----------------------------------------------------------------------------

componentSignal
  :: Semantics -> SignalDecType Int -> StateT ST (Either Error) (Maybe (Either Value Value))

componentSignal sem s = do
  st <- get
  (i,v,r) <- case s of
    SDSingle (i,_) -> case idType $ tLookup st ! i of
      TSignal io  -> return (i,VSignal io $ idName (tLookup st ! i), Nothing)
      _           -> assert False undefined
    SDBus (i,_) e  ->
      evalNum sem e >>= \case
        VNumber n -> case idType $ tLookup st ! i of
          TBus io  -> return (i,VBus io n $ idName (tLookup st ! i), Nothing)
          _        -> assert False undefined
        _         -> assert False undefined
    SDEnum (i,_) _ ->
      case idType $ tLookup st ! i of
        TTypedBus io _ t -> case find ((== t) . eName) $ enums st of
          Nothing -> assert False undefined
          Just e  ->
            let
              m = idName (tLookup st ! i)
              v = VBus io (eSize e) m
              r = case eMissing e of
                [] -> Nothing
                xs -> case io of
                  STInput  -> Just $ Left $
                             missing (delimiter st) m Input (eSize e) xs
                  STOutput -> Just $ Right $
                             missing (delimiter st) m Output (eSize e) xs
                  _        -> assert False undefined
            in
              return (i,v,r)

        _                -> assert False undefined

  put $ st {
    tValues = IM.insert i v $ tValues st
    }

  return r

  where
    missing d m c n xs = VLtl $ And $ map (tB d m c n) xs

    tB d m c n f = Or $ map (tV d m c f) [0,1..n-1]

    tV d m c f i = case f i of
      Left True  -> Not $ Atomic $ c $ m ++ d ++ show i
      Left False -> Atomic $ c $ m ++ d ++ show i
      _          -> assert False undefined

-----------------------------------------------------------------------------

evalExpr
  :: Semantics -> Evaluator (Expr Int)

evalExpr s e = case expr e of
  BaseWild            -> evalF e
  BaseCon {}          -> evalN e
  NumSMin {}          -> evalN e
  NumSMax {}          -> evalN e
  NumSSize {}         -> evalN e
  NumSizeOf {}        -> evalN e
  NumPlus {}          -> evalN e
  NumMinus {}         -> evalN e
  NumMul {}           -> evalN e
  NumDiv {}           -> evalN e
  NumMod {}           -> evalN e
  NumRPlus {}         -> evalN e
  NumRMul {}          -> evalN e
  BaseTrue            -> evalF e
  BaseFalse           -> evalF e
  BaseBus {}          -> evalF e
  BlnEQ {}            -> evalF e
  BlnNEQ {}           -> evalF e
  BlnGE {}            -> evalF e
  BlnGEQ {}           -> evalF e
  BlnLE {}            -> evalF e
  BlnLEQ {}           -> evalF e
  BlnNot {}           -> evalF e
  BlnOr {}            -> evalF e
  BlnROr {}           -> evalF e
  BlnAnd {}           -> evalF e
  BlnRAnd {}          -> evalF e
  BlnImpl {}          -> evalF e
  BlnElem {}          -> evalF e
  BlnEquiv {}         -> evalF e
  LtlNext {}          -> evalF e
  LtlStrongNext {}    -> evalF e
  LtlRNext {}         -> evalF e
  LtlRStrongNext {}   -> evalF e
  LtlPrevious {}      -> evalF e
  LtlRPrevious {}     -> evalF e
  LtlGlobally {}      -> evalF e
  LtlRGlobally {}     -> evalF e
  LtlFinally {}       -> evalF e
  LtlRFinally {}      -> evalF e
  LtlHistorically {}  -> evalF e
  LtlRHistorically {} -> evalF e
  LtlOnce {}          -> evalF e
  LtlROnce {}         -> evalF e
  LtlUntil {}         -> evalF e
  LtlWeak {}          -> evalF e
  LtlRelease {}       -> evalF e
  LtlSince {}         -> evalF e
  LtlTriggered{}      -> evalF e
  SetExplicit {}      -> evalS e
  SetRange {}         -> evalS e
  SetCup {}           -> evalS e
  SetRCup {}          -> evalS e
  SetCap {}           -> evalS e
  SetRCap {}          -> evalS e
  SetMinus {}         -> evalS e
  BaseId x            -> idValue x
  BaseFml xs x        -> fmlValue s xs (srcPos e) x
  Colon {}            -> evalColon s e
  _                   -> assert False undefined
  where
  evalF = evalLtl s

  evalN = evalNum s

  evalS = evalSet s

-----------------------------------------------------------------------------

evalLtl
  :: Semantics -> Evaluator (Expr Int)

evalLtl s e = case expr e of
  BaseTrue             -> return $ VLtl TTrue
  BaseFalse            -> return $ VLtl FFalse
  BlnNot x             -> liftMLtl Not x
  LtlNext x            -> liftMLtl wNext x
  LtlStrongNext x      -> liftMLtl sNext x
  LtlPrevious x        -> liftMLtl Previous x
  LtlGlobally x        -> liftMLtl Globally x
  LtlFinally x         -> liftMLtl Finally x
  LtlHistorically x    -> liftMLtl Historically x
  LtlOnce x            -> liftMLtl Once x
  LtlUntil x y         -> liftM2Ltl Until x y
  LtlRelease x y       -> liftM2Ltl Release x y
  LtlWeak x y          -> liftM2Ltl Weak x y
  LtlSince x y         -> liftM2Ltl Since x y
  LtlTriggered x y     -> liftM2Ltl Triggered x y
  BlnImpl x y          -> liftM2Ltl Implies x y
  BlnEquiv x y         -> liftM2Ltl Equiv x y
  BlnEQ x y            ->
    evalEquality s (==) "==" x y (srcPos e) >>= \case
      Left v -> return v
      Right True -> return $ VLtl TTrue
      Right False -> return $ VLtl FFalse
  BlnNEQ x y           ->
    evalEquality s (/=) "!=" x y (srcPos e) >>= \case
      Left v -> return v
      Right True -> return $ VLtl TTrue
      Right False -> return $ VLtl FFalse
  BlnGE x y            -> liftM2Num (>) x y
  BlnGEQ x y           -> liftM2Num (>=) x y
  BlnLE x y            -> liftM2Num (<) x y
  BlnLEQ x y           -> liftM2Num (<=) x y
  BaseId _             ->
    evalE e >>= \case
      VLtl y             -> return $ VLtl y
      VSignal STInput y  -> return $ VLtl $ Atomic $ Input y
      VSignal STOutput y -> return $ VLtl $ Atomic $ Output y
      _                  -> assert False undefined
  BaseFml _ _          ->
    evalE e >>= \case
      VLtl y             -> return $ VLtl y
      VSignal STInput y  -> return $ VLtl $ Atomic $ Input y
      VSignal STOutput y -> return $ VLtl $ Atomic $ Output y
      _                  -> assert False undefined
  LtlRNext x y         ->
    evalN x >>= \case
      VNumber n -> evalF y >>= \case
        VLtl v -> return $ VLtl $ iter wNext n v
        _      -> assert False undefined
      _         -> assert False undefined
  LtlRStrongNext x y    ->
    evalN x >>= \case
      VNumber n -> evalF y >>= \case
        VLtl v -> return $ VLtl $ iter sNext n v
        _      -> assert False undefined
      _         -> assert False undefined
  LtlRPrevious x y     ->
    evalN x >>= \case
      VNumber n -> evalF y >>= \case
        VLtl v -> return $ VLtl $ iter Previous n v
        _      -> assert False undefined
      _         -> assert False undefined
  LtlRGlobally x y     -> do
    (i,j) <- evalRange s x
    if i > j
    then return $ VLtl TTrue
    else evalF y >>= \case
      VLtl v -> return $ VLtl $ iter sNext i $
                 iter (\a -> And [v, sNext a]) (j - i) v
      _      -> assert False undefined
  LtlRFinally x y      -> do
    (i,j) <- evalRange s x
    if i > j
    then return $ VLtl TTrue
    else evalF y >>= \case
      VLtl v -> return $ VLtl $ iter sNext i $
                 iter (\a -> Or [v, sNext a]) (j - i) v
      _      -> assert False undefined
  LtlRHistorically x y -> do
    (i,j) <- evalRange s x
    if i > j
    then return $ VLtl TTrue
    else evalF y >>= \case
      VLtl v -> return $ VLtl $ iter Previous i $
                 iter (\a -> And [v, Previous a]) (j - i) v
      _      -> assert False undefined
  LtlROnce x y         -> do
    (i,j) <- evalRange s x
    if i > j
    then return $ VLtl TTrue
    else evalF y >>= \case
      VLtl v -> return $ VLtl $ iter Previous i $
                 iter (\a -> Or [v, Previous a]) (j - i) v
      _      -> assert False undefined
  BlnElem x y          -> do
    a <- evalE x
    evalE y >>= \case
      VSet b -> return $ VLtl $ if S.member a b then TTrue else FFalse
      _      -> assert False undefined
  BlnOr x y            ->
    evalF x >>= \case
      VLtl a -> evalF y >>= \case
        VLtl b -> return $ VLtl $ Or [a,b]
        _      -> assert False undefined
      _      -> assert False undefined
  BlnAnd x y           ->
    evalF x >>= \case
      VLtl a -> evalF y >>= \case
        VLtl b -> return $ VLtl $ And [a,b]
        _      -> assert False undefined
      _      -> assert False undefined
  BlnRAnd xs x         ->
    let f = VLtl . And . map (\(VLtl v) -> v)
    in evalCond evalF f xs x
  BlnROr xs x          ->
    let f = VLtl . Or . map (\(VLtl v) -> v)
    in evalCond evalF f xs x
  BaseBus x y          ->
    idValue y >>= \case
      VBus io l s -> evalN x >>= \case
        VNumber b -> do
          when (b < 0 || b >= l) $
            errBounds s l b $ srcPos e

          st <- get
          return $ VLtl $ Atomic $ case io of
            STInput   -> Input $ s ++ delimiter st ++ show b
            STOutput  -> Output $ s ++  delimiter st ++ show b
            STGeneric -> assert False undefined
        _         -> assert False undefined
      _           -> assert False undefined
  _                    -> assert False undefined

  where
    liftMLtl f m =
      evalF m >>= \case
        VLtl x -> return $ VLtl $ f x
        _      -> assert False undefined

    liftM2Ltl f m n =
      evalF m >>= \case
        VLtl x -> evalF n >>= \case
          VLtl y -> return $ VLtl $ f x y
          _      -> assert False undefined
        _      -> assert False undefined

    liftM2Num f m n =
      evalN m >>= \case
        VNumber x -> evalN n >>= \case
          VNumber y -> return $ VLtl $ if f x y then TTrue else FFalse
          _         -> assert False undefined
        _         -> assert False undefined
    
    sNext = if s `elem` [SemanticsFiniteMealy, SemanticsFiniteMoore]
            then StrongNext
            else Next

    wNext = if s `elem` [SemanticsFiniteMealy, SemanticsFiniteMoore]
            then WeakNext
            else Next

    evalF = evalLtl s

    evalE = evalExpr s

    evalN = evalNum s

    evalCond = evalConditional s

-----------------------------------------------------------------------------

idValue
  :: Evaluator Int

idValue i = do
  st <- get
  case IM.lookup i $ tValues st of
    Just x  -> return x
    Nothing -> assert False undefined

-----------------------------------------------------------------------------

evalNum
  :: Semantics -> Evaluator (Expr Int)

evalNum s e = case expr e of
  BaseCon x     -> return $ VNumber x
  NumPlus x y   -> liftM2Num (+) x y
  NumMinus x y  -> liftM2Num (-) x y
  NumMul x y    -> liftM2Num (*) x y
  NumDiv x y    -> liftM2Num div x y
  NumMod x y    -> liftM2Num mod x y
  NumSMin x     ->
    evalE x >>= \case
      VSet y ->
        let xs = map (\(VNumber v) -> v) $ S.elems y in
        if null xs
        then errMinSet $ srcPos e
        else return $ VNumber $ foldl min (head xs) xs
      _      -> assert False undefined
  NumSMax x     -> do
    evalE x >>= \case
      VSet y ->
        let xs = map (\(VNumber v) -> v) $ S.elems y in
        if null xs
        then errMaxSet $ srcPos e
        else return $ VNumber $ foldl max (head xs) xs
      _      -> assert False undefined
  NumSSize x    ->
    evalE x >>= \case
      VSet y -> return $ VNumber $ S.size y
      _      -> assert False undefined
  NumSizeOf x   ->
    evalE x >>= \case
      VBus _ i _ -> return $ VNumber i
      _          -> assert False undefined
  NumRPlus xs x ->
    let f = VNumber . sum . map (\(VNumber v) -> v)
    in evalCond evalN f xs x
  NumRMul xs x  ->
    let f = VNumber . product . map (\(VNumber v) -> v)
    in evalCond evalN f xs x
  BaseId _      ->
    evalE e >>= \case
      VNumber x -> return $ VNumber x
      _         -> assert False undefined
  BaseFml _ _   ->
    evalE e >>= \case
      VNumber x -> return $ VNumber x
      _         -> assert False undefined
  _             -> assert False undefined

  where
    liftM2Num f m n =
      evalN m >>= \case
        VNumber x -> evalN n >>= \case
          VNumber y -> return $ VNumber $ f x y
          _         -> assert False undefined
        _         -> assert False undefined

    evalN = evalNum s

    evalE = evalExpr s

    evalCond = evalConditional s

-----------------------------------------------------------------------------

evalBool
  :: Semantics -> Expr Int -> StateT ST (Either Error) Bool

evalBool s e = case expr e of
  BaseOtherwise -> return True
  BaseTrue      -> return True
  BaseFalse     -> return False
  BlnNot x      -> liftM not $ evalB x
  BlnImpl x y   -> do
    a <- evalB x
    if a then
      evalB y
    else
      return True
  BlnEquiv x y  -> liftM2 (==) (evalB x) (evalB y)
  BlnOr x y     -> liftM2 (||) (evalB x) (evalB y)
  BlnAnd x y    -> liftM2 (&&) (evalB x) (evalB y)
  BlnEQ x y     -> do
    b <- evalEquality s (==) "==" x y $ srcPos e
    case b of
      Left _  -> assert False undefined
      Right v -> return v
  BlnNEQ x y    -> do
    b <- evalEquality s (/=) "!=" x y $ srcPos e
    case b of
      Left _  -> assert False undefined
      Right v -> return v
  BlnGE x y     -> liftM2Num (>) x y
  BlnGEQ x y    -> liftM2Num (>=) x y
  BlnLE x y     -> liftM2Num (<) x y
  BlnLEQ x y    -> liftM2Num (<=) x y
  BlnElem x y   -> do
    a <- evalE x
    evalE y >>= \case
      VSet b -> return $ S.member a b
      _      -> assert False undefined
  BlnRAnd xs x  -> evalCond evalB and xs x
  BlnROr xs x   -> evalCond evalB or xs x
  Pattern x y   ->
    evalLtl s x >>= \case
      VLtl a -> checkPattern a y
      _      -> assert False undefined
  _             -> assert False undefined

  where
    liftM2Num f m n =
      evalNum s m >>= \case
        VNumber x -> evalNum s n >>= \case
          VNumber y -> return $ f x y
          _         -> assert False undefined
        _         -> assert False undefined

    evalB = evalBool s

    evalE = evalExpr s

    evalCond = evalConditional s

-----------------------------------------------------------------------------

checkPattern
  :: Formula -> Expr Int -> StateT ST (Either Error) Bool

checkPattern f e = case (f,expr e) of
  (_, BaseWild)                       -> return True
  (TTrue, BaseTrue)                   -> return True
  (FFalse, BaseFalse)                 -> return True
  (Not x, BlnNot y)                   -> checkPattern x y
  (Next x, LtlNext y)                 -> checkPattern x y
  (WeakNext x, LtlNext y)             -> checkPattern x y
  (StrongNext x, LtlStrongNext y)     -> checkPattern x y
  (Previous x, LtlPrevious y)         -> checkPattern x y
  (Globally x, LtlGlobally y)         -> checkPattern x y
  (Finally x, LtlFinally y)           -> checkPattern x y
  (Historically x, LtlHistorically y) -> checkPattern x y
  (Once x, LtlOnce y)                 -> checkPattern x y
  (Implies x y, BlnImpl z v)          -> binary x y z v
  (Equiv x y, BlnEquiv z v)           -> binary x y z v
  (Until x y, LtlUntil z v)           -> binary x y z v
  (Release x y, LtlRelease z v)       -> binary x y z v
  (Since x y, LtlSince z v)           -> binary x y z v
  (Triggered x y, LtlTriggered z v)   -> binary x y z v
  (And xs, BlnAnd z v)                -> case xs of
    [x,y] -> binary x y z v
    _     -> assert False undefined
  (Or xs, BlnOr z v)                  -> case xs of
    [x,y] -> binary x y z v
    _     -> assert False undefined
  (_, BaseId i)                       -> do
    st <- get
    put st {
      tValues = IM.insert i (VLtl f) $ tValues st
      }
    return True
  _                                   -> return False

  where
    binary x y z a = do
      b <- checkPattern x z
      if b then
        checkPattern  y a
      else
        return False

-----------------------------------------------------------------------------

evalSet
  :: Semantics -> Evaluator (Expr Int)

evalSet s e = case expr e of
  SetExplicit xs -> do
    ys <- mapM (evalExpr s) xs
    return $ VSet $ S.fromList ys
  SetRange x y z ->
    evalNum s x >>= \case
      VNumber a -> evalNum s y >>= \case
        VNumber b -> evalNum s z >>= \case
          VNumber c -> return $ VSet $ S.fromList $ map VNumber [a,b..c]
          _         -> assert False undefined
        _         -> assert False undefined
      _         -> assert False undefined
  SetCup x y     -> liftM2Set S.union x y
  SetCap x y     -> liftM2Set S.intersection x y
  SetMinus x y   -> liftM2Set S.difference x y
  SetRCup xs x   ->
    let f = VSet . S.unions . map (\(VSet v) -> v)
    in evalCond evalS f xs x
  SetRCap xs x   ->
    if null xs then
      errSetCap $ srcPos e
    else
      let
        g vs = foldl S.intersection (head vs) vs
        f = VSet . g . map (\(VSet v) -> v)
      in
        evalCond evalS f xs x
  BaseId _       ->
    evalExpr s e >>= \case
      VSet x -> return $ VSet x
      _      -> assert False undefined
  BaseFml _ _    -> do
    evalExpr s e >>= \case
      VSet x -> return $ VSet x
      _      -> assert False undefined
  _ -> assert False undefined

  where
    liftM2Set f x y =
      evalSet s x >>= \case
        VSet a -> evalSet s y >>= \case
          VSet b -> return $ VSet $ f a b
          _      -> assert False undefined
        _      -> assert False undefined

    evalCond = evalConditional s
    
    evalS = evalSet s

-----------------------------------------------------------------------------

evalConditional
  :: Semantics -> (Expr Int -> StateT ST (Either Error) a) -> ([a] -> a)
      -> [Expr Int] -> Expr Int -> StateT ST (Either Error) a

evalConditional sem fun f xs x =
  if null xs then
    fun x
  else do
    st <- get
    let
      i = case expr $ head xs of
        BlnElem e _ -> case expr e of
          BaseId r -> r
          _ -> assert False undefined
        BlnLE e _   -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          _ -> assert False undefined
        BlnLEQ e _  -> case expr e of
          BlnLE _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          BlnLEQ _ m -> case expr m of
            BaseId r -> r
            _ -> assert False undefined
          _ -> assert False undefined
        _ -> assert False undefined
      s = idBindings $ tLookup st ! i

    evalSet sem s >>= \case
      VSet vs ->
        mapM (bindExec i (tail xs) x) (S.toList vs)
        >>= (return . f)
      _       -> assert False undefined

  where
    bindExec i xr e v = do
      st <- get
      put st {
        tValues = IM.insert i v $ tValues st
        }
      r <- evalConditional sem fun f xr e
      put st
      return r

-----------------------------------------------------------------------------

evalRange
  :: Semantics -> Expr Int -> StateT ST (Either Error) (Int,Int)

evalRange s e = case expr e of
  Colon x y ->
    evalNum s x >>= \case
      VNumber a -> evalNum s y >>= \case
        VNumber b -> return (a,b)
        _         -> assert False undefined
      _         -> assert False undefined
  _         -> assert False undefined

-----------------------------------------------------------------------------

evalColon
  :: Semantics -> Evaluator (Expr Int)

evalColon s e = case expr e of
  Colon x y -> do
    st <- get
    a <- evalBool s x
    if a then do
      r <- evalExpr s y
      put st
      return r
    else do
      put st
      return VEmpty
  _ -> assert False undefined

-----------------------------------------------------------------------------

evalEquality
  :: Semantics -> (Value -> Value -> Bool) -> String -> Expr Int -> Expr Int -> ExprPos
  -> StateT ST (Either Error) (Either Value Bool)

evalEquality s eq eqs e1 e2 pos = do
  a <- evalExpr s e1
  b <- evalExpr s e2

  st <- get
  case (a,b) of
    (VEnum n i xs, VBus io l s)
      | l /= i     -> errBusCmp n s eqs l i pos
      | otherwise -> return $ Left $ VLtl $ opeq eq $ Or $
                    map (toB (delimiter st) io s [] i) xs
    (VBus io l s, VEnum n i xs)
      | l /= i     -> errBusCmp n s eqs l i pos
      | otherwise -> return $ Left $ VLtl $ opeq eq $ Or $
                    map (toB (delimiter st) io s [] i) xs
    (VBus io l s, VBus io' l' s')
      | l /= l'    -> errBusCmp s s' eqs l l' pos
      | otherwise ->
          return $ Left $ VLtl $ And
            [ Equiv (Atomic $ cio io $ s ++ delimiter st ++ show i)
              (Atomic $ cio io' $ s' ++ delimiter st ++ show i)
            | i <- [0,1..l-1] ]
    (VBus io l s, VSignal io' s')
      | l /= 1     -> errBusCmp s s' eqs l 1 pos
      | otherwise ->
          return $ Left $ VLtl $ Equiv
            (Atomic $ cio io $ s ++ delimiter st ++ "0")
            (Atomic $ cio io' s')
    (VSignal io' s', VBus io l s)
      | l /= 1     -> errBusCmp s s' eqs l 1 pos
      | otherwise ->
          return $ Left $ VLtl $ Equiv
            (Atomic $ cio io $ s ++ delimiter st ++ "0")
            (Atomic $ cio io' s')
    (VEnum _ 1 [f], VSignal io' s')  ->
      return $ Left $ VLtl $ case f 0 of
        Right () -> TTrue
        Left False -> Not $ Atomic $ cio io' s'
        Left True  -> Atomic $ cio io' s'
    (VSignal io' s', VEnum _ 1 [f])  ->
      return $ Left $ VLtl $ case f 0 of
        Right () -> TTrue
        Left False -> Not $ Atomic $ cio io' s'
        Left True  -> Atomic $ cio io' s'
    (VEnum n j _, VSignal _ s') ->
      errBusCmp n s' eqs j 1 pos
    (VSignal _ s', VEnum n j _) ->
      errBusCmp n s' eqs j 1 pos
    (VEnum {}, _) -> assert False undefined
    (VBus {}, _)  -> assert False undefined
    (_, VEnum {}) -> assert False undefined
    (_, VBus {})  -> assert False undefined
    _             ->
      if eq a b
      then return $ Right True
      else return $ Right False

  where
    opeq o
      | o VEmpty VEmpty = id
      | otherwise       = fNot

    toB _ _  _ a 0 _ = And a
    toB d io s a i f = case f (i-1) of
      Right ()    -> toB d io s a (i-1) f
      Left True  ->
        toB d io s ((Atomic $ cio io $ s ++ d ++ show (i-1)):a) (i-1) f
      Left False ->
        toB d io s ((Not $ Atomic $ cio io $ s ++ d ++ show (i-1)):a) (i-1) f

    cio STInput   = Input
    cio STOutput  = Output
    cio STGeneric = assert False undefined

-----------------------------------------------------------------------------

fmlValue
  :: Semantics -> [Expr Int] -> ExprPos -> Evaluator Int

fmlValue s args p i = do
  st <- get
  as <- mapM (evalExpr s) args
  let xs = zip as $ idArgs $ tLookup st ! i
  put st {
    tValues = foldl (\m (v,j) -> IM.insert j v m) (tValues st) xs
    }

  evalSet s (idBindings $ tLookup st ! i) >>= \case
    VSet a -> do
      put st
      let ys = filter (/= VEmpty) $ S.toList a
      if null ys then
        errNoMatch (idName $ tLookup st ! i) (map (prVal . fst) xs) p
      else
        return $ head ys
    _      -> assert False undefined

-----------------------------------------------------------------------------

vltl
  :: Value -> Formula

vltl x = case x of
  VLtl y -> y
  _      -> assert False undefined

-----------------------------------------------------------------------------

prVal
  :: Value -> String

prVal v = case v of
  VNumber x -> show x
  VLtl x -> asciiLTL x
  VSet x -> case S.toList x of
    []     -> "{}"
    (y:yr) -> "{ " ++ prVal y ++
             concatMap ((:) ',' . (:) ' ' . prVal) yr ++ " }"
  _ -> assert False undefined

-----------------------------------------------------------------------------

asciiLTL
  :: Formula -> String

asciiLTL fml = case fml of
  TTrue                   -> ptrue
  FFalse                  -> pfalse
  Not (Atomic (Input x))  -> pnot ++ prUO' (Atomic (Input x))
  Not (Atomic (Output x)) -> pnot ++ prUO' (Atomic (Output x))
  Atomic (Input x)        -> x
  Atomic (Output x)       -> x
  Not x                   -> pnot ++ prUO' x
  And []                  -> asciiLTL TTrue
  And [x]                 -> asciiLTL x
  And (x:xr)              -> prAnd' x ++ concatMap (((" " ++ pand ++ " ") ++)
                                                    . prAnd') xr
  Or []                   -> asciiLTL FFalse
  Or [x]                  -> asciiLTL x
  Or (x:xr)               -> prOr' x ++ concatMap (((" " ++ por ++ " ") ++)
                                                   . prOr') xr
  Implies x y             -> prOr' x ++ " " ++ pimplies ++ " " ++ prOr' y
  Equiv x y               -> prOr' x ++ " " ++ pequiv ++ " " ++ prOr' y
  Next x                  -> pnext ++ prUO' x
  StrongNext x            -> pstrongnext ++ prUO' x
  WeakNext x              -> pnext ++ prUO' x
  Previous x              -> pprevious ++ prUO' x
  Globally x              -> pglobally ++ prUO' x
  Finally x               -> pfinally ++ prUO' x
  Historically x          -> phistorically ++ prUO' x
  Once x                  -> ponce ++ prUO' x
  Until x y               -> prOr' x ++ " " ++ puntil ++ " " ++ prOr' y
  Release x y             -> prOr' x ++ " " ++ prelease ++ " " ++ prOr' y
  Weak x y                -> prOr' x ++ " " ++ pweak ++ " " ++ prOr' y
  Since x y               -> prOr' x ++ " " ++ psince ++ " " ++ prOr' y
  Triggered x y           -> prOr' x ++ " " ++ ptriggered ++ " " ++ prOr' y

  where
    parens x = "(" ++ x ++ ")"

    prUO p f = case f of
      And _       -> parens $ p f
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f

    prAnd p f = case f of
      Or _        -> parens $ p f
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f

    prOr p f = case f of
      Implies _ _ -> parens $ p f
      Equiv _ _   -> parens $ p f
      Until _ _   -> parens $ p f
      Release _ _ -> parens $ p f
      _              -> p f

    prUO' =  prUO asciiLTL
    prAnd' = prAnd asciiLTL
    prOr' = prOr asciiLTL

    ptrue = "true"
    pfalse = "false"
    pnot = "!"
    pand = "&&"
    por = "||"
    pimplies = "->"
    pequiv = "<->"
    pnext = "X"
    pstrongnext = "X[!]"
    pprevious = "Y"
    pglobally = "F"
    pfinally = "G"
    phistorically = "H"
    ponce = "O"
    puntil = "U"
    prelease = "R"
    pweak = "W"
    psince = "S"
    ptriggered = "T"

-----------------------------------------------------------------------------

-- | Overwrites a given parameter by the given new value.

overwriteParameter
  :: Specification -> (String, Int) -> Either Error Specification

overwriteParameter s (n,v) =
  case find ((n ==) . idName . (symboltable s !) . bIdent) $ parameters s of
  Nothing -> cfgError $ "Specification has no parameter: " ++ n
  Just b  -> do
    let b' = b {
          bVal = if null $ bVal b
                 then []
                 else [ Expr (BaseCon v) $ srcPos $ head $ bVal b ]
          }
    return s {
      parameters = map (replace b') $ parameters s,
      symboltable = updSymTable b' $ symboltable s
      }

  where
    replace b' b =
      if bIdent b == bIdent b'
      then b' else b

    updSymTable y t =
      A.amap (\x -> if idName x /= idName (t ! bIdent y) then x else x {
                 idBindings =
                    if null $ bVal y
                    then Expr (SetExplicit []) $ bPos y
                    else Expr (SetExplicit [head $ bVal y]) $ bPos y
                 }) t

-----------------------------------------------------------------------------

-- | Replaces at and prime symbols by their defined counterpart.

repap
  :: String -> String -> String -> String

repap atSym primeSym =
  concatMap (\x -> if x == '@' then atSym else [x]) .
  concatMap (\x -> if x == '\'' then primeSym else [x])

-----------------------------------------------------------------------------
