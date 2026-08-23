-----------------------------------------------------------------------------
{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- | 🍜 🍄 miso-mario
--
-- A small side-scrolling platformer written with miso.
--
--  * Frame-rate independent physics driven by @requestAnimationFrame@.
--  * Arrow keys / WASD to move, Up / W / Space to jump (hold for a higher jump).
--  * Platforms, question blocks (bump them from below!), coins, a pit and a flag.
--  * The camera follows Mario through a level wider than the screen.
--
module Main (main) where
-----------------------------------------------------------------------------
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
-----------------------------------------------------------------------------
import           Miso hiding (Phase, Style)
import           Miso (CSS(Style))
import           Miso.Lens
import           Miso.Html
import           Miso.Html.Property
import qualified Miso.CSS as CSS
import           Miso.CSS (StyleSheet, px, pct)
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component initialModel updateModel (\_ _ -> viewModel))
  { subs =
      [ keyboardSub Keys
      , rAFSub Tick
      ]
  , styles = [ Style (CSS.renderStyleSheet sheet) ]
  }
-----------------------------------------------------------------------------
-- * Model
-----------------------------------------------------------------------------
data Action
  = Keys !IntSet
  -- ^ The set of keys currently held (keyCodes), from 'keyboardSub'
  | Tick !Double
  -- ^ A @requestAnimationFrame@ timestamp in milliseconds
  | Restart
-----------------------------------------------------------------------------
data Direction = L | R
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Phase
  = Playing
  | Won
  deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model = Model
  { _x, _y      :: !Double
  -- ^ Mario's position in world space (bottom-left corner, y grows upwards)
  , _vx, _vy    :: !Double
  -- ^ Velocity in px / ms
  , _dir        :: !Direction
  , _grounded   :: !Bool
  , _coyote     :: !Double
  -- ^ Milliseconds left in which a jump is still allowed after leaving a ledge
  , _lastTime   :: !Double
  -- ^ Last rAF timestamp, 0 until the first frame
  , _walkClock  :: !Double
  -- ^ Accumulated walking time, drives the run animation
  , _input      :: !Input
  , _camera     :: !Double
  -- ^ World x coordinate of the left edge of the viewport
  , _coins      :: ![Coin]
  , _blocks     :: ![Block]
  , _score      :: !Int
  , _deaths     :: !Int
  , _elapsed    :: !Double
  -- ^ Milliseconds spent in the current run
  , _phase      :: !Phase
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Input = Input
  { left, right, jump :: !Bool
  , jumpUsed          :: !Bool
  -- ^ The current jump press has already been consumed (no auto bunny-hop)
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Coin = Coin
  { coinX, coinY :: !Double
  , coinTaken    :: !Bool
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Block = Block
  { blockX, blockY :: !Double
  , blockHit       :: !Bool
  -- ^ Question blocks turn into used blocks once bumped from below
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Static, solid world geometry. A platform is an axis-aligned box.
data Platform = Platform
  { pX, pY, pW, pH :: !Double
  , pKind          :: !PlatformKind
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data PlatformKind = Ground | Brick | Stair | Pipe
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- * Lenses
-----------------------------------------------------------------------------
x, y, vx, vy, lastTime, walkClock, camera, elapsed, coyote :: Lens Model Double
x         = lens _x         $ \m v -> m { _x = v }
coyote    = lens _coyote    $ \m v -> m { _coyote = v }
y         = lens _y         $ \m v -> m { _y = v }
vx        = lens _vx        $ \m v -> m { _vx = v }
vy        = lens _vy        $ \m v -> m { _vy = v }
lastTime  = lens _lastTime  $ \m v -> m { _lastTime = v }
walkClock = lens _walkClock $ \m v -> m { _walkClock = v }
camera    = lens _camera    $ \m v -> m { _camera = v }
elapsed   = lens _elapsed   $ \m v -> m { _elapsed = v }

dir :: Lens Model Direction
dir = lens _dir $ \m v -> m { _dir = v }

grounded :: Lens Model Bool
grounded = lens _grounded $ \m v -> m { _grounded = v }

input :: Lens Model Input
input = lens _input $ \m v -> m { _input = v }

coins :: Lens Model [Coin]
coins = lens _coins $ \m v -> m { _coins = v }

blocks :: Lens Model [Block]
blocks = lens _blocks $ \m v -> m { _blocks = v }

score :: Lens Model Int
score  = lens _score  $ \m v -> m { _score = v }

phase :: Lens Model Phase
phase = lens _phase $ \m v -> m { _phase = v }
-----------------------------------------------------------------------------
-- * Level
-----------------------------------------------------------------------------
-- | Size of one tile, in world pixels. The sprite is 37px; the world is
-- built out of 32px tiles and everything is scaled 2x by CSS.
tile :: Double
tile = 32

viewportW, viewportH :: Double
viewportW = 400
viewportH = 240

marioW, marioH :: Double
marioW = 24  -- collision box, narrower than the 37px sprite
marioH = 36

levelW :: Double
levelW = 212 * tile

flagX :: Double
flagX = 198 * tile + 14

-- | Mario falls into the void below this
killY :: Double
killY = -6 * tile

-- | Ground segments in tiles: (start, end exclusive). Gaps are pits,
-- matching World 1-1.
groundSegments :: [(Double, Double)]
groundSegments = [(0, 69), (71, 86), (89, 153), (155, 212)]

-- | Brick tiles (x, y) in tile units. y = 4 is the low row Mario can bump
-- from the ground, y = 8 the high row.
brickTiles :: [(Double, Double)]
brickTiles = concat
  [ [(20, 4), (22, 4), (24, 4)]
  , [(77, 4), (79, 4)]
  , [(x', 8) | x' <- [80 .. 87]]
  , [(91, 8), (92, 8), (93, 8), (94, 4)]
  , [(100, 4), (101, 4)]
  , [(118, 4)]
  , [(121, 8), (122, 8), (123, 8)]
  , [(128, 8), (131, 8), (129, 4), (130, 4)]
  , [(168, 4), (169, 4), (171, 4)]
  ]

-- | Staircase blocks: (x, height) columns of solid stair tiles.
stairColumns :: [(Double, Double)]
stairColumns = concat
  [ [(134 + i, i + 1) | i <- [0 .. 3]]
  , [(140 + i, 4 - i) | i <- [0 .. 3]]
  , [(148 + i, i + 1) | i <- [0 .. 3]], [(152, 4)]
  , [(155, 4)], [(156 + i, 3 - i) | i <- [0 .. 2]]
  , [(181 + i, i + 1) | i <- [0 .. 7]], [(189, 8)]
  ]

platforms :: [Platform]
platforms =
  [ Platform (a * tile) (-tile) ((b - a) * tile) tile Ground
  | (a, b) <- groundSegments
  ] ++
  [ Platform (tx * tile) ((ty - 1) * tile) tile tile Brick
  | (tx, ty) <- brickTiles
  ] ++
  [ Platform (tx * tile) 0 tile (h * tile) Stair
  | (tx, h) <- stairColumns
  ] ++
  [ Platform (tx * tile) 0 (2 * tile) (h * tile) Pipe
  | (tx, h) <- [(28, 2), (38, 3), (46, 4), (57, 4), (163, 2), (179, 2)]
  ]

initialBlocks :: [Block]
initialBlocks =
  [ Block (tx * tile) ((ty - 1) * tile) False
  | (tx, ty) <-
      [ (16, 4), (21, 4), (23, 4), (22, 8)
      , (78, 4), (94, 8)
      , (106, 4), (109, 4), (112, 4), (109, 8)
      , (129, 8), (130, 8)
      , (170, 4)
      ]
  ]

initialCoins :: [Coin]
initialCoins =
  [ Coin (tx * tile + 8) ((ty - 1) * tile + 4) False
  | (tx, ty) <-
      [ (18, 1), (19, 1)
      , (31, 1), (33, 3), (35, 1)
      , (41, 4), (43, 4)
      , (52, 1), (53, 1), (54, 1)
      , (63, 5), (64, 5), (65, 5)
      , (69, 3), (70, 3)
      , (83, 9), (84, 9), (85, 9)
      , (87, 3), (88, 3)
      , (97, 1), (98, 1)
      , (104, 5), (108, 6), (110, 6)
      , (115, 1), (116, 1)
      , (125, 1), (126, 1)
      , (138, 5), (139, 5)
      , (145, 1), (146, 1)
      , (153, 5), (154, 5)
      , (161, 1), (166, 3), (175, 1), (176, 1)
      , (192, 1), (193, 1), (194, 1), (195, 1)
      ]
  ]
-----------------------------------------------------------------------------
initialModel :: Model
initialModel = Model
  { _x = 2 * tile
  , _y = 0
  , _vx = 0
  , _vy = 0
  , _dir = R
  , _grounded = True
  , _coyote = 0
  , _lastTime = 0
  , _walkClock = 0
  , _input = Input False False False False
  , _camera = 0
  , _coins = initialCoins
  , _blocks = initialBlocks
  , _score = 0
  , _deaths = 0
  , _elapsed = 0
  , _phase = Playing
  }
-----------------------------------------------------------------------------
-- * Update
-----------------------------------------------------------------------------
updateModel :: Action -> Effect context props Model Action
updateModel = \case
  Keys keys -> do
    prevIn <- use input
    let nextIn = readInput keys
    -- A jump press is consumed once; it is re-armed when the key is released.
    input .= nextIn { jumpUsed = jump nextIn && jumpUsed prevIn }
    -- Enter / R restarts after winning
    p <- use phase
    when (p == Won && any (`IntSet.member` keys) [13, 82]) $
      put initialModel
  Restart ->
    put initialModel
  Tick t -> do
    prev <- use lastTime
    lastTime .= t
    p <- use phase
    -- Clamp dt so a backgrounded tab doesn't launch Mario into orbit.
    let dt = if prev == 0 then 0 else min 40 (t - prev)
    when (p == Playing && dt > 0) (modify (step dt))
  where
    when c m = if c then m else pure ()
-----------------------------------------------------------------------------
-- | Map raw keyCodes to game input. Arrows, WASD and Space are all supported.
readInput :: IntSet -> Input
readInput keys = Input
  { left  = held [37, 65]
  , right = held [39, 68]
  , jump  = held [38, 87, 32]
  , jumpUsed = False
  }
  where held = any (`IntSet.member` keys)
-----------------------------------------------------------------------------
-- | Advance the simulation by @dt@ milliseconds.
step :: Double -> Model -> Model
step dt
  = followCamera
  . checkWin
  . checkDeath
  . collectCoins
  . moveY dt
  . moveX dt
  . applyInput dt
  . (elapsed +~ dt)
-----------------------------------------------------------------------------
-- Tuning constants. Velocities are px / ms, accelerations px / ms².
walkSpeed, runAccel, friction, airControl, gravity, fallGravity, jumpSpeed, maxFall, coyoteTime :: Double
walkSpeed   = 0.12
runAccel    = 0.0008
friction    = 0.0012
airControl  = 0.5
gravity     = 0.0014  -- while rising with jump held
fallGravity = 0.0024  -- while falling, or rising after jump was released
jumpSpeed   = 0.60    -- ~4 tiles of height with jump held, ~2.3 tiles tapped
maxFall     = 0.45
coyoteTime  = 80      -- ms after leaving a ledge during which jumping is still allowed
-----------------------------------------------------------------------------
applyInput :: Double -> Model -> Model
applyInput dt m@Model{ _input = Input{..} } =
  m & vx .~ vx'
    & vy .~ vy'
    & dir .~ dir'
    & walkClock .~ clock'
    & grounded .~ (if jumping then False else _grounded m)
    & coyote .~ (if jumping then 0 else _coyote m)
    & input .~ (if jumping then (_input m) { jumpUsed = True } else _input m)
  where
    wantX | left && not right = -1
          | right && not left = 1
          | otherwise         = 0 :: Double
    accel = runAccel * (if _grounded m then 1 else airControl)
    target = wantX * walkSpeed
    cur = _vx m
    vx' | wantX /= 0 = approach cur target (accel * dt)
        | _grounded m = approach cur 0 (friction * dt)
        | otherwise = cur
    canJump = _grounded m || _coyote m > 0
    jumping = jump && canJump
    -- Variable-height jump: gravity is stronger once the key is released
    -- (or on the way down), instead of slicing the velocity.
    g | _vy m > 0 && jump = gravity
      | otherwise         = fallGravity
    vy' | jumping   = jumpSpeed
        | otherwise = max (-maxFall) (_vy m - g * dt)
    dir' | wantX < 0 = L
         | wantX > 0 = R
         | otherwise = _dir m
    clock' | _grounded m && abs vx' > 0.02 = _walkClock m + dt * (abs vx' / walkSpeed)
           | otherwise = 0

-- | Move @cur@ towards @target@ by at most @delta@.
approach :: Double -> Double -> Double -> Double
approach cur target delta
  | cur < target = min target (cur + delta)
  | otherwise    = max target (cur - delta)
-----------------------------------------------------------------------------
-- | Horizontal movement, resolving collisions against solid geometry.
moveX :: Double -> Model -> Model
moveX dt m = m & x .~ x' & vx .~ (if blocked then 0 else _vx m)
  where
    proposed = clamp 0 (levelW - marioW) (_x m + _vx m * dt)
    box = marioBox proposed (_y m)
    hits = [ b | b <- solids m, overlaps box b ]
    blocked = not (null hits)
    x' | null hits = proposed
       | _vx m > 0 = minimum [ bx b - marioW | b <- hits ]
       | otherwise = maximum [ bx b + bw b | b <- hits ]
-----------------------------------------------------------------------------
-- | Vertical movement: gravity, landing on things, bumping head on blocks.
moveY :: Double -> Model -> Model
moveY dt m =
  m & y .~ y'
    & vy .~ vy'
    & grounded .~ landed
    & coyote .~ coyote'
    & blocks .~ blocks'
    & score +~ bumpedScore
  where
    vy0 = _vy m
    proposed = _y m + vy0 * dt
    coyote' | landed      = coyoteTime
            | _grounded m = coyoteTime   -- just walked off a ledge
            | otherwise   = max 0 (_coyote m - dt)
    box = marioBox (_x m) proposed
    hits = [ b | b <- solids m, overlaps box b ]
    falling = vy0 <= 0
    landed = falling && not (null hits)
    y' | null hits = proposed
       | falling   = maximum [ by b + bh b | b <- hits ]
       | otherwise = minimum [ by b - marioH | b <- hits ]
    vy' | null hits = vy0
        | otherwise = 0
    -- Question blocks directly above Mario's head get bumped when rising.
    bumped = [ b | not falling, not (null hits), b <- _blocks m, not (blockHit b)
             , overlaps box (blockBox b) ]
    blocks' | null bumped = _blocks m
            | otherwise   = [ b { blockHit = blockHit b || b `elem` bumped } | b <- _blocks m ]
    bumpedScore = 200 * length bumped
-----------------------------------------------------------------------------
collectCoins :: Model -> Model
collectCoins m
  | got == 0  = m
  | otherwise = m & coins .~ coins' & score +~ (100 * got)
  where
    box = marioBox (_x m) (_y m)
    hit c = not (coinTaken c) && overlaps box (coinBox c)
    got = length (filter hit (_coins m)) :: Int
    coins' = [ if hit c then c { coinTaken = True } else c | c <- _coins m ]
-----------------------------------------------------------------------------
checkDeath :: Model -> Model
checkDeath m
  | _y m < killY =
      initialModel
        { _deaths = _deaths m + 1
        , _coins = _coins m
        , _blocks = _blocks m
        , _score = _score m
        , _lastTime = _lastTime m
        , _elapsed = _elapsed m
        , _input = (_input m) { jumpUsed = True }
        }
  | otherwise = m
-----------------------------------------------------------------------------
checkWin :: Model -> Model
checkWin m
  | _x m + marioW >= flagX = m & phase .~ Won & vx .~ 0
  | otherwise = m
-----------------------------------------------------------------------------
-- | Camera follows Mario, keeping him in the left third of the screen
-- while clamping to the level bounds.
followCamera :: Model -> Model
followCamera m = m & camera .~ clamp 0 (levelW - viewportW) target
  where target = _x m - viewportW / 3
-----------------------------------------------------------------------------
-- * Collision helpers
-----------------------------------------------------------------------------
-- | An axis-aligned box: x, y, w, h.
data Box = Box { bx, by, bw, bh :: !Double }

overlaps :: Box -> Box -> Bool
overlaps a b =
  bx a < bx b + bw b && bx a + bw a > bx b &&
  by a < by b + bh b && by a + bh a > by b

marioBox :: Double -> Double -> Box
marioBox mx my = Box mx my marioW marioH

platformBox :: Platform -> Box
platformBox Platform{..} = Box pX pY pW pH

blockBox :: Block -> Box
blockBox Block{..} = Box blockX blockY tile tile

coinBox :: Coin -> Box
coinBox Coin{..} = Box coinX coinY 16 24

-- | Static geometry, built once.
platformBoxes :: [Box]
platformBoxes = map platformBox platforms

-- | Solid boxes that could possibly touch Mario's box (cheap x broad-phase),
-- so the narrow-phase only looks at a handful of boxes per frame.
solids :: Model -> [Box]
solids m = filter nearby platformBoxes ++ filter nearby (map blockBox (_blocks m))
  where
    lo = _x m - 2 * tile
    hi = _x m + marioW + 2 * tile
    nearby b = bx b < hi && bx b + bw b > lo

clamp :: Double -> Double -> Double -> Double
clamp lo hi = max lo . min hi
-----------------------------------------------------------------------------
-- * View
-----------------------------------------------------------------------------
viewModel :: Model -> View context Model Action
viewModel m@Model{..} =
  div_ [ class_ "game" ]
    [ viewHud m
    , div_ [ class_ "viewport" ]
        [ div_ [ class_ "sky" ] (map viewCloud clouds)
        , div_ [ class_ "world", CSS.style_ [ CSS.transforms [ CSS.translateX (pxD (negate _camera)) ] ] ] $
            concat
              [ map viewBush (filter (\tx -> visible (tx * tile) 64) bushes)
              , map viewPlatform (filter (\p -> visible (pX p) (pW p)) platforms)
              , map viewBlock (filter (\b -> visible (blockX b) tile) _blocks)
              , [ viewCoin c | c <- _coins, not (coinTaken c), visible (coinX c) 16 ]
              , [ viewFlag | visible flagX 4 ]
              , [ viewMario m ]
              ]
        , if _phase == Won then viewWin m else text ""
        ]
    , p_ [ class_ "help" ]
        [ text "← → / A D to move · ↑ / W / Space to jump (hold for a higher jump) · bump ? blocks · reach the flag" ]
    ]
  where
    clouds = [ (40, 150, 1.2), (180, 190, 0.8), (300, 165, 1.0) ]
    bushes = [ 11, 23, 41, 59, 71, 89, 107, 119, 137, 155, 167, 185 ]
    -- Only render world objects within (a margin of) the camera window;
    -- this keeps the virtual DOM diff small every frame.
    visible ox w = ox + w >= _camera - tile && ox <= _camera + viewportW + tile
-----------------------------------------------------------------------------
viewHud :: Model -> View context Model Action
viewHud Model{..} =
  div_ [ class_ "hud" ]
    [ span_ [] [ text ("SCORE " <> pad 6 _score) ]
    , span_ [] [ text ("🪙 " <> ms taken <> "/" <> ms (length _coins)) ]
    , span_ [] [ text ("TIME " <> ms (floor (_elapsed / 1000) :: Int)) ]
    , span_ [] [ text ("💀 " <> ms _deaths) ]
    , button_ [ class_ "btn", onClick Restart ] [ text "↻" ]
    ]
  where
    taken = length (filter coinTaken _coins)
    pad n v = let s = ms v in ms (replicate (n - length (show v)) '0') <> s
-----------------------------------------------------------------------------
viewWin :: Model -> View context Model Action
viewWin Model{..} =
  div_ [ class_ "overlay" ]
    [ h2_ [] [ text "🏁 Course clear!" ]
    , p_ [] [ text ("Score " <> ms _score <> " · " <> ms (floor (_elapsed / 1000) :: Int) <> "s") ]
    , button_ [ class_ "btn", onClick Restart ] [ text "Play again (Enter)" ]
    ]
-----------------------------------------------------------------------------
viewMario :: Model -> View context Model Action
viewMario Model{..} =
  div_
    [ class_ "mario"
    , CSS.style_
        [ CSS.transforms
            [ CSS.translate (pxD (_x - (37 - marioW) / 2)) (pxD (negate _y - marioH))
            , CSS.scaleX (if _dir == L then -1 else 1)
            ]
        , CSS.backgroundPosition (px (negate (frame * 37)) <> " 0")
        ]
    ] []
  where
    -- Sprite sheet: 8 frames of 37px. 0 = stand, 1 = skid, 2..7 = run cycle.
    frame :: Int
    frame
      | not _grounded = 5
      | abs _vx < 0.02 = 0
      | skidding = 1
      | otherwise = 2 + (floor (_walkClock / 70) `mod` 6)
    skidding = (left _input && _vx > 0.05) || (right _input && _vx < -0.05)
-----------------------------------------------------------------------------
viewPlatform :: Platform -> View context Model Action
viewPlatform Platform{..} =
  div_ [ class_ cls, CSS.style_ (place pX pY pW pH) ] children
  where
    cls = case pKind of
      Ground -> "ground"
      Brick  -> "brick"
      Stair  -> "stair"
      Pipe   -> "pipe"
    children = case pKind of
      Pipe -> [ div_ [ class_ "pipe-top" ] [] ]
      _    -> []
-----------------------------------------------------------------------------
viewBlock :: Block -> View context Model Action
viewBlock Block{..} =
  div_ [ class_ (if blockHit then "block used" else "block"), CSS.style_ (place blockX blockY tile tile) ]
    [ text (if blockHit then "" else "?") ]
-----------------------------------------------------------------------------
viewCoin :: Coin -> View context Model Action
viewCoin Coin{..} = div_ [ class_ "coin", CSS.style_ (place coinX coinY 16 24) ] []
-----------------------------------------------------------------------------
viewFlag :: View context Model Action
viewFlag =
  div_ [ class_ "flagpole", CSS.style_ (place flagX 0 4 (9 * tile)) ]
    [ div_ [ class_ "flag" ] [] ]
-----------------------------------------------------------------------------
viewCloud :: (Double, Double, Double) -> View context Model Action
viewCloud (cx, cy, s) =
  div_ [ class_ "cloud"
       , CSS.style_
           [ CSS.left (pxD cx)
           , CSS.top (pxD (viewportH - cy))
           , CSS.transforms [ CSS.scale s ]
           ]
       ] []
-----------------------------------------------------------------------------
viewBush :: Double -> View context Model Action
viewBush tx = div_ [ class_ "bush", CSS.style_ (place (tx * tile) 0 64 24) ] []
-----------------------------------------------------------------------------
-- | Position an absolutely-positioned world element. World @y@ grows
-- upward from the ground line, so convert to a CSS @bottom@.
place :: Double -> Double -> Double -> Double -> [CSS.Style]
place px' py' w h =
  [ CSS.left (pxD px')
  , CSS.bottom (pxD (py' + tile))
  , CSS.width (pxD w)
  , CSS.height (pxD h)
  ]

pxD :: Double -> MisoString
pxD v = ms (fromIntegral (round v :: Int) :: Double) <> "px"
-----------------------------------------------------------------------------
-- * Styles
-----------------------------------------------------------------------------
sheet :: StyleSheet
sheet = CSS.sheet_
  [ CSS.selector_ "body"
      [ CSS.margin "0"
      , CSS.backgroundColor (CSS.Hex "1b1b2f")
      , CSS.color (CSS.Hex "fff")
      , CSS.fontFamily "ui-monospace, Menlo, Consolas, monospace"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.minHeight "100vh"
      ]
  , CSS.selector_ ".game"
      [ CSS.width (pxD (viewportW * 2))
      , CSS.maxWidth "100vw"
      , CSS.marginTop (px 16)
      ]
  , CSS.selector_ ".hud"
      [ CSS.display "flex"
      , CSS.justifyContent "space-between"
      , CSS.alignItems "center"
      , CSS.padding "4px 8px"
      , CSS.fontSize (px 18)
      , CSS.letterSpacing (px 1)
      ]
  , CSS.selector_ ".btn"
      [ CSS.fontFamily "inherit"
      , CSS.fontSize (px 16)
      , CSS.padding "4px 12px"
      , CSS.border "2px solid #fff"
      , CSS.borderRadius (px 4)
      , CSS.backgroundColor (CSS.Hex "e52521")
      , CSS.color (CSS.Hex "fff")
      , CSS.cursor "pointer"
      ]
  , CSS.selector_ ".viewport"
      [ CSS.position "relative"
      , CSS.width (pxD viewportW)
      , CSS.height (pxD viewportH)
      , CSS.overflow "hidden"
      , CSS.backgroundColor (CSS.Hex "5c94fc")
      , CSS.border "4px solid #fff"
      , CSS.borderRadius (px 6)
      , CSS.transforms [ CSS.scale 2 ]
      , CSS.transformOrigin "top left"
      -- The 2x transform doesn't affect layout, so reserve the extra
      -- height (plus the scaled 4px border and a gap) explicitly.
      , CSS.marginBottom (pxD (viewportH + 8 + 16))
      , CSS.imageRendering "pixelated"
      ]
  , CSS.selector_ ".sky, .world"
      [ CSS.position "absolute"
      , CSS.left "0"
      , CSS.top "0"
      , CSS.width (pct 100)
      , CSS.height (pct 100)
      ]
  , CSS.selector_ ".world"
      [ CSS.width (pxD levelW)
      , CSS.willChange "transform"
      ]
  , CSS.selector_ ".world > div"
      [ CSS.position "absolute"
      , CSS.boxSizing "border-box"
      ]
  , CSS.selector_ ".mario"
      [ CSS.left "0"
      , CSS.top (pxD (viewportH - tile))
      , CSS.width (px 37)
      , CSS.height (px 37)
      , CSS.backgroundImage (CSS.url "assets/mario.png")
      , CSS.backgroundRepeat "no-repeat"
      , CSS.willChange "transform"
      , CSS.zIndex 10
      ]
  , CSS.selector_ ".ground"
      [ CSS.backgroundColor (CSS.Hex "c84c0c")
      , CSS.backgroundImage "linear-gradient(#e8a060 2px, transparent 2px), linear-gradient(90deg, #8a2c00 1px, transparent 1px)"
      , CSS.backgroundSize "32px 32px"
      , CSS.borderTop "2px solid #e8a060"
      ]
  , CSS.selector_ ".brick"
      [ CSS.backgroundColor (CSS.Hex "b5471d")
      , CSS.backgroundImage "linear-gradient(#5a1c00 2px, transparent 2px), linear-gradient(90deg, #5a1c00 2px, transparent 2px)"
      , CSS.backgroundSize "32px 16px"
      , CSS.boxShadow "inset 0 -2px 0 #5a1c00"
      ]
  , CSS.selector_ ".stair"
      [ CSS.backgroundColor (CSS.Hex "c84c0c")
      , CSS.backgroundImage "linear-gradient(#e8a060 2px, transparent 2px), linear-gradient(90deg, #e8a060 2px, transparent 2px), linear-gradient(transparent 30px, #5a1c00 30px), linear-gradient(90deg, transparent 30px, #5a1c00 30px)"
      , CSS.backgroundSize "32px 32px"
      ]
  , CSS.selector_ ".pipe"
      [ CSS.backgroundColor (CSS.Hex "3cb043")
      , CSS.backgroundImage "linear-gradient(90deg, #8ff58f 6px, transparent 6px, transparent 52px, #1d6e22 52px)"
      , CSS.border "2px solid #0b3d0f"
      , CSS.borderBottom "none"
      ]
  , CSS.selector_ ".pipe-top"
      [ CSS.position "absolute"
      , CSS.left (px (-6))
      , CSS.top (px (-2))
      , CSS.width "calc(100% + 12px)"
      , CSS.height (px 16)
      , CSS.backgroundColor (CSS.Hex "3cb043")
      , CSS.backgroundImage "linear-gradient(90deg, #8ff58f 6px, transparent 6px, transparent 64px, #1d6e22 64px)"
      , CSS.border "2px solid #0b3d0f"
      , CSS.borderRadius (px 2)
      , CSS.boxSizing "border-box"
      ]
  , CSS.selector_ ".block"
      [ CSS.backgroundColor (CSS.Hex "f8b800")
      , CSS.border "3px solid #8a4a00"
      , CSS.boxShadow "inset -3px -3px 0 #c86c00"
      , CSS.color (CSS.Hex "8a4a00")
      , CSS.fontWeight "bold"
      , CSS.fontSize (px 20)
      , CSS.textAlign "center"
      , CSS.lineHeight (px 26)
      , CSS.animation "blink 1s steps(2) infinite"
      ]
  , CSS.selector_ ".block.used"
      [ CSS.backgroundColor (CSS.Hex "9c6a3c")
      , CSS.boxShadow "inset -3px -3px 0 #5a3a1c"
      , CSS.animation "none"
      ]
  , CSS.selector_ ".coin"
      [ CSS.backgroundColor (CSS.Hex "ffd400")
      , CSS.border "2px solid #b07800"
      , CSS.borderRadius (pct 50)
      , CSS.boxShadow "inset 3px 0 0 #fff3a0"
      , CSS.animation "spin 0.8s ease-in-out infinite"
      ]
  , CSS.selector_ ".flagpole"
      [ CSS.backgroundColor (CSS.Hex "d0d0d0")
      , CSS.borderRadius (px 2)
      ]
  , CSS.selector_ ".flag"
      [ CSS.position "absolute"
      , CSS.right (px 4)
      , CSS.top (px 6)
      , CSS.width "0"
      , CSS.height "0"
      , CSS.borderTop "14px solid transparent"
      , CSS.borderBottom "14px solid transparent"
      , CSS.borderRight "28px solid #e52521"
      ]
  , CSS.selector_ ".cloud"
      [ CSS.position "absolute"
      , CSS.width (px 56)
      , CSS.height (px 20)
      , CSS.backgroundColor (CSS.Hex "fff")
      , CSS.borderRadius (px 20)
      , CSS.boxShadow "14px -10px 0 2px #fff, 30px -6px 0 0 #fff"
      , CSS.opacity 0.9
      , CSS.animation "drift 40s linear infinite"
      ]
  , CSS.selector_ ".bush"
      [ CSS.backgroundColor (CSS.Hex "1d9e2a")
      , CSS.borderRadius "32px 32px 0 0"
      , CSS.boxShadow "inset 0 6px 0 #4fd35e"
      ]
  , CSS.selector_ ".overlay"
      [ CSS.position "absolute"
      , CSS.left "0"
      , CSS.top "0"
      , CSS.width (pct 100)
      , CSS.height (pct 100)
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.backgroundColor (CSS.rgba 0 0 0 0.55)
      , CSS.textAlign "center"
      , CSS.zIndex 20
      ]
  , CSS.selector_ ".overlay h2" [ CSS.margin "0 0 8px", CSS.fontSize (px 20) ]
  , CSS.selector_ ".overlay p"  [ CSS.margin "0 0 12px", CSS.fontSize (px 12) ]
  , CSS.selector_ ".overlay .btn" [ CSS.fontSize (px 10) ]
  , CSS.selector_ ".help"
      [ CSS.textAlign "center"
      , CSS.opacity 0.7
      , CSS.fontSize (px 13)
      ]
  , CSS.keyframes_ "blink"
      [ CSS.from_ [ CSS.backgroundColor (CSS.Hex "f8b800") ]
      , CSS.to_   [ CSS.backgroundColor (CSS.Hex "ffd860") ]
      ]
  , CSS.keyframes_ "spin"
      [ CSS.from_ [ CSS.transforms [ CSS.scaleX 1 ] ]
      , CSS.at (pct 50) [ CSS.transforms [ CSS.scaleX 0.2 ] ]
      , CSS.to_   [ CSS.transforms [ CSS.scaleX 1 ] ]
      ]
  , CSS.keyframes_ "drift"
      [ CSS.from_ [ CSS.marginLeft (px 0) ]
      , CSS.to_   [ CSS.marginLeft (px (-120)) ]
      ]
  ]
-----------------------------------------------------------------------------
