module ReactiveMarkup.Runners.StateTree where

import System.Mem.StableName
import qualified Data.Text as T

import ReactiveMarkup
import Data.Coerce (coerce)
import Data.IORef (readIORef, newIORef)

newtype StableName' = StableName' (StableName ()) deriving Eq

makeStableName' :: a -> IO StableName'
makeStableName' = fmap coerce . makeStableName

instance Show StableName' where
  show (StableName' stableName) = show $ hashStableName stableName

data StateTree
  = SingleWidget WidgetDescription
  | ContainerWidget WidgetDescription (StableName', [StateTree])
  deriving (Eq, Show)

data WidgetDescription = WidgetDescription
  { widgetType :: StateTreeElement,
    widgetAttributes :: StableName'
  } deriving (Eq, Show)


data Transition =
  Keep | Delete | Add | Replace | Modify AttributeTransition [Transition] deriving Show

data AttributeTransition = AttrKeep | AttrReplace deriving Show

computeTransition :: StateTree -> StateTree -> Transition
computeTransition (SingleWidget _) (ContainerWidget _ _) = Replace
computeTransition (ContainerWidget _ _) (SingleWidget _) = Replace
computeTransition (SingleWidget wd1) (SingleWidget wd2)
  | wd1 == wd2 = Keep
  | widgetType wd1 == widgetType wd2 = Modify AttrReplace []
  | otherwise = Replace
computeTransition (ContainerWidget wd1 (s1,c1)) (ContainerWidget wd2 (s2,c2))
  | wd1 == wd2 && s1 == s2 = Keep
  | wd1 == wd2 = Modify AttrKeep $ 
      let children = zipWith computeTransition c1 c2
      in if length children <= length c1 
        then children ++ take (length c1 - length children) (repeat Delete)
        else children ++ take (length c2 - length children) (repeat Add)
  | widgetType wd1 /= widgetType wd2 = Replace
  | s1 == s2 = Modify AttrReplace $ const Keep <$> c1
  | otherwise = Replace


type StateTreeElements a =
  [ Label ('[Text] |-> BasicStyling),
    List ('[Orientation] |-> Expandable),
    Button ('[Text, Activate, Click] |-> Expandable |-> BasicStyling),
    ToggleButton '[Toggle],
    DynamicState,
    DynamicStateIO,
    DynamicMarkup,
    HandleEvent,
    HandleEventIO,
    FlowLayout '[Orientation],
    GridLayout [HomogenousRows, HomogenousColumns],
    TextInput ('[Text, TextChange, Activate] |-> BasicStyling),
    HotKey,
    DrawingBoard '[DrawDiagram a, DrawDynamicDiagram a, MouseClickWithPosition, AspectRatio],
    Notebook,
    Menu
  ]

type Expandable = '[HorizontalExpand, VerticalExpand]

type BasicStyling = '[FontSize, FontWeight, FontStyle, FontColour, BackgroundColour]

data StateTreeElement = STLabel | STList deriving (Eq, Enum, Ord, Show)

runLabel :: Runner '[Label ('[Text] |-> BasicStyling)] Void (IO StateTree)
runLabel = simpleRun $ \(Label options) -> do
  SingleWidget . WidgetDescription STLabel . coerce <$> makeStableName' options

runList :: Runner '[List ('[Orientation] |-> Expandable)] Void (IO StateTree)
runList = simpleRun $ \(List options children) -> do
  SingleWidget . WidgetDescription STList . coerce <$> makeStableName' options
