{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Application (runApp) where

import Control.Lens ( (&), (.~), (?~), makeLenses, element )
import Control.Monad ( unless )
import Data.Ord ( clamp )
import Data.Text (Text, append, concat, init, intercalate, lines, pack, splitOn, unpack)
import Data.Default ( Default(def) )
import Data.Maybe (fromMaybe)
import Monomer
import Monomer.Core.Themes.BaseTheme (BaseThemeColors (..), baseTheme)
import System.Clipboard ( getClipboardString )
import System.IO ( readFile )
import Tikka

import Monomer.Lens qualified as L
import TextAreaScroll qualified as T

import System.Info (os)
import System.FilePath ((</>), (<.>), takeDirectory)

-- Application model / state
data AppModel = AppModel
  { _dirFilePath :: Text
  , _filePath :: Text
  , _code :: Text
  , _trace :: Text
  , _errors :: Text
  , _tracingFlag :: Int
  , _verbosityFlag :: Int
  , _dumpFlag :: Text
  , _stcFlag :: Bool
  , _visibleTrace :: [Bool]
  , _lineNumbers :: Text
  , _command :: Text
  , _commandMemory :: [Text]
  , _memoryLocation :: Int
  , _isDarkMode :: Bool
  }
  deriving (Eq, Show)

-- Application events
data AppEvent
  = AppInit
  | AppIgnore
  | RunCode
  | SaveCode
  | LoadCode
  | WriteCode Text
  | WriteTraceAndError Text Text
  | OpenTrace Int
  | CodeChanged Text
  | ConsoleCommand Bool
  | ClearCommand Text
  | WriteTraceAndErrorConsole Text Text
  | UpdateScroll ScrollMessage
  | PasteCode
  | PasteCommand
  | PasteToCode Text
  | PasteToCommand Text
  | IncMemoryLocation
  | DecMemoryLocation
  | CommandChange Text
  deriving (Eq, Show)

makeLenses 'AppModel

-- Converts verbosity level to text for the dropdown
verbFlagToText :: Int -> Text
verbFlagToText 1 = "No Explanation"
verbFlagToText 2 = "Concise"
verbFlagToText 3 = "Concise Detail"
verbFlagToText 4 = "Conversational"
verbFlagToText _ = error "Unknown verbosity level"

-- Converts trace level to text for the dropdown
traceFlagToText :: Int -> Text
traceFlagToText 1 = "No Trace"
traceFlagToText 2 = "Reduced Case Selection"
traceFlagToText 3 = "Reduced Operations"
traceFlagToText 4 = "Full"
traceFlagToText _ = error "Unknown tracing level"

-- Builds the widget for a single row of the trace output
-- 1st arg = App model
-- 2nd arg = Row's step index
-- 3rd arg = Row's step text (console-printed trace)
traceRow :: AppModel -> Int -> Text -> WidgetNode AppModel AppEvent
traceRow model i t = row
 where
  rowSep = rgbaHex "#A9A9A9" 0.5
  rowBg = rgbaHex "#A9A9A9" 0.8
  traceOutput = head (splitOn "    [" t)
  traceExplanation = if null $ tail (splitOn "    [" t) then "" else Data.Text.init $ head $ tail (splitOn "    [" t)

  row =
    vstack
      [ hstack
          [ button_ traceOutput (OpenTrace i) [ignoreTheme] `styleBasic` [textSize 16, padding 10, textColor (fromMaybe white $ _txsFontColor $ fromMaybe mempty $ _sstText $ _thsLabelStyle $ _themeBasic $ fst $ activeTheme (_isDarkMode model))] ]
          `styleBasic` [borderB 1 rowSep]
          `styleHover` [bgColor rowBg]
      , label traceExplanation `styleBasic` [padding 10] `nodeVisible` (traceExplanation /= "" && _visibleTrace model !! i)
      ]

-- Gets the active theme, dark or light mode
activeTheme :: Bool -> (Theme, Theme)
activeTheme True = (appDarkTheme, lineNumberDarkTheme)
activeTheme False = (appLightTheme, lineNumberLightTheme)

-- Builds the entire UI as a widget
buildUI :: FilePath -> WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI p _ model = widgetTree
 where
  widgetTree =
    keystroke [("Ctrl-s", SaveCode)] $ themeSwitch_ (fst $ activeTheme (_isDarkMode model)) [themeClearBg] $
      vstack
        [ hstack
            [ vstack [ hstack
                [ mainButton "Run" RunCode `styleBasic` [height 70]
                , spacer
                , vstack
                    [ button "Load" LoadCode `styleBasic` [height 35]
                    , spacer
                    , button "Save" SaveCode `styleBasic` [height 35]
                    ]
                ] ]
            , spacer
            , vstack
                [ label (pack ("Base file location: " ++ unpack (_dirFilePath model)))
                , hstack
                    [ box_ [sizeReqUpdater (\(_, y) -> (width 230, y))] $
                        vstack
                          [ label "Trace level:"
                          , spacer
                          , textDropdown_ tracingFlag [1, 2, 3, 4] traceFlagToText []
                          ]
                    , spacer
                    , box_ [sizeReqUpdater (\(_, y) -> (width 155, y))] $
                        vstack
                          [ label "Verbosity:"
                          , spacer
                          , textDropdown_ verbosityFlag [1, 2, 3, 4] verbFlagToText []
                          ]
                    , spacer
                    , box $
                        vstack
                          [ label "Load/Save location:"
                          , spacer
                          , textField filePath
                          ]
                    , spacer
                    , box $
                        vstack
                          [ label "De-sugared code dump:"
                          , spacer
                          , textField dumpFlag
                          ]
                    , spacer
                    , vstack
                        [ label "STC:"
                        , spacer
                        , box_ [sizeReqUpdater (\(x, _) -> (x, height 34)), alignCenter] $ checkbox stcFlag `styleBasic` [fgColor highlightColor, hlColor highlightColor]
                        ]
                        `styleBasic` [padding 10]
                    ]
                ]
            ]
        , spacer
        , vsplit_
            [splitHandleSize 10]
            ( vstack
                [ hsplit_
                    [splitHandleSize 10]
                    ( box_ [sizeReqUpdater (const (maxWidth (20000 / 3), flexHeight 10000))] $
                        hstack
                          [ vstack
                              [ label "Code" `styleBasic` [padding 5]
                              , scroll
                                  ( hstack
                                      [ themeSwitch (snd $ activeTheme (_isDarkMode model)) $ box_ [sizeReqUpdater (\(_, y) -> (width 50, y))] $ textArea_ lineNumbers [readOnly] `nodeEnabled` False
                                      , keystroke_ [("Ctrl-v", PasteCode)] [ignoreChildrenEvts] $ scroll_ [scrollStyle L.textAreaStyle, scrollFwdStyle scrollFwdDefault] $ T.textArea_ code [onChange CodeChanged, acceptTab] UpdateScroll `nodeKey` "Code"
                                      ]
                                  )
                                  `nodeKey` "CodeScroll"
                                  `styleBasic` [padding 5]
                              ]
                          , separatorLine
                          ]
                    , hstack
                        [ separatorLine
                        , vstack
                            [ label_ (if _verbosityFlag model > 1 then "Trace (click to see explanation)" else "Trace") [ellipsis] `styleBasic` [padding 5, maxWidth 1000]
                            , box_ [sizeReqUpdater (const (maxWidth (10000 / 3), flexHeight 10000))] $ scroll $ vstack (traceRows model 0 (filter (/= "") $ Data.Text.lines $ _trace model)) `styleBasic` [padding 5]
                            ]
                        ]
                    )
                    `styleBasic` []
                , separatorLine
                ]
            , vstack
                [ separatorLine
                , label "Console" `styleBasic` [padding 5]
                , box_ [sizeReqUpdater (const (maxWidth 10000, minHeight 110))] $ textArea_ errors [readOnly] `styleBasic` [padding 5]
                ]
            )
        , spacer
        , hstack
            [ label ">>> "
            , keystroke_ [("Enter", ConsoleCommand (_command model == "")), ("Ctrl-v", PasteCommand)] [ignoreChildrenEvts] $ keystroke [("Up", DecMemoryLocation), ("Down", IncMemoryLocation)] $ textField_ command [placeholder "Define / run function, 'clear' to wipe memory", onChange CommandChange]
            , spacer_ [width 5]
            , zstack
              [ image_ (pack $ p </> "icons" </> "light-dark-mode" <.> "png") [fitEither] `styleBasic` [width 32, height 32] -- "Night mode" icon created by Freepik - Flaticon
              , toggleButton_ " " isDarkMode [toggleButtonOffStyle darkToggleOffStyle] `styleBasic` [bgColor transparent, textColor transparent, border 0 transparent]
              ]
            ]
        ]
        `styleBasic` [padding 10]

-- Helper for the dark mode toggle's style when pressed (i.e. app currently in dark mode)
darkToggleOffStyle :: Style
darkToggleOffStyle = def & L.basic ?~ (def & L.bgColor ?~ transparent)

-- Builds all trace output rows from a trace
traceRows :: AppModel -> Int -> [Text] -> [WidgetNode AppModel AppEvent]
traceRows _ _ [] = []
traceRows model i (t : ts) = traceRow model i t : traceRows model (i + 1) ts

-- Handles events thrown by UI widgets
handleEvent ::
  WidgetEnv AppModel AppEvent ->
  WidgetNode AppModel AppEvent ->
  AppModel ->
  AppEvent ->
  [AppEventResponse AppModel AppEvent]
handleEvent _ _ model evt = case evt of
  AppInit -> []
  AppIgnore -> []
  RunCode -> [Task (runCode (unpack $ _dirFilePath model) fullFilePath (Flags{tracing = _tracingFlag model, verbosity = _verbosityFlag model, dump = unpack (_dumpFlag model), stc = _stcFlag model, visual = True, repl = False}) (unpack $ _code model))]
  SaveCode -> [Task $ saveCode fullFilePath (unpack $ _code model)]
  LoadCode -> [Task $ loadCode fullFilePath]
  WriteCode c -> [Model (model & code .~ c & lineNumbers .~ getLineNumbers c)]
  WriteTraceAndError toPrint err -> [Model (model & errors .~ append err "\n\n" & trace .~ toPrint & visibleTrace .~ replicate (Prelude.length $ Data.Text.lines toPrint) False)]
  OpenTrace i -> [Model (model & (visibleTrace .~ (_visibleTrace model & element i .~ not (_visibleTrace model !! i))))]
  CodeChanged c -> [Model (model & lineNumbers .~ getLineNumbers c)]
  ConsoleCommand True -> []
  ConsoleCommand False -> [Model (model & commandMemory .~ _commandMemory model ++ [""] & memoryLocation .~ length (_commandMemory model)), Task $ consoleCommand model (Flags{tracing = _tracingFlag model, verbosity = _verbosityFlag model, dump = unpack (_dumpFlag model), stc = _stcFlag model, visual = True, repl = False}) (unpack $ _dirFilePath model) (takeDirectory fullFilePath) (unpack $ _code model) (unpack $ _command model)]
  ClearCommand c -> [Model (model & command .~ "" & errors .~ append (_errors model) c)]
  WriteTraceAndErrorConsole toPrint err -> [Model (model & errors .~ err & trace .~ toPrint & visibleTrace .~ replicate (Prelude.length $ Data.Text.lines toPrint) False & command .~ "" & errors .~ append (_errors model) (append (append (append (head (Data.Text.lines toPrint)) "\n\n") (last (Data.Text.lines toPrint))) "\n\n"))]
  UpdateScroll msg -> [Message "CodeScroll" msg]
  PasteCode -> [Task pasteCode]
  PasteCommand -> [Task pasteCommand]
  PasteToCode t -> [Message "Code" (T.Insert t)]
  PasteToCommand t -> [Model (model & command .~ append (_command model) t)]
  IncMemoryLocation -> [Model (model & memoryLocation .~ min (_memoryLocation model + 1) (length (_commandMemory model) - 1) & command .~ (_commandMemory model !! min (_memoryLocation model + 1) (length (_commandMemory model) - 1)))]
  DecMemoryLocation -> [Model (model & memoryLocation .~ max (_memoryLocation model - 1) 0 & command .~ (_commandMemory model !! max (_memoryLocation model - 1) 0))]
  CommandChange t -> [Model (model & commandMemory .~ (_commandMemory model & element (_memoryLocation model) .~ t))]
  where
    fullFilePath = unpack (_dirFilePath model) </> unpack (_filePath model)

-- Event called when pressing "Run"
runCode :: FilePath -> FilePath -> Flags -> String -> IO AppEvent
runCode dirFP fp fs s = do
  saveCode fp s
  o <- parseAndEvaluateProject fs s (takeDirectory fp) "main"
  case o of
    Left err -> pure (WriteTraceAndError "" (pack err))
    Right (toDump, toPrint, err) -> do
      unless (dump fs == "") $ writeFile (dirFP </> dump fs) toDump
      pure $ WriteTraceAndError (intercalate "\n" (map pack toPrint)) (pack err)

-- Event called when pressing "Save"
saveCode :: FilePath -> String -> IO AppEvent
saveCode "" _ = pure AppIgnore
saveCode fp s = writeFile fp s >> pure AppIgnore

-- Event called when pressing "Load"
loadCode :: FilePath -> IO AppEvent
loadCode "" = pure AppIgnore
loadCode fp = do
  c <- System.IO.readFile fp
  pure $ WriteCode (pack c)

-- Creates string of newline-separated line numbers
getLineNumbers :: Text -> Text
getLineNumbers c = Data.Text.concat [pack (show n ++ "\n") | n <- [1 .. (Prelude.length (Data.Text.lines c))]]

-- Event called when text entered to the console
consoleCommand :: AppModel -> Flags -> FilePath -> FilePath -> String -> String -> IO AppEvent
consoleCommand model fs dirFP fp file s = do
  exePath <- getExecutablePath'
  let dirPath p = takeDirectory (takeDirectory exePath) </> p

  if s == "clear"
    then writeFile (dirPath "imports/local-decl") "" >> pure (ClearCommand "Repl declarations cleared\n\n")
    else do
      case isFunction (pack s) of
        Just _ -> runCommand fs dirFP fp file s
        Nothing -> addToLocalDecl s >> pure (ClearCommand (append (_command model) "\n\n"))

-- Event called when a command is to be executed from the console
runCommand :: Flags -> FilePath -> FilePath -> String -> String -> IO AppEvent
runCommand fs dirFP fp s n = do
  exePath <- getExecutablePath'
  let dirPath p = takeDirectory (takeDirectory exePath) </> p

  writeFile (dirPath "imports/command-line") ("commandLineREPL = " ++ n ++ "\n")
  o <- parseAndEvaluateProject fs s fp "commandLineREPL"
  case o of
    Left err -> pure (WriteTraceAndError "" (pack err))
    Right (toDump, toPrint, err) -> do
      unless (dump fs == "") $ writeFile (dirFP </> dump fs) toDump
      pure $ WriteTraceAndErrorConsole (intercalate "\n" (map pack (n : toPrint))) (pack err)

-- Event called when Ctrl+V is detected in the code input
pasteCode :: IO AppEvent
pasteCode = do
  t <- getClipboardString
  case t of
    Nothing -> pure AppIgnore
    Just x -> pure $ PasteToCode (pack x)

-- Event called when Ctrl+V is detected in the console
pasteCommand :: IO AppEvent
pasteCommand = do
  t <- getClipboardString
  case t of
    Nothing -> pure AppIgnore
    Just x -> pure $ PasteToCommand (pack x)

-- Entry point to the application
runApp :: Flags -> [String] -> FilePath -> IO ()
runApp flags args fp = do
  let fp' = head $ pack (takeDirectory (head args)) `splitOn` pack fp
  let f = if null args then "" else unpack fp' </> head args
  file <- if null f then pure "" else System.IO.readFile f

  exePath <- getExecutablePath'
  let assetPath = takeDirectory (takeDirectory exePath) </> "assets"
  startApp (model fp' (pack $ head args) flags (pack file)) handleEvent (buildUI assetPath) (config assetPath)
 where
  config p =
    if os == "mingw32"
      then
        [ appWindowState (MainWindowNormal (1000, 600))
        , appWindowTitle "Tikka"
        , appWindowIcon $ pack $ p </> "icons" </> "lambda" <.> "bmp" -- Term icon created by Freepik - Flaticon
        , appTheme appLightTheme
        , appFontDef "Regular" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Medium" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Bold" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Italic" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appInitEvent AppInit
        ]
      else
        [ appWindowState (MainWindowNormal (1000, 600))
        , appWindowTitle "Tikka"
        , appWindowIcon $ pack $ p </> "icons" </> "lambda" <.> "bmp" -- Term icon created by Freepik - Flaticon
        , appDisableAutoScale True
        , appTheme appLightTheme
        , appFontDef "Regular" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Medium" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Bold" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appFontDef "Italic" $ pack $ p </> "fonts" </> "Consolas" <.> "ttf"
        , appInitEvent AppInit
        ]
  model fp' f fs c =
    AppModel
      { _dirFilePath = fp'
      , _filePath = f
      , _code = c
      , _trace = ""
      , _errors = ""
      , _tracingFlag = clamp (1, 3) (tracing fs)
      , _verbosityFlag = clamp (1, 4) (verbosity fs)
      , _dumpFlag = pack $ dump fs
      , _stcFlag = stc fs
      , _visibleTrace = [False]
      , _lineNumbers = getLineNumbers c
      , _command = ""
      , _commandMemory = [""]
      , _memoryLocation = 0
      , _isDarkMode = True
      }
-- Light mode primary color
lightColor :: Color
lightColor = rgbHex "f7f7f7"

-- Dark mode primary color
darkColor :: Color
darkColor = rgbHex "404040"

-- Highlight color
highlightColor :: Color
highlightColor = rgbHex "c27e00"

-- Darker highlight color
darkHighlightColor :: Color
darkHighlightColor = rgbHex "c26b00"

-- Lighter highlight color
lightHighlightColor :: Color
lightHighlightColor = rgbHex "c28800"

-- Application light theme
appLightTheme :: Theme
appLightTheme =
  baseTheme
    lightThemeColors
      { clearColor = lightColor
      , inputBgBasic = lightColor
      , inputFocusBorder = highlightColor
      , btnFocusBorder = highlightColor
      , btnMainFocusBorder = darkHighlightColor
      , btnMainBgBasic = highlightColor
      , btnMainBgHover = lightHighlightColor
      , btnMainBgFocus = highlightColor
      , btnMainBgActive = darkHighlightColor
      , slNormalFocusBorder = highlightColor
      , slSelectedFocusBorder = highlightColor
      }

-- Theme for the line number text box when in light mode
lineNumberLightTheme :: Theme
lineNumberLightTheme =
  baseTheme
    lightThemeColors
      { inputBgBasic = lightColor
      , inputBgDisabled = lightColor
      , inputTextDisabled = rgbHex "a3a3a3"
      , inputBorder = lightColor
      , scrollBarBasic = lightColor
      , scrollBarHover = lightColor
      , scrollThumbBasic = lightColor
      , scrollThumbHover = lightColor
      }

-- Application dark theme
appDarkTheme :: Theme
appDarkTheme =
  baseTheme
    darkThemeColors
      { clearColor = darkColor
      , inputBgBasic = rgbHex "505050"
      , inputFocusBorder = highlightColor
      , btnFocusBorder = highlightColor
      , btnMainFocusBorder = darkHighlightColor
      , btnMainBgBasic = highlightColor
      , btnMainBgHover = lightHighlightColor
      , btnMainBgFocus = highlightColor
      , btnMainBgActive = darkHighlightColor
      , slNormalFocusBorder = highlightColor
      , slSelectedFocusBorder = highlightColor
      }

-- Theme for the line number text box when in dark mode
lineNumberDarkTheme :: Theme
lineNumberDarkTheme =
  baseTheme
    darkThemeColors
      { inputBgBasic = darkColor
      , inputBgDisabled = darkColor
      , inputTextDisabled = rgbHex "a3a3a3"
      , inputBorder = darkColor
      , scrollBarBasic = darkColor
      , scrollBarHover = darkColor
      , scrollThumbBasic = darkColor
      , scrollThumbHover = darkColor
      }
