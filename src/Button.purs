module Button where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type State = Int

initialState :: Unit -> State
initialState _ = 0

data Action = Increment | Decrement

handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state - 1

  Increment ->
    H.modify_ \state -> state + 1

render :: forall m. State -> H.ComponentHTML Action () m
render _ = HH.div
  [HP.classes [HH.ClassName "flex", HH.ClassName "flex-row", HH.ClassName "mt-4", HH.ClassName "ml-4"]]
  [
    button Primary [HH.ClassName "mr-2"] (HH.text "Primary")
  , button Secondary [HH.ClassName "mr-2"] (HH.text "Secondary")
  , button Success [HH.ClassName "mr-2"] (HH.text "Success")
  , button Info [HH.ClassName "mr-2"] (HH.text "Info")
  , button Warning [HH.ClassName "mr-2"] (HH.text "Warning")
  , button Help [HH.ClassName "mr-2"] (HH.text "Help")
  , button Danger [] (HH.text "Danger")
  ]
  

buttonComponent :: forall query output m. H.Component query Unit output m
buttonComponent =
  H.mkComponent
    {
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

data ButtonType =
    Primary
  | Secondary
  | Success
  | Info
  | Warning
  | Help
  | Danger

button :: forall w i. ButtonType -> Array HH.ClassName -> HH.HTML w i -> HH.HTML w i
button buttonType classes content =
  HH.button
    [ HP.classes $
      [
        HH.ClassName "flx-button"
      , HH.ClassName buttonTypeClassName
      ] <> classes
    ]
    [ content ]
  
  where
    prefix = "flx-button-"
    buttonTypeClassName = case buttonType of
      Primary -> prefix <> "primary"
      Secondary -> prefix <> "secondary"
      Success -> prefix <> "success"
      Info -> prefix <> "info"
      Warning -> prefix <> "warning"
      Help -> prefix <> "help"
      Danger -> prefix <> "danger"