module ButtonBackup where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

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
render state =
  HH.div_
    [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
    , HH.text (show state)
    , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+"]
    ]

buttonComponent :: forall query output m. H.Component query Unit output m
buttonComponent =
  H.mkComponent
    {
      initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
