module Main where

import Prelude
import Control.Monad.Eff.Ref (REF)

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as SM

import DOM (DOM)
import DOM.BrowserFeatures.Detectors (detectBrowserFeatures)

import Halogen (action, modify, query, request) as H
import Halogen.Component (Component, ParentDSL, ParentHTML, parentComponent) as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Aff (runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)

import Text.Markdown.SlamDown.Halogen.Component (SlamDownConfig, SlamDownFormState, SlamDownQuery(GetFormState, SetDocument), slamDownComponent)
import Text.Markdown.SlamDown.Parser (parseMd)

type State =
  { markdown ∷ String
  , formState ∷ SlamDownFormState String
  }

initialState ∷ State
initialState =
  { markdown : ""
  , formState : SM.empty
  }

data Query a = DocumentChanged String a | UpdateFormState a

data SlamDownSlot = SlamDownSlot

instance ordSlamDownSlot ∷ Ord SlamDownSlot where
  compare _ _ = EQ

instance eqSlamDownSlot ∷ Eq SlamDownSlot where
  eq _ _ = true

type DemoHTML = H.ParentDSL State Query (SlamDownQuery String) SlamDownSlot Void

ui ∷ ∀ g. (Functor g) ⇒ SlamDownConfig → H.Component HH.HTML Query Unit Void g
ui config = H.parentComponent {
    render,
    eval,
    initialState: \_ -> initialState,
    receiver
}
  where
    --render ∷ State → DemoHTML g
    render :: State -> H.ParentHTML Query (SlamDownQuery String) SlamDownSlot g
    render state = do
      HH.div
        [ HP.class_ $ HH.ClassName "container" ]
        [ HH.h2_ [ HH.text "Markdown" ]
        , HH.div_
            [ HH.textarea
                [ HP.class_ $ HH.ClassName "form-control"
                , HP.value state.markdown
                , HE.onValueInput $ HE.input DocumentChanged
                ]
            ]
        , HH.h2_ [ HH.text "HTML Output" ]
        , HH.div
            [ HP.class_ (HH.ClassName "well") ]
            [ HH.slot SlamDownSlot (slamDownComponent config) unit handleSlamDownOuput
            ]
        , HH.h2_ [ HH.text "Form State" ]
        , HH.pre_ [ HH.code_ [ HH.text (show state.formState) ] ]
        ]

    eval :: Query ~> DemoHTML g
    eval (DocumentChanged text next) = do
      for_ (parseMd text) \md →
        H.query SlamDownSlot $ H.action $ SetDocument md
      updateFormState
      pure next

    eval (UpdateFormState next) = do
        updateFormState
        pure next

    handleSlamDownOuput :: SlamDownQuery String Unit -> Maybe (Query Unit)
    handleSlamDownOuput _ = Just $ UpdateFormState unit

    updateFormState ∷ DemoHTML g Unit
    updateFormState =
      H.query SlamDownSlot (H.request GetFormState) >>=
        maybe (pure unit) \formState → H.modify (_ { formState = formState })

    receiver :: Unit -> Maybe (Query Unit)
    receiver _ = Just $ UpdateFormState unit

main ∷ Eff (avar ∷ AVAR, err ∷ EXCEPTION, dom ∷ DOM, ref :: REF) Unit
main = do
  browserFeatures ← detectBrowserFeatures
  let config = { formName : "slamdown-demo-form", browserFeatures : browserFeatures }
  runHalogenAff $
    runUI (ui config) unit =<< awaitBody
