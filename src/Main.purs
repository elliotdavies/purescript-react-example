module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Console as Console

import Data.Array (snoc, modifyAt, elemIndex)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.HTML.HTMLElement (HTMLElement, focus)
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM

import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

import React as React
import React.Ref as Ref
import React.DOM as React.DOM
import React.DOM.Props as Props
import React.SyntheticEvent (currentTarget)
import ReactDOM as ReactDOM

import Example.TodoList (todoListClass)
import Example.Types (Todo(..), TodoStatus(..))

main :: Effect Unit
main = void $ do
  window <- DOM.window

  document <- DOM.document window

  let
      node = DOM.toNonElementParentNode document

  element <- DOM.getElementById "example" node

  let
      element' = unsafePartial (fromJust element)

  ReactDOM.render (React.createLeafElement mainClass { }) element'

mainClass :: React.ReactClass { }
mainClass = React.component "Main" component
  where
  component this =
    pure { state:
            { todo: Nothing
            , todos: []
            }
         , render: render <$> React.getState this
         }
    where
    render
      { todo
      , todos
      } =
      (React.createLeafElement todoListClass
        { todos
        , todo

        , onAdd: \todo' -> React.modifyState this \a ->
            a { todo = Nothing
              , todos = snoc a.todos todo'
              }

        , onEdit: \todo' -> React.modifyState this
            _ { todo = Just todo'
              }

        , onDone: \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoDone
              }

        , onClear : \todo' -> React.modifyState this \a ->
            a { todos = setStatus a.todos todo' TodoCleared
              }
        })
        <>
        (React.createLeafElement refsClass {})

    setStatus todos todo status = fromMaybe todos $ do
      i <- elemIndex todo todos

      modifyAt i (\(Todo a) -> Todo a { status = status }) todos


------------------------


type State
  = { domRef      :: Ref.Ref HTMLElement
    , instanceRef :: Ref.Ref Ref.ReactInstance
    , text        :: String
    }


initialState :: Ref.Ref HTMLElement -> Ref.Ref Ref.ReactInstance -> State
initialState domRef instanceRef
  = { domRef
    , instanceRef
    , text: ""
    }


refsClass :: React.ReactClass {}
refsClass = React.component "App" \this -> do
  domRef <- Ref.createDOMRef
  instanceRef <- Ref.createInstanceRef
  pure
    { state: initialState domRef instanceRef
    , render: render this <$> React.getState this
    , componentDidMount: componentDidMount this
    , componentDidUpdate
    }
  where
    render this { domRef, instanceRef, text }
      = React.DOM.div
          []

          -- DOM ref using createRef
          [ React.DOM.input
              [ Props._type "text"
              , Props.value text
              , Props.onChange $ changeHandler this
              , Props.ref domRef
              ]
          
          -- DOM ref using callback
          , React.DOM.div
              [ Props.callbackRef \nullableElement -> do
                  Console.log "DOM ref via callback:"
                  logAny nullableElement
              ]
              [ React.DOM.text "div"
              ]
          
          -- Instance ref using createRef
          , React.createLeafElement classWithRef
              { ref: Ref.withObjectRef instanceRef
              }

          -- Instance ref using callback
          , React.createLeafElement classWithRef
              { ref: Ref.withCallbackRef \nullableInstance -> do
                  Console.log "Instance ref via callback:"
                  logAny nullableInstance
              }
          ]

    -- Focus input field on mount
    componentDidMount this = do
      { domRef } <- React.getState this
      maybeEl <- Ref.getCurrentRef domRef
      case maybeEl of
        Just el -> focus el
        Nothing -> pure unit

    -- Debugging
    componentDidUpdate _ state _ = do
       let { domRef, instanceRef } = state
       Console.log "DOM ref:"
       Ref.getCurrentRef domRef >>= logAny
       Console.log "Instance ref:"
       Ref.getCurrentRef instanceRef >>= logAny

    changeHandler this event
      = do
          text' <- (unsafeCoerce >>> _.value) <$> (currentTarget event)
          React.modifyState this $ _ { text = text' }


classWithRef :: React.ReactClass {}
classWithRef = React.pureComponent "ClassWithRef" \_ -> do
  pure { render }
  where
    render
      = pure $
          React.DOM.p
            []
            [ React.DOM.text "classWithRef"
            ]


logAny :: forall a. a -> Effect Unit
logAny = unsafeCoerce >>> Console.log
