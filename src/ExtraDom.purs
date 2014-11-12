module ExtraDom where

import Data.DOM.Simple.Types
import Control.Monad.Eff

foreign import createElement
  "function createElement(tagName) { \
  \ return function() { \
  \   return document.createElement(tagName); \
  \ } \
  \}" :: forall eff. String -> Eff (dom :: DOM | eff) HTMLElement

-- bit of a hack, this should really return a Node, but it'll do for now.
foreign import createTextNode
  "function createTextNode(contents) { \
  \ return function() { \
  \   return document.createTextNode(contents); \
  \ } \
  \}" :: forall eff. String -> Eff (dom :: DOM | eff) HTMLElement

foreign import appendChild
  "function appendChild(parent) { \
  \ return function(node) { \
  \   return function() { \
  \     parent.appendChild(node); \
  \   } \
  \ } \
  \}" :: forall eff. HTMLElement -> HTMLElement -> Eff (dom :: DOM | eff) Unit
