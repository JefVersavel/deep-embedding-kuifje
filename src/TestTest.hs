{-# LANGUAGE TemplateHaskell #-}

module TestTest where

import TypeCheckedDSLTH

tte3 = eval $(tevall te3)
