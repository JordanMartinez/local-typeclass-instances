module Main where

import Prelude

import Effect (Effect)
import Step1 as Step1
import Step2 as Step2
import Step3 as Step3
import Step4 as Step4
import Complex as Complex

main :: Effect Unit
main = do
  Step1.main
  Step2.main
  Step3.main
  Step4.main
  Complex.main
