-- module Ports: defines JavaScript communication


port module Ports exposing (..)

import Types exposing (PremiumEntry, WriteEntry)



-- writes a new subscription to Firebase


port firebaseWrite : WriteEntry -> Cmd msg



-- initiates Stripe checkout for Premium


port processPremium : PremiumEntry -> Cmd msg



-- after successful payment, upgrades account on Firebase


port firebaseUpgrade : String -> Cmd msg



-- if a user is not Premium, cannot subscribe to more than one place


port invalidSubscription : (Bool -> msg) -> Sub msg
