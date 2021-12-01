{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import


data Post =
  Post { a :: Integer, b :: Integer }
  deriving Show

postForm :: Form Post
postForm =
  renderDivs $
    Post <$> areq intField  "Digit a: " Nothing <*> areq intField  "Digit b: " Nothing

getPostR :: Handler Html
getPostR = do
  (widget, enctype) <- generateFormPost postForm
  defaultLayout $ do
    setTitle "Lab5"
    $(widgetFile "post")

headCustom :: [a] -> a
headCustom (x:xs) = x

uniquePure :: Eq a => [a] -> [a]
uniquePure [] = []
uniquePure [x] = [x]
uniquePure (x:xs) = if x == headCustom xs
                    then uniquePure xs
                    else x : uniquePure xs

postPostR :: Handler Html
postPostR = do
  ((result, widget), enctype) <- runFormPost postForm

  case result of
      FormSuccess post ->
        defaultLayout [whamlet|<p>#{show post}|]
