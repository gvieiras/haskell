{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Selo where

import Import
import Database.Persist.Postgresql
import Text.Julius 
import Text.Lucius

formSelo :: Form (Text,Text,Maybe FileInfo)
formSelo = renderDivs $ (,,)
    <$> areq textField "Nome do Selo: " Nothing
    <*> areq textField "Fundador do Selo: " Nothing
    <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Arquivo: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing
                                
getSeloR :: Handler Html
getSeloR = do
    sess <- lookupSession "_USR"  
    (widget,enctype) <- generateFormPost formSelo
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/inserirSelo.hamlet")                               
                                

