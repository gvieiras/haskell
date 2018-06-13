{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Evento where

import Import
import Database.Persist.Postgresql
import Text.Julius 
import Text.Lucius

formEvento :: Form (Text,Text,Int,Text,Day,Maybe FileInfo)
formEvento = renderDivs $ (,,,,,)
    <$> areq textField "Nome do Evento: " Nothing
    <*> areq textField "Artistas: " Nothing
    <*> areq intField "Preco: " Nothing
    <*> areq textField  "Local: " Nothing
    <*> areq dayField  "Data do Evento: " Nothing
    <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Arquivo: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing
                                
getEventoR :: Handler Html
getEventoR = do
    sess <- lookupSession "_USR"  
    (widget,enctype) <- generateFormPost formEvento
    defaultLayout $ do
        toWidget $(luciusFile "templates/inserirEvento.lucius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/inserirEvento.hamlet")
