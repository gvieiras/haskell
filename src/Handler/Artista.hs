{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Artista where

import Import
import Database.Persist.Postgresql
import Text.Julius 
import Text.Lucius


formArtista :: Form (Text,Text,Day,Maybe FileInfo)
formArtista = renderDivs $ (,,,)
        <$> areq textField "Nome: " Nothing
        <*> areq textField "Gênero: " Nothing
        <*> areq dayField  "Data de formação: " Nothing
        <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Foto do artista: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing
                                
getArtistaR :: Handler Html
getArtistaR = do
    sess <- lookupSession "_USR" 
    (widget,enctype) <- generateFormPost formArtista
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/inserirArtista.hamlet")
