{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Album where

import Import
import Database.Persist.Postgresql()
import Text.Julius() 
import Text.Lucius

formAlbum :: Form (Text,Day,Maybe FileInfo)
formAlbum = renderDivs $ (,,)
        <$> areq textField "Título: " Nothing
        <*> areq dayField  "Data de lançamento: " Nothing
        <*> aopt fileField FieldSettings{fsId=Just "hident4",
                                fsLabel="Capa do álbum: ",
                                fsTooltip= Nothing,
                                fsName= Nothing,
                                fsAttrs=[("accept","image/jpeg")]} 
                                Nothing
