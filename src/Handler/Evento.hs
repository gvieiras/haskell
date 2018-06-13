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


        
postEventoR :: Handler Html 
postEventoR = do 
    ((res,_),_) <- runFormPost formEvento
    case res of 
        FormSuccess (nome,artistas,preco,local,dte,Just arq) -> do 
            eid <- runDB $ insert $ Evento nome artistas preco local dte (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (EventoInfoR eid)
        FormSuccess (nome,artistas,preco,local,dte,Nothing) -> do 
            eid <- runDB $ insert $ Evento nome artistas preco local dte Nothing
            redirect (EventoInfoR eid)
        _ -> redirect HomeR


getEventoInfoR :: EventoId -> Handler Html
getEventoInfoR eid = do
    sess <- lookupSession "_USR"  
    evento <- runDB $ get404 eid
    imagem <- return $ eventoImagem evento
    staticDir <- return $ "../static/"
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius") 
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/paginaevento.hamlet")    

postEventoInfoR :: EventoId -> Handler Html
postEventoInfoR eid = do 
    runDB $ delete eid 
    redirect ListaEventosR        


 
        