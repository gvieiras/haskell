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
        
postArtistaR :: Handler Html
postArtistaR = do 
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formArtista
    case res of
        FormSuccess (nome,gen,dat,Just arq) -> do 
            aid <- runDB $ insert $ Artista nome gen dat (Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (PerfilArtistaR aid)
        FormSuccess (nome,gen,dat,Nothing) -> do 
            aid <- runDB $ insert $ Artista nome gen dat Nothing
            redirect (PerfilArtistaR aid)
        _ -> redirect HomeR

getPerfilArtistaR :: ArtistaId -> Handler Html
getPerfilArtistaR aid = do
    albs <- runDB $ selectList[AlbumArtid ==. aid][Asc AlbumLancamento]
    sess <- lookupSession "_USR"
    art <- runDB $ get404 aid
    imagem <- return $ artistaImagem art
    staticDir <- return $ "../../static/"
    defaultLayout $ do
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/artistapage.hamlet")
