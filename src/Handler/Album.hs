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

getAlbumR :: ArtistaId -> Handler Html
getAlbumR artid = do
    sess <- lookupSession "_USR"
    art <- runDB $ get404 artid 
    (widget,enctype) <- generateFormPost formAlbum
    defaultLayout $ do 
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/inserirAlbum.hamlet")

postAlbumR :: ArtistaId -> Handler Html
postAlbumR  artid = do 
    -- LEIO OS PARAMETROS DO FORM
    ((res,_),_) <- runFormPost formAlbum
    case res of
        FormSuccess (titulo,lan,Just arq) -> do
            alid <- runDB $ insert $ Album titulo artid lan(Just $ (fileName arq))
            liftIO $ fileMove arq ("static/" ++ (unpack $ fileName arq))
            redirect (PerfilArtistaR artid)
        FormSuccess (titulo,lan,Nothing) -> do 
            alid <- runDB $ insert $ Album titulo artid lan Nothing
            redirect (PerfilArtistaR artid)
        _ -> redirect HomeR
        
getPaginaAlbumR :: ArtistaId -> AlbumId -> Handler Html
getPaginaAlbumR artid alid = do 
    alb <- runDB $ get404 alid
    imagem <- return $ albumCapa alb
    staticDir <- return $ "../../../../static/"
    defaultLayout $ do 
        [whamlet|
            <h1>
                #{albumTitulo alb}
            <h2>
                $maybe img <- imagem 
                    <img width="250px" height="250px" src=#{staticDir ++ img}>
        |]
