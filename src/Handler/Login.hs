{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Login where

import Import
import Text.Julius 
import Text.Lucius
import           Yesod.Auth
import Database.Persist.Postgresql

formLogin :: Form (Text, Text)
formLogin = renderDivs $ (,) 
        <$> areq emailField "Email: " Nothing
        <*> areq passwordField "Senha: " Nothing

getLogarR :: Handler Html
getLogarR = do 
    (widget,enctype) <- generateFormPost formLogin
    msg <- getMessage
    sess <- lookupSession "_USR"
    defaultLayout $ do
        toWidget $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        $(whamletFile "templates/menuetc.hamlet")
        $(whamletFile "templates/login.hamlet")

autenticar :: Text -> Text -> HandlerT App IO (Maybe (Entity Usuario))
autenticar email senha = runDB $ selectFirst [UsuarioEmail ==. email
                                             ,UsuarioSenha ==. senha] []

postLogarR :: Handler Html 
postLogarR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("root@root.com","root") -> do 
            setSession "_USR" (pack (show $ Usuario "admin" "" ""))
            redirect AdminR 
        FormSuccess (email,senha) -> do 
            talvezUsuario <- autenticar email senha 
            case talvezUsuario of 
                Nothing -> do 
                    setMessage [shamlet| 
                        <h1> 
                            Usuário não encontrado. 
                    |]
                    redirect LogarR
                Just (Entity uid (Usuario n e _)) -> do 
                    setSession "_USR" (pack (show $ Usuario n e ""))
                    redirect (HomeR)
        _ -> redirect HomeR
