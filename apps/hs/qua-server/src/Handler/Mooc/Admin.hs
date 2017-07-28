{-# OPTIONS_HADDOCK hide, prune #-}
module Handler.Mooc.Admin
    ( getAdminR
    , requireAdmin
    , postSendReviewRequestR
    ) where

import Data.Time.Calendar (addDays)
import Import
import Import.BootstrapUtil
import qualified Network.Mail.Mime as Mail

data SendReviewParams = SendReviewParams {
      expertId   :: Key User
    , onlyBefore :: UTCTime
    }


getAdminR :: Handler Html
getAdminR = do
    experts <- selectExperts
    defaultDay <- lift (getCurrentTime >>= return . addDays (-7) . utctDay)
    (sendReviewRequestFormWidget, _) <-
      generateFormPost $ sendReviewRequestForm experts defaultDay
    fullLayout Nothing "Welcome to the admin page" $ do
        setTitle "qua-kit - admin page"
        toWidgetHead $
          [cassius|
            .form-inline
              .form-group
                display: inline-block
                margin-right: 15px
          |]
        $(widgetFile "mooc/admin")

postSendReviewRequestR :: Handler Html
postSendReviewRequestR = do
    experts <- selectExperts
    ((res, _), _) <- runFormPost $ sendReviewRequestForm experts $ ModifiedJulianDay 0
    case res of
      (FormSuccess params) -> sendReviewRequest params
      _ -> defaultLayout [whamlet|<p>Invalid input</p>|]

requireAdmin :: Handler ()
requireAdmin = do
    role <- muserRole <$> maybeAuth
    unless (role == UR_ADMIN) $
        sendResponseStatus
            status403
            ("You must be an admin to access this page" :: Text)

sendReviewRequestForm :: [Entity User] -> Day -> Html -> MForm Handler (FormResult SendReviewParams, Widget)
sendReviewRequestForm experts defaultDay extra = do
  let expertsList = map (\(Entity usrId ex) -> (userName ex, usrId)) experts
  (expertRes, expertView) <- mreq (bootstrapSelectFieldList expertsList) "" Nothing
  (onlyBeforeRes, onlyBeforeView) <- mreq bootstrapDayField "" $ Just defaultDay
  let toTime day = UTCTime day $ fromInteger 0
  let params = SendReviewParams <$> expertRes
                                <*> fmap toTime onlyBeforeRes
  let widget = do
        [whamlet|
          #{extra}
          ^{fvInput expertView}
          ^{fvInput onlyBeforeView}
          <input type=submit value="Send Mail" class="btn btn-default">
        |]
  return (params, widget)


sendReviewRequest :: SendReviewParams -> Handler Html
sendReviewRequest params = do
  mexpert <- runDB $ get $ expertId params
  let showErr = defaultLayout [whamlet|<p>Expert or his/her email not found</p>|]
  case mexpert of
    Nothing -> showErr
    Just expert -> case userEmail expert of
      Nothing -> showErr
      Just email -> do
        scenarios <- runDB $ selectList [
                         CurrentScenarioGrade ==. Nothing
                       , CurrentScenarioLastUpdate <. onlyBefore params
                     ] []
        if length scenarios > 0 then do
          render <- getUrlRender
          let toLink (Entity _ sc) = render $ SubmissionViewerR
                                                (currentScenarioTaskId sc)
                                                (currentScenarioAuthorId sc)
          let scLinks = intercalate "\n" $ fmap toLink scenarios
          let mailText = intercalate "\n\n" [
                             "Dear " <> userName expert
                           , "The following submissions are in need of reviewing:"
                           , scLinks
                           , "Thank you for your help!"
                           ]
          $(logDebug) mailText
          liftIO $ Mail.renderSendMail $ Mail.simpleMail'
              (Mail.Address Nothing email) --to address
              (Mail.Address (Just "ETH qua-kit") "noreply@qua-kit.ethz.ch") --from
              "Please help review the following submissions" --subject
                $ fromStrict mailText
          defaultLayout [whamlet|
                           <p>Success! Email sent.
                           <p><a onclick="window.history.back()" href="#">Back
                        |]
        else
          defaultLayout [whamlet|<p>No scenarios to review. Email not sent.</p>|]

selectExperts :: Handler [Entity User]
selectExperts = runDB $
  selectList [UserRole ==. UR_EXPERT, UserEmail /<-. [Nothing]] []
