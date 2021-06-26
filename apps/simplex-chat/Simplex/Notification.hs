{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Simplex.Notification (Notification (..), initializeNotifications) where

import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Simplex.Util (safeDecodeUtf8)
import System.Directory (doesFileExist, getAppUserDataDirectory)
import System.FilePath (combine)
import System.Info (os)
import System.Process (readCreateProcess, shell)

data Notification = Notification {title :: ByteString, text :: ByteString}

initializeNotifications :: IO (Notification -> IO ())
initializeNotifications = case os of
  "darwin" -> pure $ notify macScript
  "mingw32" -> initWinNotify
  "linux" ->
    doesFileExist "/proc/sys/kernel/osrelease" >>= \case
      False -> pure $ notify linuxScript
      True -> do
        v <- readFile "/proc/sys/kernel/osrelease"
        if "wsl" `isInfixOf` map toLower v
          then initWinNotify
          else pure $ notify linuxScript
  _ -> pure . const $ pure ()

notify :: (Notification -> Text) -> Notification -> IO ()
notify script notification =
  void $ readCreateProcess (shell . T.unpack $ script notification) ""

linuxScript :: Notification -> Text
linuxScript Notification {title, text} = "notify-send \"" <> safeDecodeUtf8 title <> "\" \"" <> safeDecodeUtf8 text <> "\""

macScript :: Notification -> Text
macScript Notification {title, text} = "osascript -e 'display notification \"" <> safeDecodeUtf8 text <> "\" with title \"" <> safeDecodeUtf8 title <> "\"'"

initWinNotify :: IO (Notification -> IO ())
initWinNotify = notify . winScript <$> savePowershellScript

winScript :: FilePath -> Notification -> Text
winScript path Notification {title, text} = "powershell.exe \"" <> T.pack path <> " \'" <> safeDecodeUtf8 title <> "\' \'" <> safeDecodeUtf8 text <> "\'\""

savePowershellScript :: IO FilePath
savePowershellScript = do
  appDir <- getAppUserDataDirectory "simplex"
  let psScript = combine appDir "win-toast-notify.ps1"
  writeFile
    psScript
    "[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime] > $null\n\
    \$Template = [Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent([Windows.UI.Notifications.ToastTemplateType]::ToastText02)\n\
    \$RawXml = [xml] $Template.GetXml()\n\
    \($RawXml.toast.visual.binding.text|where {$_.id -eq \"1\"}).AppendChild($RawXml.CreateTextNode($args[0])) > $null\n\
    \($RawXml.toast.visual.binding.text|where {$_.id -eq \"2\"}).AppendChild($RawXml.CreateTextNode($args[1])) > $null\n\
    \$SerializedXml = New-Object Windows.Data.Xml.Dom.XmlDocument\n\
    \$SerializedXml.LoadXml($RawXml.OuterXml)\n\
    \$Toast = [Windows.UI.Notifications.ToastNotification]::new($SerializedXml)\n\
    \$Toast.Tag = \"simplex-chat\"\n\
    \$Toast.Group = \"simplex-chat\"\n\
    \$Toast.ExpirationTime = [DateTimeOffset]::Now.AddMinutes(1)\n\
    \$Notifier = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier(\"PowerShell\")\n\
    \$Notifier.Show($Toast);\n"
  return psScript
