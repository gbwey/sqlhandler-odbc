let Level = < Debug | Info | Warn | Error >
let ScreenType = < StdOut | StdErr >
let Screen = { ScreenType : ScreenType, Level : Level}
let Email = { SmtpServer : Text, SmtpPort : Optional Natural, SmtpTo : Text, SmtpFrom : Text }
let LogFile = {  Prefix : Text, LongName : Bool, Level : Level, Dir : Text }
in { debug = Level.Debug
   , info = Level.Info
   , warn = Level.Warn
   , error = Level.Error
   , stdout = ScreenType.StdOut
   , stderr = ScreenType.StdErr
   , screen = Screen
   , email = Email
   , file = LogFile
   , noscreen = None Screen
   , nofile = None LogFile
   , noemail = None Email
   , nodir = "."
   , Type = { LogFile : Optional LogFile, Screen : Optional Screen, Email : Optional Email, Debug : Bool }
   , default = { LogFile = Some { Prefix = "def", LongName = True, Level = Level.Debug, Dir = "." }
               , Screen = Some { ScreenType = ScreenType.StdOut, Level = Level.Info }
               , Email = None Email
               , Debug = False
               }
  }
