let x = ./corelog.dhall
in {
     LogFile = Some { Prefix = "def", LongName = True, Level = x.debug, Dir = x.nodir }
   , Screen = Some { ScreenType = x.stdout, Level = x.info }
   , Email = x.noemail
}
