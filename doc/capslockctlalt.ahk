#SingleInstance force
Loop {
   If GetKeyState("ScrollLock", "T")
   {
      Suspend, Off
   } else {
      Suspend, On
   }
   Sleep, 50
}
Ctrl::Alt
Alt::Ctrl
Tab::Send {Blind}{Tab}
