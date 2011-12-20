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
Capslock::Ctrl
Ctrl::Capslock
