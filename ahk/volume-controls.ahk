#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


F10::
SendInput {Volume_Mute}
return

F11::
SendInput {Volume_Down}
return

F12::
SendInput {Volume_Up}
return