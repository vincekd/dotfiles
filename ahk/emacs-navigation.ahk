#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.


^a::Send {Home} ; ctrl + a = start of line
^e::Send {End} ; ctrl + e = end of line
!^a::sendInput, ^{Home}^+{End} ; map select all onto ctrl + alt + a
!+,::Send ^{Home} ; alt + shift + , = start of doc
!+.::Send ^{End} ; alt + shift + . = end of doc
!f::Send ^{Right} ; alt + f = forward word
!b::Send ^{Left} ; alt + b = backward word
; +f::Send {Right} ; ctrl + f = forward char
; +b::Send {Left} ; ctrl + b = backward char
