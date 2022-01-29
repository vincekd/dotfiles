#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

; windows key + alt
; #!Down::Send {Media_Play_Pause}
; #!Left::Send {Media_Prev}
; #!Right::Send {Media_Next}
; #!Space::Send {Media_Play_Pause}

; Ctrl Alt 
^!Down::Send {Media_Play_Pause}
^!Space::Send {Media_Play_Pause}
^!Left::Send {Media_Prev}
^!Right::Send {Media_Next}

; right alt
; >!Down::Send {Media_Play_Pause}
; >!Left::Send {Media_Prev}
; >!Right::Send {Media_Next}

; alt
; !Space::Send {Media_Play_Pause}
; !+,::Send {Media_Prev}
; !+.::Send {Media_Next}