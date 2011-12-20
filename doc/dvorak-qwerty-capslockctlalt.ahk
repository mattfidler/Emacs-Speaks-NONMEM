#SingleInstance force
DetectHiddenWindows On
Loop {
   If GetKeyState("ScrollLock", "T")
   {
     Suspend, Off
     If GetKeyState("Control") 
       Or GetKeyState("Alt") 
       Or GetKeyState("Lwin") 
       Or GetKeyState("Rwin") 
     {
       Gui 1:Show, Hide, ModKy
     } else 
     {
       Gui 1:Destroy
     }
   } else {
      Suspend, On
   }
   Sleep, 50
}


#IfWinExist ModKy ; below the HotKeys are active when the mod keys are activated.
[::[
]::]

'::'
,::,
.::.
p::p
y::y
f::f
g::g
c::c
r::r
l::l
/::/
=::=

a::a
o::o
e::e
u::u
i::i
d::d 
h::h
t::t
n::n
s::s
-::-

`;::`;
q::q
j::j
k::k
x::x
b::b
m::m
w::w
v::v
z::z
Capslock::Ctrl
Ctrl::Alt
Alt::Ctrl
Tab::Send {Blind}{Tab}
;...

#IfWinExist       ; below the HotKeys are active when all toggles are Off
Capslock::Ctrl
Ctrl::Alt
Alt::Ctrl
Tab::Send {Blind}{Tab}
-::[
=::]

q::'
w::,
e::.
r::p
t::y
y::f
u::g
i::c
o::r
p::l
[::/
]::=

;a::a
s::o
d::e
f::u
g::i
h::d
j::h
k::t
l::n
`;::s
'::-

z::`;
x::q
c::j
v::k
b::x
n::b
;m::m
,::w
.::v
/::z

;...

