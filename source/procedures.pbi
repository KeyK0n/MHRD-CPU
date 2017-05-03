EnableExplicit

LoadFont(0,"Fixedsys",16)
LoadFont(1,"Fixedsys",12)

#NL = Chr(13)+Chr(10)

Enumeration RegEx
  #RE_SplitLines
  
  #RE_OnlyBin
  #RE_OnlyHex
  
  #RE_OnlyAddr
  #RE_OnlyImme
  #RE_OnlyConstAddr
  #RE_OnlyConstImme
  #RE_OnlyAtLabel
  
  #RE_ASMCode
  #RE_ASMLabel
  #RE_ASMConstant
  #RE_ASMComment
  #RE_ASMInline
EndEnumeration

CreateRegularExpression(#RE_SplitLines,".+",#PB_RegularExpression_AnyNewLine)

CreateRegularExpression(#RE_OnlyBin,"^[01]{16}$")
CreateRegularExpression(#RE_OnlyHex,"^[0-9a-f]{4}$")

CreateRegularExpression(#RE_OnlyAddr,"^\[(-{0,1}[0-9]{1,}|[0-1]{1,}b|[0-9a-f]{1,}h)\]$")
CreateRegularExpression(#RE_OnlyImme,"^(-{0,1}[0-9]{1,}|[0-1]{1,}b|[0-9a-f]{1,}h)$")
CreateRegularExpression(#RE_OnlyConstAddr,"^[ *#[a-z0-9_]+ *]$")
CreateRegularExpression(#RE_OnlyConstImme,"^#[a-z0-9_]+$")

CreateRegularExpression(#RE_OnlyAtLabel,"^@[a-z][a-z0-9_]{0,}$")

CreateRegularExpression(#RE_ASMCode    ,"^([a-z]{2,})(?: +([a-z0-9\[\]@#_-]{1,}) *)?(?:, *([a-z0-9\[\]@#_-]{1,}) *)?(?:, *([a-z0-9\[\]@#_-]{1,}) *)?$")
CreateRegularExpression(#RE_ASMLabel   ,"^([a-z][a-z0-9_]{1,}\:|@@:?)$")
CreateRegularExpression(#RE_ASMConstant,"^#([a-z0-9_]+?)( *= *(-{0,1}[0-9]{1,}|[0-1]{1,}b|[0-9a-f]{1,}h)|)$")
CreateRegularExpression(#RE_ASMComment ,"^\/\/.*$")
CreateRegularExpression(#RE_ASMInline  ,"^(?: *[01] *){16}$")

;---ProceduresGlobal
Procedure Error(msg.s)
  CompilerIf #PB_Compiler_OS = #PB_OS_Windows
    MessageRequester("Error","An error occured:"+Chr(10)+msg,#MB_ICONERROR)
  CompilerElse
    MessageRequester("Error","An error occured:"+Chr(10)+msg)
  CompilerEndIf
EndProcedure

Procedure.s ToHex(val.w)
  ProcedureReturn "0x"+RSet(Hex(val,#PB_Word),4,"0")
EndProcedure

Procedure.s ToBin(val.w, spaces = 1)
  Select spaces
    Case 0
      ProcedureReturn RSet(Bin(val,#PB_Word),16,"0")
    Case 1
      ProcedureReturn InsertString(InsertString(InsertString(RSet(Bin(val,#PB_Word),16,"0")," ",13)," ",9)," ",5)
    Case 2
      ProcedureReturn InsertString(InsertString(InsertString(InsertString(InsertString(InsertString(RSet(Bin(val,#PB_Word),16,"0")," ",12)," ",11)," ",7)," ",5)," ",4)," ",2)
  EndSelect
EndProcedure

Procedure.b GetBit(val.w,pos)
  Protected mask.w
  
  If pos > 15
    Error("Bitpos out of val")
  EndIf
  
  mask = Pow(2,pos)
  If val&mask
    ProcedureReturn 1
  Else
    ProcedureReturn 0
  EndIf
EndProcedure

Procedure.w GetBits(val.w,pos,len)
  Protected mask.w
  Protected i.i
  
  If pos > 15
    Error("Bitpos out of val")
  EndIf
  
  mask = 0
  For i = 0 To len-1
    mask + Pow(2,pos+i)
  Next
  
  val = val&mask
  val = val>>pos
  
  ProcedureReturn val
  
EndProcedure

Procedure.q CVal(str.s)
  Protected castW.w
  
  Select Right(str,1)
    Case "h"
      If Len(str) > 5
        ProcedureReturn $10000
      Else
        castW = Val("$"+Mid(str,0,Len(str)-1))
        ProcedureReturn castW
      EndIf
    Case "b"
      If Len(str) > 17
        ProcedureReturn $10000
      Else
        castW = Val("%"+Mid(str,0,Len(str)-1))
        ProcedureReturn castW
      EndIf
    Default
      ProcedureReturn Val(str)
  EndSelect
  
EndProcedure

Procedure.i ValS(val.i)
  If val >= -16 And val < 16
    ProcedureReturn 0
  ElseIf val >= 0 And val < 32768
    ProcedureReturn 1
  ElseIf val >= -32768 And val < 32768
    ProcedureReturn 2
  EndIf
  
  ProcedureReturn -1
  
EndProcedure

Macro mMC(const,dest,op1,op2,opCode,jz,c)
  
  (%const&%1)<<15 | (%dest&%11)<<13 | (%op1&%1)<<12 | (%op2&%11)<<10 | (%opCode&%1111)<<6 | (%jz&%1)<<5 | (%c&%11111)

EndMacro

Macro mmMC(const,dest,op1,op2,opCode,jz,c)
  
  (const&%1)<<15 | (dest&%11)<<13 | (op1&%1)<<12 | (op2&%11)<<10 | (opCode&%1111)<<6 | (jz&%1)<<5 | (c&%11111)

EndMacro

Macro mMCC(c)
  
  $8000 | (c&$7FFF)

EndMacro

CompilerIf #PB_Compiler_IsMainFile
  Debug "ProcedureDirect"
  
  Debug ToBin(mMC(0,01,0,10,1001,0,10101),2)
  Debug ToBin(mMCC(%101010101010101),2)
  
CompilerEndIf



; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 49
; FirstLine = 18
; Folding = --
; EnableXP