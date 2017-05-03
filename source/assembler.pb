EnableExplicit

IncludeFile "procedures.pbi"

;---Enumeration

Enumeration Window
  #W_ASM
  #W_ASM_CompileS
  
EndEnumeration

Enumeration Gadget
  
  #G_A_ASM
  #G_A_MC
  
  #G_A_CS_ASMLine
  #G_A_CS_ASM
  #G_A_CS_Comment
  #G_A_CS_Error
  #G_A_CS_Label
  #G_A_CS_NL
  
EndEnumeration

Enumeration Menu
  #M
  
  #M_A_New
  #M_A_Load
  #M_A_Save
  #M_A_SaveAS
  #M_A_Close
  
  #M_A_Compile
  #M_A_AutoCompile
  #M_A_CompileS
  
  #M_A_Help
  #M_A_Info
  
EndEnumeration

Enumeration instrType
  #IT_Undefined
  
  #IT_Inline
  
  #IT_ASM_MOV
  
  #IT_ASM_ADD
  #IT_ASM_SUB
  #IT_ASM_AND
  #IT_ASM_NAND
  #IT_ASM_OR
  #IT_ASM_NOT
  
  #IT_ASM_INC
  #IT_ASM_DEC
  
  #IT_ASM_JMP
  #IT_ASM_JZ
  
  #IT_ASM_NOP
  
  #IT_ASM_END
  
EndEnumeration

Enumeration OpId
  #Op_Empty
  #Op_Reg_NIL
  #Op_Reg_AR
  #Op_Reg_MR
  #Op_Reg_RAM
  #Op_Addr
  #Op_Imme
  
  #Op_Label
  
EndEnumeration

#REG_NONE      = %00000000
#REG_Write_AR  = %00000001
#REG_Write_MR  = %00000010
#REG_Write_RAM = %00000100
#REG_Write_PC  = %00001000
#REG_Read_AR   = %00010000
#REG_Read_MR   = %00100000
#REG_Read_RAM  = %01000000

#ASM_NONE      = %00000000
#ASM_Error     = %00000001
#ASM_Comment   = %00000010
#ASM_Label     = %00000100
#ASM_ASM       = %00001000
#ASM_ASMLine   = %00010000
#ASM_NL        = %00100000

;---Variables


Structure op
  text.s
  type.i
  bin.s
  val.i
  vals.b
  
EndStructure

Structure programLine
  line.i
  raw.s
  
  error.s
  comments.s
  List labels.s()
  anonymousLabel.b
  
  type.i
  
  instr.s
  instrType.i
  
  op.op[3]
  jz.b
  
  List mc.w()
  offset.w
  
  compileLabel.b
  registers.b
  
EndStructure

Structure value
  val.i
  valS.b
  
EndStructure

Define.s savePath = "Unsaved", path
Define.i saveChange = -1

Define.i event,i

Define.i compileFlags = #ASM_NL|#ASM_Error|#ASM_Comment

Define.w line

NewMap labels.i()
NewMap constants.value()
NewList program.programLine()


;---Procedures

Procedure CompileText(List p.programLine(), Map l.i(), Map c.value(), text.s)
  Dim lines.s(0)
  
  Protected.i i, n, lines, enum
  Protected.s instr
  
  ClearMap(l())
  ClearMap(c())
  ClearList(p())
  AddElement(p())
  
  ;lines = ExtractRegularExpression(#RE_SplitLines,LCase(text),lines())-1
  
  text = LCase(RemoveString(text,Chr(13)))
  lines = CountString(text,Chr(10))
  ReDim lines(lines)
  For i = 0 To lines
    lines(i) = Trim(StringField(text,i+1,Chr(10)))
  Next
  
  For i = 0 To lines
    If lines(i) = ""

      Continue
      
    ElseIf ExamineRegularExpression(#RE_ASMCode ,lines(i)) And NextRegularExpressionMatch(#RE_ASMCode)
      
      
      Select RegularExpressionGroup(#RE_ASMCode,1)
        Case "end"
          Break
        Case "enum"
          If RegularExpressionGroup(#RE_ASMCode,3) = "" And RegularExpressionGroup(#RE_ASMCode,4) = "" And MatchRegularExpression(#RE_OnlyImme,RegularExpressionGroup(#RE_ASMCode,2))
            n = CVal(RegularExpressionGroup(#RE_ASMCode,2))
            If ValS(n) = -1
              p()\error = "//ERROR enum out of range" +#NL
            Else
              enum = n
            EndIf
          Else
            p()\error = "//ERROR enum illegal operands" +#NL
          EndIf
        Default
          p()\raw = lines(i)
          p()\line = i+1
          
          p()\instr = RegularExpressionGroup(#RE_ASMCode,1)
          ;Debug p()\instr
          For n = 0 To 2
            p()\op[n]\text = RegularExpressionGroup(#RE_ASMCode,2+n)
            ;Debug p()\op[n]\text
          Next
          ;Debug "---"
          
          AddElement(p())
      EndSelect
      
    ElseIf ExamineRegularExpression(#RE_ASMConstant,lines(i)) And NextRegularExpressionMatch(#RE_ASMConstant)
      
      instr = RegularExpressionGroup(#RE_ASMConstant,1)
      
      If FindMapElement(c(),instr)
        p()\error + "//ERROR constant duplicate" +#NL
      Else
        AddMapElement(c(),instr,#PB_Map_NoElementCheck)
        If RegularExpressionGroup(#RE_ASMConstant,2) = ""
          c(instr)\val = enum
          enum + 1
        Else
          c(instr)\val = CVal(RegularExpressionGroup(#RE_ASMConstant,3))
        EndIf
        c(instr)\valS = ValS(c(instr)\val)
        If c(instr)\valS = -1
          p()\error + "//ERROR constant out of range" +#NL
          c(instr)\val = 0
        EndIf
      EndIf
      
    ElseIf MatchRegularExpression(#RE_ASMComment,lines(i))

      p()\comments+lines(i) +#NL
      
    ElseIf MatchRegularExpression(#RE_ASMLabel  ,lines(i))
      
      If Left(lines(i),1) = "@"
        p()\anonymousLabel = 1
      Else
        
        instr = Left(lines(i),Len(lines(i))-1)
        If FindMapElement(l(),instr)
          p()\error + "//ERROR label duplicate" +#NL
        Else
          AddMapElement(l(),instr,#PB_Map_NoElementCheck)
          l(instr) = @p()
          
          AddElement(p()\labels())
          p()\labels() = instr
        EndIf
      EndIf
      
    ElseIf MatchRegularExpression(#RE_ASMInline ,lines(i))
      
      p()\raw = "Inline"
      p()\line = i+1
      p()\instr = lines(i)
      p()\instrType = #IT_Inline
      AddElement(p()\mc())
      p()\mc() = Val("%"+RemoveString(lines(i)," "))
      
      AddElement(p())
      
    Else
      
      p()\raw = lines(i)
      p()\line = i+1
      p()\error = "//ERROR unparsable"
      
      AddElement(p())
      
    EndIf
    
    
  Next
  
  p()\raw = "end"
  p()\line = i+1
  p()\instr = "end"
  p()\instrType = #IT_ASM_END
  
  
EndProcedure


Procedure ParseMnemnonic(*p.programLine,Map l.i(), Map c.value())
  Protected.i i
  Protected.s instr
  
  Protected op.op
  
  If *p\instrType = #IT_Inline
    ProcedureReturn 0
  EndIf
  
  For i = 0 To 2
    Select *p\op[i]\text
      Case ""
        *p\op[i]\type = #Op_Empty
        *p\op[i]\bin  = "00"
      Case "nil"
        *p\op[i]\type = #Op_Reg_Nil
        *p\op[i]\bin  = "00"
      Case "mr"
        *p\op[i]\type = #Op_Reg_MR
        *p\op[i]\bin  = "10"
      Case "ar"
        *p\op[i]\type = #Op_Reg_AR
        *p\op[i]\bin  = "01"
      Case "[mr]"
        *p\op[i]\type = #Op_Reg_RAM
        *p\op[i]\bin  = "11"
      Default
        If MatchRegularExpression(#RE_OnlyAddr,*p\op[i]\text)
          *p\op[i]\type = #Op_Addr
          *p\op[i]\bin  = "11"
          *p\op[i]\val  = CVal(Mid(*p\op[i]\text,2,Len(*p\op[i]\text)-2))
          If *p\op[i]\vals = -1
            *p\error + "//ERROR address out of range" +#NL
          EndIf
        ElseIf MatchRegularExpression(#RE_OnlyConstAddr,*p\op[i]\text)
          *p\op[i]\type = #Op_Addr
          *p\op[i]\bin  = "11"
          If FindMapElement(c(),Mid(*p\op[i]\text,3,Len(*p\op[i]\text)-3))
            *p\op[i]\val = c()\val
            If *p\op[i]\vals = -1
              *p\error + "//ERROR address out of range" +#NL
            EndIf
          Else
            *p\error + "//ERROR unkown constant"
          EndIf
        ElseIf MatchRegularExpression(#RE_OnlyImme,*p\op[i]\text)
          *p\op[i]\type = #Op_Imme
          *p\op[i]\bin  = "00"
          *p\op[i]\val = CVal(*p\op[i]\text)
          *p\op[i]\vals = ValS(*p\op[i]\val)
          If *p\op[i]\vals = -1
            *p\error + "//ERROR immediate out of range" +#NL
          EndIf
        ElseIf MatchRegularExpression(#RE_OnlyConstImme,*p\op[i]\text)
          *p\op[i]\type = #Op_Imme
          *p\op[i]\bin  = "00"
          If FindMapElement(c(),Mid(*p\op[i]\text,2))
            *p\op[i]\val = c()\val
            *p\op[i]\valS = c()\vals
          Else
            *p\error + "//ERROR unkown constant"
          EndIf
        ElseIf MatchRegularExpression(#RE_OnlyAtLabel,*p\op[i]\text)
          *p\op[i]\type = #Op_Label
          *p\op[i]\bin = "xx"
          If *p\op[i]\text = "@b" Or *p\op[i]\text = "@r"
            *p\op[i]\vals = -1
          ElseIf *p\op[i]\text = "@f"
            *p\op[i]\vals = 1
          ElseIf FindMapElement(l(),Mid(*p\op[i]\text,2))
            *p\op[i]\val =  l()
          Else
            *p\error + "//ERROR unknown label" +#NL
          EndIf
        Else
          *p\error + "//ERROR illegal operand" +#NL
        EndIf
    EndSelect
  Next
  
  instr = *p\instr
  If Len(instr) >= 3 And Right(instr,2) = "jz"
    *p\jz = 1
    instr = Left(instr,Len(instr)-2)
  EndIf
  
  Select instr
    Case "mov"
      *p\instrType = #IT_ASM_MOV
      If *p\jz
        *p\error + "//ERROR mov + jz not supported" +#NL
      EndIf
    Case "add"
      *p\instrType = #IT_ASM_ADD
    Case "sub"
      *p\instrType = #IT_ASM_SUB
    Case "and"
      *p\instrType = #IT_ASM_AND
    Case "nand"
      *p\instrType = #IT_ASM_NAND
    Case "or"
      *p\instrType = #IT_ASM_OR
    Case "not"
      *p\instrType = #IT_ASM_NOT
    Case "inc"
      *p\instrType = #IT_ASM_INC
    Case "dec"
      *p\instrType = #IT_ASM_DEC
    Case "jmp"
      *p\instrType = #IT_ASM_JMP
      If *p\jz
        *p\error + "//ERROR jmp + jz not supported" +#NL
      EndIf
    Case "jz"
      *p\instrType = #IT_ASM_JZ
      If *p\jz
        *p\error + "//ERROR jz + jz not supported" +#NL
      EndIf
    Case "nop"
      *p\instrType = #IT_ASM_NOP
      If *p\jz
        *p\error + "//ERROR nop + jz not supported" +#NL
      EndIf
    Case "end"
      *p\instrType = #IT_ASM_END
    Default
      *p\error + "//ERROR unknown instruction" +#NL
  EndSelect
  
EndProcedure

Procedure CompileMnemnonic(*p.programLine)
  
  Protected.i i, val
  Protected.s mc
  
  Protected op.op
  
  Select *p\instrType
    Case #IT_Inline
      
      ProcedureReturn 1
      
    Case #IT_ASM_MOV
      
      
      If *p\op[2]\type <> #Op_Empty
        *p\error + "//ERROR third operand not supported" +#NL
      EndIf
      Select *p\op[0]\type
          
          
        Case #Op_Addr
          Select *p\op[1]\type
            Case #Op_Imme
              
              Select *p\op[1]\vals
                Case 0
                  AddElement(*p\mc()) : *p\mc() = mMCC(*p\op[0]\val)
                  AddElement(*p\mc()) : *p\mc() = mmMC(%0,%11,%0,%00,%0011,%0,*p\op[1]\val)
                Case 1
                  AddElement(*p\mc()) : *p\mc() = mMCC(*p\op[1]\val)
                  AddElement(*p\mc()) : *p\mc() = mMC(0,01,0,10,0000,0,00000)
                  AddElement(*p\mc()) : *p\mc() = mMCC(*p\op[0]\val)
                  AddElement(*p\mc()) : *p\mc() = mMC(0,11,0,01,0000,0,00000)
                Case 2
                  AddElement(*p\mc()) : *p\mc() = mMCC(~*p\op[1]\val)
                  AddElement(*p\mc()) : *p\mc() = mMC(0,01,0,10,0100,0,00000)
                  AddElement(*p\mc()) : *p\mc() = mMCC(*p\op[0]\val)
                  AddElement(*p\mc()) : *p\mc() = mMC(0,11,0,01,0000,0,00000)
                Default
                  *p\error + "//ERROR unknown immediate error" +#NL
              EndSelect
            Case #Op_Reg_MR
              mc = "0 01 0 10 0000 0 00000"                                    + #NL
              mc + "1 "+Right(ToBin(*p\op[0]\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Case #Op_Reg_AR
              mc = "1 "+Right(ToBin(*p\op[0]\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(*p\op[1]\val),18)                             + #NL
              mc + "0 01 0 11 0000 0 00000"                                    + #NL
              mc + "1 "+Right(ToBin(*p\op[0]\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Default
              *p\error + "//ERROR unsupported source" +#NL
          EndSelect
          
          
        Case #Op_Reg_RAM
          Select *p\op[1]\type
            Case #Op_Imme
              Select *p\op[1]\vals
                Case 0
                  mc = "0 11 0 00 0011 0 "+RSet(Right(Bin(*p\op[1]\val),5),5,"0")
                Case 1,2
                  *p\error + "//ERROR cant write big values direct to [MR]" +#NL
                Default
                  *p\error + "//ERROR unknown immediate error" +#NL
              EndSelect
            Case #Op_Reg_MR
              mc = "0 11 0 10 0000 0 00000";SINN?
            Case #Op_Reg_AR
              mc = "0 11 0 01 0000 0 00000"
            ;Case #Op_Addr
            ;  *p\error + "//ERROR cant write from RAM to [MR]" +#NL
            Default
              *p\error + "//ERROR unsupported source" +#NL
          EndSelect
          
          
        Case #Op_Reg_AR
          Select *p\op[1]\type
            Case #Op_Imme
              Select *p\op[1]\vals
                Case 0
                  mc = "0 01 0 00 0011 0 "+RSet(Right(Bin(*p\op[1]\val),5),5,"0")
                Case 1
                  mc = "1 "+Right(ToBin(*p\op[1]\val),18) + #NL
                  mc + "0 01 0 10 0000 0 00000"
                Case 2
                  mc = "1 "+Right(ToBin(~*p\op[1]\val),18) + #NL
                  mc + "0 01 0 10 0100 0 00000"
                Default
                  *p\error + "//ERROR unknown immediate error" +#NL
              EndSelect
            Case #Op_Reg_MR
              mc = "0 01 0 10 0000 0 00000"
            Case #Op_Reg_RAM
              mc = "0 01 0 11 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(*p\op[1]\val),18) + #NL
              mc + "0 01 0 11 0000 0 00000"
            Default
              *p\error + "//ERROR unsupported source" +#NL
          EndSelect
          
          
        Case #Op_Reg_MR
          Select *p\op[1]\type
            Case #Op_Imme
              Select *p\op[1]\vals
                Case 0,1
                  mc = "1 "+Right(ToBin(*p\op[1]\val),18)
                Case 2
                  mc = "1 "+Right(ToBin(~*p\op[1]\val),18) + #NL
                  mc + "0 10 0 10 0100 0 00000"
                Default
                  *p\error + "//ERROR unknown immediate error" +#NL
              EndSelect
            Case #Op_Reg_AR
              mc = "0 10 0 01 0000 0 00000"
            Case #Op_Reg_RAM
              mc = "0 10 0 11 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(*p\op[1]\val),18) + #NL
              mc + "0 10 0 11 0000 0 00000"
            Case #Op_Label
              mc = "1 000 0000 0000 0000"
              *p\compileLabel = 2
            Default
              *p\error + "//ERROR unsupported source" +#NL
          EndSelect
          
          
        Default
          *p\error + "//ERROR unsupported destination" +#NL
      EndSelect
      
      
    Case #IT_ASM_ADD, #IT_ASM_SUB, #IT_ASM_AND, #IT_ASM_OR, #IT_ASM_NAND
      
      
      If *p\op[1]\type = #Op_Imme Or *p\op[1]\type = #Op_Reg_AR
        If *p\op[2]\type = #Op_Addr And *p\op[2]\type = #Op_Empty
          *p\error + "//ERROR unsupported operands" +#NL
        EndIf
      Else
        If *p\op[2]\type = #Op_Imme Or *p\op[2]\type = #Op_Reg_AR
          CopyStructure(@*p\op[2],@op,op)
          CopyStructure(@*p\op[1],@*p\op[2],op)
          CopyStructure(@op,@*p\op[1],op)
          i = 1
        Else
          *p\error + "//ERROR unsupported operands" +#NL
        EndIf
      EndIf
      
      If *p\op[1]\type = #Op_Imme And *p\op[2]\type = #Op_Imme And *p\op[1]\val <> *p\op[2]\val
        *p\error + "//ERROR can't use two immediates" +#NL
      ElseIf *p\op[1]\vals > 0 Or *p\op[2]\vals > 0
        *p\error + "//ERROR can't use big immediates" +#NL
      EndIf
      
      If *p\op[1]\type = #Op_Imme
        val = *p\op[1]\val
      ElseIf *p\op[2]\type = #Op_Imme
        val = *p\op[2]\val
      EndIf
      
      If *p\op[0]\type <> #Op_Reg_NIL And *p\op[0]\type <> #Op_Reg_AR And *p\op[0]\type <> #Op_Reg_MR And *p\op[0]\type <> #Op_Reg_RAM
        *p\error + "//ERROR unsupported destination" +#NL
      EndIf
      If *p\op[1]\type <> #Op_Imme And *p\op[1]\type <> #Op_Reg_AR And *p\op[2]\type <> #Op_Imme And *p\op[2]\type <> #Op_Reg_AR And *p\op[2]\type <> #Op_Reg_MR And *p\op[2]\type <> #Op_Reg_RAM
        *p\error + "//ERROR unsupported operands" +#NL
      EndIf
      
      mc = "0 "+*p\op[0]\bin+" "+Right(*p\op[1]\bin,1)+" "+*p\op[2]\bin
      
      Select Left(*p\instr,2)
        Case "ad"
          mc + " 0000"
        Case "su"
          If i = 1
            mc + " 1001"
          Else
            mc + " 0101"
          EndIf
        Case "an"
          mc + " 0011"
        Case "na"
          mc + " 0010"
        Case "or"
          mc + " 1110"
        Default
          *p\error + "//ERROR unknown opcode error" +#NL
      EndSelect
      
      If *p\jz
        mc + " 1 "
      Else
        mc + " 0 "
      EndIf
      
      mc + RSet(Right(Bin(val),5),5,"0")
      
      
    Case #IT_ASM_NOT
      
      If Not (*p\op[0]\type = #Op_Reg_AR Or *p\op[0]\type = #Op_Reg_MR Or *p\op[0]\type = #Op_Reg_RAM) Or *p\op[1]\type <> #Op_Empty Or *p\op[2]\type <> #Op_Empty
        *p\error + "//ERROR unsupported operand"
      EndIf
      
      mc = "0 00 0 "+*p\op[0]\bin+" 0001"
      
      If *p\jz
        mc + " 1 "
      Else
        mc + " 0 "
      EndIf
      
      mc + "00000"
      
    Case #IT_ASM_INC, #IT_ASM_DEC
      
      
      If *p\op[1]\type <> #Op_Empty Or *p\op[2]\type <> #Op_Empty
        *p\error + "//ERROR second, third operand not supported" +#NL
      EndIf
      If *p\op[0]\type = #Op_Imme Or *p\op[0]\type = #Op_Empty Or *p\op[0]\type = #Op_Addr
        *p\error + "//ERROR unsupported operand" +#NL
      EndIf
      
      mc = "0 "+*p\op[0]\bin+" 0 "+*p\op[0]\bin+" 0000"
      
      If *p\jz
        mc + " 1 "
      Else
        mc + " 0 "
      EndIf
      
      If *p\instrType = #IT_ASM_INC
        mc + "00001"
      Else
        mc + "11111"
      EndIf
      
      
    Case #IT_ASM_JMP
      
      
      If *p\op[0]\type <> #Op_Label Or *p\op[1]\type <> #Op_Empty Or *p\op[2]\type <> #Op_Empty
        *p\error + "//ERROR unsupported operand" +#NL
      EndIf
      
      
      mc = "1 000 0000 0000 0000" + #NL
      mc + "0 00 0 00 0000 1 00000"
      
      *p\compileLabel = 1
      
    Case #IT_ASM_JZ
      
      If *p\op[0]\type <> #Op_Label Or Not (*p\op[1]\type = #Op_Reg_AR Or *p\op[1]\type = #Op_Reg_MR Or *p\op[1]\type = #Op_Reg_RAM) Or *p\op[2]\type <> #Op_Empty
        *p\error + "//ERROR unsupported operand" +#NL
      EndIf
      
      mc = "1 000 0000 0000 0000" + #NL
      mc + "0 00 0 "+*p\op[1]\bin+" 0000 1 00000"
      
      *p\compileLabel = 1
      
    Case #IT_ASM_NOP
      AddElement(*p\mc()) : *p\mc() = 0;mMC(0,00,0,00,0000,0,00000)
      
    Case #IT_ASM_END
      
    Default
      *p\error + "//ERROR unknown instruction" +#NL
  EndSelect
  
  If mc <> ""
    For i = 1 To CountString(mc,#NL)+1
      AddElement(*p\mc())
      *p\mc() = Val("%"+RemoveString(StringField(mc,i,#NL)," "))
    Next
  EndIf
  
  ProcedureReturn ListSize(*p\mc())
  
EndProcedure

Procedure CompileLabels(*p.programLine,List p.programLine(),Map l.i())
  Protected.i ppos = @p()
  
  With *p
    
    If \compileLabel > 0
      If \op[\compileLabel-1]\vals = -1
        ChangeCurrentElement(p(),*p)
        Repeat
          If p()\anonymousLabel
            Break
          EndIf
        Until PreviousElement(p()) = 0
        If p()\anonymousLabel = 0
          \error + "//ERROR no backward anonymouslabel" +#NL
        EndIf
      ElseIf \op[\compileLabel-1]\vals = 1
        ChangeCurrentElement(p(),*p)
        While NextElement(p())
          If p()\anonymousLabel
            Break
          EndIf
        Wend
        If p()\anonymousLabel = 0
          \error + "//ERROR no forward anonymouslabel" +#NL
        EndIf
      Else
        If \op[\compileLabel-1]\val <> 0
          ChangeCurrentElement(p(),\op[\compileLabel-1]\val)
        Else
          \error + "//ERROR label not found" +#NL
        EndIf
      EndIf
      FirstElement(\mc())
      \mc() + p()\offset&$7FFF
    EndIf
    
  EndWith
  
  ChangeCurrentElement(p(),ppos)
EndProcedure


Procedure CompileASM()
  Shared program()
  Shared labels()
  Shared constants()
  
  Protected.i errors, offset
  
  Debug "--- CompileText ---"
  
  CompileText(program(),labels(),constants(),GetGadgetText(#G_A_ASM))
  
  Debug "--- ParseMnemnonic ---"
  
  ForEach program()
    If program()\error = ""
      ParseMnemnonic(@program(),labels(),constants())
    EndIf
  Next
  
  Debug "--- CompileMnemnonic ---"
  
  ForEach program()
    If program()\error = ""
      program()\offset = offset
      offset + CompileMnemnonic(@program())
    EndIf
  Next
  
  Debug "--- CompileLabels ---"
  
  ForEach program()
    If program()\error = ""
      CompileLabels(@program(),program(),labels())
      If program()\error <> ""
        errors + 1
      EndIf
    Else
      errors + CountString(program()\error,#NL)
    EndIf
  Next
  
  Debug "Errors: "+Str(errors)
  
  ProcedureReturn errors
  
EndProcedure

Procedure Compile(flags=0)
  Shared program()
  
  Protected error.i
  Protected textOut.s = ""
  
  SetGadgetData(#G_A_ASM,0)
  
  error = CompileASM()
  
  ForEach program()
    If flags&#ASM_Comment And program()\comments <> ""
      textOut + program()\comments
    EndIf
    
    If flags&#ASM_Label
      If ListSize(program()\labels())
        textOut + "//LABEL: "
        If program()\anonymousLabel
          textOut + "anonymous, "
        EndIf
        ForEach program()\labels()
          textOut + program()\labels()+", "
        Next
        textOut +#NL
      Else
        If program()\anonymousLabel
          textOut + "//LABEL: anonymous" +#NL
        EndIf
      EndIf
    EndIf
    
    If flags&#ASM_ASM And flags&#ASM_ASMLine
      textOut + "//"+RSet(Str(program()\line),3," ")+" "+program()\raw +#NL
    ElseIf flags&#ASM_ASMLine
      textOut + "//"+RSet(Str(program()\line),3," ") +#NL
    ElseIf flags&#ASM_ASM
      textOut + "//"+program()\raw +#NL
    EndIf
    
    If program()\error
      If flags&#ASM_Error
        textOut + program()\error
      EndIf
    Else
    
      ForEach program()\mc()
        textOut + ToBin(program()\mc(),2) +#NL
      Next
      
    EndIf
    
    If flags&#ASM_NL
      textOut +#NL
    EndIf
  Next
  
  SetGadgetText(#G_A_MC,textOut)
  
  ProcedureReturn error
  
EndProcedure

Procedure Load(path.s)
   Protected.i fID
  
  fID = ReadFile(#PB_Any,path)
  If fID
    ReadStringFormat(fID)
    SetGadgetText(#G_A_ASM,ReadString(fID,#PB_File_IgnoreEOL))
    
    CloseFile(fID)
  Else
    Error("Unable to read from "+Chr(34)+path+Chr(34))
  EndIf
  
EndProcedure

Procedure Save(path.s)
  Protected.i fID
  
  fID = CreateFile(#PB_Any,path)
  If fID
    WriteString(fID,GetGadgetText(#G_A_ASM))
    
    CloseFile(fID)
  Else
    Error("Unable to write to "+Chr(34)+path+Chr(34))
  EndIf
  
  path = RemoveString(path,".asm.txt")+".instr.txt"
  
  fID = CreateFile(#PB_Any,path)
  If fID
    WriteString(fID,GetGadgetText(#G_A_MC))
    
    CloseFile(fID)
  Else
    Error("Unable to write to "+Chr(34)+path+Chr(34))
  EndIf
  
EndProcedure

Procedure CheckChanges()
  Shared saveChange
  Shared savePath
  
  If saveChange = 1
    Select MessageRequester("Unsaved changes!","There are unsaved changes, save now?",#PB_MessageRequester_YesNoCancel|#MB_ICONQUESTION)
      Case #PB_MessageRequester_Yes
        If savePath = "Unsaved"
          PostEvent(#PB_Event_Menu,#W_ASM,#M_A_Save)
          ProcedureReturn 1
        Else
          Save(savePath)
          ProcedureReturn 0
        EndIf
      Case #PB_MessageRequester_No
        ProcedureReturn 0
      Default
        ProcedureReturn 1
    EndSelect
  EndIf
  
  ProcedureReturn 0
  
EndProcedure

Procedure Ende()
  
  If CheckChanges()
    ProcedureReturn 0
  EndIf
  
  HideWindow(#W_ASM,1)
  
  CloseWindow(#W_ASM)
  
  End
  
EndProcedure

If #PB_Compiler_Debugger
  savePath = GetCurrentDirectory()+"multiply.asm.txt"
EndIf

;---MainWindow
If Not OpenWindow(#W_ASM,0,0,750,750,"MHRD Assembler - "+savePath,#PB_Window_ScreenCentered|#PB_Window_SystemMenu|#PB_Window_MinimizeGadget|#PB_Window_MaximizeGadget|#PB_Window_SizeGadget|#PB_Window_Invisible)
  Error("Error create assembler window")
  End
EndIf
WindowBounds(#W_ASM,750,500,#PB_Default,#PB_Default)

If Not CreateMenu(#M,WindowID(#W_ASM))
  Error("Error create assembler menu")
  End
EndIf

MenuTitle("File")
MenuItem(#M_A_New   ,"New")
MenuItem(#M_A_Load  ,"Load"+Chr(9)+"Ctrl+O") : AddKeyboardShortcut(#W_ASM,#PB_Shortcut_Control|#PB_Shortcut_O,#M_A_Load)
MenuItem(#M_A_Save  ,"Save"+Chr(9)+"Ctrl+S") : AddKeyboardShortcut(#W_ASM,#PB_Shortcut_Control|#PB_Shortcut_S,#M_A_Save)
MenuItem(#M_A_SaveAs,"Save As")
MenuBar()
MenuItem(#M_A_Close ,"Close"+Chr(9)+"Alt+F4") : AddKeyboardShortcut(#W_ASM,#PB_Shortcut_Alt|#PB_Shortcut_F4,#M_A_Close)

MenuTitle("Project")
MenuItem(#M_A_Compile,"Compile"+Chr(9)+"F5") : AddKeyboardShortcut(#W_ASM,#PB_Shortcut_F5,#M_A_Compile)
MenuItem(#M_A_AutoCompile,"Auto-Compile")
MenuBar()
MenuItem(#M_A_CompileS,"Compile Settings")

MenuTitle("Info")
MenuItem(#M_A_Help,"Help"+Chr(9)+"F1") : AddKeyboardShortcut(#W_ASM,#PB_Shortcut_F1,#M_A_Help)
MenuItem(#M_A_Info,"Info")



EditorGadget(#G_A_ASM, 10,10,360,700)                     : SetGadgetFont(#G_A_ASM,FontID(0))
EditorGadget(#G_A_MC ,380,10,360,700,#PB_Editor_ReadOnly) : SetGadgetFont(#G_A_MC ,FontID(0))



;---CompileSWindow
If Not OpenWindow(#W_ASM_CompileS,0,0,200,200,"Compile Settings",#PB_Window_ScreenCentered|#PB_Window_SystemMenu|#PB_Window_Invisible,WindowID(#W_ASM))
  Error("Error create compile settings window")
  End
EndIf

CheckBoxGadget(#G_A_CS_Error  , 10, 10,100,20,"Errors")
CheckBoxGadget(#G_A_CS_ASM    , 10, 40,100,20,"Original Line")
CheckBoxGadget(#G_A_CS_ASMLine, 10, 70,100,20,"Line Number")
CheckBoxGadget(#G_A_CS_Label  , 10,100,100,20,"Label")
CheckBoxGadget(#G_A_CS_Comment, 10,130,100,20,"Comment")
CheckBoxGadget(#G_A_CS_NL     , 10,160,100,20,"LineBreak")

SetGadgetState(#G_A_CS_NL     ,1)


;---MainLoop

If savePath <> "Unsaved"
  Load(savePath)
EndIf

HideWindow(#W_ASM,0)
Repeat
  
  event = WaitWindowEvent()
  
  Select event
    Case #PB_Event_Gadget
      Select EventWindow()
        Case #W_ASM
          Select EventGadget()
            Case #G_A_ASM
              If EventType() = #PB_EventType_Change
                SetGadgetData(#G_A_ASM,1)
                If saveChange = 0
                  saveChange = 1
                  SetWindowTitle(#W_ASM,"MHRD Assembler - "+savePath+"*")
                ElseIf saveChange = -1
                  saveChange = 0
                EndIf
              EndIf
          EndSelect
          
        Case #W_ASM_CompileS
          Select EventGadget()
            Case #G_A_CS_ASM
              If GetGadgetState(#G_A_CS_ASM)
                compileFlags = compileFlags|#ASM_ASM
              Else
                compileFlags = compileFlags&~#ASM_ASM
              EndIf
            Case #G_A_CS_ASMLine
              If GetGadgetState(#G_A_CS_ASMLine)
                compileFlags = compileFlags|#ASM_ASMLine
              Else
                compileFlags = compileFlags&~#ASM_ASMLine
              EndIf
            Case #G_A_CS_Error
              If GetGadgetState(#G_A_CS_Error)
                compileFlags = compileFlags|#ASM_Error
              Else
                compileFlags = compileFlags&~#ASM_Error
              EndIf
            Case #G_A_CS_Comment
              If GetGadgetState(#G_A_CS_Comment)
                compileFlags = compileFlags|#ASM_Comment
              Else
                compileFlags = compileFlags&~#ASM_Comment
              EndIf
            Case #G_A_CS_Label
              If GetGadgetState(#G_A_CS_Label)
                compileFlags = compileFlags|#ASM_Label
              Else
                compileFlags = compileFlags&~#ASM_Label 
              EndIf
            Case #G_A_CS_NL
              If GetGadgetState(#G_A_CS_NL)
                compileFlags = compileFlags|#ASM_NL
              Else
                compileFlags = compileFlags&~#ASM_NL
              EndIf
          EndSelect
          
          Compile(compileFlags)
          
      EndSelect
    Case #PB_Event_Timer
      If EventTimer() = #M_A_Compile
        If GetGadgetData(#G_A_ASM)
          Compile(compileFlags)
        EndIf
      EndIf
    Case #PB_Event_Menu
      Select EventMenu()
        Case #M_A_New
          If CheckChanges()
            Continue
          EndIf
          
          savePath = "Unsaved"
          SetGadgetText(#G_A_ASM,"")
          
          saveChange = 0
          SetWindowTitle(#W_ASM,"MHRD Assembler - "+savePath)
        Case #M_A_Load
          If CheckChanges()
            Continue
          EndIf
          If savePath = "Unsaved"
            path = GetHomeDirectory()
          Else
            path = savePath
          EndIf
          path = OpenFileRequester("Choose file...",path,"asm.txt|*.asm.txt|Any|*.*",0)
          If path = ""
            Continue
          EndIf
          
          savePath = path
          Load(savePath)
          
          saveChange = 0
          SetGadgetData(#G_A_ASM,1)
          SetWindowTitle(#W_ASM,"MHRD Assembler - "+savePath)
        Case #M_A_Save, #M_A_SaveAs
          If EventMenu() = #M_A_SaveAs Or savePath = "Unsaved"
            If savePath = "Unsaved"
              path = GetHomeDirectory()
            Else
              path = savePath
            EndIf
            path = SaveFileRequester("Choose save location...",path,"asm.txt|*.asm.txt|Any|*.*",0)
            If path = ""
              Continue
            EndIf
            If Right(UCase(path),8) <> ".asm.txt"
              path + ".asm.txt"
            EndIf
            If FileSize(path) <> -1
              If MessageRequester("File exists!",Chr(34)+path+Chr(34)+" already exists!"+#NL+"Overwrite?",#PB_MessageRequester_YesNo) = #PB_MessageRequester_No
                Continue
              EndIf
            EndIf
            savePath = path
          EndIf
          
          Save(savePath)
          
          saveChange = 0
          SetWindowTitle(#W_ASM,"MHRD Assembler - "+savePath)
        Case #M_A_Compile
          Compile(compileFlags)
        Case #M_A_AutoCompile
          If GetMenuItemState(#M,#M_A_AutoCompile)
            RemoveWindowTimer(#W_ASM,#M_A_Compile)
            SetMenuItemState(#M,#M_A_AutoCompile,0)
          Else
            AddWindowTimer(#W_ASM,#M_A_Compile,500)
            SetMenuItemState(#M,#M_A_AutoCompile,1)
          EndIf
        Case #M_A_CompileS
          HideWindow(#W_ASM_CompileS,0)
        Case #M_A_Close
          Ende()
      EndSelect
    Case #PB_Event_SizeWindow
      ResizeGadget(#G_A_ASM,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_ASM)-40)
      ResizeGadget(#G_A_MC ,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_ASM)-40)
    Case #PB_Event_CloseWindow
      Select EventWindow()
        Case #W_ASM
          Ende()
        Case #W_ASM_CompileS
          HideWindow(#W_ASM_CompileS,1)
      EndSelect
  EndSelect
  
ForEver





; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 1129
; FirstLine = 1095
; Folding = --
; EnableXP