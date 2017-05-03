Procedure ASMCycle_CPU()
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  
  
  !MOVSX rcx, word [v_reg_PC]
  !AND rcx, 3FFFh
  !ADD rcx, rcx
  !ADD rcx, [a_mem_Instr]
  !MOV cx, word [rcx]
  
  !BT cx, 15
  !JNC ll_asmcycle_cpu_noconstant
    !AND cx, 7FFFh
    !MOV word [v_reg_MR], cx
    !INC word[v_reg_PC]
    ProcedureReturn
    
    
  noconstant:  
    op1select:
    !BT cx, 12
    !JNC ll_asmcycle_cpu_op1c
      !MOV ax, word [v_reg_AR]
      !JMP ll_asmcycle_cpu_op1selectfinish
    op1C:
      !MOV ax, 0000000000011111b
      !AND ax, cx
      !BT cx, 4
      !JNC ll_asmcycle_cpu_op1selectfinish
        !OR ax, 1111111111100000b
    op1selectFinish:
    
    
    op2select:
    !BT cx, 11
    !JNC ll_asmcycle_cpu_op2b2
      !BT cx, 10
      !JNC ll_asmcycle_cpu_op2mr

        !MOVSX rdx, word [v_reg_MR]
        !AND rdx, 3FFFh
        !ADD rdx, rdx
        !ADD rdx, [a_mem_Data]
        !MOV dx, word [rdx]
        
        !JMP ll_asmcycle_cpu_op2selectfinish
      op2MR:
        !MOV dx, word [v_reg_MR]
        !JMP ll_asmcycle_cpu_op2selectfinish
    op2B2:
      !BT cx, 10
      !JNC ll_asmcycle_cpu_op2c
        !MOV dx, word [v_reg_AR]
        !JMP ll_asmcycle_cpu_op2selectfinish
      op2C:
        !MOV dx, 0000000000011111b
          !AND dx, cx
          !BT cx, 4
          !JNC ll_asmcycle_cpu_op2selectfinish
            !OR dx, 1111111111100000b
    op2selectFinish:
    
    
    alu:
    !BT cx, 9
    !JNC ll_asmcycle_cpu_notop1
      !NOT ax
    notop1:
    !BT cx, 8
    !JNC ll_asmcycle_cpu_notop2
      !NOT dx
    notop2:
    !BT cx, 7
    !JNC ll_asmcycle_cpu_notand
      !AND ax, dx
      !BT cx, 6
      !JC ll_asmcycle_cpu_alufinish
        !NOT ax
      !JMP ll_asmcycle_cpu_alufinish
    notand:
      !ADD ax, dx
      !BT cx, 6
      !JNC ll_asmcycle_cpu_alufinish
        !NOT ax
    aluFinish:
    

    destSelect:
    !BT cx, 14
    !JNC ll_asmcycle_cpu_destb2
      !BT cx, 13
      !JNC ll_asmcycle_cpu_destmr

        !MOVSX rdx, word [v_reg_MR]
        !AND rdx, 3FFFh
        !ADD rdx, rdx
        !ADD rdx, [a_mem_Data]
        !MOV word [rdx], ax
        
        !JMP ll_asmcycle_cpu_destselectfinish
      destMR:
        !MOV word [v_reg_MR], ax
        !JMP ll_asmcycle_cpu_destselectfinish
    destB2:
      !BT cx, 13
      !JNC ll_asmcycle_cpu_destselectfinish
        !MOV word [v_reg_AR], ax
    destSelectFinish:
    
    
    !TEST ax, ax
    !JNZ ll_asmcycle_cpu_nojump
      !BT cx, 5
        !JNC ll_asmcycle_cpu_nojump
        !MOV ax, word[v_reg_MR]
        !MOV word[v_reg_PC], ax
        ProcedureReturn
    noJump:
      !INC word[v_reg_PC]
  
EndProcedure


Procedure OldCycle_CPU()
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  Shared cycles
  
  Protected.w instr, datas
  Protected.w op1, op2, out
  
  instr = mem_Instr(reg_PC&$3FFF)
  datas = mem_Data(reg_MR&$3FFF)
  
  
  If GetBit(instr,15)
    
    reg_MR = GetBits(instr,0,15);PAD?
    
  Else
    
    If GetBit(instr,12)
      op1 = reg_AR
    Else
      op1 = GetBits(instr,0,5)
      If op1&%10000
        op1 = op1|%1111111111100000
      EndIf
    EndIf
    
    If GetBit(instr,11)
      If GetBit(instr,10)
        op2 = datas
      Else
        op2 = reg_MR
      EndIf
    Else
      If GetBit(instr,10)
        op2 = reg_AR
      Else
        op2 = GetBits(instr,0,5)
        If op2&%10000
          op2 = op2|%1111111111100000
        EndIf
      EndIf
    EndIf
    
    If GetBit(instr,9)
      op1 = ~op1
    EndIf
    If GetBit(instr,8)
      op2 = ~op2
    EndIf
    If GetBit(instr,7)
      out = ~(op1&op2)
    Else
      out = op1+op2
    EndIf
    If GetBit(instr,6)
      out = ~out
    EndIf
    
    If GetBit(instr,14)
      If GetBit(instr,13)
        mem_Data(reg_MR&$3FFF) = out;loadD
      Else
        reg_MR = out;loadM
      EndIf
    Else
      If GetBit(instr,13)
        reg_AR = out;loadA
      EndIf
    EndIf
    
    If out = 0 And GetBit(instr,5)
      reg_PC = reg_MR-1
    EndIf
    
  EndIf
  
  reg_PC+1
  cycles+1
  
EndProcedure

Procedure SlowerCycle_CPU()
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  Shared cycles
  
  Protected.w instr
  Protected.w op1, op2, out
  
  instr = mem_Instr(reg_PC&$3FFF)
  
  If instr&$8000
    
    reg_MR = instr&$7FFF ;PAD?
    
  Else
    
    If instr&$1000
      op1 = reg_AR
    Else
      op1 = instr&%0000000000011111
      If op1&%10000
        op1 = op1|%1111111111100000
      EndIf
    EndIf
    
    If Not instr&$0C00
      op2 = instr&%0000000000011111
      If op2&%10000
        op2 = op2|%1111111111100000
      EndIf
    ElseIf instr&$0C00 = $0400
      op2 = reg_AR
    ElseIf instr&$0C00 = $0800
      op2 = reg_MR
    Else
      op2 = mem_Data(reg_MR&$3FFF)
    EndIf
    
    If instr&$0200
      op1 = ~op1
    EndIf
    If instr&$0100
      op2 = ~op2
    EndIf
    If instr&$0080
      out = ~(op1&op2)
    Else
      out = op1+op2
    EndIf
    If instr&$0040
      out = ~out
    EndIf
    
    If instr&$6000 = $6000
      mem_Data(reg_MR&$3FFF) = out;loadD
    ElseIf instr&$6000 = $2000
      reg_AR = out;loadA
    ElseIf instr&$6000 = $4000
      reg_MR = out;loadM
    EndIf
    
    
    If out = 0 And instr&$0020
      reg_PC = reg_MR-1
    EndIf
    
  EndIf
  
  reg_PC+1
  cycles+1
  
EndProcedure




Procedure.s CompileMnemnonicOld(mnemno.s)
  Shared labels()
  Shared line
  
  Dim op.op(3)
  
  Protected.i i, val
  Protected.s mc
  Protected.s instr, argu, op1, op2, op3
  
  mnemno = Trim(LCase(mnemno))
  
  If mnemno = "@@"
    ProcedureReturn "LABEL anonymous"
  EndIf
  
  If MatchRegularExpression(#RE_ASMLabel,mnemno)
    instr = Left(mnemno,Len(mnemno)-1)
    If FindMapElement(labels(),instr)
      ProcedureReturn "ERROR label already in use"
    Else
      AddMapElement(labels(),instr,#PB_Map_NoElementCheck)
      labels(instr) = line
      ProcedureReturn "LABEL "+instr+" "+ToHex(line)
    EndIf
  EndIf
  
  instr =  StringField(mnemno,1," ")
  argu =  RemoveString(Mid(mnemno,FindString(mnemno," "))," ")
  
  For i = 0 To 2
    op(i)\text = StringField(argu,i+1,",")
    
    Select op(i)\text
      Case ""
        op(i)\type = #Op_Empty
        op(i)\bin  = "00"
      Case "mr"
        op(i)\type = #Op_Reg_MR
        op(i)\bin  = "10"
      Case "ar"
        op(i)\type = #Op_Reg_AR
        op(i)\bin  = "01"
      Case "[mr]"
        op(i)\type = #Op_Reg_RAM
        op(i)\bin  = "11"
      Default
        If MatchRegularExpression(#RE_OnlyAddr,op(i)\text)
          op(i)\type = #Op_Addr
          op(i)\bin  = "11"
          val        = CStr(Mid(op(i)\text,2,Len(op(i)\text)-2))
          If val > $3FFF Or val < 0
            ProcedureReturn "ERROR address out of range"
          EndIf
          op(i)\val  = val
        ElseIf MatchRegularExpression(#RE_OnlyImme,op(i)\text)
          op(i)\type = #Op_Imme
          op(i)\bin  = "00"
          val        = CStr(op(i)\text)
;           If val > $FFFF Or val < 0
;             ProcedureReturn "ERROR immediate out of range"
;           EndIf
          If val >= -16 And val < 16
            op(i)\vals = 0
          ElseIf val >= 0 And val < 32768
            op(i)\vals = 1
          ElseIf val >= -32768 And val < 32768
            op(i)\vals = 2
          Else
            ProcedureReturn "ERROR immediate out of range"
          EndIf
          op(i)\val  = val
        ElseIf MatchRegularExpression(#RE_OnlyAtLabel,op(i)\text)
          op(i)\type = #Op_Label
          op(i)\bin = "xx"
          If op(i)\text = "@b" Or op(i)\text = "@r"
            op(i)\val = -1
          ElseIf op(i)\text = "@f"
            op(i)\val = -2
          ElseIf FindMapElement(labels(),Mid(op(i)\text,2))
            op(i)\val =  labels()
          Else
            ProcedureReturn "ERROR unknown label"
          EndIf
        Else
          ProcedureReturn "ERROR illegal argument"
        EndIf
    EndSelect
  Next
  
  Select instr
    Case "mov"
      
      
      If op(2)\type <> #Op_Empty
        ProcedureReturn "ERROR third operand not supported"
      EndIf
      Select op(0)\type
          
          
        Case #Op_Addr
          Select op(1)\type
            Case #Op_Imme
              Select op(1)\vals
                Case 0
                  mc = "0 01 0 00 0011 0 "+RSet(Right(Bin(op(1)\val),5),5,"0") + #NL
                  mc + "1 "+Right(ToBin(op(0)\val),18)                         + #NL
                  mc + "0 11 0 01 0000 0 00000"
                Case 1
                  mc = "1 "+Right(ToBin(op(1)\val),18)                         + #NL
                  mc + "0 01 0 10 0000 0 00000"                                + #NL
                  mc + "1 "+Right(ToBin(op(0)\val),18)                         + #NL
                  mc + "0 11 0 01 0000 0 00000"
                Case 2
                  mc = "1 "+Right(ToBin(~op(1)\val),18)                        + #NL
                  mc + "0 01 0 10 0100 0 00000"                                + #NL
                  mc + "1 "+Right(ToBin(op(0)\val),18)                         + #NL
                  mc + "0 11 0 01 0000 0 00000"
                Default
                  ProcedureReturn "ERROR unknown immediate error"
              EndSelect
            Case #Op_Reg_MR
              mc = "0 01 0 10 0000 0 00000"                                    + #NL
              mc + "1 "+Right(ToBin(op(0)\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Case #Op_Reg_AR
              mc = "1 "+Right(ToBin(op(0)\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(op(1)\val),18)                             + #NL
              mc + "0 01 0 11 0000 0 00000"                                    + #NL
              mc + "1 "+Right(ToBin(op(0)\val),18)                             + #NL
              mc + "0 11 0 01 0000 0 00000"
            Default
              ProcedureReturn "ERROR unsupported source"
          EndSelect
          
          
        Case #Op_Reg_RAM
          Select op(1)\type
            Case #Op_Imme
              Select op(1)\vals
                Case 0
                  mc = "0 11 0 00 0011 0 "+RSet(Right(Bin(op(1)\val),5),5,"0")
                Case 1,2
                  ProcedureReturn "ERROR cant write big values direct to [MR]"
                Default
                  ProcedureReturn "ERROR unknown immediate error"
              EndSelect
            Case #Op_Reg_MR
              mc = "0 11 0 10 0000 0 00000";SINN?
            Case #Op_Reg_AR
              mc = "0 11 0 01 0000 0 00000"
            ;Case #Op_Addr
            ;  ProcedureReturn "ERROR cant write from RAM to [MR]"
            Default
              ProcedureReturn "ERROR unsupported source"
          EndSelect
          
          
        Case #Op_Reg_AR
          Select op(1)\type
            Case #Op_Imme
              Select op(1)\vals
                Case 0
                  mc = "0 01 0 00 0011 0 "+RSet(Right(Bin(op(1)\val),5),5,"0")
                Case 1
                  mc = "1 "+Right(ToBin(op(1)\val),18) + #NL
                  mc + "0 01 0 10 0000 0 00000"
                Case 2
                  mc = "1 "+Right(ToBin(~op(1)\val),18) + #NL
                  mc + "0 01 0 10 0100 0 00000"
                Default
                  ProcedureReturn "ERROR unknown immediate error"
              EndSelect
            Case #Op_Reg_MR
              mc = "0 01 0 10 0000 0 00000"
            Case #Op_Reg_RAM
              mc = "0 01 0 11 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(op(1)\val),18) + #NL
              mc + "0 01 0 11 0000 0 00000"
            Default
              ProcedureReturn "ERROR unsupported source"
          EndSelect
          
          
        Case #Op_Reg_MR
          Select op(1)\type
            Case #Op_Imme
              Select op(1)\vals
                Case 0,1
                  mc = "1 "+Right(ToBin(op(1)\val),18)
                Case 2
                  mc = "1 "+Right(ToBin(~op(1)\val),18) + #NL
                  mc + "0 10 0 10 0100 0 00000"
                Default
                  ProcedureReturn "ERROR unknown immediate error"
              EndSelect
            Case #Op_Reg_AR
              mc = "0 10 0 01 0000 0 00000"
            Case #Op_Reg_RAM
              mc = "0 10 0 11 0000 0 00000"
            Case #Op_Addr
              mc = "1 "+Right(ToBin(op(1)\val),18) + #NL
              mc + "0 10 0 11 0000 0 00000"
            Default
              ProcedureReturn "ERROR unsupported source"
          EndSelect
          
          
        Default
          ProcedureReturn "ERROR unsupported destination"
      EndSelect
      
      
    Case "add", "sub", "and", "nand", "or",  "addjz", "subjz", "andjz", "nandjz", "orjz"
      
      
      If op(1)\type = #Op_Imme Or op(1)\type = #Op_Reg_AR
        If op(2)\type = #Op_Addr And op(2)\type = #Op_Empty
          ProcedureReturn "ERROR unsupported operands"
        EndIf
      Else
        If op(2)\type = #Op_Imme Or op(2)\type = #Op_Reg_AR
          CopyStructure(@op(2),@op(3),op)
          CopyStructure(@op(1),@op(2),op)
          CopyStructure(@op(3),@op(1),op)
          i = 1
        Else
          ProcedureReturn "ERROR unsupported operands"
        EndIf
      EndIf
      
      If op(1)\type = #Op_Imme And op(2)\type = #Op_Imme And op(1)\val <> op(2)\val
        ProcedureReturn "ERROR can't use two immediates"
      ElseIf op(1)\vals > 0 Or op(2)\vals > 0
        ProcedureReturn "ERROR can't use big immediates"
      EndIf
      
      If op(0)\type <> #Op_Empty And op(0)\type <> #Op_Reg_AR And op(0)\type <> #Op_Reg_MR And op(0)\type <> #Op_Reg_RAM
        ProcedureReturn "ERROR unsupported destination"
      EndIf
      If op(1)\type <> #Op_Imme And op(1)\type <> #Op_Reg_AR And op(2)\type <> #Op_Imme And op(2)\type <> #Op_Reg_AR And op(2)\type <> #Op_Reg_MR And op(2)\type <> #Op_Reg_RAM
        ProcedureReturn "ERROR unsupported operands"
      EndIf
      
      mc = "0 "+op(0)\bin+" "+Right(op(1)\bin,1)+" "+op(2)\bin
      
      Select Left(instr,2)
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
          ProcedureReturn "ERROR unknown opcode error"
      EndSelect
      
      If Right(instr,2) = "jz"
        mc + " 1 "
      Else
        mc + " 0 "
      EndIf
      
      mc + RSet(Right(Bin(val),5),5,"0")
      
      
    Case "inc", "dec"
      
      
      If op(1)\type <> #Op_Empty Or op(2)\type <> #Op_Empty
        ProcedureReturn "ERROR second, third operand not supported"
      EndIf
      If op(0)\type = #Op_Imme Or op(0)\type = #Op_Empty Or op(0)\type = #Op_Addr
        ProcedureReturn "ERROR unsupported operand"
      EndIf
      
      mc = "0 "+op(0)\bin+" 0 "+op(0)\bin+" 0000 0 "
      If instr = "inc"
        mc + "00001"
      Else
        mc + "11111"
      EndIf
      
      
    Case "jmp"
      
      
      If op(0)\type <> #Op_Label And op(1)\type <> #Op_Empty And op(2)\type <> #Op_Empty
        ProcedureReturn "ERROR unsupported operand"
      EndIf
      
      
      mc = "1 "+Right(ToBin(op(1)\val),18) + #NL
      mc + "0 00 0 00 0000 1 00000"
      
      
    Case "jz"
      
      If op(0)\type <> #Op_Label And op(2)\type <> #Op_Empty And Not (op(1)\type = #Op_Reg_AR Or op(1)\type = #Op_Reg_MR Or op(1)\type = #Op_Reg_RAM)
        ProcedureReturn "ERROR unsupported operand"
      EndIf
      
      mc = "1 "+Right(ToBin(op(1)\val),18) + #NL
      mc + "0 00 0 "+op(1)\bin+" 0000 1 00000"
      
    Case "nop"
      mc = "0 00 0 00 0000 0 00000"
    Default
      ProcedureReturn "ERROR unknown instruction"
  EndSelect
  
  line + 1 + CountString(mc,#NL)
  ProcedureReturn mc
  
EndProcedure

Procedure OldCompileASM()
  Shared labels()
  Shared line
  
  Protected.i i
  Protected.s mctext, text
  
  ClearMap(labels())
  line = 0
  
  For i = 0 To CountGadgetItems(#G_A_ASM)-1
    If MatchRegularExpression(#RE_OnlyBin,RemoveString(GetGadgetItemText(#G_A_ASM,i)," "))
      mctext + ToBin(Val("%"+RemoveString(GetGadgetItemText(#G_A_ASM,i)," ")),2) + #NL
    ElseIf Len(GetGadgetItemText(#G_A_ASM,i)) And Left(GetGadgetItemText(#G_A_ASM,i),2) <> "//"
      
      text   = CompileMnemnonicOld(GetGadgetItemText(#G_A_ASM,i))
      Select Left(text,5)
        Case "ERROR"
          mctext + "//" + GetGadgetItemText(#G_A_ASM,i) + #NL
          ;SetGadgetItemColor(#G_A_ASM,i,#PB_Gadget_BackColor,RGB(255,128,128))
        Case "LABEL"
          
        Default
          mctext + "//" + GetGadgetItemText(#G_A_ASM,i) + #NL
          ;SetGadgetItemColor(#G_A_ASM,i,#PB_Gadget_BackColor,#PB_Default)
      EndSelect
      mctext + text + #NL + #NL
    EndIf
  Next
  
  ClearGadgetItems(#G_A_MC)
  SetGadgetText(#G_A_MC,mctext)
  
EndProcedure
; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 651
; FirstLine = 601
; Folding = -
; EnableXP