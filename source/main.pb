EnableExplicit

IncludeFile "procedures.pbi"

#Benchmark = 0
#Ver_Sim = "v1.0.1 b"

;---Enumeration
Enumeration Window
  #W_Main
  
EndEnumeration
Enumeration Gadget
  #G_F_Instr
  #G_F_CPU
  #G_F_Data
  
  #G_MemInstr
  #G_MemData
  
  #G_ResetAll
  #G_Cycle
  #G_CTime
  
  #G_Instr_Load
  #G_Instr_PathT
  #G_Instr_Path
  #G_Instr_Spin
  
  #G_CPU_Step
  #G_CPU_Reset
  #G_CPU_10Hz
  #G_CPU_10kHz
  #G_CPU_10MHz
  #G_CPU_Max
  
  #G_RegAR
  #G_RegMR
  #G_RegPC
  
  #G_ALU1
  #G_ALU2
  #G_ALU3
  
  #G_Data_Load
  #G_Data_Display
  #G_Data_Spin
  #G_Data_Reset
  #G_Data_Save
  
EndEnumeration


;---Variables
Dim mem_Instr.w($FFFF)
Dim mem_Data.w($FFFF)

Define reg_AR.w
Define reg_MR.w
Define reg_PC.w

Define.i cycles, cycleT, cycleTimer

Define.i event, i, time
Define.d hz

Define.i autoStep

Define.s old
Define.s instrPath = GetCurrentDirectory()+"..\testdata\example.instr.txt"
Define.s dataPath  = GetCurrentDirectory()

;---Procedures

Procedure.i uWordToInt(word.w)
  
  !MOVZX rax, word[p.v_word]
  ProcedureReturn
  
EndProcedure

Procedure Fill_GadgetRam(gID.i,Array mem.w(1),active,val,len,highlight=-1,display=0)
  Protected.i i, pos, offset
  Protected.l big
  Protected line.s
  
  If highlight = -1
    highlight = active
  EndIf
  If highlight > len/2 And highlight < $FFFF-len/2
    offset = highlight-len/2
  ElseIf highlight >= $FFFF-len/2
    offset = $FFFF-len
  Else
    offset = 0
  EndIf
  
  ClearGadgetItems(gID)
  
  For i = -1 To len
    If i > -1
      pos = i+offset
    Else
      pos = active
    EndIf
    If pos > $FFFF Or pos < 0
      Break
    EndIf
    
    line = ""
    line+ToHex(pos)+Chr(10)+Chr(10)
    If val = 1
      Select display
        Case 0
          line+Str(mem(pos))+Chr(10)
        Case 1
          big = mem(pos)
          big&$0000FFFF
          line+Str(big)+Chr(10)
        Case 2
          big = mem(pos)>>8
          If big < 32
            big = 32
          EndIf
          line+Chr(big)
          big = mem(pos)&$00FF
          If big < 32
            big = 32
          EndIf
          line+Chr(big)+Chr(10)
        Default
          line+"E"+Chr(10)
      EndSelect
    EndIf
    line+ToHex(mem(pos))+Chr(10)
    If val = 1
      line+ToBin(mem(pos))
    Else
      line+ToBin(mem(pos),2)
    EndIf
    AddGadgetItem(gID,i+1,line)
    SetGadgetItemData(gID,i+1,mem(pos))
    If pos = highlight
      SetGadgetItemColor(gID,i+1,#PB_Gadget_BackColor,RGB(255,255,128))
    EndIf
    If i = -1
      SetGadgetItemColor(gID,i+1,#PB_Gadget_BackColor,RGB(255,128,128))
    ElseIf pos = active
      SetGadgetItemColor(gID,i+1,#PB_Gadget_BackColor,RGB(255,192,192))
    EndIf
  Next
  
  SetGadgetData(gID,offset)
  
EndProcedure


Procedure Cycle_CPU()
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  
  Protected.w instr
  Protected.w op1, op2, out
  
  instr = mem_Instr(reg_PC) 
  
  If instr&$8000 ;GetBit(instr,15)
    
    reg_MR = instr&$7FFF ;GetBits(instr,0,15);PAD?
    reg_PC+1
    
  Else
    
    If instr&$1000 ;GetBit(instr,12)
      op1 = reg_AR
    Else
      op1 = instr&%0000000000011111 ;GetBits(instr,0,5)
      If op1&%10000
        op1 = op1|%1111111111100000
      EndIf
    EndIf
    
    If instr&$0800 ;GetBit(instr,11)
      If instr&$0400 ;GetBit(instr,10)
        op2 = mem_Data(reg_MR)
      Else
        op2 = reg_MR
      EndIf
    Else
      If instr&$0400 ;GetBit(instr,10)
        op2 = reg_AR
      Else
        op2 = instr&%0000000000011111 ;GetBits(instr,0,5)
        If op2&%10000
          op2 = op2|%1111111111100000
        EndIf
      EndIf
    EndIf
    
    If instr&$0200 ;GetBit(instr,9)
      op1 = ~op1
    EndIf
    If instr&$0100 ;GetBit(instr,8)
      op2 = ~op2
    EndIf
    If instr&$0080 ;GetBit(instr,7)
      out = ~(op1&op2)
    Else
      out = op1+op2
    EndIf
    If instr&$0040 ;GetBit(instr,6)
      out = ~out
    EndIf
    
    
    If instr&$4000 ;GetBit(instr,14)
      If instr&$2000 ;GetBit(instr,13)
        mem_Data(reg_MR) = out;loadD
      Else
        reg_MR = out;loadM
      EndIf
    Else
      If instr&$2000 ;GetBit(instr,13)
        reg_AR = out;loadA
      EndIf
    EndIf
    
    If out = 0 And instr&$0020 ;GetBit(instr,5)
      reg_PC = reg_MR
    Else
      reg_PC+1
    EndIf
    
  EndIf
  
EndProcedure

Procedure ASMCycleN_CPU(n.q)
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  
  !MOV rcx, qword [p.v_n]
  
  top:
  
  !PUSH rcx
  !MOVZX rcx, word [v_reg_PC]
  ;!AND rcx, 3FFFh
  !ADD rcx, rcx
  !ADD rcx, [a_mem_Instr]
  !MOV cx, word [rcx]
  
  !BT cx, 15
  !JNC ll_asmcyclen_cpu_noconstant
    !AND cx, 7FFFh
    !MOV word [v_reg_MR], cx
    !INC word[v_reg_PC]
    !POP rcx
    !LOOP ll_asmcyclen_cpu_top
    ProcedureReturn
    
    
  noconstant:  
    op1select:
    !BT cx, 12
    !JNC ll_asmcyclen_cpu_op1c
      !MOV ax, word [v_reg_AR]
      !JMP ll_asmcyclen_cpu_op1selectfinish
    op1C:
      !MOV ax, 0000000000011111b
      !AND ax, cx
      !BT cx, 4
      !JNC ll_asmcyclen_cpu_op1selectfinish
        !OR ax, 1111111111100000b
    op1selectFinish:
    
    
    op2select:
    !BT cx, 11
    !JNC ll_asmcyclen_cpu_op2b2
      !BT cx, 10
      !JNC ll_asmcyclen_cpu_op2mr

        !MOVZX rdx, word [v_reg_MR]
        ;!AND rdx, FFFFh
        !ADD rdx, rdx
        !ADD rdx, [a_mem_Data]
        !MOV dx, word [rdx]
        
        !JMP ll_asmcyclen_cpu_op2selectfinish
      op2MR:
        !MOV dx, word [v_reg_MR]
        !JMP ll_asmcyclen_cpu_op2selectfinish
    op2B2:
      !BT cx, 10
      !JNC ll_asmcyclen_cpu_op2c
        !MOV dx, word [v_reg_AR]
        !JMP ll_asmcyclen_cpu_op2selectfinish
      op2C:
        !MOV dx, 0000000000011111b
          !AND dx, cx
          !BT cx, 4
          !JNC ll_asmcyclen_cpu_op2selectfinish
            !OR dx, 1111111111100000b
    op2selectFinish:
    
    
    alu:
    !BT cx, 9
    !JNC ll_asmcyclen_cpu_notop1
      !NOT ax
    notop1:
    !BT cx, 8
    !JNC ll_asmcyclen_cpu_notop2
      !NOT dx
    notop2:
    !BT cx, 7
    !JNC ll_asmcyclen_cpu_notand
      !AND ax, dx
      !BT cx, 6
      !JC ll_asmcyclen_cpu_alufinish
        !NOT ax
      !JMP ll_asmcyclen_cpu_alufinish
    notand:
      !ADD ax, dx
      !BT cx, 6
      !JNC ll_asmcyclen_cpu_alufinish
        !NOT ax
    aluFinish:
    

    destSelect:
    !BT cx, 14
    !JNC ll_asmcyclen_cpu_destb2
      !BT cx, 13
      !JNC ll_asmcyclen_cpu_destmr

        !MOVZX rdx, word [v_reg_MR]
        ;!AND rdx, 3FFFh
        !ADD rdx, rdx
        !ADD rdx, [a_mem_Data]
        !MOV word [rdx], ax
        
        !JMP ll_asmcyclen_cpu_destselectfinish
      destMR:
        !MOV word [v_reg_MR], ax
        !JMP ll_asmcyclen_cpu_destselectfinish
    destB2:
      !BT cx, 13
      !JNC ll_asmcyclen_cpu_destselectfinish
        !MOV word [v_reg_AR], ax
    destSelectFinish:
    
    
    !TEST ax, ax
    !JNZ ll_asmcyclen_cpu_nojump
      !BT cx, 5
        !JNC ll_asmcyclen_cpu_nojump
        !MOV ax, word[v_reg_MR]
        !MOV word[v_reg_PC], ax
        !POP rcx
        !LOOP ll_asmcyclen_cpu_bottom
        ProcedureReturn
    noJump:
    !INC word[v_reg_PC]
    !POP rcx
    !LOOP ll_asmcyclen_cpu_bottom
    ProcedureReturn
    
    bottom:
    !JMP ll_asmcyclen_cpu_top
  
EndProcedure

Procedure.s ALUtext()
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  
  Protected instr.w
  Protected text.s
  Protected.s op1,op2,out, notout, operator
  Protected.s nop1,nop2,nout
  Protected.w wop1,wop2,wout,newMR
  
  instr = mem_Instr(reg_PC)
  
  text = ""
  
  If instr&$8000 = $8000
    
    text = "                  C  »     MR" +Chr(10)
    text + "             "+ToHex(instr&$7FFF)+"  » "+ToHex(instr&$7FFF)
    
  Else
    
    Select instr&$1000
      Case $1000
        op1 = "AR"
        wop1 = reg_AR
      Case $0000
        op1 = "C"
        wop1 = instr&%0000000000011111
        If wop1&%10000
          wop1 = wop1|%1111111111100000
        EndIf
    EndSelect
    nop1 = ToHex(wop1)
    
    Select instr&$0C00
      Case $0C00
        op2 = "Data"
        wop2 = mem_Data(reg_MR)
      Case $0800
        op2 = "MR"
        wop2 = reg_MR
      Case $0400
        op2 = "AR"
        wop2 = reg_AR
      Case $0000
        op2 = "C"
        wop2 = instr&%0000000000011111
        If wop2&%10000
          wop2 = wop2|%1111111111100000
        EndIf
    EndSelect
    nop2 = ToHex(wop2)
    
    notout = " "
    If instr&$0200
      op1 = "!"+op1
      nop1 = "!"+nop1
      wop1 = ~wop1
    EndIf
    If instr&$0100
      op2 = "!"+op2
      nop2 = "!"+op2
      wop2 = ~wop2
    EndIf
    If instr&$0080
      operator = "&"
      wout = wop1&wop2
      If Not instr&$0040
        notout = "!"
        wout = ~wout
      EndIf
    Else
      operator = "+"
      wout = wop1+wop2
      If instr&$0040
        notout = "!"
        wout = ~wout
      EndIf
    EndIf
    
    
    newMR = reg_MR
    Select instr&$6000
      Case $6000
        out = "Data"
      Case $4000
        out = "MR"
        newMR = wout
      Case $2000
        out = "AR"
      Case $0000
        out = "-"
    EndSelect
    nout = ToHex(wout)
    
    text = notout+"("+RSet(op1,7)+" "+operator+" "+RSet(op2,7)+") » "+RSet(out,6) +Chr(10)
    text + notout+"("+RSet(nop1,7)+" "+operator+" "+RSet(nop2,7)+") » "+RSet(nout,6)
    
    If instr&$0020
      text + Chr(10) + "             jmpIfZ  » "
      
      If wout = 0
        text + ToHex(newMR)
      Else
        text + "   ---"
      EndIf
    EndIf
    
  EndIf
  
  ProcedureReturn text

EndProcedure

Procedure Refresh_View(part = 0)
  Shared mem_Instr()
  Shared mem_Data()
  Shared reg_AR
  Shared reg_MR
  Shared reg_PC
  Shared cycles
  
  Protected int.i
  Protected text.s
  
  If part = 0 Or part = #G_Instr_Spin
    Fill_GadgetRam(#G_MemInstr,mem_Instr(),uWordToInt(reg_PC),0,Round((WindowHeight(#W_Main)-191)/17,#PB_Round_Down),GetGadgetState(#G_Instr_Spin))
  EndIf
  If part = 0 Or part = #G_Data_Spin
    Fill_GadgetRam(#G_MemData,mem_Data(),uWordToInt(reg_MR),1,Round((WindowHeight(#W_Main)-191)/17,#PB_Round_Down),GetGadgetState(#G_Data_Spin),GetGadgetState(#G_Data_Display))
  EndIf
  
  If part > 0
    ProcedureReturn 0
  EndIf
  
  SetGadgetText(#G_Cycle,Str(cycles)+" Cycles")
  
  SetGadgetText(#G_RegPC,"< PC "+ToHex(reg_PC))
  SetGadgetText(#G_RegAR,"AR "+ToHex(reg_AR));+"/"+Str(reg_AR))
  SetGadgetText(#G_RegMR,ToHex(reg_MR)+" MR >")
  
  text = ALUtext()
  SetGadgetText(#G_ALU1,StringField(text,1,Chr(10)))
  SetGadgetText(#G_ALU2,StringField(text,2,Chr(10)))
  SetGadgetText(#G_ALU3,StringField(text,3,Chr(10)))
  
EndProcedure

Procedure LoadInstr(path.s)
  Shared mem_Instr()
  
  Protected fID.i
  Protected i.i
  Protected line.s
  
  fID = ReadFile(#PB_Any,path)
  If fID
    
    FillMemory(@mem_Instr(0),$10000*2,0,#PB_Word)
    
    While Not Eof(fID)
      line = ReadString(fID)
      line = RemoveString(line," ")
      line = RemoveString(line,Chr(9))
      
      If Len(line) = 16 And MatchRegularExpression(#RE_OnlyBin,line)
        mem_Instr(i) = Val("%"+line)
        i+1
      EndIf
      If i = $FFFF
        Break
      EndIf
    Wend
    CloseFile(fID)
    Refresh_View()
  Else
    Error("No "+path+" found!")
  EndIf
  
EndProcedure

Procedure LoadData(path.s)
  Shared mem_Data()
  
  Protected fID.i
  Protected i.i
  Protected line.s
  
  fID = ReadFile(#PB_Any,path)
  If fID
    
    FillMemory(@mem_Data(0),$10000*2,0,#PB_Word)
    
    While Not Eof(fID)
      line = LCase(ReadString(fID))
      line = RemoveString(line," ")
      line = RemoveString(line,Chr(9))
      line = RemoveString(line,"0x")
      
      If Len(line) = 4 And  MatchRegularExpression(#RE_OnlyHex,line)
        mem_Data(i) = Val("$"+line)
        i+1
      EndIf
      If i = $FFFF
        Break
      EndIf
    Wend
    CloseFile(fID)
    Refresh_View()
  Else
    Error("No "+path+" found!")
  EndIf
  
EndProcedure

Procedure SaveData(path.s)
  Shared mem_Data()
  
  Protected fID.i
  Protected i.i
  Protected line.s
  
  fID = CreateFile(#PB_Any,path)
  If fID
    For i = 0 To $FFFF
      WriteStringN(fID,ToHex(mem_Data(i)))
    Next
    CloseFile(fID)
  Else
    Error("Couldn't create "+path+"!")
  EndIf
  
EndProcedure

Procedure ResizeGadgets()
  
  Refresh_View(#G_Instr_Spin)
  Refresh_View(#G_Data_Spin)
  
  ResizeGadget(#G_F_Instr,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_Main)-50)
  ResizeGadget(#G_F_CPU  ,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_Main)-50)
  ResizeGadget(#G_F_Data ,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_Main)-50)
  
  ResizeGadget(#G_MemInstr,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_Main)-130)
  ResizeGadget(#G_MemData ,#PB_Ignore,#PB_Ignore,#PB_Ignore,WindowHeight(#W_Main)-130)
  
EndProcedure

If Not OpenWindow(#W_Main,0,0,944,800,"MHRD CPU Sim - "+#Ver_Sim,#PB_Window_ScreenCentered|#PB_Window_SystemMenu|#PB_Window_MinimizeGadget|#PB_Window_SizeGadget|#PB_Window_Invisible)
  Error("Opening Main Window")
  End
EndIf

;---Gadgets

WindowBounds(#W_Main,944,400,944,#PB_Default)

ButtonGadget(#G_ResetAll,20,10,100,25,"Reset All")
TextGadget(#G_Cycle,130,15,100,20,"0 Cycles")
TextGadget(#G_CTime,240,15,100,20,"- Hz")

FrameGadget(#G_F_Instr,10,40,264,888,"Instruction ROM")
ButtonGadget(#G_Instr_Load,20,85,100,20,"Reload Instr")
TextGadget(#G_Instr_PathT,23,58,200,20,GetFilePart(instrPath))
ButtonGadget(#G_Instr_Path,204,55,60,20,"Path")
SpinGadget(#G_Instr_Spin,204,85,60,20,-1,$FFFF) : SetGadgetText(#G_Instr_Spin,"0x0000")

ListIconGadget(#G_MemInstr,20,110,244,470,"Addr",50,#PB_ListIcon_AlwaysShowSelection|#PB_ListIcon_FullRowSelect|#PB_ListIcon_GridLines) ;: SetGadgetFont(#G_MemInstr,FontID(1))
AddGadgetColumn(#G_MemInstr,1,"",5)
AddGadgetColumn(#G_MemInstr,2,"Hex",55)
AddGadgetColumn(#G_MemInstr,3,"Bin",130)


FrameGadget(#G_F_CPU,280,40,340,888,"CPU")
ButtonGadget(#G_CPU_Reset ,290,55,100,20,"CPU Reset")
ButtonGadget(#G_CPU_Step  ,400,55,100,20,"CPU Step")
ButtonGadget(#G_CPU_10Hz  ,290,85,100,20,"CPU 10Hz",#PB_Button_Toggle)
ButtonGadget(#G_CPU_10kHz ,400,85,100,20,"CPU 10kHz",#PB_Button_Toggle)
ButtonGadget(#G_CPU_10MHz ,510,85,100,20,"CPU 10MHz",#PB_Button_Toggle)
ButtonGadget(#G_CPU_Max   ,510,55,100,20,"CPU Max",#PB_Button_Toggle)

TextGadget(#G_RegPC,290,135,100,20,"< PC 0x0000",#PB_Text_Border)                : SetGadgetFont(#G_RegPC,FontID(1))
TextGadget(#G_RegAR,400,135,100,20,"AR 0x0000",#PB_Text_Center|#PB_Text_Border)   : SetGadgetFont(#G_RegAR,FontID(1))
TextGadget(#G_RegMR,510,135,100,20,"0x0000 MR >",#PB_Text_Right|#PB_Text_Border) : SetGadgetFont(#G_RegMR,FontID(1))

TextGadget(#G_ALU1,290,180,320,25,"",#PB_Text_Center) : SetGadgetFont(#G_ALU1,FontID(0))
TextGadget(#G_ALU2,290,205,320,25,"",#PB_Text_Center) : SetGadgetFont(#G_ALU2,FontID(0))
TextGadget(#G_ALU3,290,230,320,25,"",#PB_Text_Center) : SetGadgetFont(#G_ALU3,FontID(0))

FrameGadget(#G_F_Data,630,40,304,888,"Data RAM")
ButtonGadget(#G_Data_Load,640,85,90,20,"Load Data")
;ButtonGadget(#G_Data_Path,864,55,60,20,"Path")
ComboBoxGadget(#G_Data_Display,737,55,90,20)
AddGadgetItem(#G_Data_Display,0,"Signed")
AddGadgetItem(#G_Data_Display,1,"Unsigned")
AddGadgetItem(#G_Data_Display,2,"ASCII")
SetGadgetState(#G_Data_Display,0)

ButtonGadget(#G_Data_Reset,640,55,90,20,"Clear Data")
ButtonGadget(#G_Data_Save,737,85,90,20,"Save Data")
SpinGadget(#G_Data_Spin,864,85,60,20,-1,$FFFF) : SetGadgetText(#G_Data_Spin,"0x0000")

ListIconGadget(#G_MemData,640,110,284,470,"Addr",50,#PB_ListIcon_AlwaysShowSelection|#PB_ListIcon_FullRowSelect|#PB_ListIcon_GridLines) ;: SetGadgetFont(#G_MemData,FontID(1))
AddGadgetColumn(#G_MemData,1,"",5)
AddGadgetColumn(#G_MemData,2,"Dec",50)
AddGadgetColumn(#G_MemData,3,"Hex",55)
AddGadgetColumn(#G_MemData,4,"Bin",120)



LoadInstr(instrPath)
Refresh_View()
ResizeGadgets()

HideWindow(#W_Main,0)

;---MainLoop
Repeat
  
  event = WaitWindowEvent()
  
  Select event
    Case #PB_Event_Gadget
      Select EventGadget()
        Case #G_ResetAll
          FillMemory(@mem_Data(0),$10000*2,0,#PB_Word)
          reg_PC = 0
          reg_AR = 0
          reg_MR = 0
          cycles = 0
          Refresh_View()
        Case #G_Instr_Load
          LoadInstr(instrPath)
        Case #G_Instr_Path
          old = instrPath
          instrPath = OpenFileRequester("Choose Instruction File...",instrPath,"instr.txt|*.instr.txt|Any|*.*",0)
          If instrPath = ""
            instrPath = old
          Else
            SetGadgetText(#G_Instr_PathT,GetFilePart(instrPath))
            LoadInstr(instrPath)
          EndIf
        Case #G_CPU_Reset
          reg_PC = 0
          Refresh_View()
        Case #G_CPU_Step
          If autoStep = 0
            PostEvent(#PB_Event_Timer,#W_Main,#G_CPU_Step)
          EndIf
        Case #G_CPU_10Hz, #G_CPU_10kHz, #G_CPU_10MHz, #G_CPU_Max
          If GetGadgetState(EventGadget())
            AddWindowTimer(#W_Main,#G_CPU_Step,90)
            
            Select EventGadget()
              Case #G_CPU_10Hz
                autoStep = 1
              Case #G_CPU_10kHz
                autoStep = 1000
              Case #G_CPU_10MHz
                autoStep = 1000000
              Case #G_CPU_Max
                autoStep = -1
            EndSelect
                
            cycleTimer = ElapsedMilliseconds()
            cycleT = cycles
            
            For i = #G_CPU_10Hz To #G_CPU_Max
              If Not i = EventGadget()
                SetGadgetState(i,0)
              EndIf
            Next
          Else
            SetGadgetText(#G_CTime,"- Hz")
            RemoveWindowTimer(#W_Main,#G_CPU_Step)
            autoStep = 0
          EndIf
        Case #G_Data_Load
          old = dataPath
          dataPath = OpenFileRequester("Choose Data File...",dataPath,"txt|*.txt|Any|*.*",0)
          If dataPath = ""
            dataPath = old
          Else
            LoadData(dataPath)
          EndIf
          
        Case #G_Data_Display
          Refresh_View(#G_Data_Spin)
        Case #G_Data_Reset
          FillMemory(@mem_Data(0),$10000*2,0,#PB_Word)
          Refresh_View()
        Case #G_Data_Save
          old = dataPath
          dataPath = SaveFileRequester("Choose Data File...",dataPath,"txt|*.txt|Any|*.*",0)
          If dataPath = ""
            dataPath = old
          Else
            SaveData(dataPath)
          EndIf
          
        Case #G_Instr_Spin,#G_Data_Spin
          If EventType() = #PB_EventType_Change
            If GetGadgetText(EventGadget()) = ""
              SetGadgetState(EventGadget(),-1)
            Else
              SetGadgetState(EventGadget(),Val("$"+RemoveString(GetGadgetText(EventGadget()),"0x")))
            EndIf
          Else
            If GetGadgetState(EventGadget()) = -1
              SetGadgetText(EventGadget(),"")
            Else
              SetGadgetText(EventGadget(),ToHex(GetGadgetState(EventGadget())))
            EndIf
          EndIf
          Refresh_View(EventGadget())
      EndSelect
    Case #PB_Event_Timer
      Select EventTimer()
        Case #G_CPU_Step
          If autoStep = 0
            ASMCycleN_CPU(1)
            cycles+1
          Else
CompilerIf #Benchmark
            time = ElapsedMilliseconds()
            ASMCycleN_CPU(100000000)
            hz = 100000000000/(ElapsedMilliseconds()-time)
            cycles+100000000
CompilerElse  
            If autoStep = -1
              time = ElapsedMilliseconds()
              Repeat
                ASMCycleN_CPU(10000)
                cycles+10000
              Until ElapsedMilliseconds()-time > 85
            Else
              ASMCycleN_CPU(autoStep)
              cycles+autoStep
            EndIf
            hz = (cycles-cycleT)/((ElapsedMilliseconds()-cycleTimer)/1000)            
CompilerEndIf
            If hz > 1000000
              SetGadgetText(#G_CTime,StrD(hz/1000000,1)+" MHz")
            ElseIf hz > 1000
              SetGadgetText(#G_CTime,StrD(hz/1000,1)+" kHz")
            Else
              SetGadgetText(#G_CTime,StrD(hz,1)+" Hz")
            EndIf
          EndIf
          Refresh_View()
      EndSelect
    Case #PB_Event_SizeWindow
      ResizeGadgets()
    Case #PB_Event_CloseWindow
      If EventWindow() = #W_Main
        End
      EndIf
  EndSelect
  
ForEver






; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 69
; FirstLine = 25
; Folding = -4
; EnableXP
; EnableUnicode