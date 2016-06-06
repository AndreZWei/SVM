{- file: svm.elm
    author: Zhangyang Wei
    date: May 31, 2016

    This program implements the SVM.
-}
import Html exposing (..)
import Html.App exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


-- Main program implementing an svm







type alias Registers = {r0: Int, r1: Int, r2: Int, r3: Int}

string_of_regs: Registers -> String
string_of_regs r = 
    let 
      r0s = toString r.r0  
      r1s = toString r.r1  
      r2s = toString r.r2  
      r3s = toString r.r3 
    in 
      "{r0=" ++ r0s ++ "; r1=" ++ r1s ++ "; r2=" ++ r2s ++ "; r3=" ++ r3s ++ "}"

-- Names for the registers
type Reg = R0 | R1 | R2 | R3 | Zero

string_of_reg: Reg -> String
string_of_reg r = 
    case r of 
        R0 -> "R0"
        R1 -> "R1"
        R2 -> "R2"
        R3 -> "R3"
        Zero -> "Zero"

reg_of_string: String -> Reg
reg_of_string s = 
    case s of 
        "R0" -> R0
        "R1" -> R1
        "R2" -> R2
        "R3" -> R3
        "Zero" -> Zero 
        _ -> Debug.crash "Not a valid register name" 

registerGet: Reg -> Registers -> Int
registerGet register r =
    case register of 
        R0 -> r.r0
        R1 -> r.r1
        R2 -> r.r2
        R3 -> r.r3
        Zero -> 0

registerPut: Int -> Reg -> Registers -> Registers
registerPut value dest r =
    case dest of 
        R0 -> {r | r0 = value}
        R1 -> {r | r1 = value}
        R2 -> {r | r2 = value}
        R3 -> {r | r3 = value}
        Zero -> Debug.log "Can't write in Zero Register" r

type Instruction = 
      Lod {rd: Reg, offset: Int, rs: Reg}   --  Let base be the contents of register Rs, loads RAM[base + offset] into register Rd.
    | Li {rd: Reg, number: Int}             --  Loads number into register Rd
    | Sto {rs: Reg, offset: Int, rd: Reg}   --  Let base be the contents of register Rd, stores the contents of register Rs into location base + offset in the memory
    | Mov {rd: Reg, rs: Reg}                --  Copies the content of register Rs into the register Rd
    | Add {rd: Reg, rs: Reg, rt: Reg}       --  Adds the contents of registers Rs and Rt and stores the sum in register Rd
    | Sub {rd: Reg, rs: Reg, rt: Reg}       --  Subtracts the contents of register Rt from Rs and stores the difference in register Rd
    | Mul {rd: Reg, rs: Reg, rt: Reg}       --  Multiplies the contents of registers Rs and Rt and stores the sum in register Rd
    | Div {rd: Reg, rs: Reg, rt: Reg}       --  Divides the contents of register Rt from Rs and stores the difference in register Rd
    | Cmp {rs: Reg, rt: Reg}                --  Sets pwc = Rs - Rt
    | Jsr Int                               --  Sets RA = PC and then PC = PC + disp
    | R                                     --  Sets PC = RA
    | Blt Int                               --  If PSW is negative, PC = PC + disp
    | Beq Int                               --  If PSW == 0, PC = PC + disp
    | Bgt Int                               --  If PSW is positive, PC = PC + disp
    | Jmp Int                               --  PC = PC + disp
    | Hlt                                   --  Print the contents of registers PC, PSW, RA, R0, R1, R2 and R3. It then stops, returning ().


string_of_instruction: Instruction -> String
string_of_instruction i = 
    let si = toString in 
    let sr = string_of_reg in 
    case i of 
         Lod {rd, offset, rs} -> "Lod\t" ++ (sr rd) ++ ", " ++ (si offset) ++ "(" ++ (sr rs) ++ ")"
         Li  {rd, number} -> "Li\t" ++ (sr rd) ++ ", " ++ (si number)
         Sto {rs, offset, rd} -> "Sto\t" ++ (sr rs) ++ ", " ++ (si offset) ++ "(" ++ (sr rd) ++ ")"
         Mov {rd, rs} -> "Mov\t" ++ (sr rd) ++ ", " ++ (sr rs)
         Add {rd, rs, rt} -> "Add\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Sub {rd, rs, rt} -> "Sub\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Mul {rd, rs, rt} -> "Mul\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Div {rd, rs, rt} -> "Div\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Cmp {rs, rt} -> "Cmp\t" ++ (sr rs) ++ ", " ++ (sr rt)
         Blt disp -> "Blt\t" ++ (si disp)
         Beq disp -> "Beq\t" ++ (si disp)
         Bgt disp -> "Bgt\t" ++ (si disp)
         Jmp disp -> "Jmp\t" ++ (si disp)
         Jsr disp -> "Jsr\t" ++ (si disp)
         R -> "R"
         Hlt -> "Hlt"

type alias Datasegment = List Int
type alias Textsegment = List Instruction

type alias Image = {data: Datasegment, text: Textsegment}


firstElement: List a -> a  --A helper function for ramGet
firstElement list = 
    case list of 
        [] -> Debug.crash "Index Out of Bound"
        x::xs -> x

ramGet: Int -> List a -> a
ramGet n segment = 
    case n of
        0 -> firstElement segment 
        _ -> firstElement (List.drop n segment)


ramPut: a -> Int -> List a -> List a
ramPut v n segment = 
    case n of 
        0 -> v::segment
        _ -> List.append (List.take (n-1) segment) (v::(List.drop (n-1) segment))



-- Debugging Support
debug: Bool 
debug = True

step: Bool
step = True

printState: Int -> Int -> Int -> Registers -> ()
printState pc psw ra registers =
    let regstr = string_of_regs registers in 
    let si = toString 
    in 
    Debug.log ("\n dbg: (pc, psw, ra) = (" ++ si pc ++ "," ++ si psw ++ "," ++ si ra ++ ")\n" ++ "dbg: regs =" ++ regstr) ()

read_line: () -> ()
read_line () = ()

dbgOut: Int -> Int -> Int -> Registers -> ()
dbgOut pc psw ra registers =
    case (debug, step) of 
        (True, True) -> let () = 
                            printState pc psw ra registers
                        in 
                            read_line()
        (True, False) -> printState pc psw ra registers
        _ -> ()

{- An implementation of the Simple Virtual Machine. A call (svm ram pc)
   executes the SVM program starting with instruction in the text segment
   of image at pc.
-}

{- The CPU Instruction Cycle

cycle: Int -> Int -> Int -> Registers -> Image -> ()
cycle pc psw ra registers img = 
    let instruction = ramGet pc img.text
        newpc = pc + 1
        () = dbgOut pc psw ra registers
    in 
    case instruction of
        Lod {rd, offset, rs} ->
            let 
                addr = offset + (registerGet rs registers)
                value = ramGet addr img.data
                newRegisters = registerPut value rd registers
            in 
                cycle newpc psw ra newRegisters img
        
        Li {rd, number} ->
            let 
                newRegisters = registerPut number rd registers
            in 
                cycle newpc psw ra newRegisters img
        
        Sto {rs, offset, rd} ->
            let 
                addr = offset + (registerGet rs registers)
                value = registerGet rs registers
                newData = ramPut value addr img.data
                newImage = {text = img.text, data = newData}
            in 
                cycle newpc psw ra registers newImage

        Mov {rd, rs} -> 
            let 
                v = registerGet rs registers 
                newRegisters = registerPut v rd registers
            in 
                cycle newpc psw ra newRegisters img

        Add {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 + v2) rd registers 
            in
                cycle newpc psw ra newRegisters img

        Sub {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 - v2) rd registers 
            in
                cycle newpc psw ra newRegisters img

        Mul {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 * v2) rd registers 
            in
                cycle newpc psw ra newRegisters img  

        Div {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 // v2) rd registers 
            in
                cycle newpc psw ra newRegisters img    

        Cmp {rs, rt} ->
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                value = v1 - v2
            in 
                cycle newpc value ra registers img

        Blt disp ->
            case psw < 0 of 
                True -> cycle (newpc + disp) psw ra registers img
                False -> cycle newpc psw ra registers img

        Beq disp ->
            case psw == 0 of 
                True -> cycle (newpc + disp) psw ra registers img
                False -> cycle newpc psw ra registers img

        Bgt disp ->
            case psw > 0 of 
                True -> cycle (newpc + disp) psw ra registers img
                False -> cycle newpc psw ra registers img

        Jmp disp -> cycle (newpc + disp) psw ra registers img

        Jsr disp -> cycle (newpc + disp) psw newpc registers img

        R -> cycle ra psw ra registers img

        Hlt -> 
            let 
                () = (Debug.log "\nSVM Halt" (), printState pc psw ra registers)
            in 
                ()


svm: Image -> Int -> ()
svm img pc = 
    let 
        initialPSW = 0
        initialRA = 0
        initialRegisters = {r0 = 0, r1 = 0, r2 = 0, r3 = 0 }
    in 
        cycle pc initialPSW initialRA initialRegisters img-}

-- An example of a virtual machine program that counts the data


 
data = 
    [2, 2, 2, 2, 4, 5, 6, 7, -1]


text =
    [
        Mov {rd=R0, rs=Zero},             -- R0 is the counter 
        Mov {rd=R1, rs=R0},               -- R1 is the address of a number 
        Li  {rd=R2, number=1},            -- R2 is for incrementing 
        Lod {rd=R3, offset=0, rs=R1},     -- R3 is the number in the data 
        Cmp {rs=R3, rt=Zero},            
        Blt 3,
        Add {rd=R1, rs=R1, rt=R2},
        Add {rd=R0, rs=R0, rt=R2},
        Jmp (-6),
        Hlt
    ]  
 
image = 
    {data=data, text=text}


main = beginnerProgram {model = model, update = update, view = view }

--Model 

type alias Model = {registers: Registers, pc: Int, ra: Int, psw: Int, image: Image, ptInstruction: String}

model: Model
model =
    {registers = {r0 = 0, r1 = 0, r2 = 0, r3 = 0}, pc = 0, ra = 0, psw = 0, image = {text = [], data = []}, ptInstruction = ""}


--Update

type Msg = 
      Instructions
    | SaveInstruction String
    | Run
    | RAM String
    | Step

--A helper function for processing instructions

processCode: List String -> Instruction
processCode s = 
    case s of 
        ["LOD",rd,offset,rs] -> Lod {rd = reg_of_string rd, offset = Result.withDefault 0 (String.toInt offset), rs = reg_of_string rs}
        ["LI",rd,number] -> Li {rd = reg_of_string rd, number = Result.withDefault 0 (String.toInt number)}
        ["STO",rs,offset,rd] -> Sto {rs = reg_of_string rs, offset = Result.withDefault 0 (String.toInt offset), rd = reg_of_string rd}
        ["MOV",rd,rs] -> Mov {rd = reg_of_string rd, rs = reg_of_string rs}
        ["ADD",rd,rs,rt] -> Add {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt}
        ["SUB",rd,rs,rt] -> Sub {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt}
        ["MUL",rd,rs,rt] -> Mul {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt}
        ["DIV",rd,rs,rt] -> Div {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt}
        ["CMP",rs,rt] -> Cmp {rs = reg_of_string rs, rt = reg_of_string rt}
        ["BLT",disp] -> Blt (Result.withDefault 0 (String.toInt disp))
        ["BEQ",disp] -> Beq (Result.withDefault 0 (String.toInt disp))      
        ["BGT",disp] -> Bgt (Result.withDefault 0 (String.toInt disp))
        ["JMP",disp] -> Jmp (Result.withDefault 0 (String.toInt disp))
        ["JSR",disp] -> Jsr (Result.withDefault 0 (String.toInt disp))
        ["R"] -> R
        ["HLT"] -> Hlt
        _ -> Debug.crash "Invalid instruction"


cycle: Model -> Model
cycle model = 
    let
        instruction = ramGet model.pc model.image.text 
        registers = model.registers
        pc = model.pc
        ra = model.ra
        psw = model.psw
        img = model.image
        ptInstruction = model.ptInstruction
        newpc = pc + 1
    in 

    case instruction of
        Lod {rd, offset, rs} ->
            let 
                addr = offset + (registerGet rs registers)
                value = ramGet addr img.data
                newRegisters = registerPut value rd registers
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel
        
        Li {rd, number} ->
            let 
                newRegisters = registerPut number rd registers
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel
        
        Sto {rs, offset, rd} ->
            let 
                addr = offset + (registerGet rs registers)
                value = registerGet rs registers
                newData = ramPut value addr img.data
                newImage = {text = img.text, data = newData}
                newModel = {registers = registers, pc = newpc, ra = ra, psw = psw, image = newImage, ptInstruction = ptInstruction}
            in 
                newModel

        Mov {rd, rs} -> 
            let 
                v = registerGet rs registers 
                newRegisters = registerPut v rd registers
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel

        Add {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 + v2) rd registers 
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel

        Sub {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 - v2) rd registers 
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel

        Mul {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 * v2) rd registers 
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel  

        Div {rd, rs, rt} -> 
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                newRegisters = registerPut (v1 // v2) rd registers 
                newModel = {registers = newRegisters, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
            in 
                newModel    

        Cmp {rs, rt} ->
            let 
                v1 = registerGet rs registers 
                v2 = registerGet rt registers 
                value = v1 - v2
                newModel = {registers = registers, pc = newpc, ra = ra, psw = value, image = img, ptInstruction = ptInstruction}
            in 
                newModel

        Blt disp ->
            case psw < 0 of 
                True -> 
                let
                    newModel = {registers = registers, pc = pc + disp, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel
                False ->
                let 
                    newModel = {registers = registers, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        Beq disp ->
            case psw == 0 of 
                True -> 
                let
                    newModel = {registers = registers, pc = pc + disp, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel
                False -> 
                let
                    newModel = {registers = registers, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        Bgt disp ->
            case psw > 0 of 
                True -> 
                let
                    newModel = {registers = registers, pc = pc + disp, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel
                False -> 
                let
                    newModel = {registers = registers, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        Jmp disp -> 
                let
                    newModel = {registers = registers, pc = pc + disp, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        Jsr disp -> 
                let
                    newModel = {registers = registers, pc = pc + disp, ra = newpc, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        R -> 
                let
                    newModel = {registers = registers, pc = ra, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

        Hlt -> 
                let 
                    () = (Debug.log "\nSVM Halt" (), printState pc psw ra registers)
                    newModel = {registers = registers, pc = newpc, ra = ra, psw = psw, image = img, ptInstruction = ptInstruction}
                in 
                    newModel

svm: Model -> Model
svm model = 
    let 
        instruction = ramGet model.pc model.image.text
    in 
    case instruction of 
        Hlt -> cycle model
        _   -> 
            let 
                newModel = cycle model
            in 
                svm newModel 


update: Msg -> Model -> Model
update msg model = 
    case msg of 
        Step -> cycle model

        SaveInstruction code ->
            let 
                ptInstruction = code 
            in 
                {model | ptInstruction = ptInstruction}
        Instructions ->
            let 
                text = List.map processCode (List.map String.words (String.lines model.ptInstruction))
                () = Debug.log (toString text) ()
                newImage = {text = text, data = model.image.data}
            in 
                {model | image = newImage}
        RAM numbers ->
            let 
                data = List.map (Result.withDefault 0) (List.map String.toInt (String.words numbers))
                newImage = {text = model.image.text, data = data}
            in 
                {model | image = newImage}
        Run ->  svm model


--View

view: Model -> Html Msg
view model = 
    div []
      [ div [] [ Html.text "RAM data", input [onInput RAM] []]
      , div [] [ Html.text "Instruction", textarea [onInput SaveInstruction] []]
      , button [ onClick Instructions ] [ Html.text "Submit"]
      , button [ onClick Run ] [ Html.text "Run" ]
      , button [ onClick Step] [ Html.text "Step"]
      , div [] [ Html.text "registers", 
                ul [class "registers"] 
                [ li [] [Html.text (toString model.registers.r0)]
                , li [] [Html.text (toString model.registers.r1)]
                , li [] [Html.text (toString model.registers.r2)]
                , li [] [Html.text (toString model.registers.r3)]
                , li [] [Html.text (toString model.pc)]
                , li [] [Html.text (toString model.ra)]
                , li [] [Html.text (toString model.psw)]
                ]
               ]
      ]



