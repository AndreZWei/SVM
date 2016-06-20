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
import Time exposing (..)
import Json.Decode as Json


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

string_of_reg: Maybe Reg -> String
string_of_reg r = 
    case r of 
        Just R0 -> "R0"
        Just R1 -> "R1"
        Just R2 -> "R2"
        Just R3 -> "R3"
        Just Zero -> "Zero"
        Nothing -> "Invalid Register Name"

reg_of_string: String -> Maybe Reg
reg_of_string s = 
    case s of 
        "R0" -> Just R0
        "R1" -> Just R1
        "R2" -> Just R2
        "R3" -> Just R3
        "ZERO" -> Just Zero 
        _ -> Nothing

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
      Lod {rd: Maybe Reg, offset: Maybe Int, rs: Maybe Reg}   --  Let base be the contents of register Rs, loads RAM[base + offset] into register Rd.
    | Li {rd: Maybe Reg, number: Maybe Int}                   --  Loads number into register Rd
    | Sto {rs: Maybe Reg, offset: Maybe Int, rd: Maybe Reg}   --  Let base be the contents of register Rd, stores the contents of register Rs into location base + offset in the memory
    | Mov {rd: Maybe Reg, rs: Maybe Reg}                      --  Copies the content of register Rs into the register Rd
    | Add {rd: Maybe Reg, rs: Maybe Reg, rt: Maybe Reg}       --  Adds the contents of registers Rs and Rt and stores the sum in register Rd
    | Sub {rd: Maybe Reg, rs: Maybe Reg, rt: Maybe Reg}       --  Subtracts the contents of register Rt from Rs and stores the difference in register Rd
    | Mul {rd: Maybe Reg, rs: Maybe Reg, rt: Maybe Reg}       --  Multiplies the contents of registers Rs and Rt and stores the sum in register Rd
    | Div {rd: Maybe Reg, rs: Maybe Reg, rt: Maybe Reg}       --  Divides the contents of register Rt from Rs and stores the difference in register Rd
    | Cmp {rs: Maybe Reg, rt: Maybe Reg}                      --  Sets pwc = Rs - Rt
    | Jsr (Maybe Int)                                           --  Sets RA = PC and then PC = PC + disp
    | R                                                       --  Sets PC = RA
    | Blt (Maybe Int)                                           --  If PSW is negative, PC = PC + disp
    | Beq (Maybe Int)                                           --  If PSW == 0, PC = PC + disp
    | Bgt (Maybe Int)                                           --  If PSW is positive, PC = PC + disp
    | Jmp (Maybe Int)                                           --  PC = PC + disp
    | Hlt                                                     --  Print the contents of registers PC, PSW, RA, R0, R1, R2 and R3. It then stops, returning ().


string_of_int: Maybe Int -> String
string_of_int n = 
    case n of 
        Just n -> "n"
        Nothing -> "Not a number"


string_of_instruction: Instruction -> String
string_of_instruction i = 
    let si = string_of_int in 
    let sr = string_of_reg in 
    case i of 
         Lod {rd, offset, rs} -> "Lod\t" ++ (sr rd) ++ ", " ++ (si offset) ++ "(" ++ (sr rs) ++ ")"
         Li  {rd, number} -> "Li\t" ++ (sr rd) ++ ", " ++ (si number)
         Sto {rs, offset, rd} -> "Sto\t" ++ (sr rs) ++ ", " ++ (si offset) ++ "(" ++ (sr rd) ++ ")"
         Mov { rd,  rs} -> "Mov\t" ++ (sr rd) ++ ", " ++ (sr rs)
         Add { rd,  rs,  rt} -> "Add\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Sub { rd,  rs,  rt} -> "Sub\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Mul { rd,  rs,  rt} -> "Mul\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Div { rd,  rs,  rt} -> "Div\t" ++ (sr rd) ++ ", " ++ (sr rs) ++ ", " ++ (sr rt)
         Cmp { rs,  rt} -> "Cmp\t" ++ (sr rs) ++ ", " ++ (sr rt)
         Blt  disp -> "Blt\t" ++ (si disp)
         Beq  disp -> "Beq\t" ++ (si disp)
         Bgt  disp -> "Bgt\t" ++ (si disp)
         Jmp  disp -> "Jmp\t" ++ (si disp)
         Jsr  disp -> "Jsr\t" ++ (si disp)
         R -> "R"
         Hlt -> "Hlt"

type alias Datasegment = List Int
type alias Textsegment = List (Maybe Instruction)

type alias Image = {data: Datasegment, text: Textsegment}

ramGet: Int -> List a -> Maybe a
ramGet n segment = 
    case n of
        0 -> List.head segment 
        _ -> List.head (List.drop n segment)


ramPut: a -> Int -> List a -> List a
ramPut v n segment = 
    case n of 
        0 -> v::segment
        _ -> List.append (List.take (n-1) segment) (v::(List.drop (n-1) segment))



-- Debugging Support

printState: Int -> Int -> Int -> Registers -> ()
printState pc psw ra registers =
    let regstr = string_of_regs registers in 
    let si = toString 
    in 
    Debug.log ("\n dbg: (pc, psw, ra) = (" ++ si pc ++ "," ++ si psw ++ "," ++ si ra ++ ")\n" ++ "dbg: regs =" ++ regstr) ()



main = 
    program {init = init, update = update, view = view, subscriptions = subs }

--Model 

type alias Model = {registers: Registers, pc: Int, ra: Int, psw: Int, image: Image, field: String, error: Bool, errorMsg: String, finished: Bool, running: Bool, time: Time, speed: Float, scrollTop: Int}

model: Model
model =
    {registers = {r0 = 0, r1 = 0, r2 = 0, r3 = 0}, pc = 0, ra = 0, psw = 0, image = {text = [], data = []}, field = "", error = False, errorMsg = "", finished = False, running = False, time = 0, speed = 1, scrollTop = 0}

initmodel: Model
initmodel = model


init: (Model, Cmd Msg)
init = (model, Cmd.none)

--Update

type Msg = 
      Instructions String
    | Run
    | RAM String
    | Step
    | Reset
    | UpdateTime Time
    | SetSpeed String
    | Stop
    | Position Int

--A helper function for processing instructions

processCode: List String -> Maybe Instruction
processCode s = 
    case s of 
        ["LOD",rd,offset,rs] -> Just (Lod {rd = reg_of_string rd, offset = Result.toMaybe (String.toInt offset), rs = reg_of_string rs})
        ["LI",rd,number] -> Just (Li {rd = reg_of_string rd, number = Result.toMaybe (String.toInt number)})
        ["STO",rs,offset,rd] -> Just (Sto {rs = reg_of_string rs, offset = Result.toMaybe (String.toInt offset), rd = reg_of_string rd})
        ["MOV",rd,rs] -> Just (Mov {rd = reg_of_string rd, rs = reg_of_string rs})
        ["ADD",rd,rs,rt] -> Just (Add {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt})
        ["SUB",rd,rs,rt] -> Just (Sub {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt})
        ["MUL",rd,rs,rt] -> Just (Mul {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt})
        ["DIV",rd,rs,rt] -> Just (Div {rd = reg_of_string rd, rs = reg_of_string rs, rt = reg_of_string rt})
        ["CMP",rs,rt] -> Just (Cmp {rs = reg_of_string rs, rt = reg_of_string rt})
        ["BLT",disp] -> Just (Blt (Result.toMaybe (String.toInt disp)))
        ["BEQ",disp] -> Just (Beq (Result.toMaybe (String.toInt disp)))     
        ["BGT",disp] -> Just (Bgt (Result.toMaybe (String.toInt disp)))
        ["JMP",disp] -> Just (Jmp (Result.toMaybe (String.toInt disp)))
        ["JSR",disp] -> Just (Jsr (Result.toMaybe (String.toInt disp)))
        ["R"] -> Just R
        ["HLT"] -> Just Hlt
        _ -> Nothing

getInstruction: Int -> List (Maybe a) -> Maybe a
getInstruction a text = 
    let 
        instruction = ramGet a text
    in 
        case instruction of 
            Just (Just i) -> Just i
            _ -> Nothing


cycle: Model -> Model
cycle model = 
    let
        instruction = getInstruction model.pc model.image.text 
        registers = model.registers
        pc = model.pc
        ra = model.ra
        psw = model.psw
        img = model.image
        field = model.field
        newpc = pc + 1
    in 

    case instruction of
        Just (Lod {rd, offset, rs}) ->
            case (rd, offset, rs) of
                (Just rd, Just offset, Just rs) ->
                    let 
                        addr = offset + (registerGet rs registers)
                        value = Maybe.withDefault 0 (ramGet addr img.data)
                        newRegisters = registerPut value rd registers
                    in 
                        { model | registers = newRegisters , pc = newpc}
                _ -> { model | error = True , errorMsg = "There is something wrong with your Lod command in line " ++ (toString pc)}

        
        Just (Li {rd, number}) ->
            case (rd, number) of 
                (Just rd, Just number) ->
                    let 
                        newRegisters = registerPut number rd registers
                    in 
                        { model | registers = newRegisters , pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Li command in line " ++ (toString pc)}
        
        Just (Sto {rs, offset, rd}) ->
            case (rs, offset, rd) of 
                (Just rs, Just offset, Just rd) ->
                    let 
                        addr = offset + (registerGet rs registers)
                        value = registerGet rs registers
                        newData = ramPut value addr img.data
                        newImage = {text = img.text, data = newData}
                    in 
                        { model | pc = newpc, image = newImage}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Sto command in line " ++ (toString pc)}

        Just (Mov {rd, rs}) -> 
            case (rd, rs) of 
                (Just rd, Just rs) ->
                    let 
                        v = registerGet rs registers 
                        newRegisters = registerPut v rd registers
                    in 
                        { model | registers = newRegisters, pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Mov command in line " ++ (toString pc)}

        Just (Add {rd, rs, rt}) -> 
            case (rd, rs, rt) of 
                (Just rd, Just rs, Just rt) ->
                    let 
                        v1 = registerGet rs registers 
                        v2 = registerGet rt registers 
                        newRegisters = registerPut (v1 + v2) rd registers 
                    in
                        { model | registers = newRegisters, pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Add command in line " ++ (toString pc)}

        Just (Sub {rd, rs, rt}) -> 
            case (rd, rs, rt) of 
                (Just rd, Just rs, Just rt) ->
                    let 
                        v1 = registerGet rs registers 
                        v2 = registerGet rt registers 
                        newRegisters = registerPut (v1 - v2) rd registers 
                    in
                        { model | registers = newRegisters, pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Sub command in line " ++ (toString pc)}

        Just (Mul {rd, rs, rt}) -> 
            case (rd, rs, rt) of 
                (Just rd, Just rs, Just rt) ->
                    let 
                        v1 = registerGet rs registers 
                        v2 = registerGet rt registers 
                        newRegisters = registerPut (v1 * v2) rd registers 
                    in
                        { model | registers = newRegisters, pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Mul command in line " ++ (toString pc)}

        Just (Div {rd, rs, rt}) -> 
            case (rd, rs, rt) of 
                (Just rd, Just rs, Just rt) ->
                    let 
                        v1 = registerGet rs registers 
                        v2 = registerGet rt registers 
                        newRegisters = registerPut (v1 // v2) rd registers 
                    in
                        { model | registers = newRegisters, pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Div command in line " ++ (toString pc)}   

        Just (Cmp {rs, rt}) ->
            case (rs, rt) of 
                (Just rs, Just rt) ->
                    let 
                        v1 = registerGet rs registers 
                        v2 = registerGet rt registers 
                        value = v1 - v2
                    in 
                        { model | pc = newpc, psw = value}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Cmp command in line " ++ (toString pc)}

        Just (Blt disp) ->
            case disp of 
                Just disp ->
                    case psw < 0 of 
                        True -> { model | pc = newpc +disp}
                        False -> { model | pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Blt command in line " ++ (toString pc)}

        Just (Beq disp) ->
            case disp of 
                Just disp ->
                    case psw == 0 of 
                        True -> { model | pc = newpc + disp}
                        False -> { model | pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Beq command in line " ++ (toString pc)}

        Just (Bgt disp) ->
            case disp of 
                Just disp ->
                    case psw < 0 of 
                        True -> { model | pc = newpc + disp}
                        False -> { model | pc = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Bgt command in line " ++ (toString pc)}

        Just (Jmp disp) ->
            case disp of 
                Just disp -> { model | pc = newpc + disp}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Jmp command in line " ++ (toString pc)}

        Just (Jsr disp) -> 
            case disp of 
                Just disp -> { model | pc = newpc + disp, ra = newpc}
                _ -> { model | error = True, errorMsg = "There is something wrong with your Jsr command in line " ++ (toString pc)}

        Just (R) ->{ model | pc = ra}

        Just (Hlt) -> 
                let 
                    () = (Debug.log "\nSVM Halt" (), printState pc psw ra registers)
                in 
                    { model | pc = newpc, errorMsg = "SVM Halt", finished = True, running = False}
        _ -> Debug.log "Invalid instructions" { model | error = True, errorMsg = "There is something wrong with your instruction in line " ++ (toString newpc) }

svm: Model -> Model
svm model = 
    let 
        instruction = getInstruction model.pc model.image.text
    in 
    case instruction of 
        Just Hlt -> cycle model
        _   ->  case model.error of 
            False ->  let 
                        newModel = cycle model
                      in 
                        svm newModel
            True -> model 

-- A helper function to facilitate the processing of codes
separate: String -> List String
separate s = List.concat (List.map String.words (String.split "," s))


update: Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        Step -> (cycle model, Cmd.none)

        Instructions code ->
            let 
                text = List.map processCode (List.map separate (String.lines (String.toUpper code)))
                () = Debug.log (toString text) ()
                newImage = {text = text, data = model.image.data}
                newModel = {model | image = newImage, field = code}
            in 
                (newModel, Cmd.none)
        
        RAM numbers ->
            let 
                data = List.map (Result.withDefault 0) (List.map String.toInt (separate numbers))
                newImage = {text = model.image.text, data = data} 
                newModel = {model | image = newImage}
            in 
                (newModel, Cmd.none)
        
        Run ->  
            let 
                newModel = { model | running = True}
            in 
                (newModel, Cmd.none)

        Reset -> 
            let 
                newModel = {initmodel | field = model.field, image = model.image, speed = model.speed}
            in 
                (newModel, Cmd.none)

        UpdateTime time -> if (model.running == True) then (cycle model, Cmd.none) else (model, Cmd.none)

        SetSpeed speed  -> Debug.log ("Current speed is" ++ (toString model.speed)) ({model|speed = (Result.withDefault 1 (String.toFloat speed))}, Cmd.none)

        Stop -> { model| running = False, errorMsg = "SVM Halt"} ! []

        Position scrollTop -> Debug.log "HELLO" ( { model | scrollTop = scrollTop }, Cmd.none)


--View

view: Model -> Html Msg
view model =
    div []
      [ header [] [h1 [] [Html.text "Simple Virtual Machine"]] 
      , section [ class "body"] 
      [ span [ class "left" ]
        [ span [ class "text"] [Html.text "RAM Data"]
        , div [ id "RAM_data" ] [ input [placeholder "e.g. 1, 1, 2, 3, 5...", onInput RAM, class "RAM"] []]
        , br [] []
        , p [] [Html.text "Instructions"]
        {-, div [class "instruction"] 
            [ let int = List.length (String.lines model.field) in div [class "line_number"] (draw int int)
            , textarea [onInput Instructions, class "input_instruction"] []
            , p [] [Html.text ("Number of lines = " ++ (toString (List.length model.image.text)))]
            ]
        -}
        , createTextAreaWithLines model "input_instruction"
        , br [] []   
        , div [ class "button_list"]
            [ button [ onClick Run, disabled model.finished, class "button1" ] [ Html.text "Run" ]
            , button [ onClick Step, disabled model.finished, class "button2" ] [ Html.text "Step"]
            , button [ onClick Reset, disabled (model == initmodel), class "button3"] [ Html.text "Reset"] 
            , button [ onClick Stop, disabled (model == initmodel), class "button4"] [Html.text "Stop"] 
            ]
        ]
      
      , span [ class "right" ]
        [   p [] [  Html.text "Processing speed = "
                 ,  input [onInput SetSpeed, class "speed"] []
                 ,  Html.text "sec per instruction"
                 ]
        ,   div [class "registers"] [ Html.text "Registers", 
                    ul [] 
                    [ li [] [Html.text "R0 = ", Html.text (toString model.registers.r0)]
                    , li [] [Html.text "R1 = ", Html.text (toString model.registers.r1)]
                    , li [] [Html.text "R2 = ", Html.text (toString model.registers.r2)]
                    , li [] [Html.text "R3 = ", Html.text (toString model.registers.r3)]
                    , li [] [Html.text "PC = ", Html.text (toString model.pc)]
                    , li [] [Html.text "RA = ", Html.text (toString model.ra)]
                    , li [] [Html.text "PSW = ", Html.text (toString model.psw)]
                    ]
                 ]
        , br [] []
        , span [] [ Html.text "System Message"]
        ,  div [] [ textarea [disabled model.error, class "message"] [Html.text model.errorMsg]]
        ]
      ]
      ]

subs: Model -> Sub Msg
subs model =
    Time.every (model.speed * second) UpdateTime


{--draw: Int -> Int ->List (Html Msg)
draw int total = 
    case (total < 12) of 
        True -> let drawless int total = 
                    case (int == 1) of 
                        True -> [div [class "line"] [Html.text (toString (1+total-int))]]
                        False -> ((div [class "line"] [Html.text ((toString (1 + total - int))++ "\n")])  :: (drawless (int-1) total))
                in 
                    drawless int total
        False -> let drawmore int total =
                    case (int == total - 11) of 
                        True -> [div [class "line"] [Html.text ((toString (total)))]]
                        False -> ((div [class "line"] [Html.text ((toString (2 * total - int - 11))++ "\n")])  :: (draw (int-1) total))
                in 
                    drawmore int total--}

-- Create a string of line numbers
string: Int -> Int -> String
string n total = 
    case (n == 1) of 
        True -> (toString (total + 1 - n))
        False -> (toString (total + 1 - n)) ++ "\n" ++ (string (n-1) total) 


createTextAreaWithLines: Model -> String -> Html Msg
createTextAreaWithLines model id =
    div [ class "textAreaWithLines"
        , styleEl
        ]
        [ div [ class "lineObj"
              , (styleLo model)
              ]
              [ Html.text (string 1000 1000)]
        , textarea [ styleTa
                   , on "keyDown" (Json.succeed (Position model.scrollTop))
                   , onMouseDown (Position model.scrollTop)
                   , onScroll Position
                   , onBlur (Position model.scrollTop)
                   , onFocus (Position model.scrollTop)
                   , onMouseOver Position
                   , onInput Instructions
                   , class "input_instruction"
                   ] [ ]
        ]

styleEl: Attribute msg
styleEl = 
    style 
        [ ("width", "352px")
        , ("height", "290px")
        , ("overflow", "hidden")
        , ("position", "relative")
        ]
styleLo: Model -> Attribute msg
styleLo model =
    let scrollTop = model.scrollTop * (-1) + 2 in
    style 
        [ ("position", "absolute")
        , ("left", "0px")
        , ("width", "20px")
        , ("textAlign", "center")
        , ("top", toString (scrollTop)++"px")
        ]

--Why don't I give it a try?

scrollTop: Json.Decoder Int
scrollTop = 
    Json.at ["target", "scrollTop"] Json.int


styleTa: Attribute msg
styleTa = 
    style
        [ ("position","absolute")
        , ("left", "30px")
        ]

calculateScrollTop: Model -> Int -> Model
calculateScrollTop model scrollTop =
    { model | scrollTop = scrollTop}

onScroll : (Int -> msg) -> Attribute msg
onScroll tagger =
    on "scroll" (Json.map tagger scrollTop)

onMouseOver : (Int -> msg) -> Attribute msg
onMouseOver tagger =
    on "mouseover" (Json.map tagger scrollTop)




      




