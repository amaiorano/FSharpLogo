// Simple Logo-like DSL in F#

open System

type Value =
    | Immediate of int
    | Variable of string

type Command =
    | Repeat of iterations:Value * Command list
    | Forward of units:Value
    | Backward of units:Value
    | Right of degs:Value
    | Left of degs:Value
    | PenUp
    | PenDown
    | Make of string * int

let Make name value =
    Make (name, value) 



// All off this machinery is to support commands that can take either an
// immediate value or a variable name
let (|Integer|_|) x =
    let mutable r = 0
    if System.Int32.TryParse (string x, &r) then Some r
    else None

let ValueFunc func (v:obj) =
    match v with
    | Integer(x) -> func (Immediate x)
    | x -> func (Variable (string x))

let BindValueFunc func = ValueFunc func

let Repeat v c = BindValueFunc (fun x -> Repeat(x,c)) v
let Forward v = BindValueFunc Forward v
let Backward v = BindValueFunc Backward v
let Right v = BindValueFunc Right v
let Left v = BindValueFunc Left v

// variable name -> int value data store
open System.Collections.Generic
let variables = new Dictionary<string, int>()

let SetVariable name value =
    if variables.ContainsKey name then variables.[name] = value |> ignore
    else variables.Add (name, value) |> ignore

let ReadValue value =
    match value with
    | Immediate v -> v
    | Variable v -> variables.[v]

// Active patterns to extract final int value
let (|ForwardValue|_|) = function
    | Forward value -> Some (ReadValue value)
    | _ -> None

let (|BackwardValue|_|) = function
    | Backward value -> Some (ReadValue value)
    | _ -> None

let (|RightValue|_|) = function
    | Right value -> Some (ReadValue value)
    | _ -> None

let (|LeftValue|_|) = function
    | Left value -> Some (ReadValue value)
    | _ -> None


#r "System.Windows.Forms"
open System.Windows.Forms
open System.Drawing

// Drawing canvas with center at (0,0), Y+ is up, X+ is right
type Canvas(width, height) =
    let form = new Form(Text="Logo", TopMost=true, Width=width, Height=height)
    let picBox = new PictureBox()
    do 
        picBox.Dock <- DockStyle.Fill
        form.Controls.Add picBox
    let bitmap = new Bitmap(form.Width, form.Height)
    do
        picBox.Image <- (bitmap :> Image)
    let gfx = Graphics.FromImage(bitmap)
    let pen = new Pen(Color.Black, 3.0f)

    let toGfxSpace (pt:Point) =
        Point(pt.X + width/2, height/2 - pt.Y)

    member this.Width = float width
    member this.Height = float height
    member this.DrawLine(pt1 :Point, pt2 :Point) =
        let pt1f = toGfxSpace pt1
        let pt2f = toGfxSpace pt2
        gfx.DrawLine(pen, toGfxSpace pt1, toGfxSpace pt2)
    member this.Show() = form.Show()
    

type Vector2(x:float, y:float) =
    let mutable x = x
    let mutable y = y
    new() = Vector2(0.0, 0.0)
    member this.X with get() = x and set (v) = x <- v
    member this.Y with get() = y and set (v) = y <- v
    member this.asPoint = Point(int(x + 0.5), int(y + 0.5))
    static member (+) (v1:Vector2, v2:Vector2) = Vector2(v1.X + v2.X, v1.Y + v2.Y)
    static member (-) (v1:Vector2, v2:Vector2) = Vector2(v1.X - v2.X, v1.Y - v2.Y)
    static member (*) (v1:Vector2, s) = Vector2(v1.X * s, v1.Y * s)


let degToRad d = float(d) * System.Math.PI / 180.0


// Turtle that moves across a canvas
type Turtle(canvas:Canvas) =
    let mutable pos = Vector2()
    let mutable penDown = true
    let mutable angle = Math.PI / 2.0;
    let facingVector () = Vector2(cos angle, sin angle)
    member this.PenUp () = penDown <- false
    member this.PenDown () = penDown <- true
    member this.FollowCommand (command:Command) =
        match command with
        | ForwardValue(units) ->
            let targetPos = pos + facingVector() * (float)units
            if penDown then
                canvas.DrawLine(pos.asPoint, targetPos.asPoint)
            pos <- targetPos
        | BackwardValue(units) ->
            let targetPos = pos - facingVector() * (float)units
            if penDown then
                canvas.DrawLine(pos.asPoint, targetPos.asPoint)
            pos <- targetPos
        | RightValue(degrees) ->
            angle <- angle - (degToRad degrees)
        | LeftValue(degrees) ->
            angle <- angle + (degToRad degrees)
        | PenUp ->
            this.PenUp ()
        | PenDown ->
            this.PenDown()
        | _ ->
            ()


let canvas = Canvas(800, 800)
let turtle = Turtle(canvas)


let rec ExecuteCommand command =
    match command with
    | Repeat (iterations, commandList) -> 
        for _ in 0..ReadValue(iterations) do
            commandList |> Seq.iter ExecuteCommand
    | Make (name, value) ->
        SetVariable name value
    | _ ->
        turtle.FollowCommand command

let ExecuteCommands commands =
    commands |> Seq.iter ExecuteCommand


let program = [
    Make "Rpt1" 15
    Make "Rpt2" 10
    Make "Rpt3" 10
    Make "Radius" 350
    Make "HalfRadius" (350/2)
    PenUp
    Backward "HalfRadius"
    Repeat "Rpt1" [ 
        PenUp
        Forward "Radius"
        PenDown
        Repeat "Rpt2" [
            Right 30
            Forward 50
            Repeat "Rpt3" [
                Right 70
                Forward 20
                Backward 10
            ]
        ]
        Right 50
    ]
]

program |> ExecuteCommands
canvas.Show()
