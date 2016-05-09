// Simple Logo-like DSL in F#

open System

type Command =
    | DoRepeat of iterations:int * Command list
    | Forward of units:int
    | Right of degs:int

let Repeat iterations command =
    DoRepeat(iterations, command)


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
    let pen = new Pen(Color.Black, 5.0f)

    let toGfxSpace (pt:Point) =
        Point(pt.X + width/2, height/2 - pt.Y)

    member this.Width = float picBox.Width
    member this.Height = float picBox.Height
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


let degToRad d = float(d) * System.Math.PI / 180.0


// Turtle that moves across a canvas
type Turtle(canvas:Canvas) =
    let mutable pos = Vector2()
    let mutable penDown = true
    let mutable angle = Math.PI/2.0;
    
    member this.PenUp = penDown <- false
    member this.PenDown = penDown <- true
    member this.FollowCommand (command:Command) =
        match command with
        | Forward units ->
            let pt2 = Vector2()
            pt2.X <- pos.X + cos angle * (float)units
            pt2.Y <- pos.Y + sin angle * (float)units
            canvas.DrawLine(pos.asPoint, pt2.asPoint)
            pos <- pt2
        | Right degrees ->
            angle <- angle + (degToRad degrees)
        | _ ->
            ()


let canvas = Canvas(800, 800)
let turtle = Turtle(canvas)


let rec ExecuteCommand command =
    match command with
    | DoRepeat (iterations, commandList) -> 
        for _ in 0..iterations do
            commandList |> Seq.iter ExecuteCommand
    | _ ->
        turtle.FollowCommand command


let program =
    Repeat 15 [ 
        Forward 350
        Repeat 10 [
            Right 30
            Forward 50
            Repeat 10 [
                Right 70
                Forward 20
            ]
        ]
        Right 50
    ]
    
program |> ExecuteCommand
canvas.Show()
