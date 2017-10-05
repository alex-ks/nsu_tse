open System
open System.Globalization
open System.Text.RegularExpressions

let now = DateTime.Now;
let culture = CultureInfo.InvariantCulture

let monthPattern = 
    [1..12]
    |> List.map (fun x -> 
        let date = DateTime(now.Year, x, now.Day) in 
        date.ToString("MMM", culture))
    |> String.concat "|"

let timePattern = 
    [1..3]
    |> List.map (fun _ -> @"\d\d")
    |> String.concat ":"

let msgPattern = sprintf @"^(%s) ([ \d]\d) (%s) (\S+) (\S+)\[(\d+)\]: (.*)$" monthPattern timePattern
let repeatedMsgPattern = sprintf @"^(%s) ([ \d]\d) (%s) --- last message repeated (\d+) times ---$" monthPattern timePattern

let msgRegex = Regex(msgPattern)
let repeatedRegex = Regex(repeatedMsgPattern)

let tokenize (regex: Regex) str = [for group in regex.Match(str).Groups -> group.Value] |> List.skip 1

let (|Message|Repeated|MessagePart|) str = 
    if msgRegex.IsMatch str then 
        match tokenize msgRegex str with
        | [month; day; time; source; app; pid; msg] ->
            Message (month, int day, time, source, app, int pid, msg)
        | _ ->
            failwith "Syslog message parse error"
    elif repeatedRegex.IsMatch str then
        match tokenize repeatedRegex str with
        | [month; day; time; repeatCount] ->
            Repeated (month, day, time, repeatCount)
        | _ ->
            failwith "Syslog message parse error"
    else
        MessagePart (str.Trim ())

let msg = @"Sep 28 00:00:46 komissar-osx Yandex.Disk[371]: -[YDSystemStateController checkRestart]"
let repeatedMsg = @"Sep 28 11:09:07 --- last message repeated 10 times ---"

match msg with
| Message (month, day, time, source, app, pid, msg) ->
    do printfn "%s %d %s %s %s[%d]: %s" month day time source app pid msg
| _ -> 
    do printfn "No!"

do printfn "%A" (DateTime.Parse("Sep 28 00:00:46"))
