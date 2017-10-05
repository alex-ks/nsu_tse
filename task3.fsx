open System
open System.Linq
open System.Globalization
open System.Text.RegularExpressions

module SyslogParse = 
    module Patterns = 
        let year = DateTime.Now.Year;
        let culture = CultureInfo.InvariantCulture

        let monthPattern = 
            [1..12]
            |> List.map (fun x -> 
                let date = DateTime(year, x, 1) in 
                date.ToString("MMM", culture))
            |> String.concat "|"

        let timePattern = 
            [1..3]
            |> List.map (fun _ -> @"\d\d")
            |> String.concat ":"

        let msgPattern = 
            sprintf @"^(%s) ([ \d]\d) (%s) (\S+) (\S+)\[(\d+)\]( \((\S+)\[(\d+)\]\))?: (.*)$" monthPattern timePattern
        let repeatedMsgPattern = 
            sprintf @"^(%s) ([ \d]\d) (%s) --- last message repeated (\d+) times ---$" monthPattern timePattern
        let dateFormat = "MMM dd HH:mm:ss"

        let dateTime str = DateTime.ParseExact(str, dateFormat, culture)
        let buildDate month day time = 
            seq { yield month; yield day; yield time }
            |> String.concat " "
            |> dateTime

        let msgRegex = Regex(msgPattern)
        let repeatedRegex = Regex(repeatedMsgPattern)

    open Patterns

    let tokenize (regex: Regex) str = List.tail [for group in regex.Match(str).Groups -> group.Value]

    let (|Message|Repeated|MessagePart|) str = 
        if msgRegex.IsMatch str then 
            match tokenize msgRegex str with
            | [month; day; time; source; app; pid; optGroup; appOpt; pidOpt; msg] ->
                let date = buildDate month day time in
                if optGroup = String.Empty then
                    Message (date, source, app, int pid, msg)
                else
                    Message (date, source, appOpt, int pidOpt, msg)
            | _ ->
                failwith "Syslog message parse error"

        elif repeatedRegex.IsMatch str then
            match tokenize repeatedRegex str with
            | [month; day; time; repeatCount] ->
                let date = buildDate month day time in
                Repeated (date, int repeatCount)
            | _ ->
                failwith "Syslog message parse error"

        else
            MessagePart (str.Trim ())

    type SyslogRecord = { date: DateTime; user: string; app: string; pid: int; message: string; count: int }

    module SyslogRecord = 
        let create (date, user, app, pid, msg, count) = 
            { date = date; user = user; app = app; pid = pid; message = msg; count = count }

        let createSingle (date, user, app, pid, msg) = create (date, user, app, pid, msg, 1)

    let parseSyslog lines = 
        let rec parseSyslogCumulative ready rest = 
            if Seq.isEmpty rest then 
                ready
            else
                let newReady = 
                    match Seq.head rest with
                    | Message details -> 
                        let newRecord = SyslogRecord.createSingle details in
                        newRecord :: ready
                    | Repeated (_, count) ->
                        match ready with
                        | head :: rest ->
                            let newHead = { date = head.date
                                            user = head.user
                                            app = head.app
                                            pid = head.pid
                                            message = head.message
                                            count = head.count + count } in
                            newHead :: rest
                        | [] -> failwith "Incorrect syslog order"                    
                    | MessagePart text ->
                        match ready with
                        | head :: rest ->
                            let newHead = { date = head.date
                                            user = head.user
                                            app = head.app
                                            pid = head.pid
                                            message = head.message + text
                                            count = head.count } in
                            newHead :: rest 
                        | [] -> failwith "Incorrect syslog order" 
                in
                parseSyslogCumulative newReady (Seq.tail rest)
        in
        lines
        |> parseSyslogCumulative []
        |> List.rev

open System.IO
open SyslogParse

let openFileAsSeq filename = 
    seq {
        use reader = File.OpenText filename in
        while not reader.EndOfStream do
            yield reader.ReadLine()
    }

let logs = openFileAsSeq "system.log" |> parseSyslog

do printfn "Total records: %d" (List.length logs)

let processes = query {
    for record in logs do
        groupBy record.app into g
        select (g.Key, g.Count ())
}

for (name, count) in processes do
    printfn "Process %s was running from %d different instances" name count
do printfn "%d processes in total" (Seq.length processes)

let users = query {
    for record in logs do 
        groupBy record.user into g
        select (g.Key, float (g.Count ()) / (float (List.length logs)))
}

for (name, part) in users do
    printfn "User %s caused %f%% of logs" name (part * 100.)