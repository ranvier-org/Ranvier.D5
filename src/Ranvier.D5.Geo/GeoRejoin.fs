namespace Ranvier.D5.Geo

open Ranvier.D5

/// Fragment endpoints carry boundary parameters, so boundaries that collapse on screen (a conic apex) still order them.
type internal GeoClipBoundary = {
    Period: float
    PointAt: float -> Point
    ContainsAt: float -> bool
    /// Emits boundary points after the first parameter up to the second, then the destination point.
    Between: float -> float -> Point -> int -> bool -> (Point -> unit) -> unit
}

// Adapted from d3-geo boundary rejoining, Copyright 2010-2024 Mike Bostock.
// The ISC permission notice is distributed in licenses/d3-geo.txt.
module internal GeoRejoin =
    exception private Rejected of LayoutError

    let run maxBuffer spend boundary (fragments:ResizeArray<struct(Point array*float*float)>) =
        try
            let checkBuffer count =
                if count>maxBuffer then raise(Rejected(BudgetExceeded "Polygon rejoining exceeds its vertex buffer budget."))
            let output=ResizeArray<Point array>()
            let mutable outputVertices=0
            let publish (points:Point array) =
                if points.Length>=3 then
                    if points.Length>maxBuffer-outputVertices then
                        raise(Rejected(BudgetExceeded "Polygon rejoining exceeds its output buffer budget."))
                    outputVertices<-outputVertices+points.Length
                    output.Add points
            let working=ResizeArray<Point>()
            let add point =
                spend 1L
                if working.Count=0 || working[working.Count-1]<>point then
                    checkBuffer(working.Count+1)
                    working.Add point
            let finish () =
                if working.Count>1 && working[0]=working[working.Count-1] then working.RemoveAt(working.Count-1)
                publish(working.ToArray())
                working.Clear()
            let openFragments=ResizeArray<Point array>()
            let openParameters=ResizeArray<struct(float*float)>()
            let mutable inputVertices=0
            for struct(points,startParameter,endParameter) in fragments do
                spend 1L
                if points.Length>maxBuffer-inputVertices then
                    raise(Rejected(BudgetExceeded "Polygon fragments exceed the rejoin buffer budget."))
                inputVertices<-inputVertices+points.Length
                if points.Length>1 then
                    if points[0]=points[points.Length-1] then publish points[0..points.Length-2]
                    else
                        openFragments.Add points
                        openParameters.Add(struct(startParameter,endParameter))
            if openFragments.Count=0 then
                if boundary.ContainsAt 0. then
                    let first=boundary.PointAt 0.
                    add first
                    boundary.Between 0. 0. first 1 true add
                    finish()
            else
                let count=openFragments.Count*2
                checkBuffer count
                let points=Array.zeroCreate<Point> count
                let parameters=Array.zeroCreate<float> count
                for index=0 to count-1 do
                    spend 1L
                    let fragment=openFragments[index/2]
                    let struct(startParameter,endParameter)=openParameters[index/2]
                    points[index]<-if index%2=0 then fragment[0] else fragment[fragment.Length-1]
                    parameters[index]<-if index%2=0 then startParameter else endParameter
                let order=Array.init count id
                let scratch=Array.zeroCreate<int> count
                let mutable width=1
                while width<count do
                    let mutable start=0
                    while start<count do
                        let middle=min count (start+width)
                        let finish=min count (middle+width)
                        let mutable left,right=start,middle
                        for index=start to finish-1 do
                            spend 1L
                            if left<middle && (right>=finish || parameters[order[left]]<=parameters[order[right]]) then
                                scratch[index]<-order[left];left<-left+1
                            else scratch[index]<-order[right];right<-right+1
                        start<-finish
                    for index=0 to count-1 do
                        spend 1L
                        order[index]<-scratch[index]
                    width<-width*2
                let next=Array.zeroCreate<int> count
                let previous=Array.zeroCreate<int> count
                let entry=Array.zeroCreate<bool> count
                let visited=Array.zeroCreate<bool> count
                let seed=(parameters[order[count-1]]+parameters[order[0]]+boundary.Period)/2.
                let mutable inside=boundary.ContainsAt seed
                for index=0 to count-1 do
                    spend 1L
                    let current=order[index]
                    next[current]<-order[(index+1)%count]
                    previous[current]<-order[(index+count-1)%count]
                    inside<-not inside
                    entry[current]<-inside
                for start=0 to count-1 do
                    if not visited[start] then
                        working.Clear()
                        let mutable current=start
                        let mutable subject=true
                        let mutable running=true
                        while running do
                            spend 1L
                            visited[current]<-true
                            if subject then
                                let fragment=openFragments[current/2]
                                if current%2=0 then
                                    for point in fragment do add point
                                    current<-current+1
                                else
                                    for index=fragment.Length-1 downto 0 do add fragment[index]
                                    current<-current-1
                            else
                                let target=if entry[current] then next[current] else previous[current]
                                boundary.Between parameters[current] parameters[target] points[target] (if entry[current] then 1 else -1) false add
                                current<-target
                            subject<-not subject
                            running<-not visited[current]
                        finish()
            let mutable result=[]
            for index=output.Count-1 downto 0 do
                spend 1L
                result<-output[index]::result
            Ok result
        with Rejected error -> Error error
