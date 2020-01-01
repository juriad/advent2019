import strutils
import os
import math
import tables

const size = 5

type
    Cell = enum
        empty, bug
    Side = range[0..(size - 1)]
    Map = array[Side, array[Side, Cell]]
    RecMap = object
        min: int
        max: int
        maps: Table[int, Map]

const side = Side.low..Side.high
const mid = (Side.low + Side.high) div 2

proc readMap(fileName: string): Map =
    let input = readFile(fileName)
    var i, j = Side.low
    var p = 0

    while i <= Side.high and j <= Side.high:
        let c = input[p]
        if c == '.' or c == '#':
            result[i][j] = if c == '.': empty else: bug
            if j < Side.high:
                inc(j)
            elif i < Side.high:
                inc(i)
                j = Side.low
            else:
                break
        inc(p)

proc readRecMap(map: Map): RecMap =
    result.min = 0
    result.max = 0
    result.maps = {0: map}.toTable()

proc `$`(map: Map): string =
    for y in side:
        for x in side:
            result.add(if map[y][x] == bug: "#" else: ".")
        result.add("\n")

proc `$`(recMap: RecMap): string =
    for i in recMap.min .. recMap.max:
        echo "Depth ", i
        echo recMap.maps[i]

proc ifBug(map: Map, x: int, y: int): uint =
    if map[y][x] == bug:
        result = 1u
    else:
        result = 0u

proc adj(map: Map, x: int, y: int): uint =
    if x > Side.low:
        result += ifBug(map, x-1, y)
    if x < Side.high:
        result += ifBug(map, x+1, y)
    if y > Side.low:
        result += ifBug(map, x, y-1)
    if y < Side.high:
        result += ifBug(map, x, y+1)

proc step(map: Map): Map =
    for y in side:
        for x in side:
            var a = adj(map, x, y)
            if map[y][x] == bug:
                result[y][x] = if a == 1u: bug else: empty
            else:
                result[y][x] = if a == 1u or a == 2u: bug else: empty

proc encode(map: Map): uint =
    var p = 1u
    for y in side:
        for x in side:
            if map[y][x] == bug:
                result += p
            p *= 2

proc decode(num: uint): Map =
    var p = 1u
    for y in side:
        for x in side:
            result[y][x] = if (num and p) > 0u: bug else: empty
            p *= 2

proc run(map: Map): uint =
    var history = {0u: encode(map)}.toTable()

    var i = 1u
    var m = map
    while true:
        m = step(m)
        var e = encode(m)

        if history.hasKey(e):
            return e
        else:
            history[e] = i
        inc(i)

proc adjRec(map: Map, x: int, y: int, outer: Map, inner: Map): uint =
    # left
    if x == Side.low:
        result += ifBug(outer, mid-1, mid)
    elif x == mid + 1 and y == mid:
        for yy in side:
            result += ifBug(inner, Side.high, yy)
    else:
        result += ifBug(map, x-1, y)

    # right
    if x == Side.high:
        result += ifBug(outer, mid+1, mid)
    elif x == mid - 1 and y == mid:
        for yy in side:
            result += ifBug(inner, Side.low, yy)
    else:
        result += ifBug(map, x+1, y)

    # up
    if y == Side.low:
        result += ifBug(outer, mid, mid-1)
    elif x == mid and y == mid + 1:
        for xx in side:
            result += ifBug(inner, xx, Side.high)
    else:
        result += ifBug(map, x, y-1)

    # down
    if y == Side.high:
        result += ifBug(outer, mid, mid+1)
    elif x == mid and y == mid - 1:
        for xx in side:
            result += ifBug(inner, xx, Side.low)
    else:
        result += ifBug(map, x, y+1)

proc stepRec(map: Map, outer: Map, inner: Map): Map =
    for y in side:
        for x in side:
            if x == mid and y == mid:
                continue
            var a = adjRec(map, x, y, outer, inner)
            if map[y][x] == bug:
                result[y][x] = if a == 1u: bug else: empty
            else:
                result[y][x] = if a == 1u or a == 2u: bug else: empty

proc countBugs(map: Map): uint =
    for y in side:
        for x in side:
            if x == mid and y == mid:
                continue
            if map[y][x] == bug:
                inc(result)

proc countBugsRec(recMap: RecMap): uint =
    for i in recMap.min .. recMap.max:
        result += countBugs(recMap.maps[i])

proc stepRec(recMap: RecMap): RecMap =
    var e: Map
    for i in recMap.min .. recMap.max:
        result.maps[i] = stepRec(recMap.maps[i],
                if i == recMap.min: e else: recMap.maps[i-1],
                if i == recMap.max: e else: recMap.maps[i+1])
    let outer = stepRec(e, e, recMap.maps[recMap.min])
    if countBugs(outer) > 0u:
        result.maps[recMap.min - 1] = outer
        result.min = recMap.min - 1
    else:
        result.min = recMap.min

    let inner = stepRec(e, recMap.maps[recMap.max], e)
    if countBugs(inner) > 0u:
        result.maps[recMap.max + 1] = inner
        result.max = recMap.max + 1
    else:
        result.max = recMap.max

proc runRec(recMap: RecMap, cnt: uint): uint =
    var rm = recmap
    for i in 1..cnt:
        rm = stepRec(rm)
    result = countBugsRec(rm)

let m0 = readMap(paramStr(1))
echo run(m0)

let rm0 = readRecMap(m0)
echo runRec(rm0, parseUInt(paramStr(2)))
