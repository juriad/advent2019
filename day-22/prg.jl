struct Shuffle
    mult::UInt128 # first multiple
    add::UInt128 # then add
    deckSize::UInt128 # modulo deck size
end

combine(s1::Shuffle, s2::Shuffle) =
    Shuffle(s2.mult * s1.mult % s2.deckSize, (s2.add + s1.add * s2.mult) % s2.deckSize, s2.deckSize)

combine(shuffles::Vector{Shuffle}) =
    foldl(combine, shuffles, init = Shuffle(1, 0, 1))

inverse(s::Shuffle) =
    combine(
        Shuffle(1, s.add == 0 ? 0 : s.deckSize - s.add, s.deckSize),
        Shuffle(s.mult == 1 ? 1 : invmod(s.mult, s.deckSize), 0, s.deckSize)
    )

function iterate(s::Shuffle, count::UInt128)
    iter = [s]
    i = 1
    c::UInt128 = 1
    while c < count
        push!(iter, combine(iter[i], iter[i]))
        i += 1
        c *= 2
    end

    result = Shuffle(1, 0, 1)
    i = 1
    c = 1
    while c <= count
        if (count & c) > 0
            result = combine(result, iter[i])
        end
        i += 1
        c *= 2
    end
    return result
end

apply(s::Shuffle, card::UInt128) =
    (card * s.mult + s.add) % s.deckSize

function readShuff(fileName::String, deckSize::UInt128)
    shuffles = Shuffle[];

    open(fileName) do file
        for ln in eachline(file)
            if startswith(ln, "deal into new stack")
                n = deckSize - 1
                push!(shuffles, Shuffle(n, n, deckSize))
            elseif startswith(ln, "cut")
                n = parse(Int, split(ln, " ")[2])
                n = n < 0 ? - n : deckSize - n
                push!(shuffles, Shuffle(1, n, deckSize))
            elseif startswith(ln, "deal with increment")
                n = parse(Int, split(ln, " ")[4])
                push!(shuffles, Shuffle(n, 0, deckSize))
            elseif length(ln) == 0
                # do nothing
            else
                error("Unknown line >$ln<")
            end
        end
    end

    return shuffles
end


function task1()
    ds = parse(UInt128, ARGS[3])
    shuf = combine(readShuff(ARGS[2], ds))
    card = parse(UInt128, ARGS[4])
    pos = apply(shuf, card)

    println("The card $card is at position $pos.")
end

function task2()
    ds = parse(UInt128, ARGS[3])
    shuf = combine(readShuff(ARGS[2], ds))
    pos = parse(UInt128, ARGS[4])
    times = parse(UInt128, ARGS[5])

    inve = iterate(inverse(shuf), times)
    card = apply(inve, pos)

    println("The card $card is at position $pos.")
end

function main()
    if ARGS[1] == "1"
        task1()
    elseif ARGS[1] == "2"
        task2()
    else
        error("The first argument must be 1 for task1, or 2 for task2")
    end
end

main()
