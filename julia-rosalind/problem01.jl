module RosalindProblem1

function main()
    if isa(stdin, Base.TTY)
        println("Please pip input in.")
        return
    end

    A = C = T = G = 0
    for line in readlines()
        for c in line
            if c == 'A' A += 1
            elseif c == 'C' C += 1
            elseif c == 'G' G += 1
            elseif c == 'T' T += 1
            end
        end
    end

    println(A, " ", C, " ", G, " ", T)
end

main()

end
