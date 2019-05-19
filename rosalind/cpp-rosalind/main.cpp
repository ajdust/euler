#include "cons.hpp"
#include "dna.hpp"
#include "fib.hpp"
#include "gc.hpp"
#include "hamm.hpp"
#include "iprb.hpp"
#include "prot.hpp"
#include "revc.hpp"
#include "rna.hpp"
#include "subs.hpp"
#include <fstream>
#include <iostream>
#include <optional>
#include <vector>

/*
Better rules for initialization from Nicolai Josuttis from
https://www.youtube.com/watch?v=7DTlWPgX6zs.
- from video: default to using uniform initialization and avoid auto
- from I: prefer auto when initializing with function
*/

int main(int argc, char* argv[])
{
    if (argc <= 1) {
        std::cerr << "Choose a problem." << std::endl;
        return -1;
    }

    try {
        std::string problem{ argv[1] };
        std::optional<std::string> file = argc >= 3
            ? std::optional{ argv[2] }
            : std::optional<std::string>{};
        if (problem == "cons") {
            std::ifstream infile{ file.value_or("./rosalind_cons.txt") };
            CONS(infile, std::cout);
        } else if (problem == "dna") {
            std::ifstream infile{ file.value_or("./rosalind_dna.txt") };
            DNA(infile, std::cout);
        } else if (problem == "fib") {
            std::ifstream infile{ file.value_or("./rosalind_fib.txt") };
            FIB(infile, std::cout);
        } else if (problem == "gc") {
            std::ifstream infile{ file.value_or("./rosalind_gc.txt") };
            GC(infile, std::cout);
        } else if (problem == "hamm") {
            std::ifstream infile{ file.value_or("./rosalind_hamm.txt") };
            HAMM(infile, std::cout);
        } else if (problem == "iprb") {
            std::ifstream infile{ file.value_or("/rosalind_iprb.txt") };
            IPRB(infile, std::cout);
        } else if (problem == "prot") {
            std::ifstream infile{ file.value_or("./rosalind_prot.txt") };
            PROT(infile, std::cout);
        } else if (problem == "revc") {
            std::ifstream infile{ file.value_or("./rosalind_revc.txt") };
            REVC(infile, std::cout);
        } else if (problem == "rna") {
            std::ifstream infile{ file.value_or("./rosalind_rna.txt") };
            RNA(infile, std::cout);
        } else if (problem == "subs") {
            std::ifstream infile{ file.value_or("./rosalind_subs.txt") };
            SUBS(infile, std::cout);
        }

        std::cout << std::endl;
    } catch (const std::exception& ex) {
        std::cerr << ex.what() << std::endl;
        return -1;
    }
}
