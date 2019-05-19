#include "prot.hpp"
#include <iostream>
#include <string>
#include <vector>

enum AminoAcid { A,
    C,
    D,
    E,
    F,
    G,
    H,
    I,
    K,
    L,
    M,
    N,
    P,
    Q,
    R,
    S,
    T,
    V,
    W,
    Y,
    Stop };

AminoAcid to_aa(std::string codon)
{
    if (codon == "UUU")
        return F;
    else if (codon == "CUU")
        return L;
    else if (codon == "AUU")
        return I;
    else if (codon == "GUU")
        return V;
    else if (codon == "UUC")
        return F;
    else if (codon == "CUC")
        return L;
    else if (codon == "AUC")
        return I;
    else if (codon == "GUC")
        return V;
    else if (codon == "UUA")
        return L;
    else if (codon == "CUA")
        return L;
    else if (codon == "AUA")
        return I;
    else if (codon == "GUA")
        return V;
    else if (codon == "UUG")
        return L;
    else if (codon == "CUG")
        return L;
    else if (codon == "AUG")
        return M;
    else if (codon == "GUG")
        return V;
    else if (codon == "UCU")
        return S;
    else if (codon == "CCU")
        return P;
    else if (codon == "ACU")
        return T;
    else if (codon == "GCU")
        return A;
    else if (codon == "UCC")
        return S;
    else if (codon == "CCC")
        return P;
    else if (codon == "ACC")
        return T;
    else if (codon == "GCC")
        return A;
    else if (codon == "UCA")
        return S;
    else if (codon == "CCA")
        return P;
    else if (codon == "ACA")
        return T;
    else if (codon == "GCA")
        return A;
    else if (codon == "UCG")
        return S;
    else if (codon == "CCG")
        return P;
    else if (codon == "ACG")
        return T;
    else if (codon == "GCG")
        return A;
    else if (codon == "UAU")
        return Y;
    else if (codon == "CAU")
        return H;
    else if (codon == "AAU")
        return N;
    else if (codon == "GAU")
        return D;
    else if (codon == "UAC")
        return Y;
    else if (codon == "CAC")
        return H;
    else if (codon == "AAC")
        return N;
    else if (codon == "GAC")
        return D;
    else if (codon == "UAA")
        return Stop;
    else if (codon == "CAA")
        return Q;
    else if (codon == "AAA")
        return K;
    else if (codon == "GAA")
        return E;
    else if (codon == "UAG")
        return Stop;
    else if (codon == "CAG")
        return Q;
    else if (codon == "AAG")
        return K;
    else if (codon == "GAG")
        return E;
    else if (codon == "UGU")
        return C;
    else if (codon == "CGU")
        return R;
    else if (codon == "AGU")
        return S;
    else if (codon == "GGU")
        return G;
    else if (codon == "UGC")
        return C;
    else if (codon == "CGC")
        return R;
    else if (codon == "AGC")
        return S;
    else if (codon == "GGC")
        return G;
    else if (codon == "UGA")
        return Stop;
    else if (codon == "CGA")
        return R;
    else if (codon == "AGA")
        return R;
    else if (codon == "GGA")
        return G;
    else if (codon == "UGG")
        return W;
    else if (codon == "CGG")
        return R;
    else if (codon == "AGG")
        return R;
    else if (codon == "GGG")
        return G;

    throw "Invalid codon";
}

std::string to_str(AminoAcid aa)
{
    if (aa == A)
        return "A";
    else if (aa == C)
        return "C";
    else if (aa == D)
        return "D";
    else if (aa == E)
        return "E";
    else if (aa == F)
        return "F";
    else if (aa == G)
        return "G";
    else if (aa == G)
        return "G";
    else if (aa == H)
        return "H";
    else if (aa == I)
        return "I";
    else if (aa == K)
        return "K";
    else if (aa == L)
        return "L";
    else if (aa == M)
        return "M";
    else if (aa == N)
        return "N";
    else if (aa == P)
        return "P";
    else if (aa == Q)
        return "Q";
    else if (aa == R)
        return "R";
    else if (aa == S)
        return "S";
    else if (aa == T)
        return "T";
    else if (aa == V)
        return "V";
    else if (aa == W)
        return "W";
    else if (aa == Y)
        return "Y";
    else if (aa == Stop)
        return "M";

    throw "Amino acid code invalid";
}

void output_amino_acids(std::vector<AminoAcid> aas, std::ostream& output)
{
    for (auto aa : aas)
        output << to_str(aa);
}

void PROT(std::istream& input, std::ostream& output)
{
    std::string line{};
    bool started{ false };

    // find the starting codon
    while (std::getline(input, line)) {
        auto start = line.find("AUG");
        if (start != std::string::npos) {
            line = line.substr(start);
            started = true;
            break;
        }
    }

    if (!started) {
        output << "No starting codon found";
        return;
    }

    // chunk into codons and map to amino acids
    std::vector<AminoAcid> amino_acids{};
    std::vector<char> buffer{};
    do {
        // chunk into three, possibly across multiple lines
        auto it = line.begin();
        for (;;) {
            if (it == line.end())
                break;

            buffer.push_back(*it);
            if (++it == line.end())
                break;

            buffer.push_back(*it);
            if (++it == line.end())
                break;

            buffer.push_back(*it);
            ++it;

            // make codon string
            std::string codon{ buffer.begin(), buffer.end() };
            buffer.clear();
            AminoAcid aa{ to_aa(codon) };
            if (aa == Stop) {
                output_amino_acids(amino_acids, output);
                return;
            }

            amino_acids.push_back(to_aa(codon));
        }
    } while (std::getline(input, line));
}
