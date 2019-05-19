
export function iprb(homoD: number, heteroD: number, homoR: number): number {

    // k individuals homozygous dominant
    // m heterozygous
    // n homozygous recessive
    // chance of dominant allele being present in random mating?

    // the probability can be represented as group/total * (group remaining)/(total - 1)
    // with three groups, there are three probabilities, so P(AA or Aa) is:
    // console.log(`${k} ${m} ${n}`);

    // const total = homoD + heteroD + homoR;
    // let recPairs = homoR * (homoR - 1);
    // recPairs += 0.50 * homoR * heteroD;
    // recPairs += 0.25 * heteroD * (heteroD - 1);
    // recPairs += 0.50 * heteroD * homoR;
    // return 1.0 - recPairs / (total * (total - 1));

    const total = homoD + heteroD + homoR;
    let domPairs = homoD * (homoD - 1);
    domPairs += homoD * heteroD;
    domPairs += homoD * homoR;
    domPairs += heteroD * homoD;
    domPairs += 0.75 * heteroD * (heteroD - 1);
    domPairs += homoR * homoD;
    domPairs += 0.50 * heteroD * homoR;
    domPairs += 0.50 * homoR * heteroD;
    return domPairs / (total * (total - 1));

    // const DomHomo = "D";
    // const DomHetero = "H";
    // const RecHomo = "R";
    // const population: string[] = [];
    // for (let ki = 0; ki < homoD; ++ki) { population.push(DomHomo); }
    // for (let mi = 0; mi < heteroD; ++mi) { population.push(DomHetero); }
    // for (let ni = 0; ni < homoR; ++ni) { population.push(RecHomo); }

    // let totalCount = 0;
    // const pairs: { [pairing: string]: number } = {};
    // for (let lefti = 0; lefti < population.length; ++lefti) {
    //     for (let righti = 0; righti < population.length; ++righti) {
    //         if (righti === lefti) {
    //             continue;
    //         }

    //         totalCount += 1;
    //         const pairing = [population[lefti], population[righti]].sort().join("");
    //         if (pairs[pairing]) {
    //             pairs[pairing] += 1;
    //         } else {
    //             pairs[pairing] = 1;
    //         }
    //     }
    // }

    // // tslint:disable: no-string-literal
    // console.log(`DD:${pairs["DD"]} DH:${pairs["DH"]} DR:${pairs["DR"]} HH:${pairs["HH"]} HR:${pairs["HR"]}`);
    // return (pairs["DD"] + pairs["DH"] + pairs["DR"] +
    //     0.75 * pairs["HH"] + 0.50 * pairs["HR"]) / totalCount;
}
