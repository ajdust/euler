
def iprb(homoD: int, heteroD: int, homoR: int):
    total = homoD + heteroD + homoR
    domPairs = homoD * (homoD - 1)
    domPairs += homoD * heteroD
    domPairs += homoD * homoR
    domPairs += heteroD * homoD
    domPairs += 0.75 * heteroD * (heteroD - 1)
    domPairs += homoR * homoD
    domPairs += 0.50 * heteroD * homoR
    domPairs += 0.50 * homoR * heteroD
    return domPairs / (total * (total - 1))
